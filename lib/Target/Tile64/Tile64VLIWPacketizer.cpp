//===--- Tile64VLIWPacketizer.cpp - VLIW Packetizer -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//                               Tile64 Backend
//
// Author: David Juhasz
// E-mail: juhda@caesar.elte.hu
// Institute: Dept. of Programming Languages and Compilers, ELTE IK, Hungary
//
// The research is supported by the European Union and co-financed by the
// European Social Fund (grant agreement no. TAMOP
// 4.2.1./B-09/1/KMR-2010-0003).
//
// The original implementation of Tile64 VLIW Packetizer was made using that of
// Hexagon VLIW Packetizer.
//
// This implements a simple VLIW packetizer using DFA. The packetizer works on
// machine basic blocks. For each instruction I in BB, the packetizer consults
// the DFA to see if machine resources are available to execute I. If so, the
// packetizer checks if I depends on any instruction J in the current packet.
// If no dependency is found, I is added to current packet and machine resource
// is marked as taken. If any dependency is found, a target API call is made to
// prune the dependence.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "packets"

#include "Tile64.h"
#include "Tile64InstrInfo.h"
#include "llvm/CodeGen/DFAPacketizer.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/ScheduleDAG.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/MC/MCInstrItineraries.h"

using namespace llvm;
using namespace Tile64II;

namespace {
  class Tile64VLIWPacketizer : public MachineFunctionPass {

  public:
    static char ID;
    Tile64VLIWPacketizer() : MachineFunctionPass(ID) {}

    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      AU.addRequired<MachineDominatorTree>();
      AU.addPreserved<MachineDominatorTree>();
      AU.addRequired<MachineLoopInfo>();
      AU.addPreserved<MachineLoopInfo>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    const char *getPassName() const {
      return "Tile64 VLIW Packetizer";
    }

    bool runOnMachineFunction(MachineFunction &Fn);
  };
  char Tile64VLIWPacketizer::ID = 0;

  class Tile64VLIWPacketizerList : public VLIWPacketizerList {

    // Check if there is a dependence between some instruction already in this
    // packet and this instruction.
    bool Dependence;

    // Only check for dependence if there are resources available to schedule
    // this instruction.
    bool FoundSequentialDependence;

  public:
    // Ctor.
    Tile64VLIWPacketizerList(MachineFunction &MF, MachineLoopInfo &MLI,
                             MachineDominatorTree &MDT);

		//default implementation of virtual function addToPacket will do

    // initPacketizerState - initialize some internal flags.
    virtual void initPacketizerState(void);


    // ignorePseudoInstruction - Ignore bundling of pseudo instructions.
    virtual bool ignorePseudoInstruction(MachineInstr *MI,
                                         MachineBasicBlock *MBB);

    // isSoloInstruction - return true if instruction MI can not be packetized
    // with any other instruction, which means that MI itself is a packet.
    virtual bool isSoloInstruction(MachineInstr *MI);

    // canBundleIntoCurrentPacket - return true is instruction MI can be put
		// into the current packet according to target-specific bundle constraints.
		// Use after checking if functional units could execute the instruction.
    virtual bool canBundleIntoCurrentPacket(MachineInstr *MI);

    // isLegalToPacketizeTogether - Is it legal to packetize SUI and SUJ
    // together.
    virtual bool isLegalToPacketizeTogether(SUnit *SUI, SUnit *SUJ);

    // isLegalToPruneDependencies - Is it legal to prune dependece between SUI
    // and SUJ.
    virtual bool isLegalToPruneDependencies(SUnit *SUI, SUnit *SUJ);

  private:
    // isDirectJump - Return true if the instruction is a direct jump.
    bool isDirectJump(const MachineInstr *MI) const;

    // isTile64SoloInstruction - Return true if TSFlags:4 is 1.
    bool isTile64SoloInstruction(const MachineInstr *MI) const;

    // isTile64LongInstruction - Return true if TSFlags:5 is 1.
    bool isTile64LongInstruction(const MachineInstr *MI) const;

    // Tile64TypeOf - Return Tile64Type of MI.
		Tile64Type Tile64TypeOf(const MachineInstr *MI) const;

    // isTile64CtrInstruction - Return true if Tile64Type of MI is TypeCtr.
    bool isTile64CtrInstruction(const MachineInstr *MI) const;

		// isTile64MemInstruction - Return true if Tile64Type of MI is TypeMeS or
		// TypeMeL.
    bool isTile64MemInstruction(const MachineInstr *MI) const;
  };
}

// Tile64VLIWPacketizerList Ctor.
Tile64VLIWPacketizerList::Tile64VLIWPacketizerList(MachineFunction &MF,
                                                   MachineLoopInfo &MLI,
                                                   MachineDominatorTree &MDT)
  : VLIWPacketizerList(MF, MLI, MDT, true){
}

bool Tile64VLIWPacketizer::runOnMachineFunction(MachineFunction &Fn) {
  const TargetInstrInfo *TII = Fn.getTarget().getInstrInfo();
  MachineLoopInfo &MLI = getAnalysis<MachineLoopInfo>();
  MachineDominatorTree &MDT = getAnalysis<MachineDominatorTree>();

  // Instantiate the packetizer.
  Tile64VLIWPacketizerList Packetizer(Fn, MLI, MDT);

  // DFA state table should not be empty.
  assert(Packetizer.getResourceTracker() && "Empty DFA table!");

	// Hexagon deletes KILL instructions here, but in the case of Tile64 there
	// should be no KILLs, as there are no sub-registers.
	
  // Loop over all of the basic blocks.
  for (MachineFunction::iterator MBB = Fn.begin(), MBBe = Fn.end();
       MBB != MBBe; ++MBB) {
    // Find scheduling regions and schedule / packetize each region.
    for(MachineBasicBlock::iterator RegionEnd = MBB->end(), MBBb = MBB->begin();
        RegionEnd != MBBb;) {
      // The next region starts above the previous region. Look backward in the
      // instruction stream until we find the nearest boundary.
      MachineBasicBlock::iterator I = RegionEnd;
      for(; I != MBBb; --I) {
        if (TII->isSchedulingBoundary(llvm::prior(I), MBB, Fn))
          break;
      }

      // Skip empty regions and regions with one instruction.
      MachineBasicBlock::iterator priorEnd = llvm::prior(RegionEnd);
      if (I == RegionEnd || I == priorEnd) {
        RegionEnd = priorEnd;
        continue;
      }

      Packetizer.PacketizeMIs(MBB, I, RegionEnd);
      RegionEnd = I;
    }
  }

  return true;
}


// initPacketizerState - Initialize packetizer flags
void Tile64VLIWPacketizerList::initPacketizerState() {
  Dependence = false;
  FoundSequentialDependence = false;
}

// ignorePseudoInstruction - Ignore bundling of pseudo instructions.
bool Tile64VLIWPacketizerList::ignorePseudoInstruction(MachineInstr *MI,
                                                       MachineBasicBlock *MBB) {
  assert(!isSoloInstruction(MI) && "Solo instruction should not be here!");
  if(MI->isDebugValue()) {
    return true;
  } else {
    //all other instructions should have functional unit mapped to them.
    assert(ResourceTracker->getInstrItins()->beginStage(MI->getDesc().getSchedClass())->getUnits()
           && "Instruction without FuncUnit!");
    return false;
  }
}

// isSoloInstruction - Returns true for instructions that must be
// scheduled in their own packet.
bool Tile64VLIWPacketizerList::isSoloInstruction(MachineInstr *MI) {
  //Tile Reference Manual doesn't imply any solo instructions.
  //TODO: hexagon says EHLabel to be a solo instruction as well
  if (MI->isInlineAsm() || isTile64SoloInstruction(MI)) {
    return true;
  } else {
    return false;
  }	
}

// canBundleIntoCurrentPacket - check target-specific bundle-constraints, see
// Tile64Schedule.td bundle-approach.
bool Tile64VLIWPacketizerList::canBundleIntoCurrentPacket(MachineInstr *MI) {
  MachineInstr *MI0, *MI1;

  //Note that physical units could execute current packet and MI, which
  //simplifies the checking we need to do here.
  switch(CurrentPacketMIs.size()) {
  case 0:
    return true;
  case 1:
    MI0 = CurrentPacketMIs[0];
    return !((isTile64CtrInstruction(MI) && isTile64MemInstruction(MI0)) ||
             (isTile64CtrInstruction(MI0) && isTile64MemInstruction(MI)));
  case 2:
    MI0 = CurrentPacketMIs[0];
    MI1 = CurrentPacketMIs[1];
    return (!isTile64LongInstruction(MI) && !isTile64LongInstruction(MI0) &&
            !isTile64LongInstruction(MI1));
  default:
    llvm_unreachable("3 pipelines can't execute more than 3 instructions!");
  }
}

// isLegalToPacketizeTogether:
// SUI is the current instruction that is out side of the current packet.
// SUJ is the current instruction inside the current packet against which that
// SUI will be packetized.
bool Tile64VLIWPacketizerList::isLegalToPacketizeTogether(SUnit *SUI,
                                                          SUnit *SUJ) {
  MachineInstr *I = SUI->getInstr();
  MachineInstr *J = SUJ->getInstr();
  assert(I && J && "Unable to packetize null instruction!");
  assert(!isSoloInstruction(I) && !ignorePseudoInstruction(I, I->getParent()) &&
         "Something gone wrong with packetizer mechanism!");

  const MCInstrDesc &MCIDI = I->getDesc();
  const MCInstrDesc &MCIDJ = J->getDesc();

  //In the case of Tile64, two control flow instructions cannot have resource
  //in the same time.

  if(SUJ->isSucc(SUI)) {
    //FIXME: is Succs not a set? -- use the loop only to find the index...
    for(unsigned i = 0;
        (i < SUJ->Succs.size()) && !FoundSequentialDependence;
        ++i) {

      if(SUJ->Succs[i].getSUnit() != SUI) {
        continue;
      }

      SDep Dep = SUJ->Succs[i];
      SDep::Kind DepType = Dep.getKind();
      unsigned DepReg = 0;
      if(DepType != SDep::Order) {
        DepReg = Dep.getReg();
      }

      if((MCIDI.isCall() || MCIDI.isReturn()) && DepType == SDep::Order) {
        // do nothing
      }

      //Hexagon handles predicated instructions here, but Tile64 instructions
      //are not predicated

      else if(isDirectJump(I) &&
              !MCIDJ.isBranch() &&
              !MCIDJ.isCall() &&
              (DepType == SDep::Order)) {
        // Ignore Order dependences between unconditional direct branches
        // and non-control-flow instructions
        // do nothing
      }
      else if(MCIDI.isConditionalBranch() && (DepType != SDep::Data) &&
              (DepType != SDep::Output)) {
        // Ignore all dependences for jumps except for true and output
        // dependences
        // do nothing
      }

      // zero-reg can be targeted by multiple instructions
      else if(DepType == SDep::Output && DepReg != T64::Zero) {
        FoundSequentialDependence = true;
      }

      else if(DepType == SDep::Order && Dep.isArtificial()) {
        // Ignore artificial dependencies
        // do nothing
      }

      // Skip over anti-dependences. Two instructions that are
      // anti-dependent can share a packet
      else if(DepType != SDep::Anti) {
        FoundSequentialDependence = true;
      }
    }

    if(FoundSequentialDependence) {
      Dependence = true;
      return false;
    }
  }

  return true;
}

// isLegalToPruneDependencies
bool Tile64VLIWPacketizerList::isLegalToPruneDependencies(SUnit *SUI,
                                                          SUnit *SUJ) {
  MachineInstr *I = SUI->getInstr();
  assert(I && SUJ->getInstr() && "Unable to packetize null instruction!");

	if(Dependence) {
    //FIXME: check out what Hexagon backend does here...
    return false;
  }
  return true;
}

//===----------------------------------------------------------------------===//
//                         Private Helper Functions
//===----------------------------------------------------------------------===//

// isDirectJump - Return true if the instruction is a direct jump.
bool Tile64VLIWPacketizerList::isDirectJump(const MachineInstr *MI) const {
  return (MI->getOpcode() == T64::J);
}

// isTile64SoloInstruction - Return true if TSFlags:4 is 1.
bool Tile64VLIWPacketizerList::
isTile64SoloInstruction(const MachineInstr *MI) const {
  const uint64_t F = MI->getDesc().TSFlags;
  return ((F >> SoloPos) & SoloMask);
}

// isTile64LongInstruction - Return true if TSFlags:5 is 1.
bool Tile64VLIWPacketizerList::
isTile64LongInstruction(const MachineInstr *MI) const {
  const uint64_t F = MI->getDesc().TSFlags;
  return ((F >> LongPos) & LongMask);
}

// Tile64TypeOf - Return the appropriate value of Tile64Type.
Tile64Type Tile64VLIWPacketizerList::
Tile64TypeOf(const MachineInstr *MI) const {
  const uint64_t F = MI->getDesc().TSFlags;
	return (Tile64Type) ((F >> TypePos) & TypeMask);
}

// isTile64CtrInstruction - Return true if Tile64Type of MI is TypeCtr.
bool Tile64VLIWPacketizerList::
isTile64CtrInstruction(const MachineInstr *MI) const {
  return (Tile64TypeOf(MI) == TypeCtr);
}

// isTile64MemInstruction - Return true if Tile64Type of MI is TypeMeS or
// TypeMeL.
bool Tile64VLIWPacketizerList::
isTile64MemInstruction(const MachineInstr *MI) const {
  Tile64Type type = Tile64TypeOf(MI);
  return (type == TypeMeS || type == TypeMeL);
}

//===----------------------------------------------------------------------===//
//                         Public Constructor Functions
//===----------------------------------------------------------------------===//

FunctionPass *llvm::createTile64VLIWPacketizer() {
  return new Tile64VLIWPacketizer();
}

