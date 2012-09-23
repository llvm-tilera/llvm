//===- Tile64InstrInfo.cpp - Tile64 Instruction Information -----*- C++ -*-===//
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
// This file contains the Tile64 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "Tile64InstrInfo.h"
#include "Tile64Subtarget.h"
#include "Tile64MachineFunctionInfo.h"
#include "llvm/CodeGen/DFAPacketizer.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"

#define GET_INSTRINFO_CTOR
#define GET_INSTRINFO_ENUM
#include "Tile64GenInstrInfo.inc"
#include "Tile64GenDFAPacketizer.inc"

using namespace llvm;

Tile64InstrInfo::Tile64InstrInfo()
  : Tile64GenInstrInfo(T64::ADJCALLSTACKDOWN, T64::ADJCALLSTACKUP),
    RI(*this) {
}

//Loading and storing against FrameIndex cause register scavenging and storing
//address into that register, so they definitely have side-effects.  General
//load and store instructions on Tile Processor are not decidable whether they
//load from or store to a stack slot or not.
unsigned Tile64InstrInfo::isLoadFromStackSlot(const MachineInstr *MI,
                                              int &FrameIndex) const {
  return 0;
}

unsigned Tile64InstrInfo::isStoreToStackSlot(const MachineInstr *MI,
                                             int &FrameIndex) const {
  return 0;
}

static unsigned GetOppositeBranchOpcode(unsigned Opcode)
{
  switch(Opcode) {
    default: llvm_unreachable("Unknown branch code");
    case T64::BBNS:    return T64::BBST;
    case T64::BBNST:   return T64::BBS;
    case T64::BBS:     return T64::BBNST;
    case T64::BBST:    return T64::BBNS;
    case T64::BGEZ_nt: return T64::BLZ_t;
    case T64::BGEZ_t:  return T64::BLZ_nt;
    case T64::BGZ_nt:  return T64::BLEZ_t;
    case T64::BGZ_t:   return T64::BLEZ_nt;
    case T64::BLEZ_nt: return T64::BGZ_t;
    case T64::BLEZ_t:  return T64::BGZ_nt;
    case T64::BLZ_nt:  return T64::BGEZ_t;
    case T64::BLZ_t:   return T64::BGEZ_nt;
    case T64::BNZ_nt:  return T64::BZ_t;
    case T64::BNZ_t:   return T64::BZ_nt;
    case T64::BZ_nt:   return T64::BNZ_t;
    case T64::BZ_t:    return T64::BNZ_nt;
  }
}


bool Tile64InstrInfo::AnalyzeBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *&TBB,
                                    MachineBasicBlock *&FBB,
                                    SmallVectorImpl<MachineOperand> &Cond,
                                    bool AllowModify) const {
  TBB = FBB = 0;
  MachineBasicBlock::iterator I = MBB.end();
  MachineBasicBlock::iterator UnCondBrIter = MBB.end();
  while (I != MBB.begin()) {
    --I;

    if (I->isDebugValue())
      continue;

    //When we cannot understand a terminator, we are done
    if (!isUnpredicatedTerminator(I))
      break;

    //Terminator is not a branch
    if (!I->getDesc().isBranch())
      return true;

    //Handle Unconditional branches
    if (I->getOpcode() == T64::J) {
      UnCondBrIter = I;
      TBB = I->getOperand(0).getMBB();
      FBB = 0;
      Cond.clear();

      if (!AllowModify)
        continue;

      while (llvm::next(I) != MBB.end())
        llvm::next(I)->eraseFromParent();

      if (MBB.isLayoutSuccessor(TBB)) {
        TBB = 0;
        I->eraseFromParent();
        I = MBB.end();
        UnCondBrIter = MBB.end();
      }
      continue;
    }

    unsigned Opcode = I->getOpcode();
    switch(Opcode) {
      default: return true; //Unknown Opcode
      case T64::BBNS:    case T64::BBNST:   case T64::BBS:    case T64::BBST:
      case T64::BGEZ_nt: case T64::BGEZ_t:  case T64::BGZ_nt: case T64::BGZ_t:
      case T64::BLEZ_t:  case T64::BLEZ_nt: case T64::BLZ_t:  case T64::BLZ_nt:
      case T64::BNZ_t:   case T64::BNZ_nt:  case T64::BZ_t:   case T64::BZ_nt:
      ; //in the case of conditional branches, we can continue
    }

    if (Cond.empty()) {
      unsigned condReg = I->getOperand(0).getReg();
      MachineBasicBlock *TargetBB = I->getOperand(1).getMBB();
      if (AllowModify && UnCondBrIter != MBB.end() &&
          MBB.isLayoutSuccessor(TargetBB)) {

        //Transform the code
        //
        //    bCC L1
        //    j L2
        // L1:
        //    ..
        // L2:
        //
        // into
        //
        //   bnCC L2
        // L1:
        //   ...
        // L2:
        //
        Opcode = GetOppositeBranchOpcode(Opcode);
        MachineBasicBlock::iterator OldInst = I;
        BuildMI(MBB, UnCondBrIter, MBB.findDebugLoc(I), get(Opcode))
          .addReg(condReg).addMBB(UnCondBrIter->getOperand(0).getMBB());
        OldInst->eraseFromParent();
        UnCondBrIter->eraseFromParent();

        UnCondBrIter = MBB.end();
        I = MBB.end();

        Cond.push_back(MachineOperand::CreateImm(Opcode));
        Cond.push_back(MachineOperand::CreateReg(condReg, false));
        continue;
      }
      FBB = TBB;
      TBB = TargetBB;
      Cond.push_back(MachineOperand::CreateImm(Opcode));
      Cond.push_back(MachineOperand::CreateReg(condReg, false));
      continue;
    }
    //FIXME: Handle subsequent conditional branches
    //For now, we can't handle multiple conditional branches
    return true;
  }
  return false;
}

unsigned Tile64InstrInfo::RemoveBranch(MachineBasicBlock &MBB) const
{
  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;
  while (I != MBB.begin()) {
    --I;

    if (I->isDebugValue())
      continue;

    unsigned Opcode = I->getOpcode();
    switch(Opcode) {
      default: return Count; //Not a branch
      case T64::BBNS:    case T64::BBNST:   case T64::BBS:    case T64::BBST:
      case T64::BGEZ_nt: case T64::BGEZ_t:  case T64::BGZ_nt: case T64::BGZ_t:
      case T64::BLEZ_t:  case T64::BLEZ_nt: case T64::BLZ_t:  case T64::BLZ_nt:
      case T64::BNZ_t:   case T64::BNZ_nt:  case T64::BZ_t:   case T64::BZ_nt:
      case T64::J: ; //in the case of any branch instruction, we can continue
    }
    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }
  return Count;
}

unsigned
Tile64InstrInfo::InsertBranch(MachineBasicBlock &MBB,MachineBasicBlock *TBB,
                              MachineBasicBlock *FBB,
                              const SmallVectorImpl<MachineOperand> &Cond,
                              DebugLoc DL) const {
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 2 || Cond.size() == 0) &&
         "Tile64 branch conditions should have two components!");

  if (Cond.empty()) {
    assert(!FBB && "Unconditional branch with multiple successors!");
    BuildMI(&MBB, DL, get(T64::J)).addMBB(TBB);
    return 1;
  }

  //Conditional branch
  unsigned Opcode = Cond[0].getImm();
  unsigned condReg = Cond[1].getReg();

  BuildMI(&MBB, DL, get(Opcode)).addReg(condReg).addMBB(TBB);
  if (!FBB)
    return 1;

  BuildMI(&MBB, DL, get(T64::J)).addMBB(FBB);
  return 2;
}

void Tile64InstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator I, DebugLoc DL,
                                  unsigned DestReg, unsigned SrcReg,
                                  bool KillSrc) const {
  BuildMI(MBB, I, DL, get(T64::MOVE), DestReg).
                                 addReg(SrcReg, getKillRegState(KillSrc));
}

void Tile64InstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                    unsigned SrcReg, bool isKill, int FI,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  BuildMI(MBB, I, DL, get(T64::SW)).addFrameIndex(FI).
                                    addReg(SrcReg, getKillRegState(isKill));
}

void Tile64InstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                     unsigned DestReg, int FI,
                     const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  BuildMI(MBB, I, DL, get(T64::LW)).addReg(DestReg).addFrameIndex(FI);
}

DFAPacketizer *Tile64InstrInfo::
CreateTargetScheduleState(const TargetMachine *TM,
                          const ScheduleDAG *DAG) const {
  const InstrItineraryData *II = TM->getInstrItineraryData();
  return TM->getSubtarget<Tile64GenSubtargetInfo>().createDFAPacketizer(II);
}

bool Tile64InstrInfo::isSchedulingBoundary(const MachineInstr *MI,
                                           const MachineBasicBlock *MBB,
                                           const MachineFunction &MF) const {
	//Implementation from HexagonInstrInfo.

	// Debug info is never a scheduling boundary. It's necessary to be explicit
	// due to the special treatment of IT instructions below, otherwise a
	// dbg_value followed by an IT will result in the IT instruction being
	// considered a scheduling hazard, which is wrong. It should be the actual
	// instruction preceding the dbg_value instruction(s), just like it is when
	// debug info is not present.
  if (MI->isDebugValue())
    return false;
 
  // Terminators and labels can't be scheduled around.
  if (MI->getDesc().isTerminator() || MI->isLabel() || MI->isInlineAsm()) {
    return true;
  }

  return false;
}

unsigned Tile64InstrInfo::getGlobalBaseReg(MachineFunction *MF) const {
  Tile64MachineFunctionInfo *FI = MF->getInfo<Tile64MachineFunctionInfo>();
  unsigned GlobalBaseReg = FI->getGlobalBaseReg();
  if(GlobalBaseReg != 0)
    return GlobalBaseReg;

  // Insert the set of GlobalBaseReg into the first MBB of the function
  MachineBasicBlock &FirstMBB = MF->front();
  MachineBasicBlock::iterator MBBI = FirstMBB.begin();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();

  GlobalBaseReg = RegInfo.createVirtualRegister(&T64::T64GPRFRegClass);
  DebugLoc DL;

  BuildMI(FirstMBB, MBBI, DL, get(T64::GBR), GlobalBaseReg);
  FI->setGlobalBaseReg(GlobalBaseReg);
  return GlobalBaseReg;
}

