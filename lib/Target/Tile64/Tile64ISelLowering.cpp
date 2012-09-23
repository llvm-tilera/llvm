//===-- Tile64ISelLowering.cpp - Tile64 DAG Lowering Implementation -------===//
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
//
// This file implements the interfaces that Tile64 uses to lower LLVM code into
// a selection DAG.
//
//===----------------------------------------------------------------------===//

#include "Tile64ISelLowering.h"
#include "Tile64TargetMachine.h"
#include "Tile64MachineFunctionInfo.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

//===----------------------------------------------------------------------===//
// Implementation of Tile64 CallingConv custom function.
//===----------------------------------------------------------------------===//

static const uint16_t ArgRegs[] = {
  T64::R0, T64::R1, T64::R2, T64::R3, T64::R4,
  T64::R5, T64::R6, T64::R7, T64::R8, T64::R9,
};

static const unsigned NumArgRegs = array_lengthof(ArgRegs);

static bool CC_Tile64_Assign(unsigned &ValNo, MVT &ValVT, MVT &LocVT,
                             CCValAssign::LocInfo &LocInfo,
                             ISD::ArgFlagsTy &ArgFlags,
                             CCState &State) {
  //NOTE: this function must be executed single-threaded because of lastSplitValNo.

  //Assume that small varargs are promoted correctly to bigger values
  //on higher level, which is done e.g. by clang.

  assert((!ArgFlags.isSplit() || ArgFlags.getOrigAlign() == 8)
         && "Only double-word primitives are to be split and they must be double-word aligned.");

  //split arg number must be stored in order to second parts can be
  //distinguished from a i8 argument.
  static unsigned lastSplitValNo;
  if(ValNo == 0)
    lastSplitValNo = std::numeric_limits<unsigned>::max();//should be a good choice

  unsigned RegNum = State.getFirstUnallocated(ArgRegs, NumArgRegs);
  bool isByVal = ArgFlags.isByVal();
  bool isSplit = ArgFlags.isSplit();

  unsigned SizeInBytes = isByVal ? ArgFlags.getByValSize()
                                 : ValVT.getSizeInBits() >> 3;
  unsigned SizeInRegs = isSplit ? 2 : SizeInBytes >> 2;

  unsigned Align = isByVal ? ArgFlags.getByValAlign()
                           : ArgFlags.getOrigAlign();
  assert((Align <= 8 && (Align % 2 == 0 || Align == 1)) &&
         "Something wrong with alignment!");

  unsigned AlignRegMask = Align >> 3 ? (~0x1u) : (~0x0u);
  
  if(SizeInRegs > NumArgRegs || RegNum > ((NumArgRegs - SizeInRegs) & AlignRegMask)) {
    //Before allocating whole thing in stack, pad remaining registers if any
    for(; RegNum < NumArgRegs; ++RegNum)
      State.AllocateReg(ArgRegs[RegNum]);

    if(!isByVal) {
      //handle splitting
      if(isSplit)
        lastSplitValNo = ValNo;
      else if(lastSplitValNo != (ValNo - 1))
        //need to store in original size if it's not a second part of a split
        SizeInBytes = Align;
   }

    unsigned Offset = State.AllocateStack(SizeInBytes, Align);
    State.addLoc(CCValAssign::getCustomMem(ValNo, ValVT, Offset,
                                           LocVT, LocInfo));
  } else {
    //There is enough space for passing the argument entirely in register.
    if(Align == 8 && (RegNum % 2 == 1))
      State.AllocateReg(ArgRegs[RegNum++]);

    if(!isByVal)
      SizeInRegs = 1; //double-word primitives are lowered into two i32 args.

    for(unsigned i = 0; i < SizeInRegs; ++i) {
      assert((RegNum + i < NumArgRegs) && "Struct doesn't fit in regs!");
      unsigned Reg = ArgRegs[RegNum + i];
      State.AllocateReg(Reg);
      State.addLoc(CCValAssign::getCustomReg(ValNo, ValVT, Reg,
                                             LocVT, LocInfo));
    }
  }

  return true;
}

#include "Tile64GenCallingConv.inc"


//===----------------------------------------------------------------------===//
// Helper functions implementing features of Tile64TargetLowering
//===----------------------------------------------------------------------===//

SDValue Tile64TargetLowering::LowerGlobalAddress(SDValue Op,
                                                 SelectionDAG &DAG) const {
  const GlobalValue *GV = cast<GlobalAddressSDNode>(Op)->getGlobal();
  DebugLoc DL = Op.getDebugLoc();
  SDValue GA = DAG.getTargetGlobalAddress(GV, DL, MVT::i32);
  SDValue Addr;

  if(getTargetMachine().getRelocationModel() != Reloc::PIC_) {
    SDValue Lo = DAG.getNode(T64ISD::MOVELO, DL, MVT::i32, GA);
    Addr = DAG.getNode(T64ISD::ADDHI, DL, MVT::i32, Lo, GA);
  } else {
    bool GloballyRelocatable = (GV->getRelocationInfo() == 2);
    SDValue GBR = DAG.getNode(T64ISD::GLOBAL_BASE_REG, DL, MVT::i32);
    SDValue TA = !GloballyRelocatable ? GA :
                 DAG.getTargetExternalSymbol("_GLOBAL_OFFSET_TABLE_", MVT::i32);

    SDValue Lo = DAG.getNode(T64ISD::ADDLO_PIC, DL, MVT::i32, GBR, TA);
    Addr = DAG.getNode(T64ISD::ADDHI_PIC, DL, MVT::i32, Lo, TA);

    if(GloballyRelocatable) {
			SDValue Chain = DAG.getEntryNode();
      SDValue GOTAddr = DAG.getNode(T64ISD::ADDLI_GOT, DL, MVT::i32, Addr, GA);
      Addr = DAG.getLoad(MVT::i32, DL, Chain, GOTAddr,
                         MachinePointerInfo(), false, false, true, 0); //GOT is loaded before executing the program...
    }
  }

  return Addr;
}

SDValue Tile64TargetLowering::LowerConstantPool(SDValue Op,
                                                SelectionDAG &DAG) const {
  ConstantPoolSDNode *N = cast<ConstantPoolSDNode>(Op);
  DebugLoc DL = Op.getDebugLoc();
  const Constant *C = N->getConstVal();
  SDValue CP = DAG.getTargetConstantPool(C, MVT::i32, N->getAlignment());
  SDValue Addr;
	
  if(getTargetMachine().getRelocationModel() != Reloc::PIC_) {
    SDValue Lo = DAG.getNode(T64ISD::MOVELO, DL, MVT::i32, CP);
    Addr = DAG.getNode(T64ISD::ADDHI, DL, MVT::i32, Lo, CP);
  } else {
    SDValue GBR = DAG.getNode(T64ISD::GLOBAL_BASE_REG, DL, getPointerTy());
    SDValue Lo = DAG.getNode(T64ISD::ADDLO_PIC, DL, MVT::i32, GBR, CP);
    Addr = DAG.getNode(T64ISD::ADDHI_PIC, DL, MVT::i32, Lo, CP);
  }
  return Addr;
}

SDValue Tile64TargetLowering::LowerVASTART(SDValue Op, SelectionDAG &DAG) const{

  //VAList is a mere pointer, and we assume it's already allocated.

  MachineFunction &MF = DAG.getMachineFunction();
  Tile64MachineFunctionInfo *FuncInfo = MF.getInfo<Tile64MachineFunctionInfo>();

  DebugLoc DL = Op.getDebugLoc();
  SDValue Chain = Op.getOperand(0);
  SDValue VAListFI = Op.getOperand(1);
  const Value *VAListS = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();

  int argOffset = FuncInfo->getVarArgsOffset();

  SDValue VAList = DAG.getNode(ISD::ADD, DL, getPointerTy(),
                               DAG.getRegister(T64::FramePointer, MVT::i32),
                               DAG.getIntPtrConstant(argOffset));
  return DAG.getStore(Chain, DL, VAList, VAListFI,
                      MachinePointerInfo(VAListS), false, false, 0);
}

SDValue Tile64TargetLowering::LowerVAARG(SDValue Op, SelectionDAG &DAG) const {
  //NOTE that only primitive values can be passed as vararg.
  SDValue Chain = Op.getOperand(0);
  SDValue VAListPtr = Op.getOperand(1);
  const Value *VAListS = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
  const unsigned Align = Op.getNode()->getConstantOperandVal(3);
  EVT VT = Op.getNode()->getValueType(0);
  DebugLoc DL = Op.getDebugLoc();

  SDValue Const4 = DAG.getIntPtrConstant(4); //adjustment
  SDValue Mask   = DAG.getIntPtrConstant(7);

  SDValue VAList = DAG.getLoad(MVT::i32, DL, Chain, VAListPtr,
                               MachinePointerInfo(VAListS),
                               false, false, false, 0);
  Chain = VAList.getValue(1);

  if(Align == 8) {
    //first part of split i64 and f64 must be double-word aligned.
    SDValue MaskedAddr = DAG.getNode(ISD::AND, DL, MVT::i32, VAList, Mask);
    SDValue AdjVAList = DAG.getNode(ISD::ADD, DL, MVT::i32, VAList, Const4);
    SDValue CC = DAG.getSetCC(DL, MVT::i32, MaskedAddr,
                              DAG.getConstant(0, MVT::i32), ISD::SETEQ);
    VAList = DAG.getNode(ISD::SELECT, DL, MVT::i32, CC, VAList, AdjVAList);
  }

  MachineFunction &MF = DAG.getMachineFunction();
  Tile64MachineFunctionInfo *FuncInfo = MF.getInfo<Tile64MachineFunctionInfo>();

  //if we just ran out of spilled regs, we need to adjust the address.
  //second part of a split value can't be stored on reg/stack boundary,
  //and we can run out of spilled regs only if there were spilled regs at all.
  if(Align != 0 && FuncInfo->getVarArgsOffset() < 0) {
    SDValue AdjVAList = DAG.getNode(ISD::ADD, DL, MVT::i32, VAList,
                            DAG.getIntPtrConstant(FuncInfo->getArgAreaOffset()));
    SDValue CC = DAG.getSetCC(DL, MVT::i32, VAList,
                              DAG.getRegister(T64::FramePointer, MVT::i32),
                              ISD::SETEQ);
    VAList = DAG.getNode(ISD::SELECT, DL, MVT::i32, CC, AdjVAList, VAList);
  }

  //store the incremented VAList. next location is always at 4 bytes ahead as
  //double-word sized values are split.
  SDValue NewVAList = DAG.getNode(ISD::ADD, DL, MVT::i32, VAList, Const4);
  Chain = DAG.getStore(Chain, DL, NewVAList, VAListPtr,
                       MachinePointerInfo(VAListS), false, false, 0);

  return DAG.getLoad(VT, DL, Chain, VAList, MachinePointerInfo(),
                     false, false, false, 0);
}

SDValue Tile64TargetLowering::LowerDYNAMIC_STACKALLOC(SDValue Op,
                                                      SelectionDAG &DAG) const {
  SDValue Chain = Op.getOperand(0);  // Legalize the chain.
  SDValue Size  = Op.getOperand(1);  // Legalize the size.
  DebugLoc DL = Op.getDebugLoc();

  unsigned SPReg = T64::StackPointer;

  //compute and set new sp
  SDValue SP = DAG.getCopyFromReg(Chain, DL, SPReg, MVT::i32);
  Chain = SP.getValue(1);
  SDValue NewSP = DAG.getNode(ISD::SUB, DL, MVT::i32, SP, Size);
  Chain = DAG.getCopyToReg(Chain, DL, SPReg, NewSP);

  //store fp to [NewSP+4] if needed
  Chain = DAG.getNode(T64ISD::STORE_FP, DL, MVT::Other, Chain);

  //return the address to the newly allocated area, which is above of arg area
  SDValue NewVal = DAG.getNode(T64ISD::NEWSLOT_ADDR, DL, MVT::i32, NewSP);
  SDValue Ops[2] = { NewVal, Chain };
  return DAG.getMergeValues(Ops, 2, DL);
}

SDValue Tile64TargetLowering::LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG,
                                             bool isFrameAddrTaken) const {
  MachineFrameInfo *MFI = DAG.getMachineFunction().getFrameInfo();
  if(isFrameAddrTaken)
    MFI->setFrameAddressIsTaken(true);

  EVT VT = Op.getValueType();
  DebugLoc DL = Op.getDebugLoc();
  unsigned FrameReg = T64::FramePointer;

  uint64_t depth = Op.getConstantOperandVal(0);

  SDValue Chain = DAG.getEntryNode();
  SDValue FrameAddr = DAG.getCopyFromReg(Chain, DL, FrameReg, VT);
  for(; depth != 0; --depth) {
    SDValue Ptr = DAG.getNode(ISD::ADD, DL, MVT::i32,
                              FrameAddr, DAG.getConstant(4, MVT::i32));
    FrameAddr = DAG.getLoad(MVT::i32, DL, Chain, Ptr,
                            MachinePointerInfo(), false, false, false, 0);
  }
  return FrameAddr;
}

SDValue Tile64TargetLowering::LowerRETURNADDR(SDValue Op,
                                              SelectionDAG &DAG) const {
  MachineFrameInfo *MFI = DAG.getMachineFunction().getFrameInfo();
  MFI->setReturnAddressIsTaken(true);

  DebugLoc DL = Op.getDebugLoc();

  SDValue FrameAddr = LowerFRAMEADDR(Op, DAG, false);
  SDValue Chain = FrameAddr.getValue(1);

  SDValue RetAddr = DAG.getLoad(MVT::i32, DL, Chain, FrameAddr,
                                MachinePointerInfo(), false, false, false, 0);

  return RetAddr;
}

MachineBasicBlock *Tile64TargetLowering::
EmitSELECTWithCustomInserter(MachineInstr *MI, MachineBasicBlock *MBB) const {
  const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
  MachineRegisterInfo &MRI = MBB->getParent()->getRegInfo();
  DebugLoc DL = MI->getDebugLoc();

  unsigned Dst = MI->getOperand(0).getReg();
  unsigned CC = MI->getOperand(1).getReg();
  unsigned TV = MI->getOperand(2).getReg();
  unsigned FV = MI->getOperand(3).getReg();

  unsigned VirtTV = MRI.createVirtualRegister(&T64::T64GPRFRegClass);
  unsigned VirtFV = MRI.createVirtualRegister(&T64::T64GPRFRegClass);

  MachineBasicBlock *thisMBB = MBB;
  const BasicBlock *LLVM_BB = MBB->getBasicBlock();
  MachineFunction *MF = MBB->getParent();
  MachineBasicBlock *trueMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *falseMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *endMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  
  MachineFunction::iterator MBBIter = MBB;
  ++MBBIter;

  //Insert the new BB
  MF->insert(MBBIter, trueMBB);
  MF->insert(MBBIter, falseMBB);
  MF->insert(MBBIter, endMBB);

  //Transfer the remainder of MBB and its successor edges to endMBB.
  endMBB->splice(endMBB->begin(), thisMBB,
                 llvm::next(MachineBasicBlock::iterator(MI)),
                 thisMBB->end());
  endMBB->transferSuccessorsAndUpdatePHIs(thisMBB);

  //Arrange successor relation
  thisMBB->addSuccessor(trueMBB);
  thisMBB->addSuccessor(falseMBB);
  trueMBB->addSuccessor(endMBB);
  falseMBB->addSuccessor(endMBB);

  //
  // thisMBB
  //

  //If the condition is false, jump to falseMBB
  BuildMI(thisMBB, DL, TII->get(T64::BZ_nt)).addReg(CC).addMBB(falseMBB);

  //Else jump to trueMBB
  BuildMI(thisMBB, DL, TII->get(T64::J)).addMBB(trueMBB);

  //
  // trueMBB
  //

  //move true-value into virtual destination, then jump to endMBB
  BuildMI(trueMBB, DL, TII->get(T64::MOVE), VirtTV).addReg(TV);
  BuildMI(trueMBB, DL, TII->get(T64::J)).addMBB(endMBB);

  //
  // falseMBB
  //

  //move false-value into virtual destination, then jump to endMBB
  BuildMI(falseMBB, DL, TII->get(T64::MOVE), VirtFV).addReg(FV);
  BuildMI(falseMBB, DL, TII->get(T64::J)).addMBB(endMBB);

  //
  // endMBB
  //

  //Emit phi-instruction to select the value into real destination
  BuildMI(*endMBB, endMBB->begin(), DL, TII->get(T64::PHI), Dst).
                                        addReg(VirtTV).addMBB(trueMBB).
                                        addReg(VirtFV).addMBB(falseMBB);

  //Erase the pseudo instruction.
  MI->eraseFromParent();

  return endMBB;
}

MachineBasicBlock *Tile64TargetLowering::
EmitSTORE_FPWithCustomInserter(MachineInstr *MI, MachineBasicBlock *MBB) const {
  const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
  MachineRegisterInfo &MRI = MBB->getParent()->getRegInfo();
  MachineFrameInfo *MFI = MBB->getParent()->getFrameInfo();
  DebugLoc DL = MI->getDebugLoc();
  MachineBasicBlock::iterator MBBI(MI);

  if(MFI->hasCalls()) {
    unsigned Reg = MRI.createVirtualRegister(&T64::T64GPRFRegClass);
    BuildMI(*MBB, MBBI, DL, TII->get(T64::ADDI), Reg).addReg(T64::StackPointer)
                                                     .addImm(4);
    BuildMI(*MBB, MBBI, DL, TII->get(T64::SW)).addReg(Reg)
                                              .addReg(T64::FramePointer);
  }

  MI->eraseFromParent();
  return MBB;
}

//===----------------------------------------------------------------------===//
// TargetLowering Implementation
//===----------------------------------------------------------------------===//

Tile64TargetLowering::Tile64TargetLowering(TargetMachine &TM)
  : TargetLowering(TM, new TargetLoweringObjectFileELF()) {

  //TODO: vector register types are commented out until SIMD instructions get defined properly.
  // Set up the register classes.
  addRegisterClass(MVT::i32, &T64::T64GPRFRegClass);
  //addRegisterClass(MVT::v2i16, &T64::T64GPRFRegClass);
  //addRegisterClass(MVT::v4i8, &T64::T64GPRFRegClass);

  computeRegisterProperties();

  //
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent);
  setStackPointerRegisterToSaveRestore(T64::StackPointer);
  setIntDivIsCheap(false);
  setPow2DivIsCheap(true);

  //TODO: scheduling modes: None, Source, RegPressure, Hybrid, ILP, VLIW
  setSchedulingPreference(Sched::VLIW);

  //LoadExtActions
  setLoadExtAction(ISD::EXTLOAD, MVT::i1, Promote);
  setLoadExtAction(ISD::ZEXTLOAD, MVT::i1, Promote);
  setLoadExtAction(ISD::SEXTLOAD, MVT::i1, Promote);

  //TruncStoreActions
  setTruncStoreAction(MVT::i32, MVT::i1, Expand);

  // Custom legalize GlobalAddress nodes into LO/HI parts.
  setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);
  setOperationAction(ISD::GlobalTLSAddress, MVT::i32, Custom);
  setOperationAction(ISD::ConstantPool, MVT::i32, Custom);
  //TODO: BlockAddress?

  // Tile64 doesn't have sext_inreg, replace them with shl/sra
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1 , Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8 , Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16, Expand);

  setOperationAction(ISD::ADDC, MVT::i32, Expand);
  setOperationAction(ISD::ADDE, MVT::i32, Expand);
  setOperationAction(ISD::SUBC, MVT::i32, Expand);
  setOperationAction(ISD::SUBE, MVT::i32, Expand);

  // Tile64 has no DIV or REM or DIVREM operations.
  setOperationAction(ISD::UDIV, MVT::i32, Expand);
  setOperationAction(ISD::SDIV, MVT::i32, Expand);
  setOperationAction(ISD::UREM, MVT::i32, Expand);
  setOperationAction(ISD::SREM, MVT::i32, Expand);
  setOperationAction(ISD::SDIVREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i32, Expand);

  setOperationAction(ISD::UMUL_LOHI, MVT::i32, Expand);
  setOperationAction(ISD::SMUL_LOHI, MVT::i32, Expand);

  setOperationAction(ISD::SHL_PARTS, MVT::i32, Expand);
  setOperationAction(ISD::SRA_PARTS, MVT::i32, Expand);
  setOperationAction(ISD::SRL_PARTS, MVT::i32, Expand);

  setOperationAction(ISD::BITCAST, MVT::i32, Expand);

	// cttz/ctlz
  setOperationAction(ISD::CTTZ, MVT::i8, Promote);
  setOperationAction(ISD::CTTZ, MVT::i16, Promote);
  setOperationAction(ISD::CTLZ, MVT::i8, Promote);
  setOperationAction(ISD::CTLZ, MVT::i16, Promote);
  setOperationAction(ISD::CTTZ_ZERO_UNDEF, MVT::i8, Promote);
	AddPromotedToType(ISD::CTTZ_ZERO_UNDEF, MVT::i8, MVT::i32);
  setOperationAction(ISD::CTTZ_ZERO_UNDEF, MVT::i16, Promote);
	AddPromotedToType(ISD::CTTZ_ZERO_UNDEF, MVT::i16, MVT::i32);
	setOperationAction(ISD::CTTZ_ZERO_UNDEF, MVT::i32, Expand);
  setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i8, Promote);
	AddPromotedToType(ISD::CTLZ_ZERO_UNDEF, MVT::i8, MVT::i32);
  setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i16, Promote);
	AddPromotedToType(ISD::CTLZ_ZERO_UNDEF, MVT::i16, MVT::i32);
	setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i32, Expand);

  // Tile64 has no select or select_cc: expand to set_cc.
  setOperationAction(ISD::SELECT_CC, MVT::Other, Expand);

  // Til64 doesn't have BR_CC either, it has BRCOND.
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  setOperationAction(ISD::BR_CC, MVT::Other, Expand);


  setOperationAction(ISD::FRAMEADDR, MVT::Other, Custom);
  setOperationAction(ISD::RETURNADDR, MVT::Other, Custom);

  setOperationAction(ISD::VASTART           , MVT::Other, Custom);
  setOperationAction(ISD::VAARG             , MVT::Other, Custom);

  // Use the default implementation.
  setOperationAction(ISD::VACOPY            , MVT::Other, Expand);
  setOperationAction(ISD::VAEND             , MVT::Other, Expand);
  setOperationAction(ISD::STACKSAVE         , MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE      , MVT::Other, Expand);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i32  , Custom);

  // No debug info support yet.
  setOperationAction(ISD::EH_LABEL, MVT::Other, Expand);

  setMinFunctionAlignment(8);
}

const char *Tile64TargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch (Opcode) {
  default: return 0;
  case T64ISD::MOVELO: return "T64ISD::MOVELO";
  case T64ISD::ADDLO_PIC: return "T64ISD::ADDLO_PIC";
  case T64ISD::ADDHI: return "T64ISD::ADDHI";
  case T64ISD::ADDHI_PIC: return "T64ISD::ADDHI_PIC";
  case T64ISD::ADDLI_GOT: return "T64ISD::ADDLI_GOT";
  case T64ISD::JAL: return "T64ISD::JAL";
  case T64ISD::JALR: return "T64ISD::JALR";
  case T64ISD::JRP: return "T64ISD::JRP";
  case T64ISD::CALL: return "T64ISD::CALL";
  case T64ISD::PICCALL: return "T64ISD::PICCALL";
  case T64ISD::PREFETCH: return "T64ISD::PREFETCH";
  case T64ISD::FENCE: return "T64ISD::FENCE";
  case T64ISD::MEMBARRIER: return "T64ISD::MEMBARRIER";
  case T64ISD::MULHH_SS: return "T64ISD::MULHH_SS";
  case T64ISD::MULHH_SU: return "T64ISD::MULHH_SU";
  case T64ISD::MULHH_UU: return "T64IDF::MULHH_UU";
  case T64ISD::MULHHA_SS: return "T64ISD::MULHHA_SS";
  case T64ISD::MULHHA_SU: return "T64ISD::MULHHA_SU";
  case T64ISD::MULHHA_UU: return "T64ISD::MULHHA_UU";
  case T64ISD::MULHHSA_UU: return "T64ISD::MULHHSA_UU";
  case T64ISD::MULHL_SS: return "T64ISD::MULHL_SS";
  case T64ISD::MULHL_SU: return "T64ISD::MULHL_SU";
  case T64ISD::MULHL_US: return "T64ISD::MULHL_US";
  case T64ISD::MULHL_UU: return "T64ISD::MULHL_UU";
  case T64ISD::MULHLA_SS: return "T64ISD::MULHLA_SS";
  case T64ISD::MULHLA_SU: return "T64ISD::MULHLA_SU";
  case T64ISD::MULHLA_US: return "T64ISD::MULHLA_US";
  case T64ISD::MULHLA_UU: return "T64ISD::MULHLA_UU";
  case T64ISD::MULHLSA_UU: return "T64ISD::MULHLSA_UU";
  case T64ISD::MULLL_SS: return "T64ISD::MULLL_SS";
  case T64ISD::MULLL_SU: return "T64ISD::MULLL_SU";
  case T64ISD::MULLL_UU: return "T64ISD::MULLL_UU";
  case T64ISD::MULLLA_SS: return "T64ISD::MULLLA_SS";
  case T64ISD::MULLLA_SU: return "T64ISD::MULLLA_SU";
  case T64ISD::MULLLA_UU: return "T64ISD::MULLLA_UU";
  case T64ISD::MULLLSA_UU: return "T64ISD::MULLLSA_UU";
  case T64ISD::FNOP: return "T64ISD::FNOP";
  case T64ISD::NOP: return "T64ISD::NOP";
  case T64ISD::BITX: return "T64ISD::BITX";
  case T64ISD::CRC32_32: return "T64ISD::CRC32_32";
  case T64ISD::CRC32_8: return "T64ISD::CRC32_8";
  case T64ISD::MM: return "T64ISD::MM";
  case T64ISD::GLOBAL_BASE_REG: return "T64ISD::GLOBAL_BASE_REG";
  case T64ISD::STORE_FP: return "T64ISD::STORE_FP";
  case T64ISD::NEWSLOT_ADDR: return "T64ISD::NEWSLOT_ADDR";
  }
}

SDValue Tile64TargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Should not custom lower this!");
  case ISD::GlobalTLSAddress:
    llvm_unreachable("TLS not implemented for Tile64.");
  case ISD::GlobalAddress:      return LowerGlobalAddress(Op, DAG);
  case ISD::ConstantPool:       return LowerConstantPool(Op, DAG);
  case ISD::VASTART:            return LowerVASTART(Op, DAG);
  case ISD::VAARG:              return LowerVAARG(Op, DAG);
  case ISD::DYNAMIC_STACKALLOC: return LowerDYNAMIC_STACKALLOC(Op, DAG);
  case ISD::RETURNADDR:         return LowerRETURNADDR(Op, DAG);
  case ISD::FRAMEADDR:          return LowerFRAMEADDR(Op, DAG, true);
  }
}


MachineBasicBlock *Tile64TargetLowering::
EmitInstrWithCustomInserter(MachineInstr *MI, MachineBasicBlock *MBB) const {
  switch(MI->getOpcode()) {
    default: assert(0 && "Unexpected instr type to insert!");
    case T64::SELECT: return EmitSELECTWithCustomInserter(MI, MBB);
    case T64::STORE_FP: return EmitSTORE_FPWithCustomInserter(MI, MBB);
  }
}

SDValue
Tile64TargetLowering::LowerFormalArguments(SDValue Chain,
                                           CallingConv::ID CallConv, bool isVarArg,
                                           const SmallVectorImpl<ISD::InputArg>
                                             &Ins,
                                           DebugLoc DL, SelectionDAG &DAG,
                                           SmallVectorImpl<SDValue> &InVals)
                                             const {

  MachineFunction &MF = DAG.getMachineFunction();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  Tile64MachineFunctionInfo *FuncInfo = MF.getInfo<Tile64MachineFunctionInfo>();

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
		 getTargetMachine(), ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_Tile64);

  const unsigned StackOffset = DAG.getMachineFunction().
                               getInfo<Tile64MachineFunctionInfo>()->
                               getArgAreaOffset();

  SmallVector<SDValue, 8> MemOpChains;

  for (unsigned i = 0, realArgIdx = 0, e = ArgLocs.size();
       i != e; ++i, ++realArgIdx) {
    CCValAssign &VA = ArgLocs[i];
    ISD::ArgFlagsTy Flags = Ins[realArgIdx].Flags;
    bool isByVal = Flags.isByVal();

    SDValue ArgValue;
    if (VA.isRegLoc()) {
      if(isByVal) {
        unsigned SizeInBytes = Flags.getByValSize();
        unsigned RegNum = SizeInBytes >> 2;
        assert(RegNum != 0 && "0-reg structs should have been eliminated.");

        int Offset = -((NumArgRegs - (VA.getLocReg() - T64::R0)) * 4);

        int FI = MFI->CreateFixedObject(SizeInBytes, Offset, false);
        ArgValue = DAG.getFrameIndex(FI, getPointerTy());

        SDValue FramePtr = DAG.getRegister(T64::FramePointer, MVT::i32);
        SDValue PtrOff = DAG.getIntPtrConstant(Offset);
        SDValue Dst = DAG.getNode(ISD::ADD, DL, MVT::i32, FramePtr, PtrOff);

        for(unsigned j = 0; j < RegNum; ++j) {
          VA = ArgLocs[i + j];
          assert(VA.isRegLoc() && "Something wrong with byval in regs.");
          unsigned Reg = MF.addLiveIn(VA.getLocReg(), &T64::T64GPRFRegClass);
          SDValue Arg = DAG.getCopyFromReg(DAG.getRoot(), DL, Reg, MVT::i32);
          MemOpChains.push_back(DAG.getStore(Chain, DL, Arg, Dst,
                                             MachinePointerInfo(),
                                             false, false, 0));
          if(j != RegNum - 1)
            Dst = DAG.getNode(ISD::ADD, DL, MVT::i32, Dst,
                              DAG.getIntPtrConstant(4));
        }
        i += RegNum - 1;
      } else {
        unsigned Reg = MF.addLiveIn(VA.getLocReg(), &T64::T64GPRFRegClass);
        ArgValue = DAG.getCopyFromReg(Chain, DL, Reg, MVT::i32);

        // If this is an 8 or 16-bit value, it is really passed promoted
        // to 32 bits.  Insert an assert[sz]ext to capture this, then
        // truncate to the right size.
        EVT VT = VA.getValVT();
        EVT RegVT = VA.getLocVT();
        CCValAssign::LocInfo LI = VA.getLocInfo();
        switch (LI) {
        default: llvm_unreachable("Unknown loc info!");
        case CCValAssign::Full: break;
        case CCValAssign::BCvt:
          ArgValue = DAG.getNode(ISD::BITCAST, DL, VT, ArgValue);
          break;
        case CCValAssign::SExt:
        case CCValAssign::ZExt:
          unsigned Opc = (LI == CCValAssign::SExt) ? ISD::AssertSext
                                                   : ISD::AssertZext;
          ArgValue = DAG.getNode(Opc, DL, RegVT, ArgValue,
                                 DAG.getValueType(VT));
          ArgValue = DAG.getNode(ISD::TRUNCATE, DL, VT, ArgValue);
          break;
        }
      }
    } else {
      //sanity check
      assert(VA.isMemLoc());

      MVT ValVT = VA.getValVT();
      unsigned Size = Flags.isByVal() ? Flags.getByValSize()
                                      : ValVT.getSizeInBits() >> 3;
      unsigned Offset = VA.getLocMemOffset() + StackOffset;
  
      int FI = MF.getFrameInfo()->CreateFixedObject(Size, Offset, !isByVal);
      ArgValue = DAG.getFrameIndex(FI, getPointerTy());
      if(!isByVal)
        ArgValue = DAG.getLoad(ValVT, DL, Chain, ArgValue,
                               MachinePointerInfo(), false, false, false, 0);
    }
    InVals.push_back(ArgValue);
  }

  if(!MemOpChains.empty()) {
    MemOpChains.push_back(Chain);
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                        &MemOpChains[0], MemOpChains.size());
  }

  if (MF.getFunction()->hasStructRetAttr()) {
    //Copy the SRet Argument to SRetReturnReg
    Tile64MachineFunctionInfo *FI = MF.getInfo<Tile64MachineFunctionInfo>();
    unsigned Reg = FI->getSRetReturnReg();
    if (!Reg) {
      Reg = MF.getRegInfo().createVirtualRegister(&T64::T64GPRFRegClass);
      FI->setSRetReturnReg(Reg);
    }
    SDValue Copy = DAG.getCopyToReg(DAG.getEntryNode(), DL, Reg, InVals[0]);
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, Copy, Chain);
  }

  // Store remaining ArgRegs to the stack if this is a varargs function.
  if (isVarArg) {
    unsigned NumAllocated = CCInfo.getFirstUnallocated(ArgRegs, NumArgRegs);
    const uint16_t *CurArgReg = ArgRegs + NumAllocated;
    const uint16_t *ArgRegEnd = ArgRegs + NumArgRegs;
    int spillSlotOffset = (NumArgRegs - NumAllocated) * 4;
    spillSlotOffset = -spillSlotOffset;

    int ArgOffset = (int) CCInfo.getNextStackOffset();

    assert((spillSlotOffset == 0 || ArgOffset == 0) &&
           "In the case of empty registers, no item should be arranged to stack slots.");

    ArgOffset += StackOffset;

    // Remember the vararg offset for the va_start implementation.
    FuncInfo->setVarArgsOffset((spillSlotOffset == 0) ? ArgOffset : spillSlotOffset);

    std::vector<SDValue> OutChains;

    for (; CurArgReg != ArgRegEnd; ++CurArgReg) {
      unsigned VReg = RegInfo.createVirtualRegister(&T64::T64GPRFRegClass);
      MF.getRegInfo().addLiveIn(*CurArgReg, VReg);
      SDValue Arg = DAG.getCopyFromReg(DAG.getRoot(), DL, VReg, MVT::i32);

      int FrameIdx = MF.getFrameInfo()->CreateFixedObject(4, spillSlotOffset,
                                                          true);
      SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);

      OutChains.push_back(DAG.getStore(DAG.getRoot(), DL, Arg, FIPtr,
                                       MachinePointerInfo(),
                                       false, false, 0));
      spillSlotOffset += 4;
    }

    if (!OutChains.empty()) {
      OutChains.push_back(Chain);
      Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                          &OutChains[0], OutChains.size());
    }
  }

  return Chain;
}

SDValue Tile64TargetLowering::LowerCall(CallLoweringInfo &CLI,
                                        SmallVectorImpl<SDValue> &InVals) const{
  //FIXME: Tile64 target does not yet support tail call optimization.
  CLI.IsTailCall = false;

	//TODO: is this ok?
	//get arguments from CLI
	SDValue Chain = CLI.Chain;
	SDValue Callee = CLI.Callee;
	CallingConv::ID CallConv = CLI.CallConv;
	bool isVarArg = CLI.IsVarArg;
	//bool doesNotRet = CLI.DoesNotReturn; //FIXME:what is this arg for?
	const SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
	const SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
	const SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
	DebugLoc DL = CLI.DL;
	SelectionDAG &DAG = CLI.DAG;

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
		 DAG.getTarget(), ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, CC_Tile64);

  // Get the size of the outgoing arguments stack space requirement.
  unsigned ArgsSize = CCInfo.getNextStackOffset();

  Chain = DAG.getCALLSEQ_START(Chain, DAG.getIntPtrConstant(ArgsSize, true));

  SmallVector<std::pair<unsigned, SDValue>, 16> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;

  const unsigned StackOffset = DAG.getMachineFunction().
                               getInfo<Tile64MachineFunctionInfo>()->
                               getArgAreaOffset();

  // Walk the register/memloc assignments, inserting copies/loads.
  for (unsigned i = 0, realArgIdx = 0, e = ArgLocs.size();
       i != e; ++i, ++realArgIdx) {
    CCValAssign &VA = ArgLocs[i];
    SDValue Arg = OutVals[realArgIdx];

    ISD::ArgFlagsTy Flags = Outs[realArgIdx].Flags;
    bool isByVal = Flags.isByVal();

    assert((!Flags.isSRet() || (VA.isRegLoc() && VA.getLocReg() == T64::R0)) &&
           "SRet address are to be stored in R0!");

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
    default: llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full: break;
    case CCValAssign::SExt:
      Arg = DAG.getNode(ISD::SIGN_EXTEND, DL, VA.getLocVT(), Arg);
      break;
    case CCValAssign::ZExt:
      Arg = DAG.getNode(ISD::ZERO_EXTEND, DL, VA.getLocVT(), Arg);
      break;
    case CCValAssign::AExt:
      Arg = DAG.getNode(ISD::ANY_EXTEND, DL, VA.getLocVT(), Arg);
      break;
    case CCValAssign::BCvt:
      Arg = DAG.getNode(ISD::BITCAST, DL, VA.getLocVT(), Arg);
      break;
    }

    if(VA.isRegLoc()) {
      //Arguments that can be passed on register must be kept at
      //RegsToPass vector

      if(isByVal) {
        unsigned RegNum = Flags.getByValSize() >> 2; 
        assert(RegNum != 0 && "0-reg structs should have been eliminated.");
        SDValue Ptr = Arg;
        for(unsigned j = 0; j < RegNum; ++j) {
          VA = ArgLocs[i + j];
          assert(VA.isRegLoc() && "Something wrong with byVal in regs.");
          SDValue Load = DAG.getLoad(MVT::i32, DL, Chain, Ptr,
                                     MachinePointerInfo(),
                                     false, false, false, 0);
          MemOpChains.push_back(Load.getValue(1));
          RegsToPass.push_back(std::make_pair(VA.getLocReg(), Load));
          if(j != RegNum - 1)
            Ptr = DAG.getNode(ISD::ADD, DL, Ptr.getValueType(), Ptr,
                              DAG.getIntPtrConstant(4));
        }
        i += RegNum - 1;
      } else { //one-one connection between args and regs.
        RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
      }
    } else {
      //sanity check
      assert(VA.isMemLoc());

      // Create a store off the stack pointer for this argument.
      SDValue StackPtr = DAG.getRegister(T64::StackPointer, MVT::i32);
      SDValue PtrOff = DAG.getIntPtrConstant(VA.getLocMemOffset() + StackOffset);
      SDValue Dst = DAG.getNode(ISD::ADD, DL, MVT::i32, StackPtr, PtrOff);
      if(isByVal) {
        SDValue SizeNode = DAG.getConstant(Flags.getByValSize(), MVT::i32);
        MemOpChains.push_back(DAG.getMemcpy(Chain, DL, Dst, Arg, SizeNode,
                                            Flags.getByValAlign(), false, true,
                                            MachinePointerInfo(),
                                            MachinePointerInfo()));
      } else {
        MemOpChains.push_back(DAG.getStore(Chain, DL, Arg, Dst,
                                           MachinePointerInfo(),
                                           false, false, 0));
      }
    }
  }

  // Emit all stores, make sure they occur before any copies into physregs.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                        &MemOpChains[0], MemOpChains.size());

  // Build a sequence of copy-to-reg nodes chained together with token
  // chain and flag operands which copy the outgoing args into registers.
  // The InFlag in necessary since all emitted instructions must be
  // stuck together.
  SDValue InFlag;
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    unsigned Reg = RegsToPass[i].first;
    SDValue Arg = RegsToPass[i].second;

    Chain = DAG.getCopyToReg(Chain, DL, Reg, Arg, InFlag);
    InFlag = Chain.getValue(1);
  }

  // If the callee is a GlobalAddress node (quite common, every direct call is)
  // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // Likewise ExternalSymbol -> TargetExternalSymbol.
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), DL, MVT::i32);
  } else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee)) {
    Callee = DAG.getTargetExternalSymbol(E->getSymbol(), MVT::i32);
  } else {
    llvm_unreachable("Unexpected type of callee!");
  }

  // Returns a chain & a flag for retval copy to use
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 16> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  // Add argument registers to the end of the list so that they are
  // known live into the call.
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    unsigned Reg = RegsToPass[i].first;
    SDValue Arg = RegsToPass[i].second;
    Ops.push_back(DAG.getRegister(Reg, Arg.getValueType()));
  }
  if (InFlag.getNode())
    Ops.push_back(InFlag);

  unsigned callOpc = getTargetMachine().getRelocationModel() != Reloc::PIC_ ?
                     T64ISD::CALL : T64ISD::PICCALL;

  Chain = DAG.getNode(callOpc, DL, NodeTys, &Ops[0], Ops.size());
  InFlag = Chain.getValue(1);

  Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(ArgsSize, true),
                             DAG.getIntPtrConstant(0, true), InFlag);
  InFlag = Chain.getValue(1);

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState RVInfo(CallConv, isVarArg, DAG.getMachineFunction(),
		 DAG.getTarget(), RVLocs, *DAG.getContext());

  RVInfo.AnalyzeCallResult(Ins, RetCC_Tile64);

  // Copy all of the result registers out of their specified physreg.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    unsigned Reg = RVLocs[i].getLocReg();
    MVT VT = RVLocs[i].getValVT();

    Chain = DAG.getCopyFromReg(Chain, DL, Reg, VT, InFlag).getValue(1);
    InFlag = Chain.getValue(2);
    InVals.push_back(Chain.getValue(0));
  }

  return Chain;
}

bool Tile64TargetLowering::CanLowerReturn(CallingConv::ID CallConv,
                                          MachineFunction &MF, bool isVarArg,
                                          const SmallVectorImpl<ISD::OutputArg> &Outs,
                                          LLVMContext &Context) const {
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, isVarArg, MF, getTargetMachine(), RVLocs, Context);
  return CCInfo.CheckReturn(Outs, RetCC_Tile64);
}

SDValue
Tile64TargetLowering::LowerReturn(SDValue Chain,
                                  CallingConv::ID CallConv, bool isVarArg,
                                  const SmallVectorImpl<ISD::OutputArg> &Outs,
                                  const SmallVectorImpl<SDValue> &OutVals,
                                  DebugLoc DL, SelectionDAG &DAG) const {

  MachineFunction &MF = DAG.getMachineFunction();

  // CCValAssign - represent the assignment of the return value to locations.
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
		 DAG.getTarget(), RVLocs, *DAG.getContext());

  // Analize return values.
  CCInfo.AnalyzeReturn(Outs, RetCC_Tile64);

  // If this is the first return lowered for this function, add the regs to the
  // liveout set for the function.
  if (MF.getRegInfo().liveout_empty()) {
    for (unsigned i = 0; i != RVLocs.size(); ++i)
      if (RVLocs[i].isRegLoc())
        MF.getRegInfo().addLiveOut(RVLocs[i].getLocReg());
  }

  SDValue Flag;

  // Copy the result values into the output registers.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(),
                             OutVals[i], Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
  }

  if(MF.getFunction()->hasStructRetAttr()) {
    Tile64MachineFunctionInfo *FI = MF.getInfo<Tile64MachineFunctionInfo>();
    unsigned Reg = FI->getSRetReturnReg();
    if(!Reg)
      llvm_unreachable("sret virtual register not created in the entry block");
    SDValue Val = DAG.getCopyFromReg(Chain, DL, Reg, getPointerTy());
    Chain = DAG.getCopyToReg(Chain, DL, T64::R0, Val, Flag);
    Flag = Chain.getValue(1);
    if(MF.getRegInfo().liveout_empty())
      MF.getRegInfo().addLiveOut(T64::R0);
  }

  SDValue LinkReg = DAG.getRegister(T64::LinkRegister, getPointerTy());

  if (Flag.getNode())
    return DAG.getNode(T64ISD::JRP, DL, MVT::Other, Chain, LinkReg, Flag);
  else //return void
    return DAG.getNode(T64ISD::JRP, DL, MVT::Other, Chain, LinkReg);
}

bool
Tile64TargetLowering::isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const {
  // The Tile64 target isn't yet aware of offsets.
  return false;
}

