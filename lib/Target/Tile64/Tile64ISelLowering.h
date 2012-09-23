//===-- Til64ISelLowering.h - Tile64 DAG Lowering Interface -----*- C++ -*-===//
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
// This file defines the interfaces that Tile64 uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#ifndef TILE64_ISELLOWERING_H
#define TILE64_ISELLOWERING_H

#include "llvm/Target/TargetLowering.h"
#include "Tile64.h"

namespace llvm {
  namespace T64ISD {
    enum {
      FIRST_NUMBER = ISD::BUILTIN_OP_END,

      MOVELO, //pseudo instruction for getting the lower half of a word
      ADDHI, //pseudo instruction for adding the upper half of a word to the lower
      ADDLO_PIC, //psuedo instruction for adding the lower half of a word to a base reg
      ADDHI_PIC, //pseudo instruction for adding the upper half of a word to the lower in case of pic
			ADDLI_GOT, //pseudo instruction for adding offset to GOT address

      JAL, //call by offset
      JALR, //call by address
      JRP, //return

      CALL, //pseudo instruction for jump-and-link instructions
      PICCALL, //same as the previous for PIC relocation

      PREFETCH,
      FENCE,
      MEMBARRIER,

      //multiply instructions
      MULHH_SS,
      MULHH_SU,
      MULHH_UU,
      MULHHA_SS,
      MULHHA_SU,
      MULHHA_UU,
      MULHHSA_UU,
      MULHL_SS,
      MULHL_SU,
      MULHL_US,
      MULHL_UU,
      MULHLA_SS,
      MULHLA_SU,
      MULHLA_US,
      MULHLA_UU,
      MULHLSA_UU,
      MULLL_SS,
      MULLL_SU,
      MULLL_UU,
      MULLLA_SS,
      MULLLA_SU,
      MULLLA_UU,
      MULLLSA_UU,

      //NOOP instructions
      FNOP,
      NOP,

      //Other special instructions of Tile64
      BITX,
      CRC32_32,
      CRC32_8,
      MM,

      GLOBAL_BASE_REG, //pseudo instruction for retrieving GBR
      STORE_FP, //pseudo instruction to store fp at the bottom of frame is needed
      NEWSLOT_ADDR //pseudo instruction to get the address of a dynalloc stack slot
    };
  }


  //TODO: inlineasm support for Tile64

  class Tile64TargetLowering : public TargetLowering {
  public:
    Tile64TargetLowering(TargetMachine &TM);
    virtual SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const;

    virtual MachineBasicBlock
      *EmitInstrWithCustomInserter(MachineInstr *MI,
                                   MachineBasicBlock *MBB) const;

    virtual const char *getTargetNodeName(unsigned Opcode) const;

    virtual SDValue
      LowerFormalArguments(SDValue Chain,
                           CallingConv::ID CallConv,
                           bool isVarArg,
                           const SmallVectorImpl<ISD::InputArg> &Ins,
                           DebugLoc DL, SelectionDAG &DAG,
                           SmallVectorImpl<SDValue> &InVals) const;

    virtual SDValue LowerCall(CallLoweringInfo &CLI,
                              SmallVectorImpl<SDValue> &InVals) const;

    virtual bool CanLowerReturn(CallingConv::ID CallConv, MachineFunction &MF,
                                bool isVarArg,
                                const SmallVectorImpl<ISD::OutputArg> &Outs,
                                LLVMContext &Context) const;



    virtual SDValue
      LowerReturn(SDValue Chain,
                  CallingConv::ID CallConv, bool isVarArg,
                  const SmallVectorImpl<ISD::OutputArg> &Outs,
                  const SmallVectorImpl<SDValue> &OutVals,
                  DebugLoc DL, SelectionDAG &DAG) const;

    virtual bool isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const;

    //
    //Helper functions
    //
    SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
    SDValue LowerConstantPool(SDValue Op, SelectionDAG &DAG) const;

    SDValue LowerVASTART(SDValue Op, SelectionDAG &DAG)  const;
    SDValue LowerVAARG(SDValue Op, SelectionDAG &DAG) const;
    SDValue LowerDYNAMIC_STACKALLOC(SDValue Op, SelectionDAG &DAG) const;
    SDValue LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG,
                           bool isFrameAddrTaken) const;
    SDValue LowerRETURNADDR(SDValue Op, SelectionDAG &DAG) const;

    MachineBasicBlock
      *EmitSELECTWithCustomInserter(MachineInstr *MI,
                                    MachineBasicBlock *MBB) const;
    MachineBasicBlock
      *EmitSTORE_FPWithCustomInserter(MachineInstr *MI,
                                      MachineBasicBlock *MBB) const;
  };
} // end namespace llvm

#endif    // TILE64_ISELLOWERING_H

