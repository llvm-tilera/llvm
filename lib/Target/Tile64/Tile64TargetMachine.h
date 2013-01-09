//===-- Tile64TargetMachine.h - Define TargetMachine for Tile64 --*- C++ --===//
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
// This file declares the Tile64 specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef TILE64TARGETMACHINE_H
#define TILE64TARGETMACHINE_H

#include "Tile64InstrInfo.h"
#include "Tile64ISelLowering.h"
#include "Tile64FrameLowering.h"
#include "Tile64SelectionDAGInfo.h"
#include "Tile64Subtarget.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/DataLayout.h"
#include "llvm/Target/TargetFrameLowering.h"

namespace llvm {

class Tile64TargetMachine : public LLVMTargetMachine {
  const DataLayout DL; // Calculates type size & alignment
  Tile64Subtarget Subtarget;
  Tile64InstrInfo InstrInfo;
  Tile64TargetLowering TLInfo;
  Tile64SelectionDAGInfo TSInfo;
  Tile64FrameLowering FrameLowering;
	const InstrItineraryData *InstrItins;

public:
  Tile64TargetMachine(const Target &T, StringRef TT, StringRef CPU,
                      StringRef FS, TargetOptions Options, Reloc::Model RM,
                      CodeModel::Model CM, CodeGenOpt::Level OL);

  virtual const Tile64Subtarget *getSubtargetImpl() const {
    return &Subtarget;
  }

  virtual const Tile64InstrInfo *getInstrInfo() const {
    return &InstrInfo;
  }

  virtual const Tile64FrameLowering  *getFrameLowering() const {
    return &FrameLowering;
  }

  virtual const Tile64RegisterInfo *getRegisterInfo() const {
    return &InstrInfo.getRegisterInfo();
  }

  virtual const Tile64TargetLowering* getTargetLowering() const {
    return &TLInfo;
  }

  virtual const Tile64SelectionDAGInfo* getSelectionDAGInfo() const {
    return &TSInfo;
  }

  virtual const DataLayout *getDataLayout() const {
    return &DL;
  }

  virtual const InstrItineraryData *getInstrItineraryData() const {
    return InstrItins;
  }

  // Pass Pipeline Configuration
  virtual TargetPassConfig *createPassConfig(PassManagerBase &PM);
};

} // end namespace llvm

#endif //TILE64TARGETMACHINE_H

