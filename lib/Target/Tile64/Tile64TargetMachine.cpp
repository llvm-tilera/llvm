//===-- Tile64TargetMachine.cpp - Define TargetMachine for Tile64 -*- C++ -===//
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
//===----------------------------------------------------------------------===//

#include "Tile64.h"
#include "Tile64TargetMachine.h"
#include "llvm/PassManager.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

extern "C" void LLVMInitializeTile64Target() {
  // Register the target.
  RegisterTargetMachine<Tile64TargetMachine> X(TheTile64Target);
}

Tile64TargetMachine::Tile64TargetMachine(const Target &T, StringRef TT,
                                         StringRef CPU, StringRef FS,
                                         TargetOptions Options,
                                         Reloc::Model RM, CodeModel::Model CM,
                                         CodeGenOpt::Level OL)
  : LLVMTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL),
    DataLayout("e-p:32:32-i8:8-i16:16-i32:32-i64:64-f32:32-f64:64-a:0-S:64"),
    Subtarget(TT, CPU, FS),
    InstrInfo(), TLInfo(*this), TSInfo(*this),
    FrameLowering(),
    InstrItins(&Subtarget.getInstItineraryData()) {
}

namespace {
/// Tile64 Code Generator Pass Configuration Options.
class Tile64PassConfig: public TargetPassConfig {
public:
  Tile64PassConfig(Tile64TargetMachine *TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM) {}

  Tile64TargetMachine &getTile64TargetMachine() const {
    return getTM<Tile64TargetMachine>();
  }

  virtual bool addInstSelector();
	virtual bool addPreEmitPass();
};
} // end namespace

TargetPassConfig *Tile64TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new Tile64PassConfig(this, PM);
}

bool Tile64PassConfig::addInstSelector() {
  addPass(createTile64ISelDag(getTile64TargetMachine()));
  return false;
}

bool Tile64PassConfig::addPreEmitPass() {
  addPass(createTile64VLIWPacketizer());
  return false;
}

