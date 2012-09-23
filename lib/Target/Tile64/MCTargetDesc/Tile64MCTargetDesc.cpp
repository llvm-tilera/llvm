//===-- Tile64MCTargetDesc.cpp - Tile64 Target Descriptions ------*- C++ -*-===//
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
// This file provides Tile64 specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "Tile64MCTargetDesc.h"
#include "Tile64MCAsmInfo.h"
#include "llvm/MC/MCCodeGenInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_MC_DESC
#include "Tile64GenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "Tile64GenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "Tile64GenRegisterInfo.inc"

using namespace llvm;

static MCInstrInfo *createTile64MCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitTile64MCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createTile64MCRegisterInfo(StringRef TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitTile64MCRegisterInfo(X, T64::R51); //TODO: is that right?
  return X;
}

static MCSubtargetInfo *createTile64MCSubtargetInfo(StringRef TT, StringRef CPU,
                                                    StringRef FS) {
  MCSubtargetInfo *X = new MCSubtargetInfo();
  InitTile64MCSubtargetInfo(X, TT, CPU, FS);
  return X;
}

static MCCodeGenInfo *createTile64MCCodeGenInfo(StringRef TT, Reloc::Model RM,
                                                CodeModel::Model CM,
                                                CodeGenOpt::Level OL) {
  MCCodeGenInfo *X = new MCCodeGenInfo();
  X->InitMCCodeGenInfo(RM, CM, OL);
  return X;
}

extern "C" void LLVMInitializeTile64TargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfo<Tile64ELFMCAsmInfo> X(TheTile64Target);

  // Register the MC codegen info.
  TargetRegistry::RegisterMCCodeGenInfo(TheTile64Target,
                                        createTile64MCCodeGenInfo);

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(TheTile64Target, createTile64MCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(TheTile64Target, createTile64MCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(TheTile64Target,
                                          createTile64MCSubtargetInfo);
}

