//===-- Tile64.h - Top-level interface for Tile64 representation --*- C++ -===//
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
// This file contains the entry points for global functions defined in the LLVM
// Tile64 back-end.
//
//===----------------------------------------------------------------------===//

#ifndef TARGET_TILE64_H
#define TARGET_TILE64_H

#include "MCTargetDesc/Tile64MCTargetDesc.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"
#include <cassert>

namespace llvm {
  class FunctionPass;
  class Tile64TargetMachine;
  class formatted_raw_ostream;

  FunctionPass *createTile64ISelDag(Tile64TargetMachine &TM);

  FunctionPass *createTile64VLIWPacketizer();

} // end namespace llvm;

#endif

