//===-- Tile64MCTargetDesc.h - Tile64 Target Descriptions -------*- C++ -*-===//
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

#ifndef TILE64MCTARGETDESC_H
#define TILE64MCTARGETDESC_H

namespace llvm {
class MCSubtargetInfo;
class Target;
class StringRef;

extern Target TheTile64Target;

} // End llvm namespace

// Defines symbolic names for Tile64 registers.  This defines a mapping from
// register name to register number.
//
#define GET_REGINFO_ENUM
#include "Tile64GenRegisterInfo.inc"

// Defines symbolic names for the Tile64 instructions.
//
#define GET_INSTRINFO_ENUM
#include "Tile64GenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "Tile64GenSubtargetInfo.inc"

#endif

