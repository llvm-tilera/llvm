//===-- Tile64BaseInfo.h - Top level definitions for Tile64 ----*- C++ -*--===//
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
// This file contains small standalone helper functions and enum definitions for
// the Tile64 target useful for the compiler back-end and the MC libraries.
// As such, it deliberately does not include references to LLVM core
// code gen types, passes, etc..
//
//===----------------------------------------------------------------------===//

#ifndef TILE64BASEINFO_H
#define TILE64BASEINFO_H

namespace llvm {

// Tile64II - This namespace holds all of the target specific flags that /
//instruction info tracks.
//
namespace Tile64II {
  // *** The code below must match Tile64InstrFormat*.td *** //

  // Insn types. -- see Tile64Schedule.td bundle-approach
  // *** Must match HexagonInstrFormat*.td ***
  enum Tile64Type {
    TypePSEUDO = 0,
    TypeArS    = 1,
    TypeArL    = 2,
    TypeBmS    = 3,
    TypeBmL    = 4,
    TypeCtr    = 5,
    TypeMeS    = 6,
    TypeMeL    = 7,
    TypeIll    = 8,
    TypeNull   = 15 //not a valid type
  };



  // MCInstrDesc TSFlags
  // *** Must match Tile64InstrFormat*.td ***
  enum {
    // This 4-bit field describes the insn type.
    TypePos  = 0,
    TypeMask = 0xf,

    // Solo instructions.
    SoloPos  = 4,
    SoloMask = 0x1,

    // Long instructions. -- only for X bundles
    LongPos = 5,
    LongMask = 0x1
  };

  // *** The code above must match Tile64InstrFormat*.td *** //

} // End namespace Tile64II.

} // End namespace llvm.

#endif
