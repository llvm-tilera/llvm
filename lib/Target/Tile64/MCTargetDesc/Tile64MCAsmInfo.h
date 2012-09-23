//=====-- Tile64MCAsmInfo.h - Tile64 asm properties -----------*- C++ -*--====//
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
// This file contains the declaration of the Tile64MCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef TILE64TARGETASMINFO_H
#define TILE64TARGETASMINFO_H

#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCAsmInfo.h"

namespace llvm {
  class Target;

  struct Tile64ELFMCAsmInfo : public MCAsmInfo {
    explicit Tile64ELFMCAsmInfo(const Target &T, StringRef TT);
  };

} // namespace llvm

#endif

