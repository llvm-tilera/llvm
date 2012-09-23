//===-- Tile64SelectionDAGInfo.h - Tile64 SelectionDAG Info -----*- C++ -*-===//
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
// This file defines the Tile64 subclass for TargetSelectionDAGInfo.
//
//===----------------------------------------------------------------------===//

#ifndef TILE64SELECTIONDAGINFO_H
#define TILE64SELECTIONDAGINFO_H

#include "llvm/Target/TargetSelectionDAGInfo.h"

namespace llvm {

class Tile64TargetMachine;

class Tile64SelectionDAGInfo : public TargetSelectionDAGInfo {
public:
  explicit Tile64SelectionDAGInfo(const Tile64TargetMachine &TM);
  ~Tile64SelectionDAGInfo();
};

}

#endif

