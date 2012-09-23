//===-- Tile64SelectionDAGInfo.cpp - Tile64 SelectionDAG Info -------------===//
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
// This file implements the Tile64SelectionDAGInfo class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "tile64-selectiondag-info"
#include "Tile64TargetMachine.h"

using namespace llvm;

Tile64SelectionDAGInfo::Tile64SelectionDAGInfo(const Tile64TargetMachine &TM)
  : TargetSelectionDAGInfo(TM) {
}

Tile64SelectionDAGInfo::~Tile64SelectionDAGInfo() {
}

