//===- Tile64MachineFunctionInfo.h - Tile64 Machine Function Info -*- C++ -===//
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
// This file declares Tile64 specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef TILE64MACHINEFUNCTIONINFO_H
#define TILE64MACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

  class Tile64MachineFunctionInfo : public MachineFunctionInfo {
  private:
    /// GlobalBaseReg - Virtual register containing base address
    unsigned GlobalBaseReg;

    /// VarArgsOffset - Frame offset to start of varargs area.
    int VarArgsOffset;

    /// SRetReturnReg - Virtual register storing sret return address
    unsigned SRetReturnReg;

    /// ArgAreaOffset - offset for the first incoming arg on stack from sp
    const unsigned ArgAreaOffset;
  public:
    Tile64MachineFunctionInfo()
      : GlobalBaseReg(0), VarArgsOffset(0),
        SRetReturnReg(0), ArgAreaOffset(8) {}
    explicit Tile64MachineFunctionInfo(MachineFunction &MF)
      : GlobalBaseReg(0), VarArgsOffset(0),
        SRetReturnReg(0), ArgAreaOffset(8) {}

    unsigned getGlobalBaseReg() const { return GlobalBaseReg; }
    void setGlobalBaseReg(unsigned Reg) { GlobalBaseReg = Reg; }

    int getVarArgsOffset() const { return VarArgsOffset; }
    void setVarArgsOffset(int Offset) { VarArgsOffset = Offset; }

    unsigned getSRetReturnReg() const { return SRetReturnReg; }
    void setSRetReturnReg(const unsigned Reg) { SRetReturnReg = Reg; }

    unsigned getArgAreaOffset() const { return ArgAreaOffset; }
  };
}

#endif
