//===- Tile64CallingConv.td - Calling Conventions Tile64 ---*- tablegen -*-===//
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
// This describes the calling conventions for the Tile64 architecture.
//
//===----------------------------------------------------------------------===//

// Tile64 return-value convention.
def RetCC_Tile64 : CallingConv<[
  //Pass return value in registers if it fits them.
  CCAssignToReg<[R0, R1, R2, R3, R4, R5, R6, R7, R8, R9]>
]>;

// Tile64 Calling convention.
def CC_Tile64 : CallingConv<[
  //Assign SRet ptr to R0.
  CCIfSRet<CCAssignToReg<[R0]>>,

  //Arguments get passed in registers if there is space. Bigger values my need
  //padding, and are to be passed entirely in register or on stack. Arguments
  //passed on stack are to be stored according to ABI size and alignment.
  //However, types are already legalized at this point, i.e. the type of every
  //argument is i32, except for byval arguments. Therefore the calling
  //convention need to be realized via a custom function.
  CCCustom<"CC_Tile64_Assign">
]>;

