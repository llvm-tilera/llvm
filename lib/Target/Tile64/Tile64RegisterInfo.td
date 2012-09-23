//===- Tile64RegisterInfo.td - Tile64 Register defs --------*- tablegen -*-===//
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
//  Declarations that describe the Tile64 register file.
//
//  NOTE: SPRs are not mentioned in this definition. 
//===----------------------------------------------------------------------===//


class Tile64Reg<string n> : Register<n> {
  let Namespace = "T64";
}

//===----------------------------------------------------------------------===//
// Define registers
//===----------------------------------------------------------------------===//

//General purpose registers
def R0 : Tile64Reg<"r0">;
def R1 : Tile64Reg<"r1">;
def R2 : Tile64Reg<"r2">;
def R3 : Tile64Reg<"r3">;
def R4 : Tile64Reg<"r4">;
def R5 : Tile64Reg<"r5">;
def R6 : Tile64Reg<"r6">;
def R7 : Tile64Reg<"r7">;
def R8 : Tile64Reg<"r8">;
def R9 : Tile64Reg<"r9">;
def R10 : Tile64Reg<"r10">;
def R11 : Tile64Reg<"r11">;
def R12 : Tile64Reg<"r12">;
def R13 : Tile64Reg<"r13">;
def R14 : Tile64Reg<"r14">;
def R15 : Tile64Reg<"r15">;
def R16 : Tile64Reg<"r16">;
def R17 : Tile64Reg<"r17">;
def R18 : Tile64Reg<"r18">;
def R19 : Tile64Reg<"r19">;
def R20 : Tile64Reg<"r20">;
def R21 : Tile64Reg<"r21">;
def R22 : Tile64Reg<"r22">;
def R23 : Tile64Reg<"r23">;
def R24 : Tile64Reg<"r24">;
def R25 : Tile64Reg<"r25">;
def R26 : Tile64Reg<"r26">;
def R27 : Tile64Reg<"r27">;
def R28 : Tile64Reg<"r28">;
def R29 : Tile64Reg<"r29">;
def R30 : Tile64Reg<"r30">;
def R31 : Tile64Reg<"r31">;
def R32 : Tile64Reg<"r32">;
def R33 : Tile64Reg<"r33">;
def R34 : Tile64Reg<"r34">;
def R35 : Tile64Reg<"r35">;
def R36 : Tile64Reg<"r36">;
def R37 : Tile64Reg<"r37">;
def R38 : Tile64Reg<"r38">;
def R39 : Tile64Reg<"r39">;
def R40 : Tile64Reg<"r40">;
def R41 : Tile64Reg<"r41">;
def R42 : Tile64Reg<"r42">;
def R43 : Tile64Reg<"r43">;
def R44 : Tile64Reg<"r44">;
def R45 : Tile64Reg<"r45">;
def R46 : Tile64Reg<"r46">;
def R47 : Tile64Reg<"r47">;
def R48 : Tile64Reg<"r48">;
def R49 : Tile64Reg<"r49">;
def R50 : Tile64Reg<"r50">;
def R51 : Tile64Reg<"r51">;

//Specially handled general purpose registers
def FramePointer : Tile64Reg<"r52">;
def ThreadLocalData : Tile64Reg<"tp">;
def StackPointer : Tile64Reg<"sp">;
def LinkRegister : Tile64Reg<"lr">;

//General purpose registers for on-card networking
def SN : Tile64Reg<"sn">;
def IDN0 : Tile64Reg<"idn0">;
def IDN1 : Tile64Reg<"idn1">;
def UDN0 : Tile64Reg<"udn0">;
def UDN1 : Tile64Reg<"udn1">;
def UDN2 : Tile64Reg<"udn2">;
def UDN3 : Tile64Reg<"udn3">;

//Zero register
def Zero : Tile64Reg<"zero">;


//===----------------------------------------------------------------------===//
// Define RegisterClass
//===----------------------------------------------------------------------===//

def T64GPRF : RegisterClass<"T64", [i32, v2i16, v4i8], 32,
                            (add
                                 (sequence "R%u", 0, 51),
                                 FramePointer,
                                 ThreadLocalData,
                                 StackPointer,
                                 LinkRegister,
                                 SN,
                                 (sequence "IDN%u", 0, 1),
                                 (sequence "UDN%u", 0, 3),
                                 Zero
                            )>;

