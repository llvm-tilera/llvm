//===- Tile64InstrFormats.td - Tile64 Instruction Formats --*- tablegen -*-===//
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
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//                         Tile64 Instruction Flags
//
//                    *** Must match Tile64BaseInfo.h ***
//===----------------------------------------------------------------------===//

class T64Type<bits<4> t> {
  bits<4> Value = t;
}

def TypePSEUDO : T64Type<0>;
def TypeArS    : T64Type<1>;
def TypeArL    : T64Type<2>;
def TypeBmS    : T64Type<3>;
def TypeBmL    : T64Type<4>;
def TypeCtr    : T64Type<5>;
def TypeMeS    : T64Type<6>;
def TypeMeL    : T64Type<7>;
def TypeIll    : T64Type<8>;

//===----------------------------------------------------------------------===//
//                        Instruction class declaration
//===----------------------------------------------------------------------===//

class InstT64<dag outs, dag ins, string asmstr, list<dag> pattern, string cstr,
              InstrItinClass itin, T64Type type> : Instruction {
  let Namespace = "T64";

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
  let Constraints = cstr;
  let Itinerary = itin;

  // *** Must match Tile64BaseInfo.h ***
  T64Type Tile64Type = type;
  let TSFlags{0-3} = Tile64Type.Value;

  bits<1> isTile64Solo = 0;
  let TSFlags{4} = isTile64Solo;

  bits<1> isTile64Long = 0;
  let TSFlags{5} = isTile64Long;
  // *** The code above must match Tile64BaseInfo.h ***
}

class InstArST64<dag outs, dag ins, string asmstr, list<dag> pattern,
                 string cstr = ""> : InstT64<outs, ins, asmstr, pattern,
                                             cstr, ArS, TypeArS> {}

class InstArLT64<dag outs, dag ins, string asmstr, list<dag> pattern,
                 string cstr = ""> : InstT64<outs, ins, asmstr, pattern,
                                             cstr, ArL, TypeArL> {
  let isTile64Long = 1;
}

class InstBmST64<dag outs, dag ins, string asmstr, list<dag> pattern,
                 string cstr = ""> : InstT64<outs, ins, asmstr, pattern,
                                             cstr, BmS, TypeBmS> {}

class InstBmLT64<dag outs, dag ins, string asmstr, list<dag> pattern,
                 string cstr = ""> : InstT64<outs, ins, asmstr, pattern,
                                             cstr, BmL, TypeBmL> {
  let isTile64Long = 1;
}

class InstCtrT64<dag outs, dag ins, string asmstr, list<dag> pattern,
                 string cstr = ""> : InstT64<outs, ins, asmstr, pattern,
                                             cstr, Ctr, TypeCtr> {
  let isTile64Long = 1;
}

class InstMeST64<dag outs, dag ins, string asmstr, list<dag> pattern,
                 string cstr = ""> : InstT64<outs, ins, asmstr, pattern,
                                             cstr, MeS, TypeMeS> {}

class InstMeLT64<dag outs, dag ins, string asmstr, list<dag> pattern,
                 string cstr = ""> : InstT64<outs, ins, asmstr, pattern,
                                             cstr, MeL, TypeMeL> {
  let isTile64Long = 1;
}

class InstIllT64<dag outs, dag ins, string asmstr, list<dag> pattern,
                 string cstr = ""> : InstT64<outs, ins, asmstr, pattern,
                                             cstr, Ill, TypeIll> {}


class PseudoT64<dag outs, dag ins, string asmstr, list<dag> pattern> :
      InstT64<outs, ins, asmstr, pattern, "", PSEUDO, TypePSEUDO> {
  let isCodeGenOnly = 1;
  let isPseudo = 1;
  let isTile64Solo = 1;
}

