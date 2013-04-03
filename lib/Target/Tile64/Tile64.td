//===- Tile64.td - Describe the Tile64 Target Machine ------*- tablegen -*-===//
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
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "Tile64Schedule.td"
include "Tile64RegisterInfo.td"
include "Tile64CallingConv.td"
include "Tile64InstrInfo.td"

def Tile64InstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// Subtarget features.
//===----------------------------------------------------------------------===//

def FeatureVLIW : SubtargetFeature<"vliw", "IsVLIWEnabled", "true",
                                   "Enable VLIW scheduling">;

//===----------------------------------------------------------------------===//
// Tilera processors supported.
//===----------------------------------------------------------------------===//

class Proc<string Name, SchedMachineModel Model,
           list<SubtargetFeature> Features>
  : ProcessorModel<Name, Model, Features>;

def : Proc<"tile64", Tile64Model, []>;
def : Proc<"tile64-vliw", Tile64Model, [FeatureVLIW]>;

//===----------------------------------------------------------------------===//
// Declare the target which we are implementing
//===----------------------------------------------------------------------===//

def Tile64 : Target {
  // Pull in Instruction Info:
  let InstructionSet = Tile64InstrInfo;
}

