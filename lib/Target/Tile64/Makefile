##===- lib/Target/Tile64/Makefile --------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

LEVEL = ../../..
LIBRARYNAME = LLVMTile64CodeGen
TARGET = Tile64

# Make sure that tblgen is run, first thing.
BUILT_SOURCES = Tile64GenRegisterInfo.inc Tile64GenInstrInfo.inc \
		Tile64GenAsmWriter.inc Tile64GenDAGISel.inc \
		Tile64GenSubtargetInfo.inc Tile64GenCallingConv.inc \
		Tile64GenDFAPacketizer.inc

DIRS = TargetInfo MCTargetDesc

include $(LEVEL)/Makefile.common

