set(LLVM_TARGET_DEFINITIONS Tile64.td)

tablegen(LLVM Tile64GenRegisterInfo.inc -gen-register-info)
tablegen(LLVM Tile64GenInstrInfo.inc -gen-instr-info)
tablegen(LLVM Tile64GenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM Tile64GenDAGISel.inc -gen-dag-isel)
tablegen(LLVM Tile64GenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM Tile64GenCallingConv.inc -gen-callingconv)
tablegen(LLVM Tile64GenDFAPacketizer.inc -gen-dfa-packetizer)
add_public_tablegen_target(Tile64CommonTableGen)

add_llvm_target(Tile64CodeGen
  Tile64AsmPrinter.cpp
  Tile64InstrInfo.cpp
  Tile64ISelDAGToDAG.cpp
  Tile64ISelLowering.cpp
  Tile64FrameLowering.cpp
  Tile64RegisterInfo.cpp
  Tile64Subtarget.cpp
  Tile64TargetMachine.cpp
  Tile64SelectionDAGInfo.cpp
  Tile64VLIWPacketizer.cpp
  )

add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)

