add_llvm_library(LLVMTile64Desc
  Tile64MCTargetDesc.cpp
  Tile64MCAsmInfo.cpp
  )

add_llvm_library_dependencies(LLVMTile64Desc
  LLVMMC
  LLVMTile64Info
  LLVMSupport
  )

add_dependencies(LLVMTile64Desc Tile64CommonTableGen)
