add_llvm_library(LLVMTile64Desc
  Tile64MCTargetDesc.cpp
  Tile64MCAsmInfo.cpp
  )

add_dependencies(LLVMTile64Desc Tile64CommonTableGen)
