include_directories( ${CMAKE_CURRENT_BINARY_DIR}/.. ${CMAKE_CURRENT_SOURCE_DIR}/.. )

add_llvm_library(LLVMTile64Info
  Tile64TargetInfo.cpp
  )

add_llvm_library_dependencies(LLVMTile64Info
  LLVMMC
  LLVMSupport
  LLVMTarget
  )

add_dependencies(LLVMTile64Info Tile64CommonTableGen)
