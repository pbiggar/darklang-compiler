val generateFileExists :
  ARM64.targetConfig -> ARM64.reg -> ARM64.reg -> ARM64.instr list

val generateFileDelete :
  ARM64.targetConfig -> ARM64.reg -> ARM64.reg -> ARM64.instr list

val generateFileSetExecutable :
  ARM64.targetConfig -> ARM64.reg -> ARM64.reg -> ARM64.instr list
