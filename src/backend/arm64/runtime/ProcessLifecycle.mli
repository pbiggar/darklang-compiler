val generateHeapInit : ARM64.targetConfig -> Symbolic.instr list

val generateCliArgvHelper :
  ARM64CodeGenTypes.codeGenContext -> string -> Symbolic.instr list

val generateChildLaunchHelper : ARM64.targetConfig -> Symbolic.instr list

val generateChildCleanupHelper :
  ARM64CodeGenTypes.codeGenContext -> Symbolic.instr list
