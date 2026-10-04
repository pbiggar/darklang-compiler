val generateHeapInit : ARM64.targetConfig -> Symbolic.instr list
val generateCliArgvHelper : ARM64CodeGenTypes.codeGenContext -> string -> Symbolic.instr list
val generateLinuxCliSpawnProcessHelper : unit -> Symbolic.instr list
val generateLinuxCliProcessLifecycleHelpers : ARM64CodeGenTypes.codeGenContext -> Symbolic.instr list
