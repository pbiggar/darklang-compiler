val generateCliArgvHelper : unit -> X86_64.instr list
val generateCliEnvironmentPackedHelper : bool -> X86_64.instr list
val generateCliGetEnvHelper : bool -> X86_64.instr list
val generateChildLaunchHelper : unit -> X86_64.instr list
val generateChildCleanupHelper : bool -> X86_64.instr list
