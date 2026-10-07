(* ClockAndRandom.mli - ARM64 clock and randomness syscall generation. *)
val generateRandomInt64 : ARM64.targetConfig -> ARM64.reg -> ARM64.instr list
val generateDateTimeNow : ARM64.targetConfig -> ARM64.reg -> ARM64.instr list
