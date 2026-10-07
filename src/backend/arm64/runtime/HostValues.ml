(*
   HostValues.fs - Generate host clock and randomness operations.
*)
open Immediates






(*
   Generate ARM64 instructions to get 8 random bytes as Int64
   destReg: destination register for the random Int64
   Uses getrandom (Linux) or getentropy (macOS) syscall
   Note: This function saves/restores caller-saved registers X1, X2, X8
   that may contain live values, since the syscall clobbers them.
   Save X1 (caller-saved, may contain live value)
   Allocate 32 bytes: 8 for X1, 8 for buffer, 16 for alignment
   Save X1 at SP+24
   Call getentropy(buffer, 8)
   X0 = buffer pointer (SP), X1 = length (8)
   Load 8 bytes from buffer into X0
   Restore X1
   Cleanup stack
   Move result to destination
   Save X1, X2, X8 (caller-saved, may contain live values)
   Allocate 48 bytes: 8 for buffer, 8 each for X1/X2/X8 = 32, 16 for alignment
   Save X1 at SP+40
   Save X2 at SP+32
   Save X8 at SP+24
   Call getrandom(buffer, 8, flags=0)
   X0 = buffer pointer (SP), X1 = length (8), X2 = flags (0)
   Restore X1, X2, X8
*)
let generateRandomInt64 (target: ARM64.targetConfig) (destReg: ARM64.reg) =
    let os = ARM64.targetOS target in
    let syscalls = ARM64.targetSyscalls target in

    match os with
    | Platform.MacOS ->
        [


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);
            ARM64.STR (ARM64.X1, ARM64.SP, 24);



            ARM64.MOV_reg (ARM64.X0, ARM64.SP);
            ARM64.MOVZ (ARM64.X1, 8, 0);
            ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.getrandom, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.LDR (ARM64.X0, ARM64.SP, 0);


            ARM64.LDR (ARM64.X1, ARM64.SP, 24);


            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);


            ARM64.MOV_reg (destReg, ARM64.X0);
        ]
    | Platform.Linux ->
        [


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48);
            ARM64.STR (ARM64.X1, ARM64.SP, 40);
            ARM64.STR (ARM64.X2, ARM64.SP, 32);
            ARM64.STR (ARM64.X8, ARM64.SP, 24);



            ARM64.MOV_reg (ARM64.X0, ARM64.SP);
            ARM64.MOVZ (ARM64.X1, 8, 0);
            ARM64.MOVZ (ARM64.X2, 0, 0);
            ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.getrandom, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.LDR (ARM64.X0, ARM64.SP, 0);


            ARM64.LDR (ARM64.X1, ARM64.SP, 40);
            ARM64.LDR (ARM64.X2, ARM64.SP, 32);
            ARM64.LDR (ARM64.X8, ARM64.SP, 24);


            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48);


            ARM64.MOV_reg (destReg, ARM64.X0);
        ]





(*
   Generate ARM64 instructions to get the current UTC instant as 100ns Unix ticks.
   destReg: destination register for the timestamp
   Uses gettimeofday (macOS) or clock_gettime (Linux) syscall
   Note: This function saves/restores caller-saved registers that may contain live values.
   Preserve the scratch registers around the syscall and conversion.
   Call gettimeofday(tv, NULL)
   X0 = timeval pointer (SP), X1 = timezone (NULL)
   NULL timezone
   ticks = tv_sec * 10_000_000 + tv_usec * 10
   Move result to destination
   Save X1, X2, X8 (caller-saved, may contain live values)
   Allocate 48 bytes: 16 for timespec struct (tv_sec, tv_nsec), 8 each for X1/X2/X8 = 24, 8 for alignment
   Save X1 at SP+40
   Save X2 at SP+32
   Save X8 at SP+24
   Call clock_gettime(CLOCK_REALTIME, ts)
   X0 = clock_id (0 = CLOCK_REALTIME), X1 = timespec pointer (SP)
   CLOCK_REALTIME = 0
   ticks = tv_sec * 10_000_000 + tv_nsec / 100
   Restore X1, X2, X8
   Cleanup stack
*)
let generateDateTimeNow (target: ARM64.targetConfig) (destReg: ARM64.reg) =
    let os = ARM64.targetOS target in
    let syscalls = ARM64.targetSyscalls target in

    match os with
    | Platform.MacOS ->
        [

            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48);
            ARM64.STR (ARM64.X1, ARM64.SP, 40);
            ARM64.STR (ARM64.X2, ARM64.SP, 32);



            ARM64.MOV_reg (ARM64.X0, ARM64.SP);
            ARM64.MOVZ (ARM64.X1, 0, 0);
            ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.gettimeofday, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.LDR (ARM64.X0, ARM64.SP, 0);
            ARM64.LDR (ARM64.X1, ARM64.SP, 8);
        ] @ generateLoadUInt64Immediate ARM64.X2 10000000L @ [
            ARM64.MUL (ARM64.X0, ARM64.X0, ARM64.X2);
            ARM64.MOVZ (ARM64.X2, 10, 0);
            ARM64.MUL (ARM64.X1, ARM64.X1, ARM64.X2);
            ARM64.ADD_reg (ARM64.X0, ARM64.X0, ARM64.X1);

            ARM64.LDR (ARM64.X1, ARM64.SP, 40);
            ARM64.LDR (ARM64.X2, ARM64.SP, 32);

            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48);


            ARM64.MOV_reg (destReg, ARM64.X0);
        ]
    | Platform.Linux ->
        [


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48);
            ARM64.STR (ARM64.X1, ARM64.SP, 40);
            ARM64.STR (ARM64.X2, ARM64.SP, 32);
            ARM64.STR (ARM64.X8, ARM64.SP, 24);



            ARM64.MOVZ (ARM64.X0, 0, 0);
            ARM64.MOV_reg (ARM64.X1, ARM64.SP);
            ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.gettimeofday, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.LDR (ARM64.X0, ARM64.SP, 0);
            ARM64.LDR (ARM64.X1, ARM64.SP, 8);
        ] @ generateLoadUInt64Immediate ARM64.X2 10000000L @ [
            ARM64.MUL (ARM64.X0, ARM64.X0, ARM64.X2);
            ARM64.MOVZ (ARM64.X2, 100, 0);
            ARM64.UDIV (ARM64.X1, ARM64.X1, ARM64.X2);
            ARM64.ADD_reg (ARM64.X0, ARM64.X0, ARM64.X1);


            ARM64.LDR (ARM64.X1, ARM64.SP, 40);
            ARM64.LDR (ARM64.X2, ARM64.SP, 32);
            ARM64.LDR (ARM64.X8, ARM64.SP, 24);


            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48);


            ARM64.MOV_reg (destReg, ARM64.X0);
        ]
