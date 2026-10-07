(*
   ClockAndRandom.ml - Generate host clock and randomness operations.
*)
open Immediates

(* Obtain eight random bytes with the OS syscall, preserving X1, X2 and X8
   because they may hold live caller values. *)
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

(* Read UTC time as 100ns Unix ticks. macOS uses gettimeofday; Linux uses
   clock_gettime. Preserve caller-saved registers around the syscall. *)
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
