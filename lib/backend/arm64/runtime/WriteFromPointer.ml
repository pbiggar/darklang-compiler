(*
   WriteFromPointer.fs - Generate checked writes from native pointer ranges.
   Generate code for FileWriteFromPtr: write raw bytes to a file
   pathReg: register containing heap string pointer to file path
   ptrReg: register containing raw pointer to bytes
   lengthReg: register containing length in bytes
   destReg: destination register (result = 1 on success, 0 on failure)
   O_WRONLY | O_CREAT | O_TRUNC
   1|64|512
   1|0x200|0x400
   Save callee-saved registers
   Allocate stack for path buffer (256 bytes)
   Save registers
   X19 = path heap string
   X20 = data pointer
   X21 = length
   Copy path to stack buffer with null terminator
   X2 = path length
   X0 = dest buffer
   X1 = source data
   X4 = index
   Copy loop
   Store null terminator
   openat(AT_FDCWD, path, flags, mode)
   AT_FDCWD = -100
   path
   flags
   mode 0644
   Check if open failed
   X22 = fd
   If negative, branch to error path
   write(fd, buf, count)
   fd
   buf = data pointer
   count = length
   close(fd)
   Success: result = 1
   Skip error path
   Error path: result = 0
   Cleanup
   open(path, flags, mode) - macOS uses open, not openat
*)
let generateFileWriteFromPtr (target: ARM64.targetConfig) (destReg: ARM64.reg) (pathReg: ARM64.reg) (ptrReg: ARM64.reg) (lengthReg: ARM64.reg) =
    let os = ARM64.targetOS target in
    let syscalls = ARM64.targetSyscalls target in


    let writeFlags =
        match os with
        | Platform.Linux -> 577
        | Platform.MacOS -> 1537
    in
    match os with
    | Platform.Linux ->
        [

            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 1);


            ARM64.MOV_reg (ARM64.X19, pathReg);
            ARM64.MOV_reg (ARM64.X20, ptrReg);
            ARM64.MOV_reg (ARM64.X21, lengthReg);


            ARM64.LDR (ARM64.X2, ARM64.X19, 8);
            ARM64.MOV_reg (ARM64.X0, ARM64.SP);
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 16);
            ARM64.MOVZ (ARM64.X4, 0, 0);


            ARM64.CBZ_offset (ARM64.X2, 7);
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4);
            ARM64.STRB (ARM64.X3, ARM64.X0, 0);
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1);
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1);
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1);
            ARM64.B (-6);


            ARM64.MOVZ (ARM64.X3, 0, 0);
            ARM64.STRB (ARM64.X3, ARM64.X0, 0);


            ARM64.MOVZ (ARM64.X0, 100, 0);
            ARM64.NEG (ARM64.X0, ARM64.X0);
            ARM64.MOV_reg (ARM64.X1, ARM64.SP);
            ARM64.MOVZ (ARM64.X2, writeFlags, 0);
            ARM64.MOVZ (ARM64.X3, 420, 0);
            ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.open_, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.MOV_reg (ARM64.X22, ARM64.X0);
            ARM64.TBNZ (ARM64.X0, 63, 11);


            ARM64.MOV_reg (ARM64.X0, ARM64.X22);
            ARM64.MOV_reg (ARM64.X1, ARM64.X20);
            ARM64.MOV_reg (ARM64.X2, ARM64.X21);
            ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.write, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.MOV_reg (ARM64.X0, ARM64.X22);
            ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.close, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.MOVZ (ARM64.X0, 1, 0);
            ARM64.B 2;


            ARM64.MOVZ (ARM64.X0, 0, 0);


            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 1);
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 0);
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 16);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
            ARM64.MOV_reg (destReg, ARM64.X0);
        ]

    | Platform.MacOS ->
        [

            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 1);


            ARM64.MOV_reg (ARM64.X19, pathReg);
            ARM64.MOV_reg (ARM64.X20, ptrReg);
            ARM64.MOV_reg (ARM64.X21, lengthReg);


            ARM64.LDR (ARM64.X2, ARM64.X19, 8);
            ARM64.MOV_reg (ARM64.X0, ARM64.SP);
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 16);
            ARM64.MOVZ (ARM64.X4, 0, 0);


            ARM64.CBZ_offset (ARM64.X2, 7);
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4);
            ARM64.STRB (ARM64.X3, ARM64.X0, 0);
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1);
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1);
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1);
            ARM64.B (-6);


            ARM64.MOVZ (ARM64.X3, 0, 0);
            ARM64.STRB (ARM64.X3, ARM64.X0, 0);


            ARM64.MOV_reg (ARM64.X0, ARM64.SP);
            ARM64.MOVZ (ARM64.X1, writeFlags, 0);
            ARM64.MOVZ (ARM64.X2, 420, 0);
            ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.open_, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.MOV_reg (ARM64.X22, ARM64.X0);
            ARM64.TBNZ (ARM64.X0, 63, 10);


            ARM64.MOV_reg (ARM64.X0, ARM64.X22);
            ARM64.MOV_reg (ARM64.X1, ARM64.X20);
            ARM64.MOV_reg (ARM64.X2, ARM64.X21);
            ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.write, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.MOV_reg (ARM64.X0, ARM64.X22);
            ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.close, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.MOVZ (ARM64.X0, 1, 0);
            ARM64.B 2;


            ARM64.MOVZ (ARM64.X0, 0, 0);


            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 1);
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 0);
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 16);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
            ARM64.MOV_reg (destReg, ARM64.X0);
        ]
