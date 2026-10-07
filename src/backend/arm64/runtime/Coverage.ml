(*
   Coverage.ml - Generate coverage-buffer output at program termination.
*)
open Immediates

(*
   Generate ARM64 instructions to flush coverage data to file
   Writes coverage counters to /tmp/dark_cov.bin before program exit
   coverageExprCount: number of expressions (determines bytes to write = count * 8)
   Uses ADRP+ADD to get coverage data address from BSS section
   Opens file, writes data, closes file (errors are silently ignored)
   O_WRONLY | O_CREAT | O_TRUNC
   1|64|512
   1|0x200|0x400
   Path: "/tmp/dark_cov.bin" = 18 bytes + null = 19 bytes, round to 24 for alignment
   Allocate stack for path (24 bytes, 8-byte aligned)
   Write "/tmp/dark_cov.bin\0" to stack
   First 8 bytes: "/tmp/dar" = 0x7261642F706D742F
   "/t"
   "mp"
   "/d"
   "ar"
   Next 8 bytes: "k_cov.bi" = 0x69622E766F635F6B
   "k_"
   "co"
   "v."
   "bi"
   Last 4 bytes: "n\0\0\0" = 0x0000006E
   "n\0"
   Get coverage data address via ADRP+ADD
   openat(AT_FDCWD, path, flags, mode)
   AT_FDCWD = -100
   path
   flags
   mode 0644
   Check if open failed (X0 < 0)
   If negative, skip to cleanup
   Save fd
   write(fd, buf, count)
   fd
   buf = coverage data
   close(fd)
   Cleanup stack
   Write "/tmp/dark_cov.bin\0" to stack (same as Linux)
   open(path, flags, mode) - macOS uses direct open syscall
*)
let generateCoverageFlush (target: ARM64.targetConfig) (coverageExprCount: int) =
    if coverageExprCount = 0 then
        []
    else
        let os = ARM64.targetOS target in
        let syscalls = ARM64.targetSyscalls target in


        let writeFlags =
            match os with
            | Platform.Linux -> 577
            | Platform.MacOS -> 1537
        in


        let byteCount = Int32.to_int (Int32.mul (Int32.of_int coverageExprCount) 8l) in

        match os with
        | Platform.Linux ->
            [

                ARM64.SUB_imm (ARM64.SP, ARM64.SP, 24);



                ARM64.MOVZ (ARM64.X9, 0x2F74, 0);
                ARM64.MOVK (ARM64.X9, 0x6D70, 16);
                ARM64.MOVK (ARM64.X9, 0x642F, 32);
                ARM64.MOVK (ARM64.X9, 0x7261, 48);
                ARM64.STR (ARM64.X9, ARM64.SP, 0);


                ARM64.MOVZ (ARM64.X9, 0x5F6B, 0);
                ARM64.MOVK (ARM64.X9, 0x6F63, 16);
                ARM64.MOVK (ARM64.X9, 0x2E76, 32);
                ARM64.MOVK (ARM64.X9, 0x6962, 48);
                ARM64.STR (ARM64.X9, ARM64.SP, 8);


                ARM64.MOVZ (ARM64.X9, 0x006E, 0);
                ARM64.STR (ARM64.X9, ARM64.SP, 16);


                ARM64.ADRP (ARM64.X10, Symbolic.coverageDataLabelName);
                ARM64.ADD_label (ARM64.X10, ARM64.X10, Symbolic.coverageDataLabelName);


                ARM64.MOVZ (ARM64.X0, 100, 0);
                ARM64.NEG (ARM64.X0, ARM64.X0);
                ARM64.MOV_reg (ARM64.X1, ARM64.SP);
                ARM64.MOVZ (ARM64.X2, writeFlags, 0);
                ARM64.MOVZ (ARM64.X3, 420, 0);
                ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.open_, 0);
                ARM64.SVC syscalls.ARM64.svcImmediate;


                ARM64.TBNZ (ARM64.X0, 63, 6);


                ARM64.MOV_reg (ARM64.X11, ARM64.X0);


                ARM64.MOV_reg (ARM64.X0, ARM64.X11);
                ARM64.MOV_reg (ARM64.X1, ARM64.X10);
            ] @
            generateLoadNonNegativeIntImmediate ARM64.X2 byteCount @
            [
                ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.write, 0);
                ARM64.SVC syscalls.ARM64.svcImmediate;


                ARM64.MOV_reg (ARM64.X0, ARM64.X11);
                ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.close, 0);
                ARM64.SVC syscalls.ARM64.svcImmediate;


                ARM64.ADD_imm (ARM64.SP, ARM64.SP, 24);
            ]

        | Platform.MacOS ->
            [

                ARM64.SUB_imm (ARM64.SP, ARM64.SP, 24);


                ARM64.MOVZ (ARM64.X9, 0x2F74, 0);
                ARM64.MOVK (ARM64.X9, 0x6D70, 16);
                ARM64.MOVK (ARM64.X9, 0x642F, 32);
                ARM64.MOVK (ARM64.X9, 0x7261, 48);
                ARM64.STR (ARM64.X9, ARM64.SP, 0);

                ARM64.MOVZ (ARM64.X9, 0x5F6B, 0);
                ARM64.MOVK (ARM64.X9, 0x6F63, 16);
                ARM64.MOVK (ARM64.X9, 0x2E76, 32);
                ARM64.MOVK (ARM64.X9, 0x6962, 48);
                ARM64.STR (ARM64.X9, ARM64.SP, 8);

                ARM64.MOVZ (ARM64.X9, 0x006E, 0);
                ARM64.STR (ARM64.X9, ARM64.SP, 16);


                ARM64.ADRP (ARM64.X10, Symbolic.coverageDataLabelName);
                ARM64.ADD_label (ARM64.X10, ARM64.X10, Symbolic.coverageDataLabelName);


                ARM64.MOV_reg (ARM64.X0, ARM64.SP);
                ARM64.MOVZ (ARM64.X1, writeFlags, 0);
                ARM64.MOVZ (ARM64.X2, 420, 0);
                ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.open_, 0);
                ARM64.SVC syscalls.ARM64.svcImmediate;


                ARM64.TBNZ (ARM64.X0, 63, 6);


                ARM64.MOV_reg (ARM64.X11, ARM64.X0);


                ARM64.MOV_reg (ARM64.X0, ARM64.X11);
                ARM64.MOV_reg (ARM64.X1, ARM64.X10);
            ] @
            generateLoadNonNegativeIntImmediate ARM64.X2 byteCount @
            [
                ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.write, 0);
                ARM64.SVC syscalls.ARM64.svcImmediate;


                ARM64.MOV_reg (ARM64.X0, ARM64.X11);
                ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.close, 0);
                ARM64.SVC syscalls.ARM64.svcImmediate;


                ARM64.ADD_imm (ARM64.SP, ARM64.SP, 24);
            ]
