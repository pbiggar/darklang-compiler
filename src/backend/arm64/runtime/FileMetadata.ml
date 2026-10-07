(*
   FileMetadata.ml - Generate file existence, removal, and permission operations.
   Generate ARM64 instructions for Stdlib.File.exists
   Input: pathReg contains heap string pointer (format: [refcount:8][len:8][data:N])
   Output: destReg = 1 if file exists, 0 if not
   Algorithm:
   1. Get data pointer from heap string (path + 16)
   2. Save the path string to a null-terminated temp buffer on stack
   3. Call access(path, F_OK) or faccessat(AT_FDCWD, path, F_OK, 0)
   4. If syscall returns 0 (success), set dest = 1; else dest = 0
   We need to null-terminate the string for the syscall
   Stack layout: [saved regs:16][path:256]
   Use simpler approach: just copy the string and add null terminator
   Note: This is a simplification - paths > 255 chars will be truncated
   Save callee-saved registers we'll use
   Allocate stack space for path (256) plus caller-saved registers (64).
   X19 = heap string base (pathReg), X20 = dest pointer for later
   Preserve live caller-saved registers across the delete syscall.
   Use non-allocatable registers for copy loop to avoid clobbering live values
   X10 = string length from [X19 + 8]
   X9 = dest pointer (SP)
   X11 = source pointer (X19 + 16)
   Copy loop: copies X10 bytes from X11 to X9
   copy_loop (7 instructions, 0-6):
   0: If length == 0, skip to null_term at inst 7
   1: Load byte from src
   2: Store byte to dest
   3: dest++
   4: src++
   5: len--
   6: Loop back to CBZ
   null_term: Store null terminator at X9
   7: X12 = 0
   8: Store 0 as null terminator
   Call access(path, F_OK)
   X0 = path (SP), X1 = mode (F_OK = 0)
   F_OK = 0
   access syscall
   If X0 == 0, file exists; else doesn't
   Use CMP + CSET to convert to boolean
   X0 = 1 if was 0, else 0
   Cleanup - restore registers before moving result to dest
   Deallocate path buffer
   Move result to dest after restoration
   Save callee-saved registers we'll use, plus extra space for caller-saved
   Save X21, X22 for preserving X1, X2
   Allocate stack space for path (256 bytes)
   X19 = heap string base (pathReg)
   Save potentially live caller-saved registers (X1-X4) to callee-saved registers
   This preserves any values that might be allocated to these registers
   Save X1
   Save X2
   Copy loop (7 instructions, 0-6)
   null_term: Store null terminator
   Call faccessat(AT_FDCWD, path, F_OK, 0)
   X0 = dirfd (AT_FDCWD = -100), X1 = path, X2 = mode (F_OK = 0), X3 = flags (0)
   X0 = -100 (AT_FDCWD)
   path
   flags = 0
   faccessat syscall
   Restore caller-saved registers
   Restore X1
   Restore X2
   Cleanup - restore callee-saved registers
*)
let generateFileExists (target: ARM64.targetConfig) (destReg: ARM64.reg) (pathReg: ARM64.reg) =
    let os = ARM64.targetOS target in
    let syscalls = ARM64.targetSyscalls target in

    match os with
    | Platform.MacOS ->
        [

            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 65);


            ARM64.MOV_reg (ARM64.X19, pathReg);
            ARM64.MOV_reg (ARM64.X20, destReg);


            ARM64.STR (ARM64.X1, ARM64.SP, 256);
            ARM64.STR (ARM64.X2, ARM64.SP, 264);
            ARM64.STR (ARM64.X3, ARM64.SP, 272);
            ARM64.STR (ARM64.X4, ARM64.SP, 280);
            ARM64.STR (ARM64.X5, ARM64.SP, 288);
            ARM64.STR (ARM64.X6, ARM64.SP, 296);
            ARM64.STR (ARM64.X7, ARM64.SP, 304);
            ARM64.STR (ARM64.X8, ARM64.SP, 312);



            ARM64.LDR (ARM64.X10, ARM64.X19, 8);


            ARM64.MOV_reg (ARM64.X9, ARM64.SP);

            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 16);



            ARM64.CBZ_offset (ARM64.X10, 7);
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0);
            ARM64.STRB (ARM64.X12, ARM64.X9, 0);
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1);
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1);
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1);
            ARM64.B (-6);


            ARM64.MOVZ (ARM64.X12, 0, 0);
            ARM64.STRB (ARM64.X12, ARM64.X9, 0);



            ARM64.MOV_reg (ARM64.X0, ARM64.SP);
            ARM64.MOVZ (ARM64.X1, 0, 0);
            ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.access, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;



            ARM64.CMP_imm (ARM64.X0, 0);
            ARM64.CSET (ARM64.X0, ARM64.EQ);


            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16);
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16);
            ARM64.MOV_reg (destReg, ARM64.X0);
        ]
    | Platform.Linux ->
        [

            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256);


            ARM64.MOV_reg (ARM64.X19, pathReg);



            ARM64.MOV_reg (ARM64.X21, ARM64.X1);
            ARM64.MOV_reg (ARM64.X22, ARM64.X2);



            ARM64.LDR (ARM64.X10, ARM64.X19, 8);


            ARM64.MOV_reg (ARM64.X9, ARM64.SP);

            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 16);


            ARM64.CBZ_offset (ARM64.X10, 7);
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0);
            ARM64.STRB (ARM64.X12, ARM64.X9, 0);
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1);
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1);
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1);
            ARM64.B (-6);


            ARM64.MOVZ (ARM64.X12, 0, 0);
            ARM64.STRB (ARM64.X12, ARM64.X9, 0);



            ARM64.MOVZ (ARM64.X0, 100, 0);
            ARM64.NEG (ARM64.X0, ARM64.X0);
            ARM64.MOV_reg (ARM64.X1, ARM64.SP);
            ARM64.MOVZ (ARM64.X2, 0, 0);
            ARM64.MOVZ (ARM64.X3, 0, 0);
            ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.access, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.CMP_imm (ARM64.X0, 0);
            ARM64.CSET (ARM64.X0, ARM64.EQ);


            ARM64.MOV_reg (ARM64.X1, ARM64.X21);
            ARM64.MOV_reg (ARM64.X2, ARM64.X22);


            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 0);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16);
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16);
            ARM64.MOV_reg (destReg, ARM64.X0);
        ]

(*
   Generate ARM64 instructions to delete a file and return Result<Unit, String>
   destReg: destination register for the Result pointer
   pathReg: register containing heap string pointer to file path
   Result memory layout: [tag:8][payload:8][refcount:8] = 24 bytes
   - tag 0 = Ok, tag 1 = Error
   - payload = pointer to value (0 for Unit, string pointer for Error)
   Save callee-saved registers we'll use
   Allocate stack space for path (256) plus caller-saved registers (64).
   X19 = heap string base (pathReg), X20 = dest pointer for later
   Preserve live caller-saved registers across the chmod syscall.
   X2 = string length from [X19]
   X0 = dest pointer (SP)
   X1 = source pointer (X19 + 16)
   X4 = 0 (for LDRB offset)
   Copy loop: copies X2 bytes from X1 to X0
   copy_loop (7 instructions, 0-6):
   0: If length == 0, skip to null_term at inst 7
   1: Load byte from src [X1 + 0]
   2: Store byte to dest
   3: dest++
   4: src++
   5: len--
   6: Loop back to CBZ
   null_term: Store null terminator at X0
   X3 = 0
   Store null terminator
   Call unlink(path)
   X0 = path (SP)
   unlink syscall
   Save syscall result in X21
   Allocate Result on heap: [tag:8][payload:8][refcount:8] = 24 bytes
   X0 = result pointer
   bump allocator
   Check if unlink succeeded (X21 == 0)
   If failed, jump to error path
   Success path: Result = Ok(())
   tag = 0 (Ok)
   Store tag
   payload = 0 (Unit)
   refcount = 1
   Jump to cleanup (skip error path)
   Error path: Result = Error("Error")
   Allocate error string: "Error" (5 chars)
   String format: [refcount:8][length:8][data:N]
   X2 = error string pointer  (1)
   (2)
   length = 5  (3)
   Store length  (4)
   Store "Error" byte by byte: E=69, r=114, r=114, o=111, r=114
   'E'  (5)
   (6)
   (7)
   'r'  (8)
   (9)
   (10)
   'r'  (11)
   (12)
   (13)
   'o'  (14)
   (15)
   (16)
   'r'  (17)
   (18)
   (19)
   (20)
   refcount = 1  (21)
   Now store Result with error
   tag = 1 (Error)  (22)
   Store tag  (23)
   payload = error string pointer  (24)
   (25)
   refcount = 1  (26)
   Cleanup - restore registers and move result to dest
   Move result to dest after restoration
   X19 = heap string base (pathReg)
   Preserve live caller-saved registers across the delete syscall.
   Copy loop (7 instructions, 0-6)
   null_term: Store null terminator
   Call unlinkat(AT_FDCWD, path, 0)
   X0 = dirfd (AT_FDCWD = -100), X1 = path, X2 = flags (0)
   X0 = -100 (AT_FDCWD)
   path
   flags = 0
   unlinkat syscall
*)
let generateFileDelete (target: ARM64.targetConfig) (destReg: ARM64.reg) (pathReg: ARM64.reg) =
    let os = ARM64.targetOS target in
    let syscalls = ARM64.targetSyscalls target in

    match os with
    | Platform.MacOS ->
        [

            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 65);


            ARM64.MOV_reg (ARM64.X19, pathReg);
            ARM64.MOV_reg (ARM64.X20, destReg);


            ARM64.STR (ARM64.X1, ARM64.SP, 256);
            ARM64.STR (ARM64.X2, ARM64.SP, 264);
            ARM64.STR (ARM64.X3, ARM64.SP, 272);
            ARM64.STR (ARM64.X4, ARM64.SP, 280);
            ARM64.STR (ARM64.X5, ARM64.SP, 288);
            ARM64.STR (ARM64.X6, ARM64.SP, 296);
            ARM64.STR (ARM64.X7, ARM64.SP, 304);
            ARM64.STR (ARM64.X8, ARM64.SP, 312);


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
            ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.unlink, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.MOV_reg (ARM64.X21, ARM64.X0);


            ARM64.MOV_reg (ARM64.X0, ARM64.X28);
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);


            ARM64.CMP_imm (ARM64.X21, 0);
            ARM64.B_cond (ARM64.NE, 7);


            ARM64.MOVZ (ARM64.X1, 0, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 8);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 16);
            ARM64.B (27);

            ARM64.MOV_reg (ARM64.X2, ARM64.X28);
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
            ARM64.MOVZ (ARM64.X1, 5, 0);
            ARM64.STR (ARM64.X1, ARM64.X2, 8);

            ARM64.MOVZ (ARM64.X1, 69, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 16);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 17);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 18);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 111, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 19);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 20);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X2, 0);

            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 0);
            ARM64.STR (ARM64.X2, ARM64.X0, 8);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 16);


            ARM64.LDR (ARM64.X1, ARM64.SP, 256);
            ARM64.LDR (ARM64.X2, ARM64.SP, 264);
            ARM64.LDR (ARM64.X3, ARM64.SP, 272);
            ARM64.LDR (ARM64.X4, ARM64.SP, 280);
            ARM64.LDR (ARM64.X5, ARM64.SP, 288);
            ARM64.LDR (ARM64.X6, ARM64.SP, 296);
            ARM64.LDR (ARM64.X7, ARM64.SP, 304);
            ARM64.LDR (ARM64.X8, ARM64.SP, 312);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 65);
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 0);
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 16);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
            ARM64.MOV_reg (destReg, ARM64.X0);
        ]
    | Platform.Linux ->
        [

            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 65);


            ARM64.MOV_reg (ARM64.X19, pathReg);


            ARM64.STR (ARM64.X1, ARM64.SP, 256);
            ARM64.STR (ARM64.X2, ARM64.SP, 264);
            ARM64.STR (ARM64.X3, ARM64.SP, 272);
            ARM64.STR (ARM64.X4, ARM64.SP, 280);
            ARM64.STR (ARM64.X5, ARM64.SP, 288);
            ARM64.STR (ARM64.X6, ARM64.SP, 296);
            ARM64.STR (ARM64.X7, ARM64.SP, 304);
            ARM64.STR (ARM64.X8, ARM64.SP, 312);


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
            ARM64.MOVZ (ARM64.X2, 0, 0);
            ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.unlink, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.MOV_reg (ARM64.X21, ARM64.X0);


            ARM64.MOV_reg (ARM64.X0, ARM64.X28);
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);


            ARM64.CMP_imm (ARM64.X21, 0);
            ARM64.B_cond (ARM64.NE, 7);


            ARM64.MOVZ (ARM64.X1, 0, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 8);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 16);
            ARM64.B (27);


            ARM64.MOV_reg (ARM64.X2, ARM64.X28);
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
            ARM64.MOVZ (ARM64.X1, 5, 0);
            ARM64.STR (ARM64.X1, ARM64.X2, 8);

            ARM64.MOVZ (ARM64.X1, 69, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 16);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 17);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 18);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 111, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 19);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 20);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X2, 0);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 0);
            ARM64.STR (ARM64.X2, ARM64.X0, 8);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 16);


            ARM64.LDR (ARM64.X1, ARM64.SP, 256);
            ARM64.LDR (ARM64.X2, ARM64.SP, 264);
            ARM64.LDR (ARM64.X3, ARM64.SP, 272);
            ARM64.LDR (ARM64.X4, ARM64.SP, 280);
            ARM64.LDR (ARM64.X5, ARM64.SP, 288);
            ARM64.LDR (ARM64.X6, ARM64.SP, 296);
            ARM64.LDR (ARM64.X7, ARM64.SP, 304);
            ARM64.LDR (ARM64.X8, ARM64.SP, 312);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 65);
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 0);
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 16);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
            ARM64.MOV_reg (destReg, ARM64.X0);
        ]

(*
   Generate ARM64 instructions to set executable permission on a file
   destReg: destination register for the Result pointer
   pathReg: register containing heap string pointer to file path
   Result memory layout: [tag:8][payload:8][refcount:8] = 24 bytes
   - tag 0 = Ok, tag 1 = Error
   - payload = pointer to value (0 for Unit, string pointer for Error)
   Save callee-saved registers we'll use
   Allocate stack space for path (256 bytes, 16-byte aligned)
   X19 = heap string base (pathReg), X20 = dest pointer for later
   X2 = string length from [X19]
   X0 = dest pointer (SP)
   X1 = source pointer (X19 + 16)
   X4 = 0 (for LDRB offset)
   Copy loop: copies X2 bytes from X1 to X0
   null_term: Store null terminator at X0
   Call chmod(path, 0755)
   X0 = path (SP), X1 = mode (0755 = 493 in decimal)
   0755 = 493
   Save syscall result in X21
   Allocate Result on heap: [tag:8][payload:8][refcount:8] = 24 bytes
   Check if chmod succeeded (X21 == 0)
   Success path: Result = Ok(())
   Error path: Result = Error("Error")
   'E'
   'r'
   'o'
   Cleanup
   Allocate stack space for path (256) plus caller-saved registers (64).
   X19 = heap string base (pathReg)
   Preserve live caller-saved registers across the chmod syscall.
   Copy loop (7 instructions, 0-6)
   null_term: Store null terminator
   Call fchmodat(AT_FDCWD, path, 0755, 0)
   X0 = dirfd (AT_FDCWD = -100), X1 = path, X2 = mode (0755 = 493), X3 = flags (0)
   Allocate Result on heap
*)
let generateFileSetExecutable (target: ARM64.targetConfig) (destReg: ARM64.reg) (pathReg: ARM64.reg) =
    let os = ARM64.targetOS target in
    let syscalls = ARM64.targetSyscalls target in

    match os with
    | Platform.MacOS ->
        [

            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16);


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256);


            ARM64.MOV_reg (ARM64.X19, pathReg);
            ARM64.MOV_reg (ARM64.X20, destReg);


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
            ARM64.MOVZ (ARM64.X1, 493, 0);
            ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.chmod, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.MOV_reg (ARM64.X21, ARM64.X0);


            ARM64.MOV_reg (ARM64.X0, ARM64.X28);
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);


            ARM64.CMP_imm (ARM64.X21, 0);
            ARM64.B_cond (ARM64.NE, 7);


            ARM64.MOVZ (ARM64.X1, 0, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 8);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 16);
            ARM64.B (27);


            ARM64.MOV_reg (ARM64.X2, ARM64.X28);
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
            ARM64.MOVZ (ARM64.X1, 5, 0);
            ARM64.STR (ARM64.X1, ARM64.X2, 8);
            ARM64.MOVZ (ARM64.X1, 69, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 16);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 17);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 18);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 111, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 19);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 20);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X2, 0);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 0);
            ARM64.STR (ARM64.X2, ARM64.X0, 8);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 16);


            ARM64.LDR (ARM64.X1, ARM64.SP, 256);
            ARM64.LDR (ARM64.X2, ARM64.SP, 264);
            ARM64.LDR (ARM64.X3, ARM64.SP, 272);
            ARM64.LDR (ARM64.X4, ARM64.SP, 280);
            ARM64.LDR (ARM64.X5, ARM64.SP, 288);
            ARM64.LDR (ARM64.X6, ARM64.SP, 296);
            ARM64.LDR (ARM64.X7, ARM64.SP, 304);
            ARM64.LDR (ARM64.X8, ARM64.SP, 312);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 65);
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 0);
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 16);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
            ARM64.MOV_reg (destReg, ARM64.X0);
        ]
    | Platform.Linux ->
        [

            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);


            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 65);


            ARM64.MOV_reg (ARM64.X19, pathReg);


            ARM64.STR (ARM64.X1, ARM64.SP, 256);
            ARM64.STR (ARM64.X2, ARM64.SP, 264);
            ARM64.STR (ARM64.X3, ARM64.SP, 272);
            ARM64.STR (ARM64.X4, ARM64.SP, 280);
            ARM64.STR (ARM64.X5, ARM64.SP, 288);
            ARM64.STR (ARM64.X6, ARM64.SP, 296);
            ARM64.STR (ARM64.X7, ARM64.SP, 304);
            ARM64.STR (ARM64.X8, ARM64.SP, 312);


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
            ARM64.MOVZ (ARM64.X2, 493, 0);
            ARM64.MOVZ (ARM64.X3, 0, 0);
            ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.chmod, 0);
            ARM64.SVC syscalls.ARM64.svcImmediate;


            ARM64.MOV_reg (ARM64.X21, ARM64.X0);


            ARM64.MOV_reg (ARM64.X0, ARM64.X28);
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);


            ARM64.CMP_imm (ARM64.X21, 0);
            ARM64.B_cond (ARM64.NE, 7);


            ARM64.MOVZ (ARM64.X1, 0, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 8);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 16);
            ARM64.B (27);


            ARM64.MOV_reg (ARM64.X2, ARM64.X28);
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
            ARM64.MOVZ (ARM64.X1, 5, 0);
            ARM64.STR (ARM64.X1, ARM64.X2, 8);
            ARM64.MOVZ (ARM64.X1, 69, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 16);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 17);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 18);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 111, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 19);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 114, 0);
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 20);
            ARM64.STRB_reg (ARM64.X1, ARM64.X3);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X2, 0);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 0);
            ARM64.STR (ARM64.X2, ARM64.X0, 8);
            ARM64.MOVZ (ARM64.X1, 1, 0);
            ARM64.STR (ARM64.X1, ARM64.X0, 16);


            ARM64.LDR (ARM64.X1, ARM64.SP, 256);
            ARM64.LDR (ARM64.X2, ARM64.SP, 264);
            ARM64.LDR (ARM64.X3, ARM64.SP, 272);
            ARM64.LDR (ARM64.X4, ARM64.SP, 280);
            ARM64.LDR (ARM64.X5, ARM64.SP, 288);
            ARM64.LDR (ARM64.X6, ARM64.SP, 296);
            ARM64.LDR (ARM64.X7, ARM64.SP, 304);
            ARM64.LDR (ARM64.X8, ARM64.SP, 312);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 65);
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 0);
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 16);
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
            ARM64.MOV_reg (destReg, ARM64.X0);
        ]
