(*
   FileRead.ml - Generate checked file-read runtime operations.
   Generate ARM64 instructions to read file contents and return Result<Blob, String>
   destReg: destination register for the Result pointer
   pathReg: register containing heap string pointer to file path
   Result memory layout: [tag:8][payload:8][refcount:8] = 24 bytes
   - tag 0 = Ok, tag 1 = Error
   - payload = pointer to string
   String memory layout: [refcount:8][length:8][data:N]
   Algorithm:
   1. Copy path to stack with null terminator (same as FileExists)
   2. open() syscall - if fails, return Error("File not found")
   3. fstat() syscall - get file size
   4. Allocate string buffer on heap
   5. read() syscall - read file contents
   6. close() syscall
   7. Construct Result with Ok(string) or Error(message)
   For now, implement a simplified version that:
   - Opens the file
   - Reads up to 4096 bytes
   - Returns Ok(contents) or Error("File not found")
   Stack layout after the callee-save area: stat buffer (144), PATH_MAX
   path buffer (4096), and caller-save spill area (64).
   (stat buffer is 128 bytes on Linux, 144 on macOS - use 144 for safety)
   Save callee-saved registers
   Allocate stat buffer (144) + path (4096) + caller-saved regs (64).
   X19 = path heap string, X20 = dest register, X21 = file descriptor
   Save ALL potentially live caller-saved registers (X1-X8) at SP+400
   Use non-allocatable registers for copy loop
   X10 = string length from heap string
   X9 = stack dest for null-terminated path (SP + 144 for after stat buffer)
   X11 = heap string data (X19 + 16)
   Copy loop (7 instructions, 0-6)
   0: If length == 0, skip to null_term
   Store null terminator
   openat(AT_FDCWD, path, O_RDONLY, 0)
   X0 = dirfd (AT_FDCWD = -100)
   X1 = path
   X2 = flags (O_RDONLY = 0)
   X3 = mode (not used for O_RDONLY)
   syscall
   Check if open failed (fd < 0 means X0 has sign bit set)
   X21 = fd
   If negative, branch to error path
   fstat(fd, statbuf) - X0 = fd, X1 = statbuf
   stat buffer at SP
   Get file size from stat buffer (st_size is at offset 48 on Linux ARM64)
   X22 = file size
   Keep the next heap object word-aligned after the Blob payload.
   Allocate from heap (bump allocator)
   X24 = string pointer
   bump heap pointer
   Store the fixed dynamic-buffer header.
   read(fd, buf, count) - read file contents
   fd
   buf = string data area
   count = file size
   close(fd)
   Allocate Result: [tag:8][payload:8][refcount:8] = 24 bytes
   X25 = Result pointer
   bump heap
   Store Ok tag (0)
   tag = 0
   Store string pointer as payload
   payload = string ptr
   Store refcount = 1
   Move result to dest
   Jump to cleanup
   Skip error path
   === Error path (file not found) ===
   Create error string "File not found" and Error result
   For simplicity, create a short error message
   Allocate error string: "File not found" = 14 bytes.
   X24 = error string
   Allocate Error Result
   Store Error tag (1)
   Store error string as payload
   === Cleanup - save result to X0 before restoring callee-saved registers ===
   Restore ALL caller-saved registers (X1-X8) we saved at start
   Deallocate 464-byte stack buffer
   Restore callee-saved registers
   Move result to dest after restoration
   macOS version - similar structure with different syscall numbers
   Copy path with null terminator using non-allocatable registers
   X10 = string length
   X9 = dest (SP + 144)
   X11 = source (X19 + 16)
   open(path, O_RDONLY)
   O_RDONLY
   fstat(fd, statbuf)
   st_size at offset 96 on macOS
   read
   close
   Allocate Result
   Error path (same as Linux)
   Cleanup - save result to X0 before restoring callee-saved registers
*)
let generateFileReadBlob (target : ARM64.targetConfig) (destReg : ARM64.reg)
    (pathReg : ARM64.reg) =
  let os = ARM64.targetOS target in
  let syscalls = ARM64.targetSyscalls target in

  match os with
  | Platform.Linux ->
      [
        ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
        ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
        ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48);
        ARM64.STP (ARM64.X25, ARM64.X26, ARM64.SP, -64);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 64);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 4095);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 209);
        ARM64.MOV_reg (ARM64.X19, pathReg);
        ARM64.MOV_reg (ARM64.X20, destReg);
        ARM64.STR (ARM64.X1, ARM64.SP, 4240);
        ARM64.STR (ARM64.X2, ARM64.SP, 4248);
        ARM64.STR (ARM64.X3, ARM64.SP, 4256);
        ARM64.STR (ARM64.X4, ARM64.SP, 4264);
        ARM64.STR (ARM64.X5, ARM64.SP, 4272);
        ARM64.STR (ARM64.X6, ARM64.SP, 4280);
        ARM64.STR (ARM64.X7, ARM64.SP, 4288);
        ARM64.STR (ARM64.X8, ARM64.SP, 4296);
        ARM64.LDR (ARM64.X10, ARM64.X19, 8);
        ARM64.ADD_imm (ARM64.X9, ARM64.SP, 144);
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
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 144);
        ARM64.MOVZ (ARM64.X2, 0, 0);
        ARM64.MOVZ (ARM64.X3, 0, 0);
        ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.open_, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X21, ARM64.X0);
        ARM64.TBNZ (ARM64.X0, 63, 32);
        ARM64.MOV_reg (ARM64.X0, ARM64.X21);
        ARM64.MOV_reg (ARM64.X1, ARM64.SP);
        ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.fstat, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.LDR (ARM64.X22, ARM64.SP, 48);
        ARM64.ADD_imm (ARM64.X23, ARM64.X22, 7);
        ARM64.LSR_imm (ARM64.X23, ARM64.X23, 3);
        ARM64.LSL_imm (ARM64.X23, ARM64.X23, 3);
        ARM64.ADD_imm (ARM64.X23, ARM64.X23, 16);
        ARM64.MOV_reg (ARM64.X24, ARM64.X28);
        ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X23);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X24, 0);
        ARM64.STR (ARM64.X22, ARM64.X24, 8);
        ARM64.MOV_reg (ARM64.X0, ARM64.X21);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 16);
        ARM64.MOV_reg (ARM64.X2, ARM64.X22);
        ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.read, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X0, ARM64.X21);
        ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.close, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X25, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
        ARM64.MOVZ (ARM64.X0, 0, 0);
        ARM64.STR (ARM64.X0, ARM64.X25, 0);
        ARM64.STR (ARM64.X24, ARM64.X25, 8);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X25, 16);
        ARM64.MOV_reg (ARM64.X20, ARM64.X25);
        ARM64.B 24;
        ARM64.MOV_reg (ARM64.X24, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 32);
        ARM64.MOVZ (ARM64.X0, 14, 0);
        ARM64.STR (ARM64.X0, ARM64.X24, 8);
        ARM64.MOVZ (ARM64.X0, 0x6946, 0);
        ARM64.MOVK (ARM64.X0, 0x656c, 16);
        ARM64.MOVK (ARM64.X0, 0x6e20, 32);
        ARM64.MOVK (ARM64.X0, 0x746f, 48);
        ARM64.STR (ARM64.X0, ARM64.X24, 16);
        ARM64.MOVZ (ARM64.X0, 0x6620, 0);
        ARM64.MOVK (ARM64.X0, 0x756f, 16);
        ARM64.MOVK (ARM64.X0, 0x646e, 32);
        ARM64.STR (ARM64.X0, ARM64.X24, 24);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X24, 0);
        ARM64.MOV_reg (ARM64.X25, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X25, 0);
        ARM64.STR (ARM64.X24, ARM64.X25, 8);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X25, 16);
        ARM64.MOV_reg (ARM64.X20, ARM64.X25);
        ARM64.MOV_reg (ARM64.X0, ARM64.X20);
        ARM64.LDR (ARM64.X1, ARM64.SP, 4240);
        ARM64.LDR (ARM64.X2, ARM64.SP, 4248);
        ARM64.LDR (ARM64.X3, ARM64.SP, 4256);
        ARM64.LDR (ARM64.X4, ARM64.SP, 4264);
        ARM64.LDR (ARM64.X5, ARM64.SP, 4272);
        ARM64.LDR (ARM64.X6, ARM64.SP, 4280);
        ARM64.LDR (ARM64.X7, ARM64.SP, 4288);
        ARM64.LDR (ARM64.X8, ARM64.SP, 4296);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 4095);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 209);
        ARM64.LDP (ARM64.X25, ARM64.X26, ARM64.SP, 0);
        ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 16);
        ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 32);
        ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 48);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 64);
        ARM64.MOV_reg (destReg, ARM64.X0);
      ]
  | Platform.MacOS ->
      [
        ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
        ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
        ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48);
        ARM64.STP (ARM64.X25, ARM64.X26, ARM64.SP, -64);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 64);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 4095);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 209);
        ARM64.MOV_reg (ARM64.X19, pathReg);
        ARM64.MOV_reg (ARM64.X20, destReg);
        ARM64.STR (ARM64.X1, ARM64.SP, 4240);
        ARM64.STR (ARM64.X2, ARM64.SP, 4248);
        ARM64.STR (ARM64.X3, ARM64.SP, 4256);
        ARM64.STR (ARM64.X4, ARM64.SP, 4264);
        ARM64.STR (ARM64.X5, ARM64.SP, 4272);
        ARM64.STR (ARM64.X6, ARM64.SP, 4280);
        ARM64.STR (ARM64.X7, ARM64.SP, 4288);
        ARM64.STR (ARM64.X8, ARM64.SP, 4296);
        ARM64.LDR (ARM64.X10, ARM64.X19, 8);
        ARM64.ADD_imm (ARM64.X9, ARM64.SP, 144);
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
        ARM64.ADD_imm (ARM64.X0, ARM64.SP, 144);
        ARM64.MOVZ (ARM64.X1, 0, 0);
        ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.open_, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X21, ARM64.X0);
        ARM64.TBNZ (ARM64.X0, 63, 32);
        ARM64.MOV_reg (ARM64.X0, ARM64.X21);
        ARM64.MOV_reg (ARM64.X1, ARM64.SP);
        ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.fstat, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.LDR (ARM64.X22, ARM64.SP, 96);
        ARM64.ADD_imm (ARM64.X23, ARM64.X22, 7);
        ARM64.LSR_imm (ARM64.X23, ARM64.X23, 3);
        ARM64.LSL_imm (ARM64.X23, ARM64.X23, 3);
        ARM64.ADD_imm (ARM64.X23, ARM64.X23, 16);
        ARM64.MOV_reg (ARM64.X24, ARM64.X28);
        ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X23);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X24, 0);
        ARM64.STR (ARM64.X22, ARM64.X24, 8);
        ARM64.MOV_reg (ARM64.X0, ARM64.X21);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 16);
        ARM64.MOV_reg (ARM64.X2, ARM64.X22);
        ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.read, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X0, ARM64.X21);
        ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.close, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X25, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
        ARM64.MOVZ (ARM64.X0, 0, 0);
        ARM64.STR (ARM64.X0, ARM64.X25, 0);
        ARM64.STR (ARM64.X24, ARM64.X25, 8);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X25, 16);
        ARM64.MOV_reg (ARM64.X20, ARM64.X25);
        ARM64.B 24;
        ARM64.MOV_reg (ARM64.X24, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 32);
        ARM64.MOVZ (ARM64.X0, 14, 0);
        ARM64.STR (ARM64.X0, ARM64.X24, 8);
        ARM64.MOVZ (ARM64.X0, 0x6946, 0);
        ARM64.MOVK (ARM64.X0, 0x656c, 16);
        ARM64.MOVK (ARM64.X0, 0x6e20, 32);
        ARM64.MOVK (ARM64.X0, 0x746f, 48);
        ARM64.STR (ARM64.X0, ARM64.X24, 16);
        ARM64.MOVZ (ARM64.X0, 0x6620, 0);
        ARM64.MOVK (ARM64.X0, 0x756f, 16);
        ARM64.MOVK (ARM64.X0, 0x646e, 32);
        ARM64.STR (ARM64.X0, ARM64.X24, 24);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X24, 0);
        ARM64.MOV_reg (ARM64.X25, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X25, 0);
        ARM64.STR (ARM64.X24, ARM64.X25, 8);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X25, 16);
        ARM64.MOV_reg (ARM64.X20, ARM64.X25);
        ARM64.MOV_reg (ARM64.X0, ARM64.X20);
        ARM64.LDR (ARM64.X1, ARM64.SP, 4240);
        ARM64.LDR (ARM64.X2, ARM64.SP, 4248);
        ARM64.LDR (ARM64.X3, ARM64.SP, 4256);
        ARM64.LDR (ARM64.X4, ARM64.SP, 4264);
        ARM64.LDR (ARM64.X5, ARM64.SP, 4272);
        ARM64.LDR (ARM64.X6, ARM64.SP, 4280);
        ARM64.LDR (ARM64.X7, ARM64.SP, 4288);
        ARM64.LDR (ARM64.X8, ARM64.SP, 4296);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 4095);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 209);
        ARM64.LDP (ARM64.X25, ARM64.X26, ARM64.SP, 0);
        ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 16);
        ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 32);
        ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 48);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 64);
        ARM64.MOV_reg (destReg, ARM64.X0);
      ]
