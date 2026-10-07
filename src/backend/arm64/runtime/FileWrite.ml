(*
   FileWrite.ml - Generate checked file-write runtime operations.
   Generate ARM64 instructions to write content to a file
   pathReg: register containing pointer to heap string (path)
   contentReg: register containing pointer to heap string (content)
   append: if true, append to file; if false, overwrite
   Returns Result<Unit, String> in destReg
   On success, result contains Ok(()) - tag=0, payload=0
   On failure, result contains Error("Error") - tag=1, payload=error string ptr
   Open flags:
   Write: O_WRONLY | O_CREAT | O_TRUNC
   Append: O_WRONLY | O_CREAT | O_APPEND
   Linux: O_WRONLY=1, O_CREAT=64, O_TRUNC=512, O_APPEND=1024
   macOS: O_WRONLY=1, O_CREAT=0x200, O_TRUNC=0x400, O_APPEND=8
   1|64|512, 1|64|1024
   1|0x200|0x400, 1|0x200|8
   Preserve both inputs before claiming callee-saved working registers;
   either input may itself have been allocated to X19 or X22.
   Save callee-saved registers
   Allocate PATH_MAX path buffer (4096) + caller-saved regs (64).
   Save path and content pointers
   Save ALL potentially live caller-saved registers (X1-X8) at SP+256
   Copy path to stack buffer with null terminator using non-allocatable registers
   X10 = path length
   X9 = dest buffer (SP)
   X11 = source data (X19 + 16)
   Copy loop (7 instructions, 0-6)
   Store null terminator
   openat(AT_FDCWD, path, flags, mode)
   AT_FDCWD = -100
   path
   flags
   mode 0644
   Check if open failed
   X21 = fd
   If negative, branch to error path (17 instructions + 1)
   write(fd, buf, count)
   fd
   buf = content data
   count = content length
   close(fd)
   Allocate Ok Result: [tag=0][payload=0][refcount=1]
   tag = 0 (Ok)
   payload = 0 (Unit)
   refcount = 1
   Jump to cleanup
   Error path: Create Error("Error") result
   length = 5
   Store "Error" bytes
   'E'
   'r'
   'o'
   refcount
   Allocate Error Result
   tag = 1 (Error)
   payload = error string
   Cleanup - save result to X0 before restoring callee-saved registers
   Restore ALL caller-saved registers (X1-X8) we saved at start
   Deallocate 320-byte stack buffer
   Move result to dest after restoration
   Copy path to stack buffer using non-allocatable registers
   open(path, flags, mode)
   Allocate Ok Result
   Error path
*)
let generateFileWriteBlob (target : ARM64.targetConfig) (destReg : ARM64.reg)
    (pathReg : ARM64.reg) (contentReg : ARM64.reg) (append : bool) =
  let os = ARM64.targetOS target in
  let syscalls = ARM64.targetSyscalls target in

  let writeFlags, appendFlags =
    match os with Platform.Linux -> (577, 1089) | Platform.MacOS -> (1537, 521)
  in
  let flags = if append then appendFlags else writeFlags in

  match os with
  | Platform.Linux ->
      [
        ARM64.MOV_reg (ARM64.X16, pathReg);
        ARM64.MOV_reg (ARM64.X17, contentReg);
        ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
        ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
        ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 4095);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 65);
        ARM64.MOV_reg (ARM64.X19, ARM64.X16);
        ARM64.MOV_reg (ARM64.X22, ARM64.X17);
        ARM64.STR (ARM64.X1, ARM64.SP, 4096);
        ARM64.STR (ARM64.X2, ARM64.SP, 4104);
        ARM64.STR (ARM64.X3, ARM64.SP, 4112);
        ARM64.STR (ARM64.X4, ARM64.SP, 4120);
        ARM64.STR (ARM64.X5, ARM64.SP, 4128);
        ARM64.STR (ARM64.X6, ARM64.SP, 4136);
        ARM64.STR (ARM64.X7, ARM64.SP, 4144);
        ARM64.STR (ARM64.X8, ARM64.SP, 4152);
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
        ARM64.MOVZ (ARM64.X2, flags, 0);
        ARM64.MOVZ (ARM64.X3, 420, 0);
        ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.open_, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X21, ARM64.X0);
        ARM64.TBNZ (ARM64.X0, 63, 18);
        ARM64.MOV_reg (ARM64.X0, ARM64.X21);
        ARM64.ADD_imm (ARM64.X1, ARM64.X22, 16);
        ARM64.LDR (ARM64.X2, ARM64.X22, 8);
        ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X0, ARM64.X21);
        ARM64.MOVZ (ARM64.X8, syscalls.ARM64.numbers.Platform.close, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X23, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
        ARM64.MOVZ (ARM64.X0, 0, 0);
        ARM64.STR (ARM64.X0, ARM64.X23, 0);
        ARM64.STR (ARM64.X0, ARM64.X23, 8);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X23, 16);
        ARM64.MOV_reg (ARM64.X20, ARM64.X23);
        ARM64.B 29;
        ARM64.MOV_reg (ARM64.X24, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
        ARM64.MOVZ (ARM64.X0, 5, 0);
        ARM64.STR (ARM64.X0, ARM64.X24, 8);
        ARM64.MOVZ (ARM64.X0, 69, 0);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 16);
        ARM64.STRB_reg (ARM64.X0, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 114, 0);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 17);
        ARM64.STRB_reg (ARM64.X0, ARM64.X1);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 18);
        ARM64.STRB_reg (ARM64.X0, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 111, 0);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 19);
        ARM64.STRB_reg (ARM64.X0, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 114, 0);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 20);
        ARM64.STRB_reg (ARM64.X0, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X24, 0);
        ARM64.MOV_reg (ARM64.X23, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X23, 0);
        ARM64.STR (ARM64.X24, ARM64.X23, 8);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X23, 16);
        ARM64.MOV_reg (ARM64.X20, ARM64.X23);
        ARM64.MOV_reg (ARM64.X0, ARM64.X20);
        ARM64.LDR (ARM64.X1, ARM64.SP, 4096);
        ARM64.LDR (ARM64.X2, ARM64.SP, 4104);
        ARM64.LDR (ARM64.X3, ARM64.SP, 4112);
        ARM64.LDR (ARM64.X4, ARM64.SP, 4120);
        ARM64.LDR (ARM64.X5, ARM64.SP, 4128);
        ARM64.LDR (ARM64.X6, ARM64.SP, 4136);
        ARM64.LDR (ARM64.X7, ARM64.SP, 4144);
        ARM64.LDR (ARM64.X8, ARM64.SP, 4152);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 4095);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 65);
        ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 0);
        ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 16);
        ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 32);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48);
        ARM64.MOV_reg (destReg, ARM64.X0);
      ]
  | Platform.MacOS ->
      [
        ARM64.MOV_reg (ARM64.X16, pathReg);
        ARM64.MOV_reg (ARM64.X17, contentReg);
        ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16);
        ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32);
        ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 4095);
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 65);
        ARM64.MOV_reg (ARM64.X19, ARM64.X16);
        ARM64.MOV_reg (ARM64.X22, ARM64.X17);
        ARM64.STR (ARM64.X1, ARM64.SP, 4096);
        ARM64.STR (ARM64.X2, ARM64.SP, 4104);
        ARM64.STR (ARM64.X3, ARM64.SP, 4112);
        ARM64.STR (ARM64.X4, ARM64.SP, 4120);
        ARM64.STR (ARM64.X5, ARM64.SP, 4128);
        ARM64.STR (ARM64.X6, ARM64.SP, 4136);
        ARM64.STR (ARM64.X7, ARM64.SP, 4144);
        ARM64.STR (ARM64.X8, ARM64.SP, 4152);
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
        ARM64.MOVZ (ARM64.X1, flags, 0);
        ARM64.MOVZ (ARM64.X2, 420, 0);
        ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.open_, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X21, ARM64.X0);
        ARM64.TBNZ (ARM64.X0, 63, 18);
        ARM64.MOV_reg (ARM64.X0, ARM64.X21);
        ARM64.ADD_imm (ARM64.X1, ARM64.X22, 16);
        ARM64.LDR (ARM64.X2, ARM64.X22, 8);
        ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X0, ARM64.X21);
        ARM64.MOVZ (ARM64.X16, syscalls.ARM64.numbers.Platform.close, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.MOV_reg (ARM64.X23, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
        ARM64.MOVZ (ARM64.X0, 0, 0);
        ARM64.STR (ARM64.X0, ARM64.X23, 0);
        ARM64.STR (ARM64.X0, ARM64.X23, 8);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X23, 16);
        ARM64.MOV_reg (ARM64.X20, ARM64.X23);
        ARM64.B 29;
        ARM64.MOV_reg (ARM64.X24, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
        ARM64.MOVZ (ARM64.X0, 5, 0);
        ARM64.STR (ARM64.X0, ARM64.X24, 8);
        ARM64.MOVZ (ARM64.X0, 69, 0);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 16);
        ARM64.STRB_reg (ARM64.X0, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 114, 0);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 17);
        ARM64.STRB_reg (ARM64.X0, ARM64.X1);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 18);
        ARM64.STRB_reg (ARM64.X0, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 111, 0);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 19);
        ARM64.STRB_reg (ARM64.X0, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 114, 0);
        ARM64.ADD_imm (ARM64.X1, ARM64.X24, 20);
        ARM64.STRB_reg (ARM64.X0, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X24, 0);
        ARM64.MOV_reg (ARM64.X23, ARM64.X28);
        ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X23, 0);
        ARM64.STR (ARM64.X24, ARM64.X23, 8);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.STR (ARM64.X0, ARM64.X23, 16);
        ARM64.MOV_reg (ARM64.X20, ARM64.X23);
        ARM64.MOV_reg (ARM64.X0, ARM64.X20);
        ARM64.LDR (ARM64.X1, ARM64.SP, 4096);
        ARM64.LDR (ARM64.X2, ARM64.SP, 4104);
        ARM64.LDR (ARM64.X3, ARM64.SP, 4112);
        ARM64.LDR (ARM64.X4, ARM64.SP, 4120);
        ARM64.LDR (ARM64.X5, ARM64.SP, 4128);
        ARM64.LDR (ARM64.X6, ARM64.SP, 4136);
        ARM64.LDR (ARM64.X7, ARM64.SP, 4144);
        ARM64.LDR (ARM64.X8, ARM64.SP, 4152);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 4095);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 65);
        ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 0);
        ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 16);
        ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 32);
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48);
        ARM64.MOV_reg (destReg, ARM64.X0);
      ]
