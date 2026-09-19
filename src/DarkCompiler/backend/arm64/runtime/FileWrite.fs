// FileWrite.fs - Generate checked file-write runtime operations.

module ARM64FileWrite

/// Generate ARM64 instructions to write content to a file
/// pathReg: register containing pointer to heap string (path)
/// contentReg: register containing pointer to heap string (content)
/// append: if true, append to file; if false, overwrite
/// Returns Result<Unit, String> in destReg
/// On success, result contains Ok(()) - tag=0, payload=0
/// On failure, result contains Error("Error") - tag=1, payload=error string ptr
let generateFileWriteBlob (target: ARM64.TargetConfig) (destReg: ARM64.Reg) (pathReg: ARM64.Reg) (contentReg: ARM64.Reg) (append: bool) : ARM64.Instr list =
    let os = ARM64.targetOS target
    let syscalls = ARM64.targetSyscalls target

    // Open flags:
    // Write: O_WRONLY | O_CREAT | O_TRUNC
    // Append: O_WRONLY | O_CREAT | O_APPEND
    // Linux: O_WRONLY=1, O_CREAT=64, O_TRUNC=512, O_APPEND=1024
    // macOS: O_WRONLY=1, O_CREAT=0x200, O_TRUNC=0x400, O_APPEND=8
    let (writeFlags, appendFlags) =
        match os with
        | Platform.Linux -> (577us, 1089us)  // 1|64|512, 1|64|1024
        | Platform.MacOS -> (1537us, 521us)  // 1|0x200|0x400, 1|0x200|8
    let flags = if append then appendFlags else writeFlags

    match os with
    | Platform.Linux ->
        [
            // Preserve both inputs before claiming callee-saved working registers;
            // either input may itself have been allocated to X19 or X22.
            ARM64.MOV_reg (ARM64.X16, pathReg)
            ARM64.MOV_reg (ARM64.X17, contentReg)

            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)

            // Allocate PATH_MAX path buffer (4096) + caller-saved regs (64).
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 4095us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 65us)

            // Save path and content pointers
            ARM64.MOV_reg (ARM64.X19, ARM64.X16)
            ARM64.MOV_reg (ARM64.X22, ARM64.X17)

            // Save ALL potentially live caller-saved registers (X1-X8) at SP+256
            ARM64.STR (ARM64.X1, ARM64.SP, 4096s)
            ARM64.STR (ARM64.X2, ARM64.SP, 4104s)
            ARM64.STR (ARM64.X3, ARM64.SP, 4112s)
            ARM64.STR (ARM64.X4, ARM64.SP, 4120s)
            ARM64.STR (ARM64.X5, ARM64.SP, 4128s)
            ARM64.STR (ARM64.X6, ARM64.SP, 4136s)
            ARM64.STR (ARM64.X7, ARM64.SP, 4144s)
            ARM64.STR (ARM64.X8, ARM64.SP, 4152s)

            // Copy path to stack buffer with null terminator using non-allocatable registers
            // X10 = path length
            ARM64.LDR (ARM64.X10, ARM64.X19, 8s)
            // X9 = dest buffer (SP)
            ARM64.MOV_reg (ARM64.X9, ARM64.SP)
            // X11 = source data (X19 + 16)
            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 16us)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X10, 7)
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1us)
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1us)
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X12, 0us, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)

            // openat(AT_FDCWD, path, flags, mode)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)  // AT_FDCWD = -100
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // path
            ARM64.MOVZ (ARM64.X2, flags, 0)  // flags
            ARM64.MOVZ (ARM64.X3, 420us, 0)  // mode 0644
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Check if open failed
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)  // X21 = fd
            ARM64.TBNZ (ARM64.X0, 63, 18)  // If negative, branch to error path (17 instructions + 1)

            // write(fd, buf, count)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)  // fd
            ARM64.ADD_imm (ARM64.X1, ARM64.X22, 16us)  // buf = content data
            ARM64.LDR (ARM64.X2, ARM64.X22, 8s)  // count = content length
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Write, 0)
            ARM64.SVC syscalls.SvcImmediate

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Close, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Allocate Ok Result: [tag=0][payload=0][refcount=1]
            ARM64.MOV_reg (ARM64.X23, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X0, 0us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 0s)  // tag = 0 (Ok)
            ARM64.STR (ARM64.X0, ARM64.X23, 8s)  // payload = 0 (Unit)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 16s)  // refcount = 1
            ARM64.MOV_reg (ARM64.X20, ARM64.X23)
            ARM64.B 29  // Jump to cleanup

            // Error path: Create Error("Error") result
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 5us, 0)  // length = 5
            ARM64.STR (ARM64.X0, ARM64.X24, 8s)

            // Store "Error" bytes
            ARM64.MOVZ (ARM64.X0, 69us, 0)  // 'E'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 16us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)  // 'r'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 17us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 18us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 111us, 0)  // 'o'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 19us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)  // 'r'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 20us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)  // refcount

            // Allocate Error Result
            ARM64.MOV_reg (ARM64.X23, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 0s)  // tag = 1 (Error)
            ARM64.STR (ARM64.X24, ARM64.X23, 8s)  // payload = error string
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 16s)  // refcount
            ARM64.MOV_reg (ARM64.X20, ARM64.X23)

            // Cleanup - save result to X0 before restoring callee-saved registers
            ARM64.MOV_reg (ARM64.X0, ARM64.X20)

            // Restore ALL caller-saved registers (X1-X8) we saved at start
            ARM64.LDR (ARM64.X1, ARM64.SP, 4096s)
            ARM64.LDR (ARM64.X2, ARM64.SP, 4104s)
            ARM64.LDR (ARM64.X3, ARM64.SP, 4112s)
            ARM64.LDR (ARM64.X4, ARM64.SP, 4120s)
            ARM64.LDR (ARM64.X5, ARM64.SP, 4128s)
            ARM64.LDR (ARM64.X6, ARM64.SP, 4136s)
            ARM64.LDR (ARM64.X7, ARM64.SP, 4144s)
            ARM64.LDR (ARM64.X8, ARM64.SP, 4152s)

            // Deallocate 320-byte stack buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 4095us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 65us)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 32s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]
    | Platform.MacOS ->
        [
            // Preserve both inputs before claiming callee-saved working registers;
            // either input may itself have been allocated to X19 or X22.
            ARM64.MOV_reg (ARM64.X16, pathReg)
            ARM64.MOV_reg (ARM64.X17, contentReg)

            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)

            // Allocate PATH_MAX path buffer (4096) + caller-saved regs (64).
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 4095us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 65us)

            // Save path and content pointers
            ARM64.MOV_reg (ARM64.X19, ARM64.X16)
            ARM64.MOV_reg (ARM64.X22, ARM64.X17)

            // Save ALL potentially live caller-saved registers (X1-X8) at SP+256
            ARM64.STR (ARM64.X1, ARM64.SP, 4096s)
            ARM64.STR (ARM64.X2, ARM64.SP, 4104s)
            ARM64.STR (ARM64.X3, ARM64.SP, 4112s)
            ARM64.STR (ARM64.X4, ARM64.SP, 4120s)
            ARM64.STR (ARM64.X5, ARM64.SP, 4128s)
            ARM64.STR (ARM64.X6, ARM64.SP, 4136s)
            ARM64.STR (ARM64.X7, ARM64.SP, 4144s)
            ARM64.STR (ARM64.X8, ARM64.SP, 4152s)

            // Copy path to stack buffer using non-allocatable registers
            // X10 = path length
            ARM64.LDR (ARM64.X10, ARM64.X19, 8s)
            // X9 = dest buffer (SP)
            ARM64.MOV_reg (ARM64.X9, ARM64.SP)
            // X11 = source data (X19 + 16)
            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 16us)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X10, 7)
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1us)
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1us)
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X12, 0us, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)

            // open(path, flags, mode)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X1, flags, 0)
            ARM64.MOVZ (ARM64.X2, 420us, 0)  // mode 0644
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            ARM64.MOV_reg (ARM64.X21, ARM64.X0)
            ARM64.TBNZ (ARM64.X0, 63, 18)  // If negative, branch to error path (17 instructions + 1)

            // write(fd, buf, count)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.ADD_imm (ARM64.X1, ARM64.X22, 16us)
            ARM64.LDR (ARM64.X2, ARM64.X22, 8s)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Write, 0)
            ARM64.SVC syscalls.SvcImmediate

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Close, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Allocate Ok Result
            ARM64.MOV_reg (ARM64.X23, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X0, 0us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 0s)
            ARM64.STR (ARM64.X0, ARM64.X23, 8s)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 16s)
            ARM64.MOV_reg (ARM64.X20, ARM64.X23)
            ARM64.B 29  // Jump to cleanup

            // Error path
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 5us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 8s)

            ARM64.MOVZ (ARM64.X0, 69us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 16us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 17us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 18us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 111us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 19us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 20us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            ARM64.MOV_reg (ARM64.X23, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 0s)
            ARM64.STR (ARM64.X24, ARM64.X23, 8s)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 16s)
            ARM64.MOV_reg (ARM64.X20, ARM64.X23)

            // Cleanup - save result to X0 before restoring callee-saved registers
            ARM64.MOV_reg (ARM64.X0, ARM64.X20)

            // Restore ALL caller-saved registers (X1-X8) we saved at start
            ARM64.LDR (ARM64.X1, ARM64.SP, 4096s)
            ARM64.LDR (ARM64.X2, ARM64.SP, 4104s)
            ARM64.LDR (ARM64.X3, ARM64.SP, 4112s)
            ARM64.LDR (ARM64.X4, ARM64.SP, 4120s)
            ARM64.LDR (ARM64.X5, ARM64.SP, 4128s)
            ARM64.LDR (ARM64.X6, ARM64.SP, 4136s)
            ARM64.LDR (ARM64.X7, ARM64.SP, 4144s)
            ARM64.LDR (ARM64.X8, ARM64.SP, 4152s)

            // Deallocate 320-byte stack buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 4095us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 65us)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 32s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]
