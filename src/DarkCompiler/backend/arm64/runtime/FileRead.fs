// FileRead.fs - Generate checked file-read runtime operations.

module ARM64FileRead

/// Generate ARM64 instructions to read file contents and return Result<Blob, String>
/// destReg: destination register for the Result pointer
/// pathReg: register containing heap string pointer to file path
///
/// Result memory layout: [tag:8][payload:8][refcount:8] = 24 bytes
/// - tag 0 = Ok, tag 1 = Error
/// - payload = pointer to string
///
/// String memory layout: [refcount:8][length:8][data:N]
///
/// Algorithm:
/// 1. Copy path to stack with null terminator (same as FileExists)
/// 2. open() syscall - if fails, return Error("File not found")
/// 3. fstat() syscall - get file size
/// 4. Allocate string buffer on heap
/// 5. read() syscall - read file contents
/// 6. close() syscall
/// 7. Construct Result with Ok(string) or Error(message)
let generateFileReadBlob (target: ARM64.TargetConfig) (destReg: ARM64.Reg) (pathReg: ARM64.Reg) : ARM64.Instr list =
    let os = ARM64.targetOS target
    let syscalls = ARM64.targetSyscalls target

    // For now, implement a simplified version that:
    // - Opens the file
    // - Reads up to 4096 bytes
    // - Returns Ok(contents) or Error("File not found")
    //
    // Stack layout after the callee-save area: stat buffer (144), PATH_MAX
    // path buffer (4096), and caller-save spill area (64).
    // (stat buffer is 128 bytes on Linux, 144 on macOS - use 144 for safety)
    match os with
    | Platform.Linux ->
        [
            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48s)
            ARM64.STP (ARM64.X25, ARM64.X26, ARM64.SP, -64s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 64us)

            // Allocate stat buffer (144) + path (4096) + caller-saved regs (64).
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 4095us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 209us)

            // X19 = path heap string, X20 = dest register, X21 = file descriptor
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // Save ALL potentially live caller-saved registers (X1-X8) at SP+400
            ARM64.STR (ARM64.X1, ARM64.SP, 4240s)
            ARM64.STR (ARM64.X2, ARM64.SP, 4248s)
            ARM64.STR (ARM64.X3, ARM64.SP, 4256s)
            ARM64.STR (ARM64.X4, ARM64.SP, 4264s)
            ARM64.STR (ARM64.X5, ARM64.SP, 4272s)
            ARM64.STR (ARM64.X6, ARM64.SP, 4280s)
            ARM64.STR (ARM64.X7, ARM64.SP, 4288s)
            ARM64.STR (ARM64.X8, ARM64.SP, 4296s)

            // Use non-allocatable registers for copy loop
            // X10 = string length from heap string
            ARM64.LDR (ARM64.X10, ARM64.X19, 8s)

            // X9 = stack dest for null-terminated path (SP + 144 for after stat buffer)
            ARM64.ADD_imm (ARM64.X9, ARM64.SP, 144us)
            // X11 = heap string data (X19 + 16)
            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 16us)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X10, 7)     // 0: If length == 0, skip to null_term
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1us)
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1us)
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X12, 0us, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)

            // openat(AT_FDCWD, path, O_RDONLY, 0)
            // X0 = dirfd (AT_FDCWD = -100)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)
            // X1 = path
            ARM64.ADD_imm (ARM64.X1, ARM64.SP, 144us)
            // X2 = flags (O_RDONLY = 0)
            ARM64.MOVZ (ARM64.X2, 0us, 0)
            // X3 = mode (not used for O_RDONLY)
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            // syscall
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Check if open failed (fd < 0 means X0 has sign bit set)
            // X21 = fd
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)
            ARM64.TBNZ (ARM64.X0, 63, 29)  // If negative, branch to error path

            // fstat(fd, statbuf) - X0 = fd, X1 = statbuf
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // stat buffer at SP
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Fstat, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Get file size from stat buffer (st_size is at offset 48 on Linux ARM64)
            ARM64.LDR (ARM64.X22, ARM64.SP, 48s)  // X22 = file size

            // Allocate heap space for string: [refcount:8][len:8][data:N]
            // Size = 8 + size + 8 = size + 16, round up to next 8 bytes
            // Simpler: just add 24 (16 + 8 for alignment padding)
            ARM64.ADD_imm (ARM64.X23, ARM64.X22, 24us)  // X23 = size + 24 (with padding)

            // Allocate from heap (bump allocator)
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)  // X24 = string pointer
            ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X23)  // bump heap pointer

            // Store the fixed dynamic-buffer header.
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)
            ARM64.STR (ARM64.X22, ARM64.X24, 8s)

            // read(fd, buf, count) - read file contents
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)  // fd
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 16us)  // buf = string data area
            ARM64.MOV_reg (ARM64.X2, ARM64.X22)  // count = file size
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Read, 0)
            ARM64.SVC syscalls.SvcImmediate

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Close, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Allocate Result: [tag:8][payload:8][refcount:8] = 24 bytes
            ARM64.MOV_reg (ARM64.X25, ARM64.X28)  // X25 = Result pointer
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)  // bump heap

            // Store Ok tag (0)
            ARM64.MOVZ (ARM64.X0, 0us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)  // tag = 0

            // Store string pointer as payload
            ARM64.STR (ARM64.X24, ARM64.X25, 8s)  // payload = string ptr

            // Store refcount = 1
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 16s)

            // Move result to dest
            ARM64.MOV_reg (ARM64.X20, ARM64.X25)

            // Jump to cleanup
            ARM64.B 24  // Skip error path

            // === Error path (file not found) ===
            // Create error string "File not found" and Error result
            // For simplicity, create a short error message

            // Allocate error string: "File not found" = 14 bytes.
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)  // X24 = error string
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 32us)

            ARM64.MOVZ (ARM64.X0, 14us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 8s)

            ARM64.MOVZ (ARM64.X0, 0x6946us, 0)
            ARM64.MOVK (ARM64.X0, 0x656cus, 16)
            ARM64.MOVK (ARM64.X0, 0x6e20us, 32)
            ARM64.MOVK (ARM64.X0, 0x746fus, 48)
            ARM64.STR (ARM64.X0, ARM64.X24, 16s)
            ARM64.MOVZ (ARM64.X0, 0x6620us, 0)
            ARM64.MOVK (ARM64.X0, 0x756fus, 16)
            ARM64.MOVK (ARM64.X0, 0x646eus, 32)
            ARM64.STR (ARM64.X0, ARM64.X24, 24s)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            // Allocate Error Result
            ARM64.MOV_reg (ARM64.X25, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            // Store Error tag (1)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)

            // Store error string as payload
            ARM64.STR (ARM64.X24, ARM64.X25, 8s)

            // Store refcount = 1
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 16s)

            // Move result to dest
            ARM64.MOV_reg (ARM64.X20, ARM64.X25)

            // === Cleanup - save result to X0 before restoring callee-saved registers ===
            ARM64.MOV_reg (ARM64.X0, ARM64.X20)

            // Restore ALL caller-saved registers (X1-X8) we saved at start
            ARM64.LDR (ARM64.X1, ARM64.SP, 4240s)
            ARM64.LDR (ARM64.X2, ARM64.SP, 4248s)
            ARM64.LDR (ARM64.X3, ARM64.SP, 4256s)
            ARM64.LDR (ARM64.X4, ARM64.SP, 4264s)
            ARM64.LDR (ARM64.X5, ARM64.SP, 4272s)
            ARM64.LDR (ARM64.X6, ARM64.SP, 4280s)
            ARM64.LDR (ARM64.X7, ARM64.SP, 4288s)
            ARM64.LDR (ARM64.X8, ARM64.SP, 4296s)

            // Deallocate 464-byte stack buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 4095us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 209us)

            // Restore callee-saved registers
            ARM64.LDP (ARM64.X25, ARM64.X26, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 32s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 48s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 64us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]
    | Platform.MacOS ->
        // macOS version - similar structure with different syscall numbers
        [
            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48s)
            ARM64.STP (ARM64.X25, ARM64.X26, ARM64.SP, -64s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 64us)

            // Allocate stat buffer (144) + path (4096) + caller-saved regs (64).
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 4095us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 209us)

            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // Save ALL potentially live caller-saved registers (X1-X8) at SP+400
            ARM64.STR (ARM64.X1, ARM64.SP, 4240s)
            ARM64.STR (ARM64.X2, ARM64.SP, 4248s)
            ARM64.STR (ARM64.X3, ARM64.SP, 4256s)
            ARM64.STR (ARM64.X4, ARM64.SP, 4264s)
            ARM64.STR (ARM64.X5, ARM64.SP, 4272s)
            ARM64.STR (ARM64.X6, ARM64.SP, 4280s)
            ARM64.STR (ARM64.X7, ARM64.SP, 4288s)
            ARM64.STR (ARM64.X8, ARM64.SP, 4296s)

            // Copy path with null terminator using non-allocatable registers
            // X10 = string length
            ARM64.LDR (ARM64.X10, ARM64.X19, 8s)
            // X9 = dest (SP + 144)
            ARM64.ADD_imm (ARM64.X9, ARM64.SP, 144us)
            // X11 = source (X19 + 16)
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

            // open(path, O_RDONLY)
            ARM64.ADD_imm (ARM64.X0, ARM64.SP, 144us)
            ARM64.MOVZ (ARM64.X1, 0us, 0)  // O_RDONLY
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            ARM64.MOV_reg (ARM64.X21, ARM64.X0)
            ARM64.TBNZ (ARM64.X0, 63, 29)  // If negative, branch to error path

            // fstat(fd, statbuf)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Fstat, 0)
            ARM64.SVC syscalls.SvcImmediate

            // st_size at offset 96 on macOS
            ARM64.LDR (ARM64.X22, ARM64.SP, 96s)

            // Allocate string: size + 24 (with padding for alignment)
            ARM64.ADD_imm (ARM64.X23, ARM64.X22, 24us)

            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X23)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)
            ARM64.STR (ARM64.X22, ARM64.X24, 8s)

            // read
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 16us)
            ARM64.MOV_reg (ARM64.X2, ARM64.X22)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Read, 0)
            ARM64.SVC syscalls.SvcImmediate

            // close
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Close, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Allocate Result
            ARM64.MOV_reg (ARM64.X25, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 0us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)
            ARM64.STR (ARM64.X24, ARM64.X25, 8s)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 16s)

            ARM64.MOV_reg (ARM64.X20, ARM64.X25)

            ARM64.B 24  // Skip error path

            // Error path (same as Linux)
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 32us)

            ARM64.MOVZ (ARM64.X0, 14us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 8s)

            ARM64.MOVZ (ARM64.X0, 0x6946us, 0)
            ARM64.MOVK (ARM64.X0, 0x656cus, 16)
            ARM64.MOVK (ARM64.X0, 0x6e20us, 32)
            ARM64.MOVK (ARM64.X0, 0x746fus, 48)
            ARM64.STR (ARM64.X0, ARM64.X24, 16s)
            ARM64.MOVZ (ARM64.X0, 0x6620us, 0)
            ARM64.MOVK (ARM64.X0, 0x756fus, 16)
            ARM64.MOVK (ARM64.X0, 0x646eus, 32)
            ARM64.STR (ARM64.X0, ARM64.X24, 24s)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            ARM64.MOV_reg (ARM64.X25, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)
            ARM64.STR (ARM64.X24, ARM64.X25, 8s)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 16s)

            ARM64.MOV_reg (ARM64.X20, ARM64.X25)

            // Cleanup - save result to X0 before restoring callee-saved registers
            ARM64.MOV_reg (ARM64.X0, ARM64.X20)

            // Restore ALL caller-saved registers (X1-X8) we saved at start
            ARM64.LDR (ARM64.X1, ARM64.SP, 4240s)
            ARM64.LDR (ARM64.X2, ARM64.SP, 4248s)
            ARM64.LDR (ARM64.X3, ARM64.SP, 4256s)
            ARM64.LDR (ARM64.X4, ARM64.SP, 4264s)
            ARM64.LDR (ARM64.X5, ARM64.SP, 4272s)
            ARM64.LDR (ARM64.X6, ARM64.SP, 4280s)
            ARM64.LDR (ARM64.X7, ARM64.SP, 4288s)
            ARM64.LDR (ARM64.X8, ARM64.SP, 4296s)

            // Deallocate 464-byte stack buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 4095us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 209us)

            ARM64.LDP (ARM64.X25, ARM64.X26, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 32s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 48s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 64us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]
