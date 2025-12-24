// Runtime.fs - Runtime Support Functions
//
// Provides runtime support for language features that require complex
// instruction sequences (e.g., I/O, string conversion).
//
// Current functions:
// - generatePrintInt: Convert int64 in X0 to ASCII and print to stdout
//
// Platform-specific:
// - Uses Platform.fs to get correct syscall numbers for the target OS

module Runtime

/// Generate ARM64 instructions to print int64 in X0 to stdout with newline
/// Then exit with code 0
///
/// Algorithm:
/// 1. Allocate 32-byte stack buffer
/// 2. Convert X0 (int64) to ASCII decimal string (work backwards from end)
/// 3. Handle negative numbers (negate value, prepend '-')
/// 4. Handle special case: zero
/// 5. Write buffer to stdout via write syscall
/// 6. Exit with code 0 via exit syscall
///
/// Register usage:
/// - X0: Input value, then stdout fd, then exit code
/// - X1: Buffer pointer (works backwards from end)
/// - X2: Working value / length / temp
/// - X3: Divisor (10) / digit / temp
/// - X4: Quotient from division
/// - X5: Remainder (digit to store)
/// - X6: Negative flag (0 = positive, 1 = negative)
/// - X16: Syscall number
///
/// Uses platform-specific syscall numbers from Platform module
let generatePrintInt () : ARM64.Instr list =
    // Platform detection at runtime - if it fails, default to Linux (most common for development)
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux  // Fallback to Linux if platform detection fails
    let syscalls = Platform.getSyscallNumbers os
    [
        // Allocate 32 bytes on stack for buffer (plenty for 64-bit number + sign + newline)
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

        // Setup: X1 = buffer pointer (start at end, work backwards)
        // X2 = value to print (from X0)
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)  // X1 = SP + 31 (end of buffer)
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)  // X2 = value to convert

        // Store newline at end of buffer
        ARM64.MOVZ (ARM64.X3, 10us, 0)  // '\n' = 10
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // Move pointer back

        // Instruction 6-7: Check if negative and set flag in X6
        ARM64.MOVZ (ARM64.X6, 0us, 0)  // 6: X6 = 0 (assume positive)
        ARM64.TBNZ (ARM64.X2, 63, 29)  // 7: If negative, branch forward +29 to inst 36 (handle_negative)

        // Instruction 8: Check if zero (branch forward +24 to print_zero at inst 32)
        ARM64.CBZ_offset (ARM64.X2, 24)

        // Instruction 9-17: convert_loop - Extract digits by dividing by 10
        ARM64.MOVZ (ARM64.X3, 10us, 0)  // 9: divisor = 10
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)  // 10: X4 = value / 10
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)  // 11: X5 = value % 10
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)  // 12: Convert to ASCII
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)  // 13: Store digit
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // 14: Move pointer back
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)  // 15: value = value / 10
        ARM64.CBZ_offset (ARM64.X2, 2)  // 16: If zero, skip (+2) to store_minus_if_needed at inst 18
        ARM64.B (-8)  // 17: Loop back (-8) to inst 9 (convert_loop start)

        // Instruction 18-21: store_minus_if_needed - If X6=1 (was negative), store '-'
        ARM64.CBZ_offset (ARM64.X6, 4)  // 18: If X6=0 (positive), skip (+4) to write_output at inst 22
        ARM64.MOVZ (ARM64.X3, 45us, 0)  // 19: '-' = ASCII 45
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)  // 20: Store '-' at X1
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // 21: Move pointer back

        // Instruction 22-31: write_output - Write buffer to stdout
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)  // 22: X1 was one past first char
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32us)  // 23: End of buffer
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)  // 24: length = end - start
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // 25: stdout = 1
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)  // 26: write syscall (X16 macOS, X8 Linux)
        ARM64.SVC syscalls.SvcImmediate  // 27: call write (platform-specific)
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)  // 28: Deallocate stack
        ARM64.MOVZ (ARM64.X0, 0us, 0)  // 29: exit code = 0
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Exit, 0)  // 30: exit syscall (X16 macOS, X8 Linux)
        ARM64.SVC syscalls.SvcImmediate  // 31: call exit (platform-specific)

        // Instruction 32-35: print_zero - Special case for value 0
        ARM64.MOVZ (ARM64.X2, 48us, 0)  // 32: '0' = 48
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)  // 33: Store '0'
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // 34: Move pointer back
        ARM64.B (-13)  // 35: Branch back (-13) to inst 22 (write_output)

        // Instruction 36-38: handle_negative - Negate value and set flag
        ARM64.NEG (ARM64.X2, ARM64.X2)  // 36: X2 = -X2 (get absolute value)
        ARM64.MOVZ (ARM64.X6, 1us, 0)  // 37: X6 = 1 (negative flag)
        ARM64.B (-30)  // 38: Branch back (-30) to inst 8 (CBZ zero check)
    ]
