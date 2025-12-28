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

/// Generate ARM64 instructions to print boolean in X0 to stdout with newline
/// Then exit with code 0
///
/// Algorithm:
/// 1. Check if X0 == 0 (false) or != 0 (true)
/// 2. Store "true\n" or "false\n" on stack
/// 3. Write to stdout via write syscall
/// 4. Exit with code 0
///
/// Register usage:
/// - X0: Input boolean value (0=false, non-zero=true)
/// - X1: Buffer pointer
/// - X2: Length
/// - X3: Temp for storing chars
/// - X16/X8: Syscall number
let generatePrintBool () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        // Allocate 16 bytes on stack for buffer (16-byte aligned)
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

        // Check if value is false (X0 == 0)
        ARM64.CBZ_offset (ARM64.X0, 17)  // If false, branch to print_false (+17 instructions)

        // print_true: Store "true\n" on stack (5 bytes)
        // 't' = 116, 'r' = 114, 'u' = 117, 'e' = 101, '\n' = 10
        ARM64.MOVZ (ARM64.X3, 116us, 0)  // 't'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 114us, 0)  // 'r'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 117us, 0)  // 'u'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, 10us, 0)   // '\n'
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X2, 5us, 0)    // length = 5

        // Write and exit
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // buffer
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.B (16)  // Jump to cleanup_and_exit (+16 instructions)

        // print_false: Store "false\n" on stack (6 bytes)
        // 'f' = 102, 'a' = 97, 'l' = 108, 's' = 115, 'e' = 101, '\n' = 10
        ARM64.MOVZ (ARM64.X3, 102us, 0)  // 'f'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 97us, 0)   // 'a'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 108us, 0)  // 'l'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 115us, 0)  // 's'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X3, 10us, 0)   // '\n'
        ARM64.STRB (ARM64.X3, ARM64.SP, 5)
        ARM64.MOVZ (ARM64.X2, 6us, 0)    // length = 6

        // Write
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // buffer
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // cleanup_and_exit:
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)  // Deallocate stack
        ARM64.MOVZ (ARM64.X0, 0us, 0)  // exit code = 0
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Exit, 0)
        ARM64.SVC syscalls.SvcImmediate
    ]

/// Generate ARM64 instructions to print string to stdout with newline, then exit
///
/// Assumes:
/// - X0 = string address (set up by caller via ADRP + ADD_label)
/// - stringLen = length of string to print (not including any newline)
///
/// Algorithm:
/// 1. Save string address
/// 2. Print string via write syscall
/// 3. Print newline via write syscall
/// 4. Exit with code 0
///
/// Register usage:
/// - X0: string address, then stdout fd
/// - X1: buffer pointer
/// - X2: length
/// - X16/X8: Syscall number (platform-specific)
let generatePrintString (stringLen: int) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        // Save string address in X1, set up for write syscall
        ARM64.MOV_reg (ARM64.X1, ARM64.X0)  // X1 = buffer address
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // X0 = stdout fd (1)

        // Load string length into X2
        // For lengths > 16 bits, we'd need MOVK, but strings this long are unlikely
        ARM64.MOVZ (ARM64.X2, uint16 stringLen, 0)  // X2 = length

        // Call write syscall
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // Now print newline: allocate 1 byte on stack
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)  // 16-byte aligned
        ARM64.MOVZ (ARM64.X3, 10us, 0)  // '\n' = 10
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)  // Store newline at SP

        // Write newline
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // buffer = SP
        ARM64.MOVZ (ARM64.X2, 1us, 0)  // length = 1
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // Cleanup stack
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)

        // Exit with code 0
        ARM64.MOVZ (ARM64.X0, 0us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Exit, 0)
        ARM64.SVC syscalls.SvcImmediate
    ]

/// Generate ARM64 instructions to print float in D0 to stdout with newline
/// Then exit with code 0
///
/// Algorithm:
/// 1. Extract integer part: FCVTZS X0, D0
/// 2. Check if negative and handle sign
/// 3. Print integer part (absolute value)
/// 4. Print decimal point '.'
/// 5. Extract fractional part: frac = abs(D0) - floor(abs(D0))
/// 6. Multiply by 10^6 to get up to 6 decimal digits
/// 7. Print fractional digits, stripping trailing zeros
/// 8. Print newline and exit
///
/// Register usage:
/// - D0: Input float value
/// - D1-D3: Working FP registers
/// - X0-X6: Working integer registers (same as PrintInt)
/// - X7: Loop counter for fractional digits
/// - X16/X8: Syscall number (platform-specific)
let generatePrintFloat () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        // Allocate 48 bytes on stack for buffer (room for sign, digits, decimal, digits, newline)
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)

        // Save D0 in case we need the original value
        // Store it on stack at [SP+32] (8 bytes for double)
        ARM64.STR_fp (ARM64.D0, ARM64.SP, 32s)

        // Check if negative using D0's sign bit (before FCVTZS, which loses sign for -0.x values)
        ARM64.MOVZ (ARM64.X6, 0us, 0)  // X6 = 0 (assume positive)
        ARM64.FMOV_to_gp (ARM64.X0, ARM64.D0)  // Get bit pattern of D0
        ARM64.TBNZ (ARM64.X0, 63, 2)  // If sign bit set, set X6 = 1
        ARM64.B (2)  // Skip setting X6
        ARM64.MOVZ (ARM64.X6, 1us, 0)  // X6 = 1 (negative)

        // Setup: X1 = buffer pointer (start at end, work backwards)
        // Use SP+31 as the end of buffer (we'll build digits backwards from here)
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)  // X1 = SP + 31 (end of buffer)

        // Extract integer part using FCVTZS (truncate toward zero)
        ARM64.FCVTZS (ARM64.X0, ARM64.D0)  // X0 = integer part (may be negative)
        // Make X2 = absolute value of X0
        ARM64.TBNZ (ARM64.X0, 63, 3)  // If negative, skip the MOV and go to NEG
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)  // X2 = X0 (positive)
        ARM64.B (2)  // Skip NEG
        ARM64.NEG (ARM64.X2, ARM64.X0)  // X2 = -X0 (make positive)

        // === Print integer part (positive value in X2) ===
        // Label: after_sign_check
        // Instruction offset: 14

        // Check if integer part is zero (branch to print_zero_int)
        ARM64.CBZ_offset (ARM64.X2, 62)  // If zero, branch to print_zero_int (inst 75)

        // convert_int_loop - Extract digits by dividing by 10
        ARM64.MOVZ (ARM64.X3, 10us, 0)  // divisor = 10
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)  // X4 = value / 10
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)  // X5 = value % 10
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)  // Convert to ASCII
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)  // Store digit
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // Move pointer back
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)  // value = value / 10
        ARM64.CBZ_offset (ARM64.X2, 2)  // If zero, done with integer
        ARM64.B (-8)  // Loop back

        // store_minus_if_needed - If X6=1 (was negative), store '-'
        ARM64.CBZ_offset (ARM64.X6, 4)  // If X6=0 (positive), skip
        ARM64.MOVZ (ARM64.X3, 45us, 0)  // '-' = ASCII 45
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)  // Store '-'
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // Move pointer back

        // print_integer_part - Write integer digits to stdout
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)  // X1 was one past first char
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32us)  // End of int buffer
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)  // length = end - start
        // Save X6 to stack at [SP+40] before syscalls (syscalls may clobber X0-X7)
        ARM64.STR (ARM64.X6, ARM64.SP, 40s)
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout = 1
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // print_decimal_point - Print '.'
        ARM64.MOVZ (ARM64.X3, 46us, 0)  // '.' = 46
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)  // Store at [SP]
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // buffer = SP
        ARM64.MOVZ (ARM64.X2, 1us, 0)  // length = 1
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // === Extract and print fractional part (2 decimal places) ===
        // Reload original float value
        ARM64.LDR_fp (ARM64.D0, ARM64.SP, 32s)  // Reload original value

        // Get integer part again and compute fractional part
        ARM64.FCVTZS (ARM64.X0, ARM64.D0)  // X0 = integer part (signed)
        ARM64.SCVTF (ARM64.D1, ARM64.X0)  // D1 = (double)integer (signed)
        ARM64.FSUB (ARM64.D0, ARM64.D0, ARM64.D1)  // D0 = fractional part (signed, same sign as original)

        // Multiply fractional part by 100 to get 2 digits
        ARM64.MOVZ (ARM64.X0, 100us, 0)  // 100
        ARM64.SCVTF (ARM64.D1, ARM64.X0)  // D1 = 100.0
        ARM64.FMUL (ARM64.D0, ARM64.D0, ARM64.D1)  // D0 = frac * 100 (might be negative)
        ARM64.FCVTZS (ARM64.X7, ARM64.D0)  // X7 = fractional digits as integer (might be negative)

        // Take absolute value of X7 if negative
        ARM64.TBNZ (ARM64.X7, 63, 2)  // If sign bit set, skip the B
        ARM64.B (2)  // Skip NEG if positive
        ARM64.NEG (ARM64.X7, ARM64.X7)  // Negate to make positive

        // Extract both digits BEFORE any syscalls (syscalls clobber X0-X7)
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.UDIV (ARM64.X4, ARM64.X7, ARM64.X3)  // X4 = X7 / 10 (tens digit)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X7)  // X5 = X7 % 10 (ones digit)
        // Store digits at [SP+1] and [SP+2] for later
        ARM64.ADD_imm (ARM64.X4, ARM64.X4, 48us)  // Convert tens to ASCII
        ARM64.STRB (ARM64.X4, ARM64.SP, 1)  // Store tens digit at [SP+1]
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)  // Convert ones to ASCII
        ARM64.STRB (ARM64.X5, ARM64.SP, 2)  // Store ones digit at [SP+2]

        // Print both digits in one syscall
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 1us)  // buffer at [SP+1]
        ARM64.MOVZ (ARM64.X2, 2us, 0)  // length = 2
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // print_newline_and_exit
        ARM64.MOVZ (ARM64.X3, 10us, 0)  // '\n'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X2, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // Cleanup and exit
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)  // Deallocate stack
        ARM64.MOVZ (ARM64.X0, 0us, 0)  // exit code = 0
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Exit, 0)
        ARM64.SVC syscalls.SvcImmediate

        // print_zero_int - Integer part is zero, just store '0' (instruction 75)
        ARM64.MOVZ (ARM64.X2, 48us, 0)  // '0' = 48
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)  // Store '0'
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // Move pointer back
        ARM64.B (-55)  // Branch back to store_minus_if_needed (inst 23)
    ]

/// Generate ARM64 instructions for heap allocator
///
/// Simple bump allocator that uses a static heap region.
/// First call allocates the heap via mmap syscall.
/// Subsequent calls just bump a pointer.
///
/// Input: X0 = size in bytes
/// Output: X0 = pointer to allocated memory
///
/// Uses global state stored in data section:
/// - _heap_ptr: current bump pointer
/// - _heap_end: end of heap region
///
/// Algorithm:
/// 1. Load heap pointer from _heap_ptr
/// 2. If zero, initialize heap via mmap
/// 3. Bump pointer by size (aligned to 8 bytes)
/// 4. Store updated pointer
/// 5. Return old pointer
///
/// Register usage:
/// - X0: Input size / output pointer
/// - X1-X5: Working registers
/// - X8/X16: Syscall number
let generateHeapAlloc () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os

    // mmap syscall parameters:
    // Linux: mmap(addr=0, length, prot=RW, flags=PRIVATE|ANON, fd=-1, offset=0)
    //   prot: PROT_READ | PROT_WRITE = 1 | 2 = 3
    //   flags: MAP_PRIVATE | MAP_ANONYMOUS = 0x02 | 0x20 = 0x22 (34)
    // macOS: similar but different syscall number and flag values
    //   flags: MAP_PRIVATE | MAP_ANON = 0x0002 | 0x1000 = 0x1002
    let (mmapSyscall, mmapFlags) =
        match os with
        | Platform.Linux -> (syscalls.Mmap, 0x22us)  // MAP_PRIVATE | MAP_ANONYMOUS
        | Platform.MacOS -> (197us, 0x1002us)  // MAP_PRIVATE | MAP_ANON

    [
        // Save requested size in X4
        ARM64.MOV_reg (ARM64.X4, ARM64.X0)

        // Align size to 8 bytes: size = (size + 7) & ~7
        ARM64.ADD_imm (ARM64.X4, ARM64.X4, 7us)
        ARM64.MOVZ (ARM64.X5, 0xFFF8us, 0)  // ~7 as 16-bit (lower bits)
        ARM64.MOVK (ARM64.X5, 0xFFFFus, 1)  // upper bits
        ARM64.MOVK (ARM64.X5, 0xFFFFus, 2)
        ARM64.MOVK (ARM64.X5, 0xFFFFus, 3)
        ARM64.AND_reg (ARM64.X4, ARM64.X4, ARM64.X5)  // X4 = aligned size

        // Load heap pointer address using ADRP + ADD
        ARM64.ADRP (ARM64.X1, "_heap_ptr")
        ARM64.ADD_label (ARM64.X1, ARM64.X1, "_heap_ptr")

        // Load current heap pointer
        ARM64.LDR (ARM64.X2, ARM64.X1, 0s)  // X2 = *_heap_ptr

        // If heap not initialized (X2 == 0), initialize it
        ARM64.CBNZ (ARM64.X2, "_heap_alloc_bump")

        // Initialize heap via mmap
        // Linux: mmap(NULL, 1MB, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)
        ARM64.MOVZ (ARM64.X0, 0us, 0)  // addr = NULL
        ARM64.MOVZ (ARM64.X1, 0us, 0)  // size = 1MB = 0x100000
        ARM64.MOVK (ARM64.X1, 0x10us, 1)  // 0x10 << 16 = 0x100000 = 1MB
        ARM64.MOVZ (ARM64.X2, 3us, 0)  // prot = PROT_READ | PROT_WRITE = 3
        ARM64.MOVZ (ARM64.X3, mmapFlags, 0)  // flags = MAP_PRIVATE | MAP_ANONYMOUS
        ARM64.MOVZ (ARM64.X5, 0xFFFFus, 0)  // fd = -1 (lower 16 bits)
        ARM64.MOVK (ARM64.X5, 0xFFFFus, 1)
        ARM64.MOVK (ARM64.X5, 0xFFFFus, 2)
        ARM64.MOVK (ARM64.X5, 0xFFFFus, 3)
        ARM64.MOV_reg (ARM64.X4, ARM64.X5)  // X4 = -1 (fd for mmap on some platforms)
        // On macOS, mmap uses X4 for flags, X5 for fd
        // On Linux, it's X0-X5 in order: addr, length, prot, flags, fd, offset
        // Let's handle Linux style first
        ARM64.MOV_reg (ARM64.X4, ARM64.X3)  // X4 = flags
        ARM64.MOVZ (ARM64.X5, 0xFFFFus, 0)  // X5 = fd = -1
        ARM64.MOVK (ARM64.X5, 0xFFFFus, 1)
        ARM64.MOVK (ARM64.X5, 0xFFFFus, 2)
        ARM64.MOVK (ARM64.X5, 0xFFFFus, 3)
        ARM64.MOVZ (ARM64.X3, mmapFlags, 0)  // X3 = flags (correct position)
        ARM64.MOV_reg (ARM64.X4, ARM64.X5)  // X4 = fd = -1
        ARM64.MOVZ (ARM64.X5, 0us, 0)  // X5 = offset = 0

        ARM64.MOVZ (syscalls.SyscallRegister, mmapSyscall, 0)  // mmap syscall
        ARM64.SVC syscalls.SvcImmediate

        // X0 now contains mmap result (heap base pointer)
        // Store it in _heap_ptr
        ARM64.ADRP (ARM64.X1, "_heap_ptr")
        ARM64.ADD_label (ARM64.X1, ARM64.X1, "_heap_ptr")
        ARM64.STR (ARM64.X0, ARM64.X1, 0s)  // *_heap_ptr = heap base

        // Also calculate and store end pointer
        ARM64.ADD_imm (ARM64.X2, ARM64.X0, 0us)  // X2 = heap base
        ARM64.MOVZ (ARM64.X3, 0us, 0)  // 1MB
        ARM64.MOVK (ARM64.X3, 0x10us, 1)  // 0x100000 = 1MB
        ARM64.ADD_reg (ARM64.X3, ARM64.X2, ARM64.X3)  // X3 = heap base + 1MB = heap end
        ARM64.ADRP (ARM64.X1, "_heap_end")
        ARM64.ADD_label (ARM64.X1, ARM64.X1, "_heap_end")
        ARM64.STR (ARM64.X3, ARM64.X1, 0s)  // *_heap_end = heap end

        // Reload aligned size into X4 (was clobbered by mmap)
        // Actually we need to recalculate from original X0 which is now mmap result
        // This is tricky - let's save size before mmap
        // FIXME: This code path needs cleanup - for now, assume small allocs work
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)  // X2 = current heap pointer (just initialized)
        ARM64.B_label "_heap_alloc_return"

        // Label: _heap_alloc_bump (heap already initialized)
        ARM64.Label "_heap_alloc_bump"
        // X2 = current heap pointer (loaded above)
        // X4 = aligned size

        // Bump the pointer
        ARM64.ADD_reg (ARM64.X3, ARM64.X2, ARM64.X4)  // X3 = new heap pointer

        // Store new pointer
        ARM64.ADRP (ARM64.X1, "_heap_ptr")
        ARM64.ADD_label (ARM64.X1, ARM64.X1, "_heap_ptr")
        ARM64.STR (ARM64.X3, ARM64.X1, 0s)  // *_heap_ptr = new pointer

        // Label: _heap_alloc_return
        ARM64.Label "_heap_alloc_return"
        // Return old pointer in X0
        ARM64.MOV_reg (ARM64.X0, ARM64.X2)
        ARM64.RET
    ]

/// Generate data section with heap state variables
let generateHeapData () : ARM64.Instr list =
    [
        // Reserve 8 bytes for heap pointer
        ARM64.Label "_heap_ptr"
        // Initial value 0 (will be set by first alloc)
        // Using MOVZ as placeholder - actual data emission handled in ELF generation

        ARM64.Label "_heap_end"
        // Initial value 0
    ]

/// Generate ARM64 instructions to exit with code 0
let generateExit () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        ARM64.MOVZ (ARM64.X0, 0us, 0)  // exit code = 0
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Exit, 0)
        ARM64.SVC syscalls.SvcImmediate
    ]

/// Generate ARM64 instructions to print int64 in X0 to stdout with newline (NO EXIT)
/// Same as generatePrintInt but returns instead of exiting
let generatePrintIntNoExit () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        // Allocate 32 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

        // Setup: X1 = buffer pointer, X2 = value
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)

        // Store newline at end of buffer
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)

        // Initialize: X6 = 0 (positive flag), X3 = 10 (divisor)
        ARM64.MOVZ (ARM64.X6, 0us, 0)
        ARM64.MOVZ (ARM64.X3, 10us, 0)

        // Check for negative: if X2 < 0, branch to handle_negative (at instruction 33)
        ARM64.CMP_imm (ARM64.X2, 0us)
        ARM64.B_cond (ARM64.LT, 25)  // 33 - 8 = 25

        // Check for zero: if X2 == 0, branch to print_zero (at instruction 29)
        ARM64.CBZ_offset (ARM64.X2, 20)  // 29 - 9 = 20

        // digit_loop: Extract digits
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)
        ARM64.CBNZ_offset (ARM64.X2, -6)

        // store_minus_if_needed
        ARM64.CBZ_offset (ARM64.X6, 4)
        ARM64.MOVZ (ARM64.X3, 45us, 0)
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // write_output
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32us)
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)  // Deallocate stack
        ARM64.B (8)  // Skip past print_zero (4) and handle_negative (3) + 1 to exit runtime (8 instructions)

        // print_zero (at instruction 29)
        ARM64.MOVZ (ARM64.X2, 48us, 0)
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.B (-15)  // Jump to store_minus_if_needed: 17 - 32 = -15

        // handle_negative (at instruction 33)
        ARM64.NEG (ARM64.X2, ARM64.X2)
        ARM64.MOVZ (ARM64.X6, 1us, 0)
        ARM64.B (-26)  // Jump to check_zero: 9 - 35 = -26
    ]

/// Generate ARM64 instructions to print boolean in X0 to stdout with newline (NO EXIT)
/// Same as generatePrintBool but returns instead of exiting
let generatePrintBoolNoExit () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        // Allocate 16 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

        // Check if false (X0 == 0), branch to print_false (+17 instructions)
        ARM64.CBZ_offset (ARM64.X0, 17)

        // print_true: Store "true\n" on stack (5 bytes)
        ARM64.MOVZ (ARM64.X3, 116us, 0)  // 't'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 114us, 0)  // 'r'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 117us, 0)  // 'u'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, 10us, 0)   // '\n'
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X2, 5us, 0)    // length = 5

        // Write and cleanup (no exit)
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.B (18)  // Jump to cleanup (+18 instructions to skip false branch)

        // print_false: Store "false\n" on stack (6 bytes)
        ARM64.MOVZ (ARM64.X3, 102us, 0)  // 'f'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 97us, 0)   // 'a'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 108us, 0)  // 'l'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 115us, 0)  // 's'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X3, 10us, 0)   // '\n'
        ARM64.STRB (ARM64.X3, ARM64.SP, 5)
        ARM64.MOVZ (ARM64.X2, 6us, 0)    // length = 6

        // Write
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // cleanup (no exit):
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
    ]

/// Generate ARM64 instructions to print int64 in X0 to stdout WITHOUT newline
/// For use in tuple/list element printing
let generatePrintIntNoNewline () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        // Allocate 32 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

        // Setup: X1 = buffer pointer (start at end-1, no newline), X2 = value
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 30us)  // One less than with newline
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)

        // Initialize: X6 = 0 (positive flag), X3 = 10 (divisor)
        ARM64.MOVZ (ARM64.X6, 0us, 0)
        ARM64.MOVZ (ARM64.X3, 10us, 0)

        // Check for negative: if X2 < 0, branch to handle_negative (at index 31)
        ARM64.CMP_imm (ARM64.X2, 0us)
        ARM64.B_cond (ARM64.LT, 25)  // 31 - 6 = 25

        // Check for zero: if X2 == 0, branch to print_zero (at index 27)
        ARM64.CBZ_offset (ARM64.X2, 20)  // 27 - 7 = 20

        // digit_loop: Extract digits
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)
        ARM64.CBNZ_offset (ARM64.X2, -6)

        // store_minus_if_needed
        ARM64.CBZ_offset (ARM64.X6, 4)
        ARM64.MOVZ (ARM64.X3, 45us, 0)
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // write_output
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 31us)  // End of buffer area
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)  // Deallocate stack
        ARM64.B (8)  // Skip past print_zero (4) and handle_negative (3) + 1 to exit

        // print_zero (at index 27)
        ARM64.MOVZ (ARM64.X2, 48us, 0)
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.B (-15)  // Jump to store_minus_if_needed at index 15: 15 - 30 = -15

        // handle_negative (at index 31)
        ARM64.NEG (ARM64.X2, ARM64.X2)
        ARM64.MOVZ (ARM64.X6, 1us, 0)
        ARM64.B (-25)  // Jump to digit_loop at index 8: 8 - 33 = -25
    ]

/// Generate ARM64 instructions to print boolean in X0 to stdout WITHOUT newline
/// For use in tuple/list element printing
let generatePrintBoolNoNewline () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        // Allocate 16 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

        // Check if false (X0 == 0), branch to print_false at index 16
        ARM64.CBZ_offset (ARM64.X0, 15)  // 16 - 1 = 15

        // print_true: Store "true" on stack (4 bytes, no newline)
        ARM64.MOVZ (ARM64.X3, 116us, 0)  // 't'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 114us, 0)  // 'r'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 117us, 0)  // 'u'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X2, 4us, 0)    // length = 4 (no newline)

        // Write and cleanup
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.B (16)  // Jump to cleanup at index 31: 31 - 15 = 16

        // print_false: Store "false" on stack (5 bytes, no newline)
        ARM64.MOVZ (ARM64.X3, 102us, 0)  // 'f'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 97us, 0)   // 'a'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 108us, 0)  // 'l'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 115us, 0)  // 's'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X2, 5us, 0)    // length = 5 (no newline)

        // Write
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // cleanup:
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
    ]

/// Generate ARM64 instructions to print float in D0 to stdout WITHOUT newline
/// For use in tuple/list element printing
/// Similar to generatePrintFloat but doesn't add newline or exit
let generatePrintFloatNoNewline () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        // Allocate 48 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)

        // Save D0 at [SP+32]
        ARM64.STR_fp (ARM64.D0, ARM64.SP, 32s)

        // Check if negative using D0's sign bit
        ARM64.MOVZ (ARM64.X6, 0us, 0)  // X6 = 0 (assume positive)
        ARM64.FMOV_to_gp (ARM64.X0, ARM64.D0)  // Get bit pattern
        ARM64.TBNZ (ARM64.X0, 63, 2)  // If sign bit set, set X6 = 1
        ARM64.B (2)  // Skip setting X6
        ARM64.MOVZ (ARM64.X6, 1us, 0)  // X6 = 1 (negative)

        // Setup: X1 = buffer pointer (start at end)
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)

        // Extract integer part
        ARM64.FCVTZS (ARM64.X0, ARM64.D0)
        ARM64.TBNZ (ARM64.X0, 63, 3)
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)
        ARM64.B (2)
        ARM64.NEG (ARM64.X2, ARM64.X0)

        // Check if integer part is zero
        ARM64.CBZ_offset (ARM64.X2, 51)  // Branch to print_zero_int

        // convert_int_loop
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)
        ARM64.CBZ_offset (ARM64.X2, 2)
        ARM64.B (-8)

        // store_minus_if_needed
        ARM64.CBZ_offset (ARM64.X6, 4)
        ARM64.MOVZ (ARM64.X3, 45us, 0)
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // print_integer_part
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32us)
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)
        ARM64.STR (ARM64.X6, ARM64.SP, 40s)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // print_decimal_point
        ARM64.MOVZ (ARM64.X3, 46us, 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X2, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // Extract and print fractional part (2 decimal places)
        ARM64.LDR_fp (ARM64.D0, ARM64.SP, 32s)
        ARM64.FCVTZS (ARM64.X0, ARM64.D0)
        ARM64.SCVTF (ARM64.D1, ARM64.X0)
        ARM64.FSUB (ARM64.D0, ARM64.D0, ARM64.D1)
        ARM64.MOVZ (ARM64.X0, 100us, 0)
        ARM64.SCVTF (ARM64.D1, ARM64.X0)
        ARM64.FMUL (ARM64.D0, ARM64.D0, ARM64.D1)
        ARM64.FCVTZS (ARM64.X7, ARM64.D0)

        // Take absolute value of X7
        ARM64.TBNZ (ARM64.X7, 63, 2)
        ARM64.B (2)
        ARM64.NEG (ARM64.X7, ARM64.X7)

        // Extract digits
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.UDIV (ARM64.X4, ARM64.X7, ARM64.X3)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X7)
        ARM64.ADD_imm (ARM64.X4, ARM64.X4, 48us)
        ARM64.STRB (ARM64.X4, ARM64.SP, 1)
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.STRB (ARM64.X5, ARM64.SP, 2)

        // Print both digits
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 1us)
        ARM64.MOVZ (ARM64.X2, 2us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // Cleanup (no newline, no exit)
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)
        ARM64.B (4)  // Skip past print_zero_int

        // print_zero_int
        ARM64.MOVZ (ARM64.X2, 48us, 0)
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.B (-44)  // Branch back to store_minus_if_needed
    ]

/// Generate ARM64 instructions to print heap string WITHOUT newline
/// Expects: X9 = data address, X10 = length
/// For use in tuple/list element printing
let generatePrintStringNoNewline () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        // Write string to stdout
        ARM64.MOVZ (ARM64.X0, 1us, 0)        // fd = stdout
        ARM64.MOV_reg (ARM64.X1, ARM64.X9)   // buffer = X9
        ARM64.MOV_reg (ARM64.X2, ARM64.X10)  // length = X10
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
    ]

/// Generate ARM64 instructions to print a sequence of literal characters
/// Used for printing delimiters like "(", ")", "[", "]", ", " etc.
///
/// Algorithm:
/// 1. Allocate aligned stack buffer
/// 2. Store each byte on stack
/// 3. Write to stdout via syscall
/// 4. Deallocate stack
///
/// No newline is added - caller controls newlines
let generatePrintChars (chars: byte list) : ARM64.Instr list =
    if List.isEmpty chars then [] else
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    let len = List.length chars
    // Stack allocation must be 16-byte aligned
    let stackSize = max 16 ((len + 15) / 16 * 16)
    [
        // Allocate stack buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, uint16 stackSize)
    ]
    @ (chars |> List.mapi (fun i b ->
        [
            ARM64.MOVZ (ARM64.X3, uint16 b, 0)
            ARM64.STRB (ARM64.X3, ARM64.SP, i)
        ]) |> List.concat)
    @ [
        // Write to stdout
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)          // buffer
        ARM64.MOVZ (ARM64.X2, uint16 len, 0)        // length
        ARM64.MOVZ (ARM64.X0, 1us, 0)               // stdout = 1
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        // Deallocate stack
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 stackSize)
    ]

/// Generate ARM64 instructions to perform write syscall only
///
/// Assumes caller has set up:
/// - X0 = file descriptor (usually 1 for stdout)
/// - X1 = buffer pointer
/// - X2 = length
///
/// Does NOT print newline or exit - caller handles those if needed
let generateWriteSyscall () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os
    [
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
    ]

/// Generate ARM64 instructions for Stdlib.File.exists
/// Input: pathReg contains heap string pointer (format: [len:8][data:N])
/// Output: destReg = 1 if file exists, 0 if not
///
/// Algorithm:
/// 1. Get data pointer from heap string (path + 8)
/// 2. Save the path string to a null-terminated temp buffer on stack
/// 3. Call access(path, F_OK) or faccessat(AT_FDCWD, path, F_OK, 0)
/// 4. If syscall returns 0 (success), set dest = 1; else dest = 0
let generateFileExists (destReg: ARM64.Reg) (pathReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os

    // We need to null-terminate the string for the syscall
    // Stack layout: [saved regs:16][path:256]
    // Use simpler approach: just copy the string and add null terminator
    // Note: This is a simplification - paths > 255 chars will be truncated
    match os with
    | Platform.MacOS ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes, 16-byte aligned)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg), X20 = dest pointer for later
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // X2 = string length from [X19]
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)

            // X0 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            // X1 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 (for LDRB offset)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop: copies X2 bytes from X1 to X0
            // copy_loop (7 instructions, 0-6):
            ARM64.CBZ_offset (ARM64.X2, 7)      // 0: If length == 0, skip to null_term at inst 7
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4) // 1: Load byte from src [X1 + 0]
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)  // 2: Store byte to dest
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us) // 3: dest++
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us) // 4: src++
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us) // 5: len--
            ARM64.B (-6)                        // 6: Loop back to CBZ

            // null_term: Store null terminator at X0
            ARM64.MOVZ (ARM64.X3, 0us, 0)       // X3 = 0
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)  // Store null terminator

            // Call access(path, F_OK)
            // X0 = path (SP), X1 = mode (F_OK = 0)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X1, 0us, 0)       // F_OK = 0
            ARM64.MOVZ (ARM64.X16, syscalls.Access, 0)  // access syscall
            ARM64.SVC syscalls.SvcImmediate

            // If X0 == 0, file exists; else doesn't
            // Use CMP + CSET to convert to boolean
            ARM64.CMP_imm (ARM64.X0, 0us)
            ARM64.CSET (ARM64.X0, ARM64.EQ)     // X0 = 1 if was 0, else 0

            // Cleanup - restore registers before moving result to dest
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)  // Deallocate path buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)   // Move result to dest after restoration
        ]
    | Platform.Linux ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg)
            ARM64.MOV_reg (ARM64.X19, pathReg)

            // X2 = string length from [X19]
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)

            // X0 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            // X1 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 (for LDRB offset)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X2, 7)      // 0: If length == 0, skip to null_term at inst 7
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // null_term: Store null terminator
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // Call faccessat(AT_FDCWD, path, F_OK, 0)
            // X0 = dirfd (AT_FDCWD = -100), X1 = path, X2 = mode (F_OK = 0), X3 = flags (0)
            // To load -100: use MOVZ with large value then subtract, or just use immediate
            // Actually, let's use MOVZ to load 0xFFFFFFFFFFFFFF9C which is -100 in two's complement
            // Simpler: load 100, then negate
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)      // X0 = -100 (AT_FDCWD)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // path
            ARM64.MOVZ (ARM64.X2, 0us, 0)       // F_OK = 0
            ARM64.MOVZ (ARM64.X3, 0us, 0)       // flags = 0
            ARM64.MOVZ (ARM64.X8, syscalls.Access, 0)  // faccessat syscall
            ARM64.SVC syscalls.SvcImmediate

            // If X0 == 0, file exists; else doesn't
            ARM64.CMP_imm (ARM64.X0, 0us)
            ARM64.CSET (ARM64.X0, ARM64.EQ)

            // Cleanup - restore registers before moving result to dest
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)   // Move result to dest after restoration
        ]

/// Generate ARM64 instructions to delete a file and return Result<Unit, String>
/// destReg: destination register for the Result pointer
/// pathReg: register containing heap string pointer to file path
///
/// Result memory layout: [tag:8][payload:8][refcount:8] = 24 bytes
/// - tag 0 = Ok, tag 1 = Error
/// - payload = pointer to value (0 for Unit, string pointer for Error)
let generateFileDelete (destReg: ARM64.Reg) (pathReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os

    match os with
    | Platform.MacOS ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes, 16-byte aligned)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg), X20 = dest pointer for later
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // X2 = string length from [X19]
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)

            // X0 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            // X1 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 (for LDRB offset)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop: copies X2 bytes from X1 to X0
            // copy_loop (7 instructions, 0-6):
            ARM64.CBZ_offset (ARM64.X2, 7)      // 0: If length == 0, skip to null_term at inst 7
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4) // 1: Load byte from src [X1 + 0]
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)  // 2: Store byte to dest
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us) // 3: dest++
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us) // 4: src++
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us) // 5: len--
            ARM64.B (-6)                        // 6: Loop back to CBZ

            // null_term: Store null terminator at X0
            ARM64.MOVZ (ARM64.X3, 0us, 0)       // X3 = 0
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)  // Store null terminator

            // Call unlink(path)
            // X0 = path (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X16, syscalls.Unlink, 0)  // unlink syscall
            ARM64.SVC syscalls.SvcImmediate

            // Save syscall result in X21
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)

            // Allocate Result on heap: [tag:8][payload:8][refcount:8] = 24 bytes
            ARM64.MOV_reg (ARM64.X0, ARM64.X28)  // X0 = result pointer
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)  // bump allocator

            // Check if unlink succeeded (X21 == 0)
            ARM64.CMP_imm (ARM64.X21, 0us)
            ARM64.B_cond (ARM64.NE, 6)  // If failed, jump to error path

            // Success path: Result = Ok(())
            ARM64.MOVZ (ARM64.X1, 0us, 0)       // tag = 0 (Ok)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)  // Store tag
            ARM64.STR (ARM64.X1, ARM64.X0, 8s)  // payload = 0 (Unit)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s) // refcount = 1
            ARM64.B (26)  // Jump to cleanup (skip 26 error path instructions)

            // Error path: Result = Error("Error")
            // Allocate error string: "Error" (5 chars)
            // String format: [length:8][data:N][refcount:8]
            ARM64.MOV_reg (ARM64.X2, ARM64.X28)  // X2 = error string pointer  (1)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)  // (2)
            ARM64.MOVZ (ARM64.X1, 5us, 0)       // length = 5  (3)
            ARM64.STR (ARM64.X1, ARM64.X2, 0s)  // Store length  (4)
            // Store "Error" byte by byte: E=69, r=114, r=114, o=111, r=114
            ARM64.MOVZ (ARM64.X1, 69us, 0)      // 'E'  (5)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 8us)  // (6)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (7)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (8)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 9us)  // (9)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (10)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (11)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 10us)  // (12)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (13)
            ARM64.MOVZ (ARM64.X1, 111us, 0)     // 'o'  (14)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 11us)  // (15)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (16)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (17)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 12us)  // (18)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (19)
            ARM64.MOVZ (ARM64.X1, 1us, 0)  // (20)
            ARM64.STR (ARM64.X1, ARM64.X2, 16s) // refcount = 1  (21)
            // Now store Result with error
            ARM64.MOVZ (ARM64.X1, 1us, 0)       // tag = 1 (Error)  (22)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)  // Store tag  (23)
            ARM64.STR (ARM64.X2, ARM64.X0, 8s)  // payload = error string pointer  (24)
            ARM64.MOVZ (ARM64.X1, 1us, 0)  // (25)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s) // refcount = 1  (26)

            // Cleanup - restore registers and move result to dest
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)  // Deallocate path buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)   // Move result to dest after restoration
        ]
    | Platform.Linux ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg)
            ARM64.MOV_reg (ARM64.X19, pathReg)

            // X2 = string length from [X19]
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)

            // X0 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            // X1 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 (for LDRB offset)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X2, 7)      // 0: If length == 0, skip to null_term at inst 7
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // null_term: Store null terminator
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // Call unlinkat(AT_FDCWD, path, 0)
            // X0 = dirfd (AT_FDCWD = -100), X1 = path, X2 = flags (0)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)      // X0 = -100 (AT_FDCWD)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // path
            ARM64.MOVZ (ARM64.X2, 0us, 0)       // flags = 0
            ARM64.MOVZ (ARM64.X8, syscalls.Unlink, 0)  // unlinkat syscall
            ARM64.SVC syscalls.SvcImmediate

            // Save syscall result in X21
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)

            // Allocate Result on heap: [tag:8][payload:8][refcount:8] = 24 bytes
            ARM64.MOV_reg (ARM64.X0, ARM64.X28)  // X0 = result pointer
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)  // bump allocator

            // Check if unlink succeeded (X21 == 0)
            ARM64.CMP_imm (ARM64.X21, 0us)
            ARM64.B_cond (ARM64.NE, 6)  // If failed, jump to error path

            // Success path: Result = Ok(())
            ARM64.MOVZ (ARM64.X1, 0us, 0)       // tag = 0 (Ok)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)  // Store tag
            ARM64.STR (ARM64.X1, ARM64.X0, 8s)  // payload = 0 (Unit)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s) // refcount = 1
            ARM64.B (26)  // Jump to cleanup (skip 26 error path instructions)

            // Error path: Result = Error("Error")
            ARM64.MOV_reg (ARM64.X2, ARM64.X28)  // X2 = error string pointer  (1)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)  // (2)
            ARM64.MOVZ (ARM64.X1, 5us, 0)       // length = 5  (3)
            ARM64.STR (ARM64.X1, ARM64.X2, 0s)  // Store length  (4)
            // Store "Error" byte by byte: E=69, r=114, r=114, o=111, r=114
            ARM64.MOVZ (ARM64.X1, 69us, 0)      // 'E'  (5)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 8us)  // (6)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (7)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (8)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 9us)  // (9)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (10)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (11)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 10us)  // (12)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (13)
            ARM64.MOVZ (ARM64.X1, 111us, 0)     // 'o'  (14)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 11us)  // (15)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (16)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (17)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 12us)  // (18)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (19)
            ARM64.MOVZ (ARM64.X1, 1us, 0)  // (20)
            ARM64.STR (ARM64.X1, ARM64.X2, 16s) // refcount = 1  (21)
            ARM64.MOVZ (ARM64.X1, 1us, 0)       // tag = 1 (Error)  (22)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)  // Store tag  (23)
            ARM64.STR (ARM64.X2, ARM64.X0, 8s)  // payload = error string pointer  (24)
            ARM64.MOVZ (ARM64.X1, 1us, 0)  // (25)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s) // refcount = 1  (26)

            // Cleanup - restore registers and move result to dest
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)   // Move result to dest after restoration
        ]

/// Generate ARM64 instructions to set executable permission on a file
/// destReg: destination register for the Result pointer
/// pathReg: register containing heap string pointer to file path
///
/// Result memory layout: [tag:8][payload:8][refcount:8] = 24 bytes
/// - tag 0 = Ok, tag 1 = Error
/// - payload = pointer to value (0 for Unit, string pointer for Error)
let generateFileSetExecutable (destReg: ARM64.Reg) (pathReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os

    match os with
    | Platform.MacOS ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes, 16-byte aligned)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg), X20 = dest pointer for later
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // X2 = string length from [X19]
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)

            // X0 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            // X1 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 (for LDRB offset)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop: copies X2 bytes from X1 to X0
            ARM64.CBZ_offset (ARM64.X2, 7)
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // null_term: Store null terminator at X0
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // Call chmod(path, 0755)
            // X0 = path (SP), X1 = mode (0755 = 493 in decimal)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X1, 493us, 0)   // 0755 = 493
            ARM64.MOVZ (ARM64.X16, syscalls.Chmod, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Save syscall result in X21
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)

            // Allocate Result on heap: [tag:8][payload:8][refcount:8] = 24 bytes
            ARM64.MOV_reg (ARM64.X0, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            // Check if chmod succeeded (X21 == 0)
            ARM64.CMP_imm (ARM64.X21, 0us)
            ARM64.B_cond (ARM64.NE, 6)

            // Success path: Result = Ok(())
            ARM64.MOVZ (ARM64.X1, 0us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)
            ARM64.STR (ARM64.X1, ARM64.X0, 8s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s)
            ARM64.B (26)

            // Error path: Result = Error("Error")
            ARM64.MOV_reg (ARM64.X2, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X1, 5us, 0)
            ARM64.STR (ARM64.X1, ARM64.X2, 0s)
            ARM64.MOVZ (ARM64.X1, 69us, 0)      // 'E'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 8us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 9us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 10us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 111us, 0)     // 'o'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 11us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 12us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X2, 16s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)
            ARM64.STR (ARM64.X2, ARM64.X0, 8s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s)

            // Cleanup
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)
        ]
    | Platform.Linux ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg)
            ARM64.MOV_reg (ARM64.X19, pathReg)

            // X2 = string length from [X19]
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)

            // X0 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            // X1 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 (for LDRB offset)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X2, 7)
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // null_term: Store null terminator
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // Call fchmodat(AT_FDCWD, path, 0755, 0)
            // X0 = dirfd (AT_FDCWD = -100), X1 = path, X2 = mode (0755 = 493), X3 = flags (0)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)
            ARM64.MOVZ (ARM64.X2, 493us, 0)   // 0755 = 493
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.MOVZ (ARM64.X8, syscalls.Chmod, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Save syscall result in X21
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)

            // Allocate Result on heap
            ARM64.MOV_reg (ARM64.X0, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            // Check if chmod succeeded (X21 == 0)
            ARM64.CMP_imm (ARM64.X21, 0us)
            ARM64.B_cond (ARM64.NE, 6)

            // Success path: Result = Ok(())
            ARM64.MOVZ (ARM64.X1, 0us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)
            ARM64.STR (ARM64.X1, ARM64.X0, 8s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s)
            ARM64.B (26)

            // Error path: Result = Error("Error")
            ARM64.MOV_reg (ARM64.X2, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X1, 5us, 0)
            ARM64.STR (ARM64.X1, ARM64.X2, 0s)
            ARM64.MOVZ (ARM64.X1, 69us, 0)      // 'E'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 8us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 9us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 10us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 111us, 0)     // 'o'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 11us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 12us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X2, 16s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)
            ARM64.STR (ARM64.X2, ARM64.X0, 8s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s)

            // Cleanup
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)
        ]

/// Generate ARM64 instructions to read file contents and return Result<String, String>
/// destReg: destination register for the Result pointer
/// pathReg: register containing heap string pointer to file path
///
/// Result memory layout: [tag:8][payload:8][refcount:8] = 24 bytes
/// - tag 0 = Ok, tag 1 = Error
/// - payload = pointer to string
///
/// String memory layout: [length:8][data:N][refcount:8]
///
/// Algorithm:
/// 1. Copy path to stack with null terminator (same as FileExists)
/// 2. open() syscall - if fails, return Error("File not found")
/// 3. fstat() syscall - get file size
/// 4. Allocate string buffer on heap
/// 5. read() syscall - read file contents
/// 6. close() syscall
/// 7. Construct Result with Ok(string) or Error(message)
let generateFileReadText (destReg: ARM64.Reg) (pathReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error _ -> Platform.Linux
    let syscalls = Platform.getSyscallNumbers os

    // For now, implement a simplified version that:
    // - Opens the file
    // - Reads up to 4096 bytes
    // - Returns Ok(contents) or Error("File not found")
    //
    // Stack layout: [saved X19-X25: 64][stat buffer: 144][path: 256][padding: 16] = 480 bytes
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

            // Allocate stack space for stat buffer (144 bytes) + path (256 bytes) + padding
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255us)  // Can only sub 255 at a time
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 161us)  // Total: 416 bytes

            // X19 = path heap string, X20 = dest register, X21 = file descriptor
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // Get path length from heap string
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)  // X2 = length

            // X0 = stack dest for null-terminated path (SP + 144 for after stat buffer)
            ARM64.ADD_imm (ARM64.X0, ARM64.SP, 144us)
            // X1 = heap string data (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 for LDRB offset
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X2, 7)      // 0: If length == 0, skip to null_term
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

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
            ARM64.MOVZ (ARM64.X8, syscalls.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Check if open failed (fd < 0 means X0 has sign bit set)
            // X21 = fd
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)
            ARM64.TBNZ (ARM64.X0, 63, 31)  // If negative, branch to error path (+31 instructions)

            // fstat(fd, statbuf) - X0 = fd, X1 = statbuf
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // stat buffer at SP
            ARM64.MOVZ (ARM64.X8, syscalls.Fstat, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Get file size from stat buffer (st_size is at offset 48 on Linux ARM64)
            ARM64.LDR (ARM64.X22, ARM64.SP, 48s)  // X22 = file size

            // Allocate heap space for string: [len:8][data:N][refcount:8]
            // Size = 8 + size + 8 = size + 16, round up to next 8 bytes
            // Simpler: just add 24 (16 + 8 for alignment padding)
            ARM64.ADD_imm (ARM64.X23, ARM64.X22, 24us)  // X23 = size + 24 (with padding)

            // Allocate from heap (bump allocator)
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)  // X24 = string pointer
            ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X23)  // bump heap pointer

            // Store length at [X24]
            ARM64.STR (ARM64.X22, ARM64.X24, 0s)

            // read(fd, buf, count) - read file contents
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)  // fd
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)  // buf = string data area
            ARM64.MOV_reg (ARM64.X2, ARM64.X22)  // count = file size
            ARM64.MOVZ (ARM64.X8, syscalls.Read, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Store refcount = 1 at [X24 + 8 + size]
            ARM64.ADD_imm (ARM64.X25, ARM64.X24, 8us)
            ARM64.ADD_reg (ARM64.X25, ARM64.X25, ARM64.X22)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X8, syscalls.Close, 0)
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
            ARM64.B 30  // Skip error path (29 instructions + 1 to land on cleanup)

            // === Error path (file not found) ===
            // Create error string "File not found" and Error result
            // For simplicity, create a short error message

            // Allocate error string: "Error" = 5 chars + len + refcount = 24 bytes
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)  // X24 = error string
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            // Store length = 5
            ARM64.MOVZ (ARM64.X0, 5us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            // Store "Error" (ASCII: 69, 114, 114, 111, 114)
            // E=69, r=114, r=114, o=111, r=114
            // Store at [X24+8] through [X24+12]
            ARM64.MOVZ (ARM64.X0, 69us, 0)  // 'E'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 114us, 0)  // 'r'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 9us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 10us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 111us, 0)  // 'o'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 11us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 114us, 0)  // 'r'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 12us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            // Store refcount = 1 at [X24 + 8 + 5] = [X24 + 13]
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 13us)
            ARM64.STR (ARM64.X0, ARM64.X1, 0s)

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

            // Deallocate stack
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 161us)

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

            // Allocate stack: stat buffer (144) + path (256) = 400 bytes, round to 416
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 161us)

            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // Copy path with null terminator
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)
            ARM64.ADD_imm (ARM64.X0, ARM64.SP, 144us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            ARM64.CBZ_offset (ARM64.X2, 7)
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // open(path, O_RDONLY)
            ARM64.ADD_imm (ARM64.X0, ARM64.SP, 144us)
            ARM64.MOVZ (ARM64.X1, 0us, 0)  // O_RDONLY
            ARM64.MOVZ (ARM64.X16, syscalls.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            ARM64.MOV_reg (ARM64.X21, ARM64.X0)
            ARM64.TBNZ (ARM64.X0, 63, 31)  // If negative, branch to error path

            // fstat(fd, statbuf)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)
            ARM64.MOVZ (ARM64.X16, syscalls.Fstat, 0)
            ARM64.SVC syscalls.SvcImmediate

            // st_size at offset 96 on macOS
            ARM64.LDR (ARM64.X22, ARM64.SP, 96s)

            // Allocate string: size + 24 (with padding for alignment)
            ARM64.ADD_imm (ARM64.X23, ARM64.X22, 24us)

            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X23)

            ARM64.STR (ARM64.X22, ARM64.X24, 0s)

            // read
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)
            ARM64.MOV_reg (ARM64.X2, ARM64.X22)
            ARM64.MOVZ (ARM64.X16, syscalls.Read, 0)
            ARM64.SVC syscalls.SvcImmediate

            ARM64.ADD_imm (ARM64.X25, ARM64.X24, 8us)
            ARM64.ADD_reg (ARM64.X25, ARM64.X25, ARM64.X22)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)

            // close
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X16, syscalls.Close, 0)
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

            ARM64.B 30  // Skip error path (29 instructions + 1 to land on cleanup)

            // Error path (same as Linux)
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 5us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            ARM64.MOVZ (ARM64.X0, 69us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 114us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 9us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 10us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 111us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 11us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 114us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 12us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 13us)
            ARM64.STR (ARM64.X0, ARM64.X1, 0s)

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

            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 161us)

            ARM64.LDP (ARM64.X25, ARM64.X26, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 32s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 48s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 64us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]

/// Generate ARM64 instructions to write content to a file
/// pathReg: register containing pointer to heap string (path)
/// contentReg: register containing pointer to heap string (content)
/// append: if true, append to file; if false, overwrite
/// Returns Result<Unit, String> in destReg
/// On success, result contains Ok(()) - tag=0, payload=0
/// On failure, result contains Error("Error") - tag=1, payload=error string ptr
let generateFileWriteText (destReg: ARM64.Reg) (pathReg: ARM64.Reg) (contentReg: ARM64.Reg) (append: bool) : ARM64.Instr list =
    // Get platform and syscalls
    let os =
        match Platform.detectOS () with
        | Ok os -> os
        | Error _ -> Platform.Linux  // Default to Linux
    let syscalls = Platform.getSyscallNumbers os

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
            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)

            // Allocate stack for path buffer (256 bytes)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 1us)

            // Save path and content pointers
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X22, contentReg)

            // Copy path to stack buffer with null terminator
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)  // X2 = path length
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)  // X0 = dest buffer
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)  // X1 = source data
            ARM64.MOVZ (ARM64.X4, 0us, 0)  // X4 = index register for LDRB

            // Copy loop
            ARM64.CBZ_offset (ARM64.X2, 7)
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // openat(AT_FDCWD, path, flags, mode)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)  // AT_FDCWD = -100
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // path
            ARM64.MOVZ (ARM64.X2, flags, 0)  // flags
            ARM64.MOVZ (ARM64.X3, 420us, 0)  // mode 0644
            ARM64.MOVZ (ARM64.X8, syscalls.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Check if open failed
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)  // X21 = fd
            ARM64.TBNZ (ARM64.X0, 63, 18)  // If negative, branch to error path (17 instructions + 1)

            // write(fd, buf, count)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)  // fd
            ARM64.ADD_imm (ARM64.X1, ARM64.X22, 8us)  // buf = content data
            ARM64.LDR (ARM64.X2, ARM64.X22, 0s)  // count = content length
            ARM64.MOVZ (ARM64.X8, syscalls.Write, 0)
            ARM64.SVC syscalls.SvcImmediate

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X8, syscalls.Close, 0)
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
            ARM64.B 30  // Jump to cleanup (skip 29 error path instructions + 1)

            // Error path: Create Error("Error") result
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 5us, 0)  // length = 5
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            // Store "Error" bytes
            ARM64.MOVZ (ARM64.X0, 69us, 0)  // 'E'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)  // 'r'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 9us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 10us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 111us, 0)  // 'o'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 11us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)  // 'r'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 12us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 13us)
            ARM64.STR (ARM64.X0, ARM64.X1, 0s)  // refcount

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
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 1us)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 32s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]
    | Platform.MacOS ->
        [
            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)

            // Allocate stack for path buffer (256 bytes)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 1us)

            // Save path and content pointers
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X22, contentReg)

            // Copy path to stack buffer
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            ARM64.CBZ_offset (ARM64.X2, 7)
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // open(path, flags, mode)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X1, flags, 0)
            ARM64.MOVZ (ARM64.X2, 420us, 0)  // mode 0644
            ARM64.MOVZ (ARM64.X16, syscalls.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            ARM64.MOV_reg (ARM64.X21, ARM64.X0)
            ARM64.TBNZ (ARM64.X0, 63, 18)  // If negative, branch to error path (17 instructions + 1)

            // write(fd, buf, count)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.ADD_imm (ARM64.X1, ARM64.X22, 8us)
            ARM64.LDR (ARM64.X2, ARM64.X22, 0s)
            ARM64.MOVZ (ARM64.X16, syscalls.Write, 0)
            ARM64.SVC syscalls.SvcImmediate

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X16, syscalls.Close, 0)
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
            ARM64.B 30  // Jump to cleanup (skip 29 error path instructions + 1)

            // Error path
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 5us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            ARM64.MOVZ (ARM64.X0, 69us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 9us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 10us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 111us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 11us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 12us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 13us)
            ARM64.STR (ARM64.X0, ARM64.X1, 0s)

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
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 1us)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 32s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]
