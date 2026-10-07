(*
   PrintValues.fs - Generate ARM64 nonterminating value printers.
*)
open Immediates




(*
   Generate ARM64 instructions to print int64 in X0 to stdout with newline (NO EXIT)
   Same as generatePrintInt64 but returns instead of exiting
   Allocate 32 bytes on stack for buffer
   Setup: X1 = buffer pointer, X2 = value
   Store newline at end of buffer
   Initialize: X6 = 0 (positive flag), X3 = 10 (divisor)
   Check for negative: if X2 < 0, branch to handle_negative (at instruction 33)
   33 - 8 = 25
   Check for zero: if X2 == 0, branch to print_zero (at instruction 29)
   29 - 9 = 20
   digit_loop: Extract digits
   store_minus_if_needed
   write_output
   Deallocate stack
   Skip past print_zero (4) and handle_negative (3) + 1 to exit runtime (8 instructions)
   print_zero (at instruction 29)
   Jump to store_minus_if_needed: 17 - 32 = -15
   handle_negative (at instruction 33)
   Jump to check_zero: 9 - 35 = -26
*)
let generatePrintInt64NoExit (target: ARM64.targetConfig) =
    let syscalls = ARM64.targetSyscalls target in
    [

        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);


        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31);
        ARM64.MOV_reg (ARM64.X2, ARM64.X0);


        ARM64.MOVZ (ARM64.X3, 10, 0);
        ARM64.STRB (ARM64.X3, ARM64.X1, 0);


        ARM64.MOVZ (ARM64.X6, 0, 0);
        ARM64.MOVZ (ARM64.X3, 10, 0);


        ARM64.CMP_imm (ARM64.X2, 0);
        ARM64.B_cond (ARM64.LT, 25);


        ARM64.CBZ_offset (ARM64.X2, 20);


        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3);
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2);
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48);
        ARM64.STRB (ARM64.X5, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.MOV_reg (ARM64.X2, ARM64.X4);
        ARM64.CBNZ_offset (ARM64.X2, -6);


        ARM64.CBZ_offset (ARM64.X6, 4);
        ARM64.MOVZ (ARM64.X3, 45, 0);
        ARM64.STRB (ARM64.X3, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);


        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32);
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
        ARM64.B (8);


        ARM64.MOVZ (ARM64.X2, 48, 0);
        ARM64.STRB (ARM64.X2, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.B (-15);


        ARM64.NEG (ARM64.X2, ARM64.X2);
        ARM64.MOVZ (ARM64.X6, 1, 0);
        ARM64.B (-26);
    ]


(*
   Generate ARM64 instructions to print uint64 in X0 to stdout with newline (NO EXIT)
*)
let generatePrintUInt64NoExit (target: ARM64.targetConfig) =
    let syscalls = ARM64.targetSyscalls target in
    [
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31);
        ARM64.MOV_reg (ARM64.X2, ARM64.X0);
        ARM64.MOVZ (ARM64.X3, 10, 0);
        ARM64.STRB (ARM64.X3, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.MOVZ (ARM64.X3, 10, 0);
        ARM64.CBZ_offset (ARM64.X2, 16);
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3);
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2);
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48);
        ARM64.STRB (ARM64.X5, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.MOV_reg (ARM64.X2, ARM64.X4);
        ARM64.CBNZ_offset (ARM64.X2, -6);
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32);
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
        ARM64.B 5;
        ARM64.MOVZ (ARM64.X2, 48, 0);
        ARM64.STRB (ARM64.X2, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.B (-11);
    ]



(*
   Generate ARM64 instructions to print int64 in X0 to stderr with newline (NO EXIT)
   Same as generatePrintInt64NoExit but writes to file descriptor 2
   Allocate 32 bytes on stack for buffer
   Setup: X1 = buffer pointer, X2 = value
   Store newline at end of buffer
   Initialize: X6 = 0 (positive flag), X3 = 10 (divisor)
   Check for negative: if X2 < 0, branch to handle_negative (at instruction 33)
   33 - 8 = 25
   Check for zero: if X2 == 0, branch to print_zero (at instruction 29)
   29 - 9 = 20
   digit_loop: Extract digits
   store_minus_if_needed
   write_output
   Deallocate stack
   Skip past print_zero (4) and handle_negative (3) + 1 to exit runtime (8 instructions)
   print_zero (at instruction 29)
   Jump to store_minus_if_needed: 17 - 32 = -15
   handle_negative (at instruction 33)
   Jump to check_zero: 9 - 35 = -26
*)
let generatePrintInt64ToStderrNoExit (target: ARM64.targetConfig) =
    let syscalls = ARM64.targetSyscalls target in
    [

        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);


        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31);
        ARM64.MOV_reg (ARM64.X2, ARM64.X0);


        ARM64.MOVZ (ARM64.X3, 10, 0);
        ARM64.STRB (ARM64.X3, ARM64.X1, 0);


        ARM64.MOVZ (ARM64.X6, 0, 0);
        ARM64.MOVZ (ARM64.X3, 10, 0);


        ARM64.CMP_imm (ARM64.X2, 0);
        ARM64.B_cond (ARM64.LT, 25);


        ARM64.CBZ_offset (ARM64.X2, 20);


        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3);
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2);
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48);
        ARM64.STRB (ARM64.X5, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.MOV_reg (ARM64.X2, ARM64.X4);
        ARM64.CBNZ_offset (ARM64.X2, -6);


        ARM64.CBZ_offset (ARM64.X6, 4);
        ARM64.MOVZ (ARM64.X3, 45, 0);
        ARM64.STRB (ARM64.X3, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);


        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32);
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 2, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
        ARM64.B (8);


        ARM64.MOVZ (ARM64.X2, 48, 0);
        ARM64.STRB (ARM64.X2, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.B (-15);


        ARM64.NEG (ARM64.X2, ARM64.X2);
        ARM64.MOVZ (ARM64.X6, 1, 0);
        ARM64.B (-26);
    ]



(*
   Generate ARM64 instructions to print boolean in X0 to stdout with newline (NO EXIT)
   Same as generatePrintBool but returns instead of exiting
   Allocate 16 bytes on stack for buffer
   Check if false (X0 == 0), branch to print_false (+17 instructions)
   print_true: Store "true\n" on stack (5 bytes)
   't'
   'r'
   'u'
   'e'
   '\n'
   length = 5
   Write and cleanup (no exit)
   Jump to cleanup (+18 instructions to skip false branch)
   print_false: Store "false\n" on stack (6 bytes)
   'f'
   'a'
   'l'
   's'
   length = 6
   Write
   cleanup (no exit):
*)
let generatePrintBoolNoExit (target: ARM64.targetConfig) =
    let syscalls = ARM64.targetSyscalls target in
    [

        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16);


        ARM64.CBZ_offset (ARM64.X0, 17);


        ARM64.MOVZ (ARM64.X3, 116, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 0);
        ARM64.MOVZ (ARM64.X3, 114, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 1);
        ARM64.MOVZ (ARM64.X3, 117, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 2);
        ARM64.MOVZ (ARM64.X3, 101, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 3);
        ARM64.MOVZ (ARM64.X3, 10, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 4);
        ARM64.MOVZ (ARM64.X2, 5, 0);


        ARM64.MOV_reg (ARM64.X1, ARM64.SP);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.B (18);


        ARM64.MOVZ (ARM64.X3, 102, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 0);
        ARM64.MOVZ (ARM64.X3, 97, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 1);
        ARM64.MOVZ (ARM64.X3, 108, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 2);
        ARM64.MOVZ (ARM64.X3, 115, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 3);
        ARM64.MOVZ (ARM64.X3, 101, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 4);
        ARM64.MOVZ (ARM64.X3, 10, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 5);
        ARM64.MOVZ (ARM64.X2, 6, 0);


        ARM64.MOV_reg (ARM64.X1, ARM64.SP);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;


        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16);
    ]



(*
   Generate ARM64 instructions to print int64 in X0 to stdout WITHOUT newline
   For use in tuple/list element printing
   Allocate 32 bytes on stack for buffer
   Setup: X1 = buffer pointer (start at end-1, no newline), X2 = value
   One less than with newline
   Initialize: X6 = 0 (positive flag), X3 = 10 (divisor)
   Check for negative: if X2 < 0, branch to handle_negative (at index 31)
   31 - 6 = 25
   Check for zero: if X2 == 0, branch to print_zero (at index 27)
   27 - 7 = 20
   digit_loop: Extract digits
   store_minus_if_needed
   write_output
   End of buffer area
   Deallocate stack
   Skip past print_zero (4) and handle_negative (3) + 1 to exit
   print_zero (at index 27)
   Jump to store_minus_if_needed at index 15: 15 - 30 = -15
   handle_negative (at index 31)
   Jump to digit_loop at index 8: 8 - 33 = -25
*)
let generatePrintInt64NoNewline (target: ARM64.targetConfig) =
    let syscalls = ARM64.targetSyscalls target in
    [

        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);


        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 30);
        ARM64.MOV_reg (ARM64.X2, ARM64.X0);


        ARM64.MOVZ (ARM64.X6, 0, 0);
        ARM64.MOVZ (ARM64.X3, 10, 0);


        ARM64.CMP_imm (ARM64.X2, 0);
        ARM64.B_cond (ARM64.LT, 25);


        ARM64.CBZ_offset (ARM64.X2, 20);


        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3);
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2);
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48);
        ARM64.STRB (ARM64.X5, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.MOV_reg (ARM64.X2, ARM64.X4);
        ARM64.CBNZ_offset (ARM64.X2, -6);


        ARM64.CBZ_offset (ARM64.X6, 4);
        ARM64.MOVZ (ARM64.X3, 45, 0);
        ARM64.STRB (ARM64.X3, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);


        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 31);
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
        ARM64.B (8);


        ARM64.MOVZ (ARM64.X2, 48, 0);
        ARM64.STRB (ARM64.X2, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.B (-15);


        ARM64.NEG (ARM64.X2, ARM64.X2);
        ARM64.MOVZ (ARM64.X6, 1, 0);
        ARM64.B (-25);
    ]


(*
   Generate ARM64 instructions to print uint64 in X0 to stdout WITHOUT newline
*)
let generatePrintUInt64NoNewline (target: ARM64.targetConfig) =
    let syscalls = ARM64.targetSyscalls target in
    [
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32);
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 30);
        ARM64.MOV_reg (ARM64.X2, ARM64.X0);
        ARM64.MOVZ (ARM64.X3, 10, 0);
        ARM64.CBZ_offset (ARM64.X2, 16);
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3);
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2);
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48);
        ARM64.STRB (ARM64.X5, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.MOV_reg (ARM64.X2, ARM64.X4);
        ARM64.CBNZ_offset (ARM64.X2, -6);
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 31);
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32);
        ARM64.B 5;
        ARM64.MOVZ (ARM64.X2, 48, 0);
        ARM64.STRB (ARM64.X2, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.B (-11);
    ]



(*
   Generate ARM64 instructions to print boolean in X0 to stdout WITHOUT newline
   For use in tuple/list element printing
   Allocate 16 bytes on stack for buffer
   Check if false (X0 == 0), branch to print_false at index 16
   16 - 1 = 15
   print_true: Store "true" on stack (4 bytes, no newline)
   't'
   'r'
   'u'
   'e'
   length = 4 (no newline)
   Write and cleanup
   Jump to cleanup at index 31: 31 - 15 = 16
   print_false: Store "false" on stack (5 bytes, no newline)
   'f'
   'a'
   'l'
   's'
   length = 5 (no newline)
   Write
   cleanup:
*)
let generatePrintBoolNoNewline (target: ARM64.targetConfig) =
    let syscalls = ARM64.targetSyscalls target in
    [

        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16);


        ARM64.CBZ_offset (ARM64.X0, 15);


        ARM64.MOVZ (ARM64.X3, 116, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 0);
        ARM64.MOVZ (ARM64.X3, 114, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 1);
        ARM64.MOVZ (ARM64.X3, 117, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 2);
        ARM64.MOVZ (ARM64.X3, 101, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 3);
        ARM64.MOVZ (ARM64.X2, 4, 0);


        ARM64.MOV_reg (ARM64.X1, ARM64.SP);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.B (16);


        ARM64.MOVZ (ARM64.X3, 102, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 0);
        ARM64.MOVZ (ARM64.X3, 97, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 1);
        ARM64.MOVZ (ARM64.X3, 108, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 2);
        ARM64.MOVZ (ARM64.X3, 115, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 3);
        ARM64.MOVZ (ARM64.X3, 101, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 4);
        ARM64.MOVZ (ARM64.X2, 5, 0);


        ARM64.MOV_reg (ARM64.X1, ARM64.SP);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;


        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16);
    ]




(*
   Generate ARM64 instructions to print float in D0 to stdout WITHOUT newline
   For use in tuple/list element printing
   Similar to generatePrintFloat but doesn't add newline or exit
   Allocate 48 bytes on stack for buffer
   Save D0 at [SP+32]
   Check if negative using D0's sign bit
   X6 = 0 (assume positive)
   Get bit pattern
   If sign bit set, set X6 = 1
   Skip setting X6
   X6 = 1 (negative)
   Setup: X1 = buffer pointer (start at end)
   Extract integer part
   Check if integer part is zero
   Branch to print_zero_int (instruction 69)
   convert_int_loop
   store_minus_if_needed
   print_integer_part
   print_decimal_point
   Extract and print fractional part (1 or 2 decimal digits)
   Take absolute value of X7
   Extract digits
   Print one or two digits (trim trailing zero)
   Cleanup (no newline, no exit)
   Skip past print_zero_int (4 instructions + 1 to land after)
   print_zero_int (instruction 69)
   Branch back to store_minus_if_needed (instruction 24)
*)
let generatePrintFloatNoNewline (target: ARM64.targetConfig) =
    let syscalls = ARM64.targetSyscalls target in
    [

        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48);


        ARM64.STR_fp (ARM64.D0, ARM64.SP, 32);


        ARM64.MOVZ (ARM64.X6, 0, 0);
        ARM64.FMOV_to_gp (ARM64.X0, ARM64.D0);
        ARM64.TBNZ (ARM64.X0, 63, 2);
        ARM64.B (2);
        ARM64.MOVZ (ARM64.X6, 1, 0);


        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31);


        ARM64.FCVTZS (ARM64.X0, ARM64.D0);
        ARM64.TBNZ (ARM64.X0, 63, 3);
        ARM64.MOV_reg (ARM64.X2, ARM64.X0);
        ARM64.B (2);
        ARM64.NEG (ARM64.X2, ARM64.X0);


        ARM64.CBZ_offset (ARM64.X2, 55);


        ARM64.MOVZ (ARM64.X3, 10, 0);
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3);
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2);
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48);
        ARM64.STRB (ARM64.X5, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.MOV_reg (ARM64.X2, ARM64.X4);
        ARM64.CBZ_offset (ARM64.X2, 2);
        ARM64.B (-8);


        ARM64.CBZ_offset (ARM64.X6, 4);
        ARM64.MOVZ (ARM64.X3, 45, 0);
        ARM64.STRB (ARM64.X3, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);


        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32);
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1);
        ARM64.STR (ARM64.X6, ARM64.SP, 40);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;


        ARM64.MOVZ (ARM64.X3, 46, 0);
        ARM64.STRB (ARM64.X3, ARM64.SP, 0);
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOV_reg (ARM64.X1, ARM64.SP);
        ARM64.MOVZ (ARM64.X2, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;


        ARM64.LDR_fp (ARM64.D0, ARM64.SP, 32);
        ARM64.FCVTZS (ARM64.X0, ARM64.D0);
        ARM64.SCVTF (ARM64.D1, ARM64.X0);
        ARM64.FSUB (ARM64.D0, ARM64.D0, ARM64.D1);
        ARM64.MOVZ (ARM64.X0, 100, 0);
        ARM64.SCVTF (ARM64.D1, ARM64.X0);
        ARM64.FMUL (ARM64.D0, ARM64.D0, ARM64.D1);
        ARM64.FCVTZS (ARM64.X7, ARM64.D0);


        ARM64.TBNZ (ARM64.X7, 63, 2);
        ARM64.B (2);
        ARM64.NEG (ARM64.X7, ARM64.X7);


        ARM64.MOVZ (ARM64.X3, 10, 0);
        ARM64.UDIV (ARM64.X4, ARM64.X7, ARM64.X3);
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X7);
        ARM64.ADD_imm (ARM64.X4, ARM64.X4, 48);
        ARM64.STRB (ARM64.X4, ARM64.SP, 1);
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48);
        ARM64.STRB (ARM64.X5, ARM64.SP, 2);


        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 1);
        ARM64.SUBS_imm (ARM64.X5, ARM64.X5, 48);
        ARM64.CSET (ARM64.X2, ARM64.NE);
        ARM64.ADD_imm (ARM64.X2, ARM64.X2, 1);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;


        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48);
        ARM64.B (5);


        ARM64.MOVZ (ARM64.X2, 48, 0);
        ARM64.STRB (ARM64.X2, ARM64.X1, 0);
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1);
        ARM64.B (-48);
    ]




(*
   Generate ARM64 instructions to print heap string WITHOUT newline
   Expects: X9 = data address, X10 = length
   For use in tuple/list element printing
   Write string to stdout
   fd = stdout
   buffer = X9
   length = X10
*)
let generatePrintStringNoNewline (target: ARM64.targetConfig) =
    let syscalls = ARM64.targetSyscalls target in
    [

        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOV_reg (ARM64.X1, ARM64.X9);
        ARM64.MOV_reg (ARM64.X2, ARM64.X10);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
    ]











(*
   Generate ARM64 instructions to print a sequence of literal characters
   Used for printing delimiters like "(", ")", "[", "]", ", " etc.
   Algorithm:
   1. Allocate aligned stack buffer
   2. Store each byte on stack
   3. Write to stdout via syscall
   4. Deallocate stack
   No newline is added - caller controls newlines
   Stack allocation must be 16-byte aligned
   Allocate stack buffer
   Write to stdout
   buffer
   stdout = 1
   Deallocate stack
*)
let generatePrintChars (target: ARM64.targetConfig) (chars: int list) =
    if chars=[] then [] else
    let syscalls = ARM64.targetSyscalls target in
    let len = List.length chars in

    let stackSize = max 16 (Int32.to_int (Int32.mul (Int32.div (Int32.add (Int32.of_int len) 15l) 16l) 16l)) in
    [

        ARM64.SUB_imm (ARM64.SP, ARM64.SP, (stackSize land 65535));
    ]
    @ (chars |> List.mapi (fun i b ->
        [
            ARM64.MOVZ (ARM64.X3, b, 0);
            ARM64.STRB (ARM64.X3, ARM64.SP, i);
        ]) |> List.concat)
    @ [

        ARM64.MOV_reg (ARM64.X1, ARM64.SP);
    ]
    @ generateLoadNonNegativeIntImmediate ARM64.X2 len
    @ [
        ARM64.MOVZ (ARM64.X0, 1, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;

        ARM64.ADD_imm (ARM64.SP, ARM64.SP, (stackSize land 65535));
    ]


(*
   Generate ARM64 instructions to print literal characters to stderr
*)
let generatePrintCharsToStderr (target: ARM64.targetConfig) (chars: int list) =
    if chars=[] then [] else
    let syscalls = ARM64.targetSyscalls target in
    let len = List.length chars in
    let stackSize = max 16 (Int32.to_int (Int32.mul (Int32.div (Int32.add (Int32.of_int len) 15l) 16l) 16l)) in
    [
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, (stackSize land 65535));
    ]
    @ (chars |> List.mapi (fun i b ->
        [
            ARM64.MOVZ (ARM64.X3, b, 0);
            ARM64.STRB (ARM64.X3, ARM64.SP, i);
        ]) |> List.concat)
    @ [
        ARM64.MOV_reg (ARM64.X1, ARM64.SP);
    ]
    @ generateLoadNonNegativeIntImmediate ARM64.X2 len
    @ [
        ARM64.MOVZ (ARM64.X0, 2, 0);
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, (stackSize land 65535));
    ]



(*
   Generate ARM64 instructions for the interpreter-compatible ephemeral Blob
   rendering. Blob contents and process-local identity stay opaque.
*)
let generatePrintBlob (target: ARM64.targetConfig) =
    generatePrintChars
        target
        [ 60;
          66;
          108;
          111;
          98;
          58;
          32;
          101;
          112;
          104;
          101;
          109;
          101;
          114;
          97;
          108;
          62;
          10 ]









(*
   Generate ARM64 instructions to perform write syscall only
   Assumes caller has set up:
   - X0 = file descriptor (usually 1 for stdout)
   - X1 = buffer pointer
   - X2 = length
   Does NOT print newline or exit - caller handles those if needed
*)
let generateWriteSyscall (target: ARM64.targetConfig) =
    let syscalls = ARM64.targetSyscalls target in
    [
        ARM64.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
        ARM64.SVC syscalls.ARM64.svcImmediate;
    ]
