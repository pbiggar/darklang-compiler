(*
   ISA.fs - ARM64 Instruction Types
   Defines ARM64 instruction and register types.
   ARM64 is a RISC architecture with fixed 32-bit instruction width.
   These types represent ARM64 assembly instructions that will be encoded
   to machine code by the ARM64_Encoding pass.
   Supported instructions:
   - MOVZ/MOVK: Load immediate values (16-bit chunks)
   - ADD/SUB: Arithmetic (immediate and register forms)
   - MUL/SDIV: Multiplication and signed division
   - MOV: Register-to-register move
   - STP/LDP: Store/Load pair (for stack frames)
   - STR/LDR: Store/Load register (for stack slots)
   - BL: Branch with link (function calls)
   - RET: Return from function
   Example instructions:
   MOVZ X0, #42, LSL #0
   ADD X1, X0, #5
   RET
   ARM64 general-purpose registers
   X16/X17 are IP0/IP1 scratch registers
   Platform register; never allocated by generated code
   Callee-saved
   Reserved for free list base pointer
   Reserved for heap bump pointer
*)
type reg = 
 | X0
 | X1
 | X2
 | X3
 | X4
 | X5
 | X6
 | X7
 | X8
 | X9
 | X10
 | X11
 | X12
 | X13
 | X14
 | X15
 | X16
 | X17
 | X18
 | X19
 | X20
 | X21
 | X22
 | X23
 | X24
 | X25
 | X26
 | X27
 | X28
 | X29
 | X30
 | SP
(*
   ARM64 floating-point registers (D0-D31 for double precision)
   D0-D7: Argument/result registers (caller-saved)
   D8-D15: Callee-saved registers
   D16-D31: Additional caller-saved registers
   Used as temp for FArgMoves cycle breaking
   Used as temp for binary ops (right operand)
   Used as temp for binary ops (left operand)
   Used for float call arg temps
   Additional SSA temp registers
*)
type fReg = 
 | D0
 | D1
 | D2
 | D3
 | D4
 | D5
 | D6
 | D7
 | D8
 | D9
 | D10
 | D11
 | D12
 | D13
 | D14
 | D15
 | D16
 | D17
 | D18
 | D19
 | D20
 | D21
 | D22
 | D23
 | D24
 | D25
 | D26
 | D27
 | D28
 | D29
 | D30
 | D31
(*
   Comparison conditions (for CSET)
   Equal (Z set)
   Not equal (Z clear)
   Less than (signed)
   Greater than (signed)
   Less than or equal (signed)
   Greater than or equal (signed)
   Lower than (unsigned)
   Higher than (unsigned)
   Lower than or same (unsigned)
   Higher than or same (unsigned)
*)
type condition = 
 | EQ
 | NE
 | LT
 | GT
 | LE
 | GE
 | LO
 | HI
 | LS
 | HS
type extend = 
 | ExtendUXTB
 | ExtendUXTH
 | ExtendUXTW
 | ExtendSXTB
 | ExtendSXTH
 | ExtendSXTW
(*
   ARM64 instruction types
   Move with zero
   Move with NOT (for negative constants)
   Move with keep
   ADD with shifted register: dest = src1 + (src2 << shift)
   SUB with shift=12, value = imm * 4096
   SUB with shifted register: dest = src1 - (src2 << shift)
   SUB and set flags (for fused SUB+CMP)
   Unsigned division (for positive integers)
   Multiply-subtract: dest = src3 - src1 * src2 (for modulo)
   Multiply-add: dest = src3 + src1 * src2
   Compare with immediate (sets condition flags)
   Compare registers (sets condition flags)
   Set register to 1 if condition, 0 otherwise
   Bitwise AND
   Bit clear: dest = src1 AND NOT src2
   Bitwise AND with immediate (bitmask)
   Bitwise OR
   Bitwise XOR (exclusive or)
   Logical shift left by register
   Logical shift right by register
   Arithmetic shift right by register
   Logical shift left by immediate (0-63)
   Logical shift right by immediate (0-63)
   Arithmetic shift right by immediate (0-63)
   Bitwise NOT
   Store byte [addr + offset] = src (lower 8 bits)
   Load byte with register offset: dest = [baseAddr + index]
   Load byte with immediate offset: dest = [baseAddr + offset]
   Store byte: [addr] = src (lower 8 bits)
   Stack operations (for function calls and stack frames)
   Store pair: [addr + offset] = reg1, [addr + offset + 8] = reg2
   Store pair with pre-index: addr += offset, then store
   Load pair: reg1 = [addr + offset], reg2 = [addr + offset + 8]
   Load pair with post-index: load, then addr += offset
   Store register (unsigned offset): [addr + offset] = src (64-bit)
   Load register (unsigned offset): dest = [addr + offset] (64-bit)
   Store register (signed offset, -256 to +255): [addr + offset] = src (64-bit)
   Load register (signed offset, -256 to +255): dest = [addr + offset] (64-bit)
   Branch with link: call function at label (sets X30/LR to return address)
   Branch with link to register: call function at address in reg (indirect call)
   Branch to register without link: tail call to address in reg (no return)
   Label-based branches (for compiler-generated code with CFG)
   Compare and branch if zero (label will be resolved)
   Compare and branch if not zero
   Unconditional branch to label
   Conditional branch to label
   Offset-based branches (for handcrafted runtime code with known offsets)
   CBZ with immediate offset
   CBNZ with immediate offset
   Test bit and branch if zero
   Test bit and branch if not zero
   Test bit and branch if zero (label will be resolved)
   Test bit and branch if not zero (label will be resolved)
   Unconditional branch with immediate offset
   Conditional branch with immediate offset
   Negate: dest = 0 - src
   Supervisor call (syscall)
   Pseudo-instruction: marks a label position
   PC-relative addressing for .rodata access
   Address page: dest = PC-relative page address of label
   Add label offset: dest = src + page offset of label
   PC-relative address: dest = address of label (±1MB range)
   Floating-point instructions
   Load double from [addr + offset]
   Store double to [addr + offset]
   Store FP pair: [addr + offset] = freg1, [addr + offset + 8] = freg2
   Load FP pair: freg1 = [addr + offset], freg2 = [addr + offset + 8]
   FP add: dest = src1 + src2
   FP sub: dest = src1 - src2
   FP mul: dest = src1 * src2
   FP div: dest = src1 / src2
   FP negate: dest = -src
   FP absolute value: dest = |src|
   FP square root: dest = sqrt(src)
   FP compare (sets condition flags)
   FP move between registers
   FP immediate materialization
   FP to GP register (bit-for-bit)
   GP to FP register (bit-for-bit)
   Signed int to FP: dest = (double)src
   FP to signed int (truncate): dest = (int64)src
   Sign/zero extension instructions (for integer overflow truncation)
   Sign-extend byte: dest = sign_extend(src[7:0])
   Sign-extend halfword: dest = sign_extend(src[15:0])
   Sign-extend word: dest = sign_extend(src[31:0])
   Zero-extend byte: dest = zero_extend(src[7:0])
   Zero-extend halfword: dest = zero_extend(src[15:0])
   Zero-extend word: dest = zero_extend(src[31:0])
*)
type instr = 
 | MOVZ of reg * int * int
 | MOVN of reg * int * int
 | MOVK of reg * int * int
 | ADD_imm of reg * reg * int
 | ADD_reg of reg * reg * reg
 | ADD_shifted of reg * reg * reg * int
 | ADD_extended of reg * reg * reg * extend
 | SUB_imm of reg * reg * int
 | SUB_imm12 of reg * reg * int
 | SUB_reg of reg * reg * reg
 | SUB_shifted of reg * reg * reg * int
 | SUB_extended of reg * reg * reg * extend
 | SUBS_imm of reg * reg * int
 | MUL of reg * reg * reg
 | SDIV of reg * reg * reg
 | UDIV of reg * reg * reg
 | MSUB of reg * reg * reg * reg
 | MADD of reg * reg * reg * reg
 | CMP_imm of reg * int
 | CMP_reg of reg * reg
 | CSET of reg * condition
 | CSEL of reg * reg * reg * condition
 | AND_reg of reg * reg * reg
 | BIC_reg of reg * reg * reg
 | AND_imm of reg * reg * int64
 | ORR_reg of reg * reg * reg
 | EOR_reg of reg * reg * reg
 | LSL_reg of reg * reg * reg
 | LSR_reg of reg * reg * reg
 | ASR_reg of reg * reg * reg
 | LSL_imm of reg * reg * int
 | LSR_imm of reg * reg * int
 | ASR_imm of reg * reg * int
 | MVN of reg * reg
 | MOV_reg of reg * reg
 | STRB of reg * reg * int
 | LDRB of reg * reg * reg
 | LDRB_imm of reg * reg * int
 | STRB_reg of reg * reg
 | STP of reg * reg * reg * int
 | STP_pre of reg * reg * reg * int
 | LDP of reg * reg * reg * int
 | LDP_post of reg * reg * reg * int
 | STR of reg * reg * int
 | LDR of reg * reg * int
 | STUR of reg * reg * int
 | LDUR of reg * reg * int
 | BL of string
 | BLR of reg
 | BR of reg
 | CBZ of reg * string
 | CBNZ of reg * string
 | B_label of string
 | B_cond_label of condition * string
 | CBZ_offset of reg * int
 | CBNZ_offset of reg * int
 | TBZ of reg * int * int
 | TBNZ of reg * int * int
 | TBZ_label of reg * int * string
 | TBNZ_label of reg * int * string
 | B of int
 | B_cond of condition * int
 | NEG of reg * reg
 | RET
 | SVC of int
 | Label of string
 | ADRP of reg * string
 | ADD_label of reg * reg * string
 | ADR of reg * string
 | LDR_fp of fReg * reg * int
 | STR_fp of fReg * reg * int
 | STP_fp of fReg * fReg * reg * int
 | LDP_fp of fReg * fReg * reg * int
 | FADD of fReg * fReg * fReg
 | FSUB of fReg * fReg * fReg
 | FMUL of fReg * fReg * fReg
 | FMADD of fReg * fReg * fReg * fReg
 | FDIV of fReg * fReg * fReg
 | FNEG of fReg * fReg
 | FABS of fReg * fReg
 | FSQRT of fReg * fReg
 | FCMP of fReg * fReg
 | FMOV_reg of fReg * fReg
 | FMOV_imm of fReg * float
 | FMOV_to_gp of reg * fReg
 | FMOV_from_gp of fReg * reg
 | SCVTF of fReg * reg
 | FCVTZS of reg * fReg
 | SXTB of reg * reg
 | SXTH of reg * reg
 | SXTW of reg * reg
 | UXTB of reg * reg
 | UXTH of reg * reg
 | UXTW of reg * reg
(*
   Machine code (32-bit instruction)
*)
type machineCode = int32
(*
   ARM64-specific syscall invocation details (layered on top of Platform.SyscallNumbers).
   Platform.fs intentionally has no ARM64 dependency, so these are defined here.
   SVC instruction immediate value
   Register to hold syscall number (X16 macOS, X8 Linux)
*)
type syscallConfig = {numbers:Platform.syscallNumbers;svcImmediate:int;syscallRegister:reg}
(*
   Return the ARM64 modified-immediate byte for an encodable double-precision
   `FMOV` scalar immediate.
*)
let tryEncodeFmovFloatImmediate value =
 let candidates=List.concat_map (fun signBit -> List.concat_map (fun exponentBits -> List.init 16 (fun fractionBits ->
 let sign=if signBit=0 then 1. else -1. in
 let exponent=if exponentBits>=4 then exponentBits-7 else exponentBits+1 in
 let significand=1.+.(float_of_int fractionBits/.16.) in
 let candidate=sign*.significand*.Float.ldexp 1. exponent in
 let encoded=Int32.of_int ((signBit lsl 7) lor (exponentBits lsl 4) lor fractionBits) in
 candidate,encoded)) (List.init 8 Fun.id)) [0;1] in
 List.find_map (fun (candidate,encoded) -> if candidate=value then Some encoded else None) candidates
(*
   Build the ARM64-specific syscall config for the given OS.
*)
let syscallConfigFor = function
 | Platform.MacOS -> {numbers=Platform.syscallNumbersFor (Platform.ARM64Backend Platform.MacOSARM64);svcImmediate=0x80;syscallRegister=X16}
 | Platform.Linux -> {numbers=Platform.syscallNumbersFor (Platform.ARM64Backend Platform.LinuxARM64);svcImmediate=0;syscallRegister=X8}
(*
   Validated platform configuration threaded through ARM64 code generation.
*)
type targetConfig = {os:Platform.os;syscalls:syscallConfig}
let targetConfigFor target =
 let os=match target with Platform.MacOSARM64 -> Platform.MacOS | Platform.LinuxARM64 -> Platform.Linux in
 {os;syscalls=syscallConfigFor os}
let targetOS config=config.os
let targetSyscalls config=config.syscalls
