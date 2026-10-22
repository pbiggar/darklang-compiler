// Operands.fs - Materialize target operands, registers, and immediates.

module X64Operands

let internal syscalls = Platform.linuxX86_64SyscallNumbers

let private invalidX64PhysRegReason (reg: LIR.PhysReg) : string option =
    match reg with
    | LIR.X0
    | LIR.X1
    | LIR.X2
    | LIR.X3
    | LIR.X4
    | LIR.X5
    | LIR.X6
    | LIR.X7
    | LIR.X8
    | LIR.X9
    | LIR.X10
    | LIR.X11
    | LIR.X12
    | LIR.X13
    | LIR.X14
    | LIR.X15
    | LIR.X16
    | LIR.X17
    | LIR.X19
    | LIR.X20
    | LIR.X21
    | LIR.X29
    | LIR.X30
    | LIR.SP ->
        None
    | LIR.X22 ->
        Some "X22 maps to the x64 heap pointer runtime register"
    | LIR.X23 ->
        Some "X23 maps to the x64 free-list runtime register"
    | LIR.X24
    | LIR.X25
    | LIR.X26 ->
        Some $"{reg} has no allocatable x64 register mapping"
    | LIR.X27 ->
        Some "X27 is reserved runtime state and cannot be lowered on x64"

/// Map LIR.PhysReg to x86-64 register
let lirRegToX86 (reg: LIR.PhysReg) : X86_64.Reg =
    match reg with
    | LIR.X0  -> X86_64.RAX   // Return value
    | LIR.X1  -> X86_64.RDI   // Arg 1
    | LIR.X2  -> X86_64.RSI   // Arg 2
    | LIR.X3  -> X86_64.RCX   // Arg 3 (NOT RDX — RDX is reserved for IDIV)
    | LIR.X4  -> X86_64.R8    // Arg 4
    | LIR.X5  -> X86_64.R9    // Arg 5
    | LIR.X6  -> X86_64.R10   // Arg 6 / caller-saved
    | LIR.X7  -> X86_64.RDX   // Caller-saved (only used when IDIV isn't active)
    | LIR.X8  -> X86_64.R11   // Scratch
    | LIR.X9  -> X86_64.R11   // Scratch (shared)
    | LIR.X10 -> X86_64.R11
    | LIR.X11 -> X86_64.R11
    | LIR.X12 -> X86_64.R11
    | LIR.X13 -> X86_64.R11
    | LIR.X14 -> X86_64.R11
    | LIR.X15 -> X86_64.R11
    | LIR.X16 -> X86_64.R11
    | LIR.X17 -> X86_64.R11
    | LIR.X19 -> X86_64.RBX   // Callee-saved 1
    | LIR.X20 -> X86_64.R12   // Callee-saved 2
    | LIR.X21 -> X86_64.R13   // Callee-saved 3
    | LIR.X22
    | LIR.X23
    | LIR.X24
    | LIR.X25
    | LIR.X26
    | LIR.X27 ->
        Crash.crash $"lirRegToX86: invalid x64 physical register {reg}"
    | LIR.X29 -> X86_64.RBP   // Frame pointer
    | LIR.X30 -> X86_64.RAX   // Link register (not applicable on x86_64)
    | LIR.SP  -> X86_64.RSP

let private resolvePhysReg (context: string) (reg: LIR.PhysReg) : Result<X86_64.Reg, string> =
    match invalidX64PhysRegReason reg with
    | Some reason ->
        Error $"{context}: invalid x64 physical register {reg}: {reason}"
    | None ->
        Ok (lirRegToX86 reg)

/// Map LIR.FReg to x86-64 XMM register
let lirFRegToX86 (freg: LIR.PhysFPReg) : X86_64.FReg =
    match freg with
    | LIR.D0  -> X86_64.XMM0  | LIR.D1  -> X86_64.XMM1
    | LIR.D2  -> X86_64.XMM2  | LIR.D3  -> X86_64.XMM3
    | LIR.D4  -> X86_64.XMM4  | LIR.D5  -> X86_64.XMM5
    | LIR.D6  -> X86_64.XMM6  | LIR.D7  -> X86_64.XMM7
    | LIR.D8  -> X86_64.XMM8  | LIR.D9  -> X86_64.XMM9
    | LIR.D10 -> X86_64.XMM10 | LIR.D11 -> X86_64.XMM11
    | LIR.D12 -> X86_64.XMM12 | LIR.D13 -> X86_64.XMM13
    | LIR.D14 -> X86_64.XMM14 | LIR.D15 -> X86_64.XMM15

/// Resolve a LIR.FReg to x86-64 XMM register.
let private resolveFreg (freg: LIR.FReg) : Result<X86_64.FReg, string> =
    match freg with
    | LIR.FPhysical fp -> Ok (lirFRegToX86 fp)
    | LIR.FVirtual id -> Error $"Unresolved virtual float register f{id} in x86-64 codegen"
    | LIR.FParallelMoveTemp -> Error "Parallel-move float temporary used outside a move"

/// Resolve a LIR.Reg (Physical or Virtual) to x86-64 register.
let resolveReg (reg: LIR.Reg) : Result<X86_64.Reg, string> =
    match reg with
    | LIR.Physical phys -> resolvePhysReg "resolveReg" phys
    | LIR.Virtual id -> Error $"Unresolved virtual register v{id} in x86-64 codegen"

/// Load a 64-bit immediate into a register.
let internal loadImm64 (dest: X86_64.Reg) (value: int64) : X86_64.Instr list =
    if value = 0L then
        [X86_64.XOR_reg (dest, dest)]
    elif value >= int64 System.Int32.MinValue && value <= int64 System.Int32.MaxValue then
        [X86_64.MOV_imm32 (dest, int32 value)]
    else
        [X86_64.MOV_imm (dest, value)]

/// Scratch register for temporaries in codegen
let internal scratch = X86_64.R11

let internal arithmeticTempExcluding (excluded: X86_64.Reg list) : X86_64.Reg =
    [ X86_64.R11
      X86_64.RCX
      X86_64.R10
      X86_64.RAX
      X86_64.RDX
      X86_64.RDI
      X86_64.RSI
      X86_64.R8
      X86_64.R9
      X86_64.RBX
      X86_64.R12
      X86_64.R13 ]
    |> List.tryFind (fun candidate -> not (List.contains candidate excluded))
    |> function
        | Some temp -> temp
        | None -> Crash.crash "x64 arithmetic lowering could not find a temporary register"

let private allFloatRegs : X86_64.FReg list =
    [ X86_64.XMM0; X86_64.XMM1; X86_64.XMM2; X86_64.XMM3
      X86_64.XMM4; X86_64.XMM5; X86_64.XMM6; X86_64.XMM7
      X86_64.XMM8; X86_64.XMM9; X86_64.XMM10; X86_64.XMM11
      X86_64.XMM12; X86_64.XMM13; X86_64.XMM14; X86_64.XMM15 ]

/// Borrow an XMM register for a short lowering sequence without reserving one
/// globally from allocation. The 16-byte slot preserves stack alignment.
let internal withPreservedFloatScratch
    (excluded: X86_64.FReg list)
    (build: X86_64.FReg -> X86_64.Instr list)
    : X86_64.Instr list =
    let temp =
        allFloatRegs
        |> List.tryFind (fun candidate -> not (List.contains candidate excluded))
        |> Option.defaultWith (fun () -> Crash.crash "x64 float lowering has no scratch register")
    [ X86_64.SUB_imm (X86_64.RSP, 16)
      X86_64.MOVSD_store (X86_64.RSP, 0, temp) ]
    @ build temp
    @ [ X86_64.MOVSD_load (temp, X86_64.RSP, 0)
        X86_64.ADD_imm (X86_64.RSP, 16) ]

/// Heap bump pointer register (codegen-internal, reserved; not allocatable).
let internal heapPtr = X86_64.R14

/// Free list base register (codegen-internal, reserved; not allocatable).
let internal freeListBase = X86_64.R15

/// Size of free list heads area (32 size classes × 8 bytes = 256 bytes)
let internal freeListSize = 256

/// The process table is raw runtime state, not a managed allocation. Keeping
/// it at a fixed address avoids aliasing a free-list head in batched programs.
let internal processTableOffset = freeListSize
let internal processTableSize = 4096

/// Max payload size class for free list reuse (freeListSize - 8)
let internal maxFreeListPayload = freeListSize - 8

/// Emit inline 8-byte-at-a-time copy of a UTF-8 byte array to heap memory.
/// Stores bytes starting after the two-word dynamic-buffer header.
let private emitStringByteCopy
    (valueReg: X86_64.Reg)
    (destReg: X86_64.Reg)
    (strBytes: byte array)
    : X86_64.Instr list =
    let len = strBytes.Length
    if len = 0 then []
    else
        let chunks = (len + 7) / 8
        [0 .. chunks - 1]
        |> List.collect (fun i ->
            let offset = 16 + i * 8
            let chunkLen = min 8 (len - i * 8)
            let value =
                [0 .. chunkLen - 1]
                |> List.fold (fun acc j ->
                    let byteIdx = i * 8 + j
                    if byteIdx < strBytes.Length then
                        acc ||| (int64 strBytes.[byteIdx] <<< (j * 8))
                    else acc) 0L
            loadImm64 valueReg value
            @ [X86_64.MOV_store (destReg, int32 offset, valueReg)])

/// Load a string from the executable's immutable literal pool.
let internal emitStringLiteral (destReg: X86_64.Reg) (value: string) : X86_64.Instr list =
    [X86_64.LEA_rip (destReg, X86_64.stringLiteralLabel value)]

/// File-operation path buffers use the canonical static buffer layout.
let internal emitStringLiteralNoRefCount (destReg: X86_64.Reg) (value: string) : X86_64.Instr list =
    emitStringLiteral destReg value

/// Heap size for mmap (512 MB)
let internal heapMmapSizeBytes = 512L * 1024L * 1024L

/// Generate x86-64 write(fd, buf, len) syscall
let internal genWriteSyscall : X86_64.Instr list =
    loadImm64 X86_64.RAX (int64 syscalls.Write) @ [X86_64.SYSCALL]

/// Write a small compile-time byte sequence to stdout from a balanced stack
/// buffer. The generated syscall may clobber RAX, RCX, and R11.
let internal genPrintChars (bytes: byte list) : X86_64.Instr list =
    let len = List.length bytes
    if len = 0 then
        []
    else
        let padded = ((len + 7) / 8) * 8
        let paddedBytes = bytes @ List.replicate (padded - len) 0uy
        let pushes =
            paddedBytes
            |> List.chunkBySize 8
            |> List.rev
            |> List.collect (fun chunk ->
                let value =
                    chunk
                    |> List.mapi (fun index value -> int64 value <<< (index * 8))
                    |> List.fold (|||) 0L
                loadImm64 scratch value @ [X86_64.PUSH scratch])
        pushes
        @ [ X86_64.MOV_imm32 (X86_64.RDI, 1)
            X86_64.MOV_reg (X86_64.RSI, X86_64.RSP) ]
        @ loadImm64 X86_64.RDX (int64 len)
        @ genWriteSyscall
        @ [X86_64.ADD_imm (X86_64.RSP, int32 padded)]

/// Generate x86-64 exit(code) syscall.
/// Exit code must already be in RDI.
let internal genExitSyscall : X86_64.Instr list =
    loadImm64 X86_64.RAX (int64 syscalls.Exit) @ [X86_64.SYSCALL]

/// Label for shared OOM handler (set per-program, not per-function)
let internal oomHandlerLabel = "__heap_oom"
let internal runtimeErrorHandlerLabel = "__dark_runtime_error"

/// Generate a jump to the shared OOM handler
let internal genOomJump () : X86_64.Instr list =
    [X86_64.JMP oomHandlerLabel]

/// Generate the shared OOM handler code (placed once at end of program)
let internal genOomHandler () : X86_64.Instr list =
    [X86_64.Label oomHandlerLabel]
    @ emitStringLiteral X86_64.R8 "Out of heap memory\n"
    @ [X86_64.JMP runtimeErrorHandlerLabel]

/// Shared non-returning writer for canonical error-string buffers in R8.
let internal genRuntimeErrorHandler () : X86_64.Instr list =
    [ X86_64.Label runtimeErrorHandlerLabel
      X86_64.MOV_load (X86_64.RDX, X86_64.R8, 8)
      X86_64.LEA (X86_64.RSI, X86_64.R8, 16)
      X86_64.MOV_imm32 (X86_64.RDI, 2) ]
    @ genWriteSyscall
    @ loadImm64 X86_64.RDI 1L
    @ genExitSyscall

/// Mutable counter for generating unique labels within a compilation
let mutable private labelCounter = 0
let internal freshLabel (prefix: string) : string =
    labelCounter <- labelCounter + 1
    $"__{prefix}_{labelCounter}"
