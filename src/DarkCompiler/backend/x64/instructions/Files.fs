// Files.fs - Emit x64 instructions for files operations.

module X64EmitFiles

open X64Operands
open X64CodeGenTypes
open X64FieldReferenceCounts
open X64InstructionContext

let internal emitFileReadBlob (ctx: FuncCtx) (dest: LIR.Reg) (path: LIR.Operand) : Result<X86_64.Instr list, string> =
    // File read: open → fstat → alloc → read → close → Result
    resolveReg dest
    |> Result.bind (fun destReg ->
        let resolvePathToR10 =
            match path with
            | LIR.Reg reg ->
                resolveReg reg |> Result.map (fun srcReg ->
                    if srcReg = X86_64.R10 then [] else [X86_64.MOV_reg (X86_64.R10, srcReg)])
            | LIR.StackSlot offset ->
                Ok [X86_64.MOV_load (X86_64.R10, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
            | LIR.StringSymbol value ->
                Ok (emitStringLiteralNoRefCount X86_64.R10 value)
            | _ ->
                Error "FileReadBlob path operand must be a string pointer or string literal"
        let copyLabel = freshLabel "fr_copy"
        let doneLabel = freshLabel "fr_done"
        let errorLabel = freshLabel "fr_err"
        let cleanupLabel = freshLabel "fr_clean"
        let openSyscall = int64 syscalls.Open
        let fstatSyscall = int64 syscalls.Fstat
        let readSyscall = int64 syscalls.Read
        let closeSyscall = int64 syscalls.Close
        resolvePathToR10 |> Result.map (fun pathSetup ->
            // Save registers that syscalls will clobber
            let saves = [X86_64.PUSH X86_64.RDI; X86_64.PUSH X86_64.RSI; X86_64.PUSH X86_64.RCX
                         X86_64.PUSH X86_64.R10; X86_64.PUSH X86_64.R8; X86_64.PUSH X86_64.R9]
            let restores = [X86_64.POP X86_64.R9; X86_64.POP X86_64.R8; X86_64.POP X86_64.R10
                            X86_64.POP X86_64.RCX; X86_64.POP X86_64.RSI; X86_64.POP X86_64.RDI]
            pathSetup @ saves
            // Allocate stack: 4096 bytes for path (PATH_MAX) + 144 bytes for stat buf = 4240
            @ [X86_64.SUB_imm (X86_64.RSP, 4240)]
            // Copy heap string to null-terminated C string on stack
            // R10 points to [refcount][length][data].
            @ [X86_64.MOV_load (X86_64.RCX, X86_64.R10, 8)
               X86_64.LEA (X86_64.RSI, X86_64.R10, 16)
               X86_64.LEA (X86_64.RDI, X86_64.RSP, 144)]       // RDI = stack buf (after stat buf)
            @ loadImm64 X86_64.R10 0L
            @ [X86_64.Label copyLabel
               X86_64.CMP_reg (X86_64.R10, X86_64.RCX)
               X86_64.Jcc (X86_64.GE, doneLabel)
               X86_64.MOV_reg (scratch, X86_64.RSI)
               X86_64.ADD_reg (scratch, X86_64.R10)
               X86_64.MOV_load_byte (scratch, scratch, 0)
               X86_64.MOV_reg (X86_64.R8, X86_64.RDI)
               X86_64.ADD_reg (X86_64.R8, X86_64.R10)
               X86_64.MOV_store_byte (X86_64.R8, 0, scratch)
               X86_64.ADD_imm (X86_64.R10, 1)
               X86_64.JMP copyLabel
               X86_64.Label doneLabel]
            // Null-terminate
            @ [X86_64.MOV_reg (scratch, X86_64.RDI)
               X86_64.ADD_reg (scratch, X86_64.RCX)]
            @ loadImm64 X86_64.R10 0L
            @ [X86_64.MOV_store_byte (scratch, 0, X86_64.R10)]
            // open(path, O_RDONLY=0, 0) → fd
            @ [X86_64.LEA (X86_64.RDI, X86_64.RSP, 144)]   // path on stack
            @ loadImm64 X86_64.RSI 0L                        // O_RDONLY
            @ loadImm64 X86_64.RDX 0L                        // mode
            @ loadImm64 X86_64.RAX openSyscall
            @ [X86_64.SYSCALL]
            // Check if open failed (RAX < 0)
            @ [X86_64.CMP_imm (X86_64.RAX, 0)
               X86_64.Jcc (X86_64.LT, errorLabel)]
            // Save fd in R8
            @ [X86_64.MOV_reg (X86_64.R8, X86_64.RAX)]
            // fstat(fd, stat_buf) → get file size
            @ [X86_64.MOV_reg (X86_64.RDI, X86_64.R8)]      // fd
            @ [X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)]      // stat buf at RSP
            @ loadImm64 X86_64.RAX fstatSyscall
            @ [X86_64.SYSCALL]
            // File size is at offset 48 in stat struct (x86_64 Linux)
            @ [X86_64.MOV_load (X86_64.R9, X86_64.RSP, 48)]  // R9 = file size
            // Allocate [refcount:8][length:8][data:N].
            @ [X86_64.MOV_reg (X86_64.R10, heapPtr)]          // R10 = string ptr
            @ [X86_64.MOV_reg (scratch, X86_64.R9)
               X86_64.ADD_imm (scratch, 24)                    // size + 24
               X86_64.ADD_reg (heapPtr, scratch)
               X86_64.ADD_imm (heapPtr, 7)
               X86_64.AND_imm (heapPtr, -8)]                  // align
            // Store fixed header
            @ loadImm64 X86_64.RCX 1L
            @ [X86_64.MOV_store (X86_64.R10, 0, X86_64.RCX)
               X86_64.MOV_store (X86_64.R10, 8, X86_64.R9)]
            // read(fd, buf, count)
            @ [X86_64.MOV_reg (X86_64.RDI, X86_64.R8)]        // fd
            @ [X86_64.LEA (X86_64.RSI, X86_64.R10, 16)]
            @ [X86_64.MOV_reg (X86_64.RDX, X86_64.R9)]        // count = file size
            @ loadImm64 X86_64.RAX readSyscall
            @ [X86_64.SYSCALL]
            // close(fd)
            @ [X86_64.MOV_reg (X86_64.RDI, X86_64.R8)]
            @ loadImm64 X86_64.RAX closeSyscall
            @ [X86_64.SYSCALL]
            // Allocate Result Ok: [tag=0:8][payload=string_ptr:8][refcount=1:8]
            @ [X86_64.MOV_reg (scratch, heapPtr)
               X86_64.ADD_imm (heapPtr, 24)]
            @ loadImm64 X86_64.RCX 0L
            @ [X86_64.MOV_store (scratch, 0, X86_64.RCX)       // tag = 0 (Ok)
               X86_64.MOV_store (scratch, 8, X86_64.R10)]      // payload = string ptr
            @ loadImm64 X86_64.RCX 1L
            @ [X86_64.MOV_store (scratch, 16, X86_64.RCX)      // refcount = 1
               X86_64.MOV_reg (X86_64.RAX, scratch)
               X86_64.JMP cleanupLabel]
            // === Error path ===
            @ [X86_64.Label errorLabel]
            // Allocate error string "File not found".
            @ [X86_64.MOV_reg (X86_64.R10, heapPtr)
               X86_64.ADD_imm (heapPtr, 32)]
            @ loadImm64 scratch 1L
            @ [X86_64.MOV_store (X86_64.R10, 0, scratch)]
            @ loadImm64 scratch 14L
            @ [X86_64.MOV_store (X86_64.R10, 8, scratch)]
            @ loadImm64 scratch 0x746F6E20656C6946L
            @ [X86_64.MOV_store (X86_64.R10, 16, scratch)]
            @ loadImm64 scratch 0x646E756F6620L
            @ [X86_64.MOV_store (X86_64.R10, 24, scratch)]
            // Allocate Result Error: [tag=1:8][payload=error_str:8][refcount=1:8]
            @ [X86_64.MOV_reg (scratch, heapPtr)
               X86_64.ADD_imm (heapPtr, 24)]
            @ loadImm64 X86_64.RCX 1L
            @ [X86_64.MOV_store (scratch, 0, X86_64.RCX)        // tag = 1 (Error)
               X86_64.MOV_store (scratch, 8, X86_64.R10)]       // payload = error string
            @ loadImm64 X86_64.RCX 1L
            @ [X86_64.MOV_store (scratch, 16, X86_64.RCX)       // refcount
               X86_64.MOV_reg (X86_64.RAX, scratch)]
            // === Cleanup ===
            @ [X86_64.Label cleanupLabel
               X86_64.ADD_imm (X86_64.RSP, 4240)]
            @ restores
            @ [X86_64.MOV_reg (destReg, X86_64.RAX)]))

let internal emitFileWriteBlob (ctx: FuncCtx) (instr: LIR.Instr) (dest: LIR.Reg) (path: LIR.Operand) (content: LIR.Operand) : Result<X86_64.Instr list, string> =
    // File write/append: open → write → close → Result
    let isAppend = match instr with LIR.FileAppendText _ -> true | _ -> false
    resolveReg dest
    |> Result.bind (fun destReg ->
        let resolvePathToR10 =
            match path with
            | LIR.Reg reg ->
                resolveReg reg |> Result.map (fun srcReg ->
                    if srcReg = X86_64.R10 then [] else [X86_64.MOV_reg (X86_64.R10, srcReg)])
            | LIR.StackSlot offset ->
                Ok [X86_64.MOV_load (X86_64.R10, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
            | LIR.StringSymbol value ->
                Ok (emitStringLiteralNoRefCount X86_64.R10 value)
            | _ ->
                Error "FileWriteBlob/FileAppendText path operand must be a string pointer or string literal"
        let resolveContentToR9 =
            match content with
            | LIR.Reg reg ->
                resolveReg reg |> Result.map (fun srcReg ->
                    if srcReg = X86_64.R9 then [] else [X86_64.MOV_reg (X86_64.R9, srcReg)])
            | LIR.StackSlot offset ->
                Ok [X86_64.MOV_load (X86_64.R9, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
            | LIR.StringSymbol value ->
                Ok (emitStringLiteralNoRefCount X86_64.R9 value)
            | _ ->
                Error "FileWriteBlob/FileAppendText content operand must be a string pointer or string literal"
        let copyLabel = freshLabel "fw_copy"
        let doneLabel = freshLabel "fw_done"
        let errorLabel = freshLabel "fw_err"
        let cleanupLabel = freshLabel "fw_clean"
        let openSyscall = int64 syscalls.Open
        let writeSyscall = int64 syscalls.Write
        let closeSyscall = int64 syscalls.Close
        // O_WRONLY|O_CREAT|O_TRUNC = 577 for write, O_WRONLY|O_CREAT|O_APPEND = 1089 for append
        let openFlags = if isAppend then 1089L else 577L
        resolvePathToR10 |> Result.bind (fun pathSetup ->
            resolveContentToR9 |> Result.map (fun contentSetup ->
                let saves = [X86_64.PUSH X86_64.RDI; X86_64.PUSH X86_64.RSI; X86_64.PUSH X86_64.RCX
                             X86_64.PUSH X86_64.R10; X86_64.PUSH X86_64.R8; X86_64.PUSH X86_64.R9]
                let restores = [X86_64.POP X86_64.R9; X86_64.POP X86_64.R8; X86_64.POP X86_64.R10
                                X86_64.POP X86_64.RCX; X86_64.POP X86_64.RSI; X86_64.POP X86_64.RDI]
                pathSetup @ contentSetup @ saves
                // Allocate 4096 bytes on stack for path (PATH_MAX)
                @ [X86_64.SUB_imm (X86_64.RSP, 4096)]
                // Copy path to null-terminated stack buffer
                @ [X86_64.MOV_load (X86_64.RCX, X86_64.R10, 8)
                   X86_64.LEA (X86_64.RSI, X86_64.R10, 16)
                   X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]
                @ loadImm64 X86_64.R10 0L
                @ [X86_64.Label copyLabel
                   X86_64.CMP_reg (X86_64.R10, X86_64.RCX)
                   X86_64.Jcc (X86_64.GE, doneLabel)
                   X86_64.MOV_reg (scratch, X86_64.RSI)
                   X86_64.ADD_reg (scratch, X86_64.R10)
                   X86_64.MOV_load_byte (scratch, scratch, 0)
                   X86_64.MOV_reg (X86_64.R8, X86_64.RDI)
                   X86_64.ADD_reg (X86_64.R8, X86_64.R10)
                   X86_64.MOV_store_byte (X86_64.R8, 0, scratch)
                   X86_64.ADD_imm (X86_64.R10, 1)
                   X86_64.JMP copyLabel
                   X86_64.Label doneLabel]
                // Null-terminate
                @ [X86_64.MOV_reg (scratch, X86_64.RDI)
                   X86_64.ADD_reg (scratch, X86_64.RCX)]
                @ loadImm64 X86_64.R10 0L
                @ [X86_64.MOV_store_byte (scratch, 0, X86_64.R10)]
                // open(path, flags, mode=0666)
                @ [X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]
                @ loadImm64 X86_64.RSI openFlags
                @ loadImm64 X86_64.RDX 0o666L
                @ loadImm64 X86_64.RAX openSyscall
                @ [X86_64.SYSCALL]
                @ [X86_64.CMP_imm (X86_64.RAX, 0)
                   X86_64.Jcc (X86_64.LT, errorLabel)]
                // Save fd in R8
                @ [X86_64.MOV_reg (X86_64.R8, X86_64.RAX)]
                // write(fd, content_data, content_len)
                // R9 = content heap string (saved by PUSH above, load from stack)
                // R9 was pushed at position 5 (index from top after SUB): need to recalculate
                // After pushes (6 * 8 = 48) + SUB 4096 = 4144 bytes below original RSP
                // R9 was the last push, so at [RSP + 4096 + 0] = [RSP + 4096]
                @ [X86_64.MOV_load (X86_64.R9, X86_64.RSP, 4096)]  // reload R9 (content)
                @ [X86_64.MOV_reg (X86_64.RDI, X86_64.R8)]        // fd
                @ [X86_64.LEA (X86_64.RSI, X86_64.R9, 16)]
                @ [X86_64.MOV_load (X86_64.RDX, X86_64.R9, 8)]
                @ loadImm64 X86_64.RAX writeSyscall
                @ [X86_64.SYSCALL]
                // close(fd)
                @ [X86_64.MOV_reg (X86_64.RDI, X86_64.R8)]
                @ loadImm64 X86_64.RAX closeSyscall
                @ [X86_64.SYSCALL]
                // Allocate Result Ok: [tag=0:8][payload=0:8][refcount=1:8]
                @ [X86_64.MOV_reg (scratch, heapPtr)
                   X86_64.ADD_imm (heapPtr, 24)]
                @ loadImm64 X86_64.RCX 0L
                @ [X86_64.MOV_store (scratch, 0, X86_64.RCX)       // tag = 0 (Ok)
                   X86_64.MOV_store (scratch, 8, X86_64.RCX)]      // payload = 0 (Unit)
                @ loadImm64 X86_64.RCX 1L
                @ [X86_64.MOV_store (scratch, 16, X86_64.RCX)      // refcount
                   X86_64.MOV_reg (X86_64.RAX, scratch)
                   X86_64.JMP cleanupLabel]
                // === Error path ===
                @ [X86_64.Label errorLabel]
                @ [X86_64.MOV_reg (X86_64.R10, heapPtr)
                   X86_64.ADD_imm (heapPtr, 24)]
                @ loadImm64 scratch 1L
                @ [X86_64.MOV_store (X86_64.R10, 0, scratch)]
                @ loadImm64 scratch 5L
                @ [X86_64.MOV_store (X86_64.R10, 8, scratch)]
                @ loadImm64 scratch 0x726F727245L                    // "Error"
                @ [X86_64.MOV_store (X86_64.R10, 16, scratch)]
                @ [X86_64.MOV_reg (scratch, heapPtr)
                   X86_64.ADD_imm (heapPtr, 24)]
                @ loadImm64 X86_64.RCX 1L
                @ [X86_64.MOV_store (scratch, 0, X86_64.RCX)
                   X86_64.MOV_store (scratch, 8, X86_64.R10)]
                @ loadImm64 X86_64.RCX 1L
                @ [X86_64.MOV_store (scratch, 16, X86_64.RCX)
                   X86_64.MOV_reg (X86_64.RAX, scratch)]
                // === Cleanup ===
                @ [X86_64.Label cleanupLabel
                   X86_64.ADD_imm (X86_64.RSP, 4096)]
                @ restores
                @ [X86_64.MOV_reg (destReg, X86_64.RAX)])))

let internal emitFileExists (ctx: FuncCtx) (dest: LIR.Reg) (path: LIR.Operand) : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.bind (fun destReg ->
        let resolvePathToR10 =
            match path with
            | LIR.Reg reg ->
                resolveReg reg |> Result.map (fun srcReg ->
                    if srcReg = X86_64.R10 then [] else [X86_64.MOV_reg (X86_64.R10, srcReg)])
            | LIR.StackSlot offset ->
                Ok [X86_64.MOV_load (X86_64.R10, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
            | LIR.StringSymbol value ->
                Ok (emitStringLiteralNoRefCount X86_64.R10 value)
            | _ -> Ok (loadImm64 X86_64.R10 0L)
        let copyLabel = freshLabel "fe_copy"
        let doneLabel = freshLabel "fe_done"
        resolvePathToR10 |> Result.map (fun pathSetup ->
            let accessSyscall = int64 syscalls.Access
            // Save clobbered registers (access syscall uses RDI, RSI, RAX + copy uses RCX, R10)
            let saves = [X86_64.PUSH X86_64.RDI; X86_64.PUSH X86_64.RSI; X86_64.PUSH X86_64.RCX; X86_64.PUSH X86_64.R10]
            let restores = [X86_64.POP X86_64.R10; X86_64.POP X86_64.RCX; X86_64.POP X86_64.RSI; X86_64.POP X86_64.RDI]
            pathSetup @ saves
            // Allocate 4096 bytes on stack for null-terminated path (PATH_MAX)
            @ [X86_64.SUB_imm (X86_64.RSP, 4096)]
            // R10 points to [refcount][length][data].
            // RSI = string data addr, RDI = stack buf, RCX = length, R11 = counter
            @ [X86_64.MOV_load (X86_64.RCX, X86_64.R10, 8)
               X86_64.LEA (X86_64.RSI, X86_64.R10, 16)
               X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]       // RDI = stack buf
            // Copy loop using R11 (scratch) as counter
            @ loadImm64 X86_64.R10 0L  // R10 = counter (reuse R10 since string ptr no longer needed)
            @ [X86_64.Label copyLabel
               X86_64.CMP_reg (X86_64.R10, X86_64.RCX)
               X86_64.Jcc (X86_64.GE, doneLabel)
               // scratch = [RSI + R10]
               X86_64.MOV_reg (scratch, X86_64.RSI)
               X86_64.ADD_reg (scratch, X86_64.R10)
               X86_64.MOV_load_byte (scratch, scratch, 0)
               // [RDI + R10] = byte
               X86_64.PUSH X86_64.R8
               X86_64.MOV_reg (X86_64.R8, X86_64.RDI)
               X86_64.ADD_reg (X86_64.R8, X86_64.R10)
               X86_64.MOV_store_byte (X86_64.R8, 0, scratch)
               X86_64.POP X86_64.R8
               X86_64.ADD_imm (X86_64.R10, 1)
               X86_64.JMP copyLabel
               X86_64.Label doneLabel]
            // Null-terminate: [RDI + RCX] = 0
            @ [X86_64.MOV_reg (scratch, X86_64.RDI)
               X86_64.ADD_reg (scratch, X86_64.RCX)]
            @ loadImm64 X86_64.R10 0L
            @ [X86_64.MOV_store_byte (scratch, 0, X86_64.R10)]
            // syscall: access(path=RSP, mode=F_OK=0)
            @ [X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]
            @ loadImm64 X86_64.RSI 0L
            @ loadImm64 X86_64.RAX accessSyscall
            @ [X86_64.SYSCALL
               // RAX = 0 if exists, negative otherwise
               // Convert to boolean in R10 (safe temp, will be popped later but unused)
               X86_64.CMP_imm (X86_64.RAX, 0)
               X86_64.SETcc (X86_64.EQ, X86_64.RAX)
               X86_64.MOVZX_byte (X86_64.RAX, X86_64.RAX)
               X86_64.ADD_imm (X86_64.RSP, 4096)]
            @ restores
            // Move result to destReg after restoring saved registers
            @ [X86_64.MOV_reg (destReg, X86_64.RAX)]))

let private emitPathUnitOperation
    (ctx: FuncCtx)
    (dest: LIR.Reg)
    (path: LIR.Operand)
    (createDirectory: bool)
    : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.bind (fun destReg ->
        let pathSetup =
            match path with
            | LIR.Reg reg ->
                resolveReg reg
                |> Result.map (fun source ->
                    if source = X86_64.R10 then [] else [X86_64.MOV_reg (X86_64.R10, source)])
            | LIR.StackSlot offset ->
                Ok [X86_64.MOV_load (X86_64.R10, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
            | LIR.StringSymbol value -> Ok (emitStringLiteralNoRefCount X86_64.R10 value)
            | _ -> Error "FileDelete path operand must be a string pointer or string literal"
        let copyLabel = freshLabel "fd_copy"
        let copyDoneLabel = freshLabel "fd_copy_done"
        let errorLabel = freshLabel "fd_error"
        let cleanupLabel = freshLabel "fd_cleanup"
        pathSetup
        |> Result.map (fun setup ->
            setup
            @ [ X86_64.PUSH X86_64.RDI
                X86_64.PUSH X86_64.RSI
                X86_64.PUSH X86_64.RCX
                X86_64.PUSH X86_64.R10
                X86_64.SUB_imm (X86_64.RSP, 4096)
                X86_64.MOV_load (X86_64.RCX, X86_64.R10, 8)
                X86_64.LEA (X86_64.RSI, X86_64.R10, 16)
                X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)
                X86_64.XOR_reg (X86_64.R10, X86_64.R10)
                X86_64.Label copyLabel
                X86_64.CMP_reg (X86_64.R10, X86_64.RCX)
                X86_64.Jcc (X86_64.GE, copyDoneLabel)
                X86_64.MOV_reg (scratch, X86_64.RSI)
                X86_64.ADD_reg (scratch, X86_64.R10)
                X86_64.MOV_load_byte (scratch, scratch, 0)
                X86_64.MOV_reg (X86_64.RAX, X86_64.RDI)
                X86_64.ADD_reg (X86_64.RAX, X86_64.R10)
                X86_64.MOV_store_byte (X86_64.RAX, 0, scratch)
                X86_64.ADD_imm (X86_64.R10, 1)
                X86_64.JMP copyLabel
                X86_64.Label copyDoneLabel
                X86_64.MOV_reg (scratch, X86_64.RDI)
                X86_64.ADD_reg (scratch, X86_64.RCX)
                X86_64.XOR_reg (X86_64.R10, X86_64.R10)
                X86_64.MOV_store_byte (scratch, 0, X86_64.R10)
                X86_64.MOV_reg (X86_64.RDI, X86_64.RSP) ]
            @ (if createDirectory then loadImm64 X86_64.RSI 0o777L else [])
            @ loadImm64 X86_64.RAX (if createDirectory then 83L else int64 syscalls.Unlink)
            @ [ X86_64.SYSCALL
                X86_64.CMP_imm (X86_64.RAX, 0)
                X86_64.Jcc (X86_64.LT, errorLabel)
                X86_64.MOV_reg (X86_64.RAX, heapPtr)
                X86_64.ADD_imm (heapPtr, 24)
                X86_64.XOR_reg (X86_64.RCX, X86_64.RCX)
                X86_64.MOV_store (X86_64.RAX, 0, X86_64.RCX)
                X86_64.MOV_store (X86_64.RAX, 8, X86_64.RCX)
                X86_64.MOV_imm32 (X86_64.RCX, 1)
                X86_64.MOV_store (X86_64.RAX, 16, X86_64.RCX) ]
            @ genLeakCounterInc ctx
            @ [ X86_64.JMP cleanupLabel
                X86_64.Label errorLabel
                X86_64.MOV_reg (X86_64.R10, heapPtr)
                X86_64.ADD_imm (heapPtr, 24) ]
            @ loadImm64 X86_64.RCX 1L
            @ [X86_64.MOV_store (X86_64.R10, 0, X86_64.RCX)]
            @ loadImm64 X86_64.RCX 5L
            @ [X86_64.MOV_store (X86_64.R10, 8, X86_64.RCX)]
            @ loadImm64 X86_64.RCX 0x726F727245L
            @ [ X86_64.MOV_store (X86_64.R10, 16, X86_64.RCX)
                X86_64.MOV_reg (X86_64.RAX, heapPtr)
                X86_64.ADD_imm (heapPtr, 24) ]
            @ loadImm64 X86_64.RCX 1L
            @ [ X86_64.MOV_store (X86_64.RAX, 0, X86_64.RCX)
                X86_64.MOV_store (X86_64.RAX, 8, X86_64.R10)
                X86_64.MOV_store (X86_64.RAX, 16, X86_64.RCX) ]
            @ genLeakCounterInc ctx
            @ genLeakCounterInc ctx
            @ [ X86_64.Label cleanupLabel
                X86_64.ADD_imm (X86_64.RSP, 4096)
                X86_64.POP X86_64.R10
                X86_64.POP X86_64.RCX
                X86_64.POP X86_64.RSI
                X86_64.POP X86_64.RDI
                X86_64.MOV_reg (destReg, X86_64.RAX) ]))

let internal emitFileDelete (ctx: FuncCtx) (dest: LIR.Reg) (path: LIR.Operand) : Result<X86_64.Instr list, string> =
    emitPathUnitOperation ctx dest path false

let internal emitFileCreateDirectory (ctx: FuncCtx) (dest: LIR.Reg) (path: LIR.Operand) : Result<X86_64.Instr list, string> =
    emitPathUnitOperation ctx dest path true

let internal emitFileSetExecutable (ctx: FuncCtx) (dest: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.map (fun destReg -> loadImm64 destReg 0L)

let internal emitFileWriteFromPtr (ctx: FuncCtx) (dest: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.map (fun destReg -> loadImm64 destReg 0L)
