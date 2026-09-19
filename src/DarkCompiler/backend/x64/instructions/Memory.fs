// Memory.fs - Emit x64 instructions for memory operations.

module X64EmitMemory

open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection
open X64FieldReferenceCounts
open X64ListReferenceCounts
open X64InstructionContext

let internal emitHeapAlloc (ctx: FuncCtx) (dest: LIR.Reg) (sizeBytes: int) : Result<X86_64.Instr list, string> =
    // Bump allocator with free list reuse + bounds check
    let okLabel = freshLabel "heap_ok"
    resolveReg dest
    |> Result.map (fun destReg ->
        let totalSize = ((sizeBytes + 8) + 7) &&& (~~~7)
        let storeInitialRefcount =
            if destReg = X86_64.RCX then
                [X86_64.PUSH scratch
                 X86_64.MOV_imm32 (scratch, 1)
                 X86_64.MOV_store (destReg, sizeBytes, scratch)
                 X86_64.POP scratch]
            else
                [X86_64.PUSH X86_64.RCX
                 X86_64.MOV_imm32 (X86_64.RCX, 1)
                 X86_64.MOV_store (destReg, sizeBytes, X86_64.RCX)
                 X86_64.POP X86_64.RCX]
        // Check free list for this size class (if valid)
        let freeListAlloc =
            if sizeBytes >= 0 && sizeBytes < freeListSize then
                let bumpLabel = freshLabel "heap_bump"
                let freeListDoneLabel = freshLabel "heap_fl_done"
                let freeListHeadReg =
                    if destReg = X86_64.RCX then scratch else X86_64.RCX
                let preserveHeadReg =
                    if destReg = X86_64.RCX then [] else [X86_64.PUSH X86_64.RCX]
                let restoreHeadReg =
                    if destReg = X86_64.RCX then [] else [X86_64.POP X86_64.RCX]
                let freeListPre =
                    preserveHeadReg
                    @ [
                        X86_64.MOV_load (freeListHeadReg, freeListBase, sizeBytes)
                        X86_64.TEST_reg (freeListHeadReg, freeListHeadReg)
                        X86_64.Jcc (X86_64.EQ, bumpLabel)
                        // Free list hit: dest = block, update head to next
                        X86_64.MOV_reg (destReg, freeListHeadReg)
                        X86_64.MOV_load (freeListHeadReg, freeListHeadReg, 0) // next ptr
                        X86_64.MOV_store (freeListBase, sizeBytes, freeListHeadReg)
                      ]
                    @ restoreHeadReg
                    @ storeInitialRefcount
                    @ [X86_64.JMP freeListDoneLabel
                       X86_64.Label bumpLabel]
                    @ restoreHeadReg
                freeListPre, [X86_64.Label freeListDoneLabel]
            else [], []
        let (freeListPre, freeListPost) = freeListAlloc
        freeListPre
        // Bump allocator path
        @ [X86_64.MOV_reg (destReg, heapPtr)
           X86_64.ADD_imm (heapPtr, int32 totalSize)]
        @ storeInitialRefcount
        // Bounds check
        @ (if destReg = scratch then
            [X86_64.PUSH X86_64.RAX
             X86_64.MOV_reg (X86_64.RAX, heapPtr)
             X86_64.SUB_reg (X86_64.RAX, freeListBase)
             X86_64.CMP_imm (X86_64.RAX, int32 heapMmapSizeBytes)
             X86_64.POP X86_64.RAX
             X86_64.Jcc (X86_64.LE, okLabel)]
           else
            [X86_64.MOV_reg (scratch, heapPtr)
             X86_64.SUB_reg (scratch, freeListBase)
             X86_64.CMP_imm (scratch, int32 heapMmapSizeBytes)
             X86_64.Jcc (X86_64.LE, okLabel)])
        @ genOomJump ()
        @ [X86_64.Label okLabel]
        // A free-list hit jumps to freeListPost, so place the join before
        // leak accounting. Both allocation paths create one live root.
        @ freeListPost
        @ genLeakCounterInc ctx)

let internal emitHeapStore (ctx: FuncCtx) (addr: LIR.Reg) (offset: int) (src: LIR.Operand) : Result<X86_64.Instr list, string> =
    resolveReg addr
    |> Result.bind (fun addrReg ->
        match src with
        | LIR.Imm value ->
            if addrReg = scratch then
                // Address is R11 - can't use scratch for the immediate value
                Ok ([X86_64.PUSH X86_64.RCX]
                    @ loadImm64 X86_64.RCX value
                    @ [X86_64.MOV_store (addrReg, int32 offset, X86_64.RCX)
                       X86_64.POP X86_64.RCX])
            else
                Ok (loadImm64 scratch value @ [X86_64.MOV_store (addrReg, int32 offset, scratch)])
        | LIR.Reg srcReg ->
            resolveReg srcReg
            |> Result.map (fun srcX86 ->
                if addrReg = scratch && srcX86 = scratch then
                    // Both addr and src are R11 - store R11 at [R11 + offset]
                    [X86_64.MOV_store (scratch, int32 offset, scratch)]
                else if addrReg = scratch then
                    [X86_64.MOV_store (scratch, int32 offset, srcX86)]
                else
                    [X86_64.MOV_store (addrReg, int32 offset, srcX86)])
        | LIR.FuncAddr funcName ->
            if addrReg = scratch then
                // Address is R11 - use RCX to hold the function address
                Ok [X86_64.PUSH X86_64.RCX
                    X86_64.LEA_rip (X86_64.RCX, functionName ctx funcName)
                    X86_64.MOV_store (addrReg, int32 offset, X86_64.RCX)
                    X86_64.POP X86_64.RCX]
            else
                Ok [X86_64.LEA_rip (scratch, functionName ctx funcName)
                    X86_64.MOV_store (addrReg, int32 offset, scratch)]
        | LIR.FloatSymbol value ->
            // Store float bits as 8-byte integer value at the heap offset
            let bits = System.BitConverter.DoubleToInt64Bits(value)
            if addrReg = scratch then
                // Address is R11 - can't use scratch for the immediate value
                Ok ([X86_64.PUSH X86_64.RCX]
                    @ loadImm64 X86_64.RCX bits
                    @ [X86_64.MOV_store (addrReg, int32 offset, X86_64.RCX)
                       X86_64.POP X86_64.RCX])
            else
                Ok (loadImm64 scratch bits @ [X86_64.MOV_store (addrReg, int32 offset, scratch)])
        | LIR.StringSymbol value ->
            if addrReg = scratch then
                // Preserve both the record address in R11 and an unrelated
                // live X3 value while R11 receives the literal address.
                Ok ([X86_64.PUSH X86_64.RCX
                     X86_64.PUSH scratch]
                    @ emitStringLiteral scratch value
                    @ [X86_64.POP X86_64.RCX
                       X86_64.MOV_store (X86_64.RCX, int32 offset, scratch)
                       X86_64.POP X86_64.RCX])
            else
                Ok (emitStringLiteral scratch value
                    @ [X86_64.MOV_store (addrReg, int32 offset, scratch)])
        | LIR.StackSlot stackOffset ->
            let adjOff = adjustStackOffset ctx stackOffset
            if addrReg = scratch then
                Ok ([X86_64.PUSH X86_64.RCX
                     X86_64.MOV_load (X86_64.RCX, X86_64.RBP, int32 adjOff)
                     X86_64.MOV_store (addrReg, int32 offset, X86_64.RCX)
                     X86_64.POP X86_64.RCX])
            else
                Ok [X86_64.MOV_load (scratch, X86_64.RBP, int32 adjOff)
                    X86_64.MOV_store (addrReg, int32 offset, scratch)]
        | _ -> Error $"Unsupported HeapStore source: {src}")

let internal emitHeapLoad (ctx: FuncCtx) (dest: LIR.Reg) (addr: LIR.Reg) (offset: int) : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.bind (fun destReg ->
        resolveReg addr
        |> Result.map (fun addrReg ->
            [X86_64.MOV_load (destReg, addrReg, int32 offset)]))

// --- Floating-point operations ---

let internal emitMappedAlloc (ctx: FuncCtx) (dest: LIR.Reg) (numBytes: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun destReg ->
        resolveReg numBytes |> Result.map (fun sizeReg ->
            // Syscall-clobbered registers are protected by LIR caller saves.
            // Keep the exact mapping length in a private allocation prefix.
            [ X86_64.CMP_imm (sizeReg, 0)
              X86_64.Jcc (X86_64.LT, oomHandlerLabel)
              X86_64.MOV_reg (X86_64.RSI, sizeReg)
              X86_64.ADD_imm (X86_64.RSI, 8)
              X86_64.CMP_imm (X86_64.RSI, 0)
              X86_64.Jcc (X86_64.LE, oomHandlerLabel)
              X86_64.PUSH X86_64.RSI ]
            @ loadImm64 X86_64.RDI 0L
            @ loadImm64 X86_64.RDX 3L
            @ loadImm64 X86_64.R10 0x22L
            @ loadImm64 X86_64.R8 -1L
            @ loadImm64 X86_64.R9 0L
            @ loadImm64 X86_64.RAX (int64 syscalls.Mmap)
            @ [ X86_64.SYSCALL
                X86_64.CMP_imm (X86_64.RAX, 0)
                X86_64.Jcc (X86_64.LT, oomHandlerLabel)
                X86_64.POP scratch
                X86_64.MOV_store (X86_64.RAX, 0, scratch)
                X86_64.LEA (destReg, X86_64.RAX, 8) ]
            @ genLeakCounterInc ctx))

let internal emitMappedFree (ctx: FuncCtx) (ptr: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg ptr |> Result.map (fun ptrReg ->
        [ X86_64.LEA (X86_64.RDI, ptrReg, -8)
          X86_64.MOV_load (X86_64.RSI, X86_64.RDI, 0) ]
        @ loadImm64 X86_64.RAX (int64 syscalls.Munmap)
        @ [ X86_64.SYSCALL
            X86_64.CMP_imm (X86_64.RAX, 0)
            X86_64.Jcc (X86_64.NE, oomHandlerLabel) ]
        @ genLeakCounterDec ctx)

let internal emitRawAlloc (ctx: FuncCtx) (dest: LIR.Reg) (numBytes: LIR.Reg) : Result<X86_64.Instr list, string> =
    let okLabel = freshLabel "rawalloc_ok"
    resolveReg dest
    |> Result.bind (fun destReg ->
        resolveReg numBytes
        |> Result.map (fun sizeReg ->
            // --- Free list reuse ---
            // Before bump-allocating, check if the free list has a block of the right size class.
            // aligned_size = (numBytes + 7) & ~7; payload_class = aligned_size - 8
            // If freeList[payload_class] is non-null, pop from free list and skip bump alloc.
            let bumpLabel = freshLabel "rawalloc_bump"
            let doneLabel = freshLabel "rawalloc_done"
            // Pick two temp registers that don't conflict with destReg or sizeReg
            let candidates = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI]
            let available = candidates |> List.filter (fun r -> r <> destReg && r <> sizeReg)
            let temp1 = available.[0]  // holds aligned_size → payload_class → head → next
            let temp2 = available.[1]  // holds &freeList[payload_class]
            let freeListCheck =
                [X86_64.PUSH temp1
                 X86_64.PUSH temp2
                 // Compute aligned size
                 X86_64.MOV_reg (temp1, sizeReg)
                 X86_64.ADD_imm (temp1, 7)
                 X86_64.AND_imm (temp1, -8)         // temp1 = aligned_size
                 // Need at least 16 bytes (8 payload + 8 refcount) for free list
                 X86_64.CMP_imm (temp1, 8)
                 X86_64.Jcc (X86_64.LT, bumpLabel)
                 X86_64.SUB_imm (temp1, 8)           // temp1 = payload_class
                 X86_64.CMP_imm (temp1, maxFreeListPayload)
                 X86_64.Jcc (X86_64.GT, bumpLabel)
                 // Compute free list slot address: &freeList[payload_class]
                 X86_64.MOV_reg (temp2, freeListBase)
                 X86_64.ADD_reg (temp2, temp1)       // temp2 = &freeList[payload_class]
                 // Load free list head
                 X86_64.MOV_load (temp1, temp2, 0)   // temp1 = head
                 X86_64.TEST_reg (temp1, temp1)
                 X86_64.Jcc (X86_64.EQ, bumpLabel)
                 // Pop from free list: dest = head, freeList[class] = head->next
                 X86_64.MOV_reg (destReg, temp1)     // dest = free block
                 X86_64.MOV_load (temp1, temp1, 0)   // temp1 = next ptr
                 X86_64.MOV_store (temp2, 0, temp1)  // update head
                 X86_64.POP temp2
                 X86_64.POP temp1
                 X86_64.JMP doneLabel
                 X86_64.Label bumpLabel
                 X86_64.POP temp2
                 X86_64.POP temp1]
            // --- Bump allocation (existing) ---
            let allocInstrs =
                if destReg = sizeReg then
                    [X86_64.MOV_reg (scratch, sizeReg)
                     X86_64.MOV_reg (destReg, heapPtr)
                     X86_64.ADD_reg (heapPtr, scratch)
                     X86_64.ADD_imm (heapPtr, 7)
                     X86_64.AND_imm (heapPtr, -8)]
                else
                    [X86_64.MOV_reg (destReg, heapPtr)
                     X86_64.ADD_reg (heapPtr, sizeReg)
                     X86_64.ADD_imm (heapPtr, 7)
                     X86_64.AND_imm (heapPtr, -8)]
            // Bounds check: heapPtr - freeListBase <= heapMmapSize
            // Use RAX temp if dest or size uses scratch (R11)
            let useScratch = destReg <> scratch && sizeReg <> scratch
            let boundsCheck =
                if useScratch then
                    [X86_64.MOV_reg (scratch, heapPtr)
                     X86_64.SUB_reg (scratch, freeListBase)
                     X86_64.CMP_imm (scratch, int32 heapMmapSizeBytes)
                     X86_64.Jcc (X86_64.LE, okLabel)]
                else
                    [X86_64.PUSH X86_64.RAX
                     X86_64.MOV_reg (X86_64.RAX, heapPtr)
                     X86_64.SUB_reg (X86_64.RAX, freeListBase)
                     X86_64.CMP_imm (X86_64.RAX, int32 heapMmapSizeBytes)
                     X86_64.POP X86_64.RAX
                     X86_64.Jcc (X86_64.LE, okLabel)]
                @ genOomJump ()
                @ [X86_64.Label okLabel]
            // Recycled blocks become live allocations just like bumped
            // blocks. Both paths must reach the accounting increment.
            freeListCheck @ allocInstrs @ boundsCheck @ [X86_64.Label doneLabel] @ genLeakCounterInc ctx))

let internal emitRawFree (ctx: FuncCtx) (ptr: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg ptr
    |> Result.map (fun ptrReg ->
        [X86_64.MOV_load (scratch, freeListBase, 0)
         X86_64.MOV_store (ptrReg, 0, scratch)
         X86_64.MOV_store (freeListBase, 0, ptrReg)]
        @ genLeakCounterDec ctx)

let internal emitRawGet (ctx: FuncCtx) (dest: LIR.Reg) (ptr: LIR.Reg) (byteOffset: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d ->
        resolveReg ptr |> Result.bind (fun p ->
            resolveReg byteOffset |> Result.map (fun o ->
                if o = scratch && p <> scratch then
                    // Offset is R11: MOV scratch,p would clobber offset.
                    // Swap: compute p + o by loading o first, adding p.
                    [X86_64.ADD_reg (scratch, p)
                     X86_64.MOV_load (d, scratch, 0)]
                elif p = scratch && o <> scratch then
                    // Ptr is R11: MOV is no-op, just add offset
                    [X86_64.ADD_reg (scratch, o)
                     X86_64.MOV_load (d, scratch, 0)]
                elif p = scratch && o = scratch then
                    // Both are R11 (same virtual reg): scratch = scratch + scratch
                    [X86_64.ADD_reg (scratch, scratch)
                     X86_64.MOV_load (d, scratch, 0)]
                else
                    [X86_64.MOV_reg (scratch, p)
                     X86_64.ADD_reg (scratch, o)
                     X86_64.MOV_load (d, scratch, 0)])))

let internal emitRawGetByte (ctx: FuncCtx) (dest: LIR.Reg) (ptr: LIR.Reg) (byteOffset: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d ->
        resolveReg ptr |> Result.bind (fun p ->
            resolveReg byteOffset |> Result.map (fun o ->
                if o = scratch && p <> scratch then
                    [X86_64.ADD_reg (scratch, p)
                     X86_64.MOV_load_byte (d, scratch, 0)]
                elif p = scratch then
                    [X86_64.ADD_reg (scratch, o)
                     X86_64.MOV_load_byte (d, scratch, 0)]
                else
                    [X86_64.MOV_reg (scratch, p)
                     X86_64.ADD_reg (scratch, o)
                     X86_64.MOV_load_byte (d, scratch, 0)])))

let internal emitRawWriteWord (ctx: FuncCtx) (ptr: LIR.Reg) (byteOffset: LIR.Reg) (value: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg ptr |> Result.bind (fun p ->
        resolveReg byteOffset |> Result.bind (fun o ->
            resolveReg value |> Result.map (fun v ->
                if v = scratch || o = scratch then
                    let tempReg = X86_64.RCX
                    if p = tempReg then
                        [X86_64.PUSH tempReg
                         X86_64.PUSH scratch
                         X86_64.MOV_load (scratch, X86_64.RSP, 8)
                         X86_64.ADD_reg (scratch, o)
                         X86_64.MOV_load (tempReg, X86_64.RSP, 0)
                         X86_64.MOV_store (scratch, 0, tempReg)
                         X86_64.POP scratch
                         X86_64.POP tempReg]
                    else
                        [X86_64.PUSH tempReg
                         X86_64.MOV_reg (tempReg, v)
                         X86_64.MOV_reg (scratch, p)
                         X86_64.ADD_reg (scratch, o)
                         X86_64.MOV_store (scratch, 0, tempReg)
                         X86_64.POP tempReg]
                else
                    [X86_64.MOV_reg (scratch, p)
                     X86_64.ADD_reg (scratch, o)
                     X86_64.MOV_store (scratch, 0, v)])))

let internal emitRawSlotInit (ctx: FuncCtx) (ptr: LIR.Reg) (byteOffset: LIR.Reg) (value: LIR.Reg) (valueType: AST.Type) : Result<X86_64.Instr list, string> =
    resolveReg ptr |> Result.bind (fun p ->
        resolveReg byteOffset |> Result.bind (fun o ->
            resolveReg value |> Result.map (fun v ->
                // Raw runtime nodes own every managed value stored in an edge slot.
                // RawGet is borrowed, so RawSlotInit is the retain point for copied edges.
                let ownershipInc : X86_64.Instr list =
                    match slotInitRootRetainTarget ctx.RecordRegistry ctx.SumShapeRegistry valueType with
                    | Some SlotInitListRootRetain ->
                        let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.R10]
                        let saves = saveRegs |> List.map X86_64.PUSH
                        let restores = saveRegs |> List.rev |> List.map X86_64.POP
                        saves
                        @ [X86_64.MOV_reg (X86_64.RAX, v); X86_64.CALL listRefCountIncHelperLabel]
                        @ restores
                    | Some SlotInitDictRootRetain ->
                        let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI; X86_64.R8; X86_64.R9; X86_64.R10]
                        let saves = saveRegs |> List.map X86_64.PUSH
                        let restores = saveRegs |> List.rev |> List.map X86_64.POP
                        saves
                        @ [X86_64.MOV_reg (X86_64.RAX, v); X86_64.CALL dictRefCountIncHelperLabel]
                        @ restores
                    | Some SlotInitDynamicBufferRetain ->
                        let literalLabel = freshLabel "slotinit_rcinc_dynamic_lit"
                        [X86_64.PUSH X86_64.RCX
                         X86_64.PUSH X86_64.RDX
                         X86_64.PUSH X86_64.R10
                         X86_64.PUSH scratch
                         X86_64.MOV_reg (X86_64.R10, v)
                         X86_64.MOV_load (X86_64.RDX, X86_64.R10, 0)]
                        @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL
                        @ [X86_64.CMP_reg (X86_64.RDX, scratch)
                           X86_64.Jcc (X86_64.EQ, literalLabel)
                           X86_64.ADD_imm (X86_64.RDX, 1)
                           X86_64.MOV_store (X86_64.R10, 0, X86_64.RDX)
                           X86_64.Label literalLabel
                           X86_64.POP scratch
                           X86_64.POP X86_64.R10
                           X86_64.POP X86_64.RDX
                           X86_64.POP X86_64.RCX]
                    | Some SlotInitClosureRootRetain ->
                        let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.R10]
                        let saves = saveRegs |> List.map X86_64.PUSH
                        let restores = saveRegs |> List.rev |> List.map X86_64.POP
                        saves
                        @ [X86_64.MOV_reg (X86_64.RAX, v); X86_64.CALL closureRefCountIncHelperLabel]
                        @ restores
                    | Some (SlotInitGenericRootRetain payloadSize) ->
                        genRefCountIncGeneric v payloadSize
                    | _ ->
                        []

                let storeInstrs =
                    if v = scratch || o = scratch then
                        // An operand is R11 (scratch) which we need for address computation.
                        // Use RCX as an extra temp (save/restore if needed).
                        let tempReg = X86_64.RCX
                        if p = tempReg then
                            // ptr is RCX: can't use RCX as temp without saving ptr first.
                            // Use two pushes: save ptr, save value, compute address, store.
                            [X86_64.PUSH tempReg             // save ptr (RCX)
                             X86_64.PUSH scratch              // save value/offset (R11)
                             // Stack: [R11] [RCX] ...
                             // Compute address: R11 = ptr + offset
                             X86_64.MOV_load (scratch, X86_64.RSP, 8) // R11 = saved ptr (RCX)
                             X86_64.ADD_reg (scratch, o)      // R11 = ptr + offset
                             // Get value
                             X86_64.MOV_load (tempReg, X86_64.RSP, 0) // RCX = saved R11 (value)
                             X86_64.MOV_store (scratch, 0, tempReg) // [addr] = value
                             X86_64.POP scratch               // restore R11
                             X86_64.POP tempReg]              // restore RCX
                        else
                            [X86_64.PUSH tempReg
                             X86_64.MOV_reg (tempReg, v)   // save value in temp
                             X86_64.MOV_reg (scratch, p)
                             X86_64.ADD_reg (scratch, o)
                             X86_64.MOV_store (scratch, 0, tempReg)
                             X86_64.POP tempReg]
                    else
                        [X86_64.MOV_reg (scratch, p)
                         X86_64.ADD_reg (scratch, o)
                         X86_64.MOV_store (scratch, 0, v)]

                // Retain before address calculation: storeInstrs uses R11 as
                // its address scratch, which may also carry the value.
                ownershipInc @ storeInstrs)))

let internal emitRawWriteByte (ctx: FuncCtx) (ptr: LIR.Reg) (byteOffset: LIR.Reg) (value: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg ptr |> Result.bind (fun p ->
        resolveReg byteOffset |> Result.bind (fun o ->
            resolveReg value |> Result.map (fun v ->
                if v = scratch || o = scratch then
                    let tempReg = X86_64.RCX
                    if p = tempReg then
                        // ptr is RCX: save both before clobbering
                        [X86_64.PUSH tempReg
                         X86_64.PUSH scratch
                         X86_64.MOV_load (scratch, X86_64.RSP, 8)  // R11 = saved ptr
                         X86_64.ADD_reg (scratch, o)
                         X86_64.MOV_load (tempReg, X86_64.RSP, 0)  // RCX = saved value/offset
                         X86_64.MOV_store_byte (scratch, 0, tempReg)
                         X86_64.POP scratch
                         X86_64.POP tempReg]
                    else
                        [X86_64.PUSH tempReg
                         X86_64.MOV_reg (tempReg, v)
                         X86_64.MOV_reg (scratch, p)
                         X86_64.ADD_reg (scratch, o)
                         X86_64.MOV_store_byte (scratch, 0, tempReg)
                         X86_64.POP tempReg]
                else
                    [X86_64.MOV_reg (scratch, p)
                     X86_64.ADD_reg (scratch, o)
                     X86_64.MOV_store_byte (scratch, 0, v)])))
