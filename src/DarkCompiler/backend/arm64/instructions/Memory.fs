// Memory.fs - Emit arm64 instructions for memory operations.

module ARM64EmitMemory

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64LeakAccounting
open ARM64Operands

let internal emitHeapAlloc (ctx: CodeGenContext) (dest: LIR.Reg) (sizeBytes: int) : Result<ARM64Symbolic.Instr list, string> =
    // Heap allocator with free list support
    // X27 = free list heads base, X28 = bump allocator pointer
    //
    // Memory layout with reference counting:
    //   [payload: sizeBytes][refcount: 8 bytes]
    //
    // Algorithm:
    // 1. Check free list for this size class (sizeClassOffset = sizeBytes)
    // 2. If free list non-empty: pop from list, initialize refcount, return
    // 3. If empty: bump allocate from X28
    //
    // Code structure (10 instructions):
    //   LDR X15, [X27, sizeBytes]         ; Load free list head
    //   CBZ X15, +5                       ; If empty, skip to bump alloc (5 instrs)
    //   MOV dest, X15                     ; dest = freed block
    //   LDR X14, [X15, 0]                 ; Load next pointer from freed block
    //   STR X14, [X27, sizeBytes]         ; Update free list head
    //   MOVZ X14, 1                       ; X14 = 1 (initial ref count)
    //   STR X14, [dest, sizeBytes]        ; Store ref count
    //   B +5                              ; Skip bump allocator (5 instrs)
    //   ; Bump allocator:
    //   MOV dest, X28                     ; dest = current heap pointer
    //   MOVZ X15, 1                       ; X15 = 1 (initial ref count)
    //   STR X15, [X28, sizeBytes]         ; store ref count after payload
    //   ADD X28, X28, totalSize           ; bump pointer
    //   (continue)
    lirRegToARM64Reg dest
    |> Result.map (fun destReg ->
        // Total size includes 8 bytes for ref count, aligned to 8 bytes
        let totalSize = ((sizeBytes + 8) + 7) &&& (~~~7)
        if ctx.Options.DisableFreeList then
            // Bump allocator only (no free list reuse)
            (withHeapBoundsCheck
                ctx.HeapOverflowLabel
                [ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X28, uint16 totalSize)]
                [
                    ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)                  // dest = current heap pointer
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)                      // X15 = 1 (initial ref count)
                    ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X28, int16 sizeBytes)   // store ref count after payload
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize) // bump pointer
                ])
            @ generateLeakCounterInc ctx
        else
            // Full allocator with free list support
            let popFreeList = [
                ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X15)                  // dest = freed block
                ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X15, 0s)        // Load next pointer
                ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X27, int16 sizeBytes)   // Update free list head
                ARM64Symbolic.MOVZ (ARM64Symbolic.X14, 1us, 0)                       // X14 = 1 (initial ref count)
                ARM64Symbolic.STR (ARM64Symbolic.X14, destReg, int16 sizeBytes)      // Store ref count
            ]

            let bumpAlloc =
                withHeapBoundsCheck
                    ctx.HeapOverflowLabel
                    [ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X28, uint16 totalSize)]
                    [
                        ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)                  // dest = current heap pointer
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)                      // X15 = 1 (initial ref count)
                        ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X28, int16 sizeBytes)   // store ref count after payload
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize) // bump pointer
                    ]

            [
                ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X27, int16 sizeBytes)   // Load free list head
                ARM64Symbolic.CBZ_offset (ARM64Symbolic.X15, popFreeList.Length + 2)         // If empty, jump past pop path + branch to bump alloc
            ]
            @ popFreeList
            // B uses a current-PC-relative instruction offset, so skipping N instructions needs N + 1.
            @ [ARM64Symbolic.B (bumpAlloc.Length + 1)]
            @ bumpAlloc
            @ generateLeakCounterInc ctx)

let internal emitHeapStore (ctx: CodeGenContext) (addr: LIR.Reg) (offset: int) (src: LIR.Operand) (valueType: AST.SemanticType option) : Result<ARM64Symbolic.Instr list, string> =
    // Store value at addr + offset (offset is in bytes)
    lirRegToARM64Reg addr
    |> Result.bind (fun addrReg ->
        match src, valueType with
        | LIR.Imm value, _ ->
            // Load immediate into temp register, then store
            let tempReg = ARM64Symbolic.X9
            Ok (loadImmediate tempReg value @
                [ARM64Symbolic.STR (tempReg, addrReg, int16 offset)])
        | LIR.Reg srcReg, Some AST.TFloat64 ->
            // Float value in register: interpret as FReg and use STR_fp
            // The srcReg ID is actually an FVirtual, convert to ARM64 FP register
            lirFRegToARM64FReg (virtualToFVirtual srcReg)
            |> Result.map (fun srcARM64FP ->
                [ARM64Symbolic.STR_fp (srcARM64FP, addrReg, int16 offset)])
        | LIR.Reg srcReg, _ ->
            lirRegToARM64Reg srcReg
            |> Result.map (fun srcARM64 ->
                // If src and addr are the same register, we have a problem
                // due to register allocation bug. Use temp register as workaround.
                if srcARM64 = addrReg then
                    // Save value to temp, use temp for store
                    let tempReg = ARM64Symbolic.X9
                    [ARM64Symbolic.MOV_reg (tempReg, srcARM64); ARM64Symbolic.STR (tempReg, addrReg, int16 offset)]
                else
                    [ARM64Symbolic.STR (srcARM64, addrReg, int16 offset)])
        | LIR.StackSlot slotOffset, _ ->
            // Load from stack slot into temp, then store to heap
            let tempReg = ARM64Symbolic.X9
            loadStackSlot tempReg slotOffset
            |> Result.map (fun loadInstrs ->
                loadInstrs @ [ARM64Symbolic.STR (tempReg, addrReg, int16 offset)])
        | LIR.FuncAddr funcName, _ ->
            // Load function address into temp, then store to heap
            let tempReg = ARM64Symbolic.X9
            Ok [ARM64Symbolic.ADR (tempReg, codeLabel (functionName ctx funcName)); ARM64Symbolic.STR (tempReg, addrReg, int16 offset)]
        | LIR.StringSymbol value, _ ->
            // Convert literal string to heap format when storing in tuples/data structures
            // Dynamic and literal strings share [refcount:8][length:8][data:N].
            // We must convert because tuple extraction expects heap format
            let len = utf8Len value
            let labelRef = stringDataLabel value
            let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
            Ok ([
                // Load literal string address into X10
                // Load the literal data pointer after its two-word header.
                ARM64Symbolic.ADRP (ARM64Symbolic.X10, labelRef)
                ARM64Symbolic.ADD_label (ARM64Symbolic.X10, ARM64Symbolic.X10, labelRef)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 16us)
                // Allocate heap space (bump allocator), store address in X9
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X28)  // X9 = current heap pointer (result)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)  // bump pointer
                // Store length (known at compile time)
            ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)
                ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X9, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X11, ARM64Symbolic.X9, 8s)
                // Copy bytes: counter in X13, limit in X11
                ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0us, 0)  // X13 = 0
                // Loop start (if X13 >= len, done)
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X13, ARM64Symbolic.X11)
                ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)  // Skip 7 instructions to exit loop
                ARM64Symbolic.LDRB (ARM64Symbolic.X15, ARM64Symbolic.X10, ARM64Symbolic.X13)  // X15 = literal[X13]
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X9, 16us)
                ARM64Symbolic.ADD_reg (ARM64Symbolic.X14, ARM64Symbolic.X14, ARM64Symbolic.X13)  // X14 = heap + 8 + X13
                ARM64Symbolic.STRB_reg (ARM64Symbolic.X15, ARM64Symbolic.X14)  // heap_data[X13] = byte
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)  // X13++
                ARM64Symbolic.B (-7)  // Loop back to CMP
                // Store heap string address to tuple slot
                ARM64Symbolic.STR (ARM64Symbolic.X9, addrReg, int16 offset)
            ] @ generateLeakCounterInc ctx)
        | LIR.FloatSymbol value, _ ->
            // Load float literal from pool into temp FP register, then store to heap
            let labelRef = floatDataLabel value
            Ok [
                ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)              // Load page address
                ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef) // Add offset
                ARM64Symbolic.LDR_fp (ARM64Symbolic.D15, ARM64Symbolic.X9, 0s)         // Load float into D15
                ARM64Symbolic.STR_fp (ARM64Symbolic.D15, addrReg, int16 offset) // Store float to heap
            ]
        | _ -> Error "Unsupported operand type in HeapStore")

let internal emitHeapLoad (ctx: CodeGenContext) (dest: LIR.Reg) (addr: LIR.Reg) (offset: int) : Result<ARM64Symbolic.Instr list, string> =
    // Load value from addr + offset (offset is in bytes)
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg addr
        |> Result.map (fun addrReg ->
            [ARM64Symbolic.LDR (destReg, addrReg, int16 offset)]))

let internal emitMappedAlloc (ctx: CodeGenContext) (dest: LIR.Reg) (numBytes: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest |> Result.bind (fun destReg ->
        lirRegToARM64Reg numBytes |> Result.map (fun sizeReg ->
            let syscalls = ARM64.targetSyscalls ctx.Target
            let os = ARM64.targetOS ctx.Target
            let flags = if os = Platform.MacOS then 0x1002us else 0x22us
            // The mapping length lives before the returned, word-aligned
            // payload. Preserve it across the syscall, not in a volatile reg.
            [ ARM64Symbolic.CMP_imm (sizeReg, 0us)
              ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, ctx.HeapOverflowLabel)
              ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, sizeReg, 8us)
              ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 0us)
              ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, ctx.HeapOverflowLabel)
              ARM64Symbolic.STP_pre (ARM64Symbolic.X1, ARM64Symbolic.X30, ARM64Symbolic.SP, -16s)
              ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
              ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 3us, 0)
              ARM64Symbolic.MOVZ (ARM64Symbolic.X3, flags, 0)
              ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 0us, 0)
              ARM64Symbolic.MVN (ARM64Symbolic.X4, ARM64Symbolic.X4)
              ARM64Symbolic.MOVZ (ARM64Symbolic.X5, 0us, 0)
              ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Mmap, 0)
              ARM64Symbolic.SVC syscalls.SvcImmediate ]
            @ (if os = Platform.MacOS then
                   [ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, ctx.HeapOverflowLabel)]
               else
                   [ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us); ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, ctx.HeapOverflowLabel)])
            @ [ ARM64Symbolic.LDP_post (ARM64Symbolic.X1, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.STR (ARM64Symbolic.X1, ARM64Symbolic.X0, 0s)
                ARM64Symbolic.ADD_imm (destReg, ARM64Symbolic.X0, 8us) ]
            @ generateLeakCounterInc ctx))

let internal emitMappedFree (ctx: CodeGenContext) (ptr: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg ptr |> Result.map (fun ptrReg ->
        let syscalls = ARM64.targetSyscalls ctx.Target
        [ ARM64Symbolic.SUB_imm (ARM64Symbolic.X0, ptrReg, 8us)
          ARM64Symbolic.LDR (ARM64Symbolic.X1, ARM64Symbolic.X0, 0s)
          ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Munmap, 0)
          ARM64Symbolic.SVC syscalls.SvcImmediate
          ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
          ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, ctx.HeapOverflowLabel) ]
        @ generateLeakCounterDec ctx)

let internal emitRawAlloc (ctx: CodeGenContext) (dest: LIR.Reg) (numBytes: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Raw allocation: free-list reuse for small aligned size classes, else bump allocation.
    // This path is used by skew-list nodes, so freed raw blocks must be reused to avoid OOM.
    // numBytes is already in a physical register (from MIR_to_LIR)
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg numBytes
        |> Result.map (fun numBytesReg ->
            let alignSizeInstrs = [
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, numBytesReg, 7us)        // X15 = numBytes + 7
                ARM64Symbolic.MOVZ (ARM64Symbolic.X14, 3us, 0)                     // X14 = 3 (shift amount)
                ARM64Symbolic.LSR_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X14)  // X15 = (numBytes + 7) >> 3
                ARM64Symbolic.LSL_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X14)  // X15 = aligned size
            ]

            if ctx.Options.DisableFreeList then
                alignSizeInstrs
                @ checkedBumpAllocReg ctx.HeapOverflowLabel destReg ARM64Symbolic.X15
                @ generateLeakCounterInc ctx
            else
                let popFreeList = [
                    ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X14)             // dest = free-list head block
                    ARM64Symbolic.LDR (ARM64Symbolic.X13, ARM64Symbolic.X14, 0s)   // X13 = next block
                    ARM64Symbolic.STR (ARM64Symbolic.X13, ARM64Symbolic.X12, 0s)   // update free-list head
                ]

                let bumpAlloc = checkedBumpAllocReg ctx.HeapOverflowLabel destReg ARM64Symbolic.X15

                alignSizeInstrs
                @ [
                    ARM64Symbolic.CMP_imm (ARM64Symbolic.X15, 8us)                  // Need at least payload + refcount to have a payload class
                    ARM64Symbolic.B_cond (ARM64Symbolic.LT, popFreeList.Length + 8) // skip free-list lookup for undersized allocations
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X13, ARM64Symbolic.X15, 8us) // X13 = payload size class (total size minus refcount word)
                    ARM64Symbolic.CMP_imm (ARM64Symbolic.X13, 248us)                // free-list has slots for payload classes up to 248 bytes
                    ARM64Symbolic.B_cond (ARM64Symbolic.GT, popFreeList.Length + 5) // skip free-list lookup when class is out of range
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X12, ARM64Symbolic.X27, ARM64Symbolic.X13) // X12 = &free_list[payload_class]
                    ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)    // X14 = free-list head
                    ARM64Symbolic.CBZ_offset (ARM64Symbolic.X14, popFreeList.Length + 2) // if empty, jump to bump path
                ]
                @ popFreeList
                // B uses a current-PC-relative instruction offset, so skipping N instructions needs N + 1.
                @ [ARM64Symbolic.B (bumpAlloc.Length + 1)]
                @ bumpAlloc
                @ generateLeakCounterInc ctx))

let internal emitRawFree (ctx: CodeGenContext) (ptr: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // RawFree is an internal 8-byte cell operation. Stream producer-state
    // cells and lifecycle probes are its only callers, so the size class is
    // statically exact even though RawPtr itself is erased.
    lirRegToARM64Reg ptr
    |> Result.map (fun ptrReg ->
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X27, 0s)
            ARM64Symbolic.STR (ARM64Symbolic.X14, ptrReg, 0s)
            ARM64Symbolic.STR (ptrReg, ARM64Symbolic.X27, 0s)
        ]
        @ generateLeakCounterDec ctx)

let internal emitRawGet (ctx: CodeGenContext) (dest: LIR.Reg) (ptr: LIR.Reg) (byteOffset: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Load 8 bytes from ptr + byteOffset
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg ptr
        |> Result.bind (fun ptrReg ->
            lirRegToARM64Reg byteOffset
            |> Result.map (fun offsetReg ->
                [
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X15, ptrReg, offsetReg)   // X15 = ptr + offset
                    ARM64Symbolic.LDR (destReg, ARM64Symbolic.X15, 0s)             // dest = [X15]
                ])))

let internal emitRawGetByte (ctx: CodeGenContext) (dest: LIR.Reg) (ptr: LIR.Reg) (byteOffset: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Load 1 byte from ptr + byteOffset (zero-extended to 64 bits)
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg ptr
        |> Result.bind (fun ptrReg ->
            lirRegToARM64Reg byteOffset
            |> Result.map (fun offsetReg ->
                [
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X15, ptrReg, offsetReg)   // X15 = ptr + offset
                    ARM64Symbolic.LDRB_imm (destReg, ARM64Symbolic.X15, 0)         // dest = [X15] (byte, zero-extended)
                ])))

let internal emitRawWriteWord (ctx: CodeGenContext) (ptr: LIR.Reg) (byteOffset: LIR.Reg) (value: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Store 8 unmanaged bytes at ptr + byteOffset.
    lirRegToARM64Reg ptr
    |> Result.bind (fun ptrReg ->
        lirRegToARM64Reg byteOffset
        |> Result.bind (fun offsetReg ->
            lirRegToARM64Reg value
            |> Result.map (fun valueReg ->
                let tempReg =
                    if ptrReg = ARM64Symbolic.X15 || offsetReg = ARM64Symbolic.X15 || valueReg = ARM64Symbolic.X15 then
                        ARM64Symbolic.X14
                    else
                        ARM64Symbolic.X15
                [
                    ARM64Symbolic.ADD_reg (tempReg, ptrReg, offsetReg)   // temp = ptr + offset
                    ARM64Symbolic.STR (valueReg, tempReg, 0s)            // [temp] = value
                ])))

let internal emitRawSlotInit (ctx: CodeGenContext) (ptr: LIR.Reg) (byteOffset: LIR.Reg) (value: LIR.Reg) (valueType: AST.SemanticType) : Result<ARM64Symbolic.Instr list, string> =
    // Store 8 bytes at ptr + byteOffset.
    // If the stored value is RC-managed, increment ownership because the parent now owns that edge.
    lirRegToARM64Reg ptr
    |> Result.bind (fun ptrReg ->
        lirRegToARM64Reg byteOffset
        |> Result.bind (fun offsetReg ->
            lirRegToARM64Reg value
            |> Result.map (fun valueReg ->
                let tempReg =
                    if ptrReg = ARM64Symbolic.X15 || offsetReg = ARM64Symbolic.X15 || valueReg = ARM64Symbolic.X15 then
                        ARM64Symbolic.X14
                    else
                        ARM64Symbolic.X15
                let storeValue = [
                    ARM64Symbolic.ADD_reg (tempReg, ptrReg, offsetReg)   // temp = ptr + offset
                    ARM64Symbolic.STR (valueReg, tempReg, 0s)            // [temp] = value
                ]

                let retainTarget =
                    match ctx.RawSlotInitRetainTargets with
                    | Some targets ->
                        Map.tryFind valueType targets |> Option.flatten
                    | None ->
                        slotInitRootRetainTarget
                            ctx.RecordRegistry
                            ctx.SumShapeRegistry
                            valueType

                let ownershipInc =
                    match retainTarget with
                    | Some LIR.SlotInitListRootRetain ->
                        [
                            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -64s)
                            ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                            ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                            ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, valueReg)
                            ARM64Symbolic.BL listRefCountIncHelperLabel
                            ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                            ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                            ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 64s)
                        ]
                    | Some LIR.SlotInitDictRootRetain ->
                        [
                            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -80s)
                            ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                            ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                            ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                            ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, valueReg)
                            ARM64Symbolic.BL dictRefCountIncHelperLabel
                            ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                            ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                            ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                            ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 80s)
                        ]
                    | Some LIR.SlotInitDynamicBufferRetain ->
                        let refAddrReg, preserveAddr =
                            if valueReg = ARM64Symbolic.X13 || valueReg = ARM64Symbolic.X15 then
                                ARM64Symbolic.X12, [ARM64Symbolic.MOV_reg (ARM64Symbolic.X12, valueReg)]
                            else
                                valueReg, []
                        let refcountPath = [
                            ARM64Symbolic.LDR (ARM64Symbolic.X15, refAddrReg, 0s)
                            ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0xFFFFus, 0)
                            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 16)
                            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 32)
                            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0x7FFFus, 48)
                            ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)
                            ARM64Symbolic.B_cond (ARM64Symbolic.EQ, 3)
                            ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                            ARM64Symbolic.STR (ARM64Symbolic.X15, refAddrReg, 0s)
                        ]
                        let guards =
                            match valueType with
                            | AST.TInt ->
                                [ ARM64Symbolic.CBZ_offset (refAddrReg, List.length refcountPath + 3)
                                  ARM64Symbolic.AND_imm (ARM64Symbolic.X13, refAddrReg, 1UL)
                                  ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X13, List.length refcountPath + 1) ]
                            | _ ->
                                [ARM64Symbolic.CBZ_offset (refAddrReg, List.length refcountPath + 1)]
                        preserveAddr
                        @ guards
                        @ refcountPath
                    | Some LIR.SlotInitClosureRootRetain ->
                        let closureIncCall = [
                            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -96s)
                            ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                            ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                            ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                            ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                            ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, valueReg)
                            ARM64Symbolic.BL closureRefCountIncHelperLabel
                            ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                            ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                            ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                            ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                            ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 96s)
                        ]
                        [
                            ARM64Symbolic.CBZ_offset (valueReg, List.length closureIncCall + 1)
                        ]
                        @ closureIncCall
                    | Some (LIR.SlotInitGenericRootRetain payloadSize) ->
                        let rcReg =
                            if valueReg = ARM64Symbolic.X15 then ARM64Symbolic.X14 else ARM64Symbolic.X15
                        [
                            ARM64Symbolic.CBZ_offset (valueReg, 4)
                            ARM64Symbolic.LDR (rcReg, valueReg, int16 payloadSize)
                            ARM64Symbolic.ADD_imm (rcReg, rcReg, 1us)
                            ARM64Symbolic.STR (rcReg, valueReg, int16 payloadSize)
                        ]
                    | _ -> []

                storeValue @ ownershipInc)))

let internal emitRawWriteByte (ctx: CodeGenContext) (ptr: LIR.Reg) (byteOffset: LIR.Reg) (value: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Store 1 byte at ptr + byteOffset
    // IMPORTANT: If any input reg is X15, use X14 as temp instead
    lirRegToARM64Reg ptr
    |> Result.bind (fun ptrReg ->
        lirRegToARM64Reg byteOffset
        |> Result.bind (fun offsetReg ->
            lirRegToARM64Reg value
            |> Result.map (fun valueReg ->
                let tempReg =
                    if ptrReg = ARM64Symbolic.X15 || offsetReg = ARM64Symbolic.X15 || valueReg = ARM64Symbolic.X15 then
                        ARM64Symbolic.X14
                    else
                        ARM64Symbolic.X15
                [
                    ARM64Symbolic.ADD_reg (tempReg, ptrReg, offsetReg)   // temp = ptr + offset
                    ARM64Symbolic.STRB_reg (valueReg, tempReg)           // [temp] = value (byte)
                ])))
