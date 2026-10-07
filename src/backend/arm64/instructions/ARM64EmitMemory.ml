(*
   ARM64EmitMemory.ml - Emit arm64 instructions for memory operations.
*)
[@@@warning "-4"]

let bind f value = Result.bind value f
let add a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))

let int16 value =
  let low = value land 65535 in
  if low >= 32768 then low - 65536 else low

open ARM64CodeGenTypes
open HeapAllocation
open LeakAccounting
open ARM64Operands

(*
   Heap allocator with free list support
   X27 = free list heads base, X28 = bump allocator pointer
   Memory layout with reference counting:
   [payload: sizeBytes][refcount: 8 bytes]
   Algorithm:
   1. Check free list for this size class (sizeClassOffset = sizeBytes)
   2. If free list non-empty: pop from list, initialize refcount, return
   3. If empty: bump allocate from X28
   Code structure (10 instructions):
   LDR X15, [X27, sizeBytes]         ; Load free list head
   CBZ X15, +5                       ; If empty, skip to bump alloc (5 instrs)
   MOV dest, X15                     ; dest = freed block
   LDR X14, [X15, 0]                 ; Load next pointer from freed block
   STR X14, [X27, sizeBytes]         ; Update free list head
   MOVZ X14, 1                       ; X14 = 1 (initial ref count)
   STR X14, [dest, sizeBytes]        ; Store ref count
   B +5                              ; Skip bump allocator (5 instrs)
   ; Bump allocator:
   MOV dest, X28                     ; dest = current heap pointer
   MOVZ X15, 1                       ; X15 = 1 (initial ref count)
   STR X15, [X28, sizeBytes]         ; store ref count after payload
   ADD X28, X28, totalSize           ; bump pointer
   (continue)
   Total size includes 8 bytes for ref count, aligned to 8 bytes
   Bump allocator only (no free list reuse)
   dest = current heap pointer
   X15 = 1 (initial ref count)
   store ref count after payload
   bump pointer
   Full allocator with free list support
   dest = freed block
   Load next pointer
   Update free list head
   X14 = 1 (initial ref count)
   Store ref count
   Load free list head
   If empty, jump past pop path + branch to bump alloc
   B uses a current-PC-relative instruction offset, so skipping N instructions needs N + 1.
*)
let emitHeapAlloc (ctx : codeGenContext) (dest : LIR.reg) (sizeBytes : int) =
  lirRegToARM64Reg dest
  |> Result.map (fun destReg ->
      let totalSize = add (add sizeBytes 8) 7 land lnot 7 in
      if ctx.options.disableFreeList then
        withHeapBoundsCheck ctx.heapOverflowLabel
          [
            Symbolic.ADD_imm (Symbolic.X14, Symbolic.X28, totalSize land 65535);
          ]
          [
            Symbolic.MOV_reg (destReg, Symbolic.X28);
            Symbolic.MOVZ (Symbolic.X15, 1, 0);
            Symbolic.STR (Symbolic.X15, Symbolic.X28, int16 sizeBytes);
            Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, totalSize land 65535);
          ]
        @ generateLeakCounterInc ctx
      else
        let popFreeList =
          [
            Symbolic.MOV_reg (destReg, Symbolic.X15);
            Symbolic.LDR (Symbolic.X14, Symbolic.X15, 0);
            Symbolic.STR (Symbolic.X14, Symbolic.X27, int16 sizeBytes);
            Symbolic.MOVZ (Symbolic.X14, 1, 0);
            Symbolic.STR (Symbolic.X14, destReg, int16 sizeBytes);
          ]
        in
        let bumpAlloc =
          withHeapBoundsCheck ctx.heapOverflowLabel
            [
              Symbolic.ADD_imm (Symbolic.X14, Symbolic.X28, totalSize land 65535);
            ]
            [
              Symbolic.MOV_reg (destReg, Symbolic.X28);
              Symbolic.MOVZ (Symbolic.X15, 1, 0);
              Symbolic.STR (Symbolic.X15, Symbolic.X28, int16 sizeBytes);
              Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, totalSize land 65535);
            ]
        in
        [
          Symbolic.LDR (Symbolic.X15, Symbolic.X27, int16 sizeBytes);
          Symbolic.CBZ_offset (Symbolic.X15, List.length popFreeList + 2);
        ]
        @ popFreeList
        @ [ Symbolic.B (List.length bumpAlloc + 1) ]
        @ bumpAlloc @ generateLeakCounterInc ctx)

(*
   Store value at addr + offset (offset is in bytes)
   Load immediate into temp register, then store
   Float value in register: interpret as FReg and use STR_fp
   The srcReg ID is actually an FVirtual, convert to ARM64 FP register
   If src and addr are the same register, we have a problem
   due to register allocation bug. Use temp register as workaround.
   Save value to temp, use temp for store
   Load from stack slot into temp, then store to heap
   Load function address into temp, then store to heap
   Convert literal string to heap format when storing in tuples/data structures
   Dynamic and literal strings share [refcount:8][length:8][data:N].
   We must convert because tuple extraction expects heap format
   8-byte aligned
   Load literal string address into X10
   Load the literal data pointer after its two-word header.
   Allocate heap space (bump allocator), store address in X9
   X9 = current heap pointer (result)
   bump pointer
   Store length (known at compile time)
   Copy bytes: counter in X13, limit in X11
   X13 = 0
   Loop start (if X13 >= len, done)
   Skip 7 instructions to exit loop
   X15 = literal[X13]
   X14 = heap + 8 + X13
   heap_data[X13] = byte
   X13++
   Loop back to CMP
   Store heap string address to tuple slot
   Load float literal from pool into temp FP register, then store to heap
   Load page address
   Add offset
   Load float into D15
   Store float to heap
*)
let emitHeapStore (ctx : codeGenContext) (addr : LIR.reg) (offset : int)
    (src : LIR.operand) (valueType : AST.semanticType option) =
  lirRegToARM64Reg addr
  |> bind (fun addrReg ->
      match (src, valueType) with
      | LIR.Imm value, _ ->
          let tempReg = Symbolic.X9 in
          Ok
            (loadImmediate tempReg value
            @ [ Symbolic.STR (tempReg, addrReg, int16 offset) ])
      | LIR.Reg srcReg, Some AST.TFloat64 ->
          lirFRegToARM64FReg (virtualToFVirtual srcReg)
          |> Result.map (fun srcARM64FP ->
              [ Symbolic.STR_fp (srcARM64FP, addrReg, int16 offset) ])
      | LIR.Reg srcReg, _ ->
          lirRegToARM64Reg srcReg
          |> Result.map (fun srcARM64 ->
              if srcARM64 = addrReg then
                let tempReg = Symbolic.X9 in
                [
                  Symbolic.MOV_reg (tempReg, srcARM64);
                  Symbolic.STR (tempReg, addrReg, int16 offset);
                ]
              else [ Symbolic.STR (srcARM64, addrReg, int16 offset) ])
      | LIR.StackSlot slotOffset, _ ->
          let tempReg = Symbolic.X9 in
          loadStackSlot tempReg slotOffset
          |> Result.map (fun loadInstrs ->
              loadInstrs @ [ Symbolic.STR (tempReg, addrReg, int16 offset) ])
      | LIR.FuncAddr funcName, _ ->
          let tempReg = Symbolic.X9 in
          Ok
            [
              Symbolic.ADR (tempReg, codeLabel (functionName ctx funcName));
              Symbolic.STR (tempReg, addrReg, int16 offset);
            ]
      | LIR.StringSymbol value, _ ->
          let len = utf8Len value in
          let labelRef = stringDataLabel value in
          let totalSize = add (add len 16) 7 land lnot 7 in
          Ok
            ([
               Symbolic.ADRP (Symbolic.X10, labelRef);
               Symbolic.ADD_label (Symbolic.X10, Symbolic.X10, labelRef);
               Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 16);
               Symbolic.MOV_reg (Symbolic.X9, Symbolic.X28);
               Symbolic.ADD_imm
                 (Symbolic.X28, Symbolic.X28, totalSize land 65535);
             ]
            @ loadImmediate Symbolic.X11 (Int64.of_int len)
            @ [
                Symbolic.MOVZ (Symbolic.X15, 1, 0);
                Symbolic.STR (Symbolic.X15, Symbolic.X9, 0);
                Symbolic.STR (Symbolic.X11, Symbolic.X9, 8);
                Symbolic.MOVZ (Symbolic.X13, 0, 0);
                Symbolic.CMP_reg (Symbolic.X13, Symbolic.X11);
                Symbolic.B_cond (Symbolic.GE, 7);
                Symbolic.LDRB (Symbolic.X15, Symbolic.X10, Symbolic.X13);
                Symbolic.ADD_imm (Symbolic.X14, Symbolic.X9, 16);
                Symbolic.ADD_reg (Symbolic.X14, Symbolic.X14, Symbolic.X13);
                Symbolic.STRB_reg (Symbolic.X15, Symbolic.X14);
                Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 1);
                Symbolic.B (-7);
                Symbolic.STR (Symbolic.X9, addrReg, int16 offset);
              ]
            @ generateLeakCounterInc ctx)
      | LIR.FloatSymbol value, _ ->
          let labelRef = floatDataLabel value in
          Ok
            [
              Symbolic.ADRP (Symbolic.X9, labelRef);
              Symbolic.ADD_label (Symbolic.X9, Symbolic.X9, labelRef);
              Symbolic.LDR_fp (Symbolic.D15, Symbolic.X9, 0);
              Symbolic.STR_fp (Symbolic.D15, addrReg, int16 offset);
            ]
      | _ -> Error "Unsupported operand type in HeapStore")

(*
   Load value from addr + offset (offset is in bytes)
*)
let emitHeapLoad (_ctx : codeGenContext) (dest : LIR.reg) (addr : LIR.reg)
    (offset : int) =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      lirRegToARM64Reg addr
      |> Result.map (fun addrReg ->
          [ Symbolic.LDR (destReg, addrReg, int16 offset) ]))

(*
   The mapping length lives before the returned, word-aligned
   payload. Preserve it across the syscall, not in a volatile reg.
*)
let emitMappedAlloc (ctx : codeGenContext) (dest : LIR.reg) (numBytes : LIR.reg)
    =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      lirRegToARM64Reg numBytes
      |> Result.map (fun sizeReg ->
          let syscalls = ARM64.targetSyscalls ctx.target in
          let os = ARM64.targetOS ctx.target in
          let flags = if os = Platform.MacOS then 0x1002 else 0x22 in
          [
            Symbolic.CMP_imm (sizeReg, 0);
            Symbolic.B_cond_label (Symbolic.LT, ctx.heapOverflowLabel);
            Symbolic.ADD_imm (Symbolic.X1, sizeReg, 8);
            Symbolic.CMP_imm (Symbolic.X1, 0);
            Symbolic.B_cond_label (Symbolic.LE, ctx.heapOverflowLabel);
            Symbolic.STP_pre (Symbolic.X1, Symbolic.X30, Symbolic.SP, -16);
            Symbolic.MOVZ (Symbolic.X0, 0, 0);
            Symbolic.MOVZ (Symbolic.X2, 3, 0);
            Symbolic.MOVZ (Symbolic.X3, flags, 0);
            Symbolic.MOVZ (Symbolic.X4, 0, 0);
            Symbolic.MVN (Symbolic.X4, Symbolic.X4);
            Symbolic.MOVZ (Symbolic.X5, 0, 0);
            Symbolic.MOVZ
              ( syscalls.ARM64.syscallRegister,
                syscalls.ARM64.numbers.Platform.mmap,
                0 );
            Symbolic.SVC syscalls.ARM64.svcImmediate;
          ]
          @ (if os = Platform.MacOS then
               [ Symbolic.B_cond_label (Symbolic.HS, ctx.heapOverflowLabel) ]
             else
               [
                 Symbolic.CMP_imm (Symbolic.X0, 0);
                 Symbolic.B_cond_label (Symbolic.LT, ctx.heapOverflowLabel);
               ])
          @ [
              Symbolic.LDP_post (Symbolic.X1, Symbolic.X30, Symbolic.SP, 16);
              Symbolic.STR (Symbolic.X1, Symbolic.X0, 0);
              Symbolic.ADD_imm (destReg, Symbolic.X0, 8);
            ]
          @ generateLeakCounterInc ctx))

let emitMappedFree (ctx : codeGenContext) (ptr : LIR.reg) =
  lirRegToARM64Reg ptr
  |> Result.map (fun ptrReg ->
      let syscalls = ARM64.targetSyscalls ctx.target in
      [
        Symbolic.SUB_imm (Symbolic.X0, ptrReg, 8);
        Symbolic.LDR (Symbolic.X1, Symbolic.X0, 0);
        Symbolic.MOVZ
          ( syscalls.ARM64.syscallRegister,
            syscalls.ARM64.numbers.Platform.munmap,
            0 );
        Symbolic.SVC syscalls.ARM64.svcImmediate;
        Symbolic.CMP_imm (Symbolic.X0, 0);
        Symbolic.B_cond_label (Symbolic.NE, ctx.heapOverflowLabel);
      ]
      @ generateLeakCounterDec ctx)

(*
   Raw allocation: free-list reuse for small aligned size classes, else bump allocation.
   This path is used by skew-list nodes, so freed raw blocks must be reused to avoid OOM.
   numBytes is already in a physical register (from MIR_to_LIR)
   X15 = numBytes + 7
   X14 = 3 (shift amount)
   X15 = (numBytes + 7) >> 3
   X15 = aligned size
   dest = free-list head block
   X13 = next block
   update free-list head
   Need at least payload + refcount to have a payload class
   skip free-list lookup for undersized allocations
   X13 = payload size class (total size minus refcount word)
   free-list has slots for payload classes up to 248 bytes
   skip free-list lookup when class is out of range
   X12 = &free_list[payload_class]
   X14 = free-list head
   if empty, jump to bump path
   B uses a current-PC-relative instruction offset, so skipping N instructions needs N + 1.
*)
let emitRawAlloc (ctx : codeGenContext) (dest : LIR.reg) (numBytes : LIR.reg) =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      lirRegToARM64Reg numBytes
      |> Result.map (fun numBytesReg ->
          let alignSizeInstrs =
            [
              Symbolic.ADD_imm (Symbolic.X15, numBytesReg, 7);
              Symbolic.MOVZ (Symbolic.X14, 3, 0);
              Symbolic.LSR_reg (Symbolic.X15, Symbolic.X15, Symbolic.X14);
              Symbolic.LSL_reg (Symbolic.X15, Symbolic.X15, Symbolic.X14);
            ]
          in
          if ctx.options.disableFreeList then
            alignSizeInstrs
            @ checkedBumpAllocReg ctx.heapOverflowLabel destReg Symbolic.X15
            @ generateLeakCounterInc ctx
          else
            let popFreeList =
              [
                Symbolic.MOV_reg (destReg, Symbolic.X14);
                Symbolic.LDR (Symbolic.X13, Symbolic.X14, 0);
                Symbolic.STR (Symbolic.X13, Symbolic.X12, 0);
              ]
            in
            let bumpAlloc =
              checkedBumpAllocReg ctx.heapOverflowLabel destReg Symbolic.X15
            in
            alignSizeInstrs
            @ [
                Symbolic.CMP_imm (Symbolic.X15, 8);
                Symbolic.B_cond (Symbolic.LT, List.length popFreeList + 8);
                Symbolic.SUB_imm (Symbolic.X13, Symbolic.X15, 8);
                Symbolic.CMP_imm (Symbolic.X13, 248);
                Symbolic.B_cond (Symbolic.GT, List.length popFreeList + 5);
                Symbolic.ADD_reg (Symbolic.X12, Symbolic.X27, Symbolic.X13);
                Symbolic.LDR (Symbolic.X14, Symbolic.X12, 0);
                Symbolic.CBZ_offset (Symbolic.X14, List.length popFreeList + 2);
              ]
            @ popFreeList
            @ [ Symbolic.B (List.length bumpAlloc + 1) ]
            @ bumpAlloc @ generateLeakCounterInc ctx))

(*
   RawFree is an internal 8-byte cell operation. Stream producer-state
   cells and lifecycle probes are its only callers, so the size class is
   statically exact even though RawPtr itself is erased.
*)
let emitRawFree (ctx : codeGenContext) (ptr : LIR.reg) =
  lirRegToARM64Reg ptr
  |> Result.map (fun ptrReg ->
      [
        Symbolic.LDR (Symbolic.X14, Symbolic.X27, 0);
        Symbolic.STR (Symbolic.X14, ptrReg, 0);
        Symbolic.STR (ptrReg, Symbolic.X27, 0);
      ]
      @ generateLeakCounterDec ctx)

(*
   Load 8 bytes from ptr + byteOffset
   X15 = ptr + offset
   dest = [X15]
*)
let emitRawGet (_ctx : codeGenContext) (dest : LIR.reg) (ptr : LIR.reg)
    (byteOffset : LIR.reg) =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      lirRegToARM64Reg ptr
      |> bind (fun ptrReg ->
          lirRegToARM64Reg byteOffset
          |> Result.map (fun offsetReg ->
              [
                Symbolic.ADD_reg (Symbolic.X15, ptrReg, offsetReg);
                Symbolic.LDR (destReg, Symbolic.X15, 0);
              ])))

(*
   Load 1 byte from ptr + byteOffset (zero-extended to 64 bits)
   X15 = ptr + offset
   dest = [X15] (byte, zero-extended)
*)
let emitRawGetByte (_ctx : codeGenContext) (dest : LIR.reg) (ptr : LIR.reg)
    (byteOffset : LIR.reg) =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      lirRegToARM64Reg ptr
      |> bind (fun ptrReg ->
          lirRegToARM64Reg byteOffset
          |> Result.map (fun offsetReg ->
              [
                Symbolic.ADD_reg (Symbolic.X15, ptrReg, offsetReg);
                Symbolic.LDRB_imm (destReg, Symbolic.X15, 0);
              ])))

(*
   Store 8 unmanaged bytes at ptr + byteOffset.
   temp = ptr + offset
   [temp] = value
*)
let emitRawWriteWord (_ctx : codeGenContext) (ptr : LIR.reg)
    (byteOffset : LIR.reg) (value : LIR.reg) =
  lirRegToARM64Reg ptr
  |> bind (fun ptrReg ->
      lirRegToARM64Reg byteOffset
      |> bind (fun offsetReg ->
          lirRegToARM64Reg value
          |> Result.map (fun valueReg ->
              let tempReg =
                if
                  ptrReg = Symbolic.X15 || offsetReg = Symbolic.X15
                  || valueReg = Symbolic.X15
                then Symbolic.X14
                else Symbolic.X15
              in
              [
                Symbolic.ADD_reg (tempReg, ptrReg, offsetReg);
                Symbolic.STR (valueReg, tempReg, 0);
              ])))

(*
   Store 8 bytes at ptr + byteOffset.
   If the stored value is RC-managed, increment ownership because the parent now owns that edge.
   temp = ptr + offset
   [temp] = value
*)
let emitRawSlotInit (ctx : codeGenContext) (ptr : LIR.reg)
    (byteOffset : LIR.reg) (value : LIR.reg) (valueType : AST.semanticType) =
  lirRegToARM64Reg ptr
  |> bind (fun ptrReg ->
      lirRegToARM64Reg byteOffset
      |> bind (fun offsetReg ->
          lirRegToARM64Reg value
          |> Result.map (fun valueReg ->
              let tempReg =
                if
                  ptrReg = Symbolic.X15 || offsetReg = Symbolic.X15
                  || valueReg = Symbolic.X15
                then Symbolic.X14
                else Symbolic.X15
              in
              let storeValue =
                [
                  Symbolic.ADD_reg (tempReg, ptrReg, offsetReg);
                  Symbolic.STR (valueReg, tempReg, 0);
                ]
              in
              let retainTarget =
                match ctx.rawSlotInitRetainTargets with
                | Some targets ->
                    LIR.SemanticTypeMap.find_opt valueType targets
                    |> Option.join
                | None ->
                    slotInitRootRetainTarget ctx.recordRegistry
                      ctx.sumShapeRegistry valueType
              in
              let ownershipInc =
                match retainTarget with
                | Some LIR.SlotInitListRootRetain ->
                    [
                      Symbolic.STP_pre
                        (Symbolic.X0, Symbolic.X1, Symbolic.SP, -64);
                      Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                      Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                      Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                      Symbolic.MOV_reg (Symbolic.X0, valueReg);
                      Symbolic.BL listRefCountIncHelperLabel;
                      Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                      Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                      Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                      Symbolic.LDP_post
                        (Symbolic.X0, Symbolic.X1, Symbolic.SP, 64);
                    ]
                | Some LIR.SlotInitDictRootRetain ->
                    [
                      Symbolic.STP_pre
                        (Symbolic.X0, Symbolic.X1, Symbolic.SP, -80);
                      Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                      Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                      Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                      Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                      Symbolic.MOV_reg (Symbolic.X0, valueReg);
                      Symbolic.BL dictRefCountIncHelperLabel;
                      Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                      Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                      Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                      Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                      Symbolic.LDP_post
                        (Symbolic.X0, Symbolic.X1, Symbolic.SP, 80);
                    ]
                | Some LIR.SlotInitDynamicBufferRetain ->
                    let refAddrReg, preserveAddr =
                      if valueReg = Symbolic.X13 || valueReg = Symbolic.X15 then
                        ( Symbolic.X12,
                          [ Symbolic.MOV_reg (Symbolic.X12, valueReg) ] )
                      else (valueReg, [])
                    in
                    let refcountPath =
                      [
                        Symbolic.LDR (Symbolic.X15, refAddrReg, 0);
                        Symbolic.MOVZ (Symbolic.X13, 0xFFFF, 0);
                        Symbolic.MOVK (Symbolic.X13, 0xFFFF, 16);
                        Symbolic.MOVK (Symbolic.X13, 0xFFFF, 32);
                        Symbolic.MOVK (Symbolic.X13, 0x7FFF, 48);
                        Symbolic.CMP_reg (Symbolic.X15, Symbolic.X13);
                        Symbolic.B_cond (Symbolic.EQ, 3);
                        Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 1);
                        Symbolic.STR (Symbolic.X15, refAddrReg, 0);
                      ]
                    in
                    let guards =
                      match valueType with
                      | AST.TInt ->
                          [
                            Symbolic.CBZ_offset
                              (refAddrReg, List.length refcountPath + 3);
                            Symbolic.AND_imm (Symbolic.X13, refAddrReg, 1L);
                            Symbolic.CBNZ_offset
                              (Symbolic.X13, List.length refcountPath + 1);
                          ]
                      | _ ->
                          [
                            Symbolic.CBZ_offset
                              (refAddrReg, List.length refcountPath + 1);
                          ]
                    in
                    preserveAddr @ guards @ refcountPath
                | Some LIR.SlotInitClosureRootRetain ->
                    let closureIncCall =
                      [
                        Symbolic.STP_pre
                          (Symbolic.X0, Symbolic.X1, Symbolic.SP, -96);
                        Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                        Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                        Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                        Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                        Symbolic.STP
                          (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                        Symbolic.MOV_reg (Symbolic.X0, valueReg);
                        Symbolic.BL closureRefCountIncHelperLabel;
                        Symbolic.LDP
                          (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                        Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                        Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                        Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                        Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                        Symbolic.LDP_post
                          (Symbolic.X0, Symbolic.X1, Symbolic.SP, 96);
                      ]
                    in
                    [
                      Symbolic.CBZ_offset
                        (valueReg, List.length closureIncCall + 1);
                    ]
                    @ closureIncCall
                | Some (LIR.SlotInitGenericRootRetain payloadSize) ->
                    let rcReg =
                      if valueReg = Symbolic.X15 then Symbolic.X14
                      else Symbolic.X15
                    in
                    [
                      Symbolic.CBZ_offset (valueReg, 4);
                      Symbolic.LDR (rcReg, valueReg, int16 payloadSize);
                      Symbolic.ADD_imm (rcReg, rcReg, 1);
                      Symbolic.STR (rcReg, valueReg, int16 payloadSize);
                    ]
                | _ -> []
              in
              storeValue @ ownershipInc)))

(*
   Store 1 byte at ptr + byteOffset
   IMPORTANT: If any input reg is X15, use X14 as temp instead
   temp = ptr + offset
   [temp] = value (byte)
*)
let emitRawWriteByte (_ctx : codeGenContext) (ptr : LIR.reg)
    (byteOffset : LIR.reg) (value : LIR.reg) =
  lirRegToARM64Reg ptr
  |> bind (fun ptrReg ->
      lirRegToARM64Reg byteOffset
      |> bind (fun offsetReg ->
          lirRegToARM64Reg value
          |> Result.map (fun valueReg ->
              let tempReg =
                if
                  ptrReg = Symbolic.X15 || offsetReg = Symbolic.X15
                  || valueReg = Symbolic.X15
                then Symbolic.X14
                else Symbolic.X15
              in
              [
                Symbolic.ADD_reg (tempReg, ptrReg, offsetReg);
                Symbolic.STRB_reg (valueReg, tempReg);
              ])))
