(* X64EmitMemory.ml - Emit x64 instructions for memory operations. *)
[@@@warning "-4"]

open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection
open FieldReferenceCounts
open X64ListReferenceCounts
module X = X86_64

let emitHeapAlloc ctx dest sizeBytes =
  let okLabel = freshLabel "heap_ok" in
  Result.map
    (fun destReg ->
      (* Bump allocator with free list reuse + bounds check *)
      let totalSize =
        Int32.logand (Int32.add (Int32.of_int sizeBytes) 15l) (-8l)
      in
      let storeInitialRefcount =
        if destReg = X.RCX then
          [
            X.PUSH scratch;
            X.MOV_imm32 (scratch, 1l);
            X.MOV_store (destReg, Int32.of_int sizeBytes, scratch);
            X.POP scratch;
          ]
        else
          [
            X.PUSH X.RCX;
            X.MOV_imm32 (X.RCX, 1l);
            X.MOV_store (destReg, Int32.of_int sizeBytes, X.RCX);
            X.POP X.RCX;
          ]
      in
      (* Check free list for this size class (if valid) *)
      let freeListPre, freeListPost =
        if sizeBytes >= 0 && sizeBytes < freeListSize then
          let bumpLabel = freshLabel "heap_bump" in
          let freeListDoneLabel = freshLabel "heap_fl_done" in
          let freeListHeadReg = if destReg = X.RCX then scratch else X.RCX in
          let preserveHeadReg =
            if destReg = X.RCX then [] else [ X.PUSH X.RCX ]
          in
          let restoreHeadReg =
            if destReg = X.RCX then [] else [ X.POP X.RCX ]
          in
          ( preserveHeadReg
            @ [
                X.MOV_load
                  (freeListHeadReg, freeListBase, Int32.of_int sizeBytes);
                X.TEST_reg (freeListHeadReg, freeListHeadReg);
                X.Jcc (X.EQ, bumpLabel);
                (* Free list hit: dest = block, update head to next *)
                X.MOV_reg (destReg, freeListHeadReg);
                X.MOV_load (freeListHeadReg, freeListHeadReg, 0l);
                (* next ptr *)
                X.MOV_store
                  (freeListBase, Int32.of_int sizeBytes, freeListHeadReg);
              ]
            @ restoreHeadReg @ storeInitialRefcount
            @ [ X.JMP freeListDoneLabel; X.Label bumpLabel ]
            @ restoreHeadReg,
            [ X.Label freeListDoneLabel ] )
        else ([], [])
      in
      (* Bump allocator path *)
      freeListPre
      @ [ X.MOV_reg (destReg, heapPtr); X.ADD_imm (heapPtr, totalSize) ]
      @ storeInitialRefcount
      (* Bounds check *)
      @ (if destReg = scratch then
           [
             X.PUSH X.RAX;
             X.MOV_reg (X.RAX, heapPtr);
             X.SUB_reg (X.RAX, freeListBase);
             X.CMP_imm (X.RAX, Int64.to_int32 heapMmapSizeBytes);
             X.POP X.RAX;
             X.Jcc (X.LE, okLabel);
           ]
         else
           [
             X.MOV_reg (scratch, heapPtr);
             X.SUB_reg (scratch, freeListBase);
             X.CMP_imm (scratch, Int64.to_int32 heapMmapSizeBytes);
             X.Jcc (X.LE, okLabel);
           ])
      @ genOomJump () @ [ X.Label okLabel ]
      (* A free-list hit jumps to freeListPost, so place the join before
      leak accounting. Both allocation paths create one live root. *)
      @ freeListPost
      @ genLeakCounterInc ctx)
    (resolveReg dest)

let emitHeapStore ctx addr offset src =
  Result.bind (resolveReg addr) (fun addrReg ->
      let offset = Int32.of_int offset in
      let immediate value =
        if addrReg = scratch then
          [ X.PUSH X.RCX ] @ loadImm64 X.RCX value
          @ [ X.MOV_store (addrReg, offset, X.RCX); X.POP X.RCX ]
        else
          loadImm64 scratch value @ [ X.MOV_store (addrReg, offset, scratch) ]
      in
      match src with
      | LIR.Imm value ->
          (* Address is R11 - can't use scratch for the immediate value *)
          Ok (immediate value)
      | LIR.Reg srcReg ->
          Result.map
            (fun srcX86 ->
              (* Both addr and src are R11 - store R11 at [R11 + offset] *)
              if addrReg = scratch && srcX86 = scratch then
                [ X.MOV_store (scratch, offset, scratch) ]
              else if addrReg = scratch then
                [ X.MOV_store (scratch, offset, srcX86) ]
              else [ X.MOV_store (addrReg, offset, srcX86) ])
            (resolveReg srcReg)
      | LIR.FuncAddr funcName ->
          (* Address is R11 - use RCX to hold the function address *)
          let name = functionName ctx funcName in
          Ok
            (if addrReg = scratch then
               [
                 X.PUSH X.RCX;
                 X.LEA_rip (X.RCX, name);
                 X.MOV_store (addrReg, offset, X.RCX);
                 X.POP X.RCX;
               ]
             else
               [
                 X.LEA_rip (scratch, name);
                 X.MOV_store (addrReg, offset, scratch);
               ])
      | LIR.FloatSymbol value ->
          (* Store float bits as 8-byte integer value at the heap offset *)
          Ok (immediate (Int64.bits_of_float value))
      | LIR.StringSymbol value ->
          if addrReg = scratch then
            (* Preserve both the record address in R11 and an unrelated
        live X3 value while R11 receives the literal address. *)
            Ok
              ([ X.PUSH X.RCX; X.PUSH scratch ]
              @ emitStringLiteral scratch value
              @ [
                  X.POP X.RCX; X.MOV_store (X.RCX, offset, scratch); X.POP X.RCX;
                ])
          else
            Ok
              (emitStringLiteral scratch value
              @ [ X.MOV_store (addrReg, offset, scratch) ])
      | LIR.StackSlot stackOffset ->
          let adjOff =
            Int32.of_int
              (X64InstructionContext.adjustStackOffset ctx stackOffset)
          in
          Ok
            (if addrReg = scratch then
               [
                 X.PUSH X.RCX;
                 X.MOV_load (X.RCX, X.RBP, adjOff);
                 X.MOV_store (addrReg, offset, X.RCX);
                 X.POP X.RCX;
               ]
             else
               [
                 X.MOV_load (scratch, X.RBP, adjOff);
                 X.MOV_store (addrReg, offset, scratch);
               ])
      | LIR.FloatImm value ->
          Error
            ("Unsupported HeapStore source: "
            ^ StructuralFormat.format
                (StructuralFormat.Union
                   ( "FloatImm",
                     [ StructuralFormat.Scalar (FloatFormat.structural value) ]
                   ))))

let emitHeapLoad (_ctx : funcCtx) dest addr offset =
  Result.bind (resolveReg dest) (fun destReg ->
      Result.map
        (fun addrReg -> [ X.MOV_load (destReg, addrReg, Int32.of_int offset) ])
        (resolveReg addr))

(* --- Floating-point operations --- *)
let emitMappedAlloc ctx dest numBytes =
  Result.bind (resolveReg dest) (fun destReg ->
      Result.map
        (fun sizeReg ->
          (* Syscall-clobbered registers are protected by LIR caller saves.
     Keep the exact mapping length in a private allocation prefix. *)
          [
            X.CMP_imm (sizeReg, 0l);
            X.Jcc (X.LT, oomHandlerLabel);
            X.MOV_reg (X.RSI, sizeReg);
            X.ADD_imm (X.RSI, 8l);
            X.CMP_imm (X.RSI, 0l);
            X.Jcc (X.LE, oomHandlerLabel);
            X.PUSH X.RSI;
          ]
          @ loadImm64 X.RDI 0L @ loadImm64 X.RDX 3L @ loadImm64 X.R10 0x22L
          @ loadImm64 X.R8 (-1L) @ loadImm64 X.R9 0L
          @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.mmap)
          @ [
              X.SYSCALL;
              X.CMP_imm (X.RAX, 0l);
              X.Jcc (X.LT, oomHandlerLabel);
              X.POP scratch;
              X.MOV_store (X.RAX, 0l, scratch);
              X.LEA (destReg, X.RAX, 8l);
            ]
          @ genLeakCounterInc ctx)
        (resolveReg numBytes))

let emitMappedFree ctx ptr =
  Result.map
    (fun ptrReg ->
      [ X.LEA (X.RDI, ptrReg, -8l); X.MOV_load (X.RSI, X.RDI, 0l) ]
      @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.munmap)
      @ [ X.SYSCALL; X.CMP_imm (X.RAX, 0l); X.Jcc (X.NE, oomHandlerLabel) ]
      @ genLeakCounterDec ctx)
    (resolveReg ptr)

let emitRawAlloc ctx dest numBytes =
  let okLabel = freshLabel "rawalloc_ok" in
  Result.bind (resolveReg dest) (fun destReg ->
      Result.map
        (fun sizeReg ->
          (* --- Free list reuse ---
     Before bump-allocating, check if the free list has a block of the right size class.
     aligned_size = (numBytes + 7) & ~7; payload_class = aligned_size - 8
     If freeList[payload_class] is non-null, pop from free list and skip bump alloc. *)
          let bumpLabel = freshLabel "rawalloc_bump" in
          let doneLabel = freshLabel "rawalloc_done" in
          (* Pick two temp registers that don't conflict with destReg or sizeReg *)
          let available =
            List.filter
              (fun r -> r <> destReg && r <> sizeReg)
              [ X.RAX; X.RCX; X.RDX; X.RDI; X.RSI ]
          in
          let temp1 = List.nth available 0 in
          (* holds aligned_size → payload_class → head → next *)
          let temp2 = List.nth available 1 in
          (* holds &freeList[payload_class] *)
          let freeListCheck =
            [
              X.PUSH temp1;
              X.PUSH temp2;
              (* Compute aligned size *)
              X.MOV_reg (temp1, sizeReg);
              X.ADD_imm (temp1, 7l);
              X.AND_imm (temp1, -8l);
              (* temp1 = aligned_size *)
              (* Need at least 16 bytes (8 payload + 8 refcount) for free list *)
              X.CMP_imm (temp1, 8l);
              X.Jcc (X.LT, bumpLabel);
              X.SUB_imm (temp1, 8l);
              (* temp1 = payload_class *)
              X.CMP_imm (temp1, Int32.of_int maxFreeListPayload);
              X.Jcc (X.GT, bumpLabel);
              (* Compute free list slot address: &freeList[payload_class] *)
              X.MOV_reg (temp2, freeListBase);
              X.ADD_reg (temp2, temp1);
              (* temp2 = &freeList[payload_class] *)
              (* Load free list head *)
              X.MOV_load (temp1, temp2, 0l);
              (* temp1 = head *)
              X.TEST_reg (temp1, temp1);
              X.Jcc (X.EQ, bumpLabel);
              (* Pop from free list: dest = head, freeList[class] = head->next *)
              X.MOV_reg (destReg, temp1);
              (* dest = free block *)
              X.MOV_load (temp1, temp1, 0l);
              (* temp1 = next ptr *)
              X.MOV_store (temp2, 0l, temp1);
              (* update head *)
              X.POP temp2;
              X.POP temp1;
              X.JMP doneLabel;
              X.Label bumpLabel;
              X.POP temp2;
              X.POP temp1;
            ]
          in
          (* --- Bump allocation (existing) --- *)
          let allocInstrs =
            if destReg = sizeReg then
              [
                X.MOV_reg (scratch, sizeReg);
                X.MOV_reg (destReg, heapPtr);
                X.ADD_reg (heapPtr, scratch);
                X.ADD_imm (heapPtr, 7l);
                X.AND_imm (heapPtr, -8l);
              ]
            else
              [
                X.MOV_reg (destReg, heapPtr);
                X.ADD_reg (heapPtr, sizeReg);
                X.ADD_imm (heapPtr, 7l);
                X.AND_imm (heapPtr, -8l);
              ]
          in
          (* Bounds check: heapPtr - freeListBase <= heapMmapSize
     Use RAX temp if dest or size uses scratch (R11) *)
          let useScratch = destReg <> scratch && sizeReg <> scratch in
          let boundsCheck =
            (if useScratch then
               [
                 X.MOV_reg (scratch, heapPtr);
                 X.SUB_reg (scratch, freeListBase);
                 X.CMP_imm (scratch, Int64.to_int32 heapMmapSizeBytes);
                 X.Jcc (X.LE, okLabel);
               ]
             else
               [
                 X.PUSH X.RAX;
                 X.MOV_reg (X.RAX, heapPtr);
                 X.SUB_reg (X.RAX, freeListBase);
                 X.CMP_imm (X.RAX, Int64.to_int32 heapMmapSizeBytes);
                 X.POP X.RAX;
                 X.Jcc (X.LE, okLabel);
               ])
            @ genOomJump () @ [ X.Label okLabel ]
          in
          (* Recycled blocks become live allocations just like bumped
     blocks. Both paths must reach the accounting increment. *)
          freeListCheck @ allocInstrs @ boundsCheck @ [ X.Label doneLabel ]
          @ genLeakCounterInc ctx)
        (resolveReg numBytes))

let emitRawFree ctx ptr =
  Result.map
    (fun ptrReg ->
      [
        X.MOV_load (scratch, freeListBase, 0l);
        X.MOV_store (ptrReg, 0l, scratch);
        X.MOV_store (freeListBase, 0l, ptrReg);
      ]
      @ genLeakCounterDec ctx)
    (resolveReg ptr)

let emitRawGet (_ctx : funcCtx) dest ptr byteOffset =
  Result.bind (resolveReg dest) (fun d ->
      Result.bind (resolveReg ptr) (fun p ->
          Result.map
            (fun o ->
              if o = scratch && p <> scratch then
                (* Offset is R11: MOV scratch,p would clobber offset.
      Swap: compute p + o by loading o first, adding p. *)
                [ X.ADD_reg (scratch, p); X.MOV_load (d, scratch, 0l) ]
              else if p = scratch && o <> scratch then
                (* Ptr is R11: MOV is no-op, just add offset *)
                [ X.ADD_reg (scratch, o); X.MOV_load (d, scratch, 0l) ]
              else if p = scratch && o = scratch then
                (* Both are R11 (same virtual reg): scratch = scratch + scratch *)
                [ X.ADD_reg (scratch, scratch); X.MOV_load (d, scratch, 0l) ]
              else
                [
                  X.MOV_reg (scratch, p);
                  X.ADD_reg (scratch, o);
                  X.MOV_load (d, scratch, 0l);
                ])
            (resolveReg byteOffset)))

let emitRawGetByte (_ctx : funcCtx) dest ptr byteOffset =
  Result.bind (resolveReg dest) (fun d ->
      Result.bind (resolveReg ptr) (fun p ->
          Result.map
            (fun o ->
              if o = scratch && p <> scratch then
                [ X.ADD_reg (scratch, p); X.MOV_load_byte (d, scratch, 0l) ]
              else if p = scratch then
                [ X.ADD_reg (scratch, o); X.MOV_load_byte (d, scratch, 0l) ]
              else
                [
                  X.MOV_reg (scratch, p);
                  X.ADD_reg (scratch, o);
                  X.MOV_load_byte (d, scratch, 0l);
                ])
            (resolveReg byteOffset)))

let writeAt store p o v =
  if v = scratch || o = scratch then
    let tempReg = X.RCX in
    if p = tempReg then
      [
        X.PUSH tempReg;
        X.PUSH scratch;
        X.MOV_load (scratch, X.RSP, 8l);
        X.ADD_reg (scratch, o);
        X.MOV_load (tempReg, X.RSP, 0l);
        store (scratch, 0l, tempReg);
        X.POP scratch;
        X.POP tempReg;
      ]
    else
      [
        X.PUSH tempReg;
        X.MOV_reg (tempReg, v);
        X.MOV_reg (scratch, p);
        X.ADD_reg (scratch, o);
        store (scratch, 0l, tempReg);
        X.POP tempReg;
      ]
  else
    [ X.MOV_reg (scratch, p); X.ADD_reg (scratch, o); store (scratch, 0l, v) ]

let emitRawWriteWord (_ctx : funcCtx) ptr byteOffset value =
  Result.bind (resolveReg ptr) (fun p ->
      Result.bind (resolveReg byteOffset) (fun o ->
          Result.map
            (fun v -> writeAt (fun (a, b, c) -> X.MOV_store (a, b, c)) p o v)
            (resolveReg value)))

let emitRawSlotInit ctx ptr byteOffset value valueType =
  Result.bind (resolveReg ptr) (fun p ->
      Result.bind (resolveReg byteOffset) (fun o ->
          Result.map
            (fun v ->
              (* Raw runtime nodes own every managed value stored in an edge slot.
     RawGet is borrowed, so RawSlotInit is the retain point for copied edges. *)
              let call registers label =
                List.map (fun reg -> X.PUSH reg) registers
                @ [ X.MOV_reg (X.RAX, v); X.CALL label ]
                @ List.map (fun reg -> X.POP reg) (List.rev registers)
              in
              let ownershipInc =
                match
                  slotInitRootRetainTarget ctx.recordRegistry
                    ctx.sumShapeRegistry valueType
                with
                | Some SlotInitListRootRetain ->
                    call
                      [ X.RAX; X.RCX; X.RDX; X.RDI; X.R10 ]
                      listRefCountIncHelperLabel
                | Some SlotInitDictRootRetain ->
                    call
                      [ X.RAX; X.RCX; X.RDX; X.RDI; X.RSI; X.R8; X.R9; X.R10 ]
                      dictRefCountIncHelperLabel
                | Some SlotInitDynamicBufferRetain ->
                    let literalLabel =
                      freshLabel "slotinit_rcinc_dynamic_lit"
                    in
                    [
                      X.PUSH X.RCX;
                      X.PUSH X.RDX;
                      X.PUSH X.R10;
                      X.PUSH scratch;
                      X.MOV_reg (X.R10, v);
                      X.TEST_reg (X.R10, X.R10);
                      X.Jcc (X.EQ, literalLabel);
                    ]
                    @ (match valueType with
                      | AST.TInt ->
                          [
                            X.MOV_reg (X.RDX, X.R10);
                            X.AND_imm (X.RDX, 1l);
                            X.Jcc (X.NE, literalLabel);
                          ]
                      | _ -> [])
                    @ [ X.MOV_load (X.RDX, X.R10, 0l) ]
                    @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL
                    @ [
                        X.CMP_reg (X.RDX, scratch);
                        X.Jcc (X.EQ, literalLabel);
                        X.ADD_imm (X.RDX, 1l);
                        X.MOV_store (X.R10, 0l, X.RDX);
                        X.Label literalLabel;
                        X.POP scratch;
                        X.POP X.R10;
                        X.POP X.RDX;
                        X.POP X.RCX;
                      ]
                | Some SlotInitClosureRootRetain ->
                    call
                      [ X.RAX; X.RCX; X.RDX; X.RDI; X.R10 ]
                      closureRefCountIncHelperLabel
                | Some (SlotInitGenericRootRetain payloadSize) ->
                    genRefCountIncGeneric v payloadSize
                | None -> []
              in
              (* An operand is R11 (scratch) which we need for address computation.
     Use RCX as an extra temp (save/restore if needed).
     ptr is RCX: can't use RCX as temp without saving ptr first.
     Use two pushes: save ptr, save value, compute address, store.
     save ptr (RCX)
     save value/offset (R11)
     Stack: [R11] [RCX] ...
     Compute address: R11 = ptr + offset
     R11 = saved ptr (RCX)
     R11 = ptr + offset
     Get value
     RCX = saved R11 (value)
     [addr] = value
     restore R11
     restore RCX
     save value in temp *)
              let storeInstrs =
                writeAt (fun (a, b, c) -> X.MOV_store (a, b, c)) p o v
              in
              (* Retain before address calculation: storeInstrs uses R11 as
     its address scratch, which may also carry the value. *)
              ownershipInc @ storeInstrs)
            (resolveReg value)))

(*
   RCX = saved value/offset
*)
let emitRawWriteByte (_ctx : funcCtx) ptr byteOffset value =
  Result.bind (resolveReg ptr) (fun p ->
      Result.bind (resolveReg byteOffset) (fun o ->
          Result.map
            (* ptr is RCX: save both before clobbering *)
            (fun v ->
              writeAt (fun (a, b, c) -> X.MOV_store_byte (a, b, c)) p o v)
            (resolveReg value)))
