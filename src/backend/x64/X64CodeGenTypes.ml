(*
   X64CodeGenTypes.ml - Define target code generation options, contexts, and planning facts.
*)
[@@@warning "-4"]

open X64Operands

(*
   Function context for instructions that need stack frame info (TailCall, etc.)
*)
type funcCtx = {
  functionName : string;
  stackSize : int;
  usedCalleeSaved : LIR.physReg list;
  enableLeakCheck : bool;
  recordRegistry : LIR.recordRegistry;
  sumShapeRegistry : MemoryModel.rcSumShapeRegistry;
  functionNames : string FunctionIdMap.t;
}

let functionName (ctx : funcCtx) functionId =
  match FunctionIdMap.tryFind functionId ctx.functionNames with
  | Some name -> name
  | None ->
      Crash.crash
        (Printf.sprintf
           "x64 code generation: missing function name for identity %s"
           (Printf.sprintf "%Lu" (AST.functionIdValue functionId)))

let rcSumShapeRegistryFromVariantRegistry variantRegistry =
  StringOrder.Map.map
    (fun (typeVariants : LIR.typeVariants) ->
      let sorted =
        List.stable_sort
          (fun (left : LIR.variantInfo) (right : LIR.variantInfo) ->
            Int.compare left.LIR.tag right.LIR.tag)
          typeVariants.LIR.variants
      in
      {
        MemoryModel.typeParams = typeVariants.LIR.typeParams;
        payloads =
          List.map
            (fun (variant : LIR.variantInfo) ->
              (variant.LIR.tag, variant.LIR.payload))
            sorted;
        unaryPayloadTags =
          MemoryModel.IntSet.of_list
            (List.filter_map
               (fun (variant : LIR.variantInfo) ->
                 if variant.LIR.fieldCount = 1 then Some variant.LIR.tag
                 else None)
               typeVariants.LIR.variants);
      })
    variantRegistry

(*
   Leak Counter (data label _leak_count in ELF data section)
   Increment the leak counter (called on every heap allocation)
   LEA R11, [RIP + _leak_count]; INC [R11]
*)
let genLeakCounterInc (ctx : funcCtx) =
  if not ctx.enableLeakCheck then []
  else
    [
      X86_64.PUSH scratch;
      X86_64.PUSH X86_64.RCX;
      X86_64.LEA_rip (scratch, "_leak_count");
      X86_64.MOV_load (X86_64.RCX, scratch, 0l);
      X86_64.ADD_imm (X86_64.RCX, 1l);
      X86_64.MOV_store (scratch, 0l, X86_64.RCX);
      X86_64.POP X86_64.RCX;
      X86_64.POP scratch;
    ]

(*
   Decrement the leak counter (called when refcount hits zero and block is freed)
*)
let genLeakCounterDec (ctx : funcCtx) =
  if not ctx.enableLeakCheck then []
  else
    [
      X86_64.PUSH scratch;
      X86_64.PUSH X86_64.RCX;
      X86_64.LEA_rip (scratch, "_leak_count");
      X86_64.MOV_load (X86_64.RCX, scratch, 0l);
      X86_64.SUB_imm (X86_64.RCX, 1l);
      X86_64.MOV_store (scratch, 0l, X86_64.RCX);
      X86_64.POP X86_64.RCX;
      X86_64.POP scratch;
    ]

(*
   Reverse bytes at [RSP..RSP+RCX-1] in place
*)
let genReverseBytes () =
  let loopLabel = freshLabel "rev_loop" in
  let doneLabel = freshLabel "rev_done" in
  [
    X86_64.MOV_reg (X86_64.RDI, X86_64.RSP);
    X86_64.MOV_reg (X86_64.RSI, X86_64.RSP);
    X86_64.ADD_reg (X86_64.RSI, X86_64.RCX);
    X86_64.SUB_imm (X86_64.RSI, 1l);
    X86_64.Label loopLabel;
    X86_64.CMP_reg (X86_64.RDI, X86_64.RSI);
    X86_64.Jcc (X86_64.GE, doneLabel);
    X86_64.MOV_load_byte (X86_64.RAX, X86_64.RDI, 0l);
    X86_64.MOV_load_byte (X86_64.RDX, X86_64.RSI, 0l);
    X86_64.MOV_store_byte (X86_64.RDI, 0l, X86_64.RDX);
    X86_64.MOV_store_byte (X86_64.RSI, 0l, X86_64.RAX);
    X86_64.ADD_imm (X86_64.RDI, 1l);
    X86_64.SUB_imm (X86_64.RSI, 1l);
    X86_64.JMP loopLabel;
    X86_64.Label doneLabel;
  ]

(*
   Print an int64 value to stderr followed by newline
*)
let genPrintInt64ToStderr srcReg =
  let loopLabel = freshLabel "print_i64_stderr_loop" in
  let writeLabel = freshLabel "print_i64_stderr_write" in
  [
    X86_64.SUB_imm (X86_64.RSP, 24l);
    X86_64.MOV_imm32 (X86_64.RCX, 0l);
    X86_64.TEST_reg (srcReg, srcReg);
    X86_64.Jcc (X86_64.NE, loopLabel);
    X86_64.MOV_imm32 (scratch, 48l);
    X86_64.MOV_store_byte (X86_64.RSP, 0l, scratch);
    X86_64.MOV_imm32 (X86_64.RCX, 1l);
    X86_64.JMP writeLabel;
    X86_64.Label loopLabel;
    X86_64.TEST_reg (srcReg, srcReg);
    X86_64.Jcc (X86_64.EQ, writeLabel);
  ]
  @ loadImm64 scratch 10L
  @ [
      X86_64.PUSH X86_64.RDX;
      X86_64.MOV_reg (X86_64.RAX, srcReg);
      X86_64.XOR_reg (X86_64.RDX, X86_64.RDX);
      X86_64.IDIV scratch;
      X86_64.ADD_imm (X86_64.RDX, 48l);
      X86_64.MOV_reg (scratch, X86_64.RSP);
      X86_64.ADD_imm (scratch, 8l);
      X86_64.ADD_reg (scratch, X86_64.RCX);
      X86_64.MOV_store_byte (scratch, 0l, X86_64.RDX);
      X86_64.ADD_imm (X86_64.RCX, 1l);
      X86_64.MOV_reg (srcReg, X86_64.RAX);
      X86_64.POP X86_64.RDX;
      X86_64.JMP loopLabel;
      X86_64.Label writeLabel;
    ]
  @ genReverseBytes ()
  @ [
      X86_64.MOV_imm32 (X86_64.RDI, 2l);
      X86_64.MOV_reg (X86_64.RSI, X86_64.RSP);
      X86_64.MOV_reg (scratch, X86_64.RSP);
      X86_64.ADD_reg (scratch, X86_64.RCX);
      X86_64.MOV_imm32 (X86_64.RAX, 10l);
      X86_64.MOV_store_byte (scratch, 0l, X86_64.RAX);
      X86_64.LEA (X86_64.RDX, X86_64.RCX, 1l);
    ]
  @ loadImm64 X86_64.RAX (Int64.of_int syscalls.Platform.write)
  @ [ X86_64.SYSCALL; X86_64.ADD_imm (X86_64.RSP, 24l) ]

(*
   Generate leak check report at exit: if _leak_count > 0, print "leaks: N\n" to stderr
   "leaks: " little-endian
*)
let genLeakCheckReport () =
  let noLeaksLabel = freshLabel "no_leaks" in
  [
    X86_64.LEA_rip (scratch, "_leak_count");
    X86_64.MOV_load (X86_64.RAX, scratch, 0l);
    X86_64.TEST_reg (X86_64.RAX, X86_64.RAX);
    X86_64.Jcc (X86_64.EQ, noLeaksLabel);
    X86_64.PUSH X86_64.RAX;
    X86_64.SUB_imm (X86_64.RSP, 8l);
  ]
  @ loadImm64 scratch 0x203A736B61656CL
  @ [
      X86_64.MOV_store (X86_64.RSP, 0l, scratch);
      X86_64.MOV_imm32 (X86_64.RDI, 2l);
      X86_64.MOV_reg (X86_64.RSI, X86_64.RSP);
      X86_64.MOV_imm32 (X86_64.RDX, 7l);
    ]
  @ loadImm64 X86_64.RAX (Int64.of_int syscalls.Platform.write)
  @ [ X86_64.SYSCALL; X86_64.ADD_imm (X86_64.RSP, 8l); X86_64.POP X86_64.RAX ]
  @ genPrintInt64ToStderr X86_64.RAX
  @ [ X86_64.Label noLeaksLabel ]
