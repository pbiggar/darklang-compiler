(*
   Filters out unused stdlib functions based on call graph reachability.
   This reduces CodeGen work by only processing functions that are actually used.
*)
(* DeadCodeElimination.fs - Dead Code Elimination (Tree Shaking). *)
[@@@warning "-4"]
module FS = SpecializationIdentity.FunctionSet
(*
   Add a function identity referenced by an operand to the current call set.
*)
let addCallFromOperand op calls = match op with LIR.FuncAddr name -> FS.add name calls | _ -> calls
let addCallsFromOperands ops calls = List.fold_left (fun calls op -> addCallFromOperand op calls) calls ops
(*
   Add function identities referenced by one instruction to the current call set.
   Function pointer is in a register - we can't statically determine the target
   Closure pointer is in a register - we can't statically determine the target
*)
let addCallsFromInstr functionIds instr calls = match instr with
    | LIR.Mov (_, src) -> addCallFromOperand src calls
    | LIR.Phi (_, sources, _) -> List.fold_left (fun calls (source, _) -> addCallFromOperand source calls) calls sources
    | LIR.Store _ -> calls
    | LIR.Add (_, _, right)
    | LIR.Sub (_, _, right)
    | LIR.Cmp (_, right) -> addCallFromOperand right calls
    | LIR.Mul _
    | LIR.Sdiv _
    | LIR.Udiv _
    | LIR.Msub _
    | LIR.Madd _
    | LIR.Cset _
    | LIR.Select _
    | LIR.And _
    | LIR.And_imm _
    | LIR.Orr _
    | LIR.Eor _
    | LIR.Lsl _
    | LIR.Lsr _
    | LIR.Asr _
    | LIR.Lsl_imm _
    | LIR.Lsr_imm _
    | LIR.Asr_imm _
    | LIR.Neg _
    | LIR.Mvn _
    | LIR.Sxtb _
    | LIR.Sxth _
    | LIR.Sxtw _
    | LIR.Uxtb _
    | LIR.Uxth _
    | LIR.Uxtw _ -> calls
    | LIR.Call (_, funcName, args) -> addCallsFromOperands args (FS.add funcName calls)
    | LIR.TailCall (funcName, args) -> addCallsFromOperands args (FS.add funcName calls)
    | LIR.IndirectCall (_, _, args) -> addCallsFromOperands args calls
    | LIR.IndirectTailCall (_, args) -> addCallsFromOperands args calls
    | LIR.ClosureAlloc (_, funcName, captures) -> addCallsFromOperands captures (FS.add funcName calls)
    | LIR.ClosureCall (_, _, args) -> addCallsFromOperands args calls
    | LIR.ClosureTailCall (_, args) -> addCallsFromOperands args calls
    | LIR.SaveRegs _
    | LIR.RestoreRegs _ -> calls
    | LIR.ArgMoves moves
    | LIR.TailArgMoves moves -> List.fold_left (fun calls (_, source) -> addCallFromOperand source calls) calls moves
    | LIR.FArgMoves _
    | LIR.PrintInt64 _
    | LIR.PrintUInt64 _
    | LIR.PrintBool _
    | LIR.PrintInt64NoNewline _
    | LIR.PrintUInt64NoNewline _
    | LIR.PrintBoolNoNewline _
    | LIR.PrintFloat _
    | LIR.PrintFloatNoNewline _
    | LIR.PrintString _
    | LIR.StdoutWrite _
    | LIR.StdinReadLine _
    | LIR.RuntimeError _
    | LIR.RuntimeErrorString _
    | LIR.PrintHeapStringNoNewline _
    | LIR.PrintChars _
    | LIR.PrintBlob _
    | LIR.PrintList _
    | LIR.PrintRecord _
    | LIR.Exit
    | LIR.FPhi _
    | LIR.FMov _
    | LIR.FLoad _
    | LIR.FSpillLoad _
    | LIR.FSpillStore _
    | LIR.FAdd _
    | LIR.FSub _
    | LIR.FMul _
    | LIR.FMadd _
    | LIR.FDiv _
    | LIR.FNeg _
    | LIR.FAbs _
    | LIR.FSqrt _
    | LIR.FCmp _
    | LIR.Int64ToFloat _
    | LIR.FloatToInt64 _
    | LIR.FloatToBits _
    | LIR.GpToFp _
    | LIR.FpToGp _
    | LIR.HeapAlloc _
    | LIR.HeapLoad _
    | LIR.RefCountInc _
    | LIR.RefCountDec _
    | LIR.PrintHeapString _
    | LIR.FileWriteFromPtr _
    | LIR.RawAlloc _
    | LIR.MappedAlloc _
    | LIR.RawFree _
    | LIR.MappedFree _
    | LIR.RawGet _
    | LIR.RawGetByte _
    | LIR.RawWriteWord _
    | LIR.RawWriteByte _
    | LIR.RawSlotInit _
    | LIR.RandomInt64 _
    | LIR.DateTimeNow _
    | LIR.Sleep _
    | LIR.FloatToString _
    | LIR.CoverageHit _ -> calls
    | LIR.CliNative (_, _, args) -> addCallsFromOperands args calls
    | LIR.PrintSum (_, variants, _) -> List.fold_left (fun calls (_, _, payloadType) -> match payloadType with
  | Some (AST.TList elemType) -> (match ListDisplay.getDisplayStringFunc elemType with None -> calls | Some funcName -> (match StringOrder.Map.find_opt funcName functionIds with Some id -> FS.add id calls | None -> Crash.crash ("List display helper '" ^ funcName ^ "' has no allocated identity")))
  | _ -> calls) calls variants
    | LIR.HeapStore (_, _, src, _) -> addCallFromOperand src calls
    | LIR.StringConcat (_, first, second, remaining) -> List.fold_left (fun calls operand -> addCallFromOperand operand calls) calls (first :: second :: remaining)
    | LIR.CanonicalBufferEq (_, _, left, right) -> addCallFromOperand right (addCallFromOperand left calls)
    | LIR.LoadFuncAddr (_, funcName) -> FS.add funcName calls
    | LIR.FileReadBlob (_, path)
    | LIR.FileExists (_, path)
    | LIR.FileDelete (_, path)
    | LIR.FileCreateDirectory (_, path)
    | LIR.FileSetExecutable (_, path)
    | LIR.RefCountIncString path
    | LIR.RefCountDecString path
    | LIR.RefCountIncBlob path
    | LIR.RefCountDecBlob path -> addCallFromOperand path calls
    | LIR.RefCountIncInt path
    | LIR.RefCountDecInt path -> addCallFromOperand path calls
    | LIR.FileWriteBlob (_, path, content)
    | LIR.FileAppendText (_, path, content) -> addCallFromOperand content (addCallFromOperand path calls)
(*
   Add every function-call edge in one LIR function to an existing call set.
*)
let addCalledFunctions functionIds (func : LIR.functionDef) calls = LIR.LabelMap.fold (fun _ block calls -> List.fold_left (fun calls instr -> addCallsFromInstr functionIds instr calls) calls block.LIR.instrs) func.LIR.cfg.LIR.blocks calls
(*
   Extract function identities called from a LIR function.
*)
let getCalledFunctions functionIds func = addCalledFunctions functionIds func FS.empty
let requiresListDisplayHelpers (func : LIR.functionDef) = LIR.LabelMap.exists (fun _ block -> List.exists (function
 | LIR.PrintSum (_, variants, _) -> List.exists (fun (_, _, payloadType) -> match payloadType with Some (AST.TList elemType) -> Option.is_some (ListDisplay.getDisplayStringFunc elemType) | _ -> false) variants
 | _ -> false) block.LIR.instrs) func.LIR.cfg.LIR.blocks
(*
   Build call graph from list of functions
*)
let buildCallGraph functionIds funcs = FunctionIdMap.ofList (List.map (fun (func : LIR.functionDef) -> func.LIR.id,getCalledFunctions functionIds func) funcs)
(*
   Compute transitive closure of reachable functions.
*)
let findReachable = CallGraphReachability.findReachable
(*
   Collect the direct calls made by functions already represented in a call graph.
*)
let directCallsFromFunctions callGraph functions = List.fold_left (fun calls (func : LIR.functionDef) -> match FunctionIdMap.tryFind func.LIR.id callGraph with Some functionCalls -> FS.union calls functionCalls | None -> calls) FS.empty functions
(*
   Filter functions to only include those reachable from a precomputed user call graph.
*)
let filterFunctionsWithUserCallGraph callGraph userCallGraph userFuncs stdlibFuncs =
 let userCalls = directCallsFromFunctions userCallGraph userFuncs in
 let reachable = findReachable callGraph userCalls in
 List.filter (fun (func : LIR.functionDef) -> FS.mem func.LIR.id reachable) stdlibFuncs
(*
   Filter functions to only include reachable ones
*)
let filterFunctions callGraph functionIds userFuncs stdlibFuncs = filterFunctionsWithUserCallGraph callGraph (buildCallGraph functionIds userFuncs) userFuncs stdlibFuncs
