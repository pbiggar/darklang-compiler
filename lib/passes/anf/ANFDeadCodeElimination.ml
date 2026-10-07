(*
   Extracts call graph from ANF functions and determines reachability.
   Used for stdlib tree-shaking and coverage without re-compiling stdlib.
*)
(* ANFDeadCodeElimination.fs - ANF-level Dead Code Elimination *)
open ANF
module FS = SpecializationIdentity.FunctionSet
(*
   Extract function names from an atom
*)
let extractFromAtom atom =
    match atom with
    | ANF.FuncRef name -> [name]
    | ANF.UnitLiteral
    | ANF.IntLiteral _
    | ANF.BoolLiteral _
    | ANF.StringLiteral _
    | ANF.FloatLiteral _
    | ANF.Var _ -> []

(* / Extract function names from a list of atoms *)
let extractFromAtoms atoms =
    List.concat_map extractFromAtom atoms

(* / Extract function names from a complex expression *)
let extractFromCExpr cexpr =
    match cexpr with
    | ANF.Call (funcName, args)
    | ANF.BorrowedCall (funcName, args)
    | ANF.TailCall (funcName, args) ->
        funcName :: extractFromAtoms args
    | ANF.ClosureAlloc (funcName, captures) ->
        funcName :: extractFromAtoms captures
    | ANF.IndirectCall (func, args)
    | ANF.IndirectTailCall (func, args)
    | ANF.ClosureCall (func, args)
    | ANF.ClosureTailCall (func, args) ->
        extractFromAtom func @ extractFromAtoms args
    | ANF.Atom atom -> extractFromAtom atom
    | ANF.TypedAtom (atom, _) -> extractFromAtom atom
    | ANF.Prim (_, left, right) ->
        extractFromAtom left @ extractFromAtom right
    | ANF.UnaryPrim (_, atom) -> extractFromAtom atom
    | ANF.IfValue (cond, thenVal, elseVal) ->
        extractFromAtom cond @ extractFromAtom thenVal @ extractFromAtom elseVal
    | ANF.TupleAlloc atoms -> extractFromAtoms atoms
    | ANF.TupleGet (tuple, _) -> extractFromAtom tuple
    | ANF.RecordAlloc (_, fields) -> extractFromAtoms fields
    | ANF.RecordGet (_, record, _) -> extractFromAtom record
    | ANF.RecordClone (_, record, fields)
    | ANF.RecordReuse (_, _, record, fields) ->
        extractFromAtom record @ extractFromAtoms fields
    | ANF.StringConcat (first, second, remaining) ->
        extractFromAtoms (first :: second :: remaining)
    | ANF.CanonicalBufferEq (_, left, right) ->
        extractFromAtom left @ extractFromAtom right
    | ANF.CliNative (_, args) -> extractFromAtoms args
    | ANF.RefCountInc (atom, _, _, _) -> extractFromAtom atom
    | ANF.RefCountDec (atom, _, _, _) -> extractFromAtom atom
    | ANF.Print (atom, _) -> extractFromAtom atom
    | ANF.StdoutWrite (atom, _) -> extractFromAtom atom
    | ANF.StdinReadLine -> []
    | ANF.FileReadBlob path -> extractFromAtom path
    | ANF.FileExists path -> extractFromAtom path
    | ANF.FileWriteBlob (path, content) ->
        extractFromAtom path @ extractFromAtom content
    | ANF.FileAppendText (path, content) ->
        extractFromAtom path @ extractFromAtom content
    | ANF.FileDelete path -> extractFromAtom path
    | ANF.FileCreateDirectory path -> extractFromAtom path
    | ANF.FileSetExecutable path -> extractFromAtom path
    | ANF.FileWriteFromPtr (path, ptr, length) ->
        extractFromAtom path @ extractFromAtom ptr @ extractFromAtom length
    | ANF.FloatSqrt atom -> extractFromAtom atom
    | ANF.FloatAbs atom -> extractFromAtom atom
    | ANF.FloatNeg atom -> extractFromAtom atom
    | ANF.Int64ToFloat atom -> extractFromAtom atom
    | ANF.FloatToInt64 atom -> extractFromAtom atom
    | ANF.FloatToBits atom -> extractFromAtom atom
    | ANF.RawAlloc numBytes -> extractFromAtom numBytes
    | ANF.MappedAlloc numBytes -> extractFromAtom numBytes
    | ANF.RawFree ptr -> extractFromAtom ptr
    | ANF.MappedFree ptr -> extractFromAtom ptr
    | ANF.RawGet (ptr, offset, _) ->
        extractFromAtom ptr @ extractFromAtom offset
    | ANF.RawTake (ptr, offset, _) ->
        extractFromAtom ptr @ extractFromAtom offset
    | ANF.RawGetByte (ptr, offset) ->
        extractFromAtom ptr @ extractFromAtom offset
    | ANF.RawWriteWord (ptr, offset, value) ->
        extractFromAtom ptr @ extractFromAtom offset @ extractFromAtom value
    | ANF.RawWriteByte (ptr, offset, value) ->
        extractFromAtom ptr @ extractFromAtom offset @ extractFromAtom value
    | ANF.RawSlotInit (ptr, offset, value, _) ->
        extractFromAtom ptr @ extractFromAtom offset @ extractFromAtom value
    | ANF.StringToRawPtr value -> extractFromAtom value
    | ANF.RawPtrToString ptr -> extractFromAtom ptr
    | ANF.BlobToRawPtr value -> extractFromAtom value
    | ANF.RawPtrToBlob ptr -> extractFromAtom ptr
    | ANF.RawPtrToInt128 ptr -> extractFromAtom ptr
    | ANF.RawPtrToUInt128 ptr -> extractFromAtom ptr
    | ANF.DictToRawPtr dict -> extractFromAtom dict
    | ANF.RawPtrToDict (ptr, tag, _) ->
        extractFromAtom ptr @ extractFromAtom tag
    | ANF.ListToRawPtr list -> extractFromAtom list
    | ANF.FixedBlockToRawPtr value -> extractFromAtom value
    | ANF.RawPtrToList (ptr, tag, _) ->
        extractFromAtom ptr @ extractFromAtom tag
    | ANF.RefCountIncString atom -> extractFromAtom atom
    | ANF.RefCountDecString atom -> extractFromAtom atom
    | ANF.RefCountIncBlob atom -> extractFromAtom atom
    | ANF.RefCountDecBlob atom -> extractFromAtom atom
    | ANF.RefCountIncInt atom -> extractFromAtom atom
    | ANF.RefCountDecInt atom -> extractFromAtom atom
    | ANF.RandomInt64 -> []  (*  No atoms *)
    | ANF.DateTimeNow -> []      (*  No atoms *)
    | ANF.Sleep delayMs -> extractFromAtom delayMs
    | ANF.FloatToString atom -> extractFromAtom atom
    | ANF.RuntimeError _ -> []  (*  No atoms *)
    | ANF.RuntimeErrorString atom -> extractFromAtom atom

(* / Extract function names from an ANF expression *)
let rec extractFromAExpr aexpr =
    match aexpr with
    | ANF.Let (_, cexpr, body) ->
        extractFromCExpr cexpr @ extractFromAExpr body
    | ANF.Return atom -> extractFromAtom atom
    | ANF.Jump (_, atom) -> extractFromAtom atom
    | ANF.Join (_, continuation, entry) ->
        extractFromAExpr continuation @ extractFromAExpr entry
    | ANF.If (cond, thenBranch, elseBranch) ->
        extractFromAtom cond @ extractFromAExpr thenBranch @ extractFromAExpr elseBranch


(*
   Extract function names called from an ANF function
*)
let getCalledFunctions (func : ANF.functionDef) = FS.of_list (extractFromAExpr func.body)
(*
   Build call graph from list of ANF functions
*)
let buildCallGraph funcs = FunctionIdMap.ofList (List.map (fun (f : ANF.functionDef) -> f.id, getCalledFunctions f) funcs)
(*
   Retain functions reachable from the named roots, preserving input order.
*)
let filterReachableFunctions roots funcs =
 let reachable = CallGraphReachability.findReachable (buildCallGraph funcs) roots in
 List.filter (fun (func : ANF.functionDef) -> FS.mem func.id reachable) funcs
(*
   Get the set of stdlib functions reachable from user functions
   Get all functions called from user code
   Expand to transitive closure
*)
let getReachableStdlib stdlibCallGraph userFuncs =
 let userCalls = FS.of_list (List.concat_map (fun f -> FS.elements (getCalledFunctions f)) userFuncs) in
 CallGraphReachability.findReachable stdlibCallGraph userCalls
