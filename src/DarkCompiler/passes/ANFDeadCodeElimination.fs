// ANFDeadCodeElimination.fs - ANF-level Dead Code Elimination
//
// Extracts call graph from ANF functions and determines reachability.
// Used for stdlib tree-shaking and coverage without re-compiling stdlib.

module ANFDeadCodeElimination

/// Extract function names from an atom
let private extractFromAtom (atom: ANF.Atom) : string list =
    match atom with
    | ANF.FuncRef name -> [name]
    | ANF.UnitLiteral
    | ANF.IntLiteral _
    | ANF.BoolLiteral _
    | ANF.StringLiteral _
    | ANF.FloatLiteral _
    | ANF.Var _ -> []

/// Extract function names from a list of atoms
let private extractFromAtoms (atoms: ANF.Atom list) : string list =
    atoms |> List.collect extractFromAtom

/// Extract function names from a complex expression
let private extractFromCExpr (cexpr: ANF.CExpr) : string list =
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
    | ANF.StringConcat (left, right) ->
        extractFromAtom left @ extractFromAtom right
    | ANF.CliNative (_, args) -> extractFromAtoms args
    | ANF.RefCountInc (atom, _, _, _) -> extractFromAtom atom
    | ANF.RefCountDec (atom, _, _, _) -> extractFromAtom atom
    | ANF.Print (atom, _) -> extractFromAtom atom
    | ANF.StdoutWrite (atom, _) -> extractFromAtom atom
    | ANF.StdinReadLine -> []
    | ANF.FileReadText path -> extractFromAtom path
    | ANF.FileExists path -> extractFromAtom path
    | ANF.FileWriteText (path, content) ->
        extractFromAtom path @ extractFromAtom content
    | ANF.FileAppendText (path, content) ->
        extractFromAtom path @ extractFromAtom content
    | ANF.FileDelete path -> extractFromAtom path
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
    | ANF.RawFree ptr -> extractFromAtom ptr
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
    | ANF.DictToRawPtr dict -> extractFromAtom dict
    | ANF.RawPtrToDict (ptr, tag, _) ->
        extractFromAtom ptr @ extractFromAtom tag
    | ANF.ListToRawPtr list -> extractFromAtom list
    | ANF.RawPtrToList (ptr, tag, _) ->
        extractFromAtom ptr @ extractFromAtom tag
    | ANF.RefCountIncString atom -> extractFromAtom atom
    | ANF.RefCountDecString atom -> extractFromAtom atom
    | ANF.RefCountIncBlob atom -> extractFromAtom atom
    | ANF.RefCountDecBlob atom -> extractFromAtom atom
    | ANF.RandomInt64 -> []  // No atoms
    | ANF.DateTimeNow -> []      // No atoms
    | ANF.Sleep delayMs -> extractFromAtom delayMs
    | ANF.FloatToString atom -> extractFromAtom atom
    | ANF.RuntimeError _ -> []  // No atoms
    | ANF.RuntimeErrorString atom -> extractFromAtom atom

/// Extract function names from an ANF expression
let rec private extractFromAExpr (aexpr: ANF.AExpr) : string list =
    match aexpr with
    | ANF.Let (_, cexpr, body) ->
        extractFromCExpr cexpr @ extractFromAExpr body
    | ANF.Return atom -> extractFromAtom atom
    | ANF.If (cond, thenBranch, elseBranch) ->
        extractFromAtom cond @ extractFromAExpr thenBranch @ extractFromAExpr elseBranch

/// Extract function names called from an ANF function
let getCalledFunctions (func: ANF.Function) : Set<string> =
    extractFromAExpr func.Body |> Set.ofList

/// Build call graph from list of ANF functions
let buildCallGraph (funcs: ANF.Function list) : Map<string, Set<string>> =
    funcs
    |> List.map (fun f -> f.Name, getCalledFunctions f)
    |> Map.ofList

/// Get the set of stdlib functions reachable from user functions
let getReachableStdlib (stdlibCallGraph: Map<string, Set<string>>)
                       (userFuncs: ANF.Function list) : Set<string> =
    // Get all functions called from user code
    let userCalls =
        userFuncs
        |> List.collect (fun f -> getCalledFunctions f |> Set.toList)
        |> Set.ofList
    // Expand to transitive closure
    CallGraphReachability.findReachable stdlibCallGraph userCalls
