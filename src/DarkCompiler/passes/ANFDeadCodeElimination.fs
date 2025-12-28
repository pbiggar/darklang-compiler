// ANFDeadCodeElimination.fs - ANF-level Dead Code Elimination
//
// Extracts call graph from ANF functions and determines reachability.
// This enables lazy compilation by identifying which stdlib functions
// are actually called before running expensive passes (MIR, LIR, RegAlloc).

module ANFDeadCodeElimination

/// Extract function names from an atom
let private extractFromAtom (atom: ANF.Atom) : string list =
    match atom with
    | ANF.FuncRef name -> [name]
    | _ -> []

/// Extract function names from a complex expression
let private extractFromCExpr (cexpr: ANF.CExpr) : string list =
    match cexpr with
    | ANF.Call (funcName, args) ->
        funcName :: (args |> List.collect extractFromAtom)
    | ANF.IndirectCall (func, args) ->
        extractFromAtom func @ (args |> List.collect extractFromAtom)
    | ANF.ClosureAlloc (funcName, captures) ->
        funcName :: (captures |> List.collect extractFromAtom)
    | ANF.ClosureCall (closure, args) ->
        extractFromAtom closure @ (args |> List.collect extractFromAtom)
    | ANF.Atom atom -> extractFromAtom atom
    | ANF.Prim (_, left, right) ->
        extractFromAtom left @ extractFromAtom right
    | ANF.UnaryPrim (_, atom) -> extractFromAtom atom
    | ANF.IfValue (cond, thenVal, elseVal) ->
        extractFromAtom cond @ extractFromAtom thenVal @ extractFromAtom elseVal
    | ANF.TupleAlloc atoms -> atoms |> List.collect extractFromAtom
    | ANF.TupleGet (tuple, _) -> extractFromAtom tuple
    | ANF.StringConcat (left, right) ->
        extractFromAtom left @ extractFromAtom right
    | ANF.RefCountInc (atom, _) -> extractFromAtom atom
    | ANF.RefCountDec (atom, _) -> extractFromAtom atom
    | ANF.Print (atom, _) -> extractFromAtom atom
    | ANF.FileReadText path -> extractFromAtom path
    | ANF.FileExists path -> extractFromAtom path
    | ANF.FileWriteText (path, content) ->
        extractFromAtom path @ extractFromAtom content
    | ANF.FileAppendText (path, content) ->
        extractFromAtom path @ extractFromAtom content
    | ANF.FloatSqrt atom -> extractFromAtom atom
    | ANF.FloatAbs atom -> extractFromAtom atom
    | ANF.FloatNeg atom -> extractFromAtom atom
    | ANF.IntToFloat atom -> extractFromAtom atom
    | ANF.FloatToInt atom -> extractFromAtom atom
    | ANF.RawAlloc numBytes -> extractFromAtom numBytes
    | ANF.RawFree ptr -> extractFromAtom ptr
    | ANF.RawGet (ptr, offset) ->
        extractFromAtom ptr @ extractFromAtom offset
    | ANF.RawSet (ptr, offset, value) ->
        extractFromAtom ptr @ extractFromAtom offset @ extractFromAtom value
    | ANF.StringHash str -> extractFromAtom str
    | ANF.StringEq (left, right) ->
        extractFromAtom left @ extractFromAtom right
    | ANF.RefCountIncString atom -> extractFromAtom atom
    | ANF.RefCountDecString atom -> extractFromAtom atom

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

/// Compute transitive closure of reachable functions
let findReachable (callGraph: Map<string, Set<string>>) (roots: Set<string>) : Set<string> =
    let rec visit visited toVisit =
        if Set.isEmpty toVisit then visited
        else
            let name = Set.minElement toVisit
            let toVisit' = Set.remove name toVisit
            if Set.contains name visited then visit visited toVisit'
            else
                let visited' = Set.add name visited
                let calls = Map.tryFind name callGraph |> Option.defaultValue Set.empty
                let toVisit'' = Set.union toVisit' (Set.difference calls visited')
                visit visited' toVisit''
    visit Set.empty roots

/// Get the set of stdlib functions reachable from user functions
let getReachableStdlib (stdlibCallGraph: Map<string, Set<string>>)
                       (userFuncs: ANF.Function list) : Set<string> =
    // Get all functions called from user code
    let userCalls =
        userFuncs
        |> List.collect (fun f -> getCalledFunctions f |> Set.toList)
        |> Set.ofList
    // Expand to transitive closure
    findReachable stdlibCallGraph userCalls
