// 2.4_ANF_Inlining.fs - ANF Function Inlining Pass
//
// Inlines small, non-recursive functions at their call sites to eliminate
// function call overhead.
//
// Heuristics:
// - MaxFunctionSize: Only inline functions with <= N TempIds in body
// - MaxInlineDepth: Limit recursive inlining to prevent code explosion
// - Skip recursive functions (direct and mutual recursion via SCC detection)
// - Skip functions with closures (complex runtime behavior)
// - Skip tail calls (preserve TCO optimization)
//
// Mutual recursion detection uses Kosaraju's algorithm to find strongly
// connected components (SCCs) in the call graph. Any function in an SCC
// of size > 1, or that calls itself, is considered recursive.
//
// Literal arguments
// -----------------
// We inline calls even when arguments are literals by binding each literal to a
// fresh TempId before inlining. This preserves ANF shape and avoids re-evaluating
// literal expressions while allowing more helpers to inline.

module ANF_Inlining

open ANF

/// Inlining configuration
type InliningConfig = {
    /// Maximum function body size (in TempIds) to inline
    MaxFunctionSize: int
    /// Maximum depth of recursive inlining
    MaxInlineDepth: int
    /// Maximum external stdlib wrapper calls to inline in one caller body
    MaxExternalInlineSites: int
}

/// Default inlining configuration
let defaultConfig = {
    MaxFunctionSize = 20
    MaxInlineDepth = 3
    MaxExternalInlineSites = 8
}

/// Information about a function for inlining decisions
type FunctionInfo = {
    Func: Function
    Calls: Set<string>  // Functions called by the body
    Size: int  // Count of TempIds (Let bindings) in body
    IsRecursive: bool  // Calls itself directly
    HasClosures: bool  // Contains ClosureAlloc or ClosureCall
    HasTailCalls: bool  // Contains TailCall or ClosureTailCall
    IsExternal: bool  // Body is available only as an inline candidate
}

// ============================================================================
// Phase 1: Analysis - Build function info map
// ============================================================================

/// Properties used by call-graph construction and inlining eligibility.
/// Collecting them together keeps function analysis to one ANF traversal.
type private FunctionAnalysis = {
    Calls: Set<string>
    Size: int
    MaxTempId: int
    HasClosures: bool
    HasTailCalls: bool
}

let private emptyAnalysis = {
    Calls = Set.empty
    Size = 0
    MaxTempId = 0
    HasClosures = false
    HasTailCalls = false
}

let rec private analyzeExpr (expr: AExpr) : FunctionAnalysis =
    match expr with
    | Let (TempId tempId, cexpr, body) ->
        let bodyAnalysis = analyzeExpr body
        let letAnalysis =
            { bodyAnalysis with
                Size = bodyAnalysis.Size + 1
                MaxTempId = max tempId bodyAnalysis.MaxTempId }
        match cexpr with
        | Call (name, _)
        | BorrowedCall (name, _) ->
            { letAnalysis with Calls = Set.add name letAnalysis.Calls }
        | TailCall (name, _) ->
            { letAnalysis with
                Calls = Set.add name letAnalysis.Calls
                HasTailCalls = true }
        | ClosureTailCall _ ->
            { letAnalysis with
                HasClosures = true
                HasTailCalls = true }
        | IndirectTailCall _ ->
            { letAnalysis with HasTailCalls = true }
        | ClosureAlloc _
        | ClosureCall _ ->
            { letAnalysis with HasClosures = true }
        | _ -> letAnalysis
    | Return (Var (TempId tempId)) ->
        { emptyAnalysis with MaxTempId = tempId }
    | Return _ ->
        emptyAnalysis
    | If (condition, thenBranch, elseBranch) ->
        let thenAnalysis = analyzeExpr thenBranch
        let elseAnalysis = analyzeExpr elseBranch
        let conditionMaxTempId =
            match condition with
            | Var (TempId tempId) -> tempId
            | _ -> 0
        {
            Calls = Set.union thenAnalysis.Calls elseAnalysis.Calls
            Size = thenAnalysis.Size + elseAnalysis.Size
            MaxTempId =
                max
                    conditionMaxTempId
                    (max thenAnalysis.MaxTempId elseAnalysis.MaxTempId)
            HasClosures = thenAnalysis.HasClosures || elseAnalysis.HasClosures
            HasTailCalls = thenAnalysis.HasTailCalls || elseAnalysis.HasTailCalls
        }

// ============================================================================
// Mutual Recursion Detection via SCC (Strongly Connected Components)
// Uses Kosaraju's algorithm to find SCCs in the call graph
// ============================================================================

/// Build reverse call graph: Map<callee, Set<callers>>
let buildReverseCallGraph (callGraph: Map<string, Set<string>>) : Map<string, Set<string>> =
    callGraph
    |> Map.fold (fun acc caller callees ->
        callees
        |> Set.fold (fun acc' callee ->
            let existing = Map.tryFind callee acc' |> Option.defaultValue Set.empty
            Map.add callee (Set.add caller existing) acc'
        ) acc
    ) Map.empty

/// DFS to compute finish order (for Kosaraju's algorithm)
let rec dfsFinishOrder (graph: Map<string, Set<string>>) (node: string)
                       (visited: Set<string>) (order: string list)
    : Set<string> * string list =
    if Set.contains node visited then
        (visited, order)
    else
        let visited' = Set.add node visited
        let neighbors = Map.tryFind node graph |> Option.defaultValue Set.empty
        let (visited'', order') =
            neighbors
            |> Set.fold (fun (v, o) neighbor ->
                dfsFinishOrder graph neighbor v o
            ) (visited', order)
        (visited'', node :: order')

/// DFS to collect SCC members
let rec dfsCollectSCC (graph: Map<string, Set<string>>) (node: string)
                      (visited: Set<string>) (scc: Set<string>)
    : Set<string> * Set<string> =
    if Set.contains node visited then
        (visited, scc)
    else
        let visited' = Set.add node visited
        let scc' = Set.add node scc
        let neighbors = Map.tryFind node graph |> Option.defaultValue Set.empty
        neighbors
        |> Set.fold (fun (v, c) neighbor ->
            dfsCollectSCC graph neighbor v c
        ) (visited', scc')

/// Find all SCCs using Kosaraju's algorithm
/// Returns list of SCCs, where each SCC is a Set of function names
let findSCCs
    (funcNames: Set<string>)
    (callGraph: Map<string, Set<string>>)
    : Set<string> list =
    let reverseGraph = buildReverseCallGraph callGraph

    // Step 1: DFS on original graph to get finish order
    let (_, finishOrder) =
        funcNames
        |> Set.fold (fun (visited, order) name ->
            dfsFinishOrder callGraph name visited order
        ) (Set.empty, [])

    // Step 2: DFS on reverse graph in reverse finish order to find SCCs
    let (_, sccs) =
        finishOrder
        |> List.fold (fun (visited, components) name ->
            if Set.contains name visited then
                (visited, components)
            else
                let (visited', scc) = dfsCollectSCC reverseGraph name visited Set.empty
                (visited', scc :: components)
        ) (Set.empty, [])

    sccs

/// Find all functions involved in mutual recursion (in SCCs of size > 1)
/// or direct self-recursion (calls itself)
let findRecursiveFunctions
    (funcs: Function list)
    (callGraph: Map<string, Set<string>>)
    : Set<string> =
    let funcNames = funcs |> List.map (fun f -> f.Name) |> Set.ofList
    let sccs = findSCCs funcNames callGraph

    // Functions in SCCs of size > 1 (mutual recursion)
    let mutuallyRecursive =
        sccs
        |> List.filter (fun scc -> Set.count scc > 1)
        |> List.fold Set.union Set.empty

    // Functions that call themselves (direct recursion)
    let directlyRecursive =
        funcs
        |> List.filter (fun f ->
            let calls = Map.tryFind f.Name callGraph |> Option.defaultValue Set.empty
            Set.contains f.Name calls
        )
        |> List.map (fun f -> f.Name)
        |> Set.ofList

    Set.union mutuallyRecursive directlyRecursive

/// Build function info for a single function
let private buildFunctionInfo
    (recursiveFuncs: Set<string>)
    (func: Function)
    (analysis: FunctionAnalysis)
    : FunctionInfo =
    {
        Func = func
        Calls = analysis.Calls
        Size = analysis.Size
        IsRecursive = Set.contains func.Name recursiveFuncs
        HasClosures = analysis.HasClosures
        HasTailCalls = analysis.HasTailCalls
        IsExternal = false
    }

/// Analyze all functions once, building the inlining map while also finding the
/// highest TempId needed to initialize the inliner's fresh-variable generator.
let private buildFunctionInfoMapAndMaxTempId
    (funcs: Function list)
    : Map<string, FunctionInfo> * int =
    let analyzedFuncs = funcs |> List.map (fun func -> (func, analyzeExpr func.Body))
    let callGraph =
        analyzedFuncs
        |> List.map (fun (func, analysis) -> (func.Name, analysis.Calls))
        |> Map.ofList
    let recursiveFuncs = findRecursiveFunctions funcs callGraph
    let infoMap =
        analyzedFuncs
        |> List.map (fun (func, analysis) ->
            (func.Name, buildFunctionInfo recursiveFuncs func analysis))
        |> Map.ofList
    let maxTempId =
        analyzedFuncs
        |> List.fold (fun programMaxTempId (func, analysis) ->
            let functionMaxTempId =
                func.TypedParams
                |> List.fold (fun currentMax param ->
                    let (TempId tempId) = param.Id
                    max currentMax tempId) analysis.MaxTempId
            max programMaxTempId functionMaxTempId
        ) 0
    (infoMap, maxTempId)

/// Build function info map for all functions
let buildFunctionInfoMap (funcs: Function list) : Map<string, FunctionInfo> =
    buildFunctionInfoMapAndMaxTempId funcs |> fst

// ============================================================================
// Phase 2: TempId Renaming - Avoid variable conflicts when inlining
// ============================================================================

/// Rename an atom (substitute TempIds)
let renameAtom (mapping: Map<TempId, TempId>) (atom: Atom) : Atom =
    match atom with
    | Var tid ->
        match Map.tryFind tid mapping with
        | Some newTid -> Var newTid
        | None -> atom  // External reference, keep as-is
    | _ -> atom

/// Rename all TempIds in a CExpr
let renameCExpr (mapping: Map<TempId, TempId>) (cexpr: CExpr) : CExpr =
    let r = renameAtom mapping
    match cexpr with
    | Atom a -> Atom (r a)
    | TypedAtom (a, t) -> TypedAtom (r a, t)
    | Prim (op, left, right) -> Prim (op, r left, r right)
    | UnaryPrim (op, src) -> UnaryPrim (op, r src)
    | IfValue (cond, thenVal, elseVal) -> IfValue (r cond, r thenVal, r elseVal)
    | Call (name, args) -> Call (name, List.map r args)
    | BorrowedCall (name, args) -> BorrowedCall (name, List.map r args)
    | TailCall (name, args) -> TailCall (name, List.map r args)
    | IndirectCall (func, args) -> IndirectCall (r func, List.map r args)
    | IndirectTailCall (func, args) -> IndirectTailCall (r func, List.map r args)
    | ClosureAlloc (name, captures) -> ClosureAlloc (name, List.map r captures)
    | ClosureCall (closure, args) -> ClosureCall (r closure, List.map r args)
    | ClosureTailCall (closure, args) -> ClosureTailCall (r closure, List.map r args)
    | TupleAlloc elems -> TupleAlloc (List.map r elems)
    | TupleGet (tuple, idx) -> TupleGet (r tuple, idx)
    | StringConcat (left, right) -> StringConcat (r left, r right)
    | RefCountInc (a, size, kind, sourceType) -> RefCountInc (r a, size, kind, sourceType)
    | RefCountDec (a, size, kind, sourceType) -> RefCountDec (r a, size, kind, sourceType)
    | Print (a, t) -> Print (r a, t)
    | FileReadText path -> FileReadText (r path)
    | FileExists path -> FileExists (r path)
    | FileWriteText (path, content) -> FileWriteText (r path, r content)
    | FileAppendText (path, content) -> FileAppendText (r path, r content)
    | FileDelete path -> FileDelete (r path)
    | FileSetExecutable path -> FileSetExecutable (r path)
    | FileWriteFromPtr (path, ptr, len) -> FileWriteFromPtr (r path, r ptr, r len)
    | FloatSqrt a -> FloatSqrt (r a)
    | FloatAbs a -> FloatAbs (r a)
    | FloatNeg a -> FloatNeg (r a)
    | Int64ToFloat a -> Int64ToFloat (r a)
    | FloatToInt64 a -> FloatToInt64 (r a)
    | FloatToBits a -> FloatToBits (r a)
    | RawAlloc numBytes -> RawAlloc (r numBytes)
    | RawFree ptr -> RawFree (r ptr)
    | RawGet (ptr, offset, valueType) -> RawGet (r ptr, r offset, valueType)
    | RawGetByte (ptr, offset) -> RawGetByte (r ptr, r offset)
    | RawWriteWord (ptr, offset, value) -> RawWriteWord (r ptr, r offset, r value)
    | RawWriteByte (ptr, offset, value) -> RawWriteByte (r ptr, r offset, r value)
    | RawSlotInit (ptr, offset, value, valueType) -> RawSlotInit (r ptr, r offset, r value, valueType)
    | StringToRawPtr value -> StringToRawPtr (r value)
    | RawPtrToString ptr -> RawPtrToString (r ptr)
    | BytesToRawPtr value -> BytesToRawPtr (r value)
    | RawPtrToBytes ptr -> RawPtrToBytes (r ptr)
    | DictToRawPtr dict -> DictToRawPtr (r dict)
    | RawPtrToDict (ptr, tag, dictType) -> RawPtrToDict (r ptr, r tag, dictType)
    | ListToRawPtr list -> ListToRawPtr (r list)
    | RawPtrToList (ptr, tag, listType) -> RawPtrToList (r ptr, r tag, listType)
    | RefCountIncString a -> RefCountIncString (r a)
    | RefCountDecString a -> RefCountDecString (r a)
    | RefCountIncBytes a -> RefCountIncBytes (r a)
    | RefCountDecBytes a -> RefCountDecBytes (r a)
    | RandomInt64 -> RandomInt64
    | DateNow -> DateNow
    | FloatToString a -> FloatToString (r a)
    | RuntimeError message -> RuntimeError message

/// Rename all TempIds in an expression, allocating fresh TempIds
let rec renameExpr (mapping: Map<TempId, TempId>) (varGen: VarGen) (expr: AExpr)
    : AExpr * VarGen =
    match expr with
    | Let (tid, cexpr, body) ->
        // Allocate fresh TempId for this binding
        let (newTid, varGen') = freshVar varGen
        let mapping' = Map.add tid newTid mapping
        // Rename the CExpr (uses old mapping for references)
        let cexpr' = renameCExpr mapping cexpr
        // Rename the body (uses new mapping including this binding)
        let (body', varGen'') = renameExpr mapping' varGen' body
        (Let (newTid, cexpr', body'), varGen'')
    | Return atom ->
        (Return (renameAtom mapping atom), varGen)
    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGen') = renameExpr mapping varGen thenBranch
        let (elseBranch', varGen'') = renameExpr mapping varGen' elseBranch
        (If (renameAtom mapping cond, thenBranch', elseBranch'), varGen'')

// ============================================================================
// Phase 3: Inlining - Substitute function calls with bodies
// ============================================================================

/// Check if a function should be inlined
let shouldInline (info: FunctionInfo) (config: InliningConfig) (depth: int) : bool =
    info.Size <= config.MaxFunctionSize
    && not info.IsRecursive
    && not info.HasClosures
    && not info.HasTailCalls
    && depth < config.MaxInlineDepth

let private isSimpleExternalCExpr (cexpr: CExpr) : bool =
    match cexpr with
    | Atom _
    | TypedAtom _
    | Prim _
    | UnaryPrim _
    | IfValue _
    | TupleGet _
    | StringConcat _
    | FloatSqrt _
    | FloatAbs _
    | FloatNeg _
    | Int64ToFloat _
    | FloatToInt64 _
    | FloatToBits _
    | FloatToString _ -> true
    | _ -> false

let rec private isSimpleExternalExpr (expr: AExpr) : bool =
    match expr with
    | Let (_, cexpr, body) ->
        isSimpleExternalCExpr cexpr && isSimpleExternalExpr body
    | Return _ -> true
    | If _ -> false

let rec private countCallsToNames (names: Set<string>) (expr: AExpr) : int =
    match expr with
    | Let (_, Call (name, _), body)
    | Let (_, BorrowedCall (name, _), body) ->
        (if Set.contains name names then 1 else 0) + countCallsToNames names body
    | Let (_, _, body) ->
        countCallsToNames names body
    | Return _ -> 0
    | If (_, thenBranch, elseBranch) ->
        countCallsToNames names thenBranch + countCallsToNames names elseBranch

let private shouldUseExternalCandidate (info: FunctionInfo) (config: InliningConfig) : bool =
    shouldInline info config 0
    && Set.isEmpty info.Calls
    && isSimpleExternalExpr info.Func.Body

let filterExternalCandidates (config: InliningConfig) (functions: Function list) : Function list =
    buildFunctionInfoMap functions
    |> Map.toList
    |> List.choose (fun (_name, info) ->
        if shouldUseExternalCandidate info config then
            Some info.Func
        else
            None)

/// Substitute Return with a continuation expression
/// This replaces `Return atom` with a binding and continues with the rest
let rec substituteReturn (resultTid: TempId) (continuation: AExpr) (expr: AExpr) : AExpr =
    match expr with
    | Return atom ->
        // Replace return with a binding to resultTid, then continue
        Let (resultTid, Atom atom, continuation)
    | Let (tid, cexpr, body) ->
        Let (tid, cexpr, substituteReturn resultTid continuation body)
    | If (cond, thenBranch, elseBranch) ->
        If (cond,
            substituteReturn resultTid continuation thenBranch,
            substituteReturn resultTid continuation elseBranch)

/// Bind literal arguments to fresh TempIds and build parameter mapping
let bindLiteralArgs
    (parameters: TypedParam list)
    (args: Atom list)
    (varGen: VarGen)
    : Map<TempId, TempId> * (TempId * Atom) list * VarGen =
    let rec loop
        (paramList: TypedParam list)
        (args: Atom list)
        (mapping: Map<TempId, TempId>)
        (bindings: (TempId * Atom) list)
        (varGen: VarGen)
        : Map<TempId, TempId> * (TempId * Atom) list * VarGen =
        match paramList, args with
        | [], [] -> (mapping, List.rev bindings, varGen)
        | param :: restParams, arg :: restArgs ->
            match arg with
            | Var tid ->
                loop restParams restArgs (Map.add param.Id tid mapping) bindings varGen
            | _ ->
                let (litTid, varGen') = freshVar varGen
                loop restParams restArgs (Map.add param.Id litTid mapping) ((litTid, arg) :: bindings) varGen'
        | _ ->
            Crash.crash "ANF_Inlining: argument count mismatch when inlining"

    loop parameters args Map.empty [] varGen

/// Inline a function call
/// Returns the renamed function body and updated VarGen. Callers choose whether
/// to optimize the continuation before or after substitution based on the
/// candidate source.
let inlineCallBody (info: FunctionInfo) (args: Atom list) (varGen: VarGen)
    : AExpr * VarGen =
    // Step 1: Bind literal args and build parameter -> TempId mapping
    let (paramMapping, literalBindings, varGen') =
        bindLiteralArgs info.Func.TypedParams args varGen

    // Step 2: Rename all TempIds in the function body to fresh ones
    let (renamedBody, varGen'') = renameExpr paramMapping varGen' info.Func.Body

    // Step 3: Insert literal bindings in front of the body
    let bodyWithLiteralBindings =
        List.foldBack (fun (tid, atom) acc -> Let (tid, Atom atom, acc)) literalBindings renamedBody

    (bodyWithLiteralBindings, varGen'')

/// Recursively inline calls in an expression
let rec inlineInExpr (funcs: Map<string, FunctionInfo>) (config: InliningConfig)
                     (depth: int) (varGen: VarGen) (expr: AExpr)
    : AExpr * VarGen =
    match expr with
    | Let (tid, Call (funcName, args), body) ->
        // Check if this is a regular call (not tail call) to a user function
        match Map.tryFind funcName funcs with
        | Some info when shouldInline info config depth ->
            if info.IsExternal then
                let (body', varGen') =
                    inlineInExpr funcs config depth varGen body
                let (inlinedBody, varGen'') = inlineCallBody info args varGen'
                let (inlinedBody', varGen''') =
                    inlineInExpr funcs config (depth + 1) varGen'' inlinedBody
                let result = substituteReturn tid body' inlinedBody'
                (result, varGen''')
            else
                let (inlinedBody, varGen') = inlineCallBody info args varGen
                let inlinedExpr = substituteReturn tid body inlinedBody
                let (result, varGen'') =
                    inlineInExpr funcs config (depth + 1) varGen' inlinedExpr
                (result, varGen'')
        | _ ->
            // Don't inline - continue processing body
            let (body', varGen') = inlineInExpr funcs config depth varGen body
            (Let (tid, Call (funcName, args), body'), varGen')

    | Let (tid, cexpr, body) ->
        // Not a call, just process the body
        let (body', varGen') = inlineInExpr funcs config depth varGen body
        (Let (tid, cexpr, body'), varGen')

    | Return atom ->
        (Return atom, varGen)

    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGen') = inlineInExpr funcs config depth varGen thenBranch
        let (elseBranch', varGen'') = inlineInExpr funcs config depth varGen' elseBranch
        (If (cond, thenBranch', elseBranch'), varGen'')

/// Inline in a function body
let inlineInFunction (funcs: Map<string, FunctionInfo>) (config: InliningConfig)
                     (varGen: VarGen) (func: Function)
    : Function * VarGen =
    let (body', varGen') = inlineInExpr funcs config 0 varGen func.Body
    ({ func with Body = body' }, varGen')

// ============================================================================
// Phase 4: Main entry point
// ============================================================================

/// Inline functions in a program, using optional external candidates for calls
/// whose bodies are available but should not be emitted with this program.
let inlineProgramWithExternalCandidates
    (config: InliningConfig)
    (externalCandidates: Function list)
    (program: Program)
    : Program =
    let (Program (funcs, main)) = program

    let (localInfoMap, localMaxTempId) = buildFunctionInfoMapAndMaxTempId funcs
    let mainAnalysis = analyzeExpr main
    let calledNames =
        localInfoMap
        |> Map.fold (fun acc _ info -> Set.union acc info.Calls) mainAnalysis.Calls

    let externalCandidatesCalledByProgram =
        externalCandidates
        |> List.filter (fun func -> Set.contains func.Name calledNames)

    let externalInfoMap =
        buildFunctionInfoMap externalCandidatesCalledByProgram
        |> Map.filter (fun _ info -> shouldUseExternalCandidate info config)
        |> Map.map (fun _ info -> { info with IsExternal = true })
    let funcInfoMap =
        Map.fold (fun acc name info -> Map.add name info acc) localInfoMap externalInfoMap
    let externalNames =
        externalInfoMap |> Map.toList |> List.map fst |> Set.ofList
    let funcsForBody body =
        if countCallsToNames externalNames body <= config.MaxExternalInlineSites then
            funcInfoMap
        else
            localInfoMap

    // Start above every existing TempId, collected during inlining analysis.
    let startVarGen = VarGen (max localMaxTempId mainAnalysis.MaxTempId + 1)

    // Inline in each function (single pass for now)
    let (funcs', varGen') =
        funcs
        |> List.fold (fun (accFuncs, varGen) func ->
            let (func', varGen') = inlineInFunction (funcsForBody func.Body) config varGen func
            (func' :: accFuncs, varGen')
        ) ([], startVarGen)

    // Inline in main expression
    let (main', _) = inlineInExpr (funcsForBody main) config 0 varGen' main

    Program (List.rev funcs', main')

/// Inline functions in a program
let inlineProgram (config: InliningConfig) (program: Program) : Program =
    inlineProgramWithExternalCandidates config [] program

/// Inline functions with default configuration
let inlineProgramDefault (program: Program) : Program =
    inlineProgram defaultConfig program
