// InliningCommon.fs - Call eligibility, external candidate analysis, and ANF renaming.
//
// The SSA inliner uses these source-level facts before cloning typed blocks.

module InliningCommon

open MemoryModel

open ANF

/// Inlining configuration
type InliningConfig = {
    /// Maximum function body size (in TempIds) to inline
    MaxFunctionSize: int
    /// Maximum depth of nested inlining
    MaxInlineDepth: int
    /// Maximum external stdlib wrapper calls to inline in one caller body
    MaxExternalInlineSites: int
    /// Maximum statically proven recursive-loop iterations to expand
    MaxBoundedLoopIterations: int
    /// Maximum primitive bindings introduced by one bounded-loop expansion
    MaxBoundedLoopExpansion: int
    /// Maximum body size for a tuple-return call whose every element is projected immediately
    MaxProjectedTupleInlineSize: int
    /// Maximum projected-tuple calls expanded in one caller
    MaxProjectedTupleInlineSites: int
}

/// Default inlining configuration
let defaultConfig = {
    MaxFunctionSize = 20
    MaxInlineDepth = 3
    MaxExternalInlineSites = 8
    MaxBoundedLoopIterations = 8
    MaxBoundedLoopExpansion = 48
    MaxProjectedTupleInlineSize = 64
    MaxProjectedTupleInlineSites = 12
}

/// Information about a function for inlining decisions
type FunctionInfo = {
    Func: Function
    Calls: Set<AST.FunctionId>
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
    Calls: Set<AST.FunctionId>
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
    | Jump (TempId target, atom) ->
        let value = analyzeExpr (Return atom)
        { value with MaxTempId = max target value.MaxTempId; Size = 1 }
    | Join (parameter, continuation, entry) ->
        let body = analyzeExpr continuation
        let entry' = analyzeExpr entry
        let (TempId parameterId) = parameter.Id
        { Calls = Set.union body.Calls entry'.Calls
          Size = 1 + body.Size + entry'.Size
          MaxTempId = max parameterId (max body.MaxTempId entry'.MaxTempId)
          HasClosures = body.HasClosures || entry'.HasClosures
          HasTailCalls = body.HasTailCalls || entry'.HasTailCalls }
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
let buildReverseCallGraph (callGraph: Map<AST.FunctionId, Set<AST.FunctionId>>) : Map<AST.FunctionId, Set<AST.FunctionId>> =
    callGraph
    |> Map.fold (fun acc caller callees ->
        callees
        |> Set.fold (fun acc' callee ->
            let existing = Map.tryFind callee acc' |> Option.defaultValue Set.empty
            Map.add callee (Set.add caller existing) acc'
        ) acc
    ) Map.empty

/// DFS to compute finish order (for Kosaraju's algorithm)
let rec dfsFinishOrder (graph: Map<AST.FunctionId, Set<AST.FunctionId>>) (node: AST.FunctionId)
                       (visited: Set<AST.FunctionId>) (order: AST.FunctionId list)
    : Set<AST.FunctionId> * AST.FunctionId list =
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
let rec dfsCollectSCC (graph: Map<AST.FunctionId, Set<AST.FunctionId>>) (node: AST.FunctionId)
                      (visited: Set<AST.FunctionId>) (scc: Set<AST.FunctionId>)
    : Set<AST.FunctionId> * Set<AST.FunctionId> =
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
    (funcNames: Set<AST.FunctionId>)
    (callGraph: Map<AST.FunctionId, Set<AST.FunctionId>>)
    : Set<AST.FunctionId> list =
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
    (callGraph: Map<AST.FunctionId, Set<AST.FunctionId>>)
    : Set<AST.FunctionId> =
    let funcNames = funcs |> List.map (fun f -> f.Id) |> Set.ofList
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
            let calls = Map.tryFind f.Id callGraph |> Option.defaultValue Set.empty
            Set.contains f.Id calls
        )
        |> List.map (fun f -> f.Id)
        |> Set.ofList

    Set.union mutuallyRecursive directlyRecursive

/// Build function info for a single function
let private buildFunctionInfo
    (recursiveFuncs: Set<AST.FunctionId>)
    (func: Function)
    (analysis: FunctionAnalysis)
    : FunctionInfo =
    {
        Func = func
        Calls = analysis.Calls
        Size = analysis.Size
        IsRecursive = Set.contains func.Id recursiveFuncs
        HasClosures = analysis.HasClosures
        HasTailCalls = analysis.HasTailCalls
        IsExternal = false
    }

/// Analyze all functions once, building the inlining map while also finding the
/// highest TempId needed to initialize the inliner's fresh-variable generator.
let private buildFunctionInfoMapAndMaxTempId
    (funcs: Function list)
    : Map<AST.FunctionId, FunctionInfo> * int =
    let analyzedFuncs = funcs |> List.map (fun func -> (func, analyzeExpr func.Body))
    let callGraph =
        analyzedFuncs
        |> List.map (fun (func, analysis) -> (func.Id, analysis.Calls))
        |> Map.ofList
    let recursiveFuncs = findRecursiveFunctions funcs callGraph
    let infoMap =
        analyzedFuncs
        |> List.map (fun (func, analysis) ->
            (func.Id, buildFunctionInfo recursiveFuncs func analysis))
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
let buildFunctionInfoMap (funcs: Function list) : Map<AST.FunctionId, FunctionInfo> =
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
    | RecordAlloc (descriptor, fields) -> RecordAlloc (descriptor, List.map r fields)
    | RecordGet (descriptor, record, idx) -> RecordGet (descriptor, r record, idx)
    | RecordClone (descriptor, record, fields) ->
        RecordClone (descriptor, r record, List.map r fields)
    | RecordReuse (sourceDescriptor, targetDescriptor, record, fields) ->
        RecordReuse (sourceDescriptor, targetDescriptor, r record, List.map r fields)
    | StringConcat (first, second, remaining) ->
        StringConcat (r first, r second, List.map r remaining)
    | CanonicalBufferEq (kind, left, right) -> CanonicalBufferEq (kind, r left, r right)
    | RefCountInc (a, size, kind, sourceType) -> RefCountInc (r a, size, kind, sourceType)
    | RefCountDec (a, size, kind, sourceType) -> RefCountDec (r a, size, kind, sourceType)
    | Print (a, t) -> Print (r a, t)
    | StdoutWrite (a, appendNewline) -> StdoutWrite (r a, appendNewline)
    | StdinReadLine -> StdinReadLine
    | FileReadBlob path -> FileReadBlob (r path)
    | FileExists path -> FileExists (r path)
    | FileWriteBlob (path, content) -> FileWriteBlob (r path, r content)
    | FileAppendText (path, content) -> FileAppendText (r path, r content)
    | FileDelete path -> FileDelete (r path)
    | FileCreateDirectory path -> FileCreateDirectory (r path)
    | FileSetExecutable path -> FileSetExecutable (r path)
    | FileWriteFromPtr (path, ptr, len) -> FileWriteFromPtr (r path, r ptr, r len)
    | FloatSqrt a -> FloatSqrt (r a)
    | FloatAbs a -> FloatAbs (r a)
    | FloatNeg a -> FloatNeg (r a)
    | Int64ToFloat a -> Int64ToFloat (r a)
    | FloatToInt64 a -> FloatToInt64 (r a)
    | FloatToBits a -> FloatToBits (r a)
    | RawAlloc numBytes -> RawAlloc (r numBytes)
    | MappedAlloc numBytes -> MappedAlloc (r numBytes)
    | RawFree ptr -> RawFree (r ptr)
    | MappedFree ptr -> MappedFree (r ptr)
    | RawGet (ptr, offset, valueType) -> RawGet (r ptr, r offset, valueType)
    | RawTake (ptr, offset, valueType) -> RawTake (r ptr, r offset, valueType)
    | RawGetByte (ptr, offset) -> RawGetByte (r ptr, r offset)
    | RawWriteWord (ptr, offset, value) -> RawWriteWord (r ptr, r offset, r value)
    | RawWriteByte (ptr, offset, value) -> RawWriteByte (r ptr, r offset, r value)
    | RawSlotInit (ptr, offset, value, valueType) -> RawSlotInit (r ptr, r offset, r value, valueType)
    | StringToRawPtr value -> StringToRawPtr (r value)
    | RawPtrToString ptr -> RawPtrToString (r ptr)
    | BlobToRawPtr value -> BlobToRawPtr (r value)
    | RawPtrToBlob ptr -> RawPtrToBlob (r ptr)
    | RawPtrToInt128 ptr -> RawPtrToInt128 (r ptr)
    | RawPtrToUInt128 ptr -> RawPtrToUInt128 (r ptr)
    | DictToRawPtr dict -> DictToRawPtr (r dict)
    | RawPtrToDict (ptr, tag, dictType) -> RawPtrToDict (r ptr, r tag, dictType)
    | ListToRawPtr list -> ListToRawPtr (r list)
    | FixedBlockToRawPtr value -> FixedBlockToRawPtr (r value)
    | RawPtrToList (ptr, tag, listType) -> RawPtrToList (r ptr, r tag, listType)
    | RefCountIncString a -> RefCountIncString (r a)
    | RefCountDecString a -> RefCountDecString (r a)
    | RefCountIncBlob a -> RefCountIncBlob (r a)
    | RefCountDecBlob a -> RefCountDecBlob (r a)
    | RefCountIncInt a -> RefCountIncInt (r a)
    | RefCountDecInt a -> RefCountDecInt (r a)
    | RandomInt64 -> RandomInt64
    | DateTimeNow -> DateTimeNow
    | Sleep delayMs -> Sleep (r delayMs)
    | CliNative (operation, args) -> CliNative (operation, List.map r args)
    | FloatToString a -> FloatToString (r a)
    | RuntimeError message -> RuntimeError message
    | RuntimeErrorString atom -> RuntimeErrorString (r atom)

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
    | Jump (target, atom) ->
        let target' = Map.tryFind target mapping |> Option.defaultValue target
        (Jump (target', renameAtom mapping atom), varGen)
    | Join (parameter, continuation, entry) ->
        let newId, next = freshVar varGen
        let mapping' = Map.add parameter.Id newId mapping
        let body, afterBody = renameExpr mapping' next continuation
        let entry', final = renameExpr mapping' afterBody entry
        (Join ({ parameter with Id = newId }, body, entry'), final)
    | Return atom ->
        (Return (renameAtom mapping atom), varGen)
    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGen') = renameExpr mapping varGen thenBranch
        let (elseBranch', varGen'') = renameExpr mapping varGen' elseBranch
        (If (renameAtom mapping cond, thenBranch', elseBranch'), varGen'')

// ============================================================================
// Eligibility shared by SSA inlining and external candidate selection
// ============================================================================

/// Check if a function should be inlined
let shouldInline (info: FunctionInfo) (config: InliningConfig) (depth: int) : bool =
    info.Size <= config.MaxFunctionSize
    && not (info.Func.Name.StartsWith("Darklang.Stdlib.Json.__"))
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
    | CanonicalBufferEq _
    | FloatSqrt _
    | FloatAbs _
    | FloatNeg _
    | Int64ToFloat _
    | FloatToInt64 _
    | FloatToBits _
    | FloatToString _ -> true
    | _ -> false

let private isScalarRawReadExternalCExpr (cexpr: CExpr) : bool =
    isSimpleExternalCExpr cexpr
    || (match cexpr with
        | RawGet _
        | RawGetByte _
        | StringToRawPtr _ -> true
        | _ -> false)

let rec private isExternalExprWith
    (isAllowed: CExpr -> bool)
    (expr: AExpr)
    : bool =
    match expr with
    | Let (_, cexpr, body) ->
        isAllowed cexpr && isExternalExprWith isAllowed body
    | Return _ -> true
    | Jump _ | Join _ | If _ -> false

let private isSimpleExternalExpr (expr: AExpr) : bool =
    isExternalExprWith isSimpleExternalCExpr expr

let private isScalarRawReadReturnType (typ: AST.SemanticType) : bool =
    match typ with
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TInt
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
    | AST.TBool | AST.TFloat64 | AST.TChar | AST.TDateTime | AST.TUnit -> true
    | AST.TString | AST.TBlob | AST.TNever | AST.TInternalRawPtr
    | AST.TFunction _ | AST.TTuple _ | AST.TRecord _ | AST.TSum _ | AST.TList _
    | AST.TDict _ | AST.TStream _ | AST.TVar _ | AST.TInferenceVar _ -> false

let rec private countCallsToNames (names: Set<AST.FunctionId>) (expr: AExpr) : int =
    match expr with
    | Let (_, Call (name, _), body)
    | Let (_, BorrowedCall (name, _), body) ->
        (if Set.contains name names then 1 else 0) + countCallsToNames names body
    | Let (_, _, body) ->
        countCallsToNames names body
    | Jump _ | Return _ -> 0
    | Join (_, continuation, entry) ->
        countCallsToNames names continuation + countCallsToNames names entry
    | If (_, thenBranch, elseBranch) ->
        countCallsToNames names thenBranch + countCallsToNames names elseBranch

let private shouldUseExternalCandidate (info: FunctionInfo) (config: InliningConfig) : bool =
    shouldInline info config 0
    && Set.isEmpty info.Calls
    && (isSimpleExternalExpr info.Func.Body
        || (isScalarRawReadReturnType info.Func.ReturnType
            && isExternalExprWith isScalarRawReadExternalCExpr info.Func.Body))

/// Analyze and qualify external functions once so user-program inlining can
/// reuse the metadata without traversing stdlib bodies on every compilation.
let buildExternalCandidateInfoMap
    (config: InliningConfig)
    (functions: Function list)
    : Map<AST.FunctionId, FunctionInfo> =
    buildFunctionInfoMap functions
    |> Map.fold (fun candidates name info ->
        if shouldUseExternalCandidate info config then
            Map.add name { info with IsExternal = true } candidates
        else
            candidates) Map.empty
