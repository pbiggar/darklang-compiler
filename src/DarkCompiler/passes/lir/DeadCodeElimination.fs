// DeadCodeElimination.fs - Dead Code Elimination (Tree Shaking)
//
// Filters out unused stdlib functions based on call graph reachability.
// This reduces CodeGen work by only processing functions that are actually used.

module DeadCodeElimination

/// Add a function name referenced by an operand to the current call set.
let private addCallFromOperand (op: LIR.Operand) (calls: Set<AST.FunctionId>) : Set<AST.FunctionId> =
    match op with
    | LIR.FuncAddr name -> Set.add name calls
    | _ -> calls

let private addCallsFromOperands (ops: LIR.Operand list) (calls: Set<AST.FunctionId>) : Set<AST.FunctionId> =
    ops |> List.fold (fun calls op -> addCallFromOperand op calls) calls

/// Add function names referenced by one instruction to the current call set.
let private addCallsFromInstr
    (idsByName: Map<string, AST.FunctionId>)
    (instr: LIR.Instr)
    (calls: Set<AST.FunctionId>)
    : Set<AST.FunctionId> =
    match instr with
    | LIR.Mov (_, src) -> addCallFromOperand src calls
    | LIR.Phi (_, sources, _) ->
        sources
        |> List.fold (fun calls (source, _) -> addCallFromOperand source calls) calls
    | LIR.Store _ -> calls
    | LIR.Add (_, _, right)
    | LIR.Sub (_, _, right)
    | LIR.Cmp (_, right) ->
        addCallFromOperand right calls
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
    | LIR.Uxtw _ ->
        calls
    | LIR.Call (_, funcName, args) ->
        calls |> Set.add funcName |> addCallsFromOperands args
    | LIR.TailCall (funcName, args) ->
        calls |> Set.add funcName |> addCallsFromOperands args
    | LIR.IndirectCall (_, _, args) ->
        addCallsFromOperands args calls
    | LIR.IndirectTailCall (_, args) ->
        // Function pointer is in a register - we can't statically determine the target
        addCallsFromOperands args calls
    | LIR.ClosureAlloc (_, funcName, captures) ->
        calls |> Set.add funcName |> addCallsFromOperands captures
    | LIR.ClosureCall (_, _, args) ->
        addCallsFromOperands args calls
    | LIR.ClosureTailCall (_, args) ->
        // Closure pointer is in a register - we can't statically determine the target
        addCallsFromOperands args calls
    | LIR.SaveRegs _
    | LIR.RestoreRegs _ ->
        calls
    | LIR.ArgMoves moves
    | LIR.TailArgMoves moves ->
        moves
        |> List.fold (fun calls (_, source) -> addCallFromOperand source calls) calls
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
    | LIR.CoverageHit _ ->
        calls
    | LIR.CliNative (_, _, args) -> addCallsFromOperands args calls
    | LIR.PrintSum (_, variants) ->
        variants
        |> List.fold (fun calls (_, _, payloadType) ->
            match payloadType with
            | Some (AST.TList elemType) ->
                match ListDisplay.getDisplayStringFunc elemType with
                | Some funcName ->
                    match Map.tryFind funcName idsByName with
                    | Some id -> Set.add id calls
                    | None -> calls
                | None -> calls
            | _ -> calls) calls
    | LIR.HeapStore (_, _, src, _) -> addCallFromOperand src calls
    | LIR.StringConcat (_, first, second, remaining) ->
        first :: second :: remaining |> List.fold (fun acc operand -> addCallFromOperand operand acc) calls
    | LIR.CanonicalBufferEq (_, _, left, right) ->
        calls |> addCallFromOperand left |> addCallFromOperand right
    | LIR.LoadFuncAddr (_, funcName) -> Set.add funcName calls
    | LIR.FileReadBlob (_, path)
    | LIR.FileExists (_, path)
    | LIR.FileDelete (_, path)
    | LIR.FileCreateDirectory (_, path)
    | LIR.FileSetExecutable (_, path)
    | LIR.RefCountIncString path
    | LIR.RefCountDecString path
    | LIR.RefCountIncBlob path
    | LIR.RefCountDecBlob path ->
        addCallFromOperand path calls
    | LIR.RefCountIncInt path
    | LIR.RefCountDecInt path ->
        addCallFromOperand path calls
    | LIR.FileWriteBlob (_, path, content)
    | LIR.FileAppendText (_, path, content) ->
        calls |> addCallFromOperand path |> addCallFromOperand content

/// Add every function-call edge in one LIR function to an existing call set.
let private addCalledFunctions idsByName (func: LIR.Function) (calls: Set<AST.FunctionId>) : Set<AST.FunctionId> =
    func.CFG.Blocks
    |> Map.fold (fun calls _ block ->
        block.Instrs
        |> List.fold (fun calls instr -> addCallsFromInstr idsByName instr calls) calls) calls

/// Extract function names called from a LIR function
let getCalledFunctions (func: LIR.Function) : Set<AST.FunctionId> =
    addCalledFunctions (Map.ofList [func.Name, func.Id]) func Set.empty

let getCalledFunctionsWithNames functionNames (func: LIR.Function) : Set<AST.FunctionId> =
    let idsByName = functionNames |> Map.toSeq |> Seq.map (fun (id, name) -> name, id) |> Map.ofSeq
    let partitionFunctionNames =
        func.CodegenFacts
        |> Option.bind (fun facts -> facts.Arm64FunctionNames)
        |> Option.defaultValue functionNames
    addCalledFunctions idsByName func Set.empty
    |> Set.map (fun id ->
        partitionFunctionNames
        |> Map.tryFind id
        |> Option.bind (fun name -> Map.tryFind name idsByName)
        |> Option.defaultValue id)

/// Resolve direct-call identities through the immutable symbol table attached
/// to the compilation partition that produced the function.
let getCalledFunctionNames
    (fallbackFunctionNames: Map<AST.FunctionId, string>)
    (func: LIR.Function)
    : Set<string> =
    let partitionFunctionNames =
        func.CodegenFacts
        |> Option.bind (fun facts -> facts.Arm64FunctionNames)
        |> Option.defaultValue fallbackFunctionNames
    let helperNames =
        func.CodegenFacts
        |> Option.bind (fun facts -> facts.Arm64GenericDecHelperIds)
        |> Option.defaultValue Map.empty
        |> Map.toSeq
        |> Seq.map (fun (name, id) -> id, name)
        |> Map.ofSeq
    let allFunctionNames =
        Map.fold (fun names id name -> Map.add id name names) fallbackFunctionNames partitionFunctionNames
        |> fun names -> Map.fold (fun current id name -> Map.add id name current) names helperNames
    let idsByName =
        allFunctionNames
        |> Map.toSeq
        |> Seq.map (fun (id, name) -> name, id)
        |> Map.ofSeq
    addCalledFunctions idsByName func Set.empty
    |> Seq.choose (fun id ->
        Map.tryFind id helperNames
        |> Option.orElseWith (fun () -> Map.tryFind id partitionFunctionNames)
        |> Option.orElseWith (fun () -> Map.tryFind id fallbackFunctionNames))
    |> Set.ofSeq

let requiresListDisplayHelpers (func: LIR.Function) : bool =
    func.CFG.Blocks
    |> Map.exists (fun _ block ->
        block.Instrs
        |> List.exists (function
            | LIR.PrintSum (_, variants) ->
                variants
                |> List.exists (fun (_, _, payloadType) ->
                    match payloadType with
                    | Some (AST.TList elemType) -> ListDisplay.getDisplayStringFunc elemType |> Option.isSome
                    | _ -> false)
            | _ -> false))

/// Build call graph from list of functions
let buildCallGraphWithNames functionNames (funcs: LIR.Function list) : Map<AST.FunctionId, Set<AST.FunctionId>> =
    funcs
    |> List.map (fun f -> f.Id, getCalledFunctionsWithNames functionNames f)
    |> Map.ofList

let buildCallGraph (funcs: LIR.Function list) : Map<AST.FunctionId, Set<AST.FunctionId>> =
    let functionNames = funcs |> List.map (fun func -> func.Id, func.Name) |> Map.ofList
    buildCallGraphWithNames functionNames funcs

/// Compute transitive closure of reachable functions.
let findReachable (callGraph: Map<AST.FunctionId, Set<AST.FunctionId>>) (roots: Set<AST.FunctionId>) : Set<AST.FunctionId> =
    CallGraphReachability.findReachable callGraph roots

/// Collect the direct calls made by functions already represented in a call graph.
let directCallsFromFunctions
    (callGraph: Map<AST.FunctionId, Set<AST.FunctionId>>)
    (functions: LIR.Function list)
    : Set<AST.FunctionId> =
    functions
    |> List.fold (fun calls func ->
        match Map.tryFind func.Id callGraph with
        | Some functionCalls -> Set.union calls functionCalls
        | None -> calls) Set.empty

/// Filter functions to only include those reachable from a precomputed user call graph.
let filterFunctionsWithUserCallGraph
    (callGraph: Map<AST.FunctionId, Set<AST.FunctionId>>)
    (userCallGraph: Map<AST.FunctionId, Set<AST.FunctionId>>)
    (userFuncs: LIR.Function list)
    (stdlibFuncs: LIR.Function list)
    : LIR.Function list =
    let userCalls = directCallsFromFunctions userCallGraph userFuncs
    let reachable = findReachable callGraph userCalls
    stdlibFuncs |> List.filter (fun f -> Set.contains f.Id reachable)

/// Filter functions to only include reachable ones
let filterFunctions (callGraph: Map<AST.FunctionId, Set<AST.FunctionId>>)
                    (userFuncs: LIR.Function list)
                    (stdlibFuncs: LIR.Function list) : LIR.Function list =
    let userCallGraph = buildCallGraph userFuncs
    filterFunctionsWithUserCallGraph callGraph userCallGraph userFuncs stdlibFuncs
