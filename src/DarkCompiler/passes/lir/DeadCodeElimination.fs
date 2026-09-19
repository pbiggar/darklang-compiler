// DeadCodeElimination.fs - Dead Code Elimination (Tree Shaking)
//
// Filters out unused stdlib functions based on call graph reachability.
// This reduces CodeGen work by only processing functions that are actually used.

module DeadCodeElimination

/// Add a function name referenced by an operand to the current call set.
let private addCallFromOperand (op: LIR.Operand) (calls: Set<string>) : Set<string> =
    match op with
    | LIR.FuncAddr name -> Set.add name calls
    | _ -> calls

let private addCallsFromOperands (ops: LIR.Operand list) (calls: Set<string>) : Set<string> =
    ops |> List.fold (fun calls op -> addCallFromOperand op calls) calls

/// Add function names referenced by one instruction to the current call set.
let private addCallsFromInstr (instr: LIR.Instr) (calls: Set<string>) : Set<string> =
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
                | Some funcName -> Set.add funcName calls
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
    | LIR.FileWriteBlob (_, path, content)
    | LIR.FileAppendText (_, path, content) ->
        calls |> addCallFromOperand path |> addCallFromOperand content

/// Add every function-call edge in one LIR function to an existing call set.
let private addCalledFunctions (func: LIR.Function) (calls: Set<string>) : Set<string> =
    func.CFG.Blocks
    |> Map.fold (fun calls _ block ->
        block.Instrs
        |> List.fold (fun calls instr -> addCallsFromInstr instr calls) calls) calls

/// Extract function names called from a LIR function
let getCalledFunctions (func: LIR.Function) : Set<string> =
    addCalledFunctions func Set.empty

/// Build call graph from list of functions
let buildCallGraph (funcs: LIR.Function list) : Map<string, Set<string>> =
    funcs
    |> List.map (fun f -> f.Name, getCalledFunctions f)
    |> Map.ofList

/// Compute transitive closure of reachable functions.
let findReachable (callGraph: Map<string, Set<string>>) (roots: Set<string>) : Set<string> =
    CallGraphReachability.findReachable callGraph roots

/// Collect the direct calls made by functions already represented in a call graph.
let directCallsFromFunctions
    (callGraph: Map<string, Set<string>>)
    (functions: LIR.Function list)
    : Set<string> =
    functions
    |> List.fold (fun calls func ->
        match Map.tryFind func.Name callGraph with
        | Some functionCalls -> Set.union calls functionCalls
        | None -> calls) Set.empty

/// Filter functions to only include those reachable from a precomputed user call graph.
let filterFunctionsWithUserCallGraph
    (callGraph: Map<string, Set<string>>)
    (userCallGraph: Map<string, Set<string>>)
    (userFuncs: LIR.Function list)
    (stdlibFuncs: LIR.Function list)
    : LIR.Function list =
    let userCalls = directCallsFromFunctions userCallGraph userFuncs
    let reachable = findReachable callGraph userCalls
    stdlibFuncs |> List.filter (fun f -> Set.contains f.Name reachable)

/// Filter functions to only include reachable ones
let filterFunctions (callGraph: Map<string, Set<string>>)
                    (userFuncs: LIR.Function list)
                    (stdlibFuncs: LIR.Function list) : LIR.Function list =
    let userCallGraph = buildCallGraph userFuncs
    filterFunctionsWithUserCallGraph callGraph userCallGraph userFuncs stdlibFuncs
