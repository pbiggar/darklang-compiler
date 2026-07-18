// DeadCodeElimination.fs - Dead Code Elimination (Tree Shaking)
//
// Filters out unused stdlib functions based on call graph reachability.
// This reduces CodeGen work by only processing functions that are actually used.

module DeadCodeElimination

/// Extract function names from an operand
let private extractFromOperand (op: LIR.Operand) : string list =
    match op with
    | LIR.FuncAddr name -> [name]
    | _ -> []

let private extractFromOperands (ops: LIR.Operand list) : string list =
    ops |> List.collect extractFromOperand

/// Extract function names from a single instruction
let private extractCallsFromInstr (instr: LIR.Instr) : string list =
    match instr with
    | LIR.Mov (_, src) -> extractFromOperand src
    | LIR.Phi (_, sources, _) ->
        sources |> List.map fst |> extractFromOperands
    | LIR.Store _ -> []
    | LIR.Add (_, _, right)
    | LIR.Sub (_, _, right)
    | LIR.Cmp (_, right) ->
        extractFromOperand right
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
    | LIR.Lsl_imm _
    | LIR.Lsr_imm _
    | LIR.Mvn _
    | LIR.Sxtb _
    | LIR.Sxth _
    | LIR.Sxtw _
    | LIR.Uxtb _
    | LIR.Uxth _
    | LIR.Uxtw _ ->
        []
    | LIR.Call (_, funcName, args) ->
        funcName :: extractFromOperands args
    | LIR.TailCall (funcName, args) ->
        funcName :: extractFromOperands args
    | LIR.IndirectCall (_, _, args) ->
        extractFromOperands args
    | LIR.IndirectTailCall (_, args) ->
        // Function pointer is in a register - we can't statically determine the target
        extractFromOperands args
    | LIR.ClosureAlloc (_, funcName, captures) ->
        funcName :: extractFromOperands captures
    | LIR.ClosureCall (_, _, args) ->
        extractFromOperands args
    | LIR.ClosureTailCall (_, args) ->
        // Closure pointer is in a register - we can't statically determine the target
        extractFromOperands args
    | LIR.SaveRegs _
    | LIR.RestoreRegs _ ->
        []
    | LIR.ArgMoves moves
    | LIR.TailArgMoves moves ->
        moves |> List.map snd |> extractFromOperands
    | LIR.FArgMoves _
    | LIR.PrintInt64 _
    | LIR.PrintBool _
    | LIR.PrintInt64NoNewline _
    | LIR.PrintBoolNoNewline _
    | LIR.PrintFloat _
    | LIR.PrintFloatNoNewline _
    | LIR.PrintString _
    | LIR.RuntimeError _
    | LIR.PrintHeapStringNoNewline _
    | LIR.PrintChars _
    | LIR.PrintBytes _
    | LIR.PrintList _
    | LIR.PrintRecord _
    | LIR.Exit
    | LIR.FPhi _
    | LIR.FMov _
    | LIR.FLoad _
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
    | LIR.RawFree _
    | LIR.RawGet _
    | LIR.RawGetByte _
    | LIR.RawWriteWord _
    | LIR.RawWriteByte _
    | LIR.RawSlotInit _
    | LIR.RandomInt64 _
    | LIR.DateNow _
    | LIR.FloatToString _
    | LIR.CoverageHit _ ->
        []
    | LIR.PrintSum (_, variants) ->
        variants
        |> List.collect (fun (_, _, payloadType) ->
            match payloadType with
            | Some (AST.TList elemType) ->
                match ListDisplay.getDisplayStringFunc elemType with
                | Some funcName -> [funcName]
                | None -> []
            | _ -> [])
    | LIR.HeapStore (_, _, src, _) -> extractFromOperand src
    | LIR.StringConcat (_, left, right) ->
        extractFromOperand left @ extractFromOperand right
    | LIR.LoadFuncAddr (_, funcName) -> [funcName]
    | LIR.FileReadText (_, path)
    | LIR.FileExists (_, path)
    | LIR.FileDelete (_, path)
    | LIR.FileSetExecutable (_, path)
    | LIR.RefCountIncString path
    | LIR.RefCountDecString path
    | LIR.RefCountIncBytes path
    | LIR.RefCountDecBytes path ->
        extractFromOperand path
    | LIR.FileWriteText (_, path, content)
    | LIR.FileAppendText (_, path, content) ->
        extractFromOperand path @ extractFromOperand content

/// Extract function names called from a LIR function
let getCalledFunctions (func: LIR.Function) : Set<string> =
    func.CFG.Blocks
    |> Map.toSeq
    |> Seq.collect (fun (_, block) -> block.Instrs)
    |> Seq.collect extractCallsFromInstr
    |> Set.ofSeq

/// Build call graph from list of functions
let buildCallGraph (funcs: LIR.Function list) : Map<string, Set<string>> =
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

/// Filter functions to only include reachable ones
let filterFunctions (callGraph: Map<string, Set<string>>)
                    (userFuncs: LIR.Function list)
                    (stdlibFuncs: LIR.Function list) : LIR.Function list =
    // Get all functions called from user code
    let userCalls =
        userFuncs
        |> List.collect (fun f -> getCalledFunctions f |> Set.toList)
        |> Set.ofList
    // Expand to transitive closure
    let reachable = findReachable callGraph userCalls
    // Filter stdlib to only reachable
    stdlibFuncs |> List.filter (fun f -> Set.contains f.Name reachable)
