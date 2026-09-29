// CalleeClobbers.fs - Conservatively summarize x64 caller-register writes.

module X64CalleeClobbers

type Writes = ARM64CalleeClobbers.Writes

let private empty : Writes = { Ints = 0UL; Floats = 0UL }
let all : Writes = {
    Ints = (1UL <<< 16) - 1UL
    Floats = (1UL <<< 14) - 1UL
}

let private union (left: Writes) (right: Writes) : Writes = {
    Ints = left.Ints ||| right.Ints
    Floats = left.Floats ||| right.Floats
}

let private intWrite = function
    | LIR.Physical reg -> { empty with Ints = ARM64CalleeClobbers.intBit reg }
    | LIR.Virtual _ -> all

let private floatWrite = function
    | LIR.FPhysical reg -> { empty with Floats = ARM64CalleeClobbers.floatBit reg }
    | LIR.FVirtual -1 -> empty
    | LIR.FVirtual _ -> all

/// Only x64 instructions whose backend expansion has no hidden scratch write
/// receive a narrow summary. Every other opcode retains the full ABI set.
let private instructionWrites calleeWrites = function
    | LIR.Mov (dest, LIR.Imm _)
    | LIR.Mov (dest, LIR.Reg _)
    | LIR.Mov (dest, LIR.FuncAddr _) -> intWrite dest
    | LIR.FMov (dest, _) -> floatWrite dest
    | LIR.Call (dest, callee, _) ->
        union
            (calleeWrites callee |> Option.defaultValue all)
            (intWrite dest)
    | LIR.TailCall (callee, _) ->
        calleeWrites callee |> Option.defaultValue all
    | LIR.SaveRegs _ -> empty
    | LIR.RestoreRegs (ints, floats) ->
        { Ints = ARM64CalleeClobbers.ofInts ints
          Floats = ARM64CalleeClobbers.ofFloats floats }
    | _ -> all

let private summarizeFunction calleeWrites (func: LIR.Function) =
    func.CFG.Blocks
    |> Map.fold (fun writes _ block ->
        block.Instrs
        |> List.fold (fun writes instr -> union writes (instructionWrites calleeWrites instr)) writes) empty
    |> fun writes ->
        // Return shuttles may be emitted after symbolic LIR and use these ABI
        // result registers even for an otherwise empty function.
        union writes { empty with Ints = ARM64CalleeClobbers.intBit LIR.X0
                                  Floats = ARM64CalleeClobbers.floatBit LIR.D0 }

let summariesWithKnown
    (known: Map<AST.FunctionId, Writes>)
    (functions: LIR.Function list) =
    let rec converge local =
        let lookup id =
            Map.tryFind id local
            |> Option.orElseWith (fun () -> Map.tryFind id known)
        let next =
            functions
            |> List.fold (fun acc func ->
                let writes = summarizeFunction lookup func
                let old = Map.tryFind func.Id acc |> Option.defaultValue empty
                Map.add func.Id (union old writes) acc) local
        if next = local then local else converge next
    let initial =
        functions
        |> List.map (fun func -> func.Id, empty)
        |> Map.ofList
    converge initial

/// Calls with argument setup or an unrecognized save envelope retain the full
/// ABI clobber set. Narrow envelopes currently cover zero-argument calls.
let callWritesForSaves callees (block: LIR.BasicBlock) : Writes list =
    let rec collect instrs =
        match instrs with
        | LIR.SaveRegs ([], []) :: rest ->
            let beforeRestore =
                rest |> List.takeWhile (function LIR.RestoreRegs _ -> false | _ -> true)
            let writes =
                match beforeRestore, List.tryItem (List.length beforeRestore) rest with
                | [LIR.Call (_, callee, [])], Some (LIR.RestoreRegs ([], [])) ->
                    Map.tryFind callee callees |> Option.defaultValue all
                | _ -> all
            writes :: collect rest
        | _ :: rest -> collect rest
        | [] -> []
    collect block.Instrs

let pruneFunction callees (func: LIR.Function) : LIR.Function =
    let pruneBlock (block: LIR.BasicBlock) =
        let rec rewrite instrs =
            match instrs with
            | LIR.SaveRegs (ints, floats) :: LIR.Call (dest, callee, []) :: LIR.RestoreRegs (restoreInts, restoreFloats) :: rest
                when ints = restoreInts && floats = restoreFloats ->
                let writes = Map.tryFind callee callees |> Option.defaultValue all
                let keptInts = ints |> List.filter (fun reg -> ARM64CalleeClobbers.containsInt reg writes)
                let keptFloats = floats |> List.filter (fun reg -> ARM64CalleeClobbers.containsFloat reg writes)
                // Preserve the original stack parity at the call site. The
                // backend emits each saved GP or FP register as eight bytes.
                let keptInts, keptFloats =
                    let removed =
                        List.length ints + List.length floats
                        - List.length keptInts - List.length keptFloats
                    if removed % 2 = 0 then keptInts, keptFloats
                    else
                        match ints |> List.tryFind (fun reg -> not (List.contains reg keptInts)) with
                        | Some extra ->
                            ints |> List.filter (fun reg -> reg = extra || List.contains reg keptInts), keptFloats
                        | None ->
                            let extra =
                                floats
                                |> List.tryFind (fun reg -> not (List.contains reg keptFloats))
                                |> Option.defaultWith (fun () ->
                                    Crash.crash "x64 call-save parity has no omitted register")
                            keptInts,
                            floats |> List.filter (fun reg -> reg = extra || List.contains reg keptFloats)
                LIR.SaveRegs (keptInts, keptFloats) :: LIR.Call (dest, callee, []) ::
                LIR.RestoreRegs (keptInts, keptFloats) :: rewrite rest
            | instr :: rest -> instr :: rewrite rest
            | [] -> []
        { block with Instrs = rewrite block.Instrs }
    let blocks = func.CFG.Blocks |> Map.map (fun _ block -> pruneBlock block)
    if blocks = func.CFG.Blocks then func
    else { func with CFG = { func.CFG with Blocks = blocks } }
