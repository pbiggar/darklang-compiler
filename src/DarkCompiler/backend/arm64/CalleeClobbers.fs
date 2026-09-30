// CalleeClobbers.fs - Conservatively summarize ARM64 register writes across direct calls.

module ARM64CalleeClobbers

[<Struct>]
type Writes = {
    Ints: uint64
    Floats: uint64
}

let intBit reg =
    let index =
        match reg with
        | LIR.X0 -> 0 | LIR.X1 -> 1 | LIR.X2 -> 2 | LIR.X3 -> 3
        | LIR.X4 -> 4 | LIR.X5 -> 5 | LIR.X6 -> 6 | LIR.X7 -> 7
        | LIR.X8 -> 8 | LIR.X9 -> 9 | LIR.X10 -> 10 | LIR.X11 -> 11
        | LIR.X12 -> 12 | LIR.X13 -> 13 | LIR.X14 -> 14 | LIR.X15 -> 15
        | LIR.X16 -> 16 | LIR.X17 -> 17
        | LIR.X19 -> 18 | LIR.X20 -> 19 | LIR.X21 -> 20 | LIR.X22 -> 21
        | LIR.X23 -> 22 | LIR.X24 -> 23 | LIR.X25 -> 24 | LIR.X26 -> 25
        | LIR.X27 -> 26 | LIR.X29 -> 27 | LIR.X30 -> 28 | LIR.SP -> 29
    1UL <<< index

let floatBit reg =
    let index =
        match reg with
        | LIR.D0 -> 0 | LIR.D1 -> 1 | LIR.D2 -> 2 | LIR.D3 -> 3
        | LIR.D4 -> 4 | LIR.D5 -> 5 | LIR.D6 -> 6 | LIR.D7 -> 7
        | LIR.D8 -> 8 | LIR.D9 -> 9 | LIR.D10 -> 10 | LIR.D11 -> 11
        | LIR.D12 -> 12 | LIR.D13 -> 13 | LIR.D14 -> 14 | LIR.D15 -> 15
    1UL <<< index

let ofInts regs = regs |> List.fold (fun mask reg -> mask ||| intBit reg) 0UL
let ofFloats regs = regs |> List.fold (fun mask reg -> mask ||| floatBit reg) 0UL
let containsInt reg writes = writes.Ints &&& intBit reg <> 0UL
let containsFloat reg writes = writes.Floats &&& floatBit reg <> 0UL

let private empty = { Ints = 0UL; Floats = 0UL }
let all = {
    Ints = (1UL <<< 16) - 1UL
    Floats = (1UL <<< 8) - 1UL
}

let private union left right = {
    Ints = left.Ints ||| right.Ints
    Floats = left.Floats ||| right.Floats
}

let private writeInt = function
    | LIR.Physical reg -> { empty with Ints = intBit reg }
    | LIR.Virtual _ -> all

let private writeFloat = function
    | LIR.FPhysical reg -> { empty with Floats = floatBit reg }
    | LIR.FVirtual -1 -> empty // Reserved D16 return shuttle.
    | LIR.FVirtual _ -> all

/// The modeled operations lower to writes of their stated destinations only.
/// Every other opcode retains the complete ABI clobber set, including backend
/// expansions with hidden scratch registers or calls to runtime helpers.
let private instructionWrites (calleeWrites: AST.FunctionId -> Writes option) instr =
    match instr with
    | LIR.Mov (dest, LIR.Imm _)
    | LIR.Mov (dest, LIR.Reg _)
    | LIR.Mov (dest, LIR.StringSymbol _)
    | LIR.Mov (dest, LIR.FuncAddr _)
    | LIR.LoadFuncAddr (dest, _) -> writeInt dest
    | LIR.Mov (dest, LIR.StackSlot offset) ->
        let baseWrites = writeInt dest
        if offset >= -256 && offset <= 255 then baseWrites
        else union baseWrites { empty with Ints = intBit LIR.X10 }
    | LIR.Store (offset, _) ->
        if offset >= -256 && offset <= 255 then empty
        else { empty with Ints = intBit LIR.X10 }
    | LIR.Add (dest, _, LIR.Reg _)
    | LIR.Sub (dest, _, LIR.Reg _)
    | LIR.Add (dest, _, LIR.Imm _)
    | LIR.Sub (dest, _, LIR.Imm _) ->
        match instr with
        | LIR.Add (_, _, LIR.Imm n) | LIR.Sub (_, _, LIR.Imm n) when n < 0L || n >= 4096L ->
            union (writeInt dest) { empty with Ints = intBit LIR.X9 }
        | _ -> writeInt dest
    | LIR.Mul (dest, _, _)
    | LIR.Sdiv (dest, _, _)
    | LIR.Udiv (dest, _, _)
    | LIR.Msub (dest, _, _, _)
    | LIR.Madd (dest, _, _, _)
    | LIR.Cset (dest, _)
    | LIR.Select (dest, _, _, _)
    | LIR.And (dest, _, _)
    | LIR.Orr (dest, _, _)
    | LIR.Eor (dest, _, _)
    | LIR.Lsl (dest, _, _)
    | LIR.Lsr (dest, _, _)
    | LIR.Asr (dest, _, _)
    | LIR.Lsl_imm (dest, _, _)
    | LIR.Lsr_imm (dest, _, _)
    | LIR.Asr_imm (dest, _, _)
    | LIR.Neg (dest, _)
    | LIR.Mvn (dest, _)
    | LIR.Sxtb (dest, _)
    | LIR.Sxth (dest, _)
    | LIR.Sxtw (dest, _)
    | LIR.Uxtb (dest, _)
    | LIR.Uxth (dest, _)
    | LIR.Uxtw (dest, _) -> writeInt dest
    | LIR.Cmp (_, LIR.Reg _)
    | LIR.FCmp _ -> empty
    | LIR.Cmp (_, LIR.Imm value) ->
        if value >= 0L && value < 4096L then empty
        else { empty with Ints = intBit LIR.X9 }
    | LIR.FLoad (dest, _) ->
        // A literal outside FMOV's immediate range is addressed via X9.
        union (writeFloat dest) { empty with Ints = intBit LIR.X9 }
    | LIR.FSpillLoad (dest, _) ->
        union (writeFloat dest) { empty with Ints = intBit LIR.X10 }
    | LIR.FSpillStore _ -> { empty with Ints = intBit LIR.X10 }
    | LIR.FMov (dest, _)
    | LIR.FAdd (dest, _, _)
    | LIR.FSub (dest, _, _)
    | LIR.FMul (dest, _, _)
    | LIR.FMadd (dest, _, _, _)
    | LIR.FDiv (dest, _, _)
    | LIR.FNeg (dest, _)
    | LIR.FAbs (dest, _)
    | LIR.FSqrt (dest, _)
    | LIR.Int64ToFloat (dest, _)
    | LIR.GpToFp (dest, _) -> writeFloat dest
    | LIR.FloatToInt64 (dest, _)
    | LIR.FloatToBits (dest, _)
    | LIR.FpToGp (dest, _)
    | LIR.HeapLoad (dest, _, _) -> writeInt dest
    // ARM64 emits the call result in X0; a separate LIR Mov writes its local
    // destination. Counting Call.dest here would report a write that BL omits.
    | LIR.Call (_, callee, _) | LIR.TailCall (callee, _) ->
        calleeWrites callee |> Option.defaultValue all
    | LIR.ArgMoves moves | LIR.TailArgMoves moves ->
        if moves |> List.exists (fun (_, operand) ->
            match operand with
            | LIR.Imm _ | LIR.Reg _ | LIR.FuncAddr _ -> false
            | _ -> true) then all
        else
            // Parallel move cycles use reserved X16, outside the saved set.
            { empty with Ints = moves |> List.map fst |> ofInts }
    | LIR.FArgMoves moves ->
        { empty with Floats = moves |> List.map fst |> ofFloats }
    | LIR.SaveRegs _ -> empty
    | LIR.RestoreRegs (ints, floats) ->
        { Ints = ofInts ints; Floats = ofFloats floats }
    | _ -> all

let private summarizeFunction calleeWrites (func: LIR.Function) =
    func.CFG.Blocks
    |> Map.fold (fun writes _ block ->
        block.Instrs
        |> List.fold (fun writes instr -> union writes (instructionWrites calleeWrites instr)) writes) empty

let summariesWithKnown
    (known: FunctionIdMap<Writes>)
    (functions: LIR.Function list) =
    let rec converge local =
        let lookup id =
            FunctionIdMap.tryFind id local
            |> Option.orElseWith (fun () -> FunctionIdMap.tryFind id known)
        let next =
            functions
            |> List.fold (fun acc func ->
                let writes = summarizeFunction lookup func
                let old = FunctionIdMap.tryFind func.Id acc |> Option.defaultValue empty
                FunctionIdMap.add func.Id (union old writes) acc) local
        if next = local then local else converge next
    let initial =
        functions
        |> List.map (fun func -> func.Id, empty)
        |> FunctionIdMap.ofList
    converge initial

let summaries (functions: LIR.Function list) =
    summariesWithKnown FunctionIdMap.empty functions

let private envelopeWrites callees beforeRestore =
    let calls =
        beforeRestore
        |> List.choose (function LIR.Call (_, id, _) -> Some id | _ -> None)
    let safeEnvelope =
        beforeRestore
        |> List.forall (function
            | LIR.Call _ | LIR.ArgMoves _ | LIR.FArgMoves _
            | LIR.FMov (LIR.FVirtual -1, LIR.FPhysical LIR.D0) -> true
            | _ -> false)
    match calls with
    | [_] when safeEnvelope ->
        // Saves precede argument setup, so its writes matter too.
        let lookup id = FunctionIdMap.tryFind id callees
        beforeRestore
        |> List.fold (fun writes instr ->
            union writes (instructionWrites lookup instr)) empty
        |> Some
    | _ -> None

/// Match the liveness snapshots produced for empty call-save placeholders.
/// Unknown envelopes carry the full ABI set through allocation.
let callWritesForSaves callees (block: LIR.BasicBlock) : Writes list =
    let rec collect instrs =
        match instrs with
        | LIR.SaveRegs ([], []) :: rest ->
            let beforeRestore =
                rest
                |> List.takeWhile (function LIR.RestoreRegs _ -> false | _ -> true)
            let writes =
                match List.tryItem (List.length beforeRestore) rest with
                | Some (LIR.RestoreRegs ([], [])) ->
                    envelopeWrites callees beforeRestore |> Option.defaultValue all
                | _ -> all
            writes :: collect rest
        | _ :: rest -> collect rest
        | [] -> []
    collect block.Instrs

let private pruneBlock (callees: FunctionIdMap<Writes>) (block: LIR.BasicBlock) =
    let rec rewrite instrs =
        match instrs with
        | LIR.SaveRegs (ints, floats) :: rest ->
            let beforeRestore, afterRestore =
                rest |> List.tryFindIndex (function LIR.RestoreRegs _ -> true | _ -> false)
                |> function
                    | None -> [], rest
                    | Some index -> List.take index rest, List.skip index rest
            match afterRestore with
            | LIR.RestoreRegs (restoreInts, restoreFloats) :: tail
                when ints = restoreInts && floats = restoreFloats ->
                match envelopeWrites callees beforeRestore with
                | Some writes ->
                    let keptInts = ints |> List.filter (fun reg -> containsInt reg writes)
                    let keptFloats = floats |> List.filter (fun reg -> containsFloat reg writes)
                    LIR.SaveRegs (keptInts, keptFloats) :: beforeRestore
                    @ (LIR.RestoreRegs (keptInts, keptFloats) :: rewrite tail)
                | _ ->
                    LIR.SaveRegs (ints, floats) :: beforeRestore
                    @ (LIR.RestoreRegs (restoreInts, restoreFloats) :: rewrite tail)
            | _ -> LIR.SaveRegs (ints, floats) :: rewrite rest
        | instr :: rest -> instr :: rewrite rest
        | [] -> []
    let rewritten = rewrite block.Instrs
    if rewritten = block.Instrs then block
    else { block with Instrs = rewritten }

let refineWithCache
    (cache: (LIR.Function -> FunctionIdMap<Writes> -> (unit -> LIR.Function) -> LIR.Function) option)
    (knownWrites: FunctionIdMap<Writes> option)
    (functions: LIR.Function list)
    : LIR.Function list =
    let callees =
        match knownWrites with
        | None -> summaries functions
        | Some known ->
            // Reused stdlib variants can appear in the final binary without
            // a unique saved summary. Analyze just those bodies, using the
            // finalized summaries for every other callee.
            let missing =
                functions
                |> List.filter (fun func -> not (FunctionIdMap.containsKey func.Id known))
            summariesWithKnown known missing
            |> FunctionIdMap.fold (fun writes id value -> FunctionIdMap.add id value writes) known
    functions
    |> List.map (fun func ->
        let hasSaves =
            func.CFG.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function LIR.SaveRegs _ -> true | _ -> false))
        if not hasSaves then func
        else
            let directIds =
                func.CFG.Blocks
                |> Map.toList
                |> List.collect (fun (_, block) ->
                    block.Instrs
                    |> List.choose (function LIR.Call (_, id, _) -> Some id | _ -> None))
                |> Set.ofList
            let relevant =
                directIds
                |> Set.fold (fun writes id ->
                    match FunctionIdMap.tryFind id callees with
                    | Some value -> FunctionIdMap.add id value writes
                    | None -> writes) FunctionIdMap.empty
            let generate () =
                let blocks = func.CFG.Blocks |> Map.map (fun _ block -> pruneBlock relevant block)
                if blocks = func.CFG.Blocks then func
                else { func with CFG = { func.CFG with Blocks = blocks } }
            match cache with
            | Some reuse -> reuse func relevant generate
            | None -> generate ())

let refine (functions: LIR.Function list) : LIR.Function list =
    refineWithCache None None functions
