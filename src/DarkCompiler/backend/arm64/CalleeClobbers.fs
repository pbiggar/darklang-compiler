// CalleeClobbers.fs - Conservatively summarize ARM64 register writes across direct calls.

module ARM64CalleeClobbers

type Writes = {
    Ints: Set<LIR.PhysReg>
    Floats: Set<LIR.PhysFPReg>
}

let private empty = { Ints = Set.empty; Floats = Set.empty }
let all = {
    Ints = Set.ofList [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7;
                       LIR.X8; LIR.X9; LIR.X10; LIR.X11; LIR.X12; LIR.X13; LIR.X14; LIR.X15]
    Floats = Set.ofList [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]
}

let private union left right = {
    Ints = Set.union left.Ints right.Ints
    Floats = Set.union left.Floats right.Floats
}

let private writeInt = function
    | LIR.Physical reg -> { empty with Ints = Set.singleton reg }
    | LIR.Virtual _ -> all

let private writeFloat = function
    | LIR.FPhysical reg -> { empty with Floats = Set.singleton reg }
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
        else union baseWrites { empty with Ints = Set.singleton LIR.X10 }
    | LIR.Store (offset, _) ->
        if offset >= -256 && offset <= 255 then empty
        else { empty with Ints = Set.singleton LIR.X10 }
    | LIR.Add (dest, _, LIR.Reg _)
    | LIR.Sub (dest, _, LIR.Reg _)
    | LIR.Add (dest, _, LIR.Imm _)
    | LIR.Sub (dest, _, LIR.Imm _) ->
        match instr with
        | LIR.Add (_, _, LIR.Imm n) | LIR.Sub (_, _, LIR.Imm n) when n < 0L || n >= 4096L ->
            union (writeInt dest) { empty with Ints = Set.singleton LIR.X9 }
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
        else { empty with Ints = Set.singleton LIR.X9 }
    | LIR.FLoad (dest, _) ->
        // A literal outside FMOV's immediate range is addressed via X9.
        union (writeFloat dest) { empty with Ints = Set.singleton LIR.X9 }
    | LIR.FSpillLoad (dest, _) ->
        union (writeFloat dest) { empty with Ints = Set.singleton LIR.X10 }
    | LIR.FSpillStore _ -> { empty with Ints = Set.singleton LIR.X10 }
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
            { empty with Ints = moves |> List.map fst |> Set.ofList }
    | LIR.FArgMoves moves ->
        { empty with Floats = moves |> List.map fst |> Set.ofList }
    | LIR.SaveRegs _ -> empty
    | LIR.RestoreRegs (ints, floats) ->
        { Ints = Set.ofList ints; Floats = Set.ofList floats }
    | _ -> all

let private summarizeFunction calleeWrites (func: LIR.Function) =
    func.CFG.Blocks
    |> Map.fold (fun writes _ block ->
        block.Instrs
        |> List.fold (fun writes instr -> union writes (instructionWrites calleeWrites instr)) writes) empty

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

let summaries (functions: LIR.Function list) =
    summariesWithKnown Map.empty functions

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
        let lookup id = Map.tryFind id callees
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

let private pruneBlock (callees: Map<AST.FunctionId, Writes>) (block: LIR.BasicBlock) =
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
                    let keptInts = ints |> List.filter (fun reg -> Set.contains reg writes.Ints)
                    let keptFloats = floats |> List.filter (fun reg -> Set.contains reg writes.Floats)
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
    (cache: (LIR.Function -> Map<AST.FunctionId, Writes> -> (unit -> LIR.Function) -> LIR.Function) option)
    (knownWrites: Map<AST.FunctionId, Writes> option)
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
                |> List.filter (fun func -> not (Map.containsKey func.Id known))
            summariesWithKnown known missing
            |> Map.fold (fun writes id value -> Map.add id value writes) known
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
                    match Map.tryFind id callees with
                    | Some value -> Map.add id value writes
                    | None -> writes) Map.empty
            let generate () =
                let blocks = func.CFG.Blocks |> Map.map (fun _ block -> pruneBlock relevant block)
                if blocks = func.CFG.Blocks then func
                else { func with CFG = { func.CFG with Blocks = blocks } }
            match cache with
            | Some reuse -> reuse func relevant generate
            | None -> generate ())

let refine (functions: LIR.Function list) : LIR.Function list =
    refineWithCache None None functions
