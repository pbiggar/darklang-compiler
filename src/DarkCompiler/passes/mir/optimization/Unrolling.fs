// Unrolling.fs - Unroll bounded scalar counted-loop shapes.

module MIRUnrolling

open MIR
open SSA_Construction
open MIROptimizationFacts
open MIRLoopTopology
open MIRInduction

/// Scalar values can be duplicated or moved without changing ownership.
let internal isScalarValueType (valueType: AST.SemanticType) : bool =
    match valueType with
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TBool
    | AST.TFloat64
    | AST.TUnit -> true
    | _ -> false

type private CountedLoopUnrollCandidate = {
    Header: Label
    Latch: Label
    Exit: Label
    Guard: Instr
    GuardResult: VReg
    PhiBackedges: Map<VReg, Operand>
    LatchBlock: BasicBlock
    ExitInstrs: Instr list
    ExitResult: Operand
}

let private maxUnrolledBodyInstructions = 12

let private isUnrollableScalarInstr (instr: Instr) : bool =
    match instr with
    | Mov (_, _, Some valueType) -> isScalarValueType valueType
    | BinOp (_, _, _, _, operandType) -> isScalarValueType operandType
    | UnaryOp _
    | FloatSqrt _
    | FloatAbs _
    | FloatNeg _
    | Int64ToFloat _
    | FloatToInt64 _
    | FloatToBits _ -> true
    | _ -> false

let private isScalarExitResult
    (headerPhis: Instr list)
    (exitInstrs: Instr list)
    (result: Operand)
    : bool =
    match result with
    | Int64Const _
    | BoolConst _
    | FloatSymbol _ -> true
    | Register resultRegister ->
        (headerPhis @ exitInstrs)
        |> List.exists (fun instr -> getInstrDest instr = Some resultRegister)
    | _ -> false

let private substituteUnrolledOperand
    (substitutions: Map<VReg, Operand>)
    (operand: Operand)
    : Operand =
    match operand with
    | Register register -> Map.tryFind register substitutions |> Option.defaultValue operand
    | _ -> operand

let private cloneScalarInstr
    (substitutions: Map<VReg, Operand>)
    (destination: VReg)
    (instr: Instr)
    : Instr option =
    let substitute = substituteUnrolledOperand substitutions
    match instr with
    | Mov (_, source, valueType) -> Some (Mov (destination, substitute source, valueType))
    | BinOp (_, op, left, right, operandType) ->
        Some (BinOp (destination, op, substitute left, substitute right, operandType))
    | UnaryOp (_, op, source) -> Some (UnaryOp (destination, op, substitute source))
    | FloatSqrt (_, source) -> Some (FloatSqrt (destination, substitute source))
    | FloatAbs (_, source) -> Some (FloatAbs (destination, substitute source))
    | FloatNeg (_, source) -> Some (FloatNeg (destination, substitute source))
    | Int64ToFloat (_, source) -> Some (Int64ToFloat (destination, substitute source))
    | FloatToInt64 (_, source) -> Some (FloatToInt64 (destination, substitute source))
    | FloatToBits (_, source) -> Some (FloatToBits (destination, substitute source))
    | _ -> None

let private cloneScalarInstrs
    (firstRegisterId: int)
    (initialSubstitutions: Map<VReg, Operand>)
    (instrs: Instr list)
    : (Instr list * Map<VReg, Operand> * int) option =
    let folder state instr =
        match state, getInstrDest instr with
        | Some (clonedRev, substitutions, nextId), Some originalDestination ->
            let destination = VReg nextId
            match cloneScalarInstr substitutions destination instr with
            | Some cloned ->
                Some (
                    cloned :: clonedRev,
                    Map.add originalDestination (Register destination) substitutions,
                    nextId + 1
                )
            | None -> None
        | _ -> None

    instrs
    |> List.fold folder (Some ([], initialSubstitutions, firstRegisterId))
    |> Option.map (fun (clonedRev, substitutions, nextId) ->
        (List.rev clonedRev, substitutions, nextId))

let private isInvariantCountedLoopBound
    (preheader: Label)
    (latch: Label)
    (loopBlocks: Set<Label>)
    (cfg: CFG)
    (operand: Operand)
    : bool =
    match operand with
    | Register register ->
        let definitions =
            loopBlocks
            |> Set.toList
            |> List.collect (fun label ->
                Map.tryFind label cfg.Blocks
                |> Option.map (fun block -> block.Instrs)
                |> Option.defaultValue [])
            |> List.filter (fun instr -> getInstrDest instr = Some register)

        match definitions with
        | [] -> true
        | [Phi (destination, sources, Some AST.TInt64)] when destination = register ->
            let preheaderSources = sources |> List.filter (fun (_, label) -> label = preheader)
            let latchSources = sources |> List.filter (fun (_, label) -> label = latch)
            match sources, preheaderSources, latchSources, Map.tryFind latch cfg.Blocks with
            | [_; _], [_], [(Register backedge, _)], Some latchBlock ->
                resolveLatchCopy latchBlock.Instrs backedge = register
            | _ -> false
        | _ -> false
    | Int64Const _ -> true
    | _ -> false

let private tryCountedLoopUnrollCandidate
    (cfg: CFG)
    (header: Label)
    (loopBlocks: Set<Label>)
    : CountedLoopUnrollCandidate option =
    let predecessors = buildPredecessors cfg
    let headerPredecessors = Map.tryFind header predecessors |> Option.defaultValue []
    let outsidePredecessors =
        headerPredecessors |> List.filter (fun label -> not (Set.contains label loopBlocks))
    let insidePredecessors =
        headerPredecessors |> List.filter (fun label -> Set.contains label loopBlocks)

    match outsidePredecessors, insidePredecessors, Map.tryFind header cfg.Blocks with
    | [preheader], [latch], Some headerBlock when loopBlocks = Set.ofList [header; latch] ->
        let headerPhis, headerBody = headerBlock.Instrs |> List.partition (function Phi _ -> true | _ -> false)
        match Map.tryFind preheader cfg.Blocks, Map.tryFind latch cfg.Blocks, headerBody, headerBlock.Terminator with
        | Some preheaderBlock, Some latchBlock,
          [BinOp (guardResult, Gte, Register induction, bound, AST.TInt64) as guard],
          Branch (Register condition, exitLabel, bodyLabel)
            when preheaderBlock.Terminator = Jump header
                 && bodyLabel = latch
                 && condition = guardResult
                 && latchBlock.Terminator = Jump header
                 && List.length latchBlock.Instrs <= maxUnrolledBodyInstructions
                 && List.forall isUnrollableScalarInstr latchBlock.Instrs
                 && not (List.isEmpty headerPhis)
                 && (headerPhis
                     |> List.forall (function
                        | Phi (_, _, Some valueType) -> isScalarValueType valueType
                        | _ -> false))
                 && isInvariantCountedLoopBound preheader latch loopBlocks cfg bound ->
            let phiBackedges =
                headerPhis
                |> List.choose (function
                    | Phi (destination, sources, _) ->
                        let preheaderSources = sources |> List.filter (fun (_, label) -> label = preheader)
                        let latchSources = sources |> List.filter (fun (_, label) -> label = latch)
                        match sources, preheaderSources, latchSources with
                        | [_; _], [_], [(backedge, _)] -> Some (destination, backedge)
                        | _ -> None
                    | _ -> None)

            let inductionAdvancesByOne =
                phiBackedges
                |> List.tryPick (fun (destination, backedge) ->
                    if destination = induction then
                        match backedge with
                        | Register nextValue ->
                            let resolvedNext = resolveLatchCopy latchBlock.Instrs nextValue
                            Some (latchBlock.Instrs |> List.exists (isIncrementByOne AST.TInt64 induction resolvedNext))
                        | _ -> Some false
                    else
                        None)
                |> Option.defaultValue false

            match phiBackedges, inductionAdvancesByOne, Map.tryFind exitLabel cfg.Blocks with
            | backedges, true, Some exitBlock
                when List.length backedges = List.length headerPhis
                     && Map.tryFind exitLabel predecessors = Some [header]
                     && List.length exitBlock.Instrs <= maxUnrolledBodyInstructions
                     && List.forall isUnrollableScalarInstr exitBlock.Instrs ->
                match exitBlock.Terminator with
                | Ret exitResult when isScalarExitResult headerPhis exitBlock.Instrs exitResult ->
                    Some {
                        Header = header
                        Latch = latch
                        Exit = exitLabel
                        Guard = guard
                        GuardResult = guardResult
                        PhiBackedges = Map.ofList backedges
                        LatchBlock = latchBlock
                        ExitInstrs = exitBlock.Instrs
                        ExitResult = exitResult
                    }
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let private freshUnrollLabel (cfg: CFG) (Label baseName) (suffix: string) : Label =
    let rec choose index =
        let numberedSuffix = if index = 0 then suffix else $"{suffix}_{index}"
        let candidate = Label $"{baseName}_{numberedSuffix}"
        if Map.containsKey candidate cfg.Blocks then choose (index + 1) else candidate
    choose 0

let private replaceLatchPhiSource
    (candidate: CountedLoopUnrollCandidate)
    (secondIterationValues: Map<VReg, Operand>)
    (secondLatch: Label)
    (instr: Instr)
    : Instr =
    match instr with
    | Phi (destination, sources, valueType) ->
        let sources' =
            sources
            |> List.map (fun (operand, label) ->
                if label = candidate.Latch then
                    (
                        substituteUnrolledOperand secondIterationValues operand,
                        secondLatch
                    )
                else
                    (operand, label))
        Phi (destination, sources', valueType)
    | _ -> instr

(*
Only a two-block natural loop with one `i >= limit` guard is eligible. The
limit must be invariant, `i` must advance by exactly one, and both the latch and
scalar return path must fit the strict size cap. Calls, allocation, ownership,
memory access, and other effects are rejected. The first iteration retains its
original instruction order; cloned floating-point operations form the second
iteration in the same order, so evaluation is not reassociated.
*)
let internal applyCountedLoopUnrollingWithTopology
    (topology: LoopTopology)
    (cfg: CFG)
    : CFG * bool =
    let candidate =
        topology.Loops
        |> Map.toList
        |> List.tryPick (fun (header, loopBlocks) ->
            tryCountedLoopUnrollCandidate cfg header loopBlocks)

    match candidate with
    | None -> (cfg, false)
    | Some candidate ->
        let secondLatch = freshUnrollLabel cfg candidate.Latch "unroll_second"
        let remainderExit = freshUnrollLabel cfg candidate.Exit "unroll_remainder"
        let firstIterationValues = candidate.PhiBackedges
        let firstFreshRegister = nextRegisterId cfg

        match cloneScalarInstrs firstFreshRegister firstIterationValues [candidate.Guard] with
        | None -> (cfg, false)
        | Some (clonedGuard, guardSubstitutions, afterGuard) ->
            match Map.tryFind candidate.GuardResult guardSubstitutions with
            | None -> (cfg, false)
            | Some clonedGuardResult ->
                match cloneScalarInstrs afterGuard firstIterationValues candidate.LatchBlock.Instrs with
                | None -> (cfg, false)
                | Some (secondIterationInstrs, secondIterationValues, afterSecondIteration) ->
                    match cloneScalarInstrs afterSecondIteration firstIterationValues candidate.ExitInstrs with
                    | None -> (cfg, false)
                    | Some (remainderInstrs, remainderValues, _) ->
                        let headerBlock = Map.tryFind candidate.Header cfg.Blocks
                        match headerBlock with
                        | None -> (cfg, false)
                        | Some headerBlock ->
                            let updatedHeader = {
                                headerBlock with
                                    Instrs =
                                        headerBlock.Instrs
                                        |> List.map (replaceLatchPhiSource candidate secondIterationValues secondLatch)
                            }
                            let updatedFirstLatch = {
                                candidate.LatchBlock with
                                    Instrs = candidate.LatchBlock.Instrs @ clonedGuard
                                    Terminator = Branch (clonedGuardResult, remainderExit, secondLatch)
                            }
                            let secondLatchBlock = {
                                Label = secondLatch
                                Instrs = secondIterationInstrs
                                Terminator = Jump candidate.Header
                            }
                            let remainderExitBlock = {
                                Label = remainderExit
                                Instrs = remainderInstrs
                                Terminator =
                                    Ret (substituteUnrolledOperand remainderValues candidate.ExitResult)
                            }
                            let blocks =
                                cfg.Blocks
                                |> Map.add candidate.Header updatedHeader
                                |> Map.add candidate.Latch updatedFirstLatch
                                |> Map.add secondLatch secondLatchBlock
                                |> Map.add remainderExit remainderExitBlock
                            ({ cfg with Blocks = blocks }, true)

let applyCountedLoopUnrolling (cfg: CFG) : CFG * bool =
    match tryBuildLoopTopology cfg with
    | None -> (cfg, false)
    | Some topology -> applyCountedLoopUnrollingWithTopology topology cfg
