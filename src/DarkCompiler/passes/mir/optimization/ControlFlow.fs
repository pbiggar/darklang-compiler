// ControlFlow.fs - Simplify MIR branches, joins, and unreachable blocks.

module MIRControlFlow

open MIR
open SSA_Construction
open MIROptimizationFacts
open MIRLoopTopology
open MIRCopyPropagation
open MIRCopyPropagation

/// Merge a block ending in an unconditional jump with its sole-predecessor
/// successor. Successor phis become copies, while phi edges leaving the merged
/// block are relabeled to preserve their predecessor identity.
let mergeLinearBlocks (cfg: CFG) : CFG * bool =
    let rec mergeNext (current: CFG) (changed: bool) : CFG * bool =
        let predecessors = buildPredecessors current

        let candidate =
            current.Blocks
            |> Map.toList
            |> List.tryPick (fun (sourceLabel, sourceBlock) ->
                match sourceBlock.Terminator with
                | Jump successorLabel when successorLabel <> sourceLabel && successorLabel <> current.Entry ->
                    match Map.tryFind successorLabel predecessors, Map.tryFind successorLabel current.Blocks with
                    | Some [onlyPredecessor], Some successorBlock when onlyPredecessor = sourceLabel ->
                        let hasValidPhis =
                            successorBlock.Instrs
                            |> List.forall (fun instr ->
                                match instr with
                                | Phi (_, [(_, phiSource)], _) -> phiSource = sourceLabel
                                | Phi _ -> false
                                | _ -> true)

                        if hasValidPhis then
                            Some (sourceLabel, sourceBlock, successorLabel, successorBlock)
                        else
                            None
                    | _ -> None
                | _ -> None)

        match candidate with
        | None -> (current, changed)
        | Some (sourceLabel, sourceBlock, successorLabel, successorBlock) ->
            let successorInstrs =
                successorBlock.Instrs
                |> List.map (fun instr ->
                    match instr with
                    | Phi (dest, [(operand, phiSource)], valueType) when phiSource = sourceLabel ->
                        Mov (dest, operand, valueType)
                    | Phi _ ->
                        Crash.crash $"mergeLinearBlocks: invalid phi in sole-predecessor block {successorLabel}"
                    | other -> other)

            let mergedBlock = {
                sourceBlock with
                    Instrs = sourceBlock.Instrs @ successorInstrs
                    Terminator = successorBlock.Terminator
            }

            let blocks =
                current.Blocks
                |> Map.remove successorLabel
                |> Map.add sourceLabel mergedBlock
                |> Map.map (fun _ block ->
                    let instrs =
                        block.Instrs
                        |> List.map (fun instr ->
                            match instr with
                            | Phi (dest, sources, valueType) ->
                                let sources' =
                                    sources
                                    |> List.map (fun (operand, phiSource) ->
                                        if phiSource = successorLabel then (operand, sourceLabel)
                                        else (operand, phiSource))
                                Phi (dest, sources', valueType)
                            | other -> other)
                    { block with Instrs = instrs })

            mergeNext { current with Blocks = blocks } true

    mergeNext cfg false

/// CFG Simplification: Remove empty blocks (just a jump)
let simplifyEmptyBlocks (cfg: CFG) : CFG * bool =
    let phiSourceLabels =
        cfg.Blocks
        |> Map.fold (fun labels _ block ->
            block.Instrs
            |> List.fold (fun current instruction ->
                match instruction with
                | Phi (_, sources, _) when List.length sources > 1 ->
                    sources
                    |> List.fold (fun acc (_, source) -> Set.add source acc) current
                | _ -> current) labels) Set.empty
    // Find blocks that only contain a Jump
    let emptyBlocks =
        cfg.Blocks
        |> Map.filter (fun label block ->
            label <> cfg.Entry &&  // Don't remove entry block
            // Distinct empty predecessors may carry different phi values.
            not (Set.contains label phiSourceLabels) &&
            List.isEmpty block.Instrs &&
            match block.Terminator with
            | Jump _ -> true
            | _ -> false
        )
        |> Map.map (fun _ block ->
            match block.Terminator with
            | Jump target -> target
            | _ -> Crash.crash "Expected Jump"
        )

    if Map.isEmpty emptyBlocks then
        (cfg, false)
    else
        let preds = buildPredecessors cfg

        // Redirect jumps through empty blocks (follow chains)
        let redirectLabel label =
            let rec follow visited current =
                if Set.contains current visited then
                    current
                else
                    match Map.tryFind current emptyBlocks with
                    | None -> current
                    | Some next -> follow (Set.add current visited) next
            follow Set.empty label

        let replacementPhiSourceLabels label =
            let rec collect visited current =
                if Set.contains current visited then
                    []
                elif Map.containsKey current emptyBlocks then
                    Map.tryFind current preds
                    |> Option.defaultValue []
                    |> List.collect (collect (Set.add current visited))
                else
                    [current]

            match collect Set.empty label |> List.distinct with
            | [] -> Crash.crash $"simplifyEmptyBlocks: no remaining predecessor for phi source {label}"
            | labels -> labels

        let blocks' =
            cfg.Blocks
            |> Map.filter (fun label _ -> not (Map.containsKey label emptyBlocks))
            |> Map.map (fun _ block ->
                let term' =
                    match block.Terminator with
                    | Jump target -> Jump (redirectLabel target)
                    | Branch (cond, trueLabel, falseLabel) ->
                        Branch (cond, redirectLabel trueLabel, redirectLabel falseLabel)
                    | Ret op -> Ret op

                // Also update phi sources
                let instrs' =
                    block.Instrs
                    |> List.map (fun instr ->
                        match instr with
                        | Phi (dest, sources, valueType) ->
                            let sources' =
                                sources
                                |> List.collect (fun (op, lbl) ->
                                    replacementPhiSourceLabels lbl
                                    |> List.map (fun replacement -> (op, replacement)))
                            Phi (dest, sources', valueType)
                        | other -> other
                    )

                { block with Instrs = instrs'; Terminator = term' }
            )

        ({ cfg with Blocks = blocks' }, true)

/// Simplify join blocks that only return a phi-selected value.
/// Pattern:
///   pred1: ...; Jump join
///   pred2: ...; Jump join
///   join:
///     p <- Phi([(v1, pred1), (v2, pred2)])
///     Ret p
/// Becomes:
///   pred1: ...; Ret v1
///   pred2: ...; Ret v2
/// and removes `join`.
let private simplifyRetPhiJoinLayer (cfg: CFG) : CFG * bool =
    let potentialJoinBlocks =
        cfg.Blocks
        |> Map.toList
        |> List.filter (fun (_, block) ->
            match block.Terminator with
            | Ret (Register _) ->
                block.Instrs
                |> List.exists (function Phi _ -> true | _ -> false)
            | _ -> false)

    // Most functions have no return-phi join. Avoid constructing a complete
    // predecessor map on every fixed-point iteration until a block can
    // actually match the transformation.
    let preds =
        if List.isEmpty potentialJoinBlocks then Map.empty
        else buildPredecessors cfg

    let candidateMappings : Map<Label, Map<Label, Operand>> =
        potentialJoinBlocks
        |> List.choose (fun (joinLabel, joinBlock) ->
            match joinBlock.Terminator with
            | Ret (Register retReg) ->
                let localCopies =
                    joinBlock.Instrs
                    |> List.fold (fun copies instr ->
                        match instr with
                        | Mov (dest, source, _) -> Map.add dest source copies
                        | _ -> copies) Map.empty
                let returnedPhiDest = resolveCopy localCopies (Register retReg)
                let returnPhis, otherInstructions =
                    joinBlock.Instrs
                    |> List.partition (function
                        | Phi (phiDest, _, _) -> returnedPhiDest = Register phiDest
                        | _ -> false)
                match returnPhis with
                | [Phi (_, sources, _)]
                    when otherInstructions |> List.forall (not << hasSideEffects) ->
                    let predLabels = Map.tryFind joinLabel preds |> Option.defaultValue []
                    let predSet = predLabels |> Set.ofList
                    let sourceSet = sources |> List.map snd |> Set.ofList

                    // Require exact predecessor/source match and direct jumps to join.
                    let allJumpToJoin =
                        predLabels
                        |> List.forall (fun predLabel ->
                            match Map.tryFind predLabel cfg.Blocks with
                            | Some predBlock ->
                                match predBlock.Terminator with
                                | Jump target -> target = joinLabel
                                | _ -> false
                            | None -> false)

                    if predSet = sourceSet && allJumpToJoin then
                        let sourceMap = sources |> List.map (fun (op, lbl) -> (lbl, op)) |> Map.ofList
                        Some (joinLabel, sourceMap)
                    else
                        None
                | _ ->
                    None
            | _ ->
                None)
        |> Map.ofList

    if Map.isEmpty candidateMappings then
        (cfg, false)
    else
        let allJoinLabels = candidateMappings |> Map.keys |> Set.ofSeq
        let allPredLabels =
            candidateMappings
            |> Map.values
            |> Seq.collect Map.keys
            |> Set.ofSeq

        let blocks' =
            cfg.Blocks
            |> Map.filter (fun label _ -> not (Set.contains label allJoinLabels))
            |> Map.map (fun label block ->
                if Set.contains label allPredLabels then
                    // A predecessor may feed multiple candidate joins only in impossible CFGs
                    // (single terminator), so pick the matching join by current terminator.
                    match block.Terminator with
                    | Jump target ->
                        match Map.tryFind target candidateMappings with
                        | Some sourceMap ->
                            match Map.tryFind label sourceMap with
                            | Some retOp ->
                                { block with Terminator = Ret retOp }
                            | None ->
                                block
                        | None ->
                            block
                    | _ ->
                        block
                else
                    block)

        ({ cfg with Blocks = blocks' }, true)

let simplifyRetPhiJoins (cfg: CFG) : CFG * bool =
    let rec collapse current changed =
        match simplifyRetPhiJoinLayer current with
        | next, true -> collapse next true
        | next, false -> (next, changed)
    collapse cfg false

/// Simplify branches whose target is independent of their condition
let simplifyConstantBranches (cfg: CFG) : CFG * bool =
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let term' =
                match block.Terminator with
                | Branch (_, trueLabel, falseLabel) when trueLabel = falseLabel ->
                    Jump trueLabel
                | Branch (BoolConst true, trueLabel, _) -> Jump trueLabel
                | Branch (BoolConst false, _, falseLabel) -> Jump falseLabel
                | other -> other
            let changed' = ch || term' <> block.Terminator
            (Map.add label { block with Terminator = term' } acc, changed')
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Remove unreachable blocks and trim phi sources from removed predecessor edges.
let eliminateUnreachableBlocks (cfg: CFG) : CFG * bool =
    let succs = buildSuccessors cfg

    let rec walk (work: Label list) (visited: Set<Label>) : Set<Label> =
        match work with
        | [] -> visited
        | label :: rest ->
            if Set.contains label visited then
                walk rest visited
            else
                let next = Map.tryFind label succs |> Option.defaultValue []
                walk (next @ rest) (Set.add label visited)

    let reachable = walk [cfg.Entry] Set.empty

    let reachableBlocks =
        cfg.Blocks
        |> Map.filter (fun label _ -> Set.contains label reachable)

    let reachablePredecessors =
        buildPredecessors { cfg with Blocks = reachableBlocks }

    let (blocks', phiChanged) =
        reachableBlocks
        |> Map.fold (fun (acc, ch) label block ->
            let actualPredecessors =
                Map.tryFind label reachablePredecessors
                |> Option.defaultValue []
                |> Set.ofList

            let (instrs', instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', ch') instr ->
                    match instr with
                    | Phi (dest, sources, valueType) ->
                        let sources' =
                            sources
                            |> List.filter (fun (_, srcLabel) -> Set.contains srcLabel actualPredecessors)
                        if List.isEmpty sources' then
                            Crash.crash $"Phi in {label} has no predecessor sources after CFG prune"
                        let instr' = Phi (dest, sources', valueType)
                        (instr' :: acc', ch' || sources' <> sources)
                    | _ ->
                        (instr :: acc', ch')
                ) ([], false)
            let instrs' = List.rev instrs'
            (Map.add label { block with Instrs = instrs' } acc, ch || instrChanged)
        ) (Map.empty, false)

    let removedBlocks = Map.count cfg.Blocks <> Map.count blocks'
    ({ cfg with Blocks = blocks' }, removedBlocks || phiChanged)
