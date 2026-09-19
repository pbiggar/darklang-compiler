// LoopInvariantMotion.fs - Build preheaders and hoist proven loop-invariant operations.

module MIRLoopInvariantMotion

open MIR
open SSA_Construction
open MIROptimizationFacts
open MIRLoopTopology
open MIRInduction
open MIRUnrolling

/// Scalar results can move across loop iterations without changing ownership.
let private isScalarReturnType (returnType: AST.Type) : bool =
    isScalarValueType returnType

/// Check if an instruction is safe to hoist out of a loop.
let private isHoistableInstrWithEffectFreeCalls
    (effectFreeFunctions: Set<AST.FunctionId>)
    (instr: Instr)
    : bool =
    match instr with
    | BinOp _ -> true
    | UnaryOp _ -> true
    | Call (_, funcName, _, _, returnType) ->
        Set.contains funcName effectFreeFunctions && isScalarReturnType returnType
    | HeapLoad _ -> true
    | FloatSqrt _ -> true
    | FloatAbs _ -> true
    | FloatNeg _ -> true
    | Int64ToFloat _ -> true
    | FloatToInt64 _ -> true
    | FloatToBits _ -> true
    | _ -> false

let isHoistableInstr (instr: Instr) : bool =
    isHoistableInstrWithEffectFreeCalls Set.empty instr

/// Create preheaders only for reducible loops whose direct invariant work can use one.
///
/// A header entered from several edges cannot receive LICM output directly: any
/// hoisted definition would not dominate all entries.  This normalizes those entry
/// edges to one block and preserves SSA by merging each header phi's outside values
/// in a new preheader phi.  Existing simple preheaders are deliberately unchanged.
let private canonicalizeLoopPreheaders
    (effectFreeFunctions: Set<AST.FunctionId>)
    (topology: LoopTopology)
    (cfg: CFG)
    : CFG * bool =
    let labelName (Label name) = name
    let freshPreheaderLabel (cfg': CFG) (Label headerName) =
        let rec choose index =
            let suffix = if index = 0 then "preheader" else $"preheader_{index}"
            let candidate = Label $"{headerName}_{suffix}"
            if Map.containsKey candidate cfg'.Blocks then choose (index + 1) else candidate
        choose 0

    let rewriteTarget header preheader terminator =
        match terminator with
        | Jump target when target = header -> Jump preheader
        | Branch (condition, trueLabel, falseLabel) ->
            let trueLabel' = if trueLabel = header then preheader else trueLabel
            let falseLabel' = if falseLabel = header then preheader else falseLabel
            Branch (condition, trueLabel', falseLabel')
        | _ -> terminator

    let loops = topology.Loops
    loops
    |> Map.toList
    |> List.sortBy (fun (header, loopBlocks) -> (Set.count loopBlocks, labelName header))
    |> List.fold (fun (cfgAcc, predecessors, changedAcc) (header, loopBlocks) ->
        let outsidePreds =
            Map.tryFind header predecessors
            |> Option.defaultValue []
            |> List.filter (fun pred -> not (Set.contains pred loopBlocks))
            |> List.distinct
            |> List.sortBy labelName

        let hasSimplePreheader =
            match outsidePreds with
            | [preheader] ->
                match Map.tryFind preheader cfgAcc.Blocks with
                | Some { Terminator = Jump target } when target = header -> true
                | _ -> false
            | _ -> false

        let loopDefs =
            loopBlocks
            |> Set.fold (fun defs label ->
                match Map.tryFind label cfgAcc.Blocks with
                | Some block ->
                    block.Instrs
                    |> List.fold (fun defs' instr ->
                        match getInstrDest instr with
                        | Some dest -> Set.add dest defs'
                        | None -> defs') defs
                | None -> defs) Set.empty

        let nestedLoopBlocks =
            loops
            |> Map.fold (fun nestedBlocks nestedHeader nestedLoop ->
                if nestedHeader <> header && Set.isSubset nestedLoop loopBlocks then
                    Set.union nestedBlocks nestedLoop
                else
                    nestedBlocks) Set.empty

        let hasDirectInvariant =
            Set.difference loopBlocks nestedLoopBlocks
            |> Set.exists (fun label ->
                match Map.tryFind label cfgAcc.Blocks with
                | Some block ->
                    block.Instrs
                    |> List.exists (fun instr ->
                        match getInstrDest instr with
                        | Some _ ->
                            isHoistableInstrWithEffectFreeCalls effectFreeFunctions instr
                            && (getInstrUses instr |> Set.forall (fun usedRegister -> not (Set.contains usedRegister loopDefs)))
                        | None -> false)
                | None -> false)

        if List.isEmpty outsidePreds || hasSimplePreheader || not hasDirectInvariant then
            (cfgAcc, predecessors, changedAcc)
        else
            match Map.tryFind header cfgAcc.Blocks with
            | None -> (cfgAcc, predecessors, changedAcc)
            | Some headerBlock ->
                let preheader = freshPreheaderLabel cfgAcc header
                let nextRegister = nextRegisterId cfgAcc
                let (preheaderPhisRev, headerInstrsRev, _) =
                    headerBlock.Instrs
                    |> List.fold (fun (prePhis, rewritten, registerId) instr ->
                        match instr with
                        | Phi (dest, sources, valueType) ->
                            let outsideSources =
                                sources |> List.filter (fun (_, source) -> List.contains source outsidePreds)
                            let insideSources =
                                sources |> List.filter (fun (_, source) -> not (List.contains source outsidePreds))
                            match outsideSources with
                            | [] -> (prePhis, instr :: rewritten, registerId)
                            | _ ->
                                let merged = VReg registerId
                                let prePhi = Phi (merged, outsideSources, valueType)
                                let rewrittenPhi = Phi (dest, (Register merged, preheader) :: insideSources, valueType)
                                (prePhi :: prePhis, rewrittenPhi :: rewritten, registerId + 1)
                        | _ -> (prePhis, instr :: rewritten, registerId)) ([], [], nextRegister)
                let preheaderBlock = {
                    Label = preheader
                    Instrs = List.rev preheaderPhisRev
                    Terminator = Jump header
                }
                let blocks =
                    cfgAcc.Blocks
                    |> Map.map (fun label block ->
                        if List.contains label outsidePreds then
                            { block with Terminator = rewriteTarget header preheader block.Terminator }
                        elif label = header then
                            { block with Instrs = List.rev headerInstrsRev }
                        else
                            block)
                    |> Map.add preheader preheaderBlock
                let cfg' = { cfgAcc with Blocks = blocks }
                (cfg', buildPredecessors cfg', true)
    ) (cfg, topology.Predecessors, false)
    |> fun (canonicalizedCFG, _, changed) -> (canonicalizedCFG, changed)

/// Apply loop-invariant code motion for loops with a simple preheader.
let internal applyLoopInvariantCodeMotionWithEffectFreeCalls
    (effectFreeFunctions: Set<AST.FunctionId>)
    (topology: LoopTopology)
    (cfg: CFG)
    : CFG * bool * LoopTopology =
    let (cfgWithPreheaders, canonicalized) =
        canonicalizeLoopPreheaders effectFreeFunctions topology cfg
    let topologyWithPreheaders =
        if canonicalized then
            match tryBuildLoopTopology cfgWithPreheaders with
            | Some updated -> updated
            | None ->
                Crash.crash
                    "LICM preheader canonicalization removed every reachable loop"
        else
            topology
    let loops = topologyWithPreheaders.Loops
    let preds = topologyWithPreheaders.Predecessors
    let labelName (Label name) = name
    let buildCopyMapForLicm (cfg': CFG) : Map<VReg, VReg> =
        let phiDests =
            cfg'.Blocks
            |> Map.fold (fun dests _ block ->
                block.Instrs
                |> List.fold (fun acc instr ->
                    match instr with
                    | Phi (dest, _, _) -> Set.add dest acc
                    | _ -> acc
                ) dests
            ) Set.empty

        cfg'.Blocks
        |> Map.fold (fun acc _ block ->
            block.Instrs
            |> List.fold (fun mapAcc instr ->
                match instr with
                | Mov (dest, Register src, _) when dest <> src ->
                    if Set.contains dest phiDests || Map.containsKey dest mapAcc then mapAcc
                    else Map.add dest src mapAcc
                | _ -> mapAcc
            ) acc
        ) Map.empty

    let resolveCopyForLicm (copyMap: Map<VReg, VReg>) (op: Operand) : Operand =
        let rec resolve visited op' =
            match op' with
            | Register vreg ->
                if Set.contains vreg visited then
                    op'
                else
                    match Map.tryFind vreg copyMap with
                    | Some src -> resolve (Set.add vreg visited) (Register src)
                    | None -> op'
            | _ -> op'
        resolve Set.empty op

    loops
    |> Map.fold (fun (cfgAcc, changedAcc) header loopBlocks ->
        let copyMap = buildCopyMapForLicm cfgAcc
        let outsidePreds =
            Map.tryFind header preds
            |> Option.defaultValue []
            |> List.filter (fun pred -> not (Set.contains pred loopBlocks))

        let tryGetPreheader =
            match outsidePreds with
            | [preheader] ->
                match Map.tryFind preheader cfgAcc.Blocks with
                | Some block ->
                    match block.Terminator with
                    | Jump target when target = header -> Some preheader
                    | _ -> None
                | None -> None
            | _ -> None

        match tryGetPreheader with
        | None -> (cfgAcc, changedAcc)
        | Some preheader ->
            let loopDefs =
                loopBlocks
                |> Set.fold (fun defs label ->
                    match Map.tryFind label cfgAcc.Blocks with
                    | None -> defs
                    | Some block ->
                        block.Instrs
                        |> List.fold (fun defs' instr ->
                            match getInstrDest instr with
                            | Some dest -> Set.add dest defs'
                            | None -> defs'
                        ) defs
                ) Set.empty

            let blockOrder =
                header :: (loopBlocks |> Set.remove header |> Set.toList |> List.sortBy labelName)

            let resolveOp (op: Operand) : Operand =
                resolveCopyForLicm copyMap op

            let resolveInvariantOperand (invariantMap: Map<VReg, Operand>) (op: Operand) : Operand =
                let rec resolve visited op' =
                    match op' with
                    | Register vreg ->
                        if Set.contains vreg visited then
                            op'
                        else
                            match Map.tryFind vreg invariantMap with
                            | Some mapped -> resolve (Set.add vreg visited) mapped
                            | None -> op'
                    | _ -> op'
                resolve Set.empty op

            let rec findInvariantPhis (current: Map<VReg, Operand>) : Map<VReg, Operand> =
                let next =
                    loopBlocks
                    |> Set.fold (fun acc label ->
                        match Map.tryFind label cfgAcc.Blocks with
                        | None -> acc
                        | Some block ->
                            block.Instrs
                            |> List.fold (fun acc' instr ->
                                match instr with
                                | Phi (dest, sources, _) ->
                                    let sources' =
                                        sources
                                        |> List.map (fun (op, lbl) ->
                                            (resolveInvariantOperand acc' (resolveOp op), lbl))
                                    let outsideSources =
                                        sources'
                                        |> List.filter (fun (_, lbl) -> not (Set.contains lbl loopBlocks))
                                    let insideSources =
                                        sources'
                                        |> List.filter (fun (_, lbl) -> Set.contains lbl loopBlocks)
                                    match outsideSources with
                                    | [] -> acc'
                                    | (outsideOp, _) :: rest ->
                                        if rest |> List.forall (fun (op, _) -> op = outsideOp) then
                                            let outsideInvariant =
                                                match outsideOp with
                                                | Register vreg ->
                                                    not (Set.contains vreg loopDefs) || Map.containsKey vreg acc'
                                                | _ -> true
                                            let insideOk =
                                                insideSources
                                                |> List.forall (fun (op, _) ->
                                                    match op with
                                                    | Register vreg when vreg = dest -> true
                                                    | _ -> op = outsideOp
                                                )
                                            if outsideInvariant && insideOk then Map.add dest outsideOp acc' else acc'
                                        else
                                            acc'
                                | _ -> acc'
                            ) acc
                    ) current
                if next = current then current else findInvariantPhis next

            let invariantPhiMap = findInvariantPhis Map.empty
            let invariantPhis = invariantPhiMap |> Map.toList |> List.map fst |> Set.ofList

            let rewriteInvariantInstr (instr: Instr) : Instr =
                let rewriteOperand op = resolveInvariantOperand invariantPhiMap op
                match instr with
                | BinOp (dest, op, left, right, operandType) ->
                    BinOp (dest, op, rewriteOperand left, rewriteOperand right, operandType)
                | UnaryOp (dest, op, src) ->
                    UnaryOp (dest, op, rewriteOperand src)
                | Call (dest, funcName, args, argTypes, returnType) ->
                    Call (dest, funcName, List.map rewriteOperand args, argTypes, returnType)
                | HeapLoad (dest, addr, offset, vt) ->
                    match rewriteOperand (Register addr) with
                    | Register addr' -> HeapLoad (dest, addr', offset, vt)
                    | _ -> Crash.crash "LICM: HeapLoad address should remain a register"
                | FloatSqrt (dest, src) -> FloatSqrt (dest, rewriteOperand src)
                | FloatAbs (dest, src) -> FloatAbs (dest, rewriteOperand src)
                | FloatNeg (dest, src) -> FloatNeg (dest, rewriteOperand src)
                | Int64ToFloat (dest, src) -> Int64ToFloat (dest, rewriteOperand src)
                | FloatToInt64 (dest, src) -> FloatToInt64 (dest, rewriteOperand src)
                | FloatToBits (dest, src) -> FloatToBits (dest, rewriteOperand src)
                | _ -> instr

            let rec findHoistable invariants hoistMap =
                let (invariants', hoistMap', changed) =
                    blockOrder
                    |> List.fold (fun (invAcc, mapAcc, chAcc) label ->
                        match Map.tryFind label cfgAcc.Blocks with
                        | None -> (invAcc, mapAcc, chAcc)
                        | Some block ->
                            let (blockHoists, invAcc', blockChanged) =
                                block.Instrs
                                |> List.fold (fun (hoists, invs, ch) instr ->
                                    match getInstrDest instr with
                                    | None -> (hoists, invs, ch)
                                    | Some dest ->
                                        let usesInvariant =
                                            getInstrUses instr
                                            |> Set.forall (fun vreg ->
                                                not (Set.contains vreg loopDefs) || Set.contains vreg invs
                                            )
                                        if Set.contains dest invs then
                                            (hoists, invs, ch)
                                        elif isHoistableInstrWithEffectFreeCalls effectFreeFunctions instr && usesInvariant then
                                            (hoists @ [instr], Set.add dest invs, true)
                                        else
                                            (hoists, invs, ch)
                                ) ([], invAcc, false)
                            let mapAcc' =
                                if List.isEmpty blockHoists then mapAcc
                                else
                                    let existing = Map.tryFind label mapAcc |> Option.defaultValue []
                                    Map.add label (existing @ blockHoists) mapAcc
                            (invAcc', mapAcc', chAcc || blockChanged)
                    ) (invariants, hoistMap, false)

                if changed then findHoistable invariants' hoistMap' else (invariants', hoistMap')

            let (_, hoistMap) = findHoistable invariantPhis Map.empty
            if Map.isEmpty hoistMap then
                (cfgAcc, changedAcc)
            else
                let hoistedInstrs =
                    blockOrder
                    |> List.collect (fun label -> Map.tryFind label hoistMap |> Option.defaultValue [])
                    |> List.map rewriteInvariantInstr

                let blocks' =
                    cfgAcc.Blocks
                    |> Map.map (fun label block ->
                        if label = preheader then
                            { block with Instrs = block.Instrs @ hoistedInstrs }
                        elif Set.contains label loopBlocks then
                            let hoistedDests =
                                Map.tryFind label hoistMap
                                |> Option.defaultValue []
                                |> List.choose getInstrDest
                                |> Set.ofList
                            let instrs' =
                                block.Instrs
                                |> List.filter (fun instr ->
                                    match getInstrDest instr with
                                    | Some dest -> not (Set.contains dest hoistedDests)
                                    | None -> true)
                            { block with Instrs = instrs' }
                        else
                            block
                    )

                ({ cfgAcc with Blocks = blocks' }, true)
    ) (cfgWithPreheaders, canonicalized)
    |> fun (optimizedCFG, changed) ->
        (optimizedCFG, changed, topologyWithPreheaders)

let applyLoopInvariantCodeMotion (cfg: CFG) : CFG * bool =
    match tryBuildLoopTopology cfg with
    | None -> (cfg, false)
    | Some topology ->
        let (optimized, changed, _) =
            applyLoopInvariantCodeMotionWithEffectFreeCalls
                Set.empty
                topology
                cfg
        (optimized, changed)
