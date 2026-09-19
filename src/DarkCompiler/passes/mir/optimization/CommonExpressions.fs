// CommonExpressions.fs - Reuse and path-complete scalar expressions under effect constraints.

module MIRCommonExpressions

open MIR
open SSA_Construction
open MIRLoopTopology
/// Common Subexpression Elimination (CSE)
/// Detect identical computations and replace with reference to first result

/// Expression key for CSE - represents a pure computation
type ExprKey =
    | BinExpr of BinOp * Operand * Operand * AST.Type
    | UnaryExpr of UnaryOp * Operand
    | ScalarHeapLoadExpr of VReg * int * AST.Type
    | DirectCallExpr of funcName:AST.FunctionId * args:Operand list * returnType:AST.Type

type private ExprAvailability = {
    Arithmetic: Map<ExprKey, VReg>
    ScalarHeapLoads: Map<ExprKey, VReg>
    DirectCalls: Map<ExprKey, VReg>
}

let private emptyExprAvailability = {
    Arithmetic = Map.empty
    ScalarHeapLoads = Map.empty
    DirectCalls = Map.empty
}

let private tryFindAvailable
    (key: ExprKey)
    (available: ExprAvailability)
    : VReg option =
    match key with
    | BinExpr _
    | UnaryExpr _ -> Map.tryFind key available.Arithmetic
    | ScalarHeapLoadExpr _ -> Map.tryFind key available.ScalarHeapLoads
    | DirectCallExpr _ -> Map.tryFind key available.DirectCalls

let private addAvailable
    (key: ExprKey)
    (dest: VReg)
    (available: ExprAvailability)
    : ExprAvailability =
    match key with
    | BinExpr _
    | UnaryExpr _ ->
        { available with Arithmetic = Map.add key dest available.Arithmetic }
    | ScalarHeapLoadExpr _ ->
        { available with ScalarHeapLoads = Map.add key dest available.ScalarHeapLoads }
    | DirectCallExpr _ ->
        { available with DirectCalls = Map.add key dest available.DirectCalls }

/// Check if a binary operation is commutative (order of operands doesn't matter)
let isCommutative (op: BinOp) : bool =
    match op with
    | Add | Mul | And | Or | Eq | Neq | BitAnd | BitOr | BitXor -> true
    | Sub | Div | Mod | Lt | Gt | Lte | Gte | Shl | Shr -> false

/// Normalize operand order for commutative operations (for consistent hashing)
let normalizeOperands (op: BinOp) (left: Operand) (right: Operand) : Operand * Operand =
    if isCommutative op then
        // Use structural comparison to ensure consistent ordering
        if compare left right <= 0 then (left, right) else (right, left)
    else
        (left, right)

/// Build expression key for a BinOp
let makeBinExprKey (op: BinOp) (left: Operand) (right: Operand) (opType: AST.Type) : ExprKey =
    let (l, r) = normalizeOperands op left right
    BinExpr (op, l, r, opType)

/// Build expression key for a UnaryOp
let makeUnaryExprKey (op: UnaryOp) (src: Operand) : ExprKey =
    UnaryExpr (op, src)

/// Build an availability key for an exact typed scalar heap load.
let makeScalarHeapLoadExprKey (addr: VReg) (offset: int) (valueType: AST.Type) : ExprKey =
    ScalarHeapLoadExpr (addr, offset, valueType)

let private isCrossBlockCSEType (opType: AST.Type) : bool =
    match opType with
    | AST.TInt64 | AST.TInt32 | AST.TInt16 | AST.TInt8
    | AST.TUInt64 | AST.TUInt32 | AST.TUInt16 | AST.TUInt8
    | AST.TFloat64 | AST.TBool | AST.TChar | AST.TDateTime -> true
    | _ -> false

/// Calls, memory operations, and ownership operations invalidate heap-load
/// availability without discarding independent arithmetic expression keys.
let private clearScalarHeapLoadAvailability
    (available: ExprAvailability)
    : ExprAvailability =
    { available with ScalarHeapLoads = Map.empty }

let private clearDirectCallAvailability
    (available: ExprAvailability)
    : ExprAvailability =
    { available with DirectCalls = Map.empty }

let private clearHeapLoadAndDirectCallAvailability
    (available: ExprAvailability)
    : ExprAvailability =
    { available with
        ScalarHeapLoads = Map.empty
        DirectCalls = Map.empty }

type private PartialRedundancyCandidate = {
    Block: Label
    Dest: VReg
    Key: ExprKey
    Instr: Instr
    ValueType: AST.Type option
    Operands: Operand list
}

let private tryPartialRedundancyCandidate
    (block: Label)
    (instr: Instr)
    : PartialRedundancyCandidate option =
    match instr with
    | BinOp (dest, op, left, right, opType)
        when isCrossBlockCSEType opType && op <> Div && op <> Mod ->
        Some {
            Block = block
            Dest = dest
            Key = makeBinExprKey op left right opType
            Instr = instr
            ValueType = Some opType
            Operands = [left; right]
        }
    | UnaryOp (dest, op, src) ->
        Some {
            Block = block
            Dest = dest
            Key = makeUnaryExprKey op src
            Instr = instr
            ValueType = None
            Operands = [src]
        }
    | _ -> None

let private replaceInstrWithPhi
    (candidate: PartialRedundancyCandidate)
    (sources: (Operand * Label) list)
    (block: BasicBlock)
    : BasicBlock =
    let isPhi = function | Phi _ -> true | _ -> false
    let phis = block.Instrs |> List.takeWhile isPhi
    let remaining = block.Instrs |> List.skipWhile isPhi
    let remaining' =
        remaining
        |> List.filter (function
            | BinOp (dest, _, _, _, _)
            | UnaryOp (dest, _, _) -> dest <> candidate.Dest
            | _ -> true)
    { block with
        Instrs = phis @ [Phi (candidate.Dest, sources, candidate.ValueType)] @ remaining' }

let private withDest
    (dest: VReg)
    (instr: Instr)
    : Instr =
    match instr with
    | BinOp (_, op, left, right, opType) -> BinOp (dest, op, left, right, opType)
    | UnaryOp (_, op, src) -> UnaryOp (dest, op, src)
    | _ -> Crash.crash "MIR PRE: candidate is not an arithmetic expression"

let private maxVRegId (VReg id) (currentMax: int) : int =
    max id currentMax

/// Complete expressions that are available on only some incoming paths. The
/// insertion boundary is an unconditional edge into the join, so PRE neither
/// speculates work onto another successor nor changes when trapping operations
/// run. Expressions depending on join-local definitions are not movable.
let private applyPartialRedundancyElimination
    (exitAvailability: Map<Label, ExprAvailability>)
    (cfg: CFG)
    : CFG * bool =
    let predecessors = buildPredecessors cfg
    let candidates =
        cfg.Blocks
        |> Map.toList
        |> List.collect (fun (label, block) ->
            block.Instrs |> List.choose (tryPartialRedundancyCandidate label))
    let initialMaxReg =
        cfg.Blocks
        |> Map.fold (fun currentMax _ block ->
            Set.union (getBlockDefs block) (getBlockUses block)
            |> Set.fold (fun maxId reg -> maxVRegId reg maxId) currentMax
        ) -1

    let rec applyCandidates
        (remaining: PartialRedundancyCandidate list)
        (blocks: Map<Label, BasicBlock>)
        (availability: Map<Label, ExprAvailability>)
        (nextRegId: int)
        (changed: bool)
        : Map<Label, BasicBlock> * bool =
        match remaining with
        | [] -> (blocks, changed)
        | candidate :: rest ->
            let block =
                Map.tryFind candidate.Block blocks
                |> Option.defaultWith (fun () -> Crash.crash $"MIR PRE: missing block {candidate.Block}")
            let localDefs = getBlockDefs block
            let usesJoinLocalDefinition =
                candidate.Operands
                |> List.exists (function
                    | Register reg -> Set.contains reg localDefs
                    | _ -> false)
            let incoming =
                Map.tryFind candidate.Block predecessors
                |> Option.defaultValue []
                |> List.distinct
            let incomingAvailability =
                incoming
                |> List.map (fun predecessor ->
                    let available =
                        Map.tryFind predecessor availability
                        |> Option.bind (tryFindAvailable candidate.Key)
                    (predecessor, available))
            let hasAvailablePath =
                incomingAvailability |> List.exists (snd >> Option.isSome)
            let missingPathsCanInsert =
                incomingAvailability
                |> List.forall (fun (predecessor, available) ->
                    match available, Map.tryFind predecessor blocks with
                    | Some _, _ -> true
                    | None, Some predecessorBlock ->
                        predecessorBlock.Terminator = Jump candidate.Block
                    | None, None -> false)

            if incoming.Length < 2
               || usesJoinLocalDefinition
               || not hasAvailablePath
               || not missingPathsCanInsert then
                applyCandidates rest blocks availability nextRegId changed
            else
                let (blocks', availability', nextRegId', sourcesRev) =
                    incomingAvailability
                    |> List.fold (fun (currentBlocks, currentAvailability, freshId, sources) (predecessor, available) ->
                        match available with
                        | Some reg ->
                            (currentBlocks,
                             currentAvailability,
                             freshId,
                             (Register reg, predecessor) :: sources)
                        | None ->
                            let insertedDest = VReg freshId
                            let predecessorBlock =
                                Map.tryFind predecessor currentBlocks
                                |> Option.defaultWith (fun () -> Crash.crash $"MIR PRE: missing predecessor {predecessor}")
                            let predecessorBlock' =
                                { predecessorBlock with
                                    Instrs = predecessorBlock.Instrs @ [withDest insertedDest candidate.Instr] }
                            let predecessorAvailability =
                                Map.tryFind predecessor currentAvailability
                                |> Option.defaultValue emptyExprAvailability
                                |> addAvailable candidate.Key insertedDest
                            (Map.add predecessor predecessorBlock' currentBlocks,
                             Map.add predecessor predecessorAvailability currentAvailability,
                             freshId + 1,
                             (Register insertedDest, predecessor) :: sources)
                    ) (blocks, availability, nextRegId, [])
                let joinBlock =
                    Map.tryFind candidate.Block blocks'
                    |> Option.defaultWith (fun () -> Crash.crash $"MIR PRE: missing join {candidate.Block}")
                let joinBlock' = replaceInstrWithPhi candidate (List.rev sourcesRev) joinBlock
                applyCandidates
                    rest
                    (Map.add candidate.Block joinBlock' blocks')
                    availability'
                    nextRegId'
                    true

    let (blocks, changed) =
        applyCandidates candidates cfg.Blocks exitAvailability (initialMaxReg + 1) false
    ({ cfg with Blocks = blocks }, changed)

/// Apply CSE and PRE to a CFG, carrying available expressions into dominated
/// blocks and completing safe expressions at joins.
let internal applyCSEWithEffectFreeCallsAndTopology
    (existingTopology: DominatorTopology option)
    (effectFreeFunctions: Set<AST.FunctionId>)
    (cfg: CFG)
    : CFG * bool * DominatorTopology =
    let optimizeBlock
        (available: ExprAvailability)
        (block: BasicBlock)
        : BasicBlock * ExprAvailability * bool =
        let (instrs', _, exported', changed) =
            block.Instrs
            |> List.fold (fun (instrs, exprMap, exported, ch) instr ->
                match instr with
                | BinOp (dest, op, left, right, opType) ->
                    let key = makeBinExprKey op left right opType
                    let available' =
                        if isCrossBlockCSEType opType then
                            exprMap
                        else
                            clearScalarHeapLoadAvailability exprMap
                    match tryFindAvailable key available' with
                    | Some prevDest ->
                        (Mov (dest, Register prevDest, None) :: instrs, available', exported, true)
                    | None ->
                        let exported' =
                            if isCrossBlockCSEType opType then addAvailable key dest exported
                            else emptyExprAvailability
                        (instr :: instrs, addAvailable key dest available', exported', ch)
                | UnaryOp (dest, op, src) ->
                    let key = makeUnaryExprKey op src
                    match tryFindAvailable key exprMap with
                    | Some prevDest ->
                        (Mov (dest, Register prevDest, None) :: instrs, exprMap, exported, true)
                    | None ->
                        (instr :: instrs, addAvailable key dest exprMap, addAvailable key dest exported, ch)
                | HeapLoad (dest, addr, offset, Some valueType) when isCrossBlockCSEType valueType ->
                    let key = makeScalarHeapLoadExprKey addr offset valueType
                    match tryFindAvailable key exprMap with
                    | Some prevDest ->
                        (Mov (dest, Register prevDest, Some valueType) :: instrs, exprMap, exported, true)
                    | None ->
                        (instr :: instrs, addAvailable key dest exprMap, addAvailable key dest exported, ch)
                | HeapLoad _ ->
                    // Unknown and non-scalar values can carry ownership edges;
                    // do not make earlier scalar loads available past them.
                    (instr :: instrs, clearScalarHeapLoadAvailability exprMap, emptyExprAvailability, ch)
                | Call (dest, funcName, args, _, returnType)
                    when Set.contains funcName effectFreeFunctions
                         && isCrossBlockCSEType returnType ->
                    let key = DirectCallExpr (funcName, args, returnType)
                    match tryFindAvailable key exprMap with
                    | Some prevDest ->
                        // Exact callee, operand, and scalar-result identity plus
                        // the whole-program effect proof make reuse safe.
                        (Mov (dest, Register prevDest, Some returnType) :: instrs, exprMap, exported, true)
                    | None ->
                        // Keep only the current call available across a call
                        // boundary; independent arithmetic and safe loads remain.
                        let exprMap' =
                            exprMap
                            |> clearDirectCallAvailability
                            |> addAvailable key dest
                        let exported' =
                            exported
                            |> clearDirectCallAvailability
                            |> addAvailable key dest
                        (instr :: instrs, exprMap', exported', ch)
                | Call _ ->
                    // Unproven calls may affect memory and observable state.
                    (instr :: instrs, clearHeapLoadAndDirectCallAvailability exprMap, emptyExprAvailability, ch)
                | RefCountDec _
                | RefCountDecString _
                | RefCountDecBlob _
                | RawFree _
                | MappedFree _ ->
                    // A previously computed raw address can outlive its managed
                    // owner if reuse removes the later use that kept it alive.
                    (instr :: instrs, emptyExprAvailability, emptyExprAvailability, ch)
                | Mov (_, _, Some valueType) when not (isCrossBlockCSEType valueType) ->
                    (instr :: instrs, clearScalarHeapLoadAvailability exprMap, emptyExprAvailability, ch)
                | Phi (_, _, Some valueType) when not (isCrossBlockCSEType valueType) ->
                    (instr :: instrs, clearScalarHeapLoadAvailability exprMap, emptyExprAvailability, ch)
                | Mov _
                | Phi _ ->
                    (instr :: instrs, exprMap, exported, ch)
                | FloatSqrt _
                | FloatAbs _
                | FloatNeg _
                | Int64ToFloat _
                | FloatToInt64 _
                | FloatToBits _ ->
                    // Pure scalar instructions cannot affect memory, so exact
                    // scalar loads remain reusable locally. Preserve the bounded
                    // direct-call and cross-block live-range policy.
                    (instr :: instrs, clearDirectCallAvailability exprMap, emptyExprAvailability, ch)
                | _ ->
                    // Do not extend a new cross-block live range across calls,
                    // allocations, memory operations, or other runtime lowering.
                    // Local CSE remains available through exprMap.
                    (instr :: instrs, clearHeapLoadAndDirectCallAvailability exprMap, emptyExprAvailability, ch)
            ) ([], available, available, false)

        ({ block with Instrs = List.rev instrs' }, exported', changed)

    let dominatorTopology =
        existingTopology |> Option.defaultWith (fun () -> buildDominatorTopology cfg)
    let idoms = dominatorTopology.ImmediateDominators
    let dominatorChildren =
        idoms
        |> Map.fold (fun children child parent ->
            let existing = Map.tryFind parent children |> Option.defaultValue []
            Map.add parent (child :: existing) children
        ) Map.empty

    // Each child receives expressions available from its dominators. Availability
    // is cleared by the barriers above, and the same immutable map is passed to
    // siblings so expressions never flow between non-dominating paths.
    let rec optimizeDominatorSubtree
        (available: ExprAvailability)
        (label: Label)
        (blocks: Map<Label, BasicBlock>, exits: Map<Label, ExprAvailability>, changed: bool)
        : Map<Label, BasicBlock> * Map<Label, ExprAvailability> * bool =
        match Map.tryFind label cfg.Blocks with
        | None -> Crash.crash $"MIR CSE: missing dominator-tree block {label}"
        | Some block ->
            let (block', available', blockChanged) = optimizeBlock available block
            let state =
                (Map.add label block' blocks,
                 Map.add label available' exits,
                 changed || blockChanged)
            let children = Map.tryFind label dominatorChildren |> Option.defaultValue []
            children
            |> List.fold (fun childState child ->
                optimizeDominatorSubtree available' child childState
            ) state

    let (reachableBlocks, reachableExits, reachableChanged) =
        optimizeDominatorSubtree emptyExprAvailability cfg.Entry (Map.empty, Map.empty, false)

    // Dominators are undefined for unreachable blocks. Retain local CSE there so
    // this transformation remains complete when invoked independently.
    let (blocks', exits, cseChanged) =
        cfg.Blocks
        |> Map.fold (fun (blocks, blockExits, ch) label block ->
            if Map.containsKey label blocks then
                (blocks, blockExits, ch)
            else
                let (block', available, blockChanged) =
                    optimizeBlock emptyExprAvailability block
                (Map.add label block' blocks,
                 Map.add label available blockExits,
                 ch || blockChanged)
        ) (reachableBlocks, reachableExits, reachableChanged)

    let cseCfg = { cfg with Blocks = blocks' }
    let (preCfg, preChanged) = applyPartialRedundancyElimination exits cseCfg

    (preCfg, cseChanged || preChanged, dominatorTopology)

let applyCSEWithEffectFreeCalls
    (effectFreeFunctions: Set<AST.FunctionId>)
    (cfg: CFG)
    : CFG * bool =
    let (optimized, changed, _) =
        applyCSEWithEffectFreeCallsAndTopology None effectFreeFunctions cfg
    (optimized, changed)

let applyCSE (cfg: CFG) : CFG * bool =
    applyCSEWithEffectFreeCalls Set.empty cfg
