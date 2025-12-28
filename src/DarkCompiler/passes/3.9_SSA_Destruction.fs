// 3.9_SSA_Destruction.fs - SSA Destruction Pass
//
// Converts MIR from SSA form back to conventional form by:
// 1. Splitting critical edges
// 2. Replacing phi nodes with copies in predecessor blocks
//
// A critical edge is an edge from a block with multiple successors
// to a block with multiple predecessors. These must be split to
// correctly place copies.

module SSA_Destruction

open MIR

/// Check if an edge is critical
/// Critical = source has multiple successors AND target has multiple predecessors
let isCriticalEdge (cfg: CFG) (preds: SSA_Construction.Predecessors) (from: Label) (toLabel: Label) : bool =
    let fromBlock = Map.find from cfg.Blocks
    let numSuccessors =
        match fromBlock.Terminator with
        | Ret _ -> 0
        | Jump _ -> 1
        | Branch _ -> 2

    let numPredecessors =
        Map.tryFind toLabel preds |> Option.defaultValue [] |> List.length

    numSuccessors > 1 && numPredecessors > 1

/// Get all edges in the CFG
let getEdges (cfg: CFG) : (Label * Label) list =
    cfg.Blocks
    |> Map.toList
    |> List.collect (fun (label, block) ->
        match block.Terminator with
        | Ret _ -> []
        | Jump target -> [(label, target)]
        | Branch (_, trueLabel, falseLabel) -> [(label, trueLabel); (label, falseLabel)]
    )

/// Split a critical edge by inserting a new block
/// Returns updated CFG and the new block's label
let splitEdge (cfg: CFG) (labelGen: LabelGen) (from: Label) (toLabel: Label) : CFG * Label * LabelGen =
    let (newLabel, labelGen') = freshLabel labelGen

    // Create new block that just jumps to target
    let newBlock = {
        Label = newLabel
        Instrs = []
        Terminator = Jump toLabel
    }

    // Update the source block's terminator to point to new block
    let fromBlock = Map.find from cfg.Blocks
    let newTerminator =
        match fromBlock.Terminator with
        | Jump _ -> Jump newLabel
        | Branch (cond, trueLabel, falseLabel) ->
            if trueLabel = toLabel && falseLabel = toLabel then
                Branch (cond, newLabel, newLabel)
            else if trueLabel = toLabel then
                Branch (cond, newLabel, falseLabel)
            else
                Branch (cond, trueLabel, newLabel)
        | Ret _ -> failwith "Cannot split edge from Ret"

    let fromBlock' = { fromBlock with Terminator = newTerminator }

    // Update phi nodes in target block to reference new block instead of from
    let toBlock = Map.find toLabel cfg.Blocks
    let toInstrs' =
        toBlock.Instrs
        |> List.map (fun instr ->
            match instr with
            | Phi (dest, sources) ->
                let sources' =
                    sources
                    |> List.map (fun (op, srcLabel) ->
                        if srcLabel = from then (op, newLabel) else (op, srcLabel)
                    )
                Phi (dest, sources')
            | other -> other
        )
    let toBlock' = { toBlock with Instrs = toInstrs' }

    let cfg' = {
        cfg with
            Blocks =
                cfg.Blocks
                |> Map.add from fromBlock'
                |> Map.add newLabel newBlock
                |> Map.add toLabel toBlock'
    }

    (cfg', newLabel, labelGen')

/// Split all critical edges in the CFG
let splitCriticalEdges (cfg: CFG) (labelGen: LabelGen) : CFG * LabelGen =
    let preds = SSA_Construction.buildPredecessors cfg
    let edges = getEdges cfg

    let criticalEdges =
        edges
        |> List.filter (fun (from, toLabel) -> isCriticalEdge cfg preds from toLabel)

    // Split each critical edge
    criticalEdges
    |> List.fold (fun (cfg', lg) (from, toLabel) ->
        // Need to recompute predecessors after each split
        let preds' = SSA_Construction.buildPredecessors cfg'
        // Check if edge is still critical (may have changed)
        if isCriticalEdge cfg' preds' from toLabel then
            let (cfg'', _, lg') = splitEdge cfg' lg from toLabel
            (cfg'', lg')
        else
            (cfg', lg)
    ) (cfg, labelGen)

/// Get phi nodes from a block
let getPhiNodes (block: BasicBlock) : Instr list =
    block.Instrs |> List.filter (fun i -> match i with Phi _ -> true | _ -> false)

/// Get non-phi instructions from a block
let getNonPhiInstrs (block: BasicBlock) : Instr list =
    block.Instrs |> List.filter (fun i -> match i with Phi _ -> false | _ -> true)

/// Insert a copy instruction at the end of a block (before terminator)
/// For phi node: dest = phi[(v1, L1), (v2, L2), ...]
/// In predecessor L1: insert "dest = v1" at end
/// Skips copies for undefined sources (original VRegs that weren't renamed)
let insertCopyInPredecessor (cfg: CFG) (predLabel: Label) (dest: VReg) (src: Operand) : CFG =
    // Skip if source equals dest (self-referential phi)
    // Also skip if source is an original (unrenamed) VReg - indicates no definition on this path
    // SSA-renamed VRegs have high numbers (>= 10000), originals have low numbers
    let skip =
        match src with
        | Register srcReg ->
            let (VReg srcN) = srcReg
            let (VReg destN) = dest
            // Skip if self-referential or if source is an original (low-numbered) VReg
            // while dest is an SSA-renamed (high-numbered) VReg
            srcReg = dest || (srcN < 10000 && destN >= 10000)
        | _ -> false

    if skip then
        cfg
    else
        let predBlock = Map.find predLabel cfg.Blocks

        // Create copy instruction (Mov with no type - will be handled by LIR)
        let copyInstr = Mov (dest, src, None)

        // Add copy at end of predecessor's instructions
        let predBlock' = { predBlock with Instrs = predBlock.Instrs @ [copyInstr] }

        { cfg with Blocks = Map.add predLabel predBlock' cfg.Blocks }

/// Replace phi nodes with copies in predecessors
let eliminatePhiNodes (cfg: CFG) : CFG =
    cfg.Blocks
    |> Map.fold (fun cfg' label block ->
        let phis = getPhiNodes block

        // For each phi node, insert copies in predecessors
        let cfg'' =
            phis
            |> List.fold (fun c phi ->
                match phi with
                | Phi (dest, sources) ->
                    sources
                    |> List.fold (fun c' (src, predLabel) ->
                        insertCopyInPredecessor c' predLabel dest src
                    ) c
                | _ -> c
            ) cfg'

        // Remove phi nodes from this block
        let nonPhis = getNonPhiInstrs block
        let block' = { block with Instrs = nonPhis }
        { cfg'' with Blocks = Map.add label block' cfg''.Blocks }
    ) cfg

/// Convert a function out of SSA form
let destructSSAInFunction (func: Function) (labelGen: LabelGen) : Function * LabelGen =
    // Split critical edges
    let (cfg', labelGen') = splitCriticalEdges func.CFG labelGen

    // Eliminate phi nodes
    let cfg'' = eliminatePhiNodes cfg'

    ({ func with CFG = cfg'' }, labelGen')

/// Convert a program out of SSA form
let destructSSA (program: Program) : Program =
    let (Program (functions, strings, floats)) = program

    let (functions', _) =
        functions
        |> List.fold (fun (acc, lg) func ->
            let (func', lg') = destructSSAInFunction func lg
            (acc @ [func'], lg')
        ) ([], LabelGen 10000)  // Start high to avoid conflicts

    Program (functions', strings, floats)
