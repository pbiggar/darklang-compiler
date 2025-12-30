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
open Output

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
        | Ret _ -> crash "Cannot split edge from Ret"

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

/// Build a map from VReg to Type by scanning all Mov instructions with type info
let buildTypeMap (cfg: CFG) : Map<VReg, AST.Type> =
    cfg.Blocks
    |> Map.fold (fun typeMap _ block ->
        block.Instrs
        |> List.fold (fun tm instr ->
            match instr with
            | Mov (dest, _, Some t) -> Map.add dest t tm
            | BinOp (dest, _, _, _, operandType) when operandType = AST.TFloat64 ->
                Map.add dest AST.TFloat64 tm
            | Call (dest, _, _, _, returnType) when returnType = AST.TFloat64 ->
                Map.add dest AST.TFloat64 tm
            | _ -> tm
        ) typeMap
    ) Map.empty

/// Infer the type of an operand using the type map
let inferOperandType (typeMap: Map<VReg, AST.Type>) (src: Operand) : AST.Type option =
    match src with
    | FloatRef _ -> Some AST.TFloat64
    | Register vreg -> Map.tryFind vreg typeMap
    | _ -> None

/// Insert a copy instruction at the end of a block (before terminator)
/// For phi node: dest = phi[(v1, L1), (v2, L2), ...]
/// In predecessor L1: insert "dest = v1" at end
let insertCopyInPredecessor (cfg: CFG) (predLabel: Label) (dest: VReg) (src: Operand) (typeMap: Map<VReg, AST.Type>) (_funcName: string) (funcParams: Set<VReg>) : CFG =
    // Skip if source equals dest (self-referential phi)
    let skip =
        match src with
        | Register srcReg -> srcReg = dest
        | _ -> false

    // Skip if source is an unrenamed VReg (< 10000) that is NOT a function parameter.
    // Unrenamed VRegs that aren't params mean the variable was never defined on this path.
    // Function parameters ARE valid unrenamed sources - they're defined by the function call.
    let isUnrenamed =
        match src with
        | Register (VReg id as vreg) -> id < 10000 && not (Set.contains vreg funcParams)
        | _ -> false

    if skip || isUnrenamed then
        cfg
    else
        let predBlock = Map.find predLabel cfg.Blocks

        // Infer the type from the source operand
        let valueType = inferOperandType typeMap src

        // Create copy instruction with inferred type
        let copyInstr = Mov (dest, src, valueType)

        // Add copy at end of predecessor's instructions
        let predBlock' = { predBlock with Instrs = predBlock.Instrs @ [copyInstr] }

        { cfg with Blocks = Map.add predLabel predBlock' cfg.Blocks }

/// Collect all VRegs that are properly defined (not from unrenamed phi sources)
/// A VReg is properly defined if:
/// 1. It's defined by a non-phi instruction (Mov, BinOp, Call, etc.)
/// 2. It's defined by a phi where ALL sources are properly defined
/// 3. It's a function parameter (funcParams)
let collectProperlyDefinedVRegs (cfg: CFG) (funcParams: Set<VReg>) : Set<VReg> =
    // First, collect all VRegs defined by non-phi instructions
    let nonPhiDefs =
        cfg.Blocks
        |> Map.fold (fun defs _ block ->
            block.Instrs
            |> List.fold (fun d instr ->
                match instr with
                | Mov (dest, _, _) -> Set.add dest d
                | BinOp (dest, _, _, _, _) -> Set.add dest d
                | UnaryOp (dest, _, _) -> Set.add dest d
                | Call (dest, _, _, _, _) -> Set.add dest d
                | IndirectCall (dest, _, _, _, _) -> Set.add dest d
                | ClosureAlloc (dest, _, _) -> Set.add dest d
                | ClosureCall (dest, _, _) -> Set.add dest d
                | HeapAlloc (dest, _) -> Set.add dest d
                | HeapLoad (dest, _, _) -> Set.add dest d
                | StringConcat (dest, _, _) -> Set.add dest d
                | FileReadText (dest, _) -> Set.add dest d
                | FileExists (dest, _) -> Set.add dest d
                | FileWriteText (dest, _, _) -> Set.add dest d
                | FileAppendText (dest, _, _) -> Set.add dest d
                | RawAlloc (dest, _) -> Set.add dest d
                | RawGet (dest, _, _) -> Set.add dest d
                | RawGetByte (dest, _, _) -> Set.add dest d
                | Phi _ -> d  // Skip phi nodes
                | _ -> d
            ) defs
        ) Set.empty

    // Now iteratively add phi destinations that have all sources defined
    let allPhis =
        cfg.Blocks
        |> Map.fold (fun phis _ block ->
            block.Instrs
            |> List.fold (fun ps instr ->
                match instr with
                | Phi (dest, sources) -> (dest, sources) :: ps
                | _ -> ps
            ) phis
        ) []

    let rec fixpoint (defined: Set<VReg>) =
        let newDefined =
            allPhis
            |> List.fold (fun d (dest, sources) ->
                if Set.contains dest d then
                    d  // Already defined
                else
                    // Check if all sources are either:
                    // 1. Already defined
                    // 2. Not a register (constant)
                    // 3. A renamed register (>= 10000) that is in defined set
                    // 4. A function parameter (always valid)
                    let allSourcesValid =
                        sources |> List.forall (fun (src, _) ->
                            match src with
                            | Register vreg ->
                                let (VReg id) = vreg
                                // Function parameters are valid even if unrenamed
                                // Unrenamed VRegs that aren't params are invalid
                                // Renamed VRegs must be in the defined set
                                Set.contains vreg funcParams ||
                                (id >= 10000 && Set.contains vreg d)
                            | _ -> true  // Constants are always valid
                        )
                    if allSourcesValid then
                        Set.add dest d
                    else
                        d
            ) defined
        if newDefined = defined then defined else fixpoint newDefined

    fixpoint nonPhiDefs

/// Replace phi nodes with copies in predecessors
let eliminatePhiNodes (cfg: CFG) (funcName: string) (funcParams: Set<VReg>) : CFG =
    // Get set of properly defined VRegs (pass funcParams so they're treated as valid sources)
    let definedVRegs = collectProperlyDefinedVRegs cfg funcParams
    let _ = funcName  // suppress unused warning

    // Build type map to infer types for phi resolution copies
    let typeMap = buildTypeMap cfg

    cfg.Blocks
    |> Map.fold (fun cfg' label _ ->
        // IMPORTANT: Get the current version of the block from cfg', not the original
        let block = Map.find label cfg'.Blocks
        let phis = getPhiNodes block

        // Filter to only phi nodes whose destinations are properly defined
        let validPhis = phis |> List.filter (fun phi ->
            match phi with
            | Phi (dest, _) -> Set.contains dest definedVRegs
            | _ -> false
        )

        // For each valid phi node, insert copies in predecessors
        let cfg'' =
            validPhis
            |> List.fold (fun c phi ->
                match phi with
                | Phi (dest, sources) ->
                    sources
                    |> List.fold (fun c' (src, predLabel) ->
                        insertCopyInPredecessor c' predLabel dest src typeMap funcName funcParams
                    ) c
                | _ -> c
            ) cfg'

        // Remove ALL phi nodes from this block (including invalid ones)
        // IMPORTANT: Get the current version of the block again (may have changed)
        let updatedBlock = Map.find label cfg''.Blocks
        let nonPhis = getNonPhiInstrs updatedBlock
        let block' = { updatedBlock with Instrs = nonPhis }
        { cfg'' with Blocks = Map.add label block' cfg''.Blocks }
    ) cfg

/// Convert a function out of SSA form
let destructSSAInFunction (func: Function) (labelGen: LabelGen) : Function * LabelGen =
    // Split critical edges
    let (cfg', labelGen') = splitCriticalEdges func.CFG labelGen

    // Convert function params to a Set for O(log n) lookup
    let funcParams = func.Params |> Set.ofList

    // Eliminate phi nodes
    let cfg'' = eliminatePhiNodes cfg' func.Name funcParams

    ({ func with CFG = cfg'' }, labelGen')

/// Convert a program out of SSA form
let destructSSA (program: Program) : Program =
    let (Program (functions, strings, floats, variants, records)) = program

    let (functions', _) =
        functions
        |> List.fold (fun (acc, lg) func ->
            let (func', lg') = destructSSAInFunction func lg
            (acc @ [func'], lg')
        ) ([], LabelGen 10000)  // Start high to avoid conflicts

    Program (functions', strings, floats, variants, records)
