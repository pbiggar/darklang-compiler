// 3.1_SSA_Construction.fs - SSA Construction Pass
//
// Converts MIR to SSA (Static Single Assignment) form by:
// 1. Computing dominators and dominance frontiers
// 2. Inserting phi nodes at join points
// 3. Renaming variables so each definition has a unique name
//
// After SSA construction, every virtual register is defined exactly once.
// This enables powerful optimizations like GVN, SCCP, and easy DCE.

module SSA_Construction

open MIR

/// Predecessors map: for each label, which labels can jump to it
type Predecessors = Map<Label, Label list>

/// Build predecessors map from CFG
let buildPredecessors (cfg: CFG) : Predecessors =
    let addEdge (from: Label) (toLabel: Label) (preds: Predecessors) : Predecessors =
        let existing = Map.tryFind toLabel preds |> Option.defaultValue []
        Map.add toLabel (from :: existing) preds

    cfg.Blocks
    |> Map.fold (fun preds label block ->
        match block.Terminator with
        | Ret _ -> preds
        | Jump target -> addEdge label target preds
        | Branch (_, trueLabel, falseLabel) ->
            preds |> addEdge label trueLabel |> addEdge label falseLabel
    ) Map.empty

/// Compute immediate dominators using iterative dataflow
/// Returns map from label to its immediate dominator
type Dominators = Map<Label, Label>

let computeDominators (cfg: CFG) (preds: Predecessors) : Dominators =
    let labels = cfg.Blocks |> Map.keys |> List.ofSeq
    let entry = cfg.Entry

    // Initialize: entry dominates itself, others are undefined
    // Use a set to represent all dominators for each node initially
    let initialDoms =
        labels
        |> List.fold (fun m label ->
            if label = entry then
                Map.add label (Set.singleton label) m
            else
                Map.add label (Set.ofList labels) m  // Initially dominated by all
        ) Map.empty

    // Iterate until fixed point
    let rec iterate (doms: Map<Label, Set<Label>>) =
        let (changed, doms') =
            labels
            |> List.fold (fun (changed, m) label ->
                if label = entry then
                    (changed, m)
                else
                    let predLabels = Map.tryFind label preds |> Option.defaultValue []
                    if List.isEmpty predLabels then
                        (changed, m)
                    else
                        // Dom(n) = {n} union (intersection of Dom(p) for all predecessors p)
                        let predDoms =
                            predLabels
                            |> List.map (fun p -> Map.tryFind p m |> Option.defaultValue Set.empty)
                        let intersection =
                            match predDoms with
                            | [] -> Set.empty
                            | first :: rest -> List.fold Set.intersect first rest
                        let newDom = Set.add label intersection
                        let oldDom = Map.tryFind label m |> Option.defaultValue Set.empty
                        if newDom = oldDom then
                            (changed, m)
                        else
                            (true, Map.add label newDom m)
            ) (false, doms)
        if changed then iterate doms' else doms'

    let allDoms = iterate initialDoms

    // Extract immediate dominator from dominator sets
    // idom(n) is the dominator of n that is dominated by all other dominators of n (closest one)
    labels
    |> List.fold (fun idoms label ->
        if label = entry then
            idoms  // Entry has no immediate dominator
        else
            let doms = Map.tryFind label allDoms |> Option.defaultValue Set.empty
            // Remove self from dominators
            let strictDoms = Set.remove label doms
            if Set.isEmpty strictDoms then
                idoms
            else
                // idom is the unique strict dominator that is dominated by all other strict dominators
                // In other words, it's the "closest" dominator to the node
                let idom =
                    strictDoms
                    |> Set.toList
                    |> List.tryFind (fun d ->
                        let dDoms = Map.tryFind d allDoms |> Option.defaultValue Set.empty
                        // d is idom if all other strict dominators dominate d
                        // i.e., all other strict dominators are in Dom(d)
                        Set.forall (fun other -> other = d || Set.contains other dDoms) strictDoms
                    )
                match idom with
                | Some d -> Map.add label d idoms
                | None ->
                    // Fallback: pick any dominator (shouldn't happen in well-formed CFG)
                    match Set.toList strictDoms with
                    | d :: _ -> Map.add label d idoms
                    | [] -> idoms
    ) Map.empty

/// Dominance frontier: blocks where dominance ends
/// DF(n) = blocks that n dominates a predecessor of, but not the block itself
type DominanceFrontier = Map<Label, Set<Label>>

let computeDominanceFrontier (cfg: CFG) (preds: Predecessors) (idoms: Dominators) : DominanceFrontier =
    let labels = cfg.Blocks |> Map.keys |> List.ofSeq

    // For each block b, for each predecessor p of b:
    // Walk up the dominator tree from p until we reach idom(b)
    // All blocks on this path have b in their dominance frontier
    labels
    |> List.fold (fun df b ->
        let bPreds = Map.tryFind b preds |> Option.defaultValue []
        let bIdom = Map.tryFind b idoms

        bPreds
        |> List.fold (fun df' p ->
            // Walk from p up to idom(b) (exclusive)
            let rec walk current df'' =
                match bIdom with
                | Some idom when current = idom -> df''
                | _ ->
                    // Add b to DF(current)
                    let currentDF = Map.tryFind current df'' |> Option.defaultValue Set.empty
                    let df''' = Map.add current (Set.add b currentDF) df''
                    // Move up to idom(current)
                    match Map.tryFind current idoms with
                    | Some parent when parent <> current -> walk parent df'''
                    | _ -> df'''
            walk p df'
        ) df
    ) Map.empty

/// Get all variable definitions in a basic block
/// Returns set of VRegs that are defined (written to) in the block
let getBlockDefs (block: BasicBlock) : Set<VReg> =
    block.Instrs
    |> List.fold (fun defs instr ->
        match instr with
        | Mov (dest, _, _) -> Set.add dest defs
        | BinOp (dest, _, _, _, _) -> Set.add dest defs
        | UnaryOp (dest, _, _) -> Set.add dest defs
        | Call (dest, _, _, _, _) -> Set.add dest defs
        | IndirectCall (dest, _, _, _, _) -> Set.add dest defs
        | ClosureAlloc (dest, _, _) -> Set.add dest defs
        | ClosureCall (dest, _, _) -> Set.add dest defs
        | HeapAlloc (dest, _) -> Set.add dest defs
        | HeapStore _ -> defs  // No destination register
        | HeapLoad (dest, _, _) -> Set.add dest defs
        | StringConcat (dest, _, _) -> Set.add dest defs
        | RefCountInc _ -> defs
        | RefCountDec _ -> defs
        | Print _ -> defs
        | FileReadText (dest, _) -> Set.add dest defs
        | FileExists (dest, _) -> Set.add dest defs
        | FileWriteText (dest, _, _) -> Set.add dest defs
        | FileAppendText (dest, _, _) -> Set.add dest defs
        | FileDelete (dest, _) -> Set.add dest defs
        | FileSetExecutable (dest, _) -> Set.add dest defs
        | Phi (dest, _) -> Set.add dest defs
        | RawAlloc (dest, _) -> Set.add dest defs
        | RawFree _ -> defs
        | RawGet (dest, _, _) -> Set.add dest defs
        | RawGetByte (dest, _, _) -> Set.add dest defs
        | RawSet _ -> defs
        | FloatSqrt (dest, _) -> Set.add dest defs
        | FloatAbs (dest, _) -> Set.add dest defs
        | FloatNeg (dest, _) -> Set.add dest defs
        | IntToFloat (dest, _) -> Set.add dest defs
        | FloatToInt (dest, _) -> Set.add dest defs
        | StringHash (dest, _) -> Set.add dest defs
        | StringEq (dest, _, _) -> Set.add dest defs
        | RefCountIncString _ -> defs
        | RefCountDecString _ -> defs
        | RandomInt64 dest -> Set.add dest defs
    ) Set.empty

/// Get all variables defined anywhere in the CFG
let getAllDefs (cfg: CFG) : Map<VReg, Set<Label>> =
    cfg.Blocks
    |> Map.fold (fun defSites label block ->
        let blockDefs = getBlockDefs block
        blockDefs
        |> Set.fold (fun sites vreg ->
            let existing = Map.tryFind vreg sites |> Option.defaultValue Set.empty
            Map.add vreg (Set.add label existing) sites
        ) defSites
    ) Map.empty

/// Extract VRegs used in an operand
let getOperandUses (op: Operand) : Set<VReg> =
    match op with
    | Register vreg -> Set.singleton vreg
    | _ -> Set.empty

/// Get all variables used (read) in a basic block
/// Returns set of VRegs that are read in the block
let getBlockUses (block: BasicBlock) : Set<VReg> =
    let instrUses =
        block.Instrs
        |> List.fold (fun uses instr ->
            match instr with
            | Mov (_, src, _) -> Set.union uses (getOperandUses src)
            | BinOp (_, _, left, right, _) ->
                uses |> Set.union (getOperandUses left) |> Set.union (getOperandUses right)
            | UnaryOp (_, _, src) -> Set.union uses (getOperandUses src)
            | Call (_, _, args, _, _) ->
                args |> List.fold (fun u a -> Set.union u (getOperandUses a)) uses
            | IndirectCall (_, func, args, _, _) ->
                let funcUses = getOperandUses func
                let argUses = args |> List.fold (fun u a -> Set.union u (getOperandUses a)) Set.empty
                uses |> Set.union funcUses |> Set.union argUses
            | ClosureAlloc (_, _, captures) ->
                captures |> List.fold (fun u c -> Set.union u (getOperandUses c)) uses
            | ClosureCall (_, closure, args) ->
                let closureUses = getOperandUses closure
                let argUses = args |> List.fold (fun u a -> Set.union u (getOperandUses a)) Set.empty
                uses |> Set.union closureUses |> Set.union argUses
            | HeapAlloc _ -> uses
            | HeapStore (addr, _, src) ->
                uses |> Set.add addr |> Set.union (getOperandUses src)
            | HeapLoad (_, addr, _) -> Set.add addr uses
            | StringConcat (_, left, right) ->
                uses |> Set.union (getOperandUses left) |> Set.union (getOperandUses right)
            | RefCountInc (addr, _) -> Set.add addr uses
            | RefCountDec (addr, _) -> Set.add addr uses
            | Print (src, _) -> Set.union uses (getOperandUses src)
            | FileReadText (_, path) -> Set.union uses (getOperandUses path)
            | FileExists (_, path) -> Set.union uses (getOperandUses path)
            | FileWriteText (_, path, content) ->
                uses |> Set.union (getOperandUses path) |> Set.union (getOperandUses content)
            | FileAppendText (_, path, content) ->
                uses |> Set.union (getOperandUses path) |> Set.union (getOperandUses content)
            | FileDelete (_, path) -> Set.union uses (getOperandUses path)
            | FileSetExecutable (_, path) -> Set.union uses (getOperandUses path)
            | Phi (_, sources) ->
                sources |> List.fold (fun u (src, _) -> Set.union u (getOperandUses src)) uses
            | RawAlloc (_, numBytes) -> Set.union uses (getOperandUses numBytes)
            | RawFree ptr -> Set.union uses (getOperandUses ptr)
            | RawGet (_, ptr, offset) ->
                uses |> Set.union (getOperandUses ptr) |> Set.union (getOperandUses offset)
            | RawGetByte (_, ptr, offset) ->
                uses |> Set.union (getOperandUses ptr) |> Set.union (getOperandUses offset)
            | RawSet (ptr, offset, value) ->
                uses |> Set.union (getOperandUses ptr) |> Set.union (getOperandUses offset) |> Set.union (getOperandUses value)
            | FloatSqrt (_, src) -> Set.union uses (getOperandUses src)
            | FloatAbs (_, src) -> Set.union uses (getOperandUses src)
            | FloatNeg (_, src) -> Set.union uses (getOperandUses src)
            | IntToFloat (_, src) -> Set.union uses (getOperandUses src)
            | FloatToInt (_, src) -> Set.union uses (getOperandUses src)
            | StringHash (_, str) -> Set.union uses (getOperandUses str)
            | StringEq (_, left, right) ->
                uses |> Set.union (getOperandUses left) |> Set.union (getOperandUses right)
            | RefCountIncString str -> Set.union uses (getOperandUses str)
            | RefCountDecString str -> Set.union uses (getOperandUses str)
            | RandomInt64 _ -> uses  // No operand uses
        ) Set.empty

    // Also include uses in terminator
    let termUses =
        match block.Terminator with
        | Ret op -> getOperandUses op
        | Branch (cond, _, _) -> getOperandUses cond
        | Jump _ -> Set.empty

    Set.union instrUses termUses

/// Get successor labels of a block
let getSuccessors (block: BasicBlock) : Label list =
    match block.Terminator with
    | Ret _ -> []
    | Jump target -> [target]
    | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]

/// Compute liveness information for the CFG
/// Returns (liveIn, liveOut) maps from Label to Set<VReg>
/// A variable is live-in at a block if it may be used before being defined
/// A variable is live-out at a block if it's live-in at any successor
let computeLiveness (cfg: CFG) : Map<Label, Set<VReg>> * Map<Label, Set<VReg>> =
    let labels = cfg.Blocks |> Map.keys |> List.ofSeq

    // Precompute uses and defs for each block
    let blockUses = labels |> List.map (fun l -> (l, getBlockUses (Map.find l cfg.Blocks))) |> Map.ofList
    let blockDefs = labels |> List.map (fun l -> (l, getBlockDefs (Map.find l cfg.Blocks))) |> Map.ofList

    // Initialize live-out to empty
    let initialLiveOut = labels |> List.map (fun l -> (l, Set.empty)) |> Map.ofList

    // Iterative dataflow analysis (backward)
    let rec fixpoint (liveOut: Map<Label, Set<VReg>>) =
        let (changed, liveOut') =
            labels
            |> List.fold (fun (changed, lo) label ->
                let block = Map.find label cfg.Blocks
                let successors = getSuccessors block

                // Live-out = union of live-in of all successors
                let newLiveOut =
                    successors
                    |> List.fold (fun acc succ ->
                        // Live-in of successor = uses + (live-out - defs)
                        let succUses = Map.find succ blockUses
                        let succDefs = Map.find succ blockDefs
                        let succLiveOut = Map.tryFind succ lo |> Option.defaultValue Set.empty
                        let succLiveIn = Set.union succUses (Set.difference succLiveOut succDefs)
                        Set.union acc succLiveIn
                    ) Set.empty

                let oldLiveOut = Map.find label lo
                if newLiveOut = oldLiveOut then
                    (changed, lo)
                else
                    (true, Map.add label newLiveOut lo)
            ) (false, liveOut)

        if changed then fixpoint liveOut' else liveOut'

    let finalLiveOut = fixpoint initialLiveOut

    // Compute live-in from live-out
    let liveIn =
        labels
        |> List.map (fun label ->
            let uses = Map.find label blockUses
            let defs = Map.find label blockDefs
            let lo = Map.find label finalLiveOut
            let li = Set.union uses (Set.difference lo defs)
            (label, li)
        )
        |> Map.ofList

    (liveIn, finalLiveOut)

/// Insert phi nodes at dominance frontiers
/// For each variable v defined in block b:
///   For each block d in DF(b):
///     Insert phi node for v in d (if not already present AND v is live-in at d)
///     This also counts as a definition, so recursively process
let insertPhiNodes (cfg: CFG) (df: DominanceFrontier) (preds: Predecessors) (liveIn: Map<Label, Set<VReg>>) (_funcName: string) : CFG =
    let allDefs = getAllDefs cfg

    // Worklist algorithm: for each variable, propagate phi insertion
    let rec insertForVar (vreg: VReg) (worklist: Set<Label>) (phiBlocks: Set<Label>) (cfg': CFG) : CFG =
        if Set.isEmpty worklist then
            cfg'
        else
            let block = Set.minElement worklist
            let worklist' = Set.remove block worklist

            // Get dominance frontier of this block
            let frontier = Map.tryFind block df |> Option.defaultValue Set.empty

            // For each block in the frontier, insert phi if not already there AND variable is live
            let (worklist'', phiBlocks', cfg'') =
                frontier
                |> Set.fold (fun (wl, pb, c) dfBlock ->
                    if Set.contains dfBlock pb then
                        (wl, pb, c)  // Already has phi for this var
                    else
                        // Only insert phi if variable is live-in at this block
                        let blockLiveIn = Map.tryFind dfBlock liveIn |> Option.defaultValue Set.empty
                        if not (Set.contains vreg blockLiveIn) then
                            (wl, pb, c)  // Variable not live here, skip phi
                        else
                            // Insert phi node
                            let blockPreds = Map.tryFind dfBlock preds |> Option.defaultValue []
                            // Create phi with placeholder sources (will be renamed later)
                            let phiSources = blockPreds |> List.map (fun p -> (Register vreg, p))
                            let phiInstr = Phi (vreg, phiSources)

                            // Add to block (at the beginning)
                            let existingBlock = Map.find dfBlock c.Blocks
                            let newBlock = { existingBlock with Instrs = phiInstr :: existingBlock.Instrs }
                            let c' = { c with Blocks = Map.add dfBlock newBlock c.Blocks }

                            // Add to worklist (phi is a definition, may need more phis)
                            let wl' = Set.add dfBlock wl
                            let pb' = Set.add dfBlock pb

                            (wl', pb', c')
                ) (worklist', phiBlocks, cfg')

            insertForVar vreg worklist'' phiBlocks' cfg''

    // Process all variables
    allDefs
    |> Map.fold (fun cfg' vreg defSites ->
        insertForVar vreg defSites Set.empty cfg'
    ) cfg

/// Rename variables to SSA form
/// Each definition gets a fresh version number
/// Uses dominator tree traversal to maintain scoping
type RenamingState = {
    /// Current version for each original VReg
    CurrentVersion: Map<VReg, int>
    /// Stack of versions for each VReg (for backtracking)
    VersionStack: Map<VReg, int list>
    /// Next available version number
    NextVersion: int
    /// Map from (original VReg, version) to new VReg
    VersionToReg: Map<VReg * int, VReg>
}

/// Create initial renaming state, starting VReg numbers above any existing VRegs
let createInitialRenamingState (cfg: CFG) : RenamingState =
    // Find the highest VReg number used in the CFG
    let maxVReg =
        cfg.Blocks
        |> Map.fold (fun maxSoFar _ block ->
            let blockMax =
                block.Instrs
                |> List.fold (fun m instr ->
                    match instr with
                    | Mov (VReg n, _, _) -> max m n
                    | BinOp (VReg n, _, _, _, _) -> max m n
                    | UnaryOp (VReg n, _, _) -> max m n
                    | Call (VReg n, _, _, _, _) -> max m n
                    | IndirectCall (VReg n, _, _, _, _) -> max m n
                    | ClosureAlloc (VReg n, _, _) -> max m n
                    | ClosureCall (VReg n, _, _) -> max m n
                    | HeapAlloc (VReg n, _) -> max m n
                    | HeapLoad (VReg n, _, _) -> max m n
                    | StringConcat (VReg n, _, _) -> max m n
                    | FileReadText (VReg n, _) -> max m n
                    | FileExists (VReg n, _) -> max m n
                    | FileWriteText (VReg n, _, _) -> max m n
                    | FileAppendText (VReg n, _, _) -> max m n
                    | FileDelete (VReg n, _) -> max m n
                    | FileSetExecutable (VReg n, _) -> max m n
                    | Phi (VReg n, _) -> max m n
                    | _ -> m
                ) 0
            max maxSoFar blockMax
        ) 0

    {
        CurrentVersion = Map.empty
        VersionStack = Map.empty
        NextVersion = maxVReg + 10000  // Start SSA VRegs well above original VRegs
        VersionToReg = Map.empty
    }

/// Get current version of a VReg, creating version 0 if not seen
let getCurrentVersion (state: RenamingState) (vreg: VReg) : int * RenamingState =
    match Map.tryFind vreg state.CurrentVersion with
    | Some v -> (v, state)
    | None ->
        // First use - create version 0
        let state' = {
            state with
                CurrentVersion = Map.add vreg 0 state.CurrentVersion
                VersionStack = Map.add vreg [0] state.VersionStack
        }
        (0, state')

/// Create new version for a definition
let newVersion (state: RenamingState) (vreg: VReg) : int * VReg * RenamingState =
    let version = state.NextVersion
    let newReg = VReg (state.NextVersion)

    // Push onto stack
    let stack = Map.tryFind vreg state.VersionStack |> Option.defaultValue []
    let state' = {
        CurrentVersion = Map.add vreg version state.CurrentVersion
        VersionStack = Map.add vreg (version :: stack) state.VersionStack
        NextVersion = state.NextVersion + 1
        VersionToReg = Map.add (vreg, version) newReg state.VersionToReg
    }
    (version, newReg, state')

/// Get the renamed VReg for a use
let getRenamedReg (state: RenamingState) (vreg: VReg) : VReg =
    let version = Map.tryFind vreg state.CurrentVersion |> Option.defaultValue 0
    Map.tryFind (vreg, version) state.VersionToReg
    |> Option.defaultValue vreg

/// Rename operand
let renameOperand (state: RenamingState) (op: Operand) : Operand =
    match op with
    | Register vreg -> Register (getRenamedReg state vreg)
    | other -> other

/// Rename instruction (uses and defs)
let renameInstr (state: RenamingState) (instr: Instr) : Instr * RenamingState =
    match instr with
    | Mov (dest, src, vt) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (Mov (newDest, src', vt), state')

    | BinOp (dest, op, left, right, opType) ->
        let left' = renameOperand state left
        let right' = renameOperand state right
        let (_, newDest, state') = newVersion state dest
        (BinOp (newDest, op, left', right', opType), state')

    | UnaryOp (dest, op, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (UnaryOp (newDest, op, src'), state')

    | Call (dest, funcName, args, argTypes, returnType) ->
        let args' = args |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (Call (newDest, funcName, args', argTypes, returnType), state')

    | IndirectCall (dest, func, args, argTypes, returnType) ->
        let func' = renameOperand state func
        let args' = args |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (IndirectCall (newDest, func', args', argTypes, returnType), state')

    | ClosureAlloc (dest, funcName, captures) ->
        let captures' = captures |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (ClosureAlloc (newDest, funcName, captures'), state')

    | ClosureCall (dest, closure, args) ->
        let closure' = renameOperand state closure
        let args' = args |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (ClosureCall (newDest, closure', args'), state')

    | HeapAlloc (dest, size) ->
        let (_, newDest, state') = newVersion state dest
        (HeapAlloc (newDest, size), state')

    | HeapStore (addr, offset, src) ->
        let src' = renameOperand state src
        // addr is used, not defined
        let addr' = getRenamedReg state addr
        (HeapStore (addr', offset, src'), state)

    | HeapLoad (dest, addr, offset) ->
        let addr' = getRenamedReg state addr
        let (_, newDest, state') = newVersion state dest
        (HeapLoad (newDest, addr', offset), state')

    | StringConcat (dest, left, right) ->
        let left' = renameOperand state left
        let right' = renameOperand state right
        let (_, newDest, state') = newVersion state dest
        (StringConcat (newDest, left', right'), state')

    | RefCountInc (addr, size) ->
        let addr' = getRenamedReg state addr
        (RefCountInc (addr', size), state)

    | RefCountDec (addr, size) ->
        let addr' = getRenamedReg state addr
        (RefCountDec (addr', size), state)

    | Print (src, vt) ->
        let src' = renameOperand state src
        (Print (src', vt), state)

    | FileReadText (dest, path) ->
        let path' = renameOperand state path
        let (_, newDest, state') = newVersion state dest
        (FileReadText (newDest, path'), state')

    | FileExists (dest, path) ->
        let path' = renameOperand state path
        let (_, newDest, state') = newVersion state dest
        (FileExists (newDest, path'), state')

    | FileWriteText (dest, path, content) ->
        let path' = renameOperand state path
        let content' = renameOperand state content
        let (_, newDest, state') = newVersion state dest
        (FileWriteText (newDest, path', content'), state')

    | FileAppendText (dest, path, content) ->
        let path' = renameOperand state path
        let content' = renameOperand state content
        let (_, newDest, state') = newVersion state dest
        (FileAppendText (newDest, path', content'), state')

    | FileDelete (dest, path) ->
        let path' = renameOperand state path
        let (_, newDest, state') = newVersion state dest
        (FileDelete (newDest, path'), state')

    | FileSetExecutable (dest, path) ->
        let path' = renameOperand state path
        let (_, newDest, state') = newVersion state dest
        (FileSetExecutable (newDest, path'), state')

    | Phi (dest, sources) ->
        // Phi sources are renamed when processing predecessors
        // Here we just rename the destination
        let (_, newDest, state') = newVersion state dest
        (Phi (newDest, sources), state')

    | RawAlloc (dest, numBytes) ->
        let numBytes' = renameOperand state numBytes
        let (_, newDest, state') = newVersion state dest
        (RawAlloc (newDest, numBytes'), state')

    | RawFree ptr ->
        let ptr' = renameOperand state ptr
        (RawFree ptr', state)

    | RawGet (dest, ptr, byteOffset) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let (_, newDest, state') = newVersion state dest
        (RawGet (newDest, ptr', byteOffset'), state')

    | RawGetByte (dest, ptr, byteOffset) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let (_, newDest, state') = newVersion state dest
        (RawGetByte (newDest, ptr', byteOffset'), state')

    | RawSet (ptr, byteOffset, value) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let value' = renameOperand state value
        (RawSet (ptr', byteOffset', value'), state)

    | FloatSqrt (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatSqrt (newDest, src'), state')

    | FloatAbs (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatAbs (newDest, src'), state')

    | FloatNeg (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatNeg (newDest, src'), state')

    | IntToFloat (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (IntToFloat (newDest, src'), state')

    | FloatToInt (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatToInt (newDest, src'), state')

    | StringHash (dest, str) ->
        let str' = renameOperand state str
        let (_, newDest, state') = newVersion state dest
        (StringHash (newDest, str'), state')

    | StringEq (dest, left, right) ->
        let left' = renameOperand state left
        let right' = renameOperand state right
        let (_, newDest, state') = newVersion state dest
        (StringEq (newDest, left', right'), state')

    | RefCountIncString str ->
        let str' = renameOperand state str
        (RefCountIncString str', state)

    | RefCountDecString str ->
        let str' = renameOperand state str
        (RefCountDecString str', state)

    | RandomInt64 dest ->
        let (_, newDest, state') = newVersion state dest
        (RandomInt64 newDest, state')

/// Rename terminator
let renameTerminator (state: RenamingState) (term: Terminator) : Terminator =
    match term with
    | Ret op -> Ret (renameOperand state op)
    | Branch (cond, trueLabel, falseLabel) -> Branch (renameOperand state cond, trueLabel, falseLabel)
    | Jump label -> Jump label

/// Rename a basic block
let renameBlock (state: RenamingState) (block: BasicBlock) : BasicBlock * RenamingState =
    // Rename all instructions
    let (instrs', state') =
        block.Instrs
        |> List.fold (fun (acc, s) instr ->
            let (instr', s') = renameInstr s instr
            (acc @ [instr'], s')
        ) ([], state)

    // Rename terminator
    let term' = renameTerminator state' block.Terminator

    ({ block with Instrs = instrs'; Terminator = term' }, state')

/// Update phi sources for successors
let updatePhiSourcesForSuccessors (cfg: CFG) (currentLabel: Label) (state: RenamingState) : CFG =
    let block = Map.find currentLabel cfg.Blocks

    // Get successor labels
    let successors =
        match block.Terminator with
        | Ret _ -> []
        | Jump target -> [target]
        | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]

    // For each successor, update phi nodes that come from currentLabel
    successors
    |> List.fold (fun cfg' succLabel ->
        let succBlock = Map.find succLabel cfg'.Blocks
        let instrs' =
            succBlock.Instrs
            |> List.map (fun instr ->
                match instr with
                | Phi (dest, sources) ->
                    // Update the source that comes from currentLabel
                    let sources' =
                        sources
                        |> List.map (fun (op, fromLabel) ->
                            if fromLabel = currentLabel then
                                (renameOperand state op, fromLabel)
                            else
                                (op, fromLabel)
                        )
                    Phi (dest, sources')
                | other -> other
            )
        let succBlock' = { succBlock with Instrs = instrs' }
        { cfg' with Blocks = Map.add succLabel succBlock' cfg'.Blocks }
    ) cfg

/// Build dominator tree children
let buildDomTree (idoms: Dominators) : Map<Label, Label list> =
    idoms
    |> Map.fold (fun tree label idom ->
        let children = Map.tryFind idom tree |> Option.defaultValue []
        Map.add idom (label :: children) tree
    ) Map.empty

/// Pop versions for definitions in a block (for backtracking)
let popVersions (state: RenamingState) (block: BasicBlock) : RenamingState =
    let defs = getBlockDefs block
    defs
    |> Set.fold (fun s vreg ->
        match Map.tryFind vreg s.VersionStack with
        | Some (_ :: rest) ->
            let currentVersion = List.tryHead rest |> Option.defaultValue 0
            { s with
                VersionStack = Map.add vreg rest s.VersionStack
                CurrentVersion = Map.add vreg currentVersion s.CurrentVersion }
        | _ -> s
    ) state

/// Rename CFG using dominator tree traversal
let renameCFG (cfg: CFG) (idoms: Dominators) : CFG =
    let domTree = buildDomTree idoms

    // DFS traversal of dominator tree
    // Returns (cfg, state) where state has updated NextVersion for siblings
    let rec visit (label: Label) (state: RenamingState) (cfg': CFG) : CFG * RenamingState =
        let block = Map.find label cfg'.Blocks

        // Rename this block
        let (block', state') = renameBlock state block
        let cfg'' = { cfg' with Blocks = Map.add label block' cfg'.Blocks }

        // Update phi sources in successors
        let cfg''' = updatePhiSourcesForSuccessors cfg'' label state'

        // Visit children in dominator tree
        // Thread the state through to preserve NextVersion across siblings
        let children = Map.tryFind label domTree |> Option.defaultValue []
        let (cfg'''', finalState) =
            children
            |> List.fold (fun (c, s) child ->
                // Each child inherits current versions from state', but uses s.NextVersion
                let childState = { state' with NextVersion = s.NextVersion }
                let (c', s') = visit child childState c
                (c', s')
            ) (cfg''', state')

        // Pop versions for backtracking (restore CurrentVersion/VersionStack from before this block)
        // but keep the NextVersion from children
        let stateAfterPop = { popVersions finalState block' with NextVersion = finalState.NextVersion }

        (cfg'''', stateAfterPop)

    // Start from entry with initial state based on CFG's existing VRegs
    let initialState = createInitialRenamingState cfg
    let (resultCfg, _) = visit cfg.Entry initialState cfg
    resultCfg

/// Convert a function to SSA form
let convertFunctionToSSA (func: Function) : Function =
    let cfg = func.CFG
    let preds = buildPredecessors cfg
    let idoms = computeDominators cfg preds
    let df = computeDominanceFrontier cfg preds idoms

    // Compute liveness to only insert phi nodes for live variables
    let (liveIn, _) = computeLiveness cfg


    // Insert phi nodes (only for live variables)
    let cfgWithPhis = insertPhiNodes cfg df preds liveIn func.Name

    // Rename variables
    let ssaCFG = renameCFG cfgWithPhis idoms

    { func with CFG = ssaCFG }

/// Convert a program to SSA form
let convertToSSA (program: Program) : Program =
    let (Program (functions, strings, floats, variants, records)) = program
    let functions' = functions |> List.map convertFunctionToSSA
    Program (functions', strings, floats, variants, records)
