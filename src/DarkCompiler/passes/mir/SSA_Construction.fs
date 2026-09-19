// SSA_Construction.fs - SSA Construction Pass
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

type private LabelIndex = {
    Labels: Label array
    IndexOf: Map<Label, int>
}

type private VRegIndex = {
    VRegs: VReg array
    IndexOf: System.Collections.Generic.Dictionary<VReg, int>
    WordCount: int
}

type private PhiTypeEvidence =
    | KnownPhiType of AST.Type
    | ConflictingPhiTypes

type SSAConstructionTiming = {
    Phase: string
    ElapsedMs: float
}

let private timePhase
    (swOpt: System.Diagnostics.Stopwatch option)
    (phase: string)
    (timingsRev: SSAConstructionTiming list)
    (operation: unit -> 'a)
    : 'a * SSAConstructionTiming list =
    match swOpt with
    | None -> (operation (), timingsRev)
    | Some sw ->
        let start = sw.Elapsed.TotalMilliseconds
        let result = operation ()
        let elapsedMs = sw.Elapsed.TotalMilliseconds - start
        (result, { Phase = phase; ElapsedMs = elapsedMs } :: timingsRev)

let private buildLabelIndex (cfg: CFG) : LabelIndex =
    let labels = cfg.Blocks |> Map.keys |> Seq.toArray
    let indexOf =
        labels
        |> Array.mapi (fun idx label -> (label, idx))
        |> Array.toList
        |> Map.ofList
    { Labels = labels; IndexOf = indexOf }

let private buildVRegIndex (vregs: Set<VReg>) : VRegIndex =
    let values = vregs |> Set.toArray
    let indexOf = System.Collections.Generic.Dictionary<VReg, int>()
    values |> Array.iteri (fun idx vreg -> indexOf.[vreg] <- idx)
    { VRegs = values
      IndexOf = indexOf
      WordCount = Bitset.wordCount values.Length }

/// Build predecessors map from CFG
let buildPredecessors (cfg: CFG) : Predecessors =
    let addEdge (from: Label) (toLabel: Label) (preds: Predecessors) : Predecessors =
        let existing = Map.tryFind toLabel preds |> Option.defaultValue []
        Map.add toLabel (from :: existing) preds

    cfg.Blocks
    |> Map.fold (fun preds label block ->
        // Add edges from terminator
        match block.Terminator with
        | Ret _ -> preds
        | Jump target -> addEdge label target preds
        | Branch (_, trueLabel, falseLabel) ->
            preds |> addEdge label trueLabel |> addEdge label falseLabel
    ) Map.empty

/// Compute immediate dominators in reverse postorder.
/// Returns map from label to its immediate dominator
type Dominators = Map<Label, Label>

let computeDominators (cfg: CFG) (preds: Predecessors) : Dominators =
    let entry = cfg.Entry

    // Cooper-Harvey-Kennedy converges quickly when blocks are visited in
    // reverse postorder. Keep the DFS stack explicit because generated test
    // functions can contain thousands of blocks.
    let rec buildReversePostorder
        (work: (Label * bool) list)
        (visited: Set<Label>)
        (reversePostorder: Label list)
        : Label list =
        match work with
        | [] -> reversePostorder
        | (label, expanded) :: remaining ->
            if expanded then
                buildReversePostorder remaining visited (label :: reversePostorder)
            elif Set.contains label visited then
                buildReversePostorder remaining visited reversePostorder
            else
                let successors =
                    match Map.tryFind label cfg.Blocks with
                    | Some b ->
                        match b.Terminator with
                        | Ret _ -> []
                        | Jump target -> [target]
                        | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]
                    | None -> []
                let successorWork =
                    successors |> List.map (fun successor -> (successor, false))
                buildReversePostorder
                    (successorWork @ ((label, true) :: remaining))
                    (Set.add label visited)
                    reversePostorder

    let reversePostorder =
        buildReversePostorder [(entry, false)] Set.empty []
        |> List.filter (fun label -> Map.containsKey label cfg.Blocks)
    let positions =
        reversePostorder
        |> List.mapi (fun index label -> (label, index))
        |> Map.ofList

    let position label =
        match Map.tryFind label positions with
        | Some index -> index
        | None -> Crash.crash $"SSA: Missing reverse-postorder position for {label}"
    let parent (idoms: Dominators) label =
        match Map.tryFind label idoms with
        | Some immediateDominator -> immediateDominator
        | None -> Crash.crash $"SSA: Missing immediate dominator for {label}"
    let rec intersect (idoms: Dominators) left right =
        if left = right then
            left
        elif position left > position right then
            intersect idoms (parent idoms left) right
        else
            intersect idoms left (parent idoms right)

    // Mapping entry to itself gives intersect a sentinel root. Remove it from
    // the public result after the fixed point settles.
    let rec iterate (nonEntryLabels: Label list) (idoms: Dominators) =
        let (changed, updated) =
            nonEntryLabels
            |> List.fold (fun (changed, current) label ->
                let processedPredecessors =
                    Map.tryFind label preds
                    |> Option.defaultValue []
                    |> List.filter (fun predecessor -> Map.containsKey predecessor current)
                match processedPredecessors with
                | [] -> (changed, current)
                | first :: rest ->
                    let newIdom = rest |> List.fold (intersect current) first
                    match Map.tryFind label current with
                    | Some oldIdom when oldIdom = newIdom -> (changed, current)
                    | _ -> (true, Map.add label newIdom current)
            ) (false, idoms)
        if changed then iterate nonEntryLabels updated else updated

    match reversePostorder with
    | [] -> Map.empty
    | _ :: nonEntryLabels ->
        iterate nonEntryLabels (Map.ofList [(entry, entry)]) |> Map.remove entry

/// Dominance frontier: blocks where dominance ends
/// DF(n) = blocks that n dominates a predecessor of, but not the block itself
type DominanceFrontier = Map<Label, Set<Label>>

let computeDominanceFrontier (cfg: CFG) (preds: Predecessors) (idoms: Dominators) : DominanceFrontier =
    let labelIndex = buildLabelIndex cfg
    let labels = labelIndex.Labels
    let labelCount = labels.Length
    let wordCount = Bitset.wordCount labelCount

    let idomIndex =
        Array.init labelCount (fun idx ->
            let label = labels.[idx]
            Map.tryFind label idoms
            |> Option.bind (fun parent -> Map.tryFind parent labelIndex.IndexOf))

    let dfSets = Array.init labelCount (fun _ -> Bitset.empty wordCount)

    // For each block b, for each predecessor p of b:
    // Walk up the dominator tree from p until we reach idom(b)
    // All blocks on this path have b in their dominance frontier
    labels
    |> Array.iteri (fun bIdx b ->
        let bPreds = Map.tryFind b preds |> Option.defaultValue []
        bPreds
        |> List.iter (fun p ->
            match Map.tryFind p labelIndex.IndexOf with
            | None -> ()
            | Some pIdx ->
                let rec walk currentIdx =
                    match idomIndex.[bIdx] with
                    | Some idomIdx when currentIdx = idomIdx -> ()
                    | _ ->
                        Bitset.addIndexInPlace bIdx dfSets.[currentIdx]
                        match idomIndex.[currentIdx] with
                        | Some parentIdx when parentIdx <> currentIdx -> walk parentIdx
                        | _ -> ()
                walk pIdx))

    labels
    |> Array.mapi (fun idx label ->
        let frontier =
            dfSets.[idx]
            |> Bitset.indicesToList
            |> List.map (fun fIdx -> labels.[fIdx])
            |> Set.ofList
        (label, frontier))
    |> Array.toList
    |> Map.ofList

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
        | TailCall _ -> defs  // Tail calls have no destination
        | IndirectCall (dest, _, _, _, _) -> Set.add dest defs
        | IndirectTailCall _ -> defs  // Indirect tail calls have no destination
        | ClosureAlloc (dest, _, _) -> Set.add dest defs
        | ClosureCall (dest, _, _, _, _) -> Set.add dest defs
        | ClosureTailCall _ -> defs  // Closure tail calls have no destination
        | HeapAlloc (dest, _) -> Set.add dest defs
        | HeapStore _ -> defs  // No destination register
        | HeapLoad (dest, _, _, _) -> Set.add dest defs
        | StringConcat (dest, _, _, _) -> Set.add dest defs
        | CanonicalBufferEq (dest, _, _, _) -> Set.add dest defs
        | RefCountInc _ -> defs
        | RefCountDec _ -> defs
        | Print _ -> defs
        | StdoutWrite _ -> defs
        | StdinReadLine dest -> Set.add dest defs
        | FileReadText (dest, _) -> Set.add dest defs
        | FileExists (dest, _) -> Set.add dest defs
        | FileWriteText (dest, _, _) -> Set.add dest defs
        | FileAppendText (dest, _, _) -> Set.add dest defs
        | FileDelete (dest, _) -> Set.add dest defs
        | FileSetExecutable (dest, _) -> Set.add dest defs
        | FileWriteFromPtr (dest, _, _, _) -> Set.add dest defs
        | Phi (dest, _, _) -> Set.add dest defs
        | RawAlloc (dest, _) -> Set.add dest defs
        | MappedAlloc (dest, _) -> Set.add dest defs
        | RawFree _ -> defs
        | MappedFree _ -> defs
        | RawGet (dest, _, _, _) -> Set.add dest defs
        | RawGetByte (dest, _, _) -> Set.add dest defs
        | StringToRawPtr (dest, _) -> Set.add dest defs
        | RawPtrToString (dest, _) -> Set.add dest defs
        | BlobToRawPtr (dest, _) -> Set.add dest defs
        | RawPtrToBlob (dest, _) -> Set.add dest defs
        | DictToRawPtr (dest, _) -> Set.add dest defs
        | RawPtrToDict (dest, _, _) -> Set.add dest defs
        | ListToRawPtr (dest, _) -> Set.add dest defs
        | RawPtrToList (dest, _, _) -> Set.add dest defs
        | RawWriteWord _ -> defs
        | RawWriteByte _ -> defs
        | RawSlotInit _ -> defs
        | FloatSqrt (dest, _) -> Set.add dest defs
        | FloatAbs (dest, _) -> Set.add dest defs
        | FloatNeg (dest, _) -> Set.add dest defs
        | Int64ToFloat (dest, _) -> Set.add dest defs
        | FloatToInt64 (dest, _) -> Set.add dest defs
        | FloatToBits (dest, _) -> Set.add dest defs
        | RefCountIncString _ -> defs
        | RefCountDecString _ -> defs
        | RefCountIncBlob _ -> defs
        | RefCountDecBlob _ -> defs
        | RandomInt64 dest -> Set.add dest defs
        | DateTimeNow dest -> Set.add dest defs
        | Sleep (_, dest, _) -> Set.add dest defs
        | CliNative (dest, _, _) -> Set.add dest defs
        | FloatToString (dest, _) -> Set.add dest defs
        | RuntimeError _ -> defs
        | RuntimeErrorString _ -> defs
        | CoverageHit _ -> defs  // No destination register
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

let private addOperandUse (op: Operand) (uses: Set<VReg>) : Set<VReg> =
    match op with
    | Register vreg -> Set.add vreg uses
    | _ -> uses

/// Get all variables used (read) in a basic block
/// Returns set of VRegs that are read in the block
let getBlockUses (block: BasicBlock) : Set<VReg> =
    let instrUses =
        block.Instrs
        |> List.fold (fun uses instr ->
            match instr with
            | Mov (_, src, _) -> addOperandUse src uses
            | BinOp (_, _, left, right, _) ->
                uses |> addOperandUse left |> addOperandUse right
            | UnaryOp (_, _, src) -> addOperandUse src uses
            | Call (_, _, args, _, _) ->
                args |> List.fold (fun u a -> addOperandUse a u) uses
            | TailCall (_, args, _, _) ->
                args |> List.fold (fun u a -> addOperandUse a u) uses
            | IndirectCall (_, func, args, _, _) ->
                args
                |> List.fold (fun u a -> addOperandUse a u) (addOperandUse func uses)
            | IndirectTailCall (func, args, _, _) ->
                args
                |> List.fold (fun u a -> addOperandUse a u) (addOperandUse func uses)
            | ClosureAlloc (_, _, captures) ->
                captures |> List.fold (fun u c -> addOperandUse c u) uses
            | ClosureCall (_, closure, args, _, _) ->
                args
                |> List.fold (fun u a -> addOperandUse a u) (addOperandUse closure uses)
            | ClosureTailCall (closure, args, _) ->
                args
                |> List.fold (fun u a -> addOperandUse a u) (addOperandUse closure uses)
            | HeapAlloc _ -> uses
            | HeapStore (addr, _, src, _) ->
                uses |> Set.add addr |> addOperandUse src
            | HeapLoad (_, addr, _, _) -> Set.add addr uses
            | StringConcat (_, first, second, remaining) ->
                first :: second :: remaining
                |> List.fold (fun current operand -> addOperandUse operand current) uses
            | CanonicalBufferEq (_, _, left, right) ->
                uses |> addOperandUse left |> addOperandUse right
            | RefCountInc (addr, _, _, _) -> Set.add addr uses
            | RefCountDec (addr, _, _, _) -> Set.add addr uses
            | Print (src, _) -> addOperandUse src uses
            | StdoutWrite (_, src, _) -> addOperandUse src uses
            | StdinReadLine _ -> uses
            | FileReadText (_, path) -> addOperandUse path uses
            | FileExists (_, path) -> addOperandUse path uses
            | FileWriteText (_, path, content) ->
                uses |> addOperandUse path |> addOperandUse content
            | FileAppendText (_, path, content) ->
                uses |> addOperandUse path |> addOperandUse content
            | FileDelete (_, path) -> addOperandUse path uses
            | FileSetExecutable (_, path) -> addOperandUse path uses
            | FileWriteFromPtr (_, path, ptr, length) ->
                uses |> addOperandUse path |> addOperandUse ptr |> addOperandUse length
            | Phi (_, sources, _) ->
                sources |> List.fold (fun u (src, _) -> addOperandUse src u) uses
            | RawAlloc (_, numBytes) -> addOperandUse numBytes uses
            | MappedAlloc (_, numBytes) -> addOperandUse numBytes uses
            | RawFree ptr -> addOperandUse ptr uses
            | MappedFree ptr -> addOperandUse ptr uses
            | RawGet (_, ptr, offset, _) ->
                uses |> addOperandUse ptr |> addOperandUse offset
            | RawGetByte (_, ptr, offset) ->
                uses |> addOperandUse ptr |> addOperandUse offset
            | StringToRawPtr (_, value) ->
                addOperandUse value uses
            | RawPtrToString (_, ptr) ->
                addOperandUse ptr uses
            | BlobToRawPtr (_, value) ->
                addOperandUse value uses
            | RawPtrToBlob (_, ptr) ->
                addOperandUse ptr uses
            | DictToRawPtr (_, dict) ->
                addOperandUse dict uses
            | RawPtrToDict (_, ptr, tag) ->
                uses |> addOperandUse ptr |> addOperandUse tag
            | ListToRawPtr (_, list) ->
                addOperandUse list uses
            | RawPtrToList (_, ptr, tag) ->
                uses |> addOperandUse ptr |> addOperandUse tag
            | RawWriteWord (ptr, offset, value) ->
                uses |> addOperandUse ptr |> addOperandUse offset |> addOperandUse value
            | RawWriteByte (ptr, offset, value) ->
                uses |> addOperandUse ptr |> addOperandUse offset |> addOperandUse value
            | RawSlotInit (ptr, offset, value, _) ->
                uses |> addOperandUse ptr |> addOperandUse offset |> addOperandUse value
            | FloatSqrt (_, src) -> addOperandUse src uses
            | FloatAbs (_, src) -> addOperandUse src uses
            | FloatNeg (_, src) -> addOperandUse src uses
            | Int64ToFloat (_, src) -> addOperandUse src uses
            | FloatToInt64 (_, src) -> addOperandUse src uses
            | FloatToBits (_, src) -> addOperandUse src uses
            | RefCountIncString str -> addOperandUse str uses
            | RefCountDecString str -> addOperandUse str uses
            | RefCountIncBlob bytes -> addOperandUse bytes uses
            | RefCountDecBlob bytes -> addOperandUse bytes uses
            | RandomInt64 _ -> uses  // No operand uses
            | DateTimeNow _ -> uses      // No operand uses
            | Sleep (_, _, delayMs) -> addOperandUse delayMs uses
            | CliNative (_, _, args) ->
                args |> List.fold (fun current arg -> addOperandUse arg current) uses
            | FloatToString (_, value) -> addOperandUse value uses
            | RuntimeError _ -> uses
            | RuntimeErrorString message -> addOperandUse message uses
            | CoverageHit _ -> uses  // No operand uses
        ) Set.empty

    // Also include uses in terminator
    let termUses =
        match block.Terminator with
        | Ret op -> addOperandUse op instrUses
        | Branch (cond, _, _) -> addOperandUse cond instrUses
        | Jump _ -> instrUses

    termUses

/// Get successor labels of a block
let getSuccessors (block: BasicBlock) : Label list =
    match block.Terminator with
    | Ret _ -> []
    | Jump target -> [target]
    | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]

let private labelName (Label name) = name

let private requireBlock (context: string) (blocks: Map<Label, BasicBlock>) (label: Label) : BasicBlock =
    match Map.tryFind label blocks with
    | Some block -> block
    | None -> Crash.crash $"SSA: Missing CFG block {labelName label} while {context}"

/// Compute liveness information for the CFG
/// Returns (liveIn, liveOut) maps from Label to Set<VReg>
/// A variable is live-in at a block if it may be used before being defined
/// A variable is live-out at a block if it's live-in at any successor
let private computeLivenessForVRegs
    (trackedVRegs: Set<VReg> option)
    (includeLiveOut: bool)
    (cfg: CFG)
    : Map<Label, Set<VReg>> * Map<Label, Set<VReg>> =
    let labelIndex = buildLabelIndex cfg
    let labels = labelIndex.Labels
    let labelCount = labels.Length

    let usesAndDefs =
        labels
        |> Array.map (fun label ->
            let block = requireBlock "precomputing liveness" cfg.Blocks label
            (getBlockUses block, getBlockDefs block))

    let allVRegs =
        match trackedVRegs with
        | Some tracked -> tracked
        | None ->
            usesAndDefs
            |> Array.fold (fun all (uses, defs) ->
                all |> Set.union uses |> Set.union defs) Set.empty
    let vregIndex = buildVRegIndex allVRegs

    let setToBits (vregs: Set<VReg>) : Bitset.Bitset =
        let bits = Bitset.empty vregIndex.WordCount
        vregs
        |> Set.iter (fun vreg ->
            match vregIndex.IndexOf.TryGetValue vreg with
            | true, idx -> Bitset.addIndexInPlace idx bits
            | false, _ -> ())
        bits

    let blockUses = usesAndDefs |> Array.map (fst >> setToBits)
    let blockDefs = usesAndDefs |> Array.map (snd >> setToBits)

    let successorIndices =
        labels
        |> Array.map (fun label ->
            let block = requireBlock "computing liveness for block" cfg.Blocks label
            getSuccessors block
            |> List.map (fun successor ->
                let _successorBlock =
                    requireBlock "computing liveness successor" cfg.Blocks successor
                match Map.tryFind successor labelIndex.IndexOf with
                | Some idx -> idx
                | None ->
                    Crash.crash
                        $"SSA: Missing label index for {labelName successor} while computing liveness successor"))

    let entryIndex =
        match Map.tryFind cfg.Entry labelIndex.IndexOf with
        | Some idx -> idx
        | None -> Crash.crash "SSA: Missing entry label index while ordering liveness"

    // Liveness flows from successors to predecessors. Postorder solves an
    // acyclic region in one pass, while the surrounding fixed point retains
    // exact behavior for loop backedges. The explicit work stack keeps CFG
    // ordering stack-safe for large generated functions.
    let backwardDataflowOrder =
        let roots = entryIndex :: [0 .. labelCount - 1]
        let rec visit
            (work: (int * bool) list)
            (visited: Set<int>)
            (postorderRev: int list)
            : int list =
            match work with
            | [] -> List.rev postorderRev
            | (blockIdx, expanded) :: remaining ->
                if expanded then
                    visit remaining visited (blockIdx :: postorderRev)
                elif Set.contains blockIdx visited then
                    visit remaining visited postorderRev
                else
                    let successors =
                        successorIndices.[blockIdx]
                        |> List.map (fun successorIdx -> (successorIdx, false))
                    visit
                        (successors @ ((blockIdx, true) :: remaining))
                        (Set.add blockIdx visited)
                        postorderRev
        visit (roots |> List.map (fun blockIdx -> (blockIdx, false))) Set.empty []

    let emptyBits = Bitset.empty vregIndex.WordCount
    let liveInByIndex = Array.init labelCount (fun _ -> emptyBits)
    let computeLiveOut (blockIdx: int) =
        Array.init vregIndex.WordCount (fun wordIdx ->
            successorIndices.[blockIdx]
            |> List.fold (fun word successorIdx ->
                word ||| liveInByIndex.[successorIdx].[wordIdx]) 0UL)
    let computeLiveIn (blockIdx: int) (liveOut: Bitset.Bitset) =
        Array.init vregIndex.WordCount (fun wordIdx ->
            blockUses.[blockIdx].[wordIdx]
            ||| (liveOut.[wordIdx] &&& (~~~blockDefs.[blockIdx].[wordIdx])))

    // Within each round, predecessors see successor values computed earlier in
    // the same postorder traversal instead of waiting for another global round.
    let mutable changed = true
    while changed do
        changed <- false
        for blockIdx in backwardDataflowOrder do
            let newLiveIn =
                computeLiveOut blockIdx |> computeLiveIn blockIdx
            if not (Bitset.equal liveInByIndex.[blockIdx] newLiveIn) then
                liveInByIndex.[blockIdx] <- newLiveIn
                changed <- true

    let liveIn = Array.copy liveInByIndex
    let bitsetsToMap (bitsets: Bitset.Bitset array) : Map<Label, Set<VReg>> =
        Array.map2 (fun label bits ->
            let vregs =
                bits
                |> Bitset.indicesToList
                |> List.map (fun idx -> vregIndex.VRegs.[idx])
                |> Set.ofList
            (label, vregs)) labels bitsets
        |> Array.toList
        |> Map.ofList

    let liveOut =
        if includeLiveOut then
            Array.init labelCount computeLiveOut
            |> bitsetsToMap
        else
            Map.empty

    (bitsetsToMap liveIn, liveOut)

let computeLiveness (cfg: CFG) : Map<Label, Set<VReg>> * Map<Label, Set<VReg>> =
    computeLivenessForVRegs None true cfg

/// Insert phi nodes at dominance frontiers
/// For each variable v defined in block b:
///   For each block d in DF(b):
///     Insert phi node for v in d (if not already present AND v is live-in at d)
///     This also counts as a definition, so recursively process
let insertPhiNodes (cfg: CFG) (df: DominanceFrontier) (preds: Predecessors) (liveIn: Map<Label, Set<VReg>>) (funcParams: VReg list) (paramTypes: AST.Type list) : CFG =
    // Create a map from parameter VReg to its type
    let paramTypeMap =
        List.zip funcParams paramTypes
        |> Map.ofList
    // IfValue lowering defines both incoming arms with typed moves. Preserve
    // that type on the SSA phi so floating-point joins stay in FP registers.
    let localMoveTypeMap : Map<VReg, PhiTypeEvidence> =
        cfg.Blocks
        |> Map.fold (fun types _ block ->
            block.Instrs
            |> List.fold (fun types instr ->
                match instr with
                | Mov (dest, _, Some valueType) ->
                    match Map.tryFind dest types with
                    | None -> Map.add dest (KnownPhiType valueType) types
                    | Some (KnownPhiType existing) when existing = valueType -> types
                    | Some _ -> Map.add dest ConflictingPhiTypes types
                | _ -> types
            ) types
        ) Map.empty
    // Get all definitions from instructions in the CFG
    let instrDefs = getAllDefs cfg

    // Add function parameters as definitions at the entry block
    // This is critical for self-recursive functions: params are defined at entry (from args)
    // AND re-defined in recursive blocks (before jumping back). SSA needs both definition
    // sites to insert phi nodes at the loop header (the entry block for such functions).
    let allDefs =
        funcParams
        |> List.fold (fun defs vreg ->
            let existing = Map.tryFind vreg defs |> Option.defaultValue Set.empty
            Map.add vreg (Set.add cfg.Entry existing) defs
        ) instrDefs

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
                            let valueType =
                                match Map.tryFind vreg paramTypeMap with
                                | Some parameterType -> Some parameterType
                                | None ->
                                    match Map.tryFind vreg localMoveTypeMap with
                                    | Some (KnownPhiType localType) -> Some localType
                                    | Some ConflictingPhiTypes | None -> None
                            let phiInstr = Phi (vreg, phiSources, valueType)

                            // Add to block (at the beginning)
                            let existingBlock = requireBlock "inserting phi node" c.Blocks dfBlock
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
    /// Version stacks for original VRegs, mutated during the dominator walk.
    VersionStacks: System.Collections.Generic.Dictionary<VReg, System.Collections.Generic.Stack<int>>
    /// Original VRegs in push order, used to restore the exact scope depth.
    PushedVersions: System.Collections.Generic.Stack<VReg>
    /// Next available version number
    mutable NextVersion: int
    /// Original floatRegs set (VReg IDs that are floats)
    OriginalFloatRegs: Set<int>
    /// Updated floatRegs set (includes SSA renamed VRegs)
    FloatRegs: System.Collections.Generic.HashSet<int>
}

let private vregId (VReg id) : int = id

/// Create initial renaming state, starting VReg numbers above any existing VRegs
let createInitialRenamingState (cfg: CFG) (floatRegs: Set<int>) (extraRegs: VReg list) : RenamingState =
    // Preserve the existing numbering scheme of starting 10000 above CFG
    // definitions, while also staying above parameter registers that are not
    // materialized as MIR definitions.
    let nextVersionStart =
        let cfgMax =
            cfg.Blocks
            |> Map.fold (fun regs _ block ->
                Set.union regs (getBlockDefs block)
            ) Set.empty
            |> Set.fold (fun maxSoFar reg -> max maxSoFar (vregId reg)) 0

        let extraMax =
            extraRegs
            |> List.fold (fun maxSoFar reg -> max maxSoFar (vregId reg)) 0

        max (cfgMax + 10000) (extraMax + 1)

    {
        VersionStacks =
            System.Collections.Generic.Dictionary<VReg, System.Collections.Generic.Stack<int>>()
        PushedVersions = System.Collections.Generic.Stack<VReg>()
        NextVersion = nextVersionStart
        OriginalFloatRegs = floatRegs
        FloatRegs = System.Collections.Generic.HashSet<int>(floatRegs)
    }

/// Create new version for a definition
let newVersion (state: RenamingState) (vreg: VReg) : int * VReg * RenamingState =
    let version = state.NextVersion
    let newReg = VReg version

    let stack =
        match state.VersionStacks.TryGetValue vreg with
        | true, stack -> stack
        | false, _ ->
            let stack = System.Collections.Generic.Stack<int>()
            state.VersionStacks.[vreg] <- stack
            stack
    stack.Push version
    state.PushedVersions.Push vreg

    // If the original VReg was a float, the new SSA version is also a float
    let (VReg origId) = vreg
    if Set.contains origId state.OriginalFloatRegs then
        state.FloatRegs.Add version |> ignore

    state.NextVersion <- version + 1
    (version, newReg, state)

/// Get the renamed VReg for a use
let getRenamedReg (state: RenamingState) (vreg: VReg) : VReg =
    match state.VersionStacks.TryGetValue vreg with
    | true, stack when stack.Count > 0 -> VReg (stack.Peek())
    | _ -> vreg

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

    | TailCall (funcName, args, argTypes, returnType) ->
        let args' = args |> List.map (renameOperand state)
        (TailCall (funcName, args', argTypes, returnType), state)  // No dest

    | IndirectCall (dest, func, args, argTypes, returnType) ->
        let func' = renameOperand state func
        let args' = args |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (IndirectCall (newDest, func', args', argTypes, returnType), state')

    | IndirectTailCall (func, args, argTypes, returnType) ->
        let func' = renameOperand state func
        let args' = args |> List.map (renameOperand state)
        (IndirectTailCall (func', args', argTypes, returnType), state)  // No dest

    | ClosureAlloc (dest, funcName, captures) ->
        let captures' = captures |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (ClosureAlloc (newDest, funcName, captures'), state')

    | ClosureCall (dest, closure, args, argTypes, returnType) ->
        let closure' = renameOperand state closure
        let args' = args |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (ClosureCall (newDest, closure', args', argTypes, returnType), state')

    | ClosureTailCall (closure, args, argTypes) ->
        let closure' = renameOperand state closure
        let args' = args |> List.map (renameOperand state)
        (ClosureTailCall (closure', args', argTypes), state)  // No dest

    | HeapAlloc (dest, size) ->
        let (_, newDest, state') = newVersion state dest
        (HeapAlloc (newDest, size), state')

    | HeapStore (addr, offset, src, vt) ->
        let src' = renameOperand state src
        // addr is used, not defined
        let addr' = getRenamedReg state addr
        (HeapStore (addr', offset, src', vt), state)

    | HeapLoad (dest, addr, offset, vt) ->
        let addr' = getRenamedReg state addr
        let (_, newDest, state') = newVersion state dest
        (HeapLoad (newDest, addr', offset, vt), state')

    | StringConcat (dest, first, second, remaining) ->
        let first' = renameOperand state first
        let second' = renameOperand state second
        let remaining' = List.map (renameOperand state) remaining
        let (_, newDest, state') = newVersion state dest
        (StringConcat (newDest, first', second', remaining'), state')

    | CanonicalBufferEq (dest, kind, left, right) ->
        let left' = renameOperand state left
        let right' = renameOperand state right
        let (_, newDest, state') = newVersion state dest
        (CanonicalBufferEq (newDest, kind, left', right'), state')

    | RefCountInc (addr, size, kind, sourceType) ->
        let addr' = getRenamedReg state addr
        (RefCountInc (addr', size, kind, sourceType), state)

    | RefCountDec (addr, size, kind, sourceType) ->
        let addr' = getRenamedReg state addr
        (RefCountDec (addr', size, kind, sourceType), state)

    | Print (src, vt) ->
        let src' = renameOperand state src
        (Print (src', vt), state)

    | StdoutWrite (effectId, src, appendNewline) ->
        (StdoutWrite (effectId, renameOperand state src, appendNewline), state)

    | StdinReadLine dest ->
        let (_, newDest, state') = newVersion state dest
        (StdinReadLine newDest, state')

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

    | FileWriteFromPtr (dest, path, ptr, length) ->
        let path' = renameOperand state path
        let ptr' = renameOperand state ptr
        let length' = renameOperand state length
        let (_, newDest, state') = newVersion state dest
        (FileWriteFromPtr (newDest, path', ptr', length'), state')

    | Phi (dest, sources, valueType) ->
        // Phi sources are renamed when processing predecessors
        // Here we just rename the destination
        let (_, newDest, state') = newVersion state dest
        (Phi (newDest, sources, valueType), state')

    | RawAlloc (dest, numBytes) ->
        let numBytes' = renameOperand state numBytes
        let (_, newDest, state') = newVersion state dest
        (RawAlloc (newDest, numBytes'), state')

    | MappedAlloc (dest, numBytes) ->
        let numBytes' = renameOperand state numBytes
        let (_, newDest, state') = newVersion state dest
        (MappedAlloc (newDest, numBytes'), state')

    | RawFree ptr ->
        let ptr' = renameOperand state ptr
        (RawFree ptr', state)

    | MappedFree ptr ->
        let ptr' = renameOperand state ptr
        (MappedFree ptr', state)

    | RawGet (dest, ptr, byteOffset, valueType) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let (_, newDest, state') = newVersion state dest
        (RawGet (newDest, ptr', byteOffset', valueType), state')

    | RawGetByte (dest, ptr, byteOffset) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let (_, newDest, state') = newVersion state dest
        (RawGetByte (newDest, ptr', byteOffset'), state')

    | StringToRawPtr (dest, value) ->
        let value' = renameOperand state value
        let (_, newDest, state') = newVersion state dest
        (StringToRawPtr (newDest, value'), state')

    | RawPtrToString (dest, ptr) ->
        let ptr' = renameOperand state ptr
        let (_, newDest, state') = newVersion state dest
        (RawPtrToString (newDest, ptr'), state')

    | BlobToRawPtr (dest, value) ->
        let value' = renameOperand state value
        let (_, newDest, state') = newVersion state dest
        (BlobToRawPtr (newDest, value'), state')

    | RawPtrToBlob (dest, ptr) ->
        let ptr' = renameOperand state ptr
        let (_, newDest, state') = newVersion state dest
        (RawPtrToBlob (newDest, ptr'), state')

    | DictToRawPtr (dest, dict) ->
        let dict' = renameOperand state dict
        let (_, newDest, state') = newVersion state dest
        (DictToRawPtr (newDest, dict'), state')

    | RawPtrToDict (dest, ptr, tag) ->
        let ptr' = renameOperand state ptr
        let tag' = renameOperand state tag
        let (_, newDest, state') = newVersion state dest
        (RawPtrToDict (newDest, ptr', tag'), state')

    | ListToRawPtr (dest, list) ->
        let list' = renameOperand state list
        let (_, newDest, state') = newVersion state dest
        (ListToRawPtr (newDest, list'), state')

    | RawPtrToList (dest, ptr, tag) ->
        let ptr' = renameOperand state ptr
        let tag' = renameOperand state tag
        let (_, newDest, state') = newVersion state dest
        (RawPtrToList (newDest, ptr', tag'), state')

    | RawWriteWord (ptr, byteOffset, value) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let value' = renameOperand state value
        (RawWriteWord (ptr', byteOffset', value'), state)

    | RawWriteByte (ptr, byteOffset, value) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let value' = renameOperand state value
        (RawWriteByte (ptr', byteOffset', value'), state)

    | RawSlotInit (ptr, byteOffset, value, valueType) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let value' = renameOperand state value
        (RawSlotInit (ptr', byteOffset', value', valueType), state)

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

    | Int64ToFloat (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (Int64ToFloat (newDest, src'), state')

    | FloatToInt64 (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatToInt64 (newDest, src'), state')

    | FloatToBits (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatToBits (newDest, src'), state')

    | RefCountIncString str ->
        let str' = renameOperand state str
        (RefCountIncString str', state)

    | RefCountDecString str ->
        let str' = renameOperand state str
        (RefCountDecString str', state)
    | RefCountIncBlob bytes ->
        let bytes' = renameOperand state bytes
        (RefCountIncBlob bytes', state)

    | RefCountDecBlob bytes ->
        let bytes' = renameOperand state bytes
        (RefCountDecBlob bytes', state)

    | RandomInt64 dest ->
        let (_, newDest, state') = newVersion state dest
        (RandomInt64 newDest, state')

    | DateTimeNow dest ->
        let (_, newDest, state') = newVersion state dest
        (DateTimeNow newDest, state')

    | Sleep (effectId, dest, delayMs) ->
        let delayMs' = renameOperand state delayMs
        let (_, newDest, state') = newVersion state dest
        (Sleep (effectId, newDest, delayMs'), state')

    | CliNative (dest, operation, args) ->
        let args' = List.map (renameOperand state) args
        let (_, newDest, state') = newVersion state dest
        (CliNative (newDest, operation, args'), state')

    | FloatToString (dest, value) ->
        let value' = renameOperand state value
        let (_, newDest, state') = newVersion state dest
        (FloatToString (newDest, value'), state')

    | RuntimeError message ->
        (RuntimeError message, state)

    | RuntimeErrorString message ->
        (RuntimeErrorString (renameOperand state message), state)

    | CoverageHit exprId ->
        (CoverageHit exprId, state)  // No registers to rename

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
            (instr' :: acc, s')
        ) ([], state)

    // Rename terminator
    let term' = renameTerminator state' block.Terminator

    ({ block with Instrs = List.rev instrs'; Terminator = term' }, state')

type PhiSourceUpdates = Map<Label * Label * VReg, Operand>

/// Apply deferred predecessor-specific source versions without disturbing the
/// instruction or predecessor order established during phi insertion.
let applyPhiSourceUpdates (updates: PhiSourceUpdates) (block: BasicBlock) : BasicBlock =
    let rec updateLeadingPhiSources instrs =
        match instrs with
        | Phi (dest, sources, valueType) :: tail ->
            let sources' =
                sources
                |> List.map (fun (source, fromLabel) ->
                    match source with
                    | Register sourceReg ->
                        match Map.tryFind (block.Label, fromLabel, sourceReg) updates with
                        | Some renamedSource -> (renamedSource, fromLabel)
                        | None -> (source, fromLabel)
                    | _ -> (source, fromLabel))
            Phi (dest, sources', valueType) :: updateLeadingPhiSources tail
        | tail -> tail
    let instrs = updateLeadingPhiSources block.Instrs
    { block with Instrs = instrs }

/// Record the renamed values supplied by one predecessor. Applying these
/// records after the dominator walk avoids rebuilding successor blocks and the
/// persistent CFG map once per incoming edge.
let private collectPhiSourceUpdatesForSuccessors
    (cfg: CFG)
    (currentLabel: Label)
    (state: RenamingState)
    : ((Label * Label * VReg) * Operand) list =
    let block = requireBlock "updating successor phi sources" cfg.Blocks currentLabel

    // Get successor labels from terminator
    let terminatorSuccessors =
        match block.Terminator with
        | Ret _ -> []
        | Jump target -> [target]
        | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]

    // Each source is keyed by successor, predecessor, and original register.
    // Phi insertion creates register sources; non-register sources are retained
    // unchanged by applyPhiSourceUpdates.
    terminatorSuccessors
    |> List.collect (fun succLabel ->
        let succBlock = requireBlock "collecting successor phi sources" cfg.Blocks succLabel
        let rec collectLeadingPhiSources instrs updates =
            match instrs with
            | Phi (_, sources, _) :: tail ->
                let updates' =
                    sources
                    |> List.fold (fun collected (source, fromLabel) ->
                        match source with
                        | Register sourceReg when fromLabel = currentLabel ->
                            ((succLabel, currentLabel, sourceReg), renameOperand state source)
                            :: collected
                        | _ -> collected) updates
                collectLeadingPhiSources tail updates'
            | _ -> List.rev updates
        collectLeadingPhiSources succBlock.Instrs [])

/// Build dominator tree children
let buildDomTree (idoms: Dominators) : Map<Label, Label list> =
    idoms
    |> Map.fold (fun tree label idom ->
        let children = Map.tryFind idom tree |> Option.defaultValue []
        Map.add idom (label :: children) tree
    ) Map.empty

/// Restore the version stacks to a dominator scope boundary.
let private popVersionsToDepth (state: RenamingState) (depth: int) : unit =
    while state.PushedVersions.Count > depth do
        let originalReg = state.PushedVersions.Pop()
        match state.VersionStacks.TryGetValue originalReg with
        | true, stack when stack.Count > 0 ->
            stack.Pop() |> ignore
        | _ ->
            Crash.crash $"SSA: Missing version stack while restoring {originalReg}"

/// Rename CFG using dominator tree traversal
/// Rename CFG to SSA form
/// Returns (renamed CFG, updated floatRegs set with SSA versions)
let renameCFG (cfg: CFG) (idoms: Dominators) (floatRegs: Set<int>) (paramRegs: VReg list) : CFG * Set<int> =
    let domTree = buildDomTree idoms

    // DFS traversal of dominator tree
    // Renamed blocks and phi updates are accumulated as lists and materialized
    // once after traversal, while state carries NextVersion across siblings.
    let rec visit
        (label: Label)
        (state: RenamingState)
        (renamedBlocks: (Label * BasicBlock) list)
        (phiUpdates: ((Label * Label * VReg) * Operand) list)
        : (Label * BasicBlock) list * ((Label * Label * VReg) * Operand) list * RenamingState =
        let block = requireBlock "renaming CFG block" cfg.Blocks label
        let scopeDepth = state.PushedVersions.Count

        // Rename this block
        let (block', state') = renameBlock state block
        let renamedBlocks' = (label, block') :: renamedBlocks
        let blockPhiUpdates = collectPhiSourceUpdatesForSuccessors cfg label state'
        let phiUpdates' =
            blockPhiUpdates |> List.fold (fun updates update -> update :: updates) phiUpdates

        // Child visits restore their own pushes before returning, leaving this
        // block's versions visible to every dominated sibling.
        let children = Map.tryFind label domTree |> Option.defaultValue []
        let (finalBlocks, finalPhiUpdates, finalState) =
            children
            |> List.fold (fun (blocks, updates, s) child ->
                visit child s blocks updates
            ) (renamedBlocks', phiUpdates', state')

        popVersionsToDepth finalState scopeDepth

        (finalBlocks, finalPhiUpdates, finalState)

    // Start from entry with initial state based on CFG's existing VRegs
    let initialState = createInitialRenamingState cfg floatRegs paramRegs
    let (renamedBlocks, phiUpdateEntries, finalState) = visit cfg.Entry initialState [] []
    let renamedByLabel = renamedBlocks |> Map.ofList
    let phiUpdates = phiUpdateEntries |> Map.ofList
    let finalBlocks =
        cfg.Blocks
        |> Map.map (fun label originalBlock ->
            let renamedBlock =
                match Map.tryFind label renamedByLabel with
                | Some block -> block
                | None -> originalBlock
            applyPhiSourceUpdates phiUpdates renamedBlock)
    let resultCfg = { cfg with Blocks = finalBlocks }
    (resultCfg, finalState.FloatRegs |> Set.ofSeq)

/// Convert a function to SSA form
let private convertFunctionToSSAInternal
    (swOpt: System.Diagnostics.Stopwatch option)
    (func: Function)
    : Function * SSAConstructionTiming list =
    let cfg = func.CFG
    let (preds, timingsRev) =
        timePhase swOpt "SSA: Predecessors" [] (fun () -> buildPredecessors cfg)
    let (idoms, timingsRev) =
        timePhase swOpt "SSA: Dominators" timingsRev (fun () -> computeDominators cfg preds)
    let (df, timingsRev) =
        timePhase swOpt "SSA: Dominance Frontier" timingsRev (fun () -> computeDominanceFrontier cfg preds idoms)

    let paramRegs = func.TypedParams |> List.map (fun tp -> tp.Reg)
    let paramTypes = func.TypedParams |> List.map (fun tp -> tp.Type)
    let phiCandidateVRegs =
        let definitionSites =
            paramRegs
            |> List.fold (fun defs vreg ->
                let existing =
                    Map.tryFind vreg defs |> Option.defaultValue Set.empty
                Map.add vreg (Set.add cfg.Entry existing) defs
            ) (getAllDefs cfg)
        definitionSites
        |> Map.fold (fun candidates vreg sites ->
            let hasDominanceFrontier =
                sites
                |> Set.exists (fun site ->
                    Map.tryFind site df
                    |> Option.map (Set.isEmpty >> not)
                    |> Option.defaultValue false)
            if hasDominanceFrontier then Set.add vreg candidates else candidates
        ) Set.empty

    // Phi placement only asks whether candidate variables are live. Liveness
    // is independent per variable, so unrelated single-definition temporaries
    // need not widen every block bitset.
    let ((liveIn, _), timingsRev) =
        timePhase swOpt "SSA: Liveness" timingsRev (fun () ->
            if Set.isEmpty phiCandidateVRegs then
                (Map.empty, Map.empty)
            else
                computeLivenessForVRegs (Some phiCandidateVRegs) false cfg)

    // Insert phi nodes (only for live variables)
    // Pass function params so they're treated as defined at entry (for self-recursive functions)
    let (cfgWithPhis, timingsRev) =
        timePhase swOpt "SSA: Phi Insertion" timingsRev (fun () ->
            insertPhiNodes cfg df preds liveIn paramRegs paramTypes)

    // Rename variables and update floatRegs with SSA versions
    let ((ssaCFG, updatedFloatRegs), timingsRev) =
        timePhase swOpt "SSA: Renaming" timingsRev (fun () ->
            renameCFG cfgWithPhis idoms func.FloatRegs paramRegs)

    ({ func with CFG = ssaCFG; FloatRegs = updatedFloatRegs }, List.rev timingsRev)

/// Convert a function to SSA form.
let convertFunctionToSSA (func: Function) : Function =
    convertFunctionToSSAInternal None func |> fst

/// Convert one function to SSA form and retain its nested phase timings.
let convertFunctionToSSAWithTiming
    (func: Function)
    : Function * SSAConstructionTiming list =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    convertFunctionToSSAInternal (Some sw) func

/// Convert a program to SSA form
let convertToSSA (program: Program) : Program =
    let (Program (functions, variants, records)) = program
    let functions' = functions |> List.map convertFunctionToSSA
    Program (functions', variants, records)

/// Convert a program to SSA form and collect aggregate phase timings.
let convertToSSAWithTiming (program: Program) : Program * SSAConstructionTiming list =
    let (Program (functions, variants, records)) = program
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let (functionsRev, timingsRev) =
        functions
        |> List.fold (fun (converted, collectedTimingsRev) func ->
            let (convertedFunc, functionTimings) =
                convertFunctionToSSAInternal (Some sw) func
            let collectedTimingsRev =
                functionTimings
                |> List.fold (fun acc timing -> timing :: acc) collectedTimingsRev
            (convertedFunc :: converted, collectedTimingsRev)
        ) ([], [])
    (Program (List.rev functionsRev, variants, records), List.rev timingsRev)
