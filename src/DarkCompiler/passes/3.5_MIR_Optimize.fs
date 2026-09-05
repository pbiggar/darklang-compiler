// 3.5_MIR_Optimize.fs - MIR/SSA Optimization Pass
//
// Performs optimizations on MIR in SSA form:
// - Constant folding: evaluate constant operations
// - Common subexpression elimination (CSE): reuse identical computations
// - Copy propagation: eliminate trivial moves and phis
// - Dead code elimination (DCE): remove unused instructions
// - CFG simplification: fold branches, prune unreachable blocks, and merge linear blocks
// - Loop-invariant code motion (LICM): hoist loop-invariant expressions
// - Induction-variable strength reduction: carry a canonical affine loop expression
// - Counted-loop unrolling: execute two small scalar iterations per backedge
//
// These optimizations leverage SSA form where each variable is defined exactly once.

module MIR_Optimize

open MIR
open Output
open SSA_Construction

type OptimizeOptions = {
    EnableConstFolding: bool
    EnableCSE: bool
    EnableCopyProp: bool
    EnableDCE: bool
    EnableCFGSimplify: bool
    EnableLICM: bool
}

let defaultOptimizeOptions = {
    EnableConstFolding = true
    EnableCSE = true
    EnableCopyProp = true
    EnableDCE = true
    EnableCFGSimplify = true
    EnableLICM = true
}

/// Check if an instruction has side effects (must be preserved even if unused)
let hasSideEffects (instr: Instr) : bool =
    match instr with
    | Mov _ -> false
    | BinOp _ -> false
    | UnaryOp _ -> false
    | Phi _ -> false
    | HeapLoad _ -> false
    // These have side effects
    | Call _ -> true  // Function calls may have side effects
    | TailCall _ -> true  // Tail calls have side effects
    | IndirectCall _ -> true
    | IndirectTailCall _ -> true  // Indirect tail calls have side effects
    | ClosureAlloc _ -> true  // Allocates memory
    | ClosureCall _ -> true
    | ClosureTailCall _ -> true  // Closure tail calls have side effects
    | HeapAlloc _ -> true  // Allocates memory
    | HeapStore _ -> true  // Writes to memory
    | StringConcat _ -> true  // Allocates memory
    | RefCountInc _ -> true
    | RefCountDec _ -> true
    | Print _ -> true
    | StdoutWrite _ -> true
    | StdinReadLine _ -> true
    | FileReadText _ -> true
    | FileExists _ -> true
    | FileWriteText _ -> true
    | FileAppendText _ -> true
    | FileDelete _ -> true
    | FileSetExecutable _ -> true
    | FileWriteFromPtr _ -> true  // File I/O
    | RawAlloc _ -> true  // Allocates memory
    | RawFree _ -> true   // Frees memory
    | RawGet _ -> false   // Pure memory read
    | RawGetByte _ -> false  // Pure memory read (byte)
    | RawWriteWord _ -> true    // Writes to memory
    | RawWriteByte _ -> true  // Writes to memory (byte)
    | RawSlotInit _ -> true  // Writes to memory and may retain a typed edge
    | StringToRawPtr _ -> false
    | RawPtrToString _ -> false
    | BlobToRawPtr _ -> false
    | RawPtrToBlob _ -> false
    | DictToRawPtr _ -> false
    | RawPtrToDict _ -> false
    | ListToRawPtr _ -> false
    | RawPtrToList _ -> false
    | FloatSqrt _ -> false  // Pure float operation
    | FloatAbs _ -> false   // Pure float operation
    | FloatNeg _ -> false   // Pure float operation
    | Int64ToFloat _ -> false // Pure conversion
    | FloatToInt64 _ -> false // Pure conversion
    | FloatToBits _ -> false  // Pure conversion
    | RefCountIncString _ -> true   // Mutates refcount
    | RefCountDecString _ -> true   // Mutates refcount
    | RefCountIncBlob _ -> true    // Mutates refcount
    | RefCountDecBlob _ -> true    // Mutates refcount
    | RandomInt64 _ -> true  // Syscall
    | DateTimeNow _ -> true      // Syscall
    | Sleep _ -> true            // Blocking syscall
    | CliNative _ -> true
    | FloatToString _ -> false  // Pure conversion (allocates but no visible side effect)
    | RuntimeError _ -> true
    | RuntimeErrorString _ -> true
    | CoverageHit _ -> true  // Must not be eliminated (tracking side effect)

/// Find functions whose reachable call-graph components contain no MIR effects.
/// Starting from every locally effect-free function and removing callers of
/// unproven functions computes the greatest fixed point, so mutually recursive
/// and self-recursive components remain provable without assuming unknown calls
/// are safe.
let private analyzeEffectFreeFunctions (functions: Function list) : Set<string> =
    let directCallee instr =
        match instr with
        | Call (_, funcName, _, _, _)
        | TailCall (funcName, _, _, _) -> Some funcName
        | _ -> None

    let locallyEffectFree func =
        func.CFG.Blocks
        |> Map.forall (fun _ block ->
            block.Instrs
            |> List.forall (fun instr ->
                match directCallee instr with
                | Some _ -> true
                | None -> not (hasSideEffects instr)))

    let directCallees func =
        func.CFG.Blocks
        |> Map.toList
        |> List.collect (fun (_, block) -> block.Instrs)
        |> List.choose directCallee
        |> Set.ofList

    let candidates = functions |> List.filter locallyEffectFree

    let rec removeCallersOfUnprovenFunctions provenNames =
        let next =
            candidates
            |> List.filter (fun func ->
                directCallees func |> Set.forall (fun callee -> Set.contains callee provenNames))
            |> List.map (fun func -> func.Name)
            |> Set.ofList

        if next = provenNames then next else removeCallersOfUnprovenFunctions next

    candidates
    |> List.map (fun func -> func.Name)
    |> Set.ofList
    |> removeCallersOfUnprovenFunctions

/// Get the destination VReg of an instruction (if any)
let getInstrDest (instr: Instr) : VReg option =
    match instr with
    | Mov (dest, _, _) -> Some dest
    | BinOp (dest, _, _, _, _) -> Some dest
    | UnaryOp (dest, _, _) -> Some dest
    | Call (dest, _, _, _, _) -> Some dest
    | TailCall _ -> None  // Tail calls don't return here
    | IndirectCall (dest, _, _, _, _) -> Some dest
    | IndirectTailCall _ -> None  // Indirect tail calls don't return here
    | ClosureAlloc (dest, _, _) -> Some dest
    | ClosureCall (dest, _, _, _, _) -> Some dest
    | ClosureTailCall _ -> None  // Closure tail calls don't return here
    | HeapAlloc (dest, _) -> Some dest
    | HeapLoad (dest, _, _, _) -> Some dest
    | StringConcat (dest, _, _) -> Some dest
    | StdinReadLine dest -> Some dest
    | FileReadText (dest, _) -> Some dest
    | FileExists (dest, _) -> Some dest
    | FileWriteText (dest, _, _) -> Some dest
    | FileAppendText (dest, _, _) -> Some dest
    | FileDelete (dest, _) -> Some dest
    | FileSetExecutable (dest, _) -> Some dest
    | FileWriteFromPtr (dest, _, _, _) -> Some dest
    | Phi (dest, _, _) -> Some dest
    | RawAlloc (dest, _) -> Some dest
    | RawGet (dest, _, _, _) -> Some dest
    | RawGetByte (dest, _, _) -> Some dest
    | StringToRawPtr (dest, _) -> Some dest
    | RawPtrToString (dest, _) -> Some dest
    | BlobToRawPtr (dest, _) -> Some dest
    | RawPtrToBlob (dest, _) -> Some dest
    | DictToRawPtr (dest, _) -> Some dest
    | RawPtrToDict (dest, _, _) -> Some dest
    | ListToRawPtr (dest, _) -> Some dest
    | RawPtrToList (dest, _, _) -> Some dest
    | FloatSqrt (dest, _) -> Some dest
    | FloatAbs (dest, _) -> Some dest
    | FloatNeg (dest, _) -> Some dest
    | Int64ToFloat (dest, _) -> Some dest
    | FloatToInt64 (dest, _) -> Some dest
    | FloatToBits (dest, _) -> Some dest
    | HeapStore _ -> None
    | RefCountInc _ -> None
    | RefCountDec _ -> None
    | Print _ -> None
    | StdoutWrite _ -> None
    | RawFree _ -> None
    | RawWriteWord _ -> None
    | RawWriteByte _ -> None
    | RawSlotInit _ -> None
    | RefCountIncString _ -> None
    | RefCountDecString _ -> None
    | RefCountIncBlob _ -> None
    | RefCountDecBlob _ -> None
    | RandomInt64 dest -> Some dest
    | DateTimeNow dest -> Some dest
    | Sleep (_, dest, _) -> Some dest
    | CliNative (dest, _, _) -> Some dest
    | FloatToString (dest, _) -> Some dest
    | RuntimeError _ -> None
    | RuntimeErrorString _ -> None
    | CoverageHit _ -> None

/// Fold over the VRegs used by an instruction without allocating an intermediate collection.
let foldInstrUses (folder: 'State -> VReg -> 'State) (state: 'State) (instr: Instr) : 'State =
    let fromOperand state op =
        match op with
        | Register vreg -> folder state vreg
        | _ -> state

    let fromOperands state operands = List.fold fromOperand state operands

    match instr with
    | Mov (_, src, _) -> fromOperand state src
    | BinOp (_, _, left, right, _) -> fromOperand (fromOperand state left) right
    | UnaryOp (_, _, src) -> fromOperand state src
    | Call (_, _, args, _, _)
    | TailCall (_, args, _, _)
    | ClosureAlloc (_, _, args) -> fromOperands state args
    | IndirectCall (_, func, args, _, _)
    | IndirectTailCall (func, args, _, _)
    | ClosureCall (_, func, args, _, _)
    | ClosureTailCall (func, args, _) -> fromOperands (fromOperand state func) args
    | HeapAlloc _ -> state
    | HeapStore (addr, _, src, _) -> fromOperand (folder state addr) src
    | HeapLoad (_, addr, _, _)
    | RefCountInc (addr, _, _, _)
    | RefCountDec (addr, _, _, _) -> folder state addr
    | StringConcat (_, left, right)
    | FileWriteText (_, left, right)
    | FileAppendText (_, left, right)
    | RawGet (_, left, right, _)
    | RawGetByte (_, left, right)
    | RawPtrToDict (_, left, right)
    | RawPtrToList (_, left, right) -> fromOperand (fromOperand state left) right
    | Print (src, _)
    | StdoutWrite (_, src, _)
    | FileReadText (_, src)
    | FileExists (_, src)
    | FileDelete (_, src)
    | FileSetExecutable (_, src)
    | RawAlloc (_, src)
    | RawFree src
    | StringToRawPtr (_, src)
    | RawPtrToString (_, src)
    | BlobToRawPtr (_, src)
    | RawPtrToBlob (_, src)
    | DictToRawPtr (_, src)
    | ListToRawPtr (_, src)
    | FloatSqrt (_, src)
    | FloatAbs (_, src)
    | FloatNeg (_, src)
    | Int64ToFloat (_, src)
    | FloatToInt64 (_, src)
    | FloatToBits (_, src)
    | RefCountIncString src
    | RefCountDecString src
    | RefCountIncBlob src
    | RefCountDecBlob src
    | FloatToString (_, src) -> fromOperand state src
    | Sleep (_, _, delayMs) -> fromOperand state delayMs
    | FileWriteFromPtr (_, first, second, third)
    | RawWriteWord (first, second, third)
    | RawWriteByte (first, second, third)
    | RawSlotInit (first, second, third, _) ->
        fromOperand (fromOperand (fromOperand state first) second) third
    | Phi (_, sources, _) ->
        sources |> List.fold (fun acc (op, _) -> fromOperand acc op) state
    | CliNative (_, _, args) -> fromOperands state args
    | RandomInt64 _
    | DateTimeNow _
    | StdinReadLine _
    | RuntimeError _
    | RuntimeErrorString _
    | CoverageHit _ -> state

/// Get all VRegs used by an instruction.
let getInstrUses (instr: Instr) : Set<VReg> =
    foldInstrUses (fun uses vreg -> Set.add vreg uses) Set.empty instr

/// Fold over the VRegs used by a terminator without allocating an intermediate collection.
let foldTerminatorUses (folder: 'State -> VReg -> 'State) (state: 'State) (term: Terminator) : 'State =
    let fromOperand state op =
        match op with
        | Register vreg -> folder state vreg
        | _ -> state

    match term with
    | Ret op -> fromOperand state op
    | Branch (cond, _, _) -> fromOperand state cond
    | Jump _ -> state

/// Get VRegs used by terminator
let getTerminatorUses (term: Terminator) : Set<VReg> =
    foldTerminatorUses (fun uses vreg -> Set.add vreg uses) Set.empty term

/// Get successors from a basic block terminator
let getSuccessors (block: BasicBlock) : Label list =
    match block.Terminator with
    | Ret _ -> []
    | Jump label -> [label]
    | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]

/// Build successor map for the CFG
let buildSuccessors (cfg: CFG) : Map<Label, Label list> =
    cfg.Blocks |> Map.map (fun _ block -> getSuccessors block)

/// Check whether the reachable CFG contains any cycle.
let cfgHasReachableCycle (cfg: CFG) : bool =
    let succs = buildSuccessors cfg

    let rec visit (visiting: Set<Label>) (visited: Set<Label>) (label: Label) : bool * Set<Label> =
        if Set.contains label visiting then
            (true, visited)
        elif Set.contains label visited then
            (false, visited)
        else
            let visiting' = Set.add label visiting
            let successors = Map.tryFind label succs |> Option.defaultValue []
            let rec visitSuccessors remaining visitedAcc =
                match remaining with
                | [] -> (false, visitedAcc)
                | next :: rest ->
                    let (hasCycle, visited') = visit visiting' visitedAcc next
                    if hasCycle then (true, visited') else visitSuccessors rest visited'

            let (hasCycle, visited') = visitSuccessors successors visited
            (hasCycle, Set.add label visited')

    let (hasCycle, _) = visit Set.empty Set.empty cfg.Entry
    hasCycle

/// Check if dominator dominates node (using idom chain)
let dominates (entry: Label) (idoms: Dominators) (dominator: Label) (node: Label) : bool =
    if dominator = node then
        true
    elif dominator = entry then
        node = entry || Map.containsKey node idoms
    else
        let rec walk current =
            match Map.tryFind current idoms with
            | None -> false
            | Some parent ->
                if parent = dominator then true
                elif parent = entry then false
                else walk parent
        walk node

/// Identify natural loops via backedges (header dominates source), reusing a
/// predecessor map already computed for this CFG topology.
let private findNaturalLoopsWithPredecessors
    (cfg: CFG)
    (predecessors: Map<Label, Label list>)
    : Map<Label, Set<Label>> =
    let idoms = computeDominators cfg predecessors
    let entry = cfg.Entry
    let successors = buildSuccessors cfg

    let backedges =
        successors
        |> Map.fold (fun acc from successorLabels ->
            successorLabels
            |> List.fold (fun acc' successor ->
                if dominates entry idoms successor from then
                    let existing =
                        Map.tryFind successor acc' |> Option.defaultValue []
                    Map.add successor (from :: existing) acc'
                else
                    acc') acc) Map.empty

    backedges
    |> Map.fold (fun loops header sources ->
        let loopBlocks =
            sources
            |> List.fold (fun acc source ->
                let initial = Set.ofList [header; source]
                let rec grow work loopSet =
                    match work with
                    | [] -> loopSet
                    | node :: rest ->
                        let nodePredecessors =
                            Map.tryFind node predecessors
                            |> Option.defaultValue []
                        let (loopSet', work') =
                            nodePredecessors
                            |> List.fold (fun (setAcc, workAcc) predecessor ->
                                if Set.contains predecessor setAcc then
                                    (setAcc, workAcc)
                                elif dominates entry idoms header predecessor then
                                    (Set.add predecessor setAcc, predecessor :: workAcc)
                                else
                                    (setAcc, workAcc)) (loopSet, rest)
                        grow work' loopSet'
                Set.union acc (grow [source] initial)) Set.empty

        if Set.isEmpty loopBlocks then loops
        else Map.add header loopBlocks loops) Map.empty

/// Immutable facts shared only while CFG blocks and edges are unchanged.
type private LoopTopology = {
    Loops: Map<Label, Set<Label>>
    Predecessors: Map<Label, Label list>
}

let private tryBuildLoopTopology (cfg: CFG) : LoopTopology option =
    if not (cfgHasReachableCycle cfg) then
        None
    else
        let predecessors = buildPredecessors cfg
        Some {
            Loops = findNaturalLoopsWithPredecessors cfg predecessors
            Predecessors = predecessors
        }

/// Identify natural loops via backedges (header dominates source).
let findNaturalLoops (cfg: CFG) : Map<Label, Set<Label>> =
    match tryBuildLoopTopology cfg with
    | None -> Map.empty
    | Some topology -> topology.Loops

type private AffineInductionCandidate = {
    Header: Label
    Preheader: Label
    Latch: Label
    InitialValue: Operand
    AffineValue: VReg
    ScaleInstr: Instr
    AffineInstr: Instr
}

let private nextRegisterId (cfg: CFG) : int =
    cfg.Blocks
    |> Map.fold (fun registers _ block ->
        let instrRegisters =
            block.Instrs
            |> List.fold (fun acc instr ->
                let acc' = Set.union acc (getInstrUses instr)
                match getInstrDest instr with
                | Some dest -> Set.add dest acc'
                | None -> acc'
            ) Set.empty
        Set.unionMany [registers; instrRegisters; getTerminatorUses block.Terminator]
    ) Set.empty
    |> Set.fold (fun highest (VReg id) -> max highest id) -1
    |> fun highest -> highest + 1

let private resolveLatchCopy (instrs: Instr list) (register: VReg) : VReg =
    let rec resolve visited current =
        if Set.contains current visited then
            current
        else
            let source =
                instrs
                |> List.tryPick (function
                    | Mov (dest, Register source, _) when dest = current -> Some source
                    | _ -> None)
            match source with
            | Some source -> resolve (Set.add current visited) source
            | None -> current
    resolve Set.empty register

let private isIncrementByOne (inductionPhi: VReg) (nextValue: VReg) (instr: Instr) : bool =
    match instr with
    | BinOp (dest, Add, Register source, Int64Const 1L, AST.TInt64)
    | BinOp (dest, Add, Int64Const 1L, Register source, AST.TInt64) ->
        dest = nextValue && source = inductionPhi
    | _ -> false

let private registerInstrUsers (cfg: CFG) (value: VReg) : (Label * Instr) list =
    cfg.Blocks
    |> Map.toList
    |> List.collect (fun (label, block) ->
        block.Instrs
        |> List.choose (fun instr ->
            if Set.contains value (getInstrUses instr) then Some (label, instr) else None))

let private terminatorUsesRegister (cfg: CFG) (value: VReg) : bool =
    cfg.Blocks
    |> Map.exists (fun _ block -> Set.contains value (getTerminatorUses block.Terminator))

let private tryAffineExpression
    (cfg: CFG)
    (latchLabel: Label)
    (latch: BasicBlock)
    (inductionPhi: VReg)
    : (VReg * Instr * Instr) option =
    let candidates =
        latch.Instrs
        |> List.collect (fun scaleInstr ->
            match scaleInstr with
            | BinOp (scaledValue, Shl, Register source, Int64Const 1L, AST.TInt64)
                when source = inductionPhi ->
                latch.Instrs
                |> List.choose (fun affineInstr ->
                    match affineInstr with
                    | BinOp (affineValue, Add, Register scaledSource, Int64Const 1L, AST.TInt64)
                    | BinOp (affineValue, Add, Int64Const 1L, Register scaledSource, AST.TInt64)
                        when scaledSource = scaledValue ->
                        Some (scaledValue, affineValue, scaleInstr, affineInstr)
                    | _ -> None)
            | _ -> [])

    match candidates with
    | [(scaledValue, affineValue, scaleInstr, affineInstr)] ->
        let scaledUsers = registerInstrUsers cfg scaledValue
        let affineUsers = registerInstrUsers cfg affineValue
        let affineUsesOnlyInLatch =
            not (List.isEmpty affineUsers)
            && affineUsers |> List.forall (fun (label, _) -> label = latchLabel)

        if scaledUsers = [(latchLabel, affineInstr)]
           && affineUsesOnlyInLatch
           && not (terminatorUsesRegister cfg scaledValue)
           && not (terminatorUsesRegister cfg affineValue) then
            Some (affineValue, scaleInstr, affineInstr)
        else
            None
    | _ -> None

let private tryAffineInductionCandidate
    (cfg: CFG)
    (predecessors: Map<Label, Label list>)
    (header: Label)
    (loopBlocks: Set<Label>)
    : AffineInductionCandidate option =
    let headerPredecessors = Map.tryFind header predecessors |> Option.defaultValue []
    let outsidePredecessors =
        headerPredecessors |> List.filter (fun label -> not (Set.contains label loopBlocks))
    let insidePredecessors =
        headerPredecessors |> List.filter (fun label -> Set.contains label loopBlocks)

    match outsidePredecessors, insidePredecessors, Map.tryFind header cfg.Blocks with
    | [preheader], [latchLabel], Some headerBlock
        when loopBlocks = Set.ofList [header; latchLabel] ->
        match Map.tryFind preheader cfg.Blocks, Map.tryFind latchLabel cfg.Blocks with
        | Some preheaderBlock, Some latch
            when preheaderBlock.Terminator = Jump header
                 && latch.Terminator = Jump header ->
            let candidates =
                headerBlock.Instrs
                |> List.choose (fun instr ->
                    match instr with
                    | Phi (inductionPhi, sources, Some AST.TInt64) ->
                        let initialSources =
                            sources |> List.filter (fun (_, source) -> source = preheader)
                        let backedgeSources =
                            sources |> List.filter (fun (_, source) -> source = latchLabel)
                        match sources, initialSources, backedgeSources with
                        | [ _; _ ], [(initialValue, _)], [(Register nextValue, _)] ->
                            let resolvedNext = resolveLatchCopy latch.Instrs nextValue
                            let advancesByOne =
                                latch.Instrs |> List.exists (isIncrementByOne inductionPhi resolvedNext)
                            match advancesByOne, tryAffineExpression cfg latchLabel latch inductionPhi with
                            | true, Some (affineValue, scaleInstr, affineInstr) ->
                                Some {
                                    Header = header
                                    Preheader = preheader
                                    Latch = latchLabel
                                    InitialValue = initialValue
                                    AffineValue = affineValue
                                    ScaleInstr = scaleInstr
                                    AffineInstr = affineInstr
                                }
                            | _ -> None
                        | _ -> None
                    | _ -> None)

            match candidates with
            | [candidate] -> Some candidate
            | _ -> None
        | _ -> None
    | _ -> None

let private addPhiAfterPhis (phi: Instr) (instrs: Instr list) : Instr list =
    let rec insert remaining =
        match remaining with
        | (Phi _ as existingPhi) :: rest -> existingPhi :: insert rest
        | rest -> phi :: rest
    insert instrs

let private insertAfterLastUse (value: VReg) (inserted: Instr list) (instrs: Instr list) : Instr list =
    let rec insert remaining =
        match remaining with
        | [] -> ([], false)
        | instr :: rest ->
            let (rest', alreadyInserted) = insert rest
            if alreadyInserted then
                (instr :: rest', true)
            elif Set.contains value (getInstrUses instr) then
                (instr :: inserted @ rest', true)
            else
                (instr :: rest', false)

    let (instrs', insertedAfterUse) = insert instrs
    if insertedAfterUse then
        instrs'
    else
        Crash.crash "insertAfterLastUse: affine induction value has no latch use"

(*
Plan: recognize only the canonical two-block Int64 loop produced for
`2 * i + 1` with an `i + 1` backedge, initialize the affine value in its unique
preheader, and carry it through a header phi advanced by two. Reject additional
affine expressions, extra uses of the scaled temporary, uses outside the latch,
and non-canonical control flow so the rewrite remains a local SSA substitution.
*)
let private applyAffineInductionStrengthReductionWithTopology
    (topology: LoopTopology)
    (cfg: CFG)
    : CFG * bool =
    let candidate =
        topology.Loops
        |> Map.toList
        |> List.tryPick (fun (header, loopBlocks) ->
            tryAffineInductionCandidate
                cfg
                topology.Predecessors
                header
                loopBlocks)

    match candidate with
    | None -> (cfg, false)
    | Some candidate ->
        let firstFreshRegister = nextRegisterId cfg
        let initialScaled = VReg firstFreshRegister
        let initialAffine = VReg (firstFreshRegister + 1)
        let nextAffine = VReg (firstFreshRegister + 2)
        let nextAffinePhiSource = VReg (firstFreshRegister + 3)
        let preheaderInstrs = [
            BinOp (
                initialScaled,
                Shl,
                candidate.InitialValue,
                Int64Const 1L,
                AST.TInt64
            )
            BinOp (
                initialAffine,
                Add,
                Register initialScaled,
                Int64Const 1L,
                AST.TInt64
            )
        ]
        let derivedPhi =
            Phi (
                candidate.AffineValue,
                [
                    (Register initialAffine, candidate.Preheader)
                    (Register nextAffinePhiSource, candidate.Latch)
                ],
                Some AST.TInt64
            )
        let advanceDerived =
            BinOp (
                nextAffine,
                Add,
                Register candidate.AffineValue,
                Int64Const 2L,
                AST.TInt64
            )
        let copyDerivedToPhiSource =
            Mov (nextAffinePhiSource, Register nextAffine, Some AST.TInt64)

        let blocks =
            cfg.Blocks
            |> Map.map (fun label block ->
                if label = candidate.Preheader then
                    { block with Instrs = block.Instrs @ preheaderInstrs }
                elif label = candidate.Header then
                    { block with Instrs = addPhiAfterPhis derivedPhi block.Instrs }
                elif label = candidate.Latch then
                    let instrs =
                        block.Instrs
                        |> List.filter (fun instr ->
                            instr <> candidate.ScaleInstr && instr <> candidate.AffineInstr)
                        |> insertAfterLastUse
                            candidate.AffineValue
                            [advanceDerived; copyDerivedToPhiSource]
                    { block with Instrs = instrs }
                else
                    block)
        ({ cfg with Blocks = blocks }, true)

let applyAffineInductionStrengthReduction (cfg: CFG) : CFG * bool =
    match tryBuildLoopTopology cfg with
    | None -> (cfg, false)
    | Some topology ->
        applyAffineInductionStrengthReductionWithTopology topology cfg

/// Scalar values can be duplicated or moved without changing ownership.
let private isScalarValueType (valueType: AST.Type) : bool =
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
                            Some (latchBlock.Instrs |> List.exists (isIncrementByOne induction resolvedNext))
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
let private applyCountedLoopUnrollingWithTopology
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

/// Scalar results can move across loop iterations without changing ownership.
let private isScalarReturnType (returnType: AST.Type) : bool =
    isScalarValueType returnType

/// Check if an instruction is safe to hoist out of a loop.
let private isHoistableInstrWithEffectFreeCalls
    (effectFreeFunctions: Set<string>)
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
    (effectFreeFunctions: Set<string>)
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
let private applyLoopInvariantCodeMotionWithEffectFreeCalls
    (effectFreeFunctions: Set<string>)
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

/// Build map from SSA destination to the registers used by its defining instruction.
let private buildDefUseMap (cfg: CFG) : Map<VReg, VReg list> =
    cfg.Blocks
    |> Map.fold (fun defUses _ block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match getInstrDest instr with
            | Some dest -> Map.add dest (foldInstrUses (fun uses vreg -> vreg :: uses) [] instr) acc
            | None -> acc
        ) defUses
    ) Map.empty

/// Collect registers that are directly required by side effects and control flow.
let private collectRootUses (cfg: CFG) : Set<VReg> =
    cfg.Blocks
    |> Map.fold (fun roots _ block ->
        let sideEffectUses =
            block.Instrs
            |> List.fold (fun acc instr ->
                if hasSideEffects instr then
                    foldInstrUses (fun uses vreg -> Set.add vreg uses) acc instr
                else
                    acc
            ) Set.empty
        let roots' = Set.union roots sideEffectUses
        foldTerminatorUses (fun uses vreg -> Set.add vreg uses) roots' block.Terminator
    ) Set.empty

/// Mark live SSA destinations by walking backwards from root uses.
let private collectLiveDestinations (cfg: CFG) : Set<VReg> =
    let defUseMap = buildDefUseMap cfg
    let roots = collectRootUses cfg

    let rec loop (work: VReg list) (queued: Set<VReg>) (live: Set<VReg>) : Set<VReg> =
        match work with
        | [] -> live
        | reg :: rest ->
            let queued' = Set.remove reg queued
            match Map.tryFind reg defUseMap with
            | None ->
                // Parameters or registers without a local definition.
                loop rest queued' live
            | Some uses when Set.contains reg live ->
                loop rest queued' live
            | Some uses ->
                let (rest', queued'') =
                    uses
                    |> List.fold (fun (pending, queuedAcc) usedReg ->
                        if Set.contains usedReg queuedAcc || Set.contains usedReg live then
                            (pending, queuedAcc)
                        else
                            (usedReg :: pending, Set.add usedReg queuedAcc)
                    ) (rest, queued')
                loop rest' queued'' (Set.add reg live)

    loop (Set.toList roots) roots Set.empty

/// Dead Code Elimination
/// Remove instructions whose destinations are never used (unless they have side effects)
let eliminateDeadCode (cfg: CFG) : CFG * bool =
    let liveDests = collectLiveDestinations cfg

    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let (instrs', instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', ch') instr ->
                    match getInstrDest instr with
                    | Some dest when not (Set.contains dest liveDests) && not (hasSideEffects instr) ->
                        // Dead instruction - remove it
                        (acc', true)
                    | _ ->
                        // Keep instruction
                        (instr :: acc', ch')
                ) ([], false)
            let instrs' = List.rev instrs'

            let block' = { block with Instrs = instrs' }
            (Map.add label block' acc, ch || instrChanged)
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Copy Propagation
/// Replace uses of copy destinations with their sources
/// For: dest = src, replace all uses of dest with src
type CopyMap = Map<VReg, Operand>

let buildCopyMap (cfg: CFG) : CopyMap =
    // First, collect all phi destinations - these should not be copy propagated
    let phiDests =
        cfg.Blocks
        |> Map.fold (fun dests _ block ->
            block.Instrs
            |> List.fold (fun d instr ->
                match instr with
                | Phi (dest, _, _) -> Set.add dest d
                | _ -> d
            ) dests
        ) Set.empty

    cfg.Blocks
    |> Map.fold (fun copies _ block ->
        block.Instrs
        |> List.fold (fun m instr ->
            match instr with
            | Mov (dest, Register src, vt) when dest <> src ->
                // Don't add if dest is a phi destination or already in map
                if Set.contains dest phiDests || Map.containsKey dest m then m
                else Map.add dest (Register src) m
            | Mov (dest, (Int64Const _ as src), vt) ->
                // Constant propagation: track constant moves too
                // Only propagate if this is for integer/bool types, not for heap types like strings
                // This prevents incorrectly propagating Int64Const 0L to string variables
                let isIntOrBoolType =
                    match vt with
                    | Some AST.TInt64 | Some AST.TInt32 | Some AST.TInt16 | Some AST.TInt8
                    | Some AST.TUInt64 | Some AST.TUInt32 | Some AST.TUInt16 | Some AST.TUInt8
                    | Some AST.TBool | Some AST.TUnit | None -> true
                    | _ -> false  // Don't propagate for TString, TList, etc.
                if Set.contains dest phiDests || Map.containsKey dest m || not isIntOrBoolType then m
                else Map.add dest src m
            | Mov (dest, (BoolConst _ as src), _) ->
                // Constant propagation: track constant moves too
                if Set.contains dest phiDests || Map.containsKey dest m then m
                else Map.add dest src m
            | Phi (dest, [(Register src, _)], _) when dest <> src ->
                // Trivial phi with single register source
                if Map.containsKey dest m then m
                else Map.add dest (Register src) m
            | Phi (dest, sources, vt) ->
                // Check if all sources are the same register
                if Map.containsKey dest m then m
                else
                    match sources with
                    | (Register firstSrc, _) :: rest ->
                        if rest |> List.forall (fun (s, _) -> s = Register firstSrc) then
                            if dest <> firstSrc then
                                Map.add dest (Register firstSrc) m
                            else
                                m
                        else
                            m
                    | _ -> m
            | _ -> m
        ) copies
    ) Map.empty

/// Transitively resolve a copy chain (with cycle detection)
let resolveCopy (copies: CopyMap) (op: Operand) : Operand =
    let rec resolve visited op' =
        match op' with
        | Register vreg ->
            if Set.contains vreg visited then
                // Cycle detected, stop here
                op'
            else
                match Map.tryFind vreg copies with
                | Some resolvedOp -> resolve (Set.add vreg visited) resolvedOp
                | None -> op'
        | _ -> op'
    resolve Set.empty op

/// Resolve every copy destination once so operand propagation only needs one lookup.
let resolveCopyMap (copies: CopyMap) : CopyMap =
    copies
    |> Map.map (fun dest _ -> resolveCopy copies (Register dest))

/// Apply copy propagation to an operand
let propagateCopyOperand (copies: CopyMap) (op: Operand) : Operand =
    match op with
    | Register vreg -> Map.tryFind vreg copies |> Option.defaultValue op
    | _ -> op

/// Apply copy propagation to an instruction
let propagateCopyInstr (copies: CopyMap) (instr: Instr) : Instr =
    let p = propagateCopyOperand copies
    match instr with
    | Mov (dest, src, vt) -> Mov (dest, p src, vt)
    | BinOp (dest, op, left, right, opType) -> BinOp (dest, op, p left, p right, opType)
    | UnaryOp (dest, op, src) -> UnaryOp (dest, op, p src)
    | Call (dest, name, args, argTypes, retType) -> Call (dest, name, List.map p args, argTypes, retType)
    | TailCall (name, args, argTypes, retType) -> TailCall (name, List.map p args, argTypes, retType)
    | IndirectCall (dest, func, args, argTypes, retType) -> IndirectCall (dest, p func, List.map p args, argTypes, retType)
    | IndirectTailCall (func, args, argTypes, retType) -> IndirectTailCall (p func, List.map p args, argTypes, retType)
    | ClosureAlloc (dest, name, captures) -> ClosureAlloc (dest, name, List.map p captures)
    | ClosureCall (dest, closure, args, argTypes, retType) -> ClosureCall (dest, p closure, List.map p args, argTypes, retType)
    | ClosureTailCall (closure, args, argTypes) -> ClosureTailCall (p closure, List.map p args, argTypes)
    | HeapAlloc (dest, size) -> HeapAlloc (dest, size)
    | HeapStore (addr, offset, src, vt) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        HeapStore (addr', offset, p src, vt)
    | HeapLoad (dest, addr, offset, vt) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        HeapLoad (dest, addr', offset, vt)
    | StringConcat (dest, left, right) -> StringConcat (dest, p left, p right)
    | RefCountInc (addr, size, kind, sourceType) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        RefCountInc (addr', size, kind, sourceType)
    | RefCountDec (addr, size, kind, sourceType) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        RefCountDec (addr', size, kind, sourceType)
    | Print (src, vt) -> Print (p src, vt)
    | StdoutWrite (effectId, src, appendNewline) -> StdoutWrite (effectId, p src, appendNewline)
    | StdinReadLine dest -> StdinReadLine dest
    | FileReadText (dest, path) -> FileReadText (dest, p path)
    | FileExists (dest, path) -> FileExists (dest, p path)
    | FileWriteText (dest, path, content) -> FileWriteText (dest, p path, p content)
    | FileAppendText (dest, path, content) -> FileAppendText (dest, p path, p content)
    | FileDelete (dest, path) -> FileDelete (dest, p path)
    | FileSetExecutable (dest, path) -> FileSetExecutable (dest, p path)
    | FileWriteFromPtr (dest, path, ptr, length) -> FileWriteFromPtr (dest, p path, p ptr, p length)
    // Don't propagate copies into phi sources - phis are merge points and their
    // sources represent values flowing from specific predecessor blocks
    | Phi (dest, sources, valueType) -> Phi (dest, sources, valueType)
    | RawAlloc (dest, numBytes) -> RawAlloc (dest, p numBytes)
    | RawFree ptr -> RawFree (p ptr)
    | RawGet (dest, ptr, byteOffset, valueType) -> RawGet (dest, p ptr, p byteOffset, valueType)
    | RawGetByte (dest, ptr, byteOffset) -> RawGetByte (dest, p ptr, p byteOffset)
    | StringToRawPtr (dest, value) -> StringToRawPtr (dest, p value)
    | RawPtrToString (dest, ptr) -> RawPtrToString (dest, p ptr)
    | BlobToRawPtr (dest, value) -> BlobToRawPtr (dest, p value)
    | RawPtrToBlob (dest, ptr) -> RawPtrToBlob (dest, p ptr)
    | DictToRawPtr (dest, dict) -> DictToRawPtr (dest, p dict)
    | RawPtrToDict (dest, ptr, tag) -> RawPtrToDict (dest, p ptr, p tag)
    | ListToRawPtr (dest, list) -> ListToRawPtr (dest, p list)
    | RawPtrToList (dest, ptr, tag) -> RawPtrToList (dest, p ptr, p tag)
    | RawWriteWord (ptr, byteOffset, value) -> RawWriteWord (p ptr, p byteOffset, p value)
    | RawWriteByte (ptr, byteOffset, value) -> RawWriteByte (p ptr, p byteOffset, p value)
    | RawSlotInit (ptr, byteOffset, value, valueType) -> RawSlotInit (p ptr, p byteOffset, p value, valueType)
    | FloatSqrt (dest, src) -> FloatSqrt (dest, p src)
    | FloatAbs (dest, src) -> FloatAbs (dest, p src)
    | FloatNeg (dest, src) -> FloatNeg (dest, p src)
    | Int64ToFloat (dest, src) -> Int64ToFloat (dest, p src)
    | FloatToInt64 (dest, src) -> FloatToInt64 (dest, p src)
    | FloatToBits (dest, src) -> FloatToBits (dest, p src)
    | RefCountIncString str -> RefCountIncString (p str)
    | RefCountDecString str -> RefCountDecString (p str)
    | RefCountIncBlob bytes -> RefCountIncBlob (p bytes)
    | RefCountDecBlob bytes -> RefCountDecBlob (p bytes)
    | RandomInt64 dest -> RandomInt64 dest
    | DateTimeNow dest -> DateTimeNow dest
    | Sleep (effectId, dest, delayMs) -> Sleep (effectId, dest, p delayMs)
    | CliNative (dest, operation, args) -> CliNative (dest, operation, List.map p args)
    | FloatToString (dest, value) -> FloatToString (dest, p value)
    | RuntimeError message -> RuntimeError message
    | RuntimeErrorString message -> RuntimeErrorString (p message)
    | CoverageHit exprId -> CoverageHit exprId

/// Apply copy propagation to terminator
let propagateCopyTerminator (copies: CopyMap) (term: Terminator) : Terminator =
    let p = propagateCopyOperand copies
    match term with
    | Ret op -> Ret (p op)
    | Branch (cond, trueLabel, falseLabel) -> Branch (p cond, trueLabel, falseLabel)
    | Jump label -> Jump label

/// Apply copy propagation to CFG
let applyCopyPropagation (cfg: CFG) : CFG * bool =
    let copies = buildCopyMap cfg |> resolveCopyMap

    if Map.isEmpty copies then
        (cfg, false)
    else
        let (blocks', changed) =
            cfg.Blocks
            |> Map.fold (fun (acc, changedAcc) label block ->
                let (instrs', instrChanged) =
                    block.Instrs
                    |> List.fold (fun (instrAcc, ch) instr ->
                        let instr' = propagateCopyInstr copies instr
                        (instr' :: instrAcc, ch || instr' <> instr)
                    ) ([], false)
                let instrs' = List.rev instrs'
                let term' = propagateCopyTerminator copies block.Terminator
                let block' = { block with Instrs = instrs'; Terminator = term' }
                (Map.add label block' acc, changedAcc || instrChanged || term' <> block.Terminator)
            ) (Map.empty, false)
        ({ cfg with Blocks = blocks' }, changed)

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
    // Find blocks that only contain a Jump
    let emptyBlocks =
        cfg.Blocks
        |> Map.filter (fun label block ->
            label <> cfg.Entry &&  // Don't remove entry block
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
let simplifyRetPhiJoins (cfg: CFG) : CFG * bool =
    let preds = buildPredecessors cfg

    let candidateMappings : Map<Label, Map<Label, Operand>> =
        cfg.Blocks
        |> Map.toList
        |> List.choose (fun (joinLabel, joinBlock) ->
            match joinBlock.Instrs, joinBlock.Terminator with
            | [Phi (phiDest, sources, _)], Ret (Register retReg) when phiDest = retReg ->
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

type private EstablishedBranchCondition =
    | EstablishedTrue of VReg
    | EstablishedFalse of VReg

let private establishedConditionOnEdge
    (successorLabel: Label)
    (predecessor: BasicBlock)
    : EstablishedBranchCondition option =
    match predecessor.Terminator with
    | Branch (Register condition, trueLabel, falseLabel)
        when trueLabel = successorLabel && falseLabel <> successorLabel ->
        Some (EstablishedTrue condition)
    | Branch (Register condition, trueLabel, falseLabel)
        when falseLabel = successorLabel && trueLabel <> successorLabel ->
        Some (EstablishedFalse condition)
    | _ -> None

/// Resolve a repeated SSA Boolean branch from the sole edge entering its block.
let simplifyBranchesKnownFromPredecessor (cfg: CFG) : CFG * bool =
    let predecessors = buildPredecessors cfg

    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, changedAcc) label block ->
            let term' =
                if label = cfg.Entry then
                    block.Terminator
                else
                    match Map.tryFind label predecessors, block.Terminator with
                    | Some [predecessorLabel], Branch (Register condition, trueLabel, falseLabel) ->
                        match Map.tryFind predecessorLabel cfg.Blocks with
                        | Some predecessor ->
                            match establishedConditionOnEdge label predecessor with
                            | Some (EstablishedTrue establishedCondition)
                                when establishedCondition = condition ->
                                Jump trueLabel
                            | Some (EstablishedFalse establishedCondition)
                                when establishedCondition = condition ->
                                Jump falseLabel
                            | _ -> block.Terminator
                        | None ->
                            Crash.crash $"Missing predecessor block {predecessorLabel} for {label}"
                    | _ -> block.Terminator

            let changed' = changedAcc || term' <> block.Terminator
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

/// Truncate a 64-bit value to the appropriate integer type width
/// This ensures proper overflow/wraparound behavior for smaller integer types
let truncateToType (value: int64) (opType: AST.Type) : int64 =
    match opType with
    | AST.TInt8 -> int64 (int8 value)      // Truncate to signed 8-bit
    | AST.TInt16 -> int64 (int16 value)    // Truncate to signed 16-bit
    | AST.TInt32 -> int64 (int32 value)    // Truncate to signed 32-bit
    | AST.TUInt8 -> int64 (uint8 value)    // Truncate to unsigned 8-bit
    | AST.TUInt16 -> int64 (uint16 value)  // Truncate to unsigned 16-bit
    | AST.TUInt32 -> int64 (uint32 value)  // Truncate to unsigned 32-bit
    | _ -> value                            // Int64/UInt64 and other types: no truncation

let truncateOperandToType (operand: Operand) (opType: AST.Type) : Operand =
    match operand with
    | Int64Const value -> Int64Const (truncateToType value opType)
    | _ -> operand

/// Euclidean modulo: result has the sign of the divisor
let euclideanMod (a: int64) (b: int64) : int64 =
    let remainder = a % b
    if remainder = 0L then 0L
    elif (remainder > 0L && b < 0L) || (remainder < 0L && b > 0L) then remainder + b
    else remainder

let isReflexiveEqualityType (opType: AST.Type) : bool =
    match opType with
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TInt128
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128
    | AST.TBool
    | AST.TChar
    | AST.TDateTime
    | AST.TUnit -> true
    | _ -> false

let isTotallyOrderedIntegerType (opType: AST.Type) : bool =
    match opType with
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TInt128
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128 -> true
    | _ -> false

let isUnsignedIntegerType (opType: AST.Type) : bool =
    match opType with
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64 -> true
    | _ -> false

/// Constant Folding for MIR
/// Evaluate operations on constants at compile time
let tryFoldBinOp (op: BinOp) (left: Operand) (right: Operand) (opType: AST.Type) : Operand option =
    match op, left, right with
    // Integer arithmetic - apply truncation for proper overflow behavior
    | Add, Int64Const a, Int64Const b -> Some (Int64Const (truncateToType (a + b) opType))
    | Sub, Int64Const a, Int64Const b -> Some (Int64Const (truncateToType (a - b) opType))
    | Mul, Int64Const a, Int64Const b -> Some (Int64Const (truncateToType (a * b) opType))
    // Division: avoid divide by zero and INT64_MIN / -1 overflow
    | Div, Int64Const a, Int64Const b when isUnsignedIntegerType opType && b <> 0L ->
        Some (Int64Const (int64 (uint64 a / uint64 b)))
    | Div, Int64Const a, Int64Const b when b <> 0L && not (a = System.Int64.MinValue && b = -1L) ->
        Some (Int64Const (truncateToType (a / b) opType))
    | Mod, Int64Const a, Int64Const b when isUnsignedIntegerType opType && b <> 0L ->
        Some (Int64Const (int64 (uint64 a % uint64 b)))
    | Mod, Int64Const a, Int64Const b when b > 0L -> Some (Int64Const (truncateToType (euclideanMod a b) opType))

    // Comparisons
    | Eq, Int64Const a, Int64Const b -> Some (BoolConst (a = b))
    | Neq, Int64Const a, Int64Const b -> Some (BoolConst (a <> b))
    | Lt, Int64Const a, Int64Const b when isUnsignedIntegerType opType -> Some (BoolConst (uint64 a < uint64 b))
    | Gt, Int64Const a, Int64Const b when isUnsignedIntegerType opType -> Some (BoolConst (uint64 a > uint64 b))
    | Lte, Int64Const a, Int64Const b when isUnsignedIntegerType opType -> Some (BoolConst (uint64 a <= uint64 b))
    | Gte, Int64Const a, Int64Const b when isUnsignedIntegerType opType -> Some (BoolConst (uint64 a >= uint64 b))
    | Lt, Int64Const a, Int64Const b -> Some (BoolConst (a < b))
    | Gt, Int64Const a, Int64Const b -> Some (BoolConst (a > b))
    | Lte, Int64Const a, Int64Const b -> Some (BoolConst (a <= b))
    | Gte, Int64Const a, Int64Const b -> Some (BoolConst (a >= b))
    | Eq, x, y when x = y && isReflexiveEqualityType opType -> Some (BoolConst true)
    | Neq, x, y when x = y && isReflexiveEqualityType opType -> Some (BoolConst false)
    | Lt, x, y when x = y && isTotallyOrderedIntegerType opType -> Some (BoolConst false)
    | Gt, x, y when x = y && isTotallyOrderedIntegerType opType -> Some (BoolConst false)
    | Lte, x, y when x = y && isTotallyOrderedIntegerType opType -> Some (BoolConst true)
    | Gte, x, y when x = y && isTotallyOrderedIntegerType opType -> Some (BoolConst true)

    // Boolean operations
    | And, BoolConst a, BoolConst b -> Some (BoolConst (a && b))
    | Or, BoolConst a, BoolConst b -> Some (BoolConst (a || b))

    // Algebraic identities
    | Add, Int64Const 0L, x -> Some x
    | Add, x, Int64Const 0L -> Some x
    | Sub, x, Int64Const 0L -> Some x
    | Sub, x, y when x = y -> Some (Int64Const 0L)  // x - x = 0
    | Mul, Int64Const 1L, x -> Some x
    | Mul, x, Int64Const 1L -> Some x
    | Mul, Int64Const 0L, _ -> Some (Int64Const 0L)
    | Mul, _, Int64Const 0L -> Some (Int64Const 0L)
    | Mul, Int64Const -1L, x -> None  // Could transform to Neg, but need instruction change
    | Mul, x, Int64Const -1L -> None  // Could transform to Neg
    | Div, x, Int64Const 1L -> Some x
    // Do not fold x / x: when x is zero, preserving the runtime operation matters.
    | Mod, _, Int64Const 1L -> Some (Int64Const 0L)  // x % 1 = 0

    // Bitwise identities
    | BitAnd, Int64Const 0L, _ -> Some (Int64Const 0L)
    | BitAnd, _, Int64Const 0L -> Some (Int64Const 0L)
    | BitAnd, Int64Const -1L, x -> Some (truncateOperandToType x opType)  // -1 = all bits set
    | BitAnd, x, Int64Const -1L -> Some (truncateOperandToType x opType)
    | BitAnd, x, y when x = y -> Some x  // x & x = x
    | BitOr, Int64Const 0L, x -> Some (truncateOperandToType x opType)
    | BitOr, x, Int64Const 0L -> Some (truncateOperandToType x opType)
    | BitOr, Int64Const -1L, _ -> Some (Int64Const (truncateToType -1L opType))
    | BitOr, _, Int64Const -1L -> Some (Int64Const (truncateToType -1L opType))
    | BitOr, x, y when x = y -> Some x  // x | x = x
    | BitXor, Int64Const 0L, x -> Some (truncateOperandToType x opType)
    | BitXor, x, Int64Const 0L -> Some (truncateOperandToType x opType)
    | BitXor, x, y when x = y -> Some (Int64Const 0L)  // x ^ x = 0

    // Shift identities
    | Shl, x, Int64Const 0L -> Some x  // x << 0 = x
    | Shr, x, Int64Const 0L -> Some x  // x >> 0 = x
    | Shl, Int64Const 0L, _ -> Some (Int64Const 0L)  // 0 << n = 0
    | Shr, Int64Const 0L, _ -> Some (Int64Const 0L)  // 0 >> n = 0

    // Boolean short-circuit
    | And, BoolConst false, _ -> Some (BoolConst false)
    | And, _, BoolConst false -> Some (BoolConst false)
    | And, BoolConst true, x -> Some x
    | And, x, BoolConst true -> Some x
    | Or, BoolConst true, _ -> Some (BoolConst true)
    | Or, _, BoolConst true -> Some (BoolConst true)
    | Or, BoolConst false, x -> Some x
    | Or, x, BoolConst false -> Some x

    | _ -> None

/// Common Subexpression Elimination (CSE)
/// Detect identical computations and replace with reference to first result

/// Expression key for CSE - represents a pure computation
type ExprKey =
    | BinExpr of BinOp * Operand * Operand * AST.Type
    | UnaryExpr of UnaryOp * Operand
    | ScalarHeapLoadExpr of VReg * int * AST.Type

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
let private clearScalarHeapLoadAvailability (available: Map<ExprKey, VReg>) : Map<ExprKey, VReg> =
    available
    |> Map.filter (fun key _ ->
        match key with
        | ScalarHeapLoadExpr _ -> false
        | _ -> true)

/// Apply CSE to a CFG, carrying available expressions into dominated blocks.
let applyCSE (cfg: CFG) : CFG * bool =
    let optimizeBlock (available: Map<ExprKey, VReg>) (block: BasicBlock) : BasicBlock * Map<ExprKey, VReg> * bool =
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
                    match Map.tryFind key available' with
                    | Some prevDest ->
                        (Mov (dest, Register prevDest, None) :: instrs, available', exported, true)
                    | None ->
                        let exported' =
                            if isCrossBlockCSEType opType then Map.add key dest exported else Map.empty
                        (instr :: instrs, Map.add key dest available', exported', ch)
                | UnaryOp (dest, op, src) ->
                    let key = makeUnaryExprKey op src
                    match Map.tryFind key exprMap with
                    | Some prevDest ->
                        (Mov (dest, Register prevDest, None) :: instrs, exprMap, exported, true)
                    | None ->
                        (instr :: instrs, Map.add key dest exprMap, Map.add key dest exported, ch)
                | HeapLoad (dest, addr, offset, Some valueType) when isCrossBlockCSEType valueType ->
                    let key = makeScalarHeapLoadExprKey addr offset valueType
                    match Map.tryFind key exprMap with
                    | Some prevDest ->
                        (Mov (dest, Register prevDest, Some valueType) :: instrs, exprMap, exported, true)
                    | None ->
                        (instr :: instrs, Map.add key dest exprMap, Map.add key dest exported, ch)
                | HeapLoad _ ->
                    // Unknown and non-scalar values can carry ownership edges;
                    // do not make earlier scalar loads available past them.
                    (instr :: instrs, clearScalarHeapLoadAvailability exprMap, Map.empty, ch)
                | RefCountDec _
                | RefCountDecString _
                | RefCountDecBlob _
                | RawFree _ ->
                    // A previously computed raw address can outlive its managed
                    // owner if reuse removes the later use that kept it alive.
                    (instr :: instrs, Map.empty, Map.empty, ch)
                | Mov (_, _, Some valueType) when not (isCrossBlockCSEType valueType) ->
                    (instr :: instrs, clearScalarHeapLoadAvailability exprMap, Map.empty, ch)
                | Phi (_, _, Some valueType) when not (isCrossBlockCSEType valueType) ->
                    (instr :: instrs, clearScalarHeapLoadAvailability exprMap, Map.empty, ch)
                | Mov _
                | Phi _ ->
                    (instr :: instrs, exprMap, exported, ch)
                | _ ->
                    // Do not extend a new cross-block live range across calls,
                    // allocations, memory operations, or other runtime lowering.
                    // Local CSE remains available through exprMap.
                    (instr :: instrs, clearScalarHeapLoadAvailability exprMap, Map.empty, ch)
            ) ([], available, available, false)

        ({ block with Instrs = List.rev instrs' }, exported', changed)

    let idoms = computeDominators cfg (buildPredecessors cfg)
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
        (available: Map<ExprKey, VReg>)
        (label: Label)
        (blocks: Map<Label, BasicBlock>, changed: bool)
        : Map<Label, BasicBlock> * bool =
        match Map.tryFind label cfg.Blocks with
        | None -> Crash.crash $"MIR CSE: missing dominator-tree block {label}"
        | Some block ->
            let (block', available', blockChanged) = optimizeBlock available block
            let state = (Map.add label block' blocks, changed || blockChanged)
            let children = Map.tryFind label dominatorChildren |> Option.defaultValue []
            children
            |> List.fold (fun childState child ->
                optimizeDominatorSubtree available' child childState
            ) state

    let (reachableBlocks, reachableChanged) =
        optimizeDominatorSubtree Map.empty cfg.Entry (Map.empty, false)

    // Dominators are undefined for unreachable blocks. Retain local CSE there so
    // this transformation remains complete when invoked independently.
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (blocks, ch) label block ->
            if Map.containsKey label blocks then
                (blocks, ch)
            else
                let (block', _, blockChanged) = optimizeBlock Map.empty block
                (Map.add label block' blocks, ch || blockChanged)
        ) (reachableBlocks, reachableChanged)

    ({ cfg with Blocks = blocks' }, changed)

/// Try to fold a unary operation on a constant
let tryFoldUnaryOp (op: UnaryOp) (src: Operand) : Operand option =
    match op, src with
    | Neg, Int64Const n -> Some (Int64Const (-n))
    | Not, BoolConst b -> Some (BoolConst (not b))
    | _ -> None

/// Apply constant folding to a CFG
let applyConstantFolding (cfg: CFG) : CFG * bool =
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let (instrs', instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', ch') instr ->
                    match instr with
                    | BinOp (dest, op, left, right, opType) ->
                        match tryFoldBinOp op left right opType with
                        | Some result ->
                            (Mov (dest, result, None) :: acc', true)
                        | None ->
                            (instr :: acc', ch')
                    | UnaryOp (dest, op, src) ->
                        match tryFoldUnaryOp op src with
                        | Some result ->
                            (Mov (dest, result, None) :: acc', true)
                        | None ->
                            (instr :: acc', ch')
                    | _ ->
                        (instr :: acc', ch')
                ) ([], false)
            let instrs' = List.rev instrs'

            let block' = { block with Instrs = instrs' }
            (Map.add label block' acc, ch || instrChanged)
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Run all optimizations in a single pass (returns whether anything changed).
let private optimizeCFGOnceWithEffectFreeCalls
    (effectFreeFunctions: Set<string>)
    (options: OptimizeOptions)
    (cfg: CFG)
    : CFG * bool =
    let (cfg1, changed1) =
        if options.EnableConstFolding then applyConstantFolding cfg else (cfg, false)
    let (cfg2, changed2) =
        if options.EnableCSE then applyCSE cfg1 else (cfg1, false)
    let (cfg3, changed3) =
        if options.EnableCopyProp then applyCopyPropagation cfg2 else (cfg2, false)
    // Run constant folding again only when copy propagation changed the CFG.
    // This catches cases like: v1 = -127; v2 = v1 - 2
    // After copy prop: v2 = Int64Const(-127) - Int64Const(2) -> can fold
    let (cfg4, changed4) =
        if options.EnableConstFolding && changed3 then applyConstantFolding cfg3 else (cfg3, false)
    let (cfg5, changed5, cfg6, changed6, loopTopology) =
        if options.EnableLICM then
            match tryBuildLoopTopology cfg4 with
            | None -> (cfg4, false, cfg4, false, None)
            | Some topology ->
                let (cfg5, changed5) =
                    applyAffineInductionStrengthReductionWithTopology
                        topology
                        cfg4
                let (cfg6, changed6, topologyAfterLicm) =
                    applyLoopInvariantCodeMotionWithEffectFreeCalls
                        effectFreeFunctions
                        topology
                        cfg5
                (cfg5, changed5, cfg6, changed6, Some topologyAfterLicm)
        else
            (cfg4, false, cfg4, false, None)
    let (cfg7, changed7) =
        match loopTopology with
        | Some topology -> applyCountedLoopUnrollingWithTopology topology cfg6
        | None -> (cfg6, false)
    let (cfg8, changed8) =
        if options.EnableDCE then eliminateDeadCode cfg7 else (cfg7, false)
    let (cfg9, changed9) =
        if options.EnableCFGSimplify then simplifyConstantBranches cfg8 else (cfg8, false)
    let (cfg10, changed10) =
        if options.EnableCFGSimplify then simplifyBranchesKnownFromPredecessor cfg9 else (cfg9, false)
    let (cfg11, changed11) =
        if options.EnableCFGSimplify then eliminateUnreachableBlocks cfg10 else (cfg10, false)
    let (cfg12, changed12) =
        if options.EnableCFGSimplify then simplifyRetPhiJoins cfg11 else (cfg11, false)
    let (cfg13, changed13) =
        if options.EnableCFGSimplify then simplifyEmptyBlocks cfg12 else (cfg12, false)
    let (cfg14, changed14) =
        if options.EnableCFGSimplify then mergeLinearBlocks cfg13 else (cfg13, false)
    let changed = changed1 || changed2 || changed3 || changed4 || changed5 || changed6 || changed7 || changed8 || changed9 || changed10 || changed11 || changed12 || changed13 || changed14
    (cfg14, changed)

let optimizeCFGOnce (options: OptimizeOptions) (cfg: CFG) : CFG * bool =
    optimizeCFGOnceWithEffectFreeCalls Set.empty options cfg

/// Run all optimizations until fixed point
let private optimizeCFGWithEffectFreeCalls
    (effectFreeFunctions: Set<string>)
    (options: OptimizeOptions)
    (cfg: CFG)
    : CFG =
    let rec loop current remaining =
        if remaining <= 0 then
            current
        else
            let (next, changed) =
                optimizeCFGOnceWithEffectFreeCalls effectFreeFunctions options current
            if changed then
                loop next (remaining - 1)
            else
                next
    loop cfg 10

let optimizeCFGWithOptions (options: OptimizeOptions) (cfg: CFG) : CFG =
    optimizeCFGWithEffectFreeCalls Set.empty options cfg

let optimizeCFG (cfg: CFG) : CFG =
    optimizeCFGWithOptions defaultOptimizeOptions cfg

let private explicitFloatRegisters (cfg: CFG) : Set<int> =
    cfg.Blocks
    |> Map.fold (fun registers _ block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            let floatDestination =
                match instr with
                | Mov (destination, _, Some AST.TFloat64)
                | BinOp (destination, _, _, _, AST.TFloat64)
                | Phi (destination, _, Some AST.TFloat64)
                | FloatSqrt (destination, _)
                | FloatAbs (destination, _)
                | FloatNeg (destination, _)
                | Int64ToFloat (destination, _) -> Some destination
                | _ -> None
            match floatDestination with
            | Some (VReg id) -> Set.add id acc
            | None -> acc
        ) registers
    ) Set.empty

let private withOptimizedCFG (func: Function) (cfg: CFG) : Function =
    {
        func with
            CFG = cfg
            FloatRegs = Set.union func.FloatRegs (explicitFloatRegisters cfg)
    }

/// Optimize a function
let optimizeFunctionWithOptions (options: OptimizeOptions) (func: Function) : Function =
    let cfg' = optimizeCFGWithOptions options func.CFG
    withOptimizedCFG func cfg'

let private optimizeFunctionWithEffectFreeCalls
    (effectFreeFunctions: Set<string>)
    (options: OptimizeOptions)
    (func: Function)
    : Function =
    let cfg' = optimizeCFGWithEffectFreeCalls effectFreeFunctions options func.CFG
    withOptimizedCFG func cfg'

let optimizeFunction (func: Function) : Function =
    let cfg' = optimizeCFG func.CFG
    withOptimizedCFG func cfg'

/// Optimize a program
let optimizeProgramWithOptions (options: OptimizeOptions) (program: Program) : Program =
    let (Program (functions, variants, records)) = program
    let effectFreeFunctions =
        if options.EnableLICM then analyzeEffectFreeFunctions functions else Set.empty
    let functions' =
        functions
        |> List.map (optimizeFunctionWithEffectFreeCalls effectFreeFunctions options)
    Program (functions', variants, records)

let optimizeProgram (program: Program) : Program =
    optimizeProgramWithOptions defaultOptimizeOptions program

let optimizeConstFolding (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = true
            EnableCSE = false
            EnableCopyProp = false
            EnableDCE = false
            EnableCFGSimplify = false
            EnableLICM = false }
        program

let optimizeCopyProp (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableCSE = false
            EnableCopyProp = true
            EnableDCE = false
            EnableCFGSimplify = false
            EnableLICM = false }
        program

let optimizeDCE (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableCSE = false
            EnableCopyProp = false
            EnableDCE = true
            EnableCFGSimplify = false
            EnableLICM = false }
        program
