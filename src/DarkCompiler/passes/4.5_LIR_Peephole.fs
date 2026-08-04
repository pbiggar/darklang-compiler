// 4.5_LIR_Peephole.fs - LIR Peephole Optimizations
//
// Performs low-level optimizations on LIR:
// - Remove identity operations (add x, y, 0 → mov x, y)
// - Remove self-moves (mov x, x → remove)
// - Constant multiplication optimizations (mul x, y, 0 → mov x, 0)
// - Dead move elimination
//
// These optimizations work on individual instructions or small sequences.

module LIR_Peephole

open LIR

type private DomBitSet = Bitset.Bitset

type private Dominators = {
    IndexOf: Map<Label, int>
    Sets: DomBitSet array
}

type private DominatorCache = {
    Succs: Map<Label, Label list>
    Dominators: Dominators
}

let private dominatorSetsEqual (left: DomBitSet array) (right: DomBitSet array) : bool =
    left.Length = right.Length
    && Array.forall2 (fun leftSet rightSet -> Bitset.equal leftSet rightSet) left right

let private labelName (Label name) : string = name

/// Check if two registers are the same
let sameReg (r1: Reg) (r2: Reg) : bool =
    match r1, r2 with
    | LIR.Physical p1, LIR.Physical p2 -> p1 = p2
    | LIR.Virtual v1, LIR.Virtual v2 -> v1 = v2
    | LIR.Physical _, LIR.Virtual _
    | LIR.Virtual _, LIR.Physical _ -> false

let sameFReg (r1: FReg) (r2: FReg) : bool =
    match r1, r2 with
    | LIR.FPhysical p1, LIR.FPhysical p2 -> p1 = p2
    | LIR.FVirtual v1, LIR.FVirtual v2 -> v1 = v2
    | LIR.FPhysical _, LIR.FVirtual _
    | LIR.FVirtual _, LIR.FPhysical _ -> false

/// Get successor labels from a terminator
let getSuccessors (term: Terminator) : Label list =
    match term with
    | Ret -> []
    | Jump label -> [label]
    | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]
    | BranchZero (_, zeroLabel, nonZeroLabel) -> [zeroLabel; nonZeroLabel]
    | BranchBitZero (_, _, zeroLabel, nonZeroLabel) -> [zeroLabel; nonZeroLabel]
    | BranchBitNonZero (_, _, nonZeroLabel, zeroLabel) -> [nonZeroLabel; zeroLabel]
    | CondBranch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]

/// Build predecessor map for the CFG
let buildPredecessors (cfg: CFG) : Map<Label, Label list> =
    let emptyPreds =
        cfg.Blocks
        |> Map.toList
        |> List.map (fun (label, _) -> (label, []))
        |> Map.ofList

    cfg.Blocks
    |> Map.fold (fun preds label block ->
        getSuccessors block.Terminator
        |> List.fold (fun acc succ ->
            let existing = Map.tryFind succ acc |> Option.defaultValue []
            Map.add succ (label :: existing) acc
        ) preds
    ) emptyPreds

/// Build successor map for the CFG
let buildSuccessors (cfg: CFG) : Map<Label, Label list> =
    cfg.Blocks |> Map.map (fun _ block -> getSuccessors block.Terminator)

let private validateCFGShape (cfg: CFG) : unit =
    if not (Map.containsKey cfg.Entry cfg.Blocks) then
        Crash.crash $"LIR Peephole: entry label {labelName cfg.Entry} not found in CFG blocks"
    else
        cfg.Blocks
        |> Map.iter (fun label block ->
            getSuccessors block.Terminator
            |> List.iter (fun succ ->
                if not (Map.containsKey succ cfg.Blocks) then
                    Crash.crash $"LIR Peephole: block {labelName label} has missing successor label {labelName succ}"))

/// Compute dominator sets for each block
let private computeDominators (cfg: CFG) (preds: Map<Label, Label list>) : Dominators =
    let labels =
        cfg.Blocks
        |> Map.toList
        |> List.map fst
        |> Array.ofList
    let indexOf =
        labels
        |> Array.mapi (fun idx label -> (label, idx))
        |> Array.toList
        |> Map.ofList
    let entryIndex =
        match Map.tryFind cfg.Entry indexOf with
        | Some idx -> idx
        | None -> Crash.crash $"LIR Peephole: entry label {cfg.Entry} not found in CFG blocks"
    let labelCount = labels.Length
    let wordCount = Bitset.wordCount labelCount
    let allBits = Bitset.all labelCount
    let entryBits = Bitset.singleton wordCount entryIndex
    let predIndices =
        labels
        |> Array.map (fun label ->
            Map.tryFind label preds
            |> Option.defaultValue []
            |> List.choose (fun pred -> Map.tryFind pred indexOf))

    let initial =
        Array.init labelCount (fun idx ->
            if idx = entryIndex then entryBits else allBits)

    let rec loop (doms: DomBitSet array) : DomBitSet array =
        let updated =
            Array.init labelCount (fun idx ->
                if idx = entryIndex then
                    entryBits
                else
                    let predSets =
                        predIndices[idx]
                        |> List.choose (fun predIdx -> Array.tryItem predIdx doms)
                    match predSets with
                    | [] -> Bitset.singleton wordCount idx
                    | first :: rest ->
                        let intersected = Bitset.intersectMany first rest
                        Bitset.add idx intersected)
        if dominatorSetsEqual doms updated then updated else loop updated

    { IndexOf = indexOf; Sets = loop initial }

/// Identify natural loops via backedges (header dominates source)
let private findNaturalLoopsWithCache
    (cfg: CFG)
    (domCache: DominatorCache option)
    : Map<Label, Set<Label>> * DominatorCache option =
    let preds = buildPredecessors cfg
    let succs = buildSuccessors cfg
    let (doms, cache') =
        match domCache with
        | Some cache when cache.Succs = succs ->
            (cache.Dominators, domCache)
        | _ ->
            let doms = computeDominators cfg preds
            (doms, Some { Succs = succs; Dominators = doms })

    let dominates (dominator: Label) (node: Label) : bool =
        match Map.tryFind dominator doms.IndexOf, Map.tryFind node doms.IndexOf with
        | Some domIdx, Some nodeIdx ->
            match Array.tryItem nodeIdx doms.Sets with
            | Some set -> Bitset.containsIndex domIdx set
            | None -> false
        | _ -> false

    let backedges =
        succs
        |> Map.fold (fun acc from successors ->
            successors
            |> List.fold (fun acc' succ ->
                if dominates succ from then
                    let existing = Map.tryFind succ acc' |> Option.defaultValue []
                    Map.add succ (from :: existing) acc'
                else
                    acc'
            ) acc
        ) Map.empty

    let loops =
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
                            let nodePreds = Map.tryFind node preds |> Option.defaultValue []
                            let (loopSet', work') =
                                nodePreds
                                |> List.fold (fun (setAcc, workAcc) pred ->
                                    if Set.contains pred setAcc then
                                        (setAcc, workAcc)
                                    elif dominates header pred then
                                        (Set.add pred setAcc, pred :: workAcc)
                                    else
                                        (setAcc, workAcc)
                                ) (loopSet, rest)
                            grow work' loopSet'
                    Set.union acc (grow [source] initial)
                ) Set.empty
            if Set.isEmpty loopBlocks then loops else Map.add header loopBlocks loops
        ) Map.empty

    (loops, cache')

/// Check whether the CFG has any directed cycle.
let private hasCycle (cfg: CFG) : bool =
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
                | [] -> (false, Set.add label visitedAcc)
                | succ :: rest ->
                    let (foundCycle, visited') = visit visiting' visitedAcc succ
                    if foundCycle then (true, visited') else visitSuccessors rest visited'
            visitSuccessors successors visited

    let labels = cfg.Blocks |> Map.toList |> List.map fst
    let rec visitAll remaining visited =
        match remaining with
        | [] -> false
        | label :: rest ->
            if Set.contains label visited then
                visitAll rest visited
            else
                let (foundCycle, visited') = visit Set.empty visited label
                foundCycle || visitAll rest visited'
    visitAll labels Set.empty

/// A virtual destination whose constant definition can move outside a loop.
type private HoistableConstDest =
    | IntVirtualDest of int
    | FloatVirtualDest of int

/// Check whether an instruction is a hoistable constant definition.
let private isHoistableConstInstr (instr: Instr) : HoistableConstDest option =
    match instr with
    | Mov (LIR.Virtual id, Imm _) -> Some (IntVirtualDest id)
    | FLoad (LIR.FVirtual id, _) -> Some (FloatVirtualDest id)
    | _ -> None

/// Check whether an instruction represents a call (affects register saving)
let isCallInstr (instr: Instr) : bool =
    match instr with
    | Call _
    | TailCall _
    | IndirectCall _
    | IndirectTailCall _
    | ClosureCall _
    | ClosureTailCall _ -> true
    | _ -> false

/// Check whether an instruction is pure arithmetic/logic for LICM safety
let isPureLoopInstr (instr: Instr) : bool =
    match instr with
    | Mov _
    | Phi _
    | FPhi _
    | Add _
    | Sub _
    | Mul _
    | Sdiv _
    | Msub _
    | Madd _
    | Cmp _
    | Cset _
    | And _
    | And_imm _
    | Orr _
    | Eor _
    | Lsl _
    | Lsr _
    | Lsl_imm _
    | Lsr_imm _
    | Mvn _
    | Sxtb _
    | Sxth _
    | Sxtw _
    | Uxtb _
    | Uxth _
    | Uxtw _
    | FMov _
    | FLoad _
    | FAdd _
    | FSub _
    | FMul _
    | FDiv _
    | FNeg _
    | FAbs _
    | FSqrt _
    | FCmp _
    | Int64ToFloat _
    | FloatToInt64 _
    | GpToFp _
    | FpToGp _ -> true
    | _ -> false

/// Hoist loop-invariant integer and float constants into simple preheaders.
let private applyLoopInvariantConstHoist
    (cfg: CFG)
    (domCache: DominatorCache option)
    : CFG * bool * DominatorCache option =
    if not (hasCycle cfg) then
        (cfg, false, domCache)
    else
    let (loops, cache') = findNaturalLoopsWithCache cfg domCache
    let preds = buildPredecessors cfg

    let result =
        loops
        |> Map.fold (fun (cfgAcc, changedAcc) header loopBlocks ->
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
            let loopHasCall =
                loopBlocks
                |> Set.exists (fun label ->
                    match Map.tryFind label cfgAcc.Blocks with
                    | None -> false
                    | Some block -> block.Instrs |> List.exists isCallInstr
                )

            let loopIsPure =
                loopBlocks
                |> Set.forall (fun label ->
                    match Map.tryFind label cfgAcc.Blocks with
                    | None -> true
                    | Some block -> block.Instrs |> List.forall isPureLoopInstr
                )

            if loopHasCall || not loopIsPure then
                (cfgAcc, changedAcc)
            else
            let blockOrder =
                header :: (loopBlocks |> Set.remove header |> Set.toList |> List.sortBy labelName)

            let (hoistedRev, hoistedDests) =
                blockOrder
                |> List.fold (fun (instrs, dests) label ->
                    match Map.tryFind label cfgAcc.Blocks with
                    | None -> (instrs, dests)
                    | Some block ->
                        block.Instrs
                        |> List.fold (fun (instrsAcc, destsAcc) instr ->
                            match isHoistableConstInstr instr with
                            | Some dest when not (Set.contains dest destsAcc) ->
                                (instr :: instrsAcc, Set.add dest destsAcc)
                            | _ -> (instrsAcc, destsAcc)
                        ) (instrs, dests)
                ) ([], Set.empty)

            let hoistedInstrs = List.rev hoistedRev
            if List.isEmpty hoistedInstrs then
                (cfgAcc, changedAcc)
            else
                let blocks' =
                    cfgAcc.Blocks
                    |> Map.map (fun label block ->
                        if label = preheader then
                            { block with Instrs = block.Instrs @ hoistedInstrs }
                        elif Set.contains label loopBlocks then
                            let instrs' =
                                block.Instrs
                                |> List.filter (fun instr ->
                                    match isHoistableConstInstr instr with
                                    | Some dest -> not (Set.contains dest hoistedDests)
                                    | None -> true
                                )
                            { block with Instrs = instrs' }
                        else
                            block
                    )
                ({ cfgAcc with Blocks = blocks' }, true)
        ) (cfg, false)
    let (cfg', changed) = result
    (cfg', changed, cache')

/// Optimize a single instruction (returns None to remove, Some to replace)
let optimizeInstr (instr: Instr) : Instr option =
    match instr with
    // Remove self-moves: mov x, x → remove
    | Mov (dest, Reg src) when sameReg dest src ->
        None
    | FMov (dest, src) when sameFReg dest src ->
        None

    // Add with zero: add x, y, 0 → mov x, y (if x != y) or remove (if x == y)
    | Add (dest, left, Imm 0L) ->
        if sameReg dest left then None
        else Some (Mov (dest, Reg left))

    // Add with zero on left: we don't have this form in LIR

    // Sub with zero: sub x, y, 0 → mov x, y or remove
    | Sub (dest, left, Imm 0L) ->
        if sameReg dest left then None
        else Some (Mov (dest, Reg left))

    // Multiply by zero: mul x, y, z where z is zero → mov x, 0
    // This requires both operands to be registers in LIR, so we can't detect 0

    // Multiply by one: would require one operand to be immediate, but Mul takes two regs

    // For now, keep the instruction as-is
    | _ -> Some instr

let private fRegUsedInInstr (target: FReg) (instr: Instr) : bool =
    let same = sameFReg target
    match instr with
    | FArgMoves moves ->
        moves |> List.exists (fun (_, src) -> same src)
    | PrintFloat src
    | PrintFloatNoNewline src
    | FMov (_, src)
    | FNeg (_, src)
    | FAbs (_, src)
    | FSqrt (_, src)
    | FloatToInt64 (_, src)
    | FloatToBits (_, src)
    | FpToGp (_, src)
    | FloatToString (_, src) ->
        same src
    | FAdd (_, left, right)
    | FSub (_, left, right)
    | FMul (_, left, right)
    | FDiv (_, left, right)
    | FCmp (left, right) ->
        same left || same right
    | FPhi (_, sources) ->
        sources |> List.exists (fun (src, _) -> same src)
    | _ -> false

let private fRegUsedInInstrs (target: FReg) (instrs: Instr list) : bool =
    instrs |> List.exists (fRegUsedInInstr target)

let private tryFoldFNegIntoMove (instr: Instr) (next: Instr) (rest: Instr list) : Instr option =
    match instr, next with
    | FNeg (temp, src), FMov (dest, moveSrc)
        when sameFReg temp moveSrc && not (fRegUsedInInstrs temp rest) ->
        Some (FNeg (dest, src))
    | _ -> None

/// Optimize a list of instructions (single-pass peephole)
let optimizeInstrs (instrs: Instr list) : Instr list =
    let rec loop remaining =
        match remaining with
        | instr :: next :: rest ->
            match tryFoldFNegIntoMove instr next rest with
            | Some folded -> folded :: loop rest
            | None ->
                match optimizeInstr instr with
                | Some instr' -> instr' :: loop (next :: rest)
                | None -> loop (next :: rest)
        | instr :: rest ->
            match optimizeInstr instr with
            | Some instr' -> instr' :: loop rest
            | None -> loop rest
        | [] -> []

    loop instrs

let removeSelfMovesFromInstrs (instrs: Instr list) : Instr list =
    let rec loop remaining =
        match remaining with
        | instr :: next :: rest ->
            match tryFoldFNegIntoMove instr next rest with
            | Some folded -> folded :: loop rest
            | None ->
                match instr with
                | Mov (dest, Reg src) when sameReg dest src -> loop (next :: rest)
                | FMov (dest, src) when sameFReg dest src -> loop (next :: rest)
                | _ -> instr :: loop (next :: rest)
        | instr :: rest ->
            match instr with
            | Mov (dest, Reg src) when sameReg dest src -> loop rest
            | FMov (dest, src) when sameFReg dest src -> loop rest
            | _ -> instr :: loop rest
        | [] -> []

    loop instrs

let removeSelfMovesFromFunction (func: Function) : Function =
    let blocks =
        func.CFG.Blocks
        |> Map.map (fun _ block ->
            { block with Instrs = removeSelfMovesFromInstrs block.Instrs })
    { func with CFG = { func.CFG with Blocks = blocks } }

type private FloatingValueIdentity =
    | InitialFRegValue of FReg
    | WrittenFRegValue of int

let private currentFRegValue
    (reg: FReg)
    (aliases: Map<FReg, FloatingValueIdentity>)
    : FloatingValueIdentity =
    Map.tryFind reg aliases |> Option.defaultValue (InitialFRegValue reg)

let private recordFRegWrite
    (dest: FReg)
    (valueIdentity: int)
    (aliases: Map<FReg, FloatingValueIdentity>)
    : Map<FReg, FloatingValueIdentity> =
    Map.add dest (WrittenFRegValue valueIdentity) aliases

let private fRegWriteDest (instr: Instr) : FReg option =
    match instr with
    | FPhi (dest, _)
    | FLoad (dest, _)
    | FAdd (dest, _, _)
    | FSub (dest, _, _)
    | FMul (dest, _, _)
    | FDiv (dest, _, _)
    | FNeg (dest, _)
    | FAbs (dest, _)
    | FSqrt (dest, _)
    | Int64ToFloat (dest, _)
    | GpToFp (dest, _) -> Some dest
    | _ -> None

let private clobbersFRegs (instr: Instr) : bool =
    match instr with
    | Call _
    | TailCall _
    | IndirectCall _
    | IndirectTailCall _
    | ClosureCall _
    | ClosureTailCall _
    | RestoreRegs _
    | FArgMoves _ -> true
    | _ -> false

let removeRedundantFloatingCopyBackMoves (instrs: Instr list) : Instr list =
    let rec loop aliases nextValueIdentity acc remaining =
        match remaining with
        | [] -> List.rev acc
        | FMov (dest, src) as instr :: rest ->
            let srcValue = currentFRegValue src aliases
            let destValue = currentFRegValue dest aliases
            let aliases' = Map.add dest srcValue aliases
            let acc' = if destValue = srcValue then acc else instr :: acc
            loop aliases' nextValueIdentity acc' rest
        | instr :: rest ->
            let (aliases', nextValueIdentity') =
                if clobbersFRegs instr then
                    (Map.empty, nextValueIdentity)
                else
                    match fRegWriteDest instr with
                    | Some dest ->
                        (recordFRegWrite dest nextValueIdentity aliases, nextValueIdentity + 1)
                    | None -> (aliases, nextValueIdentity)
            loop aliases' nextValueIdentity' (instr :: acc) rest

    loop Map.empty 0 [] instrs

let removePostAllocationMovesFromFunction (func: Function) : Function =
    let blocks =
        func.CFG.Blocks
        |> Map.map (fun _ block ->
            { block with
                Instrs =
                    block.Instrs
                    |> removeSelfMovesFromInstrs
                    |> removeRedundantFloatingCopyBackMoves })
    { func with CFG = { func.CFG with Blocks = blocks } }

let private foldOperandRegUse folder state (operand: Operand) =
    match operand with
    | Reg reg -> folder state reg
    | Imm _
    | FloatImm _
    | StackSlot _
    | StringSymbol _
    | FloatSymbol _
    | FuncAddr _ -> state

/// Fold over the integer registers read by an instruction.
let private foldRegUses folder state (instr: Instr) =
    match instr with
    | Mov (_, src)
    | RefCountIncString src
    | RefCountDecString src
    | RefCountIncBytes src
    | RefCountDecBytes src ->
        foldOperandRegUse folder state src
    | Phi (_, sources, _) ->
        sources |> List.fold (fun acc (src, _) -> foldOperandRegUse folder acc src) state
    | Store (_, src)
    | PrintInt64 src
    | PrintUInt64 src
    | PrintBool src
    | PrintInt64NoNewline src
    | PrintUInt64NoNewline src
    | PrintBoolNoNewline src
    | PrintHeapStringNoNewline src
    | PrintBytes src
    | PrintList (src, _)
    | PrintSum (src, _)
    | PrintRecord (src, _, _)
    | Int64ToFloat (_, src)
    | GpToFp (_, src)
    | RawFree src
    | FloatToString (src, _) ->
        folder state src
    | Add (_, left, right)
    | Sub (_, left, right)
    | Cmp (left, right) ->
        foldOperandRegUse folder (folder state left) right
    | Mul (_, left, right)
    | Sdiv (_, left, right)
    | Udiv (_, left, right)
    | And (_, left, right)
    | Orr (_, left, right)
    | Eor (_, left, right)
    | Lsl (_, left, right)
    | Lsr (_, left, right)
    | RawGet (_, left, right)
    | RawGetByte (_, left, right) ->
        folder (folder state left) right
    | RawAlloc (_, numBytes) ->
        folder state numBytes
    | FileWriteFromPtr (_, path, ptr, length) ->
        foldOperandRegUse folder state path |> fun acc -> folder (folder acc ptr) length
    | Msub (_, mulLeft, mulRight, sub)
    | Madd (_, mulLeft, mulRight, sub)
    | RawWriteWord (mulLeft, mulRight, sub)
    | RawWriteByte (mulLeft, mulRight, sub)
    | RawSlotInit (mulLeft, mulRight, sub, _) ->
        folder (folder (folder state mulLeft) mulRight) sub
    | And_imm (_, src, _)
    | Lsl_imm (_, src, _)
    | Lsr_imm (_, src, _)
    | Mvn (_, src)
    | Sxtb (_, src)
    | Sxth (_, src)
    | Sxtw (_, src)
    | Uxtb (_, src)
    | Uxth (_, src)
    | Uxtw (_, src)
    | PrintHeapString src
    | HeapLoad (_, src, _)
    | RefCountInc (src, _, _, _)
    | RefCountDec (src, _, _, _) ->
        folder state src
    | Call (_, _, args)
    | TailCall (_, args) ->
        args |> List.fold (foldOperandRegUse folder) state
    | ArgMoves args
    | TailArgMoves args ->
        args |> List.fold (fun acc (_, src) -> foldOperandRegUse folder acc src) state
    | IndirectCall (_, func, args)
    | IndirectTailCall (func, args) ->
        args |> List.fold (foldOperandRegUse folder) (folder state func)
    | ClosureCall (_, closure, args)
    | ClosureTailCall (closure, args) ->
        args |> List.fold (foldOperandRegUse folder) (folder state closure)
    | ClosureAlloc (_, _, captures) ->
        captures |> List.fold (foldOperandRegUse folder) state
    | HeapStore (addr, _, src, _) ->
        foldOperandRegUse folder (folder state addr) src
    | StringConcat (_, left, right)
    | FileWriteText (_, left, right)
    | FileAppendText (_, left, right) ->
        foldOperandRegUse folder state left |> fun acc -> foldOperandRegUse folder acc right
    | FileReadText (_, path)
    | FileExists (_, path)
    | FileDelete (_, path)
    | FileSetExecutable (_, path) ->
        foldOperandRegUse folder state path
    | Cset _
    | SaveRegs _
    | RestoreRegs _
    | FArgMoves _
    | PrintFloat _
    | PrintFloatNoNewline _
    | PrintString _
    | RuntimeError _
    | PrintChars _
    | Exit
    | FPhi _
    | FMov _
    | FLoad _
    | FAdd _
    | FSub _
    | FMul _
    | FDiv _
    | FNeg _
    | FAbs _
    | FSqrt _
    | FCmp _
    | FloatToInt64 _
    | FloatToBits _
    | FpToGp _
    | HeapAlloc _
    | LoadFuncAddr _
    | RandomInt64 _
    | DateNow _
    | CoverageHit _ -> state

/// Check if a register is read by an instruction.
let private regUsedInInstr (target: Reg) (instr: Instr) : bool =
    foldRegUses (fun used reg -> used || sameReg reg target) false instr

/// Record the last read of each temporary whose remaining uses affect a fold.
/// Restricting the map to candidates avoids both repeated suffix scans and a
/// full-block liveness map for blocks that contain no relevant peepholes.
let private lastRelevantRegUses
    (candidates: Set<Reg>)
    (instrs: Instr list)
    : Map<Reg, int> =
    let rec loop index uses remaining =
        match remaining with
        | instr :: rest ->
            let uses' =
                foldRegUses (fun acc reg ->
                    if Set.contains reg candidates then Map.add reg index acc else acc
                ) uses instr
            loop (index + 1) uses' rest
        | [] -> uses
    loop 0 Map.empty instrs

let private regUsedAfter (lastUses: Map<Reg, int>) (index: int) (reg: Reg) : bool =
    Map.tryFind reg lastUses |> Option.exists (fun lastUse -> lastUse > index)

/// Check if a register is used in any instruction (for dead code detection)
let isRegUsedInInstrs (reg: Reg) (instrs: Instr list) : bool =
    instrs |> List.exists (regUsedInInstr reg)

/// Check if a value is suitable for multiply-by-constant strength reduction
/// Returns Some (shift, isAdd) where:
///   isAdd=true: n = 2^shift + 1 (e.g., 3=2+1, 5=4+1, 9=8+1)
///   isAdd=false: n = 2^shift - 1 (e.g., 7=8-1, 15=16-1, 31=32-1)
let tryMulConstantPattern (n: int64) : (int * bool) option =
    match n with
    | 3L -> Some (1, true)    // 3 = 2 + 1 = (1 << 1) + 1
    | 5L -> Some (2, true)    // 5 = 4 + 1 = (1 << 2) + 1
    | 7L -> Some (3, false)   // 7 = 8 - 1 = (1 << 3) - 1
    | 9L -> Some (3, true)    // 9 = 8 + 1 = (1 << 3) + 1
    | 15L -> Some (4, false)  // 15 = 16 - 1 = (1 << 4) - 1
    | 17L -> Some (4, true)   // 17 = 16 + 1 = (1 << 4) + 1
    | 31L -> Some (5, false)  // 31 = 32 - 1 = (1 << 5) - 1
    | 33L -> Some (5, true)   // 33 = 32 + 1 = (1 << 5) + 1
    | 63L -> Some (6, false)  // 63 = 64 - 1 = (1 << 6) - 1
    | 65L -> Some (6, true)   // 65 = 64 + 1 = (1 << 6) + 1
    | _ -> None

let private mulByConstantCandidates (instrs: Instr list) : Set<Reg> =
    let rec loop candidates remaining =
        match remaining with
        | Mov (constReg, Imm n) :: Mul (_, mulLeft, mulRight) :: rest
            when Option.isSome (tryMulConstantPattern n)
                 && ((sameReg constReg mulRight && not (sameReg constReg mulLeft))
                     || (sameReg constReg mulLeft && not (sameReg constReg mulRight))) ->
            loop (Set.add constReg candidates) rest
        | _ :: rest -> loop candidates rest
        | [] -> candidates
    loop Set.empty instrs

/// Try to optimize multiply-by-constant patterns
/// Pattern: Mov temp, Imm n; Mul dest, x, temp → Lsl_imm temp, x, shift; Add/Sub dest, x, Reg temp
/// This converts multiplication by constants like 3, 5, 7, 9 to shift+add/sub sequences
/// which ARM64 can execute in a single ADD_shifted/SUB_shifted instruction
let tryMulByConstant (instrs: Instr list) : Instr list =
    let candidates = mulByConstantCandidates instrs
    if Set.isEmpty candidates then
        instrs
    else
        let lastUses = lastRelevantRegUses candidates instrs
        let rec loop index acc remaining =
            match remaining with
            | [] -> List.rev acc
            | [single] -> List.rev (single :: acc)
            | Mov (constReg, Imm n) :: Mul (mulDest, mulLeft, mulRight) :: rest
                when sameReg constReg mulRight && not (sameReg constReg mulLeft) ->
                match tryMulConstantPattern n with
                | Some (shift, isAdd) when not (regUsedAfter lastUses (index + 1) constReg) ->
                    let shiftInstr = Lsl_imm (constReg, mulLeft, shift)
                    let combineInstr =
                        if isAdd then
                            Add (mulDest, mulLeft, Reg constReg)
                        else
                            Sub (mulDest, constReg, Reg mulLeft)
                    loop (index + 2) (combineInstr :: shiftInstr :: acc) rest
                | _ ->
                    loop (index + 1) (Mov (constReg, Imm n) :: acc) (Mul (mulDest, mulLeft, mulRight) :: rest)
            | Mov (constReg, Imm n) :: Mul (mulDest, mulLeft, mulRight) :: rest
                when sameReg constReg mulLeft && not (sameReg constReg mulRight) ->
                match tryMulConstantPattern n with
                | Some (shift, isAdd) when not (regUsedAfter lastUses (index + 1) constReg) ->
                    let shiftInstr = Lsl_imm (constReg, mulRight, shift)
                    let combineInstr =
                        if isAdd then
                            Add (mulDest, mulRight, Reg constReg)
                        else
                            Sub (mulDest, constReg, Reg mulRight)
                    loop (index + 2) (combineInstr :: shiftInstr :: acc) rest
                | _ ->
                    loop (index + 1) (Mov (constReg, Imm n) :: acc) (Mul (mulDest, mulLeft, mulRight) :: rest)
            | instr :: rest -> loop (index + 1) (instr :: acc) rest
        loop 0 [] instrs

let private mulAddCandidates (instrs: Instr list) : Set<Reg> =
    let rec loop candidates remaining =
        match remaining with
        | Mul (mulDest, _, _) :: Add (_, addLeft, Reg addRight) :: rest
            when (sameReg mulDest addLeft && not (sameReg mulDest addRight))
                 || (sameReg mulDest addRight && not (sameReg mulDest addLeft)) ->
            loop (Set.add mulDest candidates) rest
        | _ :: rest -> loop candidates rest
        | [] -> candidates
    loop Set.empty instrs

/// Try to fuse MUL + ADD into MADD (multiply-add)
/// Pattern: MUL temp, a, b; ADD dest, temp, Reg c → MADD dest, a, b, c
/// Or:      MUL temp, a, b; ADD dest, Reg c, temp → MADD dest, a, b, c (commutative)
let tryFuseMulAdd (instrs: Instr list) : Instr list =
    let candidates = mulAddCandidates instrs
    if Set.isEmpty candidates then
        instrs
    else
        let lastUses = lastRelevantRegUses candidates instrs
        let rec loop index acc remaining =
            match remaining with
            | [] -> List.rev acc
            | [single] -> List.rev (single :: acc)
            | Mul (mulDest, mulLeft, mulRight) :: Add (addDest, addLeft, Reg addRight) :: rest
                when sameReg mulDest addLeft && not (sameReg mulDest addRight) ->
                if not (regUsedAfter lastUses (index + 1) mulDest) then
                    loop (index + 2) (Madd (addDest, mulLeft, mulRight, addRight) :: acc) rest
                else
                    loop (index + 1) (Mul (mulDest, mulLeft, mulRight) :: acc) (Add (addDest, addLeft, Reg addRight) :: rest)
            | Mul (mulDest, mulLeft, mulRight) :: Add (addDest, addLeft, Reg addRight) :: rest
                when sameReg mulDest addRight && not (sameReg mulDest addLeft) ->
                if not (regUsedAfter lastUses (index + 1) mulDest) then
                    loop (index + 2) (Madd (addDest, mulLeft, mulRight, addLeft) :: acc) rest
                else
                    loop (index + 1) (Mul (mulDest, mulLeft, mulRight) :: acc) (Add (addDest, addLeft, Reg addRight) :: rest)
            | instr :: rest -> loop (index + 1) (instr :: acc) rest
        loop 0 [] instrs

let private mulSubCandidates (instrs: Instr list) : Set<Reg> =
    let rec loop candidates remaining =
        match remaining with
        | Mul (mulDest, _, _) :: Sub (_, minuend, Reg subtrahend) :: rest
            when sameReg mulDest subtrahend && not (sameReg mulDest minuend) ->
            loop (Set.add mulDest candidates) rest
        | _ :: rest -> loop candidates rest
        | [] -> candidates
    loop Set.empty instrs

/// Try to fuse MUL + SUB into MSUB (multiply-subtract)
/// Pattern: MUL temp, a, b; SUB dest, minuend, Reg temp → MSUB dest, a, b, minuend
let tryFuseMulSub (instrs: Instr list) : Instr list =
    let candidates = mulSubCandidates instrs
    if Set.isEmpty candidates then
        instrs
    else
        let lastUses = lastRelevantRegUses candidates instrs
        let rec loop index acc remaining =
            match remaining with
            | [] -> List.rev acc
            | [single] -> List.rev (single :: acc)
            | Mul (mulDest, mulLeft, mulRight) :: Sub (subDest, minuend, Reg subtrahend) :: rest
                when sameReg mulDest subtrahend && not (sameReg mulDest minuend) ->
                if not (regUsedAfter lastUses (index + 1) mulDest) then
                    loop (index + 2) (Msub (subDest, mulLeft, mulRight, minuend) :: acc) rest
                else
                    loop (index + 1) (Mul (mulDest, mulLeft, mulRight) :: acc) (Sub (subDest, minuend, Reg subtrahend) :: rest)
            | instr :: rest -> loop (index + 1) (instr :: acc) rest
        loop 0 [] instrs

/// Try to fuse Cset + Branch into CondBranch
/// Pattern: last instruction is Cset dest, cond; terminator is Branch dest, trueL, falseL
/// Result: remove Cset, replace Branch with CondBranch cond, trueL, falseL
let tryFuseCondBranch (instrs: Instr list) (terminator: Terminator) : (Instr list * Terminator) option =
    match terminator with
    | Branch (condReg, trueLabel, falseLabel) ->
        // Check if last instruction is Cset writing to condReg
        match List.tryLast instrs with
        | Some (Cset (dest, cond)) when sameReg dest condReg ->
            // Check that condReg is not used elsewhere in the block (except the Cset and Branch)
            let otherInstrs = instrs |> List.take (List.length instrs - 1)
            if not (isRegUsedInInstrs condReg otherInstrs) then
                // Fuse: remove Cset and replace Branch with CondBranch
                Some (otherInstrs, CondBranch (cond, trueLabel, falseLabel))
            else
                None
        | _ -> None
    | _ -> None

/// Check if a value is a power of 2 (exactly one bit set)
let isPowerOf2 (n: int64) : bool =
    n > 0L && (n &&& (n - 1L)) = 0L

/// Get the bit position of a power-of-2 value (log2)
let bitPosition (n: int64) : int =
    let rec loop pos x =
        if x = 1L then pos
        else loop (pos + 1) (x >>> 1)
    loop 0 n

/// Try to fuse AND_imm (power-of-2 mask) + BranchZero/Branch into BranchBitZero/BranchBitNonZero
/// Pattern: last instruction is AND_imm dest, src, mask where mask is power of 2
///          terminator is BranchZero(dest, ...) or Branch(dest, ...)
/// Result: BranchBitZero(src, bitNum, ...) or BranchBitNonZero(src, bitNum, ...)
/// This uses TBZ/TBNZ instructions which test a single bit
let tryFuseAndBitBranch (instrs: Instr list) (terminator: Terminator) : (Instr list * Terminator) option =
    match List.tryLast instrs with
    | Some (And_imm (andDest, andSrc, mask)) when isPowerOf2 mask ->
        let bit = bitPosition mask
        let otherInstrs = instrs |> List.take (List.length instrs - 1)
        // Check that andDest is not used in the remaining instructions
        if isRegUsedInInstrs andDest otherInstrs then
            None
        else
            match terminator with
            | BranchZero (condReg, zeroLabel, nonZeroLabel) when sameReg condReg andDest ->
                // AND_imm + CBZ → TBZ
                Some (otherInstrs, BranchBitZero (andSrc, bit, zeroLabel, nonZeroLabel))
            | Branch (condReg, nonZeroLabel, zeroLabel) when sameReg condReg andDest ->
                // AND_imm + CBNZ → TBNZ
                Some (otherInstrs, BranchBitNonZero (andSrc, bit, nonZeroLabel, zeroLabel))
            | _ -> None
    | _ -> None

/// Try to fuse CMP reg, #0 + CondBranch into Branch/BranchZero
/// Pattern: last instruction is CMP reg, #0; terminator is CondBranch(EQ/NE, ...)
/// Result:
///   - CMP reg, #0 + CondBranch(EQ, true, false) → BranchZero(reg, true, false)  [uses CBZ]
///   - CMP reg, #0 + CondBranch(NE, true, false) → Branch(reg, true, false)      [uses CBNZ]
let tryFuseCmpZeroBranch (instrs: Instr list) (terminator: Terminator) : (Instr list * Terminator) option =
    match terminator with
    | CondBranch (cond, trueLabel, falseLabel) ->
        // Check if last instruction is CMP reg, #0
        match List.tryLast instrs with
        | Some (Cmp (cmpReg, Imm 0L)) ->
            let otherInstrs = instrs |> List.take (List.length instrs - 1)
            match cond with
            | LIR.EQ ->
                // CMP reg, #0 + B.eq → CBZ reg (BranchZero)
                Some (otherInstrs, BranchZero (cmpReg, trueLabel, falseLabel))
            | LIR.NE ->
                // CMP reg, #0 + B.ne → CBNZ reg (Branch)
                Some (otherInstrs, Branch (cmpReg, trueLabel, falseLabel))
            | _ ->
                // Other conditions (LT, GT, LE, GE) can't be fused with CBZ/CBNZ
                None
        | _ -> None
    | _ -> None

/// Apply TBZ/TBNZ fusion if applicable
/// Fuses AND_imm (power-of-2 mask) + BranchZero/Branch → BranchBitZero/BranchBitNonZero
let applyAndBitBranchFusion (instrs: Instr list) (terminator: Terminator) : (Instr list * Terminator) =
    match tryFuseAndBitBranch instrs terminator with
    | Some (fusedInstrs, fusedTerminator) -> (fusedInstrs, fusedTerminator)
    | None -> (instrs, terminator)

/// Optimize a basic block (returns whether anything changed)
let optimizeBlock (block: BasicBlock) : BasicBlock * bool =
    let instrs' = optimizeInstrs block.Instrs
    let instrsCopyCleaned = removeRedundantFloatingCopyBackMoves instrs'
    // Apply multiply-by-constant strength reduction (Mov + Mul → Lsl + Add/Sub)
    let instrs1 = tryMulByConstant instrsCopyCleaned
    // Apply MUL + ADD → MADD fusion
    let instrs2 = tryFuseMulAdd instrs1
    // Apply MUL + SUB → MSUB fusion
    let instrs'' = tryFuseMulSub instrs2

    // Try to fuse Cset + Branch into CondBranch
    let (instrs''', terminator') =
        match tryFuseCondBranch instrs'' block.Terminator with
        | Some (fusedInstrs, fusedTerminator) ->
            // After fusing Cset + Branch → CondBranch, try to fuse CMP #0 + CondBranch → CBZ/CBNZ
            match tryFuseCmpZeroBranch fusedInstrs fusedTerminator with
            | Some (fusedInstrs2, fusedTerminator2) ->
                (fusedInstrs2, fusedTerminator2)
            | None ->
                (fusedInstrs, fusedTerminator)
        | None ->
            // Also try CMP #0 + CondBranch fusion on the original terminator
            match tryFuseCmpZeroBranch instrs'' block.Terminator with
            | Some (fusedInstrs, fusedTerminator) ->
                (fusedInstrs, fusedTerminator)
            | None ->
                (instrs'', block.Terminator)

    // Try to fuse AND_imm (power-of-2) + BranchZero/Branch → TBZ/TBNZ
    let (finalInstrs, finalTerminator) = applyAndBitBranchFusion instrs''' terminator'
    let block' = { block with Instrs = finalInstrs; Terminator = finalTerminator }
    (block', block' <> block)

/// Optimize a CFG in a single pass (returns whether anything changed)
let private optimizeCFGOnce
    (cfg: CFG)
    (domCache: DominatorCache option)
    : CFG * bool * DominatorCache option =
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let (block', blockChanged) = optimizeBlock block
            (Map.add label block' acc, ch || blockChanged)
        ) (Map.empty, false)
    let cfg' = { cfg with Blocks = blocks' }
    let (cfg'', hoisted, cache') = applyLoopInvariantConstHoist cfg' domCache
    (cfg'', changed || hoisted, cache')

/// Optimize a CFG until fixed point
let optimizeCFG (cfg: CFG) : CFG =
    validateCFGShape cfg

    let rec loop current remaining iteration domCache =
        if remaining <= 0 then
            current
        else
            let (next, changed, nextCache) = optimizeCFGOnce current domCache
            if changed then
                loop next (remaining - 1) (iteration + 1) nextCache
            else
                next
    loop cfg 10 1 None

/// Optimize a function
let optimizeFunction (func: Function) : Function =
    { func with CFG = optimizeCFG func.CFG }

/// Optimize a program
let optimizeProgram (program: Program) : Program =
    let (Program (functions, variants, records)) = program
    let functions' = functions |> List.map optimizeFunction
    Program (functions', variants, records)
