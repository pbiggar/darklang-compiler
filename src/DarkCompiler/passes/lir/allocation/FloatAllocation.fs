// FloatAllocation.fs - Schedule, allocate, spill, and materialize floating-point values.

module FloatAllocation

open AllocationModel
open RegisterFacts
open RegisterLiveness
open RegisterInterference
open RegisterCoalescing
open RegisterColoring

let floatCallerSavedRegs : LIR.PhysFPReg list =
    [ LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7 ]

let floatCalleeSavedRegs : LIR.PhysFPReg list =
    [ LIR.D8; LIR.D9; LIR.D10; LIR.D11; LIR.D12; LIR.D13; LIR.D14; LIR.D15 ]

let allocatableFloatRegs : LIR.PhysFPReg list =
    floatCallerSavedRegs @ floatCalleeSavedRegs

let allocatableFloatRegsFor (arch: Platform.Arch) : LIR.PhysFPReg list =
    match arch with
    | Platform.ARM64 -> allocatableFloatRegs
    | Platform.X86_64 -> allocatableFloatRegs |> List.take 14

let floatCallerSavedRegsFor (arch: Platform.Arch) : LIR.PhysFPReg list =
    match arch with
    | Platform.X86_64 -> allocatableFloatRegsFor arch
    | Platform.ARM64 -> floatCallerSavedRegs

type FAllocation =
    | FPhysReg of LIR.PhysFPReg
    | FStackSlot of int
    | FRematerialized of float

type FAllocationResult = {
    Domain: VRegDomain
    Allocations: FAllocation option array
    StackSize: int
    UsedCalleeSavedF: LIR.PhysFPReg list
    SpillScratchLeft: LIR.FReg
    SpillScratchRight: LIR.FReg
    SpillScratchThird: LIR.FReg
}

let physFPRegToInt (reg: LIR.PhysFPReg) : int =
    match reg with
    | LIR.D0 -> 0 | LIR.D1 -> 1 | LIR.D2 -> 2 | LIR.D3 -> 3
    | LIR.D4 -> 4 | LIR.D5 -> 5 | LIR.D6 -> 6 | LIR.D7 -> 7
    | LIR.D8 -> 8 | LIR.D9 -> 9 | LIR.D10 -> 10 | LIR.D11 -> 11
    | LIR.D12 -> 12 | LIR.D13 -> 13 | LIR.D14 -> 14 | LIR.D15 -> 15

let tryFloatAllocation (allocation: FAllocationResult) (fvregId: int) : FAllocation option =
    match tryIndexOf allocation.Domain fvregId with
    | Some idx -> allocation.Allocations.[idx]
    | None -> None

let private alignTo16 (size: int) : int =
    if size = 0 then 0 else ((size + 15) / 16) * 16

let private rematerializableFloatLoads (blocks: LIR.BasicBlock array) : Map<int, float> =
    blocks
    |> Array.fold (fun values block ->
        block.Instrs
        |> List.fold (fun values instr ->
            match instr with
            | LIR.FLoad (LIR.FVirtual id, value) -> Map.add id value values
            | _ -> values) values) Map.empty

let private allocateSpillSlots
    (graph: InterferenceGraph)
    (spills: BitSet)
    (rematerializable: Map<int, float>)
    : Map<int, int> * int =
    let spillIndices =
        [ for idx in 0 .. graph.Domain.Ids.Length - 1 do
            if Bitset.containsIndex idx spills
               && not (Map.containsKey graph.Domain.Ids.[idx] rematerializable) then
                yield idx ]
    let firstAvailableColor (used: Set<int>) : int =
        let rec find candidate =
            if Set.contains candidate used then find (candidate + 1) else candidate
        find 0
    let assignments =
        spillIndices
        |> List.fold (fun assigned idx ->
            let used =
                assigned
                |> Map.fold (fun colors neighborIdx color ->
                    if Bitset.containsIndex neighborIdx graph.Neighbors.[idx] then
                        Set.add color colors
                    else colors) Set.empty
            Map.add idx (firstAvailableColor used) assigned) Map.empty
    let slotCount =
        assignments |> Map.fold (fun count _ color -> max count (color + 1)) 0
    (assignments, slotCount)

let private floatColoringToAllocation
    (graph: InterferenceGraph)
    (colorResult: ColoringResult)
    (registers: LIR.PhysFPReg list)
    (initialStackSize: int)
    (rematerializable: Map<int, float>)
    : FAllocationResult =
    let (spillSlots, spillSlotCount) =
        allocateSpillSlots graph colorResult.Spills rematerializable
    let allocationAt idx =
        match colorResult.Colors.[idx] with
        | Some color when color < List.length registers ->
            Some (FPhysReg (List.item color registers))
        | _ when Bitset.containsIndex idx colorResult.Spills ->
            match Map.tryFind colorResult.Domain.Ids.[idx] rematerializable with
            | Some value -> Some (FRematerialized value)
            | None ->
                match Map.tryFind idx spillSlots with
                | Some slotColor -> Some (FStackSlot (-(initialStackSize + (slotColor + 1) * 8)))
                | None -> Crash.crash $"Missing Float spill slot for domain index {idx}"
        | _ -> None
    let allocations = Array.init colorResult.Domain.Ids.Length allocationAt
    let usedCalleeSaved =
        allocations
        |> Array.choose (function
            | Some (FPhysReg reg) when List.contains reg floatCalleeSavedRegs -> Some reg
            | _ -> None)
        |> Array.distinct
        |> Array.sort
        |> Array.toList
    let (spillScratchLeft, spillScratchRight, spillScratchThird) =
        if List.contains LIR.D15 registers then
            (LIR.FVirtual 1000, LIR.FVirtual 1001, LIR.FVirtual 1002)
        else
            (LIR.FPhysical LIR.D14, LIR.FPhysical LIR.D15, LIR.FVirtual 1002)
    { Domain = colorResult.Domain
      Allocations = allocations
      StackSize = alignTo16 (initialStackSize + spillSlotCount * 8)
      UsedCalleeSavedF = usedCalleeSaved
      SpillScratchLeft = spillScratchLeft
      SpillScratchRight = spillScratchRight
      SpillScratchThird = spillScratchThird }

/// Move pure Float literal loads immediately before their first local use. Loads
/// used only on CFG edges retain their original dominating position.
let scheduleFloatLoadsInBlock (block: LIR.BasicBlock) : LIR.BasicBlock =
    let indexed = block.Instrs |> List.indexed
    let loads =
        indexed
        |> List.choose (fun (idx, instr) ->
            match instr with
            | LIR.FLoad (LIR.FVirtual id, _) -> Some (id, (idx, instr))
            | _ -> None)
        |> Map.ofList
    let firstUses =
        indexed
        |> List.fold (fun uses (idx, instr) ->
            getUsedFVRegs instr
            |> List.fold (fun result id ->
                if Map.containsKey id result then result else Map.add id idx result) uses) Map.empty
    let moves =
        loads
        |> Map.fold (fun scheduled id (loadIdx, instr) ->
            match Map.tryFind id firstUses with
            | Some useIdx when useIdx > loadIdx -> Map.add loadIdx (useIdx, instr) scheduled
            | _ -> scheduled) Map.empty
    let loadsAtUse =
        moves
        |> Map.fold (fun byUse _ (useIdx, instr) ->
            let existing = Map.tryFind useIdx byUse |> Option.defaultValue []
            Map.add useIdx (existing @ [instr]) byUse) Map.empty
    let scheduledInstrs =
        indexed
        |> List.collect (fun (idx, instr) ->
            let before = Map.tryFind idx loadsAtUse |> Option.defaultValue []
            if Map.containsKey idx moves then before else before @ [instr])
    { block with Instrs = scheduledInstrs }

let scheduleFloatLoadsInCFG (cfg: LIR.CFG) : LIR.CFG =
    { cfg with Blocks = cfg.Blocks |> Map.map (fun _ block -> scheduleFloatLoadsInBlock block) }

let internal chordalFloatAllocationWithLiveness
    (registers: LIR.PhysFPReg list)
    (initialStackSize: int)
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    (classifiedBlocks: ClassifiedBlock array)
    (additionalVRegs: BitSet)
    (paramPrecolors: (int * int) list)
    (domain: VRegDomain)
    (livenessBits: BlockLiveness array)
    : FAllocationResult =
    let graph =
        buildFloatInterferenceGraphBitsetWithLiveness
            blockIndex classifiedBlocks domain livenessBits additionalVRegs
    let graphWithParams =
        { graph with Vertices = Bitset.union graph.Vertices additionalVRegs }
    if Bitset.isEmpty graphWithParams.Vertices then
        { Domain = domain
          Allocations = Array.create domain.Ids.Length None
          StackSize = initialStackSize
          UsedCalleeSavedF = []
          SpillScratchLeft = LIR.FVirtual 1000
          SpillScratchRight = LIR.FVirtual 1001
          SpillScratchThird = LIR.FVirtual 1002 }
    else
        let phiPairs = collectFPhiPairs blocks
        let movePairs = dedupePairs ((collectFPhiSourceMovePairs blocks) @ phiPairs)
        let phiIds =
            phiPairs
            |> List.fold (fun ids (destId, sourceId) -> ids |> Set.add destId |> Set.add sourceId) Set.empty
        let phiParamPrecolors =
            paramPrecolors |> List.filter (fun (vregId, _) -> Set.contains vregId phiIds)
        let colorResult =
            chordalGraphColor graphWithParams phiParamPrecolors (List.length registers) phiPairs movePairs
        floatColoringToAllocation
            graphWithParams colorResult registers initialStackSize (rematerializableFloatLoads blocks)

let chordalFloatAllocation (cfg: LIR.CFG) (additionalVRegs: int list) : FAllocationResult =
    let scheduledCFG = scheduleFloatLoadsInCFG cfg
    let (blockIndex, blocks) = buildBlockIndex scheduledCFG
    let classifiedBlocks = classifyBlocks blocks
    let (domain, livenessBits) =
        computeFloatLivenessBitsFromFacts blockIndex classifiedBlocks additionalVRegs
    chordalFloatAllocationWithLiveness
        allocatableFloatRegs 0 blockIndex blocks classifiedBlocks
        (vregBitsFromList domain additionalVRegs) [] domain livenessBits

let private isFixedFReg = function
    | LIR.FVirtual 1000 | LIR.FVirtual 1001 | LIR.FVirtual 1002 | LIR.FVirtual 2000 -> true
    | LIR.FVirtual n when n >= 3000 && n < 4000 -> true
    | _ -> false

let applyFloatAllocationToFReg (allocation: FAllocationResult) (freg: LIR.FReg) : LIR.FReg =
    match freg with
    | LIR.FPhysical _ -> freg
    | _ when isFixedFReg freg -> freg
    | LIR.FVirtual id ->
        match tryFloatAllocation allocation id with
        | Some (FPhysReg reg) -> LIR.FPhysical reg
        | Some (FStackSlot _) -> Crash.crash $"Spilled Float vreg {id} requires instruction repair"
        | Some (FRematerialized _) -> Crash.crash $"Rematerialized Float vreg {id} requires instruction repair"
        | None -> Crash.crash $"Float register allocation bug: FVirtual {id} not found in allocation"

let private materializeUse
    (allocation: FAllocationResult)
    (scratch: LIR.FReg)
    (freg: LIR.FReg)
    : LIR.Instr list * LIR.FReg =
    match freg with
    | LIR.FPhysical _ -> ([], freg)
    | _ when isFixedFReg freg -> ([], freg)
    | LIR.FVirtual id ->
        match tryFloatAllocation allocation id with
        | Some (FPhysReg reg) -> ([], LIR.FPhysical reg)
        | Some (FStackSlot slot) ->
            ([LIR.FSpillLoad (scratch, slot)], scratch)
        | Some (FRematerialized value) ->
            ([LIR.FLoad (scratch, value)], scratch)
        | None -> Crash.crash $"Float register allocation bug: FVirtual {id} not found in allocation"

let private destination
    (allocation: FAllocationResult)
    (scratch: LIR.FReg)
    (freg: LIR.FReg)
    : LIR.FReg * (LIR.Instr list -> LIR.Instr list) =
    match freg with
    | LIR.FPhysical _ -> (freg, id)
    | _ when isFixedFReg freg -> (freg, id)
    | LIR.FVirtual id ->
        match tryFloatAllocation allocation id with
        | Some (FPhysReg reg) -> (LIR.FPhysical reg, fun instrs -> instrs)
        | Some (FStackSlot slot) ->
            (scratch, fun instrs -> instrs @ [LIR.FSpillStore (slot, scratch)])
        | Some (FRematerialized _) ->
            Crash.crash $"Only Float literal loads may define rematerialized vreg {id}"
        | None -> Crash.crash $"Float register allocation bug: FVirtual {id} not found in allocation"

let private physFPRegAsGPReg = function
    | LIR.D0 -> LIR.X0 | LIR.D1 -> LIR.X1 | LIR.D2 -> LIR.X2 | LIR.D3 -> LIR.X3
    | LIR.D4 -> LIR.X4 | LIR.D5 -> LIR.X5 | LIR.D6 -> LIR.X6 | LIR.D7 -> LIR.X7
    | LIR.D8 -> LIR.X8 | LIR.D9 -> LIR.X9 | LIR.D10 -> LIR.X10 | LIR.D11 -> LIR.X11
    | LIR.D12 -> LIR.X12 | LIR.D13 -> LIR.X13 | LIR.D14 -> LIR.X14 | LIR.D15 -> LIR.X15

let private applyFloatArgMoves
    (allocation: FAllocationResult)
    (moves: (LIR.PhysFPReg * LIR.FReg) list)
    : LIR.Instr list =
    let located =
        moves
        |> List.map (fun (dest, src) ->
            let source =
                match src with
                | LIR.FPhysical reg -> FPhysReg reg
                | LIR.FVirtual id ->
                    match tryFloatAllocation allocation id with
                    | Some value -> value
                    | None -> Crash.crash $"Float argument source {id} has no allocation"
            (dest, source))
    let sourceRegister = function
        | FPhysReg reg -> Some reg
        | FStackSlot _ | FRematerialized _ -> None
    ParallelMoves.resolve located sourceRegister
    |> List.collect (function
        | ParallelMoves.SaveToTemp reg -> [LIR.FMov (allocation.SpillScratchRight, LIR.FPhysical reg)]
        | ParallelMoves.Move (dest, FPhysReg src) -> [LIR.FMov (LIR.FPhysical dest, LIR.FPhysical src)]
        | ParallelMoves.Move (dest, FStackSlot slot) -> [LIR.FSpillLoad (LIR.FPhysical dest, slot)]
        | ParallelMoves.Move (dest, FRematerialized value) -> [LIR.FLoad (LIR.FPhysical dest, value)]
        | ParallelMoves.MoveFromTemp dest -> [LIR.FMov (LIR.FPhysical dest, allocation.SpillScratchRight)])

let applyFloatAllocationToInstrs
    (allocation: FAllocationResult)
    (instr: LIR.Instr)
    : LIR.Instr list =
    let unary dest src makeInstr =
        let (loads, allocatedSrc) = materializeUse allocation allocation.SpillScratchLeft src
        let (allocatedDest, finish) = destination allocation allocation.SpillScratchLeft dest
        finish (loads @ [makeInstr allocatedDest allocatedSrc])
    let binary dest left right makeInstr =
        let (leftLoads, allocatedLeft) = materializeUse allocation allocation.SpillScratchLeft left
        let (rightLoads, allocatedRight) = materializeUse allocation allocation.SpillScratchRight right
        let (allocatedDest, finish) = destination allocation allocation.SpillScratchLeft dest
        finish (leftLoads @ rightLoads @ [makeInstr allocatedDest allocatedLeft allocatedRight])
    let ternary dest left right third makeInstr =
        let (leftLoads, allocatedLeft) = materializeUse allocation allocation.SpillScratchLeft left
        let (rightLoads, allocatedRight) = materializeUse allocation allocation.SpillScratchRight right
        let (thirdLoads, allocatedThird) = materializeUse allocation allocation.SpillScratchThird third
        let (allocatedDest, finish) = destination allocation allocation.SpillScratchLeft dest
        finish (leftLoads @ rightLoads @ thirdLoads @ [makeInstr allocatedDest allocatedLeft allocatedRight allocatedThird])
    let useOne src makeInstr =
        let (loads, allocatedSrc) = materializeUse allocation allocation.SpillScratchLeft src
        loads @ [makeInstr allocatedSrc]
    match instr with
    | LIR.FMov (dest, src) -> unary dest src (fun d s -> LIR.FMov (d, s))
    | LIR.FLoad (dest, value) when isFixedFReg dest -> [LIR.FLoad (dest, value)]
    | LIR.FLoad (LIR.FVirtual id, value) ->
        match tryFloatAllocation allocation id with
        | Some (FPhysReg reg) -> [LIR.FLoad (LIR.FPhysical reg, value)]
        | Some (FStackSlot slot) ->
            [ LIR.FLoad (allocation.SpillScratchLeft, value)
              LIR.FSpillStore (slot, allocation.SpillScratchLeft) ]
        | Some (FRematerialized _) -> []
        | None -> Crash.crash $"Float literal destination {id} has no allocation"
    | LIR.FLoad (dest, value) -> [LIR.FLoad (dest, value)]
    | LIR.FSpillLoad _ | LIR.FSpillStore _ -> [instr]
    | LIR.FAdd (dest, left, right) -> binary dest left right (fun d l r -> LIR.FAdd (d, l, r))
    | LIR.FSub (dest, left, right) -> binary dest left right (fun d l r -> LIR.FSub (d, l, r))
    | LIR.FMul (dest, left, right) -> binary dest left right (fun d l r -> LIR.FMul (d, l, r))
    | LIR.FMadd (dest, left, right, addend) -> ternary dest left right addend (fun d l r a -> LIR.FMadd (d, l, r, a))
    | LIR.FDiv (dest, left, right) -> binary dest left right (fun d l r -> LIR.FDiv (d, l, r))
    | LIR.FNeg (dest, src) -> unary dest src (fun d s -> LIR.FNeg (d, s))
    | LIR.FAbs (dest, src) -> unary dest src (fun d s -> LIR.FAbs (d, s))
    | LIR.FSqrt (dest, src) -> unary dest src (fun d s -> LIR.FSqrt (d, s))
    | LIR.FCmp (left, right) ->
        let (leftLoads, allocatedLeft) = materializeUse allocation allocation.SpillScratchLeft left
        let (rightLoads, allocatedRight) = materializeUse allocation allocation.SpillScratchRight right
        leftLoads @ rightLoads @ [LIR.FCmp (allocatedLeft, allocatedRight)]
    | LIR.Int64ToFloat (dest, src) ->
        let (allocatedDest, finish) = destination allocation allocation.SpillScratchLeft dest
        finish [LIR.Int64ToFloat (allocatedDest, src)]
    | LIR.GpToFp (dest, src) ->
        let (allocatedDest, finish) = destination allocation allocation.SpillScratchLeft dest
        finish [LIR.GpToFp (allocatedDest, src)]
    | LIR.FloatToInt64 (dest, src) -> useOne src (fun s -> LIR.FloatToInt64 (dest, s))
    | LIR.FloatToBits (dest, src) -> useOne src (fun s -> LIR.FloatToBits (dest, s))
    | LIR.FpToGp (dest, src) -> useOne src (fun s -> LIR.FpToGp (dest, s))
    | LIR.PrintFloat src -> useOne src LIR.PrintFloat
    | LIR.PrintFloatNoNewline src -> useOne src LIR.PrintFloatNoNewline
    | LIR.FloatToString (dest, src) -> useOne src (fun s -> LIR.FloatToString (dest, s))
    | LIR.Sleep (effectId, src) -> useOne src (fun s -> LIR.Sleep (effectId, s))
    | LIR.FArgMoves moves -> applyFloatArgMoves allocation moves
    | LIR.FPhi (dest, sources) ->
        [LIR.FPhi (applyFloatAllocationToFReg allocation dest,
                   sources |> List.map (fun (src, label) -> (applyFloatAllocationToFReg allocation src, label)))]
    | LIR.HeapStore (addr, offset, LIR.Reg (LIR.Virtual id), Some AST.TFloat64) ->
        let (loads, allocated) = materializeUse allocation allocation.SpillScratchLeft (LIR.FVirtual id)
        match allocated with
        | LIR.FPhysical reg ->
            loads @ [LIR.HeapStore (addr, offset, LIR.Reg (LIR.Physical (physFPRegAsGPReg reg)), Some AST.TFloat64)]
        | LIR.FVirtual scratchId ->
            loads @ [LIR.HeapStore (addr, offset, LIR.Reg (LIR.Virtual scratchId), Some AST.TFloat64)]
    | _ -> [instr]

let applyFloatAllocationToBlock (allocation: FAllocationResult) (block: LIR.BasicBlock) : LIR.BasicBlock =
    { block with Instrs = block.Instrs |> List.collect (applyFloatAllocationToInstrs allocation) }

let applyFloatAllocationToBlocks
    (allocation: FAllocationResult)
    (blocks: LIR.BasicBlock array)
    : LIR.BasicBlock array =
    blocks |> Array.map (applyFloatAllocationToBlock allocation)

let applyFloatAllocationToCFG (allocation: FAllocationResult) (cfg: LIR.CFG) : LIR.CFG =
    let (blockIndex, blocks) = buildBlockIndex cfg
    let updatedBlocks = applyFloatAllocationToBlocks allocation blocks
    { cfg with Blocks = blocksToMap blockIndex updatedBlocks }
