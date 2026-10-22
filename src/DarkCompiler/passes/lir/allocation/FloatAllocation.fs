// FloatAllocation.fs - Allocate floating registers and apply their physical mapping.

module FloatAllocation

open AllocationModel
open RegisterFacts
open RegisterLiveness
open RegisterInterference
open RegisterCoalescing
open RegisterColoring

// ============================================================================
// Float Register Allocation
// ============================================================================

/// Float caller-saved registers (D0-D7)
let floatCallerSavedRegs : LIR.PhysFPReg list = [
    LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7
]

/// Float callee-saved registers (D8-D15)
let floatCalleeSavedRegs : LIR.PhysFPReg list = [
    LIR.D8; LIR.D9; LIR.D10; LIR.D11; LIR.D12; LIR.D13; LIR.D14; LIR.D15
]

/// All allocatable float registers - caller-saved first, then callee-saved
let allocatableFloatRegs : LIR.PhysFPReg list = floatCallerSavedRegs @ floatCalleeSavedRegs

/// Both backends expose the complete abstract D0-D15 register set. Backend-local
/// scratch operations must preserve any physical register they borrow.
let allocatableFloatRegsFor (_arch: Platform.Arch) : LIR.PhysFPReg list =
    allocatableFloatRegs

/// AArch64 preserves D8-D15 across calls, while the System V x86-64 ABI treats
/// every XMM register as caller-saved.
let floatCallerSavedRegsFor (arch: Platform.Arch) : LIR.PhysFPReg list =
    match arch with
    | Platform.X86_64 -> allocatableFloatRegsFor arch
    | Platform.ARM64 -> floatCallerSavedRegs

/// Float allocation result
type FAllocationResult = {
    Domain: VRegDomain
    Allocations: LIR.PhysFPReg option array
    UsedCalleeSavedF: LIR.PhysFPReg list
}

/// Convert physical FP register to integer for graph coloring
let physFPRegToInt (reg: LIR.PhysFPReg) : int =
    match reg with
    | LIR.D0 -> 0 | LIR.D1 -> 1 | LIR.D2 -> 2 | LIR.D3 -> 3
    | LIR.D4 -> 4 | LIR.D5 -> 5 | LIR.D6 -> 6 | LIR.D7 -> 7
    | LIR.D8 -> 8 | LIR.D9 -> 9 | LIR.D10 -> 10 | LIR.D11 -> 11
    | LIR.D12 -> 12 | LIR.D13 -> 13 | LIR.D14 -> 14 | LIR.D15 -> 15

/// Convert float coloring result to allocation
let floatColoringToAllocation (colorResult: ColoringResult) (registers: LIR.PhysFPReg list) : FAllocationResult =
    let domain = colorResult.Domain
    let n = domain.Ids.Length
    let allocations = Array.create n None
    let mutable usedCalleeSaved : LIR.PhysFPReg list = []

    for idx in 0 .. n - 1 do
        match colorResult.Colors.[idx] with
        | Some color ->
            if color < List.length registers then
                let reg = List.item color registers
                allocations.[idx] <- Some reg
                if List.contains reg floatCalleeSavedRegs && not (List.contains reg usedCalleeSaved) then
                    usedCalleeSaved <- reg :: usedCalleeSaved
        | None -> ()

    { Domain = domain
      Allocations = allocations
      UsedCalleeSavedF = usedCalleeSaved |> List.sort }

/// Run chordal graph coloring for float register allocation
/// additionalVRegs: FVirtual IDs that must be allocated (e.g., float parameters)
/// even if they don't appear in the CFG instructions
let internal chordalFloatAllocationWithLiveness
    (registers: LIR.PhysFPReg list)
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
            blockIndex
            classifiedBlocks
            domain
            livenessBits
            additionalVRegs
    // Add additional VRegs (like float params) as isolated vertices if not already in graph
    let graphWithParams : InterferenceGraph =
        { graph with Vertices = Bitset.union graph.Vertices additionalVRegs }
    if Bitset.isEmpty graphWithParams.Vertices then
        // No float registers used - return empty allocation
        { Domain = domain
          Allocations = Array.create domain.Ids.Length None
          UsedCalleeSavedF = [] }
    else
        let phiPairs = collectFPhiPairs blocks
        let movePairs = dedupePairs ((collectFPhiSourceMovePairs blocks) @ phiPairs)
        let phiIds =
            phiPairs
            |> List.fold (fun acc (destId, sourceId) ->
                acc |> Set.add destId |> Set.add sourceId) Set.empty
        // Preserve the ABI register of parameters participating in an FPhi.
        // Otherwise hard coalescing can displace an already zero-copy return
        // value merely to remove an invariant backedge move.
        let phiParamPrecolors =
            paramPrecolors
            |> List.filter (fun (vregId, _) -> Set.contains vregId phiIds)
        let colorResult =
            chordalGraphColor
                graphWithParams
                phiParamPrecolors
                (List.length registers)
                phiPairs
                movePairs
        floatColoringToAllocation colorResult registers

/// Run chordal graph coloring for float register allocation
/// additionalVRegs: FVirtual IDs that must be allocated (e.g., float parameters)
/// even if they don't appear in the CFG instructions
let chordalFloatAllocation (cfg: LIR.CFG) (additionalVRegs: int list) : FAllocationResult =
    let (blockIndex, blocks) = buildBlockIndex cfg
    let classifiedBlocks = classifyBlocks blocks
    let (domain, livenessBits) =
        computeFloatLivenessBitsFromFacts blockIndex classifiedBlocks additionalVRegs
    let additionalBits = vregBitsFromList domain additionalVRegs
    chordalFloatAllocationWithLiveness
        allocatableFloatRegs
        blockIndex
        blocks
        classifiedBlocks
        additionalBits
        []
        domain
        livenessBits

/// Apply float allocation to an FReg, converting FVirtual to FPhysical
let applyFloatAllocationToFReg (floatAllocation: FAllocationResult) (freg: LIR.FReg) : LIR.FReg =
    match freg with
    | LIR.FPhysical _ -> freg  // Already physical
    | LIR.FParallelMoveTemp -> freg  // Fixed temp
    | LIR.FVirtual id ->
        match tryIndexOf floatAllocation.Domain id with
        | Some idx ->
            match floatAllocation.Allocations.[idx] with
            | Some physReg -> LIR.FPhysical physReg
            | None -> Crash.crash $"Float register allocation bug: FVirtual {id} not found in allocation"
        | None -> Crash.crash $"Float register allocation bug: FVirtual {id} not found in allocation"

/// Apply float allocation to an instruction
let applyFloatAllocationToInstr (floatAllocation: FAllocationResult) (instr: LIR.Instr) : LIR.Instr =
    let applyF = applyFloatAllocationToFReg floatAllocation
    match instr with
    | LIR.FMov (dest, src) -> LIR.FMov (applyF dest, applyF src)
    | LIR.FAdd (dest, left, right) -> LIR.FAdd (applyF dest, applyF left, applyF right)
    | LIR.FSub (dest, left, right) -> LIR.FSub (applyF dest, applyF left, applyF right)
    | LIR.FMul (dest, left, right) -> LIR.FMul (applyF dest, applyF left, applyF right)
    | LIR.FDiv (dest, left, right) -> LIR.FDiv (applyF dest, applyF left, applyF right)
    | LIR.FNeg (dest, src) -> LIR.FNeg (applyF dest, applyF src)
    | LIR.FAbs (dest, src) -> LIR.FAbs (applyF dest, applyF src)
    | LIR.FSqrt (dest, src) -> LIR.FSqrt (applyF dest, applyF src)
    | LIR.FCmp (left, right) -> LIR.FCmp (applyF left, applyF right)
    | LIR.FLoad (dest, value) -> LIR.FLoad (applyF dest, value)
    | LIR.Int64ToFloat (dest, src) -> LIR.Int64ToFloat (applyF dest, src)
    | LIR.FloatToInt64 (dest, src) -> LIR.FloatToInt64 (dest, applyF src)
    | LIR.FloatToBits (dest, src) -> LIR.FloatToBits (dest, applyF src)
    | LIR.FpToGp (dest, src) -> LIR.FpToGp (dest, applyF src)
    | LIR.GpToFp (dest, src) -> LIR.GpToFp (applyF dest, src)
    | LIR.PrintFloat freg -> LIR.PrintFloat (applyF freg)
    | LIR.PrintFloatNoNewline freg -> LIR.PrintFloatNoNewline (applyF freg)
    | LIR.FPhi (dest, sources) ->
        LIR.FPhi (applyF dest, sources |> List.map (fun (src, label) -> (applyF src, label)))
    | LIR.FArgMoves moves ->
        LIR.FArgMoves (moves |> List.map (fun (physReg, src) -> (physReg, applyF src)))
    | LIR.FloatToString (dest, value) -> LIR.FloatToString (dest, applyF value)
    | LIR.Sleep (effectId, delayMs) -> LIR.Sleep (effectId, applyF delayMs)
    // HeapStore with float value: the Virtual register ID is shared with FVirtual
    // We need to apply float allocation to convert Virtual(n) to the allocated physical register
    | LIR.HeapStore (addr, offset, LIR.Reg (LIR.Virtual vregId), Some AST.TFloat64) ->
        // Convert Virtual to the allocated FPhysical if it's in the float mapping
        let allocatedFReg = applyF (LIR.FVirtual vregId)
        // Convert the FReg back to a Virtual/Physical Reg for HeapStore
        let allocatedReg =
            match allocatedFReg with
            | LIR.FPhysical physFReg ->
                // Convert physical float reg to physical GP reg (for HeapStore operand format)
                // The actual STR_fp instruction will be generated in CodeGen based on valueType
                let physReg =
                    match physFReg with
                    | LIR.D0 -> LIR.X0 | LIR.D1 -> LIR.X1 | LIR.D2 -> LIR.X2 | LIR.D3 -> LIR.X3
                    | LIR.D4 -> LIR.X4 | LIR.D5 -> LIR.X5 | LIR.D6 -> LIR.X6 | LIR.D7 -> LIR.X7
                    | LIR.D8 -> LIR.X8 | LIR.D9 -> LIR.X9 | LIR.D10 -> LIR.X10 | LIR.D11 -> LIR.X11
                    | LIR.D12 -> LIR.X12 | LIR.D13 -> LIR.X13 | LIR.D14 -> LIR.X14 | LIR.D15 -> LIR.X15
                LIR.Physical physReg
            | LIR.FVirtual n -> LIR.Virtual n
            | LIR.FParallelMoveTemp -> Crash.crash "Parallel-move temporary cannot be a stored float value"
        LIR.HeapStore (addr, offset, LIR.Reg allocatedReg, Some AST.TFloat64)
    | _ -> instr  // Non-float instructions unchanged

/// Apply float allocation to a basic block
let applyFloatAllocationToBlock (floatAllocation: FAllocationResult) (block: LIR.BasicBlock) : LIR.BasicBlock =
    { block with Instrs = block.Instrs |> List.map (applyFloatAllocationToInstr floatAllocation) }

/// Apply float allocation to basic blocks
let applyFloatAllocationToBlocks
    (floatAllocation: FAllocationResult)
    (blocks: LIR.BasicBlock array)
    : LIR.BasicBlock array =
    blocks |> Array.map (applyFloatAllocationToBlock floatAllocation)

/// Apply float allocation to a CFG
let applyFloatAllocationToCFG (floatAllocation: FAllocationResult) (cfg: LIR.CFG) : LIR.CFG =
    let (blockIndex, blocks) = buildBlockIndex cfg
    let updatedBlocks = applyFloatAllocationToBlocks floatAllocation blocks
    { cfg with Blocks = blocksToMap blockIndex updatedBlocks }
