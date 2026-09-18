// SpillOperands.fs - Materialize allocated operands and spilled register values.

module SpillOperands

open AllocationModel
open RegisterPolicy
open FloatAllocation

// ============================================================================
// Linear Scan Register Allocation (kept for reference, not used)
// ============================================================================

let internal tryAllocation (allocation: AllocationResult) (vregId: int) : Allocation option =
    let domain = allocation.Domain
    let offset = vregId - domain.IndexOffset
    if offset < 0 || offset >= domain.IndexOf.Length then
        None
    else
        let idx = domain.IndexOf.[offset]
        if idx >= 0 then allocation.Allocations.[idx] else None

/// Get the caller-saved physical registers that contain live values
let getLiveCallerSavedRegs (allocation: AllocationResult) (liveVRegs: BitSet) : LIR.PhysReg list =
    let used = Array.create 7 false
    Bitset.iterIndices liveVRegs (fun idx ->
        match allocation.Allocations.[idx] with
        | Some (PhysReg LIR.X1) -> used.[0] <- true
        | Some (PhysReg LIR.X2) -> used.[1] <- true
        | Some (PhysReg LIR.X3) -> used.[2] <- true
        | Some (PhysReg LIR.X4) -> used.[3] <- true
        | Some (PhysReg LIR.X5) -> used.[4] <- true
        | Some (PhysReg LIR.X6) -> used.[5] <- true
        | Some (PhysReg LIR.X7) -> used.[6] <- true
        | _ -> ())
    callerSavedRegs
    |> List.mapi (fun idx reg -> (idx, reg))
    |> List.choose (fun (idx, reg) -> if used.[idx] then Some reg else None)

/// Get the caller-saved physical float registers that contain live values
let getLiveCallerSavedFloatRegs
    (arch: Platform.Arch)
    (liveFVRegs: BitSet)
    (floatAllocation: FAllocationResult)
    : LIR.PhysFPReg list =
    let callerSaved = floatCallerSavedRegsFor arch
    let used = Array.create 16 false
    Bitset.iterIndices liveFVRegs (fun idx ->
        match floatAllocation.Allocations.[idx] with
        | Some (FPhysReg reg) -> used.[physFPRegToInt reg] <- true
        | Some (FStackSlot _)
        | Some (FRematerialized _)
        | None -> ())
    callerSaved
    |> List.filter (fun reg -> used.[physFPRegToInt reg])

// ============================================================================
// Apply Allocation to LIR
// ============================================================================

/// Apply allocation to a register, returning the physical register and allocation info
let applyToReg (allocation: AllocationResult) (reg: LIR.Reg) : LIR.Reg * Allocation option =
    match reg with
    | LIR.Physical p -> (LIR.Physical p, None)
    | LIR.Virtual id ->
        match tryAllocation allocation id with
        | Some (PhysReg physReg) -> (LIR.Physical physReg, None)
        | Some (StackSlot offset) -> (LIR.Physical LIR.X11, Some (StackSlot offset))
        | None -> (LIR.Physical LIR.X11, None)

/// Apply allocation to an operand, returning load instructions if needed
let applyToOperand (allocation: AllocationResult) (operand: LIR.Operand) (tempReg: LIR.PhysReg)
    : LIR.Operand * LIR.Instr list =
    match operand with
    | LIR.Imm n -> (LIR.Imm n, [])
    | LIR.FloatImm f -> (LIR.FloatImm f, [])
    | LIR.StringSymbol value -> (LIR.StringSymbol value, [])
    | LIR.FloatSymbol value -> (LIR.FloatSymbol value, [])
    | LIR.StackSlot s -> (LIR.StackSlot s, [])
    | LIR.Reg reg ->
        match reg with
        | LIR.Physical p -> (LIR.Reg (LIR.Physical p), [])
        | LIR.Virtual id ->
            match tryAllocation allocation id with
            | Some (PhysReg physReg) -> (LIR.Reg (LIR.Physical physReg), [])
            | Some (StackSlot offset) ->
                let loadInstr = LIR.Mov (LIR.Physical tempReg, LIR.StackSlot offset)
                (LIR.Reg (LIR.Physical tempReg), [loadInstr])
            // Keep Virtual unchanged if not in integer mapping - it may be a float register
            // that will be handled by float allocation later
            | None -> (LIR.Reg (LIR.Virtual id), [])
    | LIR.FuncAddr name -> (LIR.FuncAddr name, [])

/// Apply allocation to an operand WITHOUT generating load instructions for spills.
/// Returns StackSlot for spilled values so CodeGen can load them at the right time.
/// Used for TailArgMoves where loads must be deferred to avoid using the same temp register.
let applyToOperandNoLoad (allocation: AllocationResult) (operand: LIR.Operand) : LIR.Operand =
    match operand with
    | LIR.Imm n -> LIR.Imm n
    | LIR.FloatImm f -> LIR.FloatImm f
    | LIR.StringSymbol value -> LIR.StringSymbol value
    | LIR.FloatSymbol value -> LIR.FloatSymbol value
    | LIR.StackSlot s -> LIR.StackSlot s
    | LIR.Reg reg ->
        match reg with
        | LIR.Physical p -> LIR.Reg (LIR.Physical p)
        | LIR.Virtual id ->
            match tryAllocation allocation id with
            | Some (PhysReg physReg) -> LIR.Reg (LIR.Physical physReg)
            | Some (StackSlot offset) -> LIR.StackSlot offset
            // Keep Virtual unchanged if not in integer mapping - it may be a float register
            | None -> LIR.Reg (LIR.Virtual id)
    | LIR.FuncAddr name -> LIR.FuncAddr name

/// Helper to load a spilled register
let loadSpilled (allocation: AllocationResult) (reg: LIR.Reg) (tempReg: LIR.PhysReg)
    : LIR.Reg * LIR.Instr list =
    match reg with
    | LIR.Physical p -> (LIR.Physical p, [])
    | LIR.Virtual id ->
        match tryAllocation allocation id with
        | Some (PhysReg physReg) -> (LIR.Physical physReg, [])
        | Some (StackSlot offset) ->
            let loadInstr = LIR.Mov (LIR.Physical tempReg, LIR.StackSlot offset)
            (LIR.Physical tempReg, [loadInstr])
        | None -> (LIR.Physical tempReg, [])

/// On x86_64, X8-X17 all alias to R11 (scratch). Using X12 and X13 as distinct
/// scratch registers for loading two spilled operands simultaneously will clobber
/// the first load when the second executes. On x86_64, the second operand of binary
/// ops uses applyToOperandNoLoad to keep it as a StackSlot, and the codegen handles
/// loading it into R11 after the first operand has been moved to the destination.
let internal isX86_64 (arch: Platform.Arch) =
    match arch with Platform.X86_64 -> true | Platform.ARM64 -> false

let internal aliasesX86ScratchReg (reg: LIR.PhysReg) : bool =
    match reg with
    | LIR.X8 | LIR.X9 | LIR.X10 | LIR.X11 | LIR.X12
    | LIR.X13 | LIR.X14 | LIR.X15 | LIR.X16 | LIR.X17 -> true
    | _ -> false

let internal x86SpillTempExcluding (excluded: LIR.Reg list) : LIR.PhysReg =
    let excludedPhysical =
        excluded
        |> List.choose (function
            | LIR.Physical reg -> Some reg
            | LIR.Virtual _ -> None)
        |> Set.ofList
    [ LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7
      LIR.X19; LIR.X20; LIR.X21; LIR.X0; LIR.X1; LIR.X2 ]
    |> List.tryFind (fun reg -> not (Set.contains reg excludedPhysical))
    |> function
        | Some reg -> reg
        | None -> Crash.crash "x86_64 spill repair could not find a preserved temporary register"

/// On x86_64, when loading two spilled Reg-typed operands, the first must go to a
/// register that won't be clobbered by the second load (into X12=R11). This function
/// picks a safe register by checking what physical register the right operand uses.
/// If both left and right are spilled to stack, loads left into dest register
/// (unless dest conflicts with right's allocated register).
let internal loadSpilledPair (arch: Platform.Arch) (mapping: AllocationResult) (left: LIR.Reg) (right: LIR.Reg) (destReg: LIR.Reg)
    : (LIR.Reg * LIR.Instr list) * (LIR.Reg * LIR.Instr list) =
    if not (isX86_64 arch) then
        (loadSpilled mapping left LIR.X12, loadSpilled mapping right LIR.X13)
    else
        // Check if left is actually spilled (StackSlot)
        let leftIsSpilled =
            match left with
            | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
            | _ -> false
        let rightIsSpilled =
            match right with
            | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
            | _ -> false
        if leftIsSpilled && rightIsSpilled then
            // Both spilled: load left into dest, right into X12
            let destPhys =
                match destReg with
                | LIR.Physical p -> p
                | LIR.Virtual id ->
                    Crash.crash $"loadSpilledPair: destination vreg {id} was not allocated before x86_64 spill repair"
            // Check that dest doesn't also alias R11
            let leftTemp =
                if aliasesX86ScratchReg destPhys then
                    Crash.crash $"loadSpilledPair: destination register {destPhys} aliases x86_64 scratch register R11"
                else
                    destPhys
            (loadSpilled mapping left leftTemp, loadSpilled mapping right LIR.X12)
        elif leftIsSpilled then
            // Only left is spilled; the right operand remains in place.
            (loadSpilled mapping left LIR.X12, loadSpilled mapping right LIR.X12)
        else
            // Right spilled or neither: use X12 for left, X12 for right (OK since left isn't spilled)
            (loadSpilled mapping left LIR.X12, loadSpilled mapping right LIR.X12)
