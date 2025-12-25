// 5_RegisterAllocation.fs - Register Allocation (Pass 5)
//
// Allocates physical ARM64 registers to virtual registers in LIR CFG.
//
// Naive spilling allocation algorithm:
// 1. Collect all virtual register IDs from all blocks in CFG
// 2. If <= 10 vregs: use X1-X10 (caller-saved, no save/restore overhead)
// 3. If 11-18 vregs: use X1-X10 + X19-X28 (callee-saved, need save/restore)
// 4. If > 18 vregs: use X1-X10 + X19-X28, spill remainder to stack
// 5. Track callee-saved register usage for prologue/epilogue generation
//
// Note: X0 reserved for return values per ARM64 calling convention
// Stack slots are negative offsets from FP (X29): 0, -8, -16, -24, ...

module RegisterAllocation

/// Result of register allocation
type AllocationResult = {
    Mapping: Map<int, Allocation>  // VReg ID -> allocation
    StackSize: int                  // Total stack space for spills (16-byte aligned)
    UsedCalleeSaved: LIR.PhysReg list  // Callee-saved registers used
}

/// Allocation target for a virtual register
and Allocation =
    | PhysReg of LIR.PhysReg
    | StackSlot of int  // Byte offset from FP (negative)

/// Collect all virtual registers used in an instruction
let rec collectVRegsInstr (instr: LIR.Instr) (acc: Set<int>) : Set<int> =
    match instr with
    | LIR.Mov (LIR.Virtual id, _) -> Set.add id acc
    | LIR.Mov (_, LIR.Reg (LIR.Virtual id)) -> Set.add id acc
    | LIR.Add (LIR.Virtual id, LIR.Virtual leftId, LIR.Reg (LIR.Virtual rightId)) ->
        acc |> Set.add id |> Set.add leftId |> Set.add rightId
    | LIR.Add (LIR.Virtual id, LIR.Virtual leftId, _) -> acc |> Set.add id |> Set.add leftId
    | LIR.Add (LIR.Virtual id, _, LIR.Reg (LIR.Virtual rightId)) -> acc |> Set.add id |> Set.add rightId
    | LIR.Add (LIR.Virtual id, _, _) -> Set.add id acc
    | LIR.Sub (LIR.Virtual id, LIR.Virtual leftId, LIR.Reg (LIR.Virtual rightId)) ->
        acc |> Set.add id |> Set.add leftId |> Set.add rightId
    | LIR.Sub (LIR.Virtual id, LIR.Virtual leftId, _) -> acc |> Set.add id |> Set.add leftId
    | LIR.Sub (LIR.Virtual id, _, LIR.Reg (LIR.Virtual rightId)) -> acc |> Set.add id |> Set.add rightId
    | LIR.Sub (LIR.Virtual id, _, _) -> Set.add id acc
    | LIR.Mul (LIR.Virtual id, LIR.Virtual left, LIR.Virtual right) ->
        acc |> Set.add id |> Set.add left |> Set.add right
    | LIR.Mul (LIR.Virtual id, LIR.Virtual left, _) -> acc |> Set.add id |> Set.add left
    | LIR.Mul (LIR.Virtual id, _, LIR.Virtual right) -> acc |> Set.add id |> Set.add right
    | LIR.Mul (LIR.Virtual id, _, _) -> Set.add id acc
    | LIR.Sdiv (LIR.Virtual id, LIR.Virtual left, LIR.Virtual right) ->
        acc |> Set.add id |> Set.add left |> Set.add right
    | LIR.Sdiv (LIR.Virtual id, LIR.Virtual left, _) -> acc |> Set.add id |> Set.add left
    | LIR.Sdiv (LIR.Virtual id, _, LIR.Virtual right) -> acc |> Set.add id |> Set.add right
    | LIR.Sdiv (LIR.Virtual id, _, _) -> Set.add id acc
    | LIR.Cmp (LIR.Virtual leftId, LIR.Reg (LIR.Virtual rightId)) ->
        acc |> Set.add leftId |> Set.add rightId
    | LIR.Cmp (LIR.Virtual leftId, _) -> Set.add leftId acc
    | LIR.Cset (LIR.Virtual id, _) -> Set.add id acc
    | LIR.And (LIR.Virtual id, LIR.Virtual left, LIR.Virtual right) ->
        acc |> Set.add id |> Set.add left |> Set.add right
    | LIR.And (LIR.Virtual id, LIR.Virtual left, _) -> acc |> Set.add id |> Set.add left
    | LIR.And (LIR.Virtual id, _, LIR.Virtual right) -> acc |> Set.add id |> Set.add right
    | LIR.And (LIR.Virtual id, _, _) -> Set.add id acc
    | LIR.Orr (LIR.Virtual id, LIR.Virtual left, LIR.Virtual right) ->
        acc |> Set.add id |> Set.add left |> Set.add right
    | LIR.Orr (LIR.Virtual id, LIR.Virtual left, _) -> acc |> Set.add id |> Set.add left
    | LIR.Orr (LIR.Virtual id, _, LIR.Virtual right) -> acc |> Set.add id |> Set.add right
    | LIR.Orr (LIR.Virtual id, _, _) -> Set.add id acc
    | LIR.Mvn (LIR.Virtual id, LIR.Virtual src) -> acc |> Set.add id |> Set.add src
    | LIR.Mvn (LIR.Virtual id, _) -> Set.add id acc
    | LIR.PrintInt (LIR.Virtual id) -> Set.add id acc
    | LIR.PrintBool (LIR.Virtual id) -> Set.add id acc
    | _ -> acc

/// Collect virtual registers from a terminator
let collectVRegsTerm (term: LIR.Terminator) (acc: Set<int>) : Set<int> =
    match term with
    | LIR.Ret -> acc
    | LIR.Branch (LIR.Virtual id, _, _) -> Set.add id acc
    | LIR.Branch (_, _, _) -> acc
    | LIR.Jump _ -> acc

/// Collect virtual registers from a basic block
let collectVRegsBlock (block: LIR.BasicBlock) (acc: Set<int>) : Set<int> =
    let acc1 = block.Instrs |> List.fold (fun a instr -> collectVRegsInstr instr a) acc
    collectVRegsTerm block.Terminator acc1

/// Collect all virtual registers from CFG
let collectVirtualRegs (cfg: LIR.CFG) : Set<int> =
    cfg.Blocks
    |> Map.toList
    |> List.fold (fun acc (_, block) -> collectVRegsBlock block acc) Set.empty

/// Caller-saved registers (no save/restore needed in prologue/epilogue)
let callerSavedRegs = [
    LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5
    LIR.X6; LIR.X7; LIR.X8; LIR.X9; LIR.X10
]

/// Callee-saved registers (must be saved/restored in prologue/epilogue)
/// Note: X19-X28 per AAPCS64. We exclude X29 (FP) and X30 (LR) as they're special
/// TODO: Add X19-X28 when LIR.PhysReg supports them
let calleeSavedRegs: LIR.PhysReg list = []

/// Helper: Align size to 16-byte boundary
let alignTo16 (size: int) : int =
    ((size + 15) / 16) * 16

/// Create allocation result for virtual registers
/// paramVRegs: list of VReg IDs that are function parameters
///             Parameters are assigned to caller-saved registers (X1-X10) because X0 is clobbered
///             by function call return values. The code generator handles moving from X0-X7 at entry.
let createAllocation (virtualRegs: Set<int>) (paramVRegs: int list) : AllocationResult =
    let sorted = Set.toList virtualRegs |> List.sort
    let numVRegs = List.length sorted

    if numVRegs = 0 then
        {
            Mapping = Map.empty
            StackSize = 0
            UsedCalleeSaved = []
        }
    else if numVRegs <= 10 then
        // Use caller-saved registers (X1-X10) for all virtual registers
        // This keeps parameters safe from call clobbering (since X0 is return value)
        let mapping =
            List.zip sorted (List.take numVRegs callerSavedRegs)
            |> List.map (fun (vregId, physReg) -> (vregId, PhysReg physReg))
            |> Map.ofList
        {
            Mapping = mapping
            StackSize = 0
            UsedCalleeSaved = []
        }
    else
        // Need to spill some vregs to stack
        let numToSpill = numVRegs - List.length callerSavedRegs
        let (inRegs, toSpill) = List.splitAt (List.length callerSavedRegs) sorted

        let regMapping =
            List.zip inRegs callerSavedRegs
            |> List.map (fun (vregId, physReg) -> (vregId, PhysReg physReg))

        let stackMapping =
            toSpill
            |> List.mapi (fun i vregId -> (vregId, StackSlot (-(i + 1) * 8)))

        let allMapping = (regMapping @ stackMapping) |> Map.ofList
        let stackSize = alignTo16 (numToSpill * 8)

        {
            Mapping = allMapping
            StackSize = stackSize
            UsedCalleeSaved = []
        }

/// Apply allocation to a register
/// Returns either a physical register or None if allocated to stack
let applyToReg (allocation: Map<int, Allocation>) (reg: LIR.Reg) : LIR.Reg option =
    match reg with
    | LIR.Physical p -> Some (LIR.Physical p)
    | LIR.Virtual id ->
        match Map.tryFind id allocation with
        | Some (PhysReg physReg) -> Some (LIR.Physical physReg)
        | Some (StackSlot _) -> None  // Allocated to stack
        | None -> Some (LIR.Physical LIR.X1)  // Fallback (shouldn't happen)

/// Apply allocation to an operand
/// Stack-allocated vregs become StackSlot operands
let applyToOperand (allocation: Map<int, Allocation>) (operand: LIR.Operand) : LIR.Operand =
    match operand with
    | LIR.Imm n -> LIR.Imm n
    | LIR.Reg reg ->
        match reg with
        | LIR.Physical p -> LIR.Reg (LIR.Physical p)
        | LIR.Virtual id ->
            match Map.tryFind id allocation with
            | Some (PhysReg physReg) -> LIR.Reg (LIR.Physical physReg)
            | Some (StackSlot offset) -> LIR.StackSlot offset
            | None -> LIR.Reg (LIR.Physical LIR.X1)  // Fallback
    | LIR.StackSlot s -> LIR.StackSlot s

/// Apply allocation to an instruction
/// Returns a list of instructions (may include store for spilled destinations)
/// Note: For destinations allocated to stack, uses X11 as temp, then stores to stack
let applyAllocation (allocation: Map<int, Allocation>) (instr: LIR.Instr) : LIR.Instr list =
    // Helper to get a register from option, using X11 as fallback for stack-allocated dests
    let regOrTemp (regOpt: LIR.Reg option) : LIR.Reg =
        match regOpt with
        | Some r -> r
        | None -> LIR.Physical LIR.X11  // Temp for stack-allocated destinations

    // Helper to check if a destination needs a store after computation
    let needsStore (dest: LIR.Reg) : LIR.Operand option =
        match dest with
        | LIR.Virtual id ->
            match Map.tryFind id allocation with
            | Some (StackSlot offset) -> Some (LIR.StackSlot offset)
            | _ -> None
        | _ -> None

    // Helper to generate store instruction if needed
    let storeIfNeeded (dest: LIR.Reg) : LIR.Instr list =
        match needsStore dest with
        | Some (LIR.StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
        | _ -> []

    // Helper to load a register from stack if needed
    // Returns (register to use, load instructions)
    // Uses X12, X13 as temps for loading stack slots
    let loadIfNeeded (reg: LIR.Reg) (tempReg: LIR.PhysReg) : LIR.Reg * LIR.Instr list =
        match reg with
        | LIR.Virtual id ->
            match Map.tryFind id allocation with
            | Some (StackSlot offset) ->
                // Load from stack into temp register
                (LIR.Physical tempReg, [LIR.Mov (LIR.Physical tempReg, LIR.StackSlot offset)])
            | Some (PhysReg physReg) ->
                (LIR.Physical physReg, [])
            | None ->
                (LIR.Physical LIR.X1, [])  // Fallback
        | LIR.Physical p ->
            (LIR.Physical p, [])

    let (rewritten, preLoads) =
        match instr with
        | LIR.Mov (dest, src) ->
            (LIR.Mov (regOrTemp (applyToReg allocation dest), applyToOperand allocation src), [])
        | LIR.Store (offset, src) ->
            (LIR.Store (offset, regOrTemp (applyToReg allocation src)), [])
        | LIR.Add (dest, left, right) ->
            (LIR.Add (regOrTemp (applyToReg allocation dest), regOrTemp (applyToReg allocation left), applyToOperand allocation right), [])
        | LIR.Sub (dest, left, right) ->
            (LIR.Sub (regOrTemp (applyToReg allocation dest), regOrTemp (applyToReg allocation left), applyToOperand allocation right), [])
        | LIR.Mul (dest, left, right) ->
            // Mul requires all operands in registers, load from stack if needed
            let (leftReg, leftLoads) = loadIfNeeded left LIR.X12
            let (rightReg, rightLoads) = loadIfNeeded right LIR.X13
            (LIR.Mul (regOrTemp (applyToReg allocation dest), leftReg, rightReg), leftLoads @ rightLoads)
        | LIR.Sdiv (dest, left, right) ->
            // Sdiv requires all operands in registers, load from stack if needed
            let (leftReg, leftLoads) = loadIfNeeded left LIR.X12
            let (rightReg, rightLoads) = loadIfNeeded right LIR.X13
            (LIR.Sdiv (regOrTemp (applyToReg allocation dest), leftReg, rightReg), leftLoads @ rightLoads)
        | LIR.Cmp (left, right) ->
            (LIR.Cmp (regOrTemp (applyToReg allocation left), applyToOperand allocation right), [])
        | LIR.Cset (dest, cond) ->
            (LIR.Cset (regOrTemp (applyToReg allocation dest), cond), [])
        | LIR.And (dest, left, right) ->
            // And requires all operands in registers, load from stack if needed
            let (leftReg, leftLoads) = loadIfNeeded left LIR.X12
            let (rightReg, rightLoads) = loadIfNeeded right LIR.X13
            (LIR.And (regOrTemp (applyToReg allocation dest), leftReg, rightReg), leftLoads @ rightLoads)
        | LIR.Orr (dest, left, right) ->
            // Orr requires all operands in registers, load from stack if needed
            let (leftReg, leftLoads) = loadIfNeeded left LIR.X12
            let (rightReg, rightLoads) = loadIfNeeded right LIR.X13
            (LIR.Orr (regOrTemp (applyToReg allocation dest), leftReg, rightReg), leftLoads @ rightLoads)
        | LIR.Mvn (dest, src) ->
            // Mvn requires all operands in registers, load from stack if needed
            let (srcReg, srcLoads) = loadIfNeeded src LIR.X12
            (LIR.Mvn (regOrTemp (applyToReg allocation dest), srcReg), srcLoads)
        | LIR.Call (dest, funcName, args) ->
            (LIR.Call (regOrTemp (applyToReg allocation dest), funcName, List.map (applyToOperand allocation) args), [])
        | LIR.SaveRegs ->
            (LIR.SaveRegs, [])
        | LIR.RestoreRegs ->
            (LIR.RestoreRegs, [])
        | LIR.PrintInt reg ->
            (LIR.PrintInt (regOrTemp (applyToReg allocation reg)), [])
        | LIR.PrintBool reg ->
            (LIR.PrintBool (regOrTemp (applyToReg allocation reg)), [])

    // Build final instruction sequence: preLoads + rewritten + postStores
    let postStores =
        match instr with
        | LIR.Mov (dest, _)
        | LIR.Add (dest, _, _)
        | LIR.Sub (dest, _, _)
        | LIR.Mul (dest, _, _)
        | LIR.Sdiv (dest, _, _)
        | LIR.Cset (dest, _)
        | LIR.And (dest, _, _)
        | LIR.Orr (dest, _, _)
        | LIR.Mvn (dest, _)
        | LIR.Call (dest, _, _) ->
            storeIfNeeded dest
        | _ -> []

    preLoads @ [rewritten] @ postStores

/// Apply allocation to a terminator
let applyAllocToTerm (allocation: Map<int, Allocation>) (term: LIR.Terminator) : LIR.Terminator =
    let regOrTemp (regOpt: LIR.Reg option) : LIR.Reg =
        match regOpt with
        | Some r -> r
        | None -> LIR.Physical LIR.X11

    match term with
    | LIR.Ret -> LIR.Ret
    | LIR.Branch (cond, trueLabel, falseLabel) ->
        LIR.Branch (regOrTemp (applyToReg allocation cond), trueLabel, falseLabel)
    | LIR.Jump label -> LIR.Jump label

/// Apply allocation to a basic block
let applyAllocToBlock (allocation: Map<int, Allocation>) (block: LIR.BasicBlock) : LIR.BasicBlock =
    {
        LIR.Label = block.Label
        LIR.Instrs = block.Instrs |> List.collect (applyAllocation allocation)  // Changed to collect since applyAllocation returns list
        LIR.Terminator = applyAllocToTerm allocation block.Terminator
    }

/// Apply allocation to CFG
let applyAllocToCFG (allocation: Map<int, Allocation>) (cfg: LIR.CFG) : LIR.CFG =
    let allocatedBlocks =
        cfg.Blocks
        |> Map.map (fun _ block -> applyAllocToBlock allocation block)

    {
        LIR.Entry = cfg.Entry
        LIR.Blocks = allocatedBlocks
    }

/// Register allocation
let allocateRegisters (func: LIR.Function) : LIR.Function =
    // Collect virtual registers from all blocks
    let virtualRegs = collectVirtualRegs func.CFG

    // Get parameter VReg IDs (these should be pre-assigned to X0, X1, etc.)
    let paramVRegIds =
        func.Params
        |> List.choose (fun reg ->
            match reg with
            | LIR.Virtual id -> Some id
            | LIR.Physical _ -> None)  // Already physical, skip

    // Create allocation result with param pre-assignment
    let allocationResult = createAllocation virtualRegs paramVRegIds

    // Apply allocation to CFG
    let allocatedCFG = applyAllocToCFG allocationResult.Mapping func.CFG

    // Apply allocation to parameters
    let allocatedParams =
        func.Params
        |> List.map (fun reg ->
            match applyToReg allocationResult.Mapping reg with
            | Some allocatedReg -> allocatedReg
            | None -> reg)  // If not allocated (shouldn't happen), keep original

    {
        LIR.Name = func.Name
        LIR.Params = allocatedParams
        LIR.CFG = allocatedCFG
        LIR.StackSize = allocationResult.StackSize
        LIR.UsedCalleeSaved = allocationResult.UsedCalleeSaved
    }
