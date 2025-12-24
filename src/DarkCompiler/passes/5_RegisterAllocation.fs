// 5_RegisterAllocation.fs - Register Allocation (Pass 5)
//
// Allocates physical ARM64 registers to virtual registers in LIR CFG.
//
// Simple greedy allocation algorithm:
// 1. Collect all virtual register IDs from all blocks in CFG
// 2. Sort virtual registers by ID
// 3. Assign physical registers in order: X1, X2, X3, ..., X15
// 4. Transform all blocks to use assigned physical registers
//
// Limitations: No spilling (fails if >15 registers needed), no liveness analysis
// Note: X0 reserved for return values per ARM64 calling convention

module RegisterAllocation

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

/// Available physical registers for allocation (avoiding X0 which is for return)
let availableRegs = [
    LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7; LIR.X8
    LIR.X9; LIR.X10; LIR.X11; LIR.X12; LIR.X13; LIR.X14; LIR.X15
]

/// Create allocation map for virtual registers
let createAllocation (virtualRegs: Set<int>) : Map<int, LIR.PhysReg> =
    let sorted = Set.toList virtualRegs |> List.sort
    let regsToUse = List.truncate sorted.Length availableRegs
    List.zip sorted regsToUse
    |> Map.ofList

/// Apply allocation to a register
let applyToReg (allocation: Map<int, LIR.PhysReg>) (reg: LIR.Reg) : LIR.Reg =
    match reg with
    | LIR.Physical p -> LIR.Physical p
    | LIR.Virtual id ->
        match Map.tryFind id allocation with
        | Some physReg -> LIR.Physical physReg
        | None -> LIR.Physical LIR.X1  // Fallback (shouldn't happen)

/// Apply allocation to an operand
let applyToOperand (allocation: Map<int, LIR.PhysReg>) (operand: LIR.Operand) : LIR.Operand =
    match operand with
    | LIR.Imm n -> LIR.Imm n
    | LIR.Reg reg -> LIR.Reg (applyToReg allocation reg)
    | LIR.StackSlot s -> LIR.StackSlot s

/// Apply allocation to an instruction
let applyAllocation (allocation: Map<int, LIR.PhysReg>) (instr: LIR.Instr) : LIR.Instr =
    match instr with
    | LIR.Mov (dest, src) ->
        LIR.Mov (applyToReg allocation dest, applyToOperand allocation src)
    | LIR.Add (dest, left, right) ->
        LIR.Add (applyToReg allocation dest, applyToReg allocation left, applyToOperand allocation right)
    | LIR.Sub (dest, left, right) ->
        LIR.Sub (applyToReg allocation dest, applyToReg allocation left, applyToOperand allocation right)
    | LIR.Mul (dest, left, right) ->
        LIR.Mul (applyToReg allocation dest, applyToReg allocation left, applyToReg allocation right)
    | LIR.Sdiv (dest, left, right) ->
        LIR.Sdiv (applyToReg allocation dest, applyToReg allocation left, applyToReg allocation right)
    | LIR.Cmp (left, right) ->
        LIR.Cmp (applyToReg allocation left, applyToOperand allocation right)
    | LIR.Cset (dest, cond) ->
        LIR.Cset (applyToReg allocation dest, cond)
    | LIR.And (dest, left, right) ->
        LIR.And (applyToReg allocation dest, applyToReg allocation left, applyToReg allocation right)
    | LIR.Orr (dest, left, right) ->
        LIR.Orr (applyToReg allocation dest, applyToReg allocation left, applyToReg allocation right)
    | LIR.Mvn (dest, src) ->
        LIR.Mvn (applyToReg allocation dest, applyToReg allocation src)
    | LIR.PrintInt reg ->
        LIR.PrintInt (applyToReg allocation reg)
    | LIR.PrintBool reg ->
        LIR.PrintBool (applyToReg allocation reg)

/// Apply allocation to a terminator
let applyAllocToTerm (allocation: Map<int, LIR.PhysReg>) (term: LIR.Terminator) : LIR.Terminator =
    match term with
    | LIR.Ret -> LIR.Ret
    | LIR.Branch (cond, trueLabel, falseLabel) ->
        LIR.Branch (applyToReg allocation cond, trueLabel, falseLabel)
    | LIR.Jump label -> LIR.Jump label

/// Apply allocation to a basic block
let applyAllocToBlock (allocation: Map<int, LIR.PhysReg>) (block: LIR.BasicBlock) : LIR.BasicBlock =
    {
        LIR.Label = block.Label
        LIR.Instrs = block.Instrs |> List.map (applyAllocation allocation)
        LIR.Terminator = applyAllocToTerm allocation block.Terminator
    }

/// Apply allocation to CFG
let applyAllocToCFG (allocation: Map<int, LIR.PhysReg>) (cfg: LIR.CFG) : LIR.CFG =
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

    // Create allocation map
    let allocation = createAllocation virtualRegs

    // Apply allocation to CFG
    let allocatedCFG = applyAllocToCFG allocation func.CFG

    {
        LIR.Name = func.Name
        LIR.CFG = allocatedCFG
        LIR.StackSize = 0  // No spilling yet
    }
