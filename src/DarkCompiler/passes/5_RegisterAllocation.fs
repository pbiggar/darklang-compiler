// 5_RegisterAllocation.fs - Register Allocation (Pass 5)
//
// Allocates physical ARM64 registers to virtual registers in LIR.
//
// Simple greedy allocation algorithm:
// 1. Collect all virtual register IDs from instructions
// 2. Sort virtual registers by ID
// 3. Assign physical registers in order: X1, X2, X3, ..., X15
// 4. Transform all instructions to use assigned physical registers
//
// Limitations: No spilling (fails if >15 registers needed)
// Note: X0 reserved for return values per ARM64 calling convention
//
// Example:
//   v0 <- Mov(Imm 42); v1 <- Add(v0, Imm 5)
//   →
//   X1 <- Mov(Imm 42); X2 <- Add(X1, Imm 5)

module RegisterAllocation

/// Collect all virtual registers used in instructions
let collectVirtualRegs (instrs: LIR.Instr list) : Set<int> =
    let rec collect (instr: LIR.Instr) (acc: Set<int>) : Set<int> =
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
        | _ -> acc

    instrs |> List.fold (fun acc instr -> collect instr acc) Set.empty

/// Available physical registers for allocation (avoiding X0 which is for return)
let availableRegs = [
    LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7; LIR.X8
    LIR.X9; LIR.X10; LIR.X11; LIR.X12; LIR.X13; LIR.X14; LIR.X15
]

/// Create allocation map for virtual registers
let createAllocation (virtualRegs: Set<int>) : Map<int, LIR.PhysReg> =
    let sorted = Set.toList virtualRegs |> List.sort
    // Pair up virtual registers with physical registers
    // If we have more virtual than physical registers, truncate physical
    // If we have fewer virtual, truncate availableRegs
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
    | LIR.PrintInt reg ->
        LIR.PrintInt (applyToReg allocation reg)
    | LIR.Ret -> LIR.Ret

/// Register allocation
let allocateRegisters (func: LIR.Function) : LIR.AllocResult =
    // Collect virtual registers
    let virtualRegs = collectVirtualRegs func.Body

    // Create allocation map
    let allocation = createAllocation virtualRegs

    // Apply allocation to all instructions
    let allocatedInstrs = func.Body |> List.map (applyAllocation allocation)

    {
        Instrs = allocatedInstrs
        StackSize = 0  // No spilling yet
    } : LIR.AllocResult
