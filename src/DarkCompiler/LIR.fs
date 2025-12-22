module LIR

/// ARM64 general-purpose registers
type PhysReg =
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
    | X10 | X11 | X12 | X13 | X14 | X15
    | X29  // Frame pointer
    | X30  // Link register
    | SP   // Stack pointer

/// Register or virtual register (before allocation)
type Reg =
    | Physical of PhysReg
    | Virtual of int

/// Operands
type Operand =
    | Imm of int64           // Immediate value
    | Reg of Reg             // Register
    | StackSlot of int       // Stack offset (for spills)

/// Instructions (closer to ARM64 instructions)
type Instr =
    | Mov of dest:Reg * src:Operand
    | Add of dest:Reg * left:Reg * right:Operand
    | Sub of dest:Reg * left:Reg * right:Operand
    | Mul of dest:Reg * left:Reg * right:Reg
    | Sdiv of dest:Reg * left:Reg * right:Reg  // Signed division
    | Ret

/// Function (preparation for future functions)
type Function = {
    Name: string
    Body: Instr list
    StackSize: int  // Bytes needed for spills
}

/// LIR program
type Program = Program of Function list

/// Live range for a virtual register
type LiveRange = {
    Start: int
    End: int
}

/// Allocation result
type Allocation =
    | Register of PhysReg
    | Spill of int  // Stack slot offset

/// Register allocation result
type AllocResult = {
    Instrs: Instr list
    StackSize: int
}

/// Convert MIR.VReg to LIR.Reg (virtual)
let vregToLIRReg (MIR.VReg id) : Reg = Virtual id

/// Convert MIR.Operand to LIR.Operand
let convertOperand (operand: MIR.Operand) : Operand =
    match operand with
    | MIR.IntConst n -> Imm n
    | MIR.Register vreg -> Reg (vregToLIRReg vreg)

/// Ensure operand is in a register (may need to load immediate)
let ensureInRegister (operand: MIR.Operand) (tempReg: Reg) : Instr list * Reg =
    match operand with
    | MIR.IntConst n ->
        // Need to load constant into a temporary register
        ([Mov (tempReg, Imm n)], tempReg)
    | MIR.Register vreg ->
        ([], vregToLIRReg vreg)

/// Convert MIR instruction to LIR instructions
let selectInstr (instr: MIR.Instr) : Instr list =
    match instr with
    | MIR.Mov (dest, src) ->
        let lirDest = vregToLIRReg dest
        let lirSrc = convertOperand src
        [Mov (lirDest, lirSrc)]

    | MIR.BinOp (dest, op, left, right) ->
        let lirDest = vregToLIRReg dest
        let leftOp = convertOperand left
        let rightOp = convertOperand right

        match op with
        | MIR.Add ->
            // ADD can have immediate or register as right operand
            // Left operand must be in a register
            let (leftInstrs, leftReg) = ensureInRegister left lirDest
            leftInstrs @ [Add (lirDest, leftReg, rightOp)]

        | MIR.Sub ->
            // SUB can have immediate or register as right operand
            let (leftInstrs, leftReg) = ensureInRegister left lirDest
            leftInstrs @ [Sub (lirDest, leftReg, rightOp)]

        | MIR.Mul ->
            // MUL requires both operands in registers
            let (leftInstrs, leftReg) = ensureInRegister left (Virtual 1000)
            let (rightInstrs, rightReg) = ensureInRegister right (Virtual 1001)
            leftInstrs @ rightInstrs @ [Mul (lirDest, leftReg, rightReg)]

        | MIR.Div ->
            // SDIV requires both operands in registers
            let (leftInstrs, leftReg) = ensureInRegister left (Virtual 1000)
            let (rightInstrs, rightReg) = ensureInRegister right (Virtual 1001)
            leftInstrs @ rightInstrs @ [Sdiv (lirDest, leftReg, rightReg)]

    | MIR.Ret operand ->
        // ARM64 returns value in X0
        let lirOp = convertOperand operand
        [
            Mov (Physical X0, lirOp)
            Ret
        ]

/// Convert MIR block to LIR instructions
let selectBlock (MIR.Block instrs) : Instr list =
    instrs |> List.collect selectInstr

/// Convert MIR program to LIR
let toLIR (program: MIR.Program) : Program =
    let (MIR.Program blocks) = program

    // For now, single function with all blocks
    let instrs = blocks |> List.collect selectBlock

    let func = {
        Name = "_start"
        Body = instrs
        StackSize = 0  // Will be determined by register allocation
    }

    Program [func]

/// Collect all virtual registers used in instructions
let collectVirtualRegs (instrs: Instr list) : Set<int> =
    let rec collect (instr: Instr) (acc: Set<int>) : Set<int> =
        match instr with
        | Mov (Virtual id, _) -> Set.add id acc
        | Mov (_, Reg (Virtual id)) -> Set.add id acc
        | Add (Virtual id, Virtual leftId, Reg (Virtual rightId)) ->
            acc |> Set.add id |> Set.add leftId |> Set.add rightId
        | Add (Virtual id, Virtual leftId, _) -> acc |> Set.add id |> Set.add leftId
        | Add (Virtual id, _, Reg (Virtual rightId)) -> acc |> Set.add id |> Set.add rightId
        | Add (Virtual id, _, _) -> Set.add id acc
        | Sub (Virtual id, Virtual leftId, Reg (Virtual rightId)) ->
            acc |> Set.add id |> Set.add leftId |> Set.add rightId
        | Sub (Virtual id, Virtual leftId, _) -> acc |> Set.add id |> Set.add leftId
        | Sub (Virtual id, _, Reg (Virtual rightId)) -> acc |> Set.add id |> Set.add rightId
        | Sub (Virtual id, _, _) -> Set.add id acc
        | Mul (Virtual id, Virtual left, Virtual right) ->
            acc |> Set.add id |> Set.add left |> Set.add right
        | Mul (Virtual id, Virtual left, _) -> acc |> Set.add id |> Set.add left
        | Mul (Virtual id, _, Virtual right) -> acc |> Set.add id |> Set.add right
        | Mul (Virtual id, _, _) -> Set.add id acc
        | Sdiv (Virtual id, Virtual left, Virtual right) ->
            acc |> Set.add id |> Set.add left |> Set.add right
        | Sdiv (Virtual id, Virtual left, _) -> acc |> Set.add id |> Set.add left
        | Sdiv (Virtual id, _, Virtual right) -> acc |> Set.add id |> Set.add right
        | Sdiv (Virtual id, _, _) -> Set.add id acc
        | _ -> acc

    instrs |> List.fold (fun acc instr -> collect instr acc) Set.empty

/// Available physical registers for allocation (avoiding X0 which is for return)
let availableRegs = [X1; X2; X3; X4; X5; X6; X7; X8; X9; X10; X11; X12; X13; X14; X15]

/// Create allocation map for virtual registers
let createAllocation (virtualRegs: Set<int>) : Map<int, PhysReg> =
    let sorted = Set.toList virtualRegs |> List.sort
    // Pair up virtual registers with physical registers
    // If we have more virtual than physical registers, truncate physical
    // If we have fewer virtual, truncate availableRegs
    let regsToUse = List.truncate sorted.Length availableRegs
    List.zip sorted regsToUse
    |> Map.ofList

/// Apply allocation to a register
let applyToReg (allocation: Map<int, PhysReg>) (reg: Reg) : Reg =
    match reg with
    | Physical p -> Physical p
    | Virtual id ->
        match Map.tryFind id allocation with
        | Some physReg -> Physical physReg
        | None -> Physical X1  // Fallback (shouldn't happen)

/// Apply allocation to an operand
let applyToOperand (allocation: Map<int, PhysReg>) (operand: Operand) : Operand =
    match operand with
    | Imm n -> Imm n
    | Reg reg -> Reg (applyToReg allocation reg)
    | StackSlot s -> StackSlot s

/// Apply allocation to an instruction
let applyAllocation (allocation: Map<int, PhysReg>) (instr: Instr) : Instr =
    match instr with
    | Mov (dest, src) ->
        Mov (applyToReg allocation dest, applyToOperand allocation src)
    | Add (dest, left, right) ->
        Add (applyToReg allocation dest, applyToReg allocation left, applyToOperand allocation right)
    | Sub (dest, left, right) ->
        Sub (applyToReg allocation dest, applyToReg allocation left, applyToOperand allocation right)
    | Mul (dest, left, right) ->
        Mul (applyToReg allocation dest, applyToReg allocation left, applyToReg allocation right)
    | Sdiv (dest, left, right) ->
        Sdiv (applyToReg allocation dest, applyToReg allocation left, applyToReg allocation right)
    | Ret -> Ret

/// Register allocation
let allocateRegisters (func: Function) : AllocResult =
    // Collect virtual registers
    let virtualRegs = collectVirtualRegs func.Body

    // Create allocation map
    let allocation = createAllocation virtualRegs

    // Apply allocation to all instructions
    let allocatedInstrs = func.Body |> List.map (applyAllocation allocation)

    {
        Instrs = allocatedInstrs
        StackSize = 0  // No spilling yet
    }
