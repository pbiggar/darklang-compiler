// 4.5_LIR_Optimize.fs - LIR Peephole Optimizations
//
// Performs low-level optimizations on LIR:
// - Remove identity operations (add x, y, 0 → mov x, y)
// - Remove self-moves (mov x, x → remove)
// - Constant multiplication optimizations (mul x, y, 0 → mov x, 0)
// - Dead move elimination
//
// These optimizations work on individual instructions or small sequences.

module LIR_Optimize

open LIR

/// Check if an operand is an immediate with value 0
let isZero (op: Operand) : bool =
    match op with
    | Imm 0L -> true
    | _ -> false

/// Check if an operand is an immediate with value 1
let isOne (op: Operand) : bool =
    match op with
    | Imm 1L -> true
    | _ -> false

/// Check if two registers are the same
let sameReg (r1: Reg) (r2: Reg) : bool =
    match r1, r2 with
    | Physical p1, Physical p2 -> p1 = p2
    | Virtual v1, Virtual v2 -> v1 = v2
    | _ -> false

/// Optimize a single instruction (returns None to remove, Some to replace)
let optimizeInstr (instr: Instr) : Instr option =
    match instr with
    // Remove self-moves: mov x, x → remove
    | Mov (dest, Reg src) when sameReg dest src ->
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

/// Optimize a list of instructions (single-pass peephole)
let optimizeInstrs (instrs: Instr list) : Instr list =
    instrs
    |> List.choose optimizeInstr

/// Optimize a basic block
let optimizeBlock (block: BasicBlock) : BasicBlock =
    { block with Instrs = optimizeInstrs block.Instrs }

/// Optimize a CFG
let optimizeCFG (cfg: CFG) : CFG =
    let blocks' =
        cfg.Blocks
        |> Map.map (fun _ block -> optimizeBlock block)
    { cfg with Blocks = blocks' }

/// Optimize a function
let optimizeFunction (func: Function) : Function =
    { func with CFG = optimizeCFG func.CFG }

/// Optimize a program
let optimizeProgram (program: Program) : Program =
    let (Program (functions, strings, floats)) = program
    let functions' = functions |> List.map optimizeFunction
    Program (functions', strings, floats)
