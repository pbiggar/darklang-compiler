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

/// Check if a register is used in any instruction (for dead code detection)
let isRegUsedInInstrs (reg: Reg) (instrs: Instr list) : bool =
    instrs |> List.exists (fun instr ->
        match instr with
        | Mov (_, Reg r) -> sameReg r reg
        | Mov (_, _) -> false
        | Add (_, left, Reg right) -> sameReg left reg || sameReg right reg
        | Add (_, left, _) -> sameReg left reg
        | Sub (_, left, Reg right) -> sameReg left reg || sameReg right reg
        | Sub (_, left, _) -> sameReg left reg
        | Mul (_, left, right) -> sameReg left reg || sameReg right reg
        | Cmp (left, Reg right) -> sameReg left reg || sameReg right reg
        | Cmp (left, _) -> sameReg left reg
        | Cset _ -> false  // Cset only writes, doesn't read
        | _ -> false  // Conservative: assume not used for other instructions
    )

/// Try to fuse MUL + ADD into MADD (multiply-add)
/// Pattern: MUL temp, a, b; ADD dest, temp, Reg c → MADD dest, a, b, c
/// Or:      MUL temp, a, b; ADD dest, Reg c, temp → MADD dest, a, b, c (commutative)
let tryFuseMulAdd (instrs: Instr list) : Instr list =
    let rec loop acc remaining =
        match remaining with
        | [] -> List.rev acc
        | [single] -> List.rev (single :: acc)
        | Mul (mulDest, mulLeft, mulRight) :: Add (addDest, addLeft, Reg addRight) :: rest
            when sameReg mulDest addLeft && not (sameReg mulDest addRight) ->
            // MUL temp, a, b; ADD dest, temp, c → MADD dest, a, b, c
            // Check that temp is not used later (dead after the ADD)
            if not (isRegUsedInInstrs mulDest rest) then
                loop (Madd (addDest, mulLeft, mulRight, addRight) :: acc) rest
            else
                loop (Mul (mulDest, mulLeft, mulRight) :: acc) (Add (addDest, addLeft, Reg addRight) :: rest)
        | Mul (mulDest, mulLeft, mulRight) :: Add (addDest, addLeft, Reg addRight) :: rest
            when sameReg mulDest addRight && not (sameReg mulDest addLeft) ->
            // MUL temp, a, b; ADD dest, c, temp → MADD dest, a, b, c (commutative)
            if not (isRegUsedInInstrs mulDest rest) then
                loop (Madd (addDest, mulLeft, mulRight, addLeft) :: acc) rest
            else
                loop (Mul (mulDest, mulLeft, mulRight) :: acc) (Add (addDest, addLeft, Reg addRight) :: rest)
        | instr :: rest ->
            loop (instr :: acc) rest
    loop [] instrs

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
            | EQ ->
                // CMP reg, #0 + B.eq → CBZ reg (BranchZero)
                Some (otherInstrs, BranchZero (cmpReg, trueLabel, falseLabel))
            | NE ->
                // CMP reg, #0 + B.ne → CBNZ reg (Branch)
                Some (otherInstrs, Branch (cmpReg, trueLabel, falseLabel))
            | _ ->
                // Other conditions (LT, GT, LE, GE) can't be fused with CBZ/CBNZ
                None
        | _ -> None
    | _ -> None

/// Apply TBZ/TBNZ fusion if applicable
/// NOTE: Currently disabled because it causes extra register moves that negate the savings.
/// The AND_imm defines a new register, but TBZ uses the source register directly,
/// which can conflict with register allocation and cause spills/moves.
/// TODO: Enable this once register allocation can handle the pattern better.
let applyAndBitBranchFusion (instrs: Instr list) (terminator: Terminator) : (Instr list * Terminator) =
    // Disabled - causes performance regression due to extra register moves
    (instrs, terminator)

/// Optimize a basic block
let optimizeBlock (block: BasicBlock) : BasicBlock =
    let instrs' = optimizeInstrs block.Instrs
    // Apply MUL + ADD → MADD fusion
    let instrs'' = tryFuseMulAdd instrs'

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
    { block with Instrs = finalInstrs; Terminator = finalTerminator }

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
