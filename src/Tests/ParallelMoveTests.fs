// ParallelMoveTests.fs - Unit tests for parallel move resolution
//
// Tests the parallel move algorithm used for TailArgMoves and (future) phi resolution.
// The algorithm must correctly sequence parallel moves to avoid clobbering values.
//
// Key edge cases:
// - Simple moves (no conflict)
// - Chain moves (must reorder: X1←X2, X2←X3 → emit X2←X3 first)
// - Cycles (need temp register: X1←X2, X2←X1 → use X16 as temp)
// - Self-moves (should be eliminated: X1←X1 → no-op)

module ParallelMoveTests

open LIR
open ARM64

/// Test result type
type TestResult = Result<unit, string>

/// Helper to convert LIR TailArgMoves to ARM64 instructions via CodeGen
let convertTailArgMoves (moves: (LIR.PhysReg * LIR.Operand) list) : Result<ARM64.Instr list, string> =
    let ctx : CodeGen.CodeGenContext = {
        Options = CodeGen.defaultOptions
        StringPool = { MIR.Strings = Map.empty; MIR.StringToId = Map.empty; MIR.NextId = 0 }
        StackSize = 0
        UsedCalleeSaved = []
    }
    CodeGen.convertInstr ctx (LIR.TailArgMoves moves)

/// Helper to check if ARM64 instruction is a MOV_reg
let isMoveReg (instr: ARM64.Instr) : bool =
    match instr with
    | ARM64.MOV_reg _ -> true
    | _ -> false

/// Helper to get register pair from MOV_reg
let getMoveRegPair (instr: ARM64.Instr) : (ARM64.Reg * ARM64.Reg) option =
    match instr with
    | ARM64.MOV_reg (dest, src) -> Some (dest, src)
    | _ -> None

// =============================================================================
// Test Cases
// =============================================================================

/// Simple move: X1 ← X2 (no conflict)
let testSimpleMove () : TestResult =
    let moves = [(LIR.X1, LIR.Reg (LIR.Physical LIR.X2))]
    match convertTailArgMoves moves with
    | Error e -> Error $"Failed to convert: {e}"
    | Ok instrs ->
        // Should produce exactly one MOV
        let movs = instrs |> List.filter isMoveReg
        if movs.Length <> 1 then
            Error $"Expected 1 MOV, got {movs.Length}"
        else
            Ok ()

/// Chain of moves: X1←X2, X2←X3 (must emit X1←X2 before X2←X3)
/// In parallel move semantics: X1 gets old_X2, X2 gets old_X3
/// To achieve this sequentially: first read X2 into X1, then overwrite X2 with X3
let testChainMoves () : TestResult =
    let moves = [
        (LIR.X1, LIR.Reg (LIR.Physical LIR.X2))
        (LIR.X2, LIR.Reg (LIR.Physical LIR.X3))
    ]
    match convertTailArgMoves moves with
    | Error e -> Error $"Failed to convert: {e}"
    | Ok instrs ->
        // Should produce exactly 2 MOVs
        let movs = instrs |> List.filter isMoveReg
        if movs.Length <> 2 then
            Error $"Expected 2 MOVs, got {movs.Length}"
        else
            // The order matters: X1←X2 must come before X2←X3
            // (save the value from X2 before it's overwritten)
            match movs |> List.choose getMoveRegPair with
            | [(dest1, src1); (dest2, src2)] ->
                // First move should write to X1 (reading from X2)
                // Second move should write to X2 (reading from X3)
                if dest1 = ARM64.X1 && src1 = ARM64.X2 && dest2 = ARM64.X2 && src2 = ARM64.X3 then
                    Ok ()
                else
                    Error $"Wrong move order: [{dest1}←{src1}, {dest2}←{src2}]"
            | _ -> Error "Could not extract move pairs"

/// Two-way swap cycle: X1←X2, X2←X1 (needs temp X16)
let testTwoWaySwap () : TestResult =
    let moves = [
        (LIR.X1, LIR.Reg (LIR.Physical LIR.X2))
        (LIR.X2, LIR.Reg (LIR.Physical LIR.X1))
    ]
    match convertTailArgMoves moves with
    | Error e -> Error $"Failed to convert: {e}"
    | Ok instrs ->
        // Should produce 3 MOVs (save to temp, two moves with temp substitution)
        let movs = instrs |> List.filter isMoveReg
        if movs.Length <> 3 then
            Error $"Expected 3 MOVs for swap, got {movs.Length}"
        else
            // First move should save X1 to X16 (the temp)
            match movs.[0] |> getMoveRegPair with
            | Some (ARM64.X16, _) -> Ok ()
            | Some (dest, src) -> Error $"Expected first move to X16, got {dest}←{src}"
            | None -> Error "First instruction is not a MOV"

/// Three-way cycle: X1←X2, X2←X3, X3←X1 (needs temp)
let testThreeWayCycle () : TestResult =
    let moves = [
        (LIR.X1, LIR.Reg (LIR.Physical LIR.X2))
        (LIR.X2, LIR.Reg (LIR.Physical LIR.X3))
        (LIR.X3, LIR.Reg (LIR.Physical LIR.X1))
    ]
    match convertTailArgMoves moves with
    | Error e -> Error $"Failed to convert: {e}"
    | Ok instrs ->
        // Should produce 4 MOVs (save + 3 moves with cycle breaking)
        let movs = instrs |> List.filter isMoveReg
        if movs.Length <> 4 then
            Error $"Expected 4 MOVs for 3-way cycle, got {movs.Length}"
        else
            Ok ()

/// Self-move: X1←X1 (should be eliminated as no-op)
let testSelfMoveEliminated () : TestResult =
    let moves = [(LIR.X1, LIR.Reg (LIR.Physical LIR.X1))]
    match convertTailArgMoves moves with
    | Error e -> Error $"Failed to convert: {e}"
    | Ok instrs ->
        // Self-move should produce no MOVs
        let movs = instrs |> List.filter isMoveReg
        if movs.Length <> 0 then
            Error $"Expected 0 MOVs for self-move, got {movs.Length}"
        else
            Ok ()

/// Mixed: cycle + independent move
let testMixedCycleAndIndependent () : TestResult =
    let moves = [
        (LIR.X1, LIR.Reg (LIR.Physical LIR.X2))
        (LIR.X2, LIR.Reg (LIR.Physical LIR.X1))
        (LIR.X3, LIR.Reg (LIR.Physical LIR.X4))
    ]
    match convertTailArgMoves moves with
    | Error e -> Error $"Failed to convert: {e}"
    | Ok instrs ->
        // Should produce: 1 for X3←X4 + 3 for swap = 4 MOVs
        let movs = instrs |> List.filter isMoveReg
        if movs.Length <> 4 then
            Error $"Expected 4 MOVs, got {movs.Length}"
        else
            Ok ()

/// Move from immediate (no conflict possible)
let testMoveFromImmediate () : TestResult =
    let moves = [(LIR.X1, LIR.Imm 42L)]
    match convertTailArgMoves moves with
    | Error e -> Error $"Failed to convert: {e}"
    | Ok instrs ->
        // Should produce instructions to load immediate
        if instrs.Length = 0 then
            Error "Expected at least one instruction for immediate move"
        else
            Ok ()

/// Move from stack slot
let testMoveFromStackSlot () : TestResult =
    let moves = [(LIR.X1, LIR.StackSlot (-8))]
    match convertTailArgMoves moves with
    | Error e -> Error $"Failed to convert: {e}"
    | Ok instrs ->
        // Should produce LDR instruction
        if instrs.Length = 0 then
            Error "Expected at least one instruction for stack slot move"
        else
            Ok ()

let tests = [
    ("simple move", testSimpleMove)
    ("chain moves", testChainMoves)
    ("two-way swap", testTwoWaySwap)
    ("three-way cycle", testThreeWayCycle)
    ("self-move eliminated", testSelfMoveEliminated)
    ("mixed cycle and independent", testMixedCycleAndIndependent)
    ("move from immediate", testMoveFromImmediate)
    ("move from stack slot", testMoveFromStackSlot)
]

/// Run all parallel move tests
let runAll () : TestResult =
    let rec runTests tests =
        match tests with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Error e -> Error $"FAIL: {name}: {e}"
            | Ok () -> runTests rest

    runTests tests
