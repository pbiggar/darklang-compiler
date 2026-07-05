# N-Queens Benchmark Optimization Investigation

## Summary

The nqueen benchmark counts solutions to the N-Queens problem (N=13) using bit manipulation. This is a recursive backtracking algorithm that tests positions using bitmasks for columns and diagonals.

**Performance Results:**
- Rust: ~23ms per run
- OCaml: ~30ms per run
- Dark: ~49ms per run
- **Dark is ~2.1x slower than Rust, ~1.6x slower than OCaml**

## Benchmark Source Code

### Dark (`benchmarks/problems/nqueen/dark/main.dark`)
```dark
def nqueenSolve(allOnes: Int64, cols: Int64, diag1: Int64, diag2: Int64, avail: Int64, count: Int64) : Int64 =
    if avail == 0 then count
    else
        let pos = avail & (0 - avail) in
        let newAvail = avail - pos in
        let newCols = Int64.bitwiseOr(cols, pos) in
        let subCount =
            if newCols == allOnes then 1
            else
                let newDiag1 = (Int64.bitwiseOr(diag1, pos)) << 1 in
                let newDiag2 = (Int64.bitwiseOr(diag2, pos)) >> 1 in
                let blocked = Int64.bitwiseOr(Int64.bitwiseOr(newCols, newDiag1), newDiag2) in
                let nextRowAvail = allOnes & (~~~blocked) in
                nqueenSolve(allOnes, newCols, newDiag1, newDiag2, nextRowAvail, 0) in
        nqueenSolve(allOnes, cols, diag1, diag2, newAvail, count + subCount)

def nqueen(n: Int64) : Int64 =
    let allOnes = (1 << n) - 1 in
    nqueenSolve(allOnes, 0, 0, 0, allOnes, 0)

nqueen(13)
```

### Rust (`benchmarks/problems/nqueen/rust/main.rs`)
```rust
fn nqueen(n: u32) -> u64 {
    let all_ones: u32 = (1 << n) - 1;

    fn solve(cols: u32, diag1: u32, diag2: u32, all_ones: u32) -> u64 {
        if cols == all_ones {
            return 1;
        }
        let mut count = 0u64;
        let mut avail = all_ones & !(cols | diag1 | diag2);
        while avail != 0 {
            let pos = avail & avail.wrapping_neg();
            avail -= pos;
            count += solve(cols | pos, (diag1 | pos) << 1, (diag2 | pos) >> 1, all_ones);
        }
        count
    }

    solve(0, 0, 0, all_ones)
}

fn main() {
    let result = nqueen(13);
    println!("{}", result);
}
```

### OCaml (`benchmarks/problems/nqueen/ocaml/main.ml`)
```ocaml
let nqueen n =
  let all_ones = (1 lsl n) - 1 in

  let rec solve cols diag1 diag2 =
    if cols = all_ones then 1L
    else begin
      let count = ref 0L in
      let avail = ref (all_ones land (lnot (cols lor diag1 lor diag2))) in
      while !avail <> 0 do
        let pos = !avail land (-(!avail)) in
        avail := !avail - pos;
        count := Int64.add !count (solve (cols lor pos) ((diag1 lor pos) lsl 1) ((diag2 lor pos) lsr 1))
      done;
      !count
    end
  in

  solve 0 0 0

let () = Printf.printf "%Ld\n" (nqueen 13)
```

## Analysis

### Key Structural Differences

#### Rust: Iterative While Loop with Single Recursive Call
Rust uses an iterative `while` loop that makes **one recursive call per iteration**. The tail call optimization converts the outer recursion into a simple jump:

```asm
; Rust solve function - HOT LOOP (only 21 instructions for recursive path)
93c4: stp  x29, x30, [sp, #-64]!    ; Prologue
93d8: mov  w8, #0x1fff              ; all_ones = 8191 for N=13
93dc: cmp  w0, w8
93e0: b.ne 9400                      ; If cols != all_ones, continue

; Base case - return 1
93e4: mov  w22, #0x1
93e8: mov  x0, x22
...
93fc: ret

; Main loop body
9440: eor  w23, w9, #0x1fff         ; avail = all_ones ^ blocked
9444: neg  w8, w23                   ; -avail
9448: and  w8, w23, w8               ; pos = avail & -avail
944c: orr  w9, w8, w21               ; cols | pos
9450: orr  w10, w8, w19              ; diag2 | pos
9454: orr  w0, w8, w20               ; diag1 | pos
9458: lsl  w1, w9, #1                ; (diag1 | pos) << 1
945c: lsr  w2, w10, #1               ; (diag2 | pos) >> 1
9460: sub  w23, w23, w8              ; avail -= pos
9464: bl   93c4                      ; SINGLE recursive call
9468: add  x22, x0, x22              ; count += result
946c: cbnz w23, 9444                 ; Loop if avail != 0
```

**Key observation**: Rust uses `orr` (bitwise OR) as a **single instruction** - no function call overhead.

#### Dark: Function Calls for Int64.bitwiseOr

Dark's `nqueenSolve` makes **5 function calls to `Stdlib.Int64.bitwiseOr`** per recursive iteration, plus **1 recursive call** (the tail call is optimized to a jump):

```
; From Dark LIR (After Register Allocation)
Label "nqueenSolve_L1":
    X19 <- Mov(Imm 0)
    X19 <- Sub(X19, Reg X21)
    X20 <- And(X21, X19)
    X24 <- Sub(X21, Reg X20)

    ; CALL #1: Int64.bitwiseOr(cols, pos)
    SaveRegs([X1, X2, X3], [])
    ArgMoves(X0 <- Reg X2, X1 <- Reg X20)
    X23 <- Call(Stdlib.Int64.bitwiseOr, [Reg X2, Reg X20])
    RestoreRegs([X1, X2, X3], [])
    X23 <- Mov(Reg X0)

    ; CALL #2: Int64.bitwiseOr(diag1, pos)
    SaveRegs([X1, X2, X3], [])
    ArgMoves(X0 <- Reg X1, X1 <- Reg X20)
    X19 <- Call(Stdlib.Int64.bitwiseOr, [Reg X1, Reg X20])
    RestoreRegs([X1, X2, X3], [])
    X19 <- Mov(Reg X0)
    X21 <- Lsl_imm(X19, #1)

    ; CALL #3: Int64.bitwiseOr(diag2, pos)
    SaveRegs([X1, X2, X3], [])
    ArgMoves(X0 <- Reg X26, X1 <- Reg X20)
    X19 <- Call(Stdlib.Int64.bitwiseOr, [Reg X26, Reg X20])
    RestoreRegs([X1, X2, X3], [])
    X19 <- Mov(Reg X0)
    X20 <- Lsr_imm(X19, #1)

    ; CALL #4: Int64.bitwiseOr(newCols, newDiag1)
    SaveRegs([X1, X2, X3], [])
    ArgMoves(X0 <- Reg X23, X1 <- Reg X21)
    X19 <- Call(Stdlib.Int64.bitwiseOr, [Reg X23, Reg X21])
    RestoreRegs([X1, X2, X3], [])
    X19 <- Mov(Reg X0)

    ; CALL #5: Int64.bitwiseOr(blocked_partial, newDiag2)
    SaveRegs([X1, X2, X3], [])
    ArgMoves(X0 <- Reg X19, X1 <- Reg X20)
    X19 <- Call(Stdlib.Int64.bitwiseOr, [Reg X19, Reg X20])
    RestoreRegs([X1, X2, X3], [])
    X19 <- Mov(Reg X0)

    X19 <- Mvn(X19)                  ; ~blocked
    X19 <- And(X3, X19)              ; allOnes & ~blocked

    ; RECURSIVE CALL
    SaveRegs([X1, X2, X3], [])
    ArgMoves(X0 <- Reg X3, X1 <- Reg X23, X2 <- Reg X21, X3 <- Reg X20, X4 <- Reg X19, X5 <- Imm 0)
    X19 <- Call(nqueenSolve, ...)
    RestoreRegs([X1, X2, X3], [])
```

Each `Int64.bitwiseOr` call involves:
1. `SaveRegs([X1, X2, X3], [])` - Save caller-saved registers
2. `ArgMoves(X0 <- ..., X1 <- ...)` - Move arguments to calling convention registers
3. `Call(Stdlib.Int64.bitwiseOr, ...)` - Branch-and-link
4. `RestoreRegs([X1, X2, X3], [])` - Restore registers
5. `Mov(Reg X0)` - Copy result from return register

**The Int64.bitwiseOr function itself is trivial:**
```
Stdlib.Int64.bitwiseOr:
  Label "Stdlib.Int64.bitwiseOr_body":
    X1 <- Orr(X2, X1)
    X0 <- Mov(Reg X1)
    Ret
```

This is a **single `orr` instruction** wrapped in function call overhead!

### Instruction Count Comparison

| Operation | Rust | Dark |
|-----------|------|------|
| Bitwise OR | 1 `orr` instruction | ~8 instructions (call overhead) |
| Per-iteration overhead | ~15 instructions | ~50+ instructions |
| Function calls in hot loop | 1 (recursive) | 6 (5 bitwiseOr + 1 recursive) |

### Redundant Move Instructions

The Dark LIR shows multiple redundant moves after register allocation:

```
Label "nqueenSolve_L5":
    X20 <- Add(X25, Reg X19)
    X19 <- Mov(Reg X3)      ; These 6 moves are setting up
    X19 <- Mov(Reg X2)      ; variables for the loop back,
    X19 <- Mov(Reg X1)      ; but they overwrite X19 multiple
    X19 <- Mov(Reg X26)     ; times, which is clearly wrong
    X19 <- Mov(Reg X24)     ; or wasteful
    X19 <- Mov(Reg X20)
    X3 <- Mov(Reg X3)       ; Self-move - no effect!
    X2 <- Mov(Reg X2)       ; Self-move - no effect!
    X1 <- Mov(Reg X1)       ; Self-move - no effect!
    X26 <- Mov(Reg X26)     ; Self-move - no effect!
    X21 <- Mov(Reg X24)
    X25 <- Mov(Reg X20)
```

This suggests issues with:
1. Dead code elimination not removing unused assignments
2. Register allocator not coalescing properly
3. Self-move elimination not running post-allocation

## Identified Optimization Opportunities

### 1. Eliminate SaveRegs/RestoreRegs for Callee-Saved Only Live Ranges

**Impact: ~5-10% performance improvement (estimated)**

**Root Cause:**
When only callee-saved registers (X19-X28) are live across a call, there's no need to save/restore caller-saved registers. However, the current code still generates SaveRegs/RestoreRegs even when the register list contains values that will be saved by the callee:

```
SaveRegs([X1, X2, X3], [])      ; X1, X2, X3 are argument registers
...
RestoreRegs([X1, X2, X3], [])
```

But notice that after allocation, the actual live values are in X19-X26 (callee-saved), so saving X1, X2, X3 may be unnecessary if they don't contain live values.

**Implementation Approach:**
1. Track which caller-saved registers actually contain live values across calls
2. Only include those registers in SaveRegs/RestoreRegs
3. Skip SaveRegs/RestoreRegs entirely when the list is effectively empty

**Files to Modify:**
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Improve live range analysis for save/restore
- `src/DarkCompiler/passes/6_CodeGen.fs` - Skip empty save/restore pairs

---

### 2. Convert Tail-Recursive Style to While Loop

**Impact: ~5% performance improvement (estimated)**

**Root Cause:**
The Dark source uses a functional recursive style, which is transformed into a tail-recursive loop at the ANF level. However, this still involves phi nodes and control flow that could be simplified.

Rust's iterative `while avail != 0` loop is more direct and generates tighter code.

**Evidence:**
Dark's control flow for the tail call path involves multiple blocks:
```
nqueenSolve_L5 -> nqueenSolve_body -> nqueenSolve_L0/L1
```

Rust's while loop is a tight:
```
cbnz w23, 9444    ; Single conditional branch back to loop start
```

**Implementation Approach:**
This could be addressed by:
1. Recognizing the tail-recursive accumulator pattern at ANF level
2. Converting to explicit loop structure in MIR
3. Or improving MIR->LIR to generate tighter loops from phi-based control flow

**Files to Modify:**
- `src/DarkCompiler/passes/2.7_TailCallDetection.fs` - Detect loop-convertible patterns
- `src/DarkCompiler/passes/3_ANF_to_MIR.fs` - Generate loop structures

---

## Summary of Expected Improvements

| Optimization | Estimated Impact | Complexity |
|--------------|-----------------|------------|
| Redundant Self-Move Elimination | 10-15% | Low |
| SaveRegs/RestoreRegs Optimization | 5-10% | Low-Medium |
| Loop Structure Improvement | 5% | Medium |

**Total estimated improvement: ~2x faster** (bringing Dark close to Rust/OCaml performance)

## Recommended Implementation Order

1. **Redundant Self-Move Elimination** - Quick win, low complexity
2. **SaveRegs/RestoreRegs Optimization** - Medium complexity, good impact
3. **Loop Structure Improvement** - More complex, moderate benefit

## Appendix: Full IR Dumps

### Dark ANF (after optimization)
```
Function nqueenSolve:
let TempId 6 = t4 == 0
if t6 then
return t5
else
let TempId 7 = 0 - t4
let TempId 8 = t4 & t7
let TempId 10 = t4 - t8
let TempId 12 = Stdlib.Int64.bitwiseOr(t1, t8)
let TempId 14 = t12 == t0
let TempId 15 = Stdlib.Int64.bitwiseOr(t2, t8)
let TempId 16 = t15 << 1
let TempId 18 = Stdlib.Int64.bitwiseOr(t3, t8)
let TempId 19 = t18 >> 1
let TempId 21 = Stdlib.Int64.bitwiseOr(t12, t16)
let TempId 22 = Stdlib.Int64.bitwiseOr(t21, t19)
let TempId 24 = ~~~t22
let TempId 25 = t0 & t24
let TempId 27 = nqueenSolve(t0, t12, t16, t19, t25, 0)
let TempId 28 = if t14 then 1 else t27
let TempId 30 = t5 + t28
let TempId 31 = TailCall(nqueenSolve, [t0, t1, t2, t3, t10, t30])
return t31
```

### Dark MIR (Control Flow Graph)
```
Function nqueenSolve:
  nqueenSolve_entry:
    jump nqueenSolve_body
  nqueenSolve_L0:
    v32 <- v5 : TInt64
    jump nqueenSolve_L2
  nqueenSolve_L1:
    v7 <- 0 - v4 : TInt64
    v8 <- v4 & v7 : TInt64
    v10 <- v4 - v8 : TInt64
    v12 <- Call(Stdlib.Int64.bitwiseOr, [v1, v8])
    v14 <- v12 == v0 : TFunction ([TInt64; TInt64], TInt64)
    v15 <- Call(Stdlib.Int64.bitwiseOr, [v2, v8])
    v16 <- v15 << 1 : TFunction ([TInt64; TInt64], TInt64)
    v18 <- Call(Stdlib.Int64.bitwiseOr, [v3, v8])
    v19 <- v18 >> 1 : TFunction ([TInt64; TInt64], TInt64)
    v21 <- Call(Stdlib.Int64.bitwiseOr, [v12, v16])
    v22 <- Call(Stdlib.Int64.bitwiseOr, [v21, v19])
    v24 <- ~~~v22
    v25 <- v0 & v24 : TInt64
    v27 <- Call(nqueenSolve, [v0, v12, v16, v19, v25, 0])
    branch v14 ? nqueenSolve_L3 : nqueenSolve_L4
  nqueenSolve_L2:
    ret v32
  nqueenSolve_L3:
    v28 <- 1 : TInt64
    jump nqueenSolve_L5
  nqueenSolve_L4:
    v28 <- v27 : TInt64
    jump nqueenSolve_L5
  nqueenSolve_L5:
    v30 <- v5 + v28 : TInt64
    ...
    jump nqueenSolve_body
  nqueenSolve_body:
    v6 <- v4 == 0 : TInt64
    branch v6 ? nqueenSolve_L0 : nqueenSolve_L1
```

### Dark LIR (After Register Allocation)
```
nqueenSolve:
  Label "nqueenSolve_L0":
    Jump(Label "nqueenSolve_L2")
  Label "nqueenSolve_L1":
    X19 <- Mov(Imm 0)
    X19 <- Sub(X19, Reg X21)
    X20 <- And(X21, X19)
    X24 <- Sub(X21, Reg X20)
    SaveRegs([X1, X2, X3], [])
    ArgMoves(X0 <- Reg X2, X1 <- Reg X20)
    X23 <- Call(Stdlib.Int64.bitwiseOr, [Reg X2, Reg X20])
    RestoreRegs([X1, X2, X3], [])
    X23 <- Mov(Reg X0)
    Cmp(X23, Reg X3)
    X22 <- Cset(EQ)
    ... (more bitwiseOr calls)
    SaveRegs([X1, X2, X3], [])
    ArgMoves(X0 <- Reg X3, X1 <- Reg X23, X2 <- Reg X21, X3 <- Reg X20, X4 <- Reg X19, X5 <- Imm 0)
    X19 <- Call(nqueenSolve, [Reg X3, Reg X23, Reg X21, Reg X20, Reg X19, Imm 0])
    RestoreRegs([X1, X2, X3], [])
    X19 <- Mov(Reg X0)
    Branch(X22, Label "nqueenSolve_L3", Label "nqueenSolve_L4")
  Label "nqueenSolve_L2":
    X0 <- Mov(Reg X25)
    Ret
  ...
```

### Rust solve function (full disassembly)
```asm
00000000000093c4 <_ZN6nqueen6nqueen5solve17hf29b6fa53339c3e5E>:
    93c4: stp  x29, x30, [sp, #-64]!
    93c8: str  x23, [sp, #16]
    93cc: stp  x22, x21, [sp, #32]
    93d0: stp  x20, x19, [sp, #48]
    93d4: mov  x29, sp
    93d8: mov  w8, #0x1fff              ; all_ones = 8191
    93dc: cmp  w0, w8
    93e0: b.ne 9400
    93e4: mov  w22, #0x1                ; Base case: return 1
    93e8: mov  x0, x22
    93ec: ldp  x20, x19, [sp, #48]
    93f0: ldr  x23, [sp, #16]
    93f4: ldp  x22, x21, [sp, #32]
    93f8: ldp  x29, x30, [sp], #64
    93fc: ret
    9400: orr  w9, w1, w0               ; cols | diag1
    9404: mov  w19, w2
    9408: mov  w20, w0
    940c: orr  w9, w9, w2               ; (cols | diag1) | diag2
    9410: mov  w21, w1
    9414: and  w9, w9, #0x1fff
    9418: cmp  w9, w8
    941c: b.ne 943c
    9420: mov  x22, xzr                 ; count = 0
    9424: mov  x0, x22
    ...
    943c: mov  x22, xzr
    9440: eor  w23, w9, #0x1fff         ; avail = all_ones ^ blocked
    9444: neg  w8, w23                  ; -avail  (LOOP START)
    9448: and  w8, w23, w8              ; pos = avail & -avail
    944c: orr  w9, w8, w21              ; diag1 | pos
    9450: orr  w10, w8, w19             ; diag2 | pos
    9454: orr  w0, w8, w20              ; cols | pos
    9458: lsl  w1, w9, #1               ; (diag1 | pos) << 1
    945c: lsr  w2, w10, #1              ; (diag2 | pos) >> 1
    9460: sub  w23, w23, w8             ; avail -= pos
    9464: bl   93c4                     ; SINGLE recursive call
    9468: add  x22, x0, x22             ; count += result
    946c: cbnz w23, 9444                ; Loop if avail != 0
    9470: b    93e8                     ; Return count
```
