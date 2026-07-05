# Collatz Benchmark Optimization Investigation

## Executive Summary

The Dark compiler produces code for the collatz benchmark that is approximately **30-40% slower than Rust** (17ms vs 12ms). Analysis reveals multiple optimization opportunities in the hot inner loop that could bring performance closer to Rust's level.

## Benchmark Results

| Language | Time (best of 3) | Relative |
|----------|------------------|----------|
| Rust     | 12ms             | 1.0x     |
| Dark     | 17ms             | 1.42x    |
| OCaml    | 18ms             | 1.50x    |

All produce correct output: `10753840`

## Source Code Comparison

### Dark (recursive with TCO)
```dark
def collatzSteps(n: Int64, steps: Int64) : Int64 =
    if n == 1 then steps
    else if n % 2 == 0 then collatzSteps(n / 2, steps + 1)
    else collatzSteps(3 * n + 1, steps + 1)
```

### Rust (iterative)
```rust
fn collatz_steps(mut n: i64) -> i64 {
    let mut steps = 0;
    while n != 1 {
        if n % 2 == 0 { n = n / 2; }
        else { n = 3 * n + 1; }
        steps += 1;
    }
    steps
}
```

## Hot Loop Disassembly Analysis

### Rust Hot Loop (7 instructions, branchless)
```asm
8430: asr  x14, x13, #1           ; n/2
8434: add  x15, x13, x13, lsl #1  ; 3*n using shift-add!
8438: tst  x13, #0x1              ; test odd/even
843c: add  x12, x12, #0x1         ; steps++
8440: csinc x13, x14, x15, eq     ; n = (even?) ? n/2 : 3*n+1
8444: cmp  x13, #0x1              ; check done
8448: b.ne 0x8430                 ; loop
```

### Dark Hot Loop (13+ instructions, with branches)

Even path (8 instructions + 2 branches):
```asm
228: cmp  x3, #0x1        ; check n==1
22c: b.eq 0x1dc           ; if done, exit
1e4: tbz  w3, #0, 0x1f4   ; test bit 0 (good - uses TBZ!)
1f4: lsr  x7, x3, #1      ; n/2
1f8: add  x6, x2, #0x1    ; steps+1
1fc: mov  x3, x7          ; n = result (redundant!)
200: mov  x2, x6          ; steps = result (redundant!)
204: b    0x228           ; loop
```

Odd path (11 instructions + 3 branches):
```asm
228: cmp  x3, #0x1
22c: b.eq/b.ne ...
1e4: tbz  w3, #0, 0x1f4
1e8: b    0x208
208: mov  x1, #0x3        ; PROBLEM: constant load in loop!
20c: mul  x5, x1, x3      ; 3*n
210: add  x4, x5, #0x1    ; 3*n+1
214: add  x1, x2, #0x1    ; steps+1
218: mov  x3, x4          ; n = result (redundant!)
21c: mov  x2, x1          ; steps = result (redundant!)
220: b    0x228           ; loop
```

## Key Optimization Opportunities

### 1. Conditional Select (CSEL/CSINC) for If-Then-Else (High Impact)

**Issue:** Dark generates branching code for simple if-expressions, while Rust uses branchless conditional select.

**IR Evidence (MIR):**
```
branch v4 ? collatzSteps_L3 : collatzSteps_L4
```
Both branches compute their results then jump back to the loop.

**Assembly Evidence (Dark):**
```asm
1e4: tbz  w3, #0, 0x1f4   ; branch on bit 0
1e8: b    0x208           ; unconditional branch to odd path
...separate code paths...
204/220: b  0x228         ; back to loop
```

**Rust Assembly (branchless):**
```asm
8438: tst  x13, #0x1              ; test (no branch!)
8440: csinc x13, x14, x15, eq     ; select result based on condition
```

**Root Cause:** The MIR-to-LIR pass generates basic blocks with branches for if-expressions. There's no if-conversion pass that transforms simple diamond patterns into conditional selects.

**Impact Estimate:** ~20-30% improvement. Branch mispredictions on random-ish Collatz sequences are expensive (~15 cycle penalty). Branchless code eliminates this.

**Implementation:**
- Add an MIR or LIR optimization pass for if-conversion
- Detect diamond CFG patterns where both paths are simple computations
- Replace with conditional select (CSEL) or conditional increment (CSINC)

**Files to Modify:**
- `src/DarkCompiler/passes/3.5_MIR_Optimize.fs` (if-conversion)
- `src/DarkCompiler/LIR.fs` (add Csel/Csinc instructions)
- `src/DarkCompiler/passes/4_MIR_to_LIR.fs` (lower IfValue to Csel)
- `src/DarkCompiler/passes/6_CodeGen.fs` (emit CSEL/CSINC)
- `src/DarkCompiler/ARM64.fs` (CSEL encoding)

---

### 2. Redundant Register Moves (Medium Impact)

**Issue:** The hot loop contains redundant register-to-register moves.

**Assembly Evidence:**
```asm
1fc: mov  x3, x7          ; n = lsr result
200: mov  x2, x6          ; steps = add result
```

These moves could be eliminated if the LSR/ADD instructions wrote directly to x3/x2.

**Root Cause:** Register allocation doesn't coalesce registers optimally for loop variables. The LIR optimizer only removes self-moves (`mov x, x`), not moves that could be coalesced.

**Impact Estimate:** ~5-10% improvement. Two fewer instructions per loop iteration.

**Implementation:**
- Implement register coalescing in the register allocator
- During LIR optimization, track when a destination is immediately moved to another register and target that register directly

**Files to Modify:**
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` (register coalescing)
- `src/DarkCompiler/passes/4.5_LIR_Optimize.fs` (move coalescing)

---

### 3. Speculative Computation of Both Branches (Medium Impact)

**Issue:** Dark computes only one branch per iteration, while Rust speculatively computes both n/2 and 3*n on every iteration.

**Rust Approach:**
```asm
8430: asr  x14, x13, #1           ; ALWAYS compute n/2
8434: add  x15, x13, x13, lsl #1  ; ALWAYS compute 3*n
8440: csinc x13, x14, x15, eq     ; SELECT result
```

**Root Cause:** This is related to issue #2 (conditional select). If-conversion naturally leads to computing both paths.

**Impact:** Included in the conditional select optimization.

---

### 4. Loop-Invariant Code Motion for Constants (Low Impact)

**Issue:** The constant 3 is loaded inside the loop (`mov x1, #0x3`) on every odd iteration.

**Root Cause:** The LIR optimizer doesn't hoist loop-invariant code. The ANF to MIR to LIR pipeline regenerates the constant load each time.

**Impact Estimate:** ~2-5% improvement. Minor because the MOV immediate is fast (1 cycle).

**Implementation:**
- Implement loop-invariant code motion (LICM) in LIR optimizer
- This becomes unnecessary if optimization #1 (shift-add for 3*n) is implemented

**Files to Modify:**
- `src/DarkCompiler/passes/4.5_LIR_Optimize.fs` (LICM pass)

**Status (2026-01-16): Implemented.** Added LIR peephole LICM to hoist loop-invariant
`Mov(Imm ...)` into single-block preheaders in `src/DarkCompiler/passes/4.5_LIR_Peephole.fs`.
This hoists the constant `3` out of `collatzSteps`' odd-path loop body. The pass
now skips loops containing calls or non-arithmetic instructions to avoid extending
live ranges in complex blocks.
Test: `src/Tests/optimization/lir.opt` (`licm_hoist_loop_constant`).

---

## Comparative Summary

| Optimization | Rust | Dark | OCaml |
|-------------|------|------|-------|
| 3*n as shift-add | Yes | Yes | Partial (uses MADD) |
| Conditional select | Yes (CSINC) | **No** | No |
| Branchless inner loop | Yes | **No** | No |
| TCO | N/A (iterative) | Yes | Yes |
| Inline hot function | Yes | Partial | No |
| Register coalescing | Excellent | **Suboptimal** | Good |

## Recommended Implementation Priority

1. **Conditional select for if-expressions** - Medium complexity, high impact
2. **Register coalescing** - Higher complexity, medium impact

## Verification

After implementing optimizations, the expected assembly for `collatzSteps` should look like:
```asm
loop:
    cmp  x3, #1
    b.eq done
    lsr  x4, x3, #1           ; n/2
    add  x5, x3, x3, lsl #1   ; 3*n
    add  x5, x5, #1           ; 3*n+1
    tst  x3, #1
    csel x3, x4, x5, eq       ; select n/2 or 3*n+1
    add  x2, x2, #1           ; steps++
    b    loop
done:
```

Or ideally (full branchless):
```asm
loop:
    lsr  x4, x3, #1           ; n/2
    add  x5, x3, x3, lsl #1   ; 3*n
    tst  x3, #1
    add  x2, x2, #1           ; steps++
    csinc x3, x4, x5, eq      ; n = (even) ? n/2 : 3*n+1
    cmp  x3, #1
    b.ne loop
```

## IR Dump Reference

### ANF (after optimization)
```
Function collatzSteps:
let TempId 2 = t0 == 1
if t2 then
return t1
else
let TempId 3 = t0 & 1        ; Strength reduction: % 2 -> & 1 (good!)
let TempId 4 = t3 == 0
if t4 then
let TempId 5 = t0 >> 1       ; Strength reduction: / 2 -> >> 1 (good!)
let TempId 6 = t1 + 1
let TempId 7 = TailCall(collatzSteps, [t5, t6])
return t7
else
let TempId 8 = 3 * t0        ; NOT strength-reduced!
let TempId 9 = t8 + 1
let TempId 10 = t1 + 1
let TempId 11 = TailCall(collatzSteps, [t9, t10])
return t11
```

### LIR (after register allocation) - collatzSteps_body
```
collatzSteps_body:
    Cmp(X3, Imm 1)
    CondBranch(EQ, Label "collatzSteps_L0", Label "collatzSteps_L1")

collatzSteps_L1:
    BranchBitZero(X3, #0, Label "collatzSteps_L3", Label "collatzSteps_L4")

collatzSteps_L3:  ; even path
    X7 <- Lsr_imm(X3, #1)
    X6 <- Add(X2, Imm 1)
    X3 <- Mov(Reg X7)
    X2 <- Mov(Reg X6)
    Jump(Label "collatzSteps_body")

collatzSteps_L4:  ; odd path
    X1 <- Mov(Imm 3)
    X5 <- Mul(X1, Reg X3)
    X4 <- Add(X5, Imm 1)
    X1 <- Add(X2, Imm 1)
    X3 <- Mov(Reg X4)
    X2 <- Mov(Reg X1)
    Jump(Label "collatzSteps_body")
```
