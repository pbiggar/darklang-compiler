# Leibniz Benchmark Optimization Investigation

## Summary

The leibniz benchmark computes pi using the Leibniz formula: `pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...`

**Performance Results:**
- Rust: 0.083s
- Dark: 1.44s
- **Dark is ~17x slower than Rust**

## Benchmark Source Code

### Dark (`benchmarks/problems/leibniz/dark/main.dark`)
```dark
def leibnizLoop(i: Int64, n: Int64, sum: Float, sign: Float) : Float =
    if i >= n then sum * 4.0
    else
        let term = sign / Stdlib.Int64.toFloat(2 * i + 1) in
        leibnizLoop(i + 1, n, sum + term, Stdlib.Float.negate(sign))

def leibnizPi(n: Int64) : Float =
    leibnizLoop(0, n, 0.0, 1.0)

Stdlib.Float.toInt(leibnizPi(100000000) * 100000000.0)
```

### Rust (`benchmarks/problems/leibniz/rust/main.rs`)
```rust
fn leibniz_pi(n: i64) -> f64 {
    let mut s: f64 = 0.0;
    let mut sign: f64 = 1.0;
    for i in 0..n {
        s += sign / (2 * i + 1) as f64;
        sign = -sign;
    }
    s * 4.0
}
```

## Analysis

### Rust Optimized Hot Loop (7 instructions per iteration)

```asm
9378: ucvtf d2, x8        // convert counter to float (counter = 2*i+1)
937c: add x8, x8, #0x2    // increment counter by 2 (next odd number)
9380: cmp x8, x9          // compare with limit
9384: fdiv d2, d1, d2     // sign / divisor
9388: fneg d1, d1         // sign = -sign
938c: fadd d0, d0, d2     // sum += term
9390: b.ne 9378           // loop
```

Key Rust optimization: LLVM performs **induction variable optimization**. Instead of computing `2*i+1` each iteration, it keeps a counter that starts at 1 and increments by 2. This eliminates the multiply and add operations.

### Current Dark LIR Hot Loop

```
Label "leibnizLoop_L1":
    X1 <- Lsl_imm(X2, #1)    // 2 * i
    X1 <- Add(X1, Imm 1)     // 2 * i + 1
    D0 <- IntToFloat(X1)     // convert to float
    D0 <- FDiv(D2, D0)       // sign / divisor
    X2 <- Add(X2, Imm 1)     // i + 1
    D1 <- FAdd(D1, D0)       // sum += term
    D0 <- FNeg(D2)           // -sign
    // REMAINING REDUNDANT MOVE:
    D2 <- FMov(D0)           // unnecessary double-move chain
    Jump(Label "leibnizLoop_body")
```

## Identified Optimization Opportunities

### 1. Induction Variable Optimization / Loop Strength Reduction

**Impact: ~15-20% performance improvement (estimated)**

**Root Cause:**
The pattern `2 * i + 1` inside a loop where `i` increments by 1 each iteration is a classic induction variable optimization opportunity. Currently Dark computes:
- `X1 <- Lsl_imm(X2, #1)` (2 * i)
- `X1 <- Add(X1, Imm 1)` (2 * i + 1)

**Evidence from IR:**
```
ANF after optimization:
let TempId 6 = t0 << 1
let TempId 7 = t6 + 1
```

**Optimal Code:**
Instead of computing `2*i+1` each iteration, maintain a separate counter `j` that starts at 1 and increments by 2:
```
// Before loop: j = 1
// In loop: use j instead of 2*i+1, then j += 2
```

This eliminates 2 integer operations per iteration.

**Implementation Approach:**
Add an induction variable detection pass in `3.5_MIR_Optimize.fs` that:
1. Detects linear induction variables (variables of the form `a*i + b` where `i` is the loop counter)
2. Creates a derived induction variable for each such expression
3. Initializes the derived variable before the loop
4. Increments it by `a` (the coefficient) each iteration instead of recomputing

**Files to Modify:**
- `src/DarkCompiler/passes/3.5_MIR_Optimize.fs` - Add induction variable optimization pass

---

### 2. Phi Resolution Move Optimization

**Impact: ~20-30% performance improvement (estimated)**

**Root Cause:**
When converting out of SSA form, phi nodes need to be resolved into parallel moves. The current implementation generates sequential moves even when registers don't interfere, and fails to optimize common patterns.

**Evidence from MIR:**
```
leibnizLoop_L1:
    ...
    v16 <- v11 : TInt64      // Copy v11 to v16
    v17 <- v1 : TInt64       // Copy v1 to v17 (but v1 is loop-invariant!)
    v18 <- v12 : TFloat64
    v19 <- v13 : TFloat64
    v0 <- v16 : TInt64       // Copy v16 to v0
    v1 <- v17 : TInt64       // Copy v17 to v1 (copying back the same value!)
    v2 <- v18 : TFloat64
    v3 <- v19 : TFloat64
```

The loop-invariant value `v1` (the loop bound `n`) is being copied through intermediate registers unnecessarily. This generates 4 moves when 0 are needed.

**Implementation Approach:**
1. In phi resolution, detect when a phi operand is loop-invariant and skip the move
2. Optimize parallel move sequences to use fewer temporaries
3. Consider using `ParallelMoves.fs` more effectively to minimize move count

**Files to Modify:**
- `src/DarkCompiler/passes/4_MIR_to_LIR.fs` - Phi resolution
- `src/DarkCompiler/ParallelMoves.fs` - Move sequence optimization

---

### 3. Tail Call Loop Optimization

**Impact: ~10-15% performance improvement (estimated)**

**Root Cause:**
The tail call is correctly detected and converted to a loop, but the parameter shuffling for each iteration generates excessive moves. In Rust, LLVM completely eliminates the function call overhead and fuses the loop body.

**Evidence from ANF (after Tail Call Detection):**
```
let TempId 14 = TailCall(leibnizLoop, [t11, t1, t12, t13])
```

This is correctly detected as a tail call. However, the conversion to a loop in MIR generates unnecessary parameter copying.

**Implementation Approach:**
1. Detect when tail-call parameters can be computed in-place
2. For parameters that don't depend on each other, allow direct assignment
3. Use register coalescing to make the loop variables and parameters share registers

**Files to Modify:**
- `src/DarkCompiler/passes/3_ANF_to_MIR.fs` - Tail call to loop conversion
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Coalescing preferences

---

## Summary of Expected Improvements

| Optimization | Estimated Impact | Complexity |
|--------------|-----------------|------------|
| Phi Resolution Optimization | 20-30% | Medium |
| Induction Variable Optimization | 15-20% | High |
| Tail Call Loop Optimization | 10-15% | Medium |

**Total estimated improvement: 2-3x faster** (bringing Dark closer to Rust's performance)

## Recommended Implementation Order

1. **Phi Resolution Optimization** - Medium complexity, good payoff
2. **Induction Variable Optimization** - Higher complexity but important for numerical code
3. **Tail Call Loop Optimization** - Builds on other improvements

## Appendix: Full IR Dumps

### Dark ANF (before optimization)
```
Function leibnizLoop:
let TempId 4 = t0 >= t1
if t4 then
let TempId 5 = t2 * 4
return t5
else
let TempId 6 = 2 * t0
let TempId 7 = t6 + 1
let TempId 8 = IntToFloat(t7)
let TempId 9 = t3 / t8
let TempId 10 = t9
let TempId 11 = t0 + 1
let TempId 12 = t2 + t10
let TempId 13 = FloatNeg(t3)
let TempId 14 = leibnizLoop(t11, t1, t12, t13)
return t14
```

### Dark ANF (after optimization)
```
Function leibnizLoop:
let TempId 4 = t0 >= t1
if t4 then
let TempId 5 = t2 * 4
return t5
else
let TempId 6 = t0 << 1      // Strength reduction: 2*i -> i<<1
let TempId 7 = t6 + 1
let TempId 8 = IntToFloat(t7)
let TempId 9 = t3 / t8
let TempId 11 = t0 + 1
let TempId 12 = t2 + t9
let TempId 13 = FloatNeg(t3)
let TempId 14 = TailCall(leibnizLoop, [t11, t1, t12, t13])
return t14
```

### Dark MIR (Control Flow Graph)
```
Function leibnizLoop:
  leibnizLoop_entry:
    jump leibnizLoop_body
  leibnizLoop_L0:
    v5 <- v2 * float[4] : TFloat64
    v15 <- v5 : TFloat64
    jump leibnizLoop_L2
  leibnizLoop_L1:
    v6 <- v0 << 1 : TInt64
    v7 <- v6 + 1 : TInt64
    v8 <- IntToFloat(v7)
    v9 <- v3 / v8 : TFloat64
    v11 <- v0 + 1 : TInt64
    v12 <- v2 + v9 : TFloat64
    v13 <- FloatNeg(v3)
    v16 <- v11 : TInt64
    v17 <- v1 : TInt64
    v18 <- v12 : TFloat64
    v19 <- v13 : TFloat64
    v0 <- v16 : TInt64
    v1 <- v17 : TInt64
    v2 <- v18 : TFloat64
    v3 <- v19 : TFloat64
    jump leibnizLoop_body
  leibnizLoop_L2:
    ret v15
  leibnizLoop_body:
    v4 <- v0 >= v1 : TInt64
    branch v4 ? leibnizLoop_L0 : leibnizLoop_L1
```
