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
let leibnizLoop(i: Int64, n: Int64, sum: Float, sign: Float) : Float =
    if i >= n then sum * 4.0
    else
        let term = sign / Stdlib.Int64.toFloat(2 * i + 1) in
        leibnizLoop(i + 1, n, sum + term, Stdlib.Float.negate(sign))

let leibnizPi(n: Int64) : Float =
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

## Identified Optimization Opportunities

### 1. Phi Resolution Move Optimization

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

### 2. Tail Call Loop Optimization

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
| Tail Call Loop Optimization | 10-15% | Medium |

## Recommended Implementation Order

1. **Phi Resolution Optimization** - Medium complexity, good payoff
2. **Tail Call Loop Optimization** - Builds on other improvements
