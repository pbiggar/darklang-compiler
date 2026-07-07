# Mandelbrot Benchmark Optimization Investigation

## Executive Summary

The Dark compiler currently executes **2.06x more instructions than Rust** and **1.10x more instructions than OCaml** for the mandelbrot benchmark. The primary causes are:

1. **Redundant tail-call phi copy traffic in the iterate loop** (estimated 10-20% improvement potential)
2. **Repeated float constant loads** (estimated 5-10% improvement potential)
3. **Missing fused multiply-add (FMA) instructions** (estimated 10-15% improvement potential)

## Benchmark Results

Cachegrind instruction counts from commit `633dca3d`:

| Language | Instructions | vs Rust |
|----------|--------------|---------|
| Rust     | 12,553,096 | 1.00x |
| OCaml    | 23,390,326 | 1.86x |
| Dark     | 25,826,540 | 2.06x |

## Hot Loop Analysis

### Dark Compiler - `iterate` Function

The `iterate` function is the innermost loop, called ~2 million times (200 * 200 * ~50 iterations average).

**Current LIR after Register Allocation (iterate_L4 - the hot continue path):**
```
Label "iterate_L4":
    D0 <- FSub(D0, D1)
    D1 <- FAdd(D0, D5)
    D0 <- FAdd(D3, D3)               ; strength-reduced 2.0 * zr
    D0 <- FMul(D0, D2)
    D0 <- FAdd(D0, D4)
    X1 <- Add(X1, Imm 1)
    D3 <- FMov(D5)
    D2 <- FMov(D4)
    D1 <- FMov(D1)                   ; PROBLEM: Self-move
    D0 <- FMov(D0)                   ; PROBLEM: Self-move
    D5 <- FMov(D3)
    D4 <- FMov(D2)
    D3 <- FMov(D1)
    D2 <- FMov(D0)
    Jump(Label "iterate_body")
```

**Instruction count in iterate hot path:** ~16 LIR instructions after register allocation
**Redundant instructions:** 2 direct self-moves in LIR plus copy traffic for tail-call phi resolution
**Effective useful instructions:** ~14 after direct self-moves are removed during encoding

**Current emitted assembly for the hot continue path:**
```asm
4001fc: fsub  d0, d0, d1
400200: fadd  d1, d0, d5
400204: fadd  d0, d3, d3
400208: fmul  d0, d0, d2
40020c: fadd  d0, d0, d4
400210: add   x1, x1, #0x1
400214: fmov  d3, d5
400218: fmov  d2, d4
40021c: fmov  d5, d3
400220: fmov  d4, d2
400224: fmov  d3, d1
400228: fmov  d2, d0
40022c: b     0x400238
```

The direct `D1 <- FMov(D1)` and `D0 <- FMov(D0)` LIR instructions are not emitted, but the sequential phi-resolution copy chain still produces redundant `fmov` traffic. The pair `d3 <- d5; d5 <- d3` and the pair `d2 <- d4; d4 <- d2` preserve invariant `cr`/`ci` values while the destination registers are overwritten again by the new `zr`/`zi` values.

### Rust Compiler - Inlined `mandelbrot` Function

Rust aggressively inlines and optimizes:
```asm
8464:   1e6708f0    fmul  d16, d7, d7           ; zr * zr
8468:   1e6608d1    fmul  d17, d6, d6           ; zi * zi
846c:   1e712a12    fadd  d18, d16, d17         ; zr2 + zi2
8470:   1e622240    fcmp  d18, d2               ; compare with 4.0 (constant in register!)
8474:   54fffe2c    b.gt  8438                  ; escape check
8478:   1e6728e7    fadd  d7, d7, d7            ; 2.0 * zr (strength reduction!)
847c:   1e713a10    fsub  d16, d16, d17         ; zr2 - zi2
8480:   7100056b    subs  w11, w11, #0x1        ; counter decrement
8484:   1e6708c6    fmul  d6, d6, d7            ; zi * (2*zr)
8488:   1e7028a7    fadd  d7, d5, d16           ; new_zr = cr + (zr2 - zi2)
848c:   1e662886    fadd  d6, d4, d6            ; new_zi = ci + zi*(2*zr)
8490:   54fffea1    b.ne  8464                  ; loop
```

**Rust hot loop instruction count:** ~11 instructions
**Key optimizations:**
- `2.0 * zr` optimized to `zr + zr` (strength reduction)
- Float constant 4.0 kept in register `d2`
- No redundant moves
- Counter-based loop (not tail recursion)

### OCaml Compiler - `camlMain__mandelbrot_267`

```asm
4eae8:  1e610825    fmul  d5, d1, d1            ; zr * zr
4eaec:  1f421446    fmadd d6, d2, d2, d5        ; FMADD! zi*zi + zr2
4eaf0:  1e6420c0    fcmp  d6, d4                ; compare
4eaf4:  5400006d    b.le  4eb00                 ; continue if <= 4.0
...
4eb04:  1f429449    fmsub d9, d2, d2, d5        ; FMSUB! zr2 - zi*zi
4eb08:  1e67292a    fadd  d10, d9, d7           ; + cr
4eb0c:  1e60100b    fmov  d11, #2.0             ; constant (could be hoisted)
4eb10:  1e61096c    fmul  d12, d11, d1          ; 2.0 * zr
4eb18:  1f423582    fmadd d2, d12, d2, d13      ; FMADD! new_zi
```

**OCaml key advantage:** Uses FMADD/FMSUB instructions for fused multiply-add

## Optimization Opportunities

### 1. Dead Store Elimination in Phi Resolution (High Impact: ~10-20%)

**Problem:** The register allocator generates redundant moves when resolving phi nodes.

**Current evidence (LIR after RegAlloc, iterate_L4):**
```
D1 <- FMov(D1)     ; self-move
D0 <- FMov(D0)     ; self-move
D3 <- FMov(D5)
D2 <- FMov(D4)
D5 <- FMov(D3)
D4 <- FMov(D2)
```

**Root Cause:** Phi resolution in `iterate_L4` generates moves for variables that will flow to `iterate_body`, but the register allocator still does not coalesce the floating-point copy chain effectively. Direct self-moves remain visible in LIR, while the emitted assembly shows the larger remaining cost is redundant copy traffic around invariant `cr`/`ci` values.

**Implementation Approach:**
1. Add a post-regalloc pass to eliminate:
   - Self-moves: `Xn <- Mov(Reg Xn)`
   - Consecutive overwrites to same register
2. Improve phi node parallel copy sequencing

**Files to modify:**
- `src/DarkCompiler/RegAlloc.fs` - Add move coalescing
- `src/DarkCompiler/LIR.fs` - Add dead store elimination pass

### 2. Float Constant Hoisting (Medium Impact: ~5-10%)

**Problem:** Float constants are loaded from memory inside the loop.

**Evidence (LIR iterate_L1 and iterate_L4):**
```
D7 <- FLoad(float[4])        ; Loaded every iteration for escape check
```

Rust keeps 4.0 in `d2` register throughout the entire function. The earlier `2.0` load in the hot loop is gone because strength reduction now emits `FAdd`.

**Implementation Approach:**
1. Identify float constants used in loops
2. Hoist loads to function entry or loop preheader
3. Allocate a dedicated register for frequently-used constants

**Files to modify:**
- `src/DarkCompiler/LIROptimizations.fs` - Add constant hoisting pass
- `src/DarkCompiler/MIRToLIR.fs` - Improve constant handling

### 3. Fused Multiply-Add Instructions (Medium Impact: ~10-15%)

**Problem:** Dark generates separate FMUL+FADD sequences where FMADD would be faster and more accurate.

**Evidence - Dark LIR (iterate_L1 and iterate_L4):**
```
D0 <- FMul(D3, D3)
D1 <- FMul(D2, D2)
D6 <- FAdd(D0, D1)   ; could use FMADD for zr*zr + zi*zi
...
D0 <- FSub(D0, D1)   ; could use FMSUB for zr*zr - zi*zi
```

**Evidence - OCaml uses FMADD:**
```asm
4eaec:  1f421446    fmadd d6, d2, d2, d5   ; d6 = d2*d2 + d5 in one instruction
4eb04:  1f429449    fmsub d9, d2, d2, d5   ; d9 = d2*d2 - d5 in one instruction
```

**Pattern to detect:**
```
t1 = a * b
t2 = t1 + c   (or t1 - c)
; Can become: FMADD t2, a, b, c (or FMSUB)
```

**Implementation Approach:**
1. Add pattern matching in LIR optimization pass to detect FMul followed by FAdd/FSub
2. Replace with FMADD/FMSUB when the multiply result is only used by the add
3. Add new LIR instructions: `FMAdd`, `FMSub`

**Files to modify:**
- `src/DarkCompiler/LIR.fs` - Add FMAdd/FMSub instructions
- `src/DarkCompiler/LIROptimizations.fs` - Add fusion pattern matching
- `src/DarkCompiler/CodeGen.fs` - Emit FMADD/FMSUB encodings
- `src/DarkCompiler/ARM64Encoding.fs` - Encode FMA instructions

## Summary of Optimization Impact

| Optimization | Estimated Impact | Complexity |
|-------------|------------------|------------|
| Dead store elimination | 10-20% | Medium |
| Constant hoisting | 5-10% | Low |
| FMA instructions | 10-15% | Medium |
| **Remaining potential** | **~25-45%** | |

If all optimizations are implemented, Dark could approach or match Rust performance on this benchmark.

## Appendix: Full IR Dumps

### Dark ANF (after optimization)

```
Function iterate:
let TempId 6 = t4 >= t5
if t6 then
  return 1
else
  let TempId 7 = t2 * t2
  let TempId 9 = t3 * t3
  let TempId 11 = t7 + t9
  let TempId 12 = t11 > 4
  if t12 then
    return 0
  else
    let TempId 13 = t7 - t9
    let TempId 14 = t13 + t0
    let TempId 16 = t2 + t2
    let TempId 17 = t16 * t3
    let TempId 18 = t17 + t1
    let TempId 20 = t4 + 1
    TailCall(iterate, [t0, t1, t14, t18, t20, t5])
```

### Dark MIR (iterate function)

```
Function iterate:
  iterate_entry:
    jump iterate_body
  iterate_L0:
    v22 <- 1 : TInt64
    jump iterate_L2
  iterate_L1:
    v7 <- v2 * v2 : TFloat64
    v9 <- v3 * v3 : TFloat64
    v11 <- v7 + v9 : TFloat64
    v12 <- v11 > float[4] : TFloat64
    branch v12 ? iterate_L3 : iterate_L4
  iterate_L2:
    ret v22
  iterate_L3:
    v23 <- 0 : TInt64
    jump iterate_L5
  iterate_L4:
    v13 <- v7 - v9 : TFloat64
    v14 <- v13 + v0 : TFloat64
    v16 <- v2 + v2 : TFloat64
    v17 <- v16 * v3 : TFloat64
    v18 <- v17 + v1 : TFloat64
    v20 <- v4 + 1 : TInt64
    [phi resolution moves...]
    jump iterate_body
  iterate_L5:
    v22 <- v23 : TInt64
    jump iterate_L2
  iterate_body:
    v6 <- v4 >= v5 : TInt64
    branch v6 ? iterate_L0 : iterate_L1
```
