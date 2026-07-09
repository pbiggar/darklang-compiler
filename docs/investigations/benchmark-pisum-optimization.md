# Pisum Benchmark Optimization Investigation

## Summary

The pisum benchmark computes the partial sum of `1 / k^2` for `k = 1..10000`, repeats that full sum 500 times, and prints the final floating-point result scaled to an integer.

Current benchmark evidence shows Dark is slower than Rust but faster than OCaml on this benchmark:

| Runtime | Instruction count | Relative to Rust |
| --- | ---: | ---: |
| Rust | 45,257,632 | 1.00x |
| Dark | 55,014,671 | 1.22x |
| OCaml | 80,422,857 | 1.78x |

The current gap is concentrated in `innerSum`. Dark does compile the recursive loop as a local branch loop after tail-call detection, so the main remaining evidence is not ordinary recursive call overhead. The current hot-loop differences are:

- Dark converts `k` to float before squaring, while Rust and OCaml square as integers and convert the integer product.
- Dark still emits small no-op or copy-through floating-point moves around phi/tail-call value passing, though this is no longer the primary cost.
- The outer `pisum` loop still calls `innerSum` once per round instead of inlining or fusing the two loops, but that is 500 calls and should be secondary to the 5,000,000 inner iterations.

## Benchmark Shape

Dark source:

```dark
def innerSum(k: Int64, n: Int64, acc: Float) : Float =
    if k > n then
        acc
    else
        let kf = Stdlib.Int64.toFloat(k) in
        innerSum(k + 1, n, acc + 1.0 / (kf * kf))

def pisum(rounds: Int64, n: Int64, lastResult: Float) : Float =
    if rounds <= 0 then
        lastResult
    else
        pisum(rounds - 1, n, innerSum(1, n, 0.0))

Stdlib.Float.toInt(pisum(500, 10000, 0.0) * 1000000000000.0)
```

Rust source uses a counted outer loop and an inclusive counted inner loop:

```rust
fn pisum(n: i64) -> f64 {
    let mut s: f64 = 0.0;
    for _ in 0..500 {
        s = 0.0;
        for k in 1..=n {
            s += 1.0 / ((k * k) as f64);
        }
    }
    s
}
```

OCaml source is similar to Rust, with mutable refs and native tagged integers:

```ocaml
let pisum n =
  let s = ref 0.0 in
  for _ = 0 to 499 do
    s := 0.0;
    for k = 1 to n do
      s := !s +. 1.0 /. float_of_int (k * k)
    done
  done;
  !s
```

The Dark source explicitly writes `let kf = Int64.toFloat(k) in kf * kf`, so the compiler currently preserves a less favorable source shape than the Rust and OCaml programs. Any rewrite from `float(k) * float(k)` to `float(k * k)` must prove integer overflow behavior is acceptable for the source program, not just for this benchmark's `k <= 10000` range.

## Current Dark IR Evidence

After tail-call detection, `innerSum` is a tail call in ANF:

```text
Function innerSum:
let TempId 3 = t0 > t1
if t3 then
return t2
else
let TempId 4 = Int64ToFloat(t0)
let TempId 6 = t0 + 1
let TempId 7 = t4 * t4
let TempId 8 = 1 / t7
let TempId 9 = t2 + t8
let TempId 10 = TailCall(innerSum, [t6, t1, t9])
return t10
```

This already rules out ordinary per-iteration function-call overhead inside the inner loop. The important source-level shape is visible before MIR: Dark converts `t0` to float, squares the float value, and only then divides.

MIR lowers that tail recursion into a loop with phi-like parameter reassignment:

```text
Function innerSum:
  innerSum_L1:
    v4 <- Int64ToFloat(v0)
    v6 <- v0 + 1 : TInt64
    v7 <- v4 * v4 : TFloat64
    v8 <- float[1] / v7 : TFloat64
    v9 <- v2 + v8 : TFloat64
    v12 <- v6 : TInt64
    v13 <- v1 : TInt64
    v14 <- v9 : TFloat64
    v0 <- v12 : TInt64
    v1 <- v13 : TInt64
    v2 <- v14 : TFloat64
    jump innerSum_body
  innerSum_body:
    v3 <- v0 > v1 : TInt64
    branch v3 ? innerSum_L0 : innerSum_L1
```

The MIR loop body still contains `float[1] / v7`. That literal is loop-invariant, but it is not hoisted before LIR/code generation.

After register allocation, the current LIR for the inner loop is compact but still exposes the hot operations:

```text
innerSum:
  Label "innerSum_L1":
    D0 <- Int64ToFloat(X1)
    X1 <- Add(X1, Imm 1)
    D0 <- FMul(D0, D0)
    D2 <- FLoad(float[1])
    D0 <- FDiv(D2, D0)
    D0 <- FAdd(D1, D0)
    D0 <- FMov(D0)
    D1 <- FMov(D0)
    Jump(Label "innerSum_body")
  Label "innerSum_body":
    Cmp(X1, Reg X2)
    CondBranch(GT, Label "innerSum_L2", Label "innerSum_L1")
```

The direct self move `D0 <- FMov(D0)` is visible in the LIR dump but is not present as a separate machine instruction in the emitted binary. The remaining emitted floating move updates the loop-carried accumulator from `d0` into `d1`.

## Current Dark Assembly Evidence

The emitted AArch64 loop for `innerSum` is:

```asm
1dc: scvtf   d0, x1
1e0: add     x1, x1, #0x1
1e4: fmul    d0, d0, d0
1e8: fmov    d2, #1.0
1ec: fdiv    d0, d2, d0
1f0: fadd    d0, d1, d0
1f4: fmov    d1, d0
1f8: b       0x204
204: cmp     x1, x2
208: b.gt    0x1fc
20c: b       0x1dc
```

The emitted loop does not include the LIR no-op `D0 <- FMov(D0)`, so post-register-allocation cleanup has already improved compared with what the raw LIR listing suggests. The emitted `fmov d1, d0` remains because the loop keeps the accumulator in `d1` at the compare block and computes the next value in `d0`.

The outer `pisum` loop still calls `innerSum` once per round:

```asm
250: sub     x19, x19, #0x1
254: mov     x0, #0x1
258: mov     x1, x20
25c: adrp    x9, 0x0
260: add     x9, x9, #0x2a0
264: ldr     d0, [x9]
268: bl      0x1b4
26c: fmov    d8, d0
270: fmov    d0, d8
274: b       0x27c
```

This call-site overhead happens 500 times, while the inner loop body happens about 5,000,000 times. It is worth tracking as an inlining opportunity, but it is unlikely to dominate the current 1.44x Rust gap.

## OCaml Assembly Comparison

OCaml native code generates an integer square before converting to float:

```asm
50278: orr     x6, xzr, #0x1
5027c: asr     x7, x3, #1
50280: sub     x8, x3, #0x1
50284: madd    x9, x8, x7, x6
50288: asr     x10, x9, #1
5028c: scvtf   d3, x10
50290: fmov    d4, #1.0
50294: fdiv    d5, d4, d3
50298: fadd    d0, d0, d5
5029c: mov     x11, x3
502a0: add     x3, x3, #0x2
502a4: cmp     x11, x0
502a8: b.eq    502bc
502ac: ldr     x16, [x28]
502b0: cmp     x27, x16
502b4: b.hi    50278
```

OCaml has tagged-integer adjustments and a GC poll in the loop, yet it still demonstrates the important arithmetic shape: integer product first, then one integer-to-float conversion. OCaml also materializes `1.0` inside the loop, but it uses a floating immediate rather than loading from a literal pool.

Rust could not be rebuilt in this sandbox because `cargo` was not installed, so the current Rust comparison in this document is based on the checked-in Rust source and the current benchmark table rather than a fresh Rust disassembly. The relevant source expression is `((k * k) as f64)`, which also squares before conversion.

## Optimization Opportunities

### 1. Square as integer before converting to float when semantics permit it

Status: open, guarded by overflow semantics.

Dark currently preserves the source shape:

```text
let TempId 4 = Int64ToFloat(t0)
let TempId 7 = t4 * t4
```

Rust and OCaml both express the benchmark as an integer square followed by conversion. For this benchmark's fixed range, `k * k` fits easily in `Int64`, and the integer-first expression would avoid the floating multiply in the current Dark loop.

This should not be implemented as a blanket algebraic rewrite. In general, `float(x) * float(x)` and `float(x * x)` can differ when `x * x` overflows `Int64`, and they can differ in rounding behavior for large values. A safe route would need one of:

- range analysis proving the integer product cannot overflow and preserves the desired rounding behavior,
- a source-level benchmark rewrite if the language-level program is intended to compute integer square before conversion,
- a checked or widened integer multiply path with explicit semantics.

Until one of those exists, this item should remain a benchmark-specific observation and not a general compiler transform.

### 2. Reduce loop-carried floating-point copy pressure

Status: partly improved; only the loop-carried phi/copy shape remains active.

The current LIR contains:

```text
D0 <- FAdd(D1, D0)
D0 <- FMov(D0)
D1 <- FMov(D0)
```

The emitted binary removes the direct no-op `fmov d0, d0`, but it still uses:

```asm
fadd d0, d1, d0
fmov d1, d0
```

The remaining `fmov` is a loop-carried accumulator copy introduced by the current phi/register assignment shape. It may be hard to remove without changing the loop's accumulator allocation. Still, a phi-aware register allocation or post-allocation copy coalescing pass could keep the accumulator in one physical register across the loop.

A rejected adjacent post-register-allocation move cleanup experiment removed local overwrite/self-move patterns in a helper but did not change emitted `pisum` instruction counts. Future work here should target the loop-carried accumulator copy directly, not generic adjacent-move cleanup.

### 3. Consider inlining `innerSum` into `pisum` only after hot-loop work

Status: open but lower priority.

`pisum` calls `innerSum` 500 times, and the call-site has ordinary call and float move overhead. Inlining could expose the two nested loops in one function, reduce entry/exit moves, and potentially make constant placement easier.

This is probably not the first optimization to implement for pisum because the inner loop runs about 10,000 times per outer round. Removing an instruction from the inner loop should matter more than reducing 500 calls. Inlining becomes more interesting after the inner-loop arithmetic shape has been addressed.

## Current Priority

1. Decide whether pisum should be source-shaped as integer square before float conversion, or add compiler analysis strong enough to prove that rewrite sound.
2. Coalesce the remaining loop-carried floating accumulator move if a local register-allocation cleanup can do it without broader churn.
3. Revisit `innerSum` inlining after the hot-loop issues are smaller.

## Evidence Commands

Evidence for this refresh came from:

```bash
./dark --dump-anf benchmarks/problems/pisum/dark/main.dark
./dark --dump-mir benchmarks/problems/pisum/dark/main.dark
./dark --dump-lir benchmarks/problems/pisum/dark/main.dark
./dark -o /tmp/pisum_dark benchmarks/problems/pisum/dark/main.dark
aarch64-linux-gnu-objdump -D -b binary -m aarch64 /tmp/pisum_dark
ocamlopt -O3 -o /tmp/pisum_ocaml benchmarks/problems/pisum/ocaml/main.ml
objdump -d /tmp/pisum_ocaml
```

The benchmark table values are from `benchmarks/RESULTS.md`.
