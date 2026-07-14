# Collatz Benchmark Optimization Investigation

## Executive Summary

Current local evidence shows the Collatz benchmark is no longer a broad arithmetic
strength-reduction problem. The current compiler already lowers `% 2` to `& 1`,
`/ 2` to `>> 1`, and `3 * n` to ARM64 shift-add form after register allocation.
The remaining hot-loop gap is control flow: Dark still emits separate even and
odd loop paths, while the best reference shape computes both candidate next
values and selects branchlessly.

The current focused Cachegrind refresh reports Dark at **81,441,905
instructions (0.83x Rust)** for Collatz, with Rust at **98,242,566
instructions** and OCaml at **259,003,323 instructions (2.6x Rust)**. That keeps
the remaining opportunity narrower than the older timing notes implied.

## Current Benchmark Context

From a focused local
`./benchmarks/run_benchmarks.sh --refresh-baseline=rust,ocaml collatz` run on
commit `16719658`:

| Language | Instruction count | Relative to Rust |
|----------|-------------------|------------------|
| Rust     | 98,242,566        | 1.00x            |
| Dark     | 81,441,905        | 0.83x            |
| OCaml    | 259,003,323       | 2.6x             |

All implementations produce the expected output:

```text
10753840
```

The same Cachegrind run reported 21,707,694 Dark branches with a 16.7%
misprediction rate for Collatz, which is consistent with the remaining
parity-branch opportunity.

## Source Shape

The Dark benchmark is tail-recursive:

```dark
def collatzSteps(n: Int64, steps: Int64) : Int64 =
    if n == 1 then steps
    else if n % 2 == 0 then collatzSteps(n / 2, steps + 1)
    else collatzSteps(3 * n + 1, steps + 1)
```

The Rust benchmark is iterative:

```rust
fn collatz_steps(mut n: i64) -> i64 {
    let mut steps = 0;
    while n != 1 {
        if n % 2 == 0 {
            n = n / 2;
        } else {
            n = 3 * n + 1;
        }
        steps += 1;
    }
    steps
}
```

The source-level recursion difference is not the current bottleneck by itself:
tail-call detection turns both recursive Dark calls into jumps back to the
function loop.

## Current Compiler Evidence

`./dark --dump-anf benchmarks/problems/collatz/dark/main.dark` shows that the
integer power-of-two reductions are already present before lowering:

```text
Function collatzSteps:
let TempId 2 = t0 == 1
if t2 then
return t1
else
let TempId 3 = t0 & 1
let TempId 4 = t3 == 0
if t4 then
let TempId 5 = t0 >> 1
let TempId 6 = t1 + 1
let TempId 7 = TailCall(collatzSteps, [t5, t6])
return t7
else
let TempId 8 = 3 * t0
let TempId 9 = t8 + 1
let TempId 10 = t1 + 1
let TempId 11 = TailCall(collatzSteps, [t9, t10])
return t11
```

`./dark --dump-lir benchmarks/problems/collatz/dark/main.dark` shows that
post-allocation LIR has also removed the older same-loop register-copy issue in
`collatzSteps`, strength-reduced `3 * n` to shift-add form, and eliminated stack
usage in the hot recursive helper:

```text
collatzSteps:
  StackSize: 0
  UsedCalleeSaved: []
  Label "collatzSteps_L1":

    BranchBitZero(X1, #0, Label "collatzSteps_L3", Label "collatzSteps_L4")
  Label "collatzSteps_L3":
    X1 <- Lsr_imm(X1, #1)
    X2 <- Add(X2, Imm 1)
    Jump(Label "collatzSteps_body")
  Label "collatzSteps_L4":
    X3 <- Lsl_imm(X1, #1)
    X1 <- Add(X1, Reg X3)
    X1 <- Add(X1, Imm 1)
    X2 <- Add(X2, Imm 1)
    Jump(Label "collatzSteps_body")
  Label "collatzSteps_body":
    Cmp(X1, Imm 1)
    CondBranch(EQ, Label "collatzSteps_L0", Label "collatzSteps_L1")
```

The generated sectionless ELF can be inspected as a raw binary:

```bash
aarch64-linux-gnu-objdump -D -b binary -m aarch64 /tmp/collatz.dark.out
```

The hot `collatzSteps` region matches the LIR shape:

```asm
1c4: 36000041  tbz  w1, #0, 0x1cc
1c8: 14000004  b    0x1d8
1cc: d341fc21  lsr  x1, x1, #1
1d0: 91000442  add  x2, x2, #0x1
1d4: 14000004  b    0x1e4
1d8: 8b010421  add  x1, x1, x1, lsl #1
1dc: 91000421  add  x1, x1, #0x1
1e0: 91000442  add  x2, x2, #0x1
1e4: f100043f  cmp  x1, #0x1
1e8: 54fffea0  b.eq 0x1bc
1ec: 17fffff6  b    0x1c4
```

## Optimization Opportunities

### 1. If-convert the parity diamond to conditional select

The remaining hot path still branches on parity, has an extra unconditional jump
from the odd test block, and duplicates the `steps + 1` increment in both arms.
An if-conversion pass could compute both next-`n` candidates and use `CSEL` or
`CSINC`-style lowering to choose the next value.

Expected target shape:

```asm
loop:
    lsr  x_next_even, x_n, #1
    add  x_next_odd, x_n, x_n, lsl #1
    add  x_next_odd, x_next_odd, #1
    tst  x_n, #1
    csel x_n, x_next_even, x_next_odd, eq
    add  x_steps, x_steps, #1
    cmp  x_n, #1
    b.ne loop
```

This should be treated as a targeted if-conversion opportunity, not as evidence
that arithmetic strength reduction is missing.

Likely implementation areas:

- `src/DarkCompiler/passes/3.5_MIR_Optimize.fs` for diamond recognition, or
- `src/DarkCompiler/passes/4.5_LIR_Peephole.fs` for a narrower LIR-level pattern,
- `src/DarkCompiler/LIR.fs`, `src/DarkCompiler/ARM64.fs`, and ARM64 codegen if a
  first-class conditional-select LIR instruction is needed.

### 2. Consider branch-local commoning of `steps + 1`

Both parity arms independently emit:

```text
X2 <- Add(X2, Imm 1)
```

If full if-conversion is too broad, a narrower transformation could sink or
hoist the common step increment around the parity diamond. This would remove one
duplicated instruction from the static loop body, but it would not remove the
unpredictable parity branch. It is therefore lower priority than conditional
select.

## Implemented or No Longer Current

- `% 2 -> & 1` is implemented in optimized ANF.
- `/ 2 -> >> 1` is implemented in optimized ANF.
- `3 * n` is lowered to `add x, x, x, lsl #1` after register allocation and in
  emitted ARM64 assembly.
- Tail calls in both `collatzSteps` and `sumCollatzRange` are lowered to local
  loop jumps, so the benchmark source comment about needing TCO is stale with
  respect to current compiler behavior.
- The older constant-load issue for `3` in the odd path is gone from current
  post-allocation LIR and emitted assembly.
- The older same-loop register moves in `collatzSteps` are gone from current
  post-allocation LIR.

## Verification Commands Used

```bash
./dark --dump-anf benchmarks/problems/collatz/dark/main.dark
./dark --dump-lir benchmarks/problems/collatz/dark/main.dark
./dark -o /tmp/collatz.dark.out benchmarks/problems/collatz/dark/main.dark
/tmp/collatz.dark.out
aarch64-linux-gnu-objdump -D -b binary -m aarch64 /tmp/collatz.dark.out
./benchmarks/run_benchmarks.sh --refresh-baseline=rust,ocaml collatz
```
