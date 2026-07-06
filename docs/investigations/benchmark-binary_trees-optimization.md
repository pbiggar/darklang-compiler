# Binary Trees Benchmark Optimization Investigation

## Current Status

As of commit `89c36312`, `binary_trees` runs correctly and reports
`6553500`. The current cachegrind run records Dark at `154,007,725`
instructions, compared with OCaml at `82,339,690` and Rust at
`1,842,791,955`.

Dark remains about `1.87x` OCaml's instruction count, but it is much faster
than the Rust reference because the Rust benchmark builds and traverses heap
tree nodes while the Dark and OCaml programs recursively count complete-tree
nodes.

| Language | Instructions | Relative to Rust |
| -------- | ------------ | ---------------- |
| OCaml | 82,339,690 | 0.04x |
| Dark | 154,007,725 | 0.08x |
| Rust | 1,842,791,955 | baseline |

## Source Shape

The runtime path uses `countTree` and `stressTest`; the `makeTree` tuple
allocator remains present in the source and early IR, but is not called by
`_start`.

```dark
def countTree(depth: Int64) : Int64 =
    if depth <= 0 then 1
    else 1 + countTree(depth - 1) + countTree(depth - 1)

def stressTest(depth: Int64, iterations: Int64, acc: Int64) : Int64 =
    if iterations <= 0 then acc
    else
        let count = countTree(depth) in
        stressTest(depth, iterations - 1, acc + count)

stressTest(15, 100, 0)
```

## Current IR Evidence

The ANF optimizer still leaves the two `depth - 1` expressions visible in
`countTree`:

```text
Function countTree:
let TempId 22 = t20 - 1
let TempId 23 = countTree(t22)
let TempId 24 = 1 + t23
let TempId 25 = t20 - 1
let TempId 26 = countTree(t25)
let TempId 27 = t24 + t26
return t27
```

This is not currently a hot-code issue after MIR/LIR optimization. The LIR
before register allocation already reuses a single lowered value for both
recursive calls:

```text
Label "countTree_L1":
  v10030 <- Sub(v20, Imm 1)
  ArgMoves(X0 <- Reg v10030)
  v10031 <- Call(countTree, [Reg v10030])
  ...
  ArgMoves(X0 <- Reg v10030)
  v10034 <- Call(countTree, [Reg v10030])
```

After register allocation, `countTree` keeps the decremented depth in `X19`
across both calls:

```text
Label "countTree_L1":
  X19 <- Sub(X19, Imm 1)
  ArgMoves(X0 <- Reg X19)
  X20 <- Call(countTree, [Reg X19])
  ...
  ArgMoves(X0 <- Reg X19)
  X19 <- Call(countTree, [Reg X19])
```

The previous investigation's tail-call phi concern is also resolved in current
LIR. `stressTest` now lowers to a compact loop without redundant self-overwrite
moves:

```text
Label "stressTest_L1":
  ArgMoves(X0 <- Reg X19)
  X22 <- Call(countTree, [Reg X19])
  X20 <- Sub(X20, Imm 1)
  X21 <- Add(X21, Reg X22)
  Jump(Label "stressTest_body")

Label "stressTest_body":
  Cmp(X20, Imm 0)
  CondBranch(LE, Label "stressTest_L2", Label "stressTest_L1")

Label "stressTest_entry":
  X19 <- Mov(Reg X0)
  X20 <- Mov(Reg X1)
  X21 <- Mov(Reg X2)
  Jump(Label "stressTest_body")
```

The previous entry-shuffle issue is likewise resolved at the LIR level:
`countTree_entry` contains only `X19 <- Mov(Reg X0)` before jumping to the body,
not the older `x0 -> temp -> x0 -> worker` sequence.

## Remaining Optimization Opportunities

### 1. Hot Recursive Calling Convention Pressure

**Status:** Open.

`countTree` uses callee-saved registers `X19`, `X20`, and `X21` for a tiny
recursive function. The generated LIR is clean, but every recursive call still
has to preserve enough state to combine the two child counts. The OCaml native
code for the same function uses stack slots around the two recursive calls and
has roughly half Dark's dynamic instruction count for the full benchmark.

This points to register-allocation and calling-convention overhead in recursive
integer functions as the highest-value remaining area, not phi resolution or
entry argument shuffling.

**Evidence to inspect next:**

- concrete emitted prologue/epilogue once the generated ELF can be disassembled
  in this environment,
- whether allocating the hot temporary/result values to caller-saved registers
  reduces save/restore traffic,
- whether recursive self-calls could use a specialized internal convention.

**Likely files:**

- `src/DarkCompiler/passes/5_RegisterAllocation.fs`
- `src/DarkCompiler/passes/arm64/6_CodeGen.fs`

## Corrected Findings

The earlier version of this investigation listed tail-call phi-node resolution
and entry-point register shuffling as active `binary_trees` issues. Current
compiler evidence no longer supports those claims:

| Finding | Current status | Evidence |
| ------- | -------------- | -------- |
| Tail-call phi overwrite moves | Resolved | `stressTest_L1` jumps directly to `stressTest_body` after updating `X20` and `X21`. |
| Entry argument shuffle through temporaries | Resolved at LIR | Function entries move arguments directly into worker registers. |
| `depth - 1` duplicate in hot recursive calls | Resolved after ANF | ANF shows duplicate expressions, but LIR reuses one lowered subtraction for both calls. |

## Validation Notes

Evidence gathered in this pass:

- `./dark -vvv --dump-anf --dump-mir --dump-lir benchmarks/problems/binary_trees/dark/main.dark -o /tmp/binary_trees_dark`
- `/tmp/binary_trees_dark` produced `6553500`
- `./benchmarks/run_benchmarks.sh binary_trees`
- `ocamlopt -O3 -o /tmp/binary_trees_ocaml benchmarks/problems/binary_trees/ocaml/main.ml`

Rust comparison used the cached benchmark baseline because `rustc` was not
installed in this sandbox.
