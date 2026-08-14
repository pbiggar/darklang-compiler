# Ackermann Benchmark Optimization Investigation

## Summary

The ackermann benchmark computes `A(3, 12) = 32765` using the classic Ackermann function. It is mainly a stress test for recursive call overhead, tail-call lowering, and register movement in tight recursive paths.

**Current performance context from `benchmarks/RESULTS.md` (2026-07-04):**
- Rust: 5,009,839,130 instructions (baseline)
- Dark: 11,450,298,027 instructions (2.29x Rust)
- OCaml: 8,946,136,766 instructions (1.79x Rust)
- Node: 3,094,160,411 instructions (0.62x Rust)
- Overall table header: Dark geometric performance ratio is 4.07x Rust

Dark remains slower than Rust and OCaml on this benchmark, but several older notes in this investigation are now implemented or partially implemented. Current LIR shows the former nine-instruction entry shuffle has been reduced to two argument moves, redundant post-register-allocation self moves are removed, the unreachable `ackermann_L5` block is gone by post-register-allocation LIR, and ARM64 `BranchZero` is lowered through `CBZ`.

## Benchmark Source Code

### Dark (`benchmarks/problems/ackermann/dark/main.dark`)

```dark
let ackermann(m: Int64, n: Int64) : Int64 =
    if m == 0 then n + 1
    else if n == 0 then ackermann(m - 1, 1)
    else ackermann(m - 1, ackermann(m, n - 1))

ackermann(3, 12)
```

### Rust (`benchmarks/problems/ackermann/rust/main.rs`)

```rust
fn ackermann(m: i64, n: i64) -> i64 {
    if m == 0 {
        n + 1
    } else if n == 0 {
        ackermann(m - 1, 1)
    } else {
        ackermann(m - 1, ackermann(m, n - 1))
    }
}

fn main() {
    let result = ackermann(3, 12);
    println!("{}", result);
}
```

### OCaml (`benchmarks/problems/ackermann/ocaml/main.ml`)

```ocaml
let rec ackermann m n =
  if m = 0 then n + 1
  else if n = 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))

let () = Printf.printf "%d\n" (ackermann 3 12)
```

## Current Compiler Evidence

### MIR

`./dark --dump-mir benchmarks/problems/ackermann/dark/main.dark` shows both syntactic tail calls have been converted into loops back to `ackermann_body`. The inner call in the nested-recursive case remains a normal call, as expected.

```text
Function ackermann:
  ackermann_entry:
    jump ackermann_body
  ackermann_L0:
    v3 <- v1 + 1 : TInt64
    v11 <- v3 : TInt64
    jump ackermann_L2
  ackermann_L1:
    v4 <- v1 == 0 : TInt64
    branch v4 ? ackermann_L3 : ackermann_L4
  ackermann_L2:
    ret v11
  ackermann_L3:
    v5 <- v0 - 1 : TInt64
    v0 <- v5 : TInt64
    v1 <- 1 : TInt64
    jump ackermann_body
  ackermann_L4:
    v7 <- v0 - 1 : TInt64
    v8 <- v1 - 1 : TInt64
    v9 <- Call(ackermann, [v0, v8])
    v0 <- v7 : TInt64
    v1 <- v9 : TInt64
    jump ackermann_body
  ackermann_L5:
    v11 <- v12 : TInt64
    jump ackermann_L2
  ackermann_body:
    v2 <- v0 == 0 : TInt64
    branch v2 ? ackermann_L0 : ackermann_L1
```

`ackermann_L5` is still present in `--dump-mir` and has no apparent incoming
edge, but it no longer survives to post-register-allocation LIR for this
benchmark.

### LIR After Register Allocation

`./dark --dump-lir benchmarks/problems/ackermann/dark/main.dark` now emits a much smaller entry sequence than the previous nine-move cycle:

```text
ackermann:
  StackSize: 0
  UsedCalleeSaved: [X19, X20, X21]
  Label "ackermann_L0":
    X19 <- Add(X19, Imm 1)
    X0 <- Mov(Reg X19)
    Ret
  Label "ackermann_L1":

    BranchZero(X19, Label "ackermann_L3", Label "ackermann_L4")
  Label "ackermann_L3":
    X20 <- Sub(X20, Imm 1)
    X19 <- Mov(Imm 1)
    Jump(Label "ackermann_body")
  Label "ackermann_L4":
    X21 <- Sub(X20, Imm 1)
    X19 <- Sub(X19, Imm 1)
    SaveRegs([], [])
    ArgMoves(X0 <- Reg X20, X1 <- Reg X19)
    X19 <- Call(ackermann, [Reg X20, Reg X19])
    RestoreRegs([], [])
    X19 <- Mov(Reg X0)
    X20 <- Mov(Reg X21)
    Jump(Label "ackermann_body")
  Label "ackermann_body":

    BranchZero(X20, Label "ackermann_L0", Label "ackermann_L1")
  Label "ackermann_entry":
    X20 <- Mov(Reg X0)
    X19 <- Mov(Reg X1)
    Jump(Label "ackermann_body")
```

Remaining local inefficiencies visible in current LIR:
- Register assignment uses `X19`, `X20`, and `X21`, with `m - 1` preserved across the inner recursive call in `X21`.

### Backend Lowering Status

ARM64 code generation now maps `LIR.BranchZero` directly to `ARM64Symbolic.CBZ` followed by an unconditional branch to the non-zero label.

## Completed Nested-Recursion Register Check

The current entry block is no longer the old nine-move cycle, so the previous
high-impact "register entry block" opportunity should be considered mostly
implemented for this benchmark. A follow-up check confirmed that `X21`
preservation across the inner recursive call is the live `m - 1` value needed
after the call returns, and the post-call moves restore the call result and
outer-loop `m` parameter for the jump back to `ackermann_body`.

Current allocated LIR evidence:

```text
X21 <- Sub(X20, Imm 1)
X19 <- Sub(X19, Imm 1)
ArgMoves(X0 <- Reg X20, X1 <- Reg X19)
X19 <- Call(ackermann, [Reg X20, Reg X19])
X19 <- Mov(Reg X0)
X20 <- Mov(Reg X21)
Jump(Label "ackermann_body")
```

## Status Corrections From Older Notes

| Older opportunity | Current status |
|-------------------|----------------|
| Nine MOVs in `ackermann_entry` | Mostly implemented for this benchmark; current entry has two argument moves. |
| Dead `ackermann_L5` in final LIR | Still visible in `--dump-mir`, but absent from post-register-allocation LIR. Early raw-MIR pruning was rejected after measurement. |

## Recommended Next Checks

1. Confirm with final ARM64 disassembly when tooling can disassemble the sectionless generated ELF cleanly.
2. Use the rejected-experiments record before revisiting early raw-MIR unreachable-block cleanup for this benchmark.
