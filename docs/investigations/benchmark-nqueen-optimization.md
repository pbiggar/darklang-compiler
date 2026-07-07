# N-Queens Benchmark Optimization Investigation

## Summary

The nqueen benchmark counts solutions to the N-Queens problem for N=13 using
bitmasks for occupied columns and diagonals. The Dark implementation is written
as a tail-recursive loop over available positions, with one non-tail recursive
call when a partial board needs deeper search.

Current benchmark table context from `benchmarks/RESULTS.md`:

| Implementation | Instruction count | Relative to Rust |
|----------------|-------------------|------------------|
| Rust | 164,529,075 | 1.00x |
| OCaml | 297,970,462 | 1.81x |
| Dark | 304,488,643 | 1.85x |
| Node | 804,233,231 | 4.89x |
| Python | 17,205,881,478 | 105x |

Dark is currently close to OCaml for this benchmark and about 1.85x slower than
Rust.

## Current Compiler Evidence

Evidence was gathered on the current compiler with:

```bash
./dark -r benchmarks/problems/nqueen/dark/main.dark
./dark --dump-anf benchmarks/problems/nqueen/dark/main.dark
./dark --dump-mir benchmarks/problems/nqueen/dark/main.dark
./dark --dump-lir benchmarks/problems/nqueen/dark/main.dark
aarch64-linux-gnu-objdump -D -b binary -m aarch64 dark.out
```

The executable prints the expected result:

```text
73712
```

### Bitwise OR Lowers to Primitive Operations

The ANF dump shows source-level stdlib calls before later optimization:

```text
let TempId 12 = Stdlib.Int64.bitwiseOr(t1, t8)
let TempId 16 = Stdlib.Int64.bitwiseOr(t2, t8)
let TempId 19 = Stdlib.Int64.bitwiseOr(t3, t8)
let TempId 22 = Stdlib.Int64.bitwiseOr(t12, t17)
let TempId 23 = Stdlib.Int64.bitwiseOr(t22, t20)
```

By MIR, those calls have been lowered to primitive bitwise operations:

```text
v45 <- v1 | v8 : TInt64
v44 <- v2 | v8 : TInt64
v43 <- v3 | v8 : TInt64
v42 <- v12 | v17 : TInt64
v41 <- v22 | v20 : TInt64
```

LIR after register allocation also uses direct `Orr` instructions:

```text
X1 <- Orr(X22, X20)
X19 <- Orr(X23, X20)
X19 <- Orr(X24, X20)
X19 <- Orr(X1, X2)
X19 <- Orr(X19, X20)
```

The generated ARM64 assembly has the same shape:

```asm
22c: aa1402c1  orr x1, x22, x20
244: aa1402f3  orr x19, x23, x20
24c: aa140313  orr x19, x24, x20
254: aa020033  orr x19, x1, x2
258: aa140273  orr x19, x19, x20
```

The current hot path uses primitive `Orr` instructions for these bitwise
operations.

### The Base Case Avoids the Inner Recursive Call

Current MIR splits the `newCols == allOnes` base case before computing the next
diagonal masks or making the deeper recursive call:

```text
v15 <- v12 == v0 : TInt64
branch v15 ? nqueenSolve_L3 : nqueenSolve_L4

nqueenSolve_L3:
  v29 <- v5 + 1 : TInt64
  ...
  jump nqueenSolve_body

nqueenSolve_L4:
  ...
  v28 <- Call(nqueenSolve, [v0, v12, v17, v20, v26, 0])
  v29 <- v5 + v28 : TInt64
  ...
  jump nqueenSolve_body
```

The generated assembly mirrors that split:

```asm
230: eb15003f  cmp x1, x21
234: 54000040  b.eq 0x23c
23c: 9100075a  add x26, x26, #0x1
240: 14000015  b 0x294
```

The benchmark does not pay the deeper recursive call cost when placing the
final queen.

### Remaining Hot-Path Cost: Recursive Call Frame and Saved Registers

The non-base-case path still has a real recursive call. LIR saves two caller
registers across that call:

```text
SaveRegs([X1, X2], [])
ArgMoves(X0 <- Reg X21, X1 <- Reg X1, X2 <- Reg X2, X3 <- Reg X20, X4 <- Reg X19, X5 <- Imm 0)
X19 <- Call(nqueenSolve, [Reg X21, Reg X1, Reg X2, Reg X20, Reg X19, Imm 0])
RestoreRegs([X1, X2], [])
```

The generated assembly expands this into a stack adjustment plus store/load
around the recursive `bl`:

```asm
268: d10143ff  sub sp, sp, #0x50
26c: a9000be1  stp x1, x2, [sp]
280: 97ffffcb  bl 0x1ac
284: a9400be1  ldp x1, x2, [sp]
288: 910143ff  add sp, sp, #0x50
```

This is the primary observed optimization target: reduce call-frame overhead or
avoid preserving values across the recursive call when the post-call
continuation only needs the result and accumulator.

### Tail Loop Shape Is Compact but Still Phi-Based

Tail-call optimization turns the outer `avail` iteration into a loop. After
register allocation, the loop body is compact:

```text
Label "nqueenSolve_body":
  BranchZero(X25, Label "nqueenSolve_L0", Label "nqueenSolve_L1")
```

The assembly loop back is also direct:

```asm
294: b4fffc19  cbz x25, 0x214
298: 17ffffe1  b 0x21c
```

Before register allocation, the loop is still represented with six phi inputs
for `allOnes`, `cols`, `diag1`, `diag2`, `avail`, and `count`. Register
allocation places the loop-carried values in callee-saved registers X21-X26,
which keeps the loop compact but contributes to the function prologue and
recursive-call preservation story.

## Current Optimization Opportunities

### 1. Reduce Recursive Call Preservation Overhead

**Status:** current opportunity.

The hot non-base-case path saves X1 and X2 across the recursive call, even
though the continuation is simple:

```text
X19 <- Mov(Reg X0)
X26 <- Add(X26, Reg X19)
Jump(Label "nqueenSolve_body")
```

Investigate whether the allocator or call-lowering can avoid keeping
`newCols`/`newDiag1` live in caller-saved argument registers across the call, or
avoid emitting the 0x50-byte stack adjustment when only X1/X2 need temporary
preservation.

Likely areas:

- `src/DarkCompiler/passes/5_RegisterAllocation.fs`
- ARM64 call lowering in `src/DarkCompiler/passes/arm64/6_CodeGen.fs`

### 2. Improve Tail-Recursive Loop Lowering

**Status:** possible opportunity, lower priority than call preservation.

The outer iteration is already a loop, but the MIR form still uses explicit
copy assignments into phi inputs. Any further win would likely come from
representing the tail-recursive accumulator loop more directly before register
allocation.

Likely areas:

- `src/DarkCompiler/passes/2.7_TailCallDetection.fs`
- `src/DarkCompiler/passes/3_ANF_to_MIR.fs`
- `src/DarkCompiler/passes/5_RegisterAllocation.fs`
