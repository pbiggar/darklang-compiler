# Focused test DSLs

End-to-end `.e2e` tests remain the default for language behavior. Two focused
fixture formats cover repetitive parser/pretty-printer and machine-code cases
without requiring a new F# test function for every input.

## Syntax fixtures

Place `.syntax` files under `src/Tests/syntax/`. A file can contain multiple
cases, each beginning with `NAME`:

```text
---NAME---
compiler to interpreter
---PARSE-AS---
compiler
---SOURCE---
let x = 5 in x
---FORMAT-AS---
interpreter
---EXPECTED---
let x = 5L in x
---ROUNDTRIP-AS---
compiler, interpreter
```

`PARSE-AS` is `compiler` or `interpreter`. A case can assert an exact formatted
result with `FORMAT-AS` and `EXPECTED`, structural AST roundtrips with
`ROUNDTRIP-AS`, or a parser diagnostic substring with `EXPECT-ERROR`.

Use F# tests when the assertion depends on a particular internal AST shape or
test-runner behavior rather than syntax acceptance, formatting, or roundtrips.

## x64 encoding and resolution fixtures

Place multi-case `.x64enc` files under `src/Tests/passes/x64enc/`. Instructions
use constructor-style syntax matching the x64 instruction union:

```text
---NAME---
forward jump
---INPUT-X64---
JMP(skip)
MOV_reg(RAX, RAX)
Label(skip)
RET
---OUTPUT-HEX---
E9 03 00 00 00 48 89 C0 C3
```

A successful case uses `OUTPUT-HEX`, `EXPECT-FIXUPS`, or both. A failing
resolution case uses `EXPECT-ERROR`. `EXPECT-FIXUPS` contains one unresolved
label per line, in emitted order.

ARM64 `.arm64enc` fixtures under `src/Tests/passes/arm64enc/` continue to map
one instruction per output word. They also support `EXPECT-ERROR`, which checks
that every listed instruction is rejected by the encoder with the requested
diagnostic substring.

Keep direct F# tests for internal helper APIs and tests that build or execute a
native binary. Run all fixtures and unit tests with `./run-tests --ai`.

## Graph-coloring fixtures

Place multi-case `.graphcolor` files under
`src/Tests/algorithms/graph-color/`. Each case describes a non-negative vertex
set, optional edges and coloring preferences, and at least one observable
property:

```text
---NAME---
precoloring is respected
---VERTICES---
0 1
---EDGES---
0-1
---AVAILABLE-COLORS---
8
---PRECOLORED---
0=3
---EXPECT-COLORS---
0=3
---EXPECT-DIFFERENT---
0-1
```

Count assertions in `EXPECT-CHROMATIC`, `EXPECT-SPILLS`, and
`EXPECT-COLORED` accept exact integers, `<= N`, or `>= N`. Fixtures can also
assert `EXPECT-SAME`, MCS coverage with `EXPECT-MCS-ORDERING: all`, and the MCS
profile's exact `EXPECT-SELECTION-CHECKS`. `PREFER` specifies phi preference
pairs; `MOVE-PREFER` specifies higher-priority copy-coalescing pairs.

Keep direct F# tests for graph construction from real CFGs, liveness behavior,
and collection of preferences from compiler instructions.

## Parallel-move fixtures

Place multi-case `.parallelmoves` files under
`src/Tests/algorithms/parallel-moves/`. Inputs use LIR physical-register
destinations and operands; outputs pin the complete symbolic ARM64 sequence:

```text
---NAME---
two-way cycle
---INPUT-MOVES---
X1 <- Reg X2
X2 <- Reg X1
---OUTPUT-ARM64---
MOV_reg(X16, X1)
MOV_reg(X1, X2)
MOV_reg(X2, X16)
```

Operands can be `Reg Xn`, `Imm N`, or `Stack N`. Use `none` as
`OUTPUT-ARM64` when all moves should be eliminated. These fixtures exercise the
shared parallel-move resolver through ARM64 `TailArgMoves` lowering.
