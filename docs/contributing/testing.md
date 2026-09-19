# Focused test DSLs

End-to-end `.e2e` tests remain the default for language behavior. Focused
fixture formats cover repetitive syntax, encoding, algorithm, formatting, and
small executable-backend cases without requiring a new F# test function for
every input.

## Optimization fixtures

Place before/after compiler fixtures in `src/Tests/optimization/`. The file
name selects ANF, MIR, or LIR, and each case compiles `INPUT` before pinning the
complete optimized IR in `EXPECTED`:

```text
---NAME---
constant integer addition
---INPUT---
1L + 2L
---EXPECTED---
return 3
```

ANF fixtures can instead select an optimized function from the prebuilt
stdlib. This is the preferred proof for a stdlib implementation optimization:

```text
---NAME---
stdlib power loop uses strength reduction
---STDLIB-FUNCTION---
Darklang.Stdlib.Int64.__powerLoop
---EXPECTED---
Function Darklang.Stdlib.Int64.__powerLoop:
...
```

Use E2E tests alongside these fixtures for observable behavior. The
optimization fixture must show that the intended optimized IR is present; a
behavior-only test is not evidence that an optimization occurred.

Value-equality E2E checks normally share a generated executable. Add
`isolated=true` to a check that must own its process or bounded heap state; the
runner then compiles and executes that check separately.

Use `compileerror="message"` when a case must be rejected before native
execution. Unlike the older `error="message"` expectation, a matching runtime
failure does not satisfy `compileerror`. Imported upstream `.dark` files retain
their original expectation and may put `#compileerror="message"` immediately
before a test to override that expectation for the AOT compiler. The directive
may override either an upstream success or error expectation.

## Syntax fixtures

Place `.syntax` files under `src/Tests/syntax/`. A file can contain multiple
cases, each beginning with `NAME`:

```text
---NAME---
canonical formatting
---SOURCE---
let x = 5 in x
---EXPECTED---
let x = 5 in x
---ROUNDTRIP---
```

Every case uses the canonical parser and can assert an exact formatted result
with `EXPECTED`, a structural AST roundtrip with `ROUNDTRIP`, or a parser
diagnostic substring with `EXPECT-ERROR`.

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

Keep direct F# tests for internal helper APIs and programs that require richer
binary layout or execution setup. Run all fixtures and unit tests with
`./build --ai && ./run-tests --ai`.

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

## IR-format snapshot fixtures

Place multi-case `.irformat` files under `src/Tests/formatting/ir/`. Select the
compact input parser with `IR`, then pin the complete pretty-printed result:

```text
---NAME---
ANF UInt64 maximum
---IR---
anf
---INPUT---
return u64[18446744073709551615]
---EXPECTED---
return 18446744073709551615
```

`IR` accepts `anf`, `mir`, or `lir`. Compact string literals use
`str[...]` with `\\`, `\"`, `\n`, `\r`, and `\t` escapes; ANF also accepts
`u64[...]`. Use direct F# construction when a formatting assertion depends on
multi-block CFG ordering or an IR shape the compact parsers intentionally do
not model.

## Executable LIR fixtures

Place multi-case `.lirexec` files under `src/Tests/backend/x64/`. Each case is
a compact, single-block LIR program with a typed expected outcome:

```text
---NAME---
ADD immediate
---INPUT-LIR---
X1 <- Mov(Imm 40)
X1 <- Add(X1, Imm 2)
Exit
Ret
---EXPECT-EXIT---
42
```

Use `EXPECT-EXIT`, `EXPECT-STDOUT`, and `EXPECT-STDERR` independently or
together. Output comparisons trim surrounding whitespace; an empty
`EXPECT-STDOUT` or `EXPECT-STDERR` section asserts no output. `LEAK-CHECK` can
be `true` or `false` and enables the x64 leak report when requested.

Use `EXPECT-CODEGEN-ERROR` instead of process expectations when translation
should fail, with the expected diagnostic substring as its value. Codegen-error
and process expectations cannot be combined in one case.

The supported LIR subset covers scalar moves/arithmetic, integer and heap-string
printing, fixed-block allocation/load/store/refcount operations, raw
allocation/free, string concatenation and refcounts, and random integers. Keep
direct F# tests for multi-block CFGs, condition-state isolation across
translations, malformed compiler data,
type/variant/record metadata, and ownership tests whose correctness depends on
rich nested runtime shapes.

## Reference-release fixtures

Place multi-case `.rcrelease` files under
`src/Tests/backend/reference-release/`. Each case describes a canonical
managed object graph whose final root reference is released on the active
ARM64 or x64 backend:

```text
---NAME---
List field release preserves X0
---ROOT-REGISTER---
X4
---ROOT---
tuple(list(i64))
---PRESERVE---
X0 = 123
```

Successful execution implicitly requires a clean exit and no leaked heap
allocations. There are no `ACTION`, `EXPECT-LEAKS`, or `EXPECT-STDOUT`
sections: releasing the final root, observing zero leaks, and checking any
preserved register values are the semantics of every case.

`ROOT` accepts the leaf shapes `i64`, `enum`, `string`, `literal-string`, and
`blob`, plus `list(shape)`, `dict(key, value)`, `tuple(...)`, `record(...)`,
`sum(payload)`, and `closure(...)`. Shapes can be nested. The root itself must
be managed; scalar shapes are useful only as fields or payloads.

By default the runner chooses the root register. Use `ROOT-REGISTER` when
register placement is observable behavior, and pair it with `PRESERVE` lines
of the form `X0 = 123` to verify that releasing the root does not clobber live
registers. These checks run on both backends; choose placements that express a
valid preservation requirement on each architecture.

Keep direct F# tests for collision-node dictionary layouts, malformed or mixed
variant metadata, release-helper selection and instruction-shape assertions,
special named-function contexts, and tests that require multiple independently
live managed objects or noncanonical heap layouts.
