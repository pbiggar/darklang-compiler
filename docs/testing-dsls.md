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
