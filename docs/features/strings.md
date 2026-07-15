# Strings

Dark strings are immutable UTF-8 byte sequences. String values can be either
literal-pool entries or heap-allocated dynamic buffers.

## Layout

Dynamic strings use an aligned dynamic-buffer layout:

```text
offset 0:                 length in bytes, Int64
offset 8:                 UTF-8 data
offset 8 + aligned(len):  refcount, Int64
```

`aligned(len)` rounds the byte length up to the next 8-byte boundary. This is
the same dynamic-buffer convention used by `Bytes`.

Literal-pool strings are immutable. Backends skip ordinary dynamic-buffer RC for
literal operands. ARM64 literal pool entries and x64 materialized string
literals carry a sentinel refcount slot so accidental dynamic RC does not
reclaim them.

## Operations

String literals are interned in `LiteralPool.StringPool`.

String concatenation lowers to `LIR.StringConcat`, which allocates a new dynamic
string, copies both inputs, initializes refcount to 1, and participates in leak
accounting when leak checking is enabled.

The stdlib exposes byte-oriented operations directly over this layout:

- `Stdlib.String.length` returns the byte length stored at offset 0.
- `Stdlib.String.getByteAt` reads a byte from the data region at offset 8.
- `startsWith`, `endsWith`, `indexOf`, `contains`, `slice`, `substring`,
  `take`, and `drop` operate on byte offsets.

Unicode helpers are layered on top of the byte representation:

- `toCodepoints` decodes UTF-8 to `List<Int64>`.
- `fromCodepoints` allocates a dynamic string and encodes UTF-8 bytes.
- `codepointLength`, `toUpperCase`, `toLowerCase`, and `reverse` use the
  codepoint conversion helpers.
- `toGraphemes` and `graphemeLength` use a simplified UAX #29-style segmenter
  with explicit handling for combining marks, variation selectors, skin tone
  modifiers, zero-width joiner, and CR/LF.

Higher-level stdlib functions such as `repeat`, `join`, `trim`, `split`,
`replace`, `first`, `last`, `dropFirst`, `dropLast`, `head`, `padStart`, and
`padEnd` are implemented in `src/DarkCompiler/stdlib/String.dark`.

String hashing and equality are backend intrinsics:

```fsharp
| StringHash of dest:Reg * str:Operand
| StringEq of dest:Reg * left:Operand * right:Operand
```

## Reference Counting

Dynamic strings are now part of the compiler-managed RC model:

- dynamic string temporaries get scoped decrefs
- fixed-block fields retain dynamic strings when stored
- fixed-block destructors release dynamic string fields
- returned borrowed string projections are retained before parent cleanup
- list, dict, sum, record, tuple, and closure paths have targeted string
  payload coverage on the active backend

The IR operations are:

```fsharp
| RefCountIncString of str:Operand
| RefCountDecString of str:Operand
```

Both operations compute the refcount offset as `8 + aligned(length)`.

## Remaining Work

The basic string lifetime model is implemented, but edge coverage and memory
reuse work remain:

- string-producing runtime paths such as float display and file I/O need more
  leak-check coverage
- deeply nested container combinations should move from ad hoc backend helpers
  to shape-driven release plans
- zero-refcount dynamic strings balance leak accounting, but variable-size
  buffer reuse is still a deferred allocator-policy decision
- x64 recursive payload parity trails ARM64

See [`memory-refcounting-remaining.md`](../../memory-refcounting-remaining.md)
for the current task breakdown.

## Implementation Files

| File | Purpose |
|---|---|
| `src/DarkCompiler/LiteralPool.fs` | literal string pool |
| `src/DarkCompiler/LIR.fs` | string and dynamic-buffer RC instructions |
| `src/DarkCompiler/passes/2.5_RefCountInsertion.fs` | string lifetime insertion |
| `src/DarkCompiler/passes/arm64/6_CodeGen.fs` | ARM64 string allocation and RC |
| `src/DarkCompiler/passes/x64/6_CodeGen.fs` | x64 string allocation and RC |
| `src/DarkCompiler/stdlib/String.dark` | stdlib string functions |
