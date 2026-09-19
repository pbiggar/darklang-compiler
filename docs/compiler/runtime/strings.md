# Strings

Dark strings are immutable UTF-8 byte sequences. String values can be either
literal-pool entries or heap-allocated dynamic buffers.

## Layout

Dynamic strings use an aligned dynamic-buffer layout:

```text
offset 0:   refcount, Int64
offset 8:   length in bytes, Int64
offset 16:  UTF-8 data
```

`aligned(len)` rounds the byte length up to the next 8-byte boundary. This is
the same dynamic-buffer convention used by `Blob`.

Literal-pool strings are immutable. Backends skip ordinary dynamic-buffer RC for
literal operands. ARM64 literal pool entries and x64 materialized string
literals carry a sentinel refcount slot so accidental dynamic RC does not
reclaim them.

## Operations

String literals are interned in `LiteralPool.StringPool`.

String concatenation trees lower to one variadic `LIR.StringConcat`. The
backends first sum every operand's UTF-8 byte length, then allocate one dynamic
string and copy each operand directly into the final buffer. The buffer starts
with refcount 1 and participates in leak accounting when leak checking is
enabled. A two-operand concat retains its compact specialized lowering.

Public concatenation is an NFC composition boundary. A fused tree concatenates
all bytes first and normalizes the result once; the ASCII fast path scans the
final buffer once and returns it unchanged.

Private stdlib helpers operate directly over this layout:

- `Stdlib.String.__byteLength` returns the byte length stored at offset 8.
- `Stdlib.String.__byteAtUnchecked` reads a byte from the data region at offset
  16 without bounds checking.
- `Stdlib.String.__byteSlice`, `__byteTake`, and `__byteDrop` use byte offsets.

The public `String.getByteAt : String -> Int -> Option<UInt8>` validates and
converts its index before using those helpers. Public text traversal such as
`length`, `slice`, `dropFirst`, and `dropLast` uses extended grapheme clusters.

Unicode helpers are layered on top of the byte representation:

- `toCodepoints` decodes UTF-8 to `List<Int>`.
- `fromCodepoints` allocates a dynamic string and encodes UTF-8 bytes.
- `codepointLength`, `toUppercase`, `toLowercase`, and `reverse` use the
  codepoint conversion helpers.
- Private grapheme helpers use the shared UAX #29 segmenter.
  It keeps combining marks, variation selectors, selected emoji modifiers, and
  CR/LF with their surrounding cluster, but it does not implement the full UAX
  #29 rules needed for complex skin-tone and zero-width-joiner emoji sequences.

Higher-level stdlib functions such as `repeat`, `join`, `trim`, `split`,
`replace`, `first`, `last`, `dropFirst`, `dropLast`, `head`, `padStart`, and
`padEnd` are implemented in `src/DarkCompiler/stdlib/String.dark`.

String equality is a representation-level compiler operation:

```fsharp
| CanonicalBufferEq of dest:Reg * kind:CanonicalBufferKind * left:Operand * right:Operand
```

Equality first checks pointer identity and byte length, then compares full
machine words followed by any remaining bytes. `CanonicalBufferKind` records
whether the bytes represent UTF-8 text, a grapheme cluster, or one of the
currently canonical-buffer-backed fixed-width integer types. Arbitrary-
precision `Int` keeps its numeric equality path because internal values are not
guaranteed to have canonical text during intermediate calculations.

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

Both operations access the refcount directly at the value pointer.

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

## Implementation Files

| File | Purpose |
|---|---|
| `src/DarkCompiler/backend/binary/LiteralPool.fs` | literal string pool |
| `src/DarkCompiler/ir/lir/LIR.fs` | string and dynamic-buffer RC instructions |
| `src/DarkCompiler/passes/anf/RefCountInsertion.fs` | string lifetime insertion |
| `src/DarkCompiler/backend/arm64/CodeGen.fs` | ARM64 string allocation and RC |
| `src/DarkCompiler/backend/x64/CodeGen.fs` | x64 string allocation and RC |
| `src/DarkCompiler/stdlib/String.dark` | stdlib string functions |
