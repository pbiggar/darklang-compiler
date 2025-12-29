# Strings

This document describes how the Dark compiler handles string types.

## Overview

Strings are immutable, UTF-8 encoded, heap-allocated values with reference
counting. The compiler maintains a string pool for deduplication of literals.

## Memory Layout

```
Offset 0:   Length in bytes (8 bytes, Int64)
Offset 8:   UTF-8 data (N bytes)
Offset 8+N: Refcount (8 bytes)
```

Example for `"hello"`:
```
[5, 'h', 'e', 'l', 'l', 'o', 1]
     ↑ length             ↑ refcount
```

## String Pool

String literals are deduplicated in a compile-time pool:

```fsharp
type StringPool = {
    Strings: Map<int, string * int>  // index -> (value, length)
    StringToId: Map<string, int>     // value -> index
    NextId: int
}
```

At runtime, string literals reference pool entries by index.

## Syntax

### Literals
```dark
"hello"
"line1\nline2"  // Escape sequences
""              // Empty string
```

### Interpolation
```dark
$"Hello {name}!"
$"Result: {compute()}"
```

### Concatenation
```dark
"hello" ++ " " ++ "world"
```

## String Operations

### Concatenation

`++` operator creates a new heap string:

1. Allocate: 8 (length) + len1 + len2 + 8 (refcount)
2. Write combined length
3. Copy first string's bytes
4. Copy second string's bytes
5. Initialize refcount to 1

### Length

```dark
Stdlib.String.length("hello")  // 5
```

Reads the 8-byte length field at offset 0.

## Stdlib.String Functions

Defined in `stdlib.dark:370-499`:

| Function | Type | Description |
|----------|------|-------------|
| `length` | `String -> Int64` | Byte count |
| `isEmpty` | `String -> Bool` | Check if empty |
| `append` | `(String, String) -> String` | Concatenate |
| `startsWith` | `(String, String) -> Bool` | Prefix check |
| `endsWith` | `(String, String) -> Bool` | Suffix check |
| `contains` | `(String, String) -> Bool` | Substring check |
| `indexOf` | `(String, String) -> Int64` | Find position (-1 if not found) |
| `slice` | `(String, Int64, Int64) -> String` | Extract substring |
| `take` | `(String, Int64) -> String` | First N bytes |
| `drop` | `(String, Int64) -> String` | Skip first N bytes |

## UTF-8 Encoding

Strings use UTF-8 internally:

- ASCII characters: 1 byte
- Extended characters: 2-4 bytes
- `length` returns byte count, not character count

```dark
Stdlib.String.length("hello")  // 5 (ASCII)
Stdlib.String.length("h")     // 4 (emoji = 4 bytes)
```

## String Hashing

For Dict keys, strings use FNV-1a hash:

```fsharp
| StringHash of dest:Reg * str:Operand
```

This is an intrinsic that computes a 64-bit hash.

## String Equality

String comparison checks byte-by-byte:

```fsharp
| StringEq of dest:Reg * left:Operand * right:Operand
```

The `==` operator on strings calls this intrinsic.

## Compilation

### String Literal

```dark
"hello"
```

1. Add to string pool (get index)
2. Generate code to reference pool entry

### Interpolated String

```dark
$"Value: {x}"
```

Compiles to concatenation:
```
"Value: " ++ Stdlib.Int64.toString(x)
```

### String Concatenation

```dark
a ++ b
```

Compiles to `StringConcat`:
```fsharp
Let (resultId, StringConcat (aAtom, bAtom), body)
```

## Reference Counting

Strings are reference-counted:

- Literals start with refcount 1
- Assignment increments refcount
- Concatenation creates new string with refcount 1
- Going out of scope decrements refcount

Special intrinsics handle the dynamic refcount offset:
```fsharp
| RefCountIncString of str:Operand  // Inc at [str + 8 + length]
| RefCountDecString of str:Operand  // Dec at [str + 8 + length]
```

## Implementation Files

| File | Purpose |
|------|---------|
| `AST.fs:35` | TString type |
| `MIR.fs:22-47` | StringPool type |
| `LIR.fs:124-125` | StringConcat, PrintHeapString |
| `LIR.fs:144-148` | StringHash, StringEq, RefCount intrinsics |
| `stdlib.dark:370-499` | String stdlib functions |
| `6_CodeGen.fs` | String allocation and concatenation |

## Tests

See `src/Tests/e2e/string.e2e` for string operation tests.
