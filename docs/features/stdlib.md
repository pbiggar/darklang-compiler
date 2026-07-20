# Standard Library

This document describes the Dark compiler's standard library implementation.

## Overview

The stdlib provides built-in modules available to all Dark programs:
- **Intrinsics**: Compiler-implemented (syscalls, platform detection)
- **Dark functions**: Implemented in Dark itself

## Module Catalog

| Module | Description |
|--------|-------------|
| `Stdlib.Int8` | 8-bit integer operations |
| `Stdlib.Int16` | 16-bit integer operations |
| `Stdlib.Int32` | 32-bit integer operations |
| `Stdlib.Int64` | Integer operations |
| `Stdlib.UInt8` | 8-bit unsigned integer operations |
| `Stdlib.UInt16` | 16-bit unsigned integer operations |
| `Stdlib.UInt32` | 32-bit unsigned integer operations |
| `Stdlib.UInt64` | 64-bit unsigned integer operations |
| `Stdlib.Bool` | Boolean operations |
| `Stdlib.Builtin` | Builtin helper functions |
| `Stdlib.Float` | Floating-point operations |
| `Stdlib.String` | String manipulation |
| `Stdlib.List` | List operations |
| `Stdlib.Option` | Optional values |
| `Stdlib.Result` | Error handling |
| `Stdlib.Tuple2/3` | Tuple operations |
| `Stdlib.Dict` | Hash map (HAMT) |
| `Stdlib.Bytes` | Byte array operations |
| `Stdlib.Char` | Character helpers |
| `Stdlib.Crypto` | Hashing and HMAC helpers |
| `Stdlib.Base64` | Base64 encoding and decoding |
| `Stdlib.AWS` | AWS signing helpers |
| `Stdlib.File` | File I/O |
| `Stdlib.Path` | Path operations |
| `Stdlib.Platform` | Platform detection |
| `Stdlib.Random` | Random numbers |
| `Stdlib.Date` / `Stdlib.DateTime` | Date/time operations |
| `Stdlib.Math` | Math helpers |
| `Stdlib.Uuid` | UUID helpers |

## Implementation Types

### Intrinsic Functions

Defined in `Stdlib.fs`, implemented in the compiler:

```fsharp
let fileModule : ModuleDef = {
    Name = "Stdlib.File"
    Functions = [
        { Name = "readText"; ParamTypes = [TString]; ReturnType = resultType TString }
        { Name = "writeText"; ParamTypes = [TString; TString]; ReturnType = resultType TUnit }
        // ...
    ]
}
```

These generate syscalls or special code sequences.

### Dark Functions

Defined in modular `src/DarkCompiler/stdlib/*.dark` files and compiled like
user code:

```dark
def Stdlib.Int64.max(a: Int64, b: Int64) : Int64 =
    if a > b then a else b

def Stdlib.List.map<a, b>(list: List<a>, fn: (a) -> b) : List<b> =
    match list with
    | [] -> []
    | [h, ...t] -> [fn(h), ...Stdlib.List.map<a, b>(t, fn)]
```

## Stdlib.Int64

```dark
def add(a: Int64, b: Int64) : Int64 = a + b
def sub(a: Int64, b: Int64) : Int64 = a - b
def mul(a: Int64, b: Int64) : Int64 = a * b
def div(a: Int64, b: Int64) : Int64 = a / b
def mod(a: Int64, b: Int64) : Int64 = a % b
def max(a: Int64, b: Int64) : Int64
def min(a: Int64, b: Int64) : Int64
def absoluteValue(a: Int64) : Int64
def negate(a: Int64) : Int64
def power(base: Int64, exponent: Int64) : Int64
def clamp(value: Int64, limitA: Int64, limitB: Int64) : Int64
def toString(n: Int64) : String
def popcount(x: Int64) : Int64  // Count set bits
// Bitwise
def bitwiseAnd(a: Int64, b: Int64) : Int64
def bitwiseXor(a: Int64, b: Int64) : Int64
def shiftLeft(a: Int64, shift: Int64) : Int64
def shiftRight(a: Int64, shift: Int64) : Int64
```

## Stdlib.Int8/Int16/Int32/UInt8/UInt16/UInt32/UInt64

These modules mirror the `Stdlib.Int64` API for their respective widths.

## Stdlib.Option

```dark
type Stdlib.Option.Option<t> = Some of t | None

def isSome<t>(opt: Option<t>) : Bool
def isNone<t>(opt: Option<t>) : Bool
def withDefault<t>(opt: Option<t>, default: t) : t
def map<t, u>(opt: Option<t>, fn: (t) -> u) : Option<u>
def andThen<t, u>(opt: Option<t>, fn: (t) -> Option<u>) : Option<u>
def toList<t>(opt: Option<t>) : List<t>
```

## Stdlib.Result

```dark
type Stdlib.Result.Result<t, e> = Ok of t | Error of e

def isOk<t, e>(result: Result<t, e>) : Bool
def isError<t, e>(result: Result<t, e>) : Bool
def withDefault<t, e>(result: Result<t, e>, default: t) : t
def map<t, u, e>(result: Result<t, e>, fn: (t) -> u) : Result<u, e>
def mapError<t, e, f>(fn: (e) -> f, result: Result<t, e>) : Result<t, f>
def andThen<t, u, e>(result: Result<t, e>, fn: (t) -> Result<u, e>) : Result<u, e>
```

## Stdlib.File (Intrinsic)

```dark
def readText(path: String) : Result<String, String>
def writeText(path: String, content: String) : Result<Unit, String>
def appendText(path: String, content: String) : Result<Unit, String>
def delete(path: String) : Result<Unit, String>
def exists(path: String) : Bool
def setExecutable(path: String) : Result<Unit, String>
```

These generate syscall sequences (open, read/write, close).

## Stdlib.Platform (Intrinsic)

```dark
def isMacOS() : Bool
def isLinux() : Bool
```

Constant-folded at compile time based on target platform.

## Stdlib.Random (Intrinsic)

```dark
def int64() : Int64  // 8 random bytes
```

Uses platform-specific random source:
- macOS: `getentropy()` syscall
- Linux: `getrandom()` syscall

## How Stdlib is Included

1. **Compilation start**: Load intrinsic module signatures from `Stdlib.fs`
   and the ordered Dark stdlib source files from `src/DarkCompiler/stdlib/`
2. **Parse**: Parse stdlib definitions
3. **Combine**: Merge with user program
4. **Type check**: Stdlib + user code together
5. **Compile**: Monomorphize and compile all

Stdlib functions are only included if called (dead code elimination).

## Implementation Files

| File | Lines | Purpose |
|------|-------|---------|
| `Stdlib.fs` | | Intrinsic module definitions |
| `stdlib/*.dark` | | Dark implementations |
| `CompilerLibrary.fs` | | Stdlib loading logic |

## Generic Function Monomorphization

Generic stdlib functions are monomorphized per use:

```dark
Stdlib.List.map<Int64, String>(nums, toString)
```

Creates `Stdlib.List.map_i64_String` specialized function.

## Adding New Stdlib Functions

1. **Dark function**: Add to the appropriate file in `src/DarkCompiler/stdlib/`
2. **Intrinsic**: Add to `Stdlib.fs` + implement in codegen

See `docs/adding-features.md` for details.
