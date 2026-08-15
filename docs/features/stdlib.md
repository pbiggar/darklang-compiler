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
| `Stdlib.Retry` | Fixed-delay and exponential-backoff retries |
| `Stdlib.Tuple2/3` | Tuple operations |
| `Stdlib.Dict` | Hash map (HAMT) |
| `Stdlib.Blob` | Immutable binary values and public codecs |
| `Stdlib.Bytes` | Legacy compiler-only Int64 bridge over Blob |
| `Stdlib.Html` | Structural HTML nodes, serialization, attributes, and tag constructors |
| `Stdlib.Http` | Structural requests/responses/cookies, parsers, and response helpers |
| `Stdlib.Char` | Character helpers |
| `Stdlib.Crypto` | Hashing and HMAC helpers |
| `Stdlib.Base64` | Base64 encoding and decoding |
| `Stdlib.X509` | Certificate public-key extraction |
| `Stdlib.AWS` | AWS signing helpers |
| `Stdlib.File` | File I/O |
| `Stdlib.Path` | Path operations |
| `Stdlib.Cli` | Execution, processes, host/system discovery, and terminal input |
| `Stdlib.Random` | Random numbers |
| `Stdlib.DateTime` | Distinct UTC instants with 100ns storage and canonical `Int` APIs |
| `Stdlib.Duration` | Canonical short-duration parsing to arbitrary-precision seconds |
| `Stdlib.Math` | Math helpers |
| `Stdlib.Uuid` | UUID helpers |

The revision-pinned DateTime representation, range, rounding, clock, parsing,
and Duration contract is documented in [temporal-parity.md](../temporal-parity.md).
The Html and Http value-module contract, including its Blob bridge and bounded
cookie/JSON surface, is documented in
[html-http-parity.md](../html-http-parity.md).
The revision-pinned Option, Result, Retry, and native delay contract is
documented in
[option-result-retry-parity.md](../option-result-retry-parity.md).
Root equality and explicit output parity are documented in
[comparison-parity.md](../comparison-parity.md) and
[cli-presentation-parity.md](../cli-presentation-parity.md).

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
module Stdlib.Int64

let max(a: Int64, b: Int64) : Int64 =
    if a > b then a else b
```

```dark
module Stdlib.List

let map<'a, 'b>(list: List<a>, fn: (a) -> b) : List<b> =
    match list with
    | [] -> []
    | h :: t -> Stdlib.List.push<b>(Stdlib.List.map<a, b>(t, fn), fn(h))
```

## Root prelude

```dark
module Stdlib

let equals<'a>(left: a, right: a) : Bool
let notEquals<'a>(left: a, right: a) : Bool
let print(str: String) : Unit
let printLine(str: String) : Unit
let printLines(lines: List<String>) : Unit
```

Equality functions monomorphize over the compiler's existing typed structural
equality plan. Printing remains String-only: `print` and `printLine` forward to
the ordered native output effects, while `printLines` is portable Dark composed
with head-to-tail `Stdlib.List.iter`. Final expression rendering remains a
separate compiler stage and suppresses Unit.

## Stdlib.Int64

```dark
let add(a: Int64, b: Int64) : Int64 = a + b
let sub(a: Int64, b: Int64) : Int64 = a - b
let mul(a: Int64, b: Int64) : Int64 = a * b
let div(a: Int64, b: Int64) : Int64 = a / b
let mod(a: Int64, b: Int64) : Int64 = a % b
let max(a: Int64, b: Int64) : Int64
let min(a: Int64, b: Int64) : Int64
let absoluteValue(a: Int64) : Int64
let negate(a: Int64) : Int64
let power(base: Int64, exponent: Int64) : Int64
let clamp(value: Int64, limitA: Int64, limitB: Int64) : Int64
let toString(n: Int64) : String
let popcount(x: Int64) : Int64  // Count set bits
// Bitwise
let bitwiseAnd(a: Int64, b: Int64) : Int64
let bitwiseXor(a: Int64, b: Int64) : Int64
let shiftLeft(a: Int64, shift: Int64) : Int64
let shiftRight(a: Int64, shift: Int64) : Int64
```

## Stdlib.Int8/Int16/Int32/UInt8/UInt16/UInt32/UInt64

These modules mirror the `Stdlib.Int64` API for their respective widths.

## Stdlib.Bool

```dark
let not(b: Bool) : Bool
let and(a: Bool, b: Bool) : Bool
let or(a: Bool, b: Bool) : Bool
let xor(a: Bool, b: Bool) : Bool
let toString(b: Bool) : String
```

## Stdlib.Option

```dark
module Stdlib.Option

type Option<'t> = | Some of t | None

let isSome<'t>(opt: Option<t>) : Bool
let isNone<'t>(opt: Option<t>) : Bool
let withDefault<'t>(opt: Option<t>, default: t) : t
let map<'t, 'u>(opt: Option<t>, fn: (t) -> u) : Option<u>
let andThen<'t, 'u>(opt: Option<t>, fn: (t) -> Option<u>) : Option<u>
let and<'a, 'b>(option1: Option<a>, option2: Option<b>) : Option<b>
let toList<'t>(opt: Option<t>) : List<t>
```

`Stdlib.List.iter<'a>(list, fn)` invokes `fn` exactly once per element in
head-to-tail order and returns Unit; the empty list performs no calls.

## Stdlib.Result

```dark
module Stdlib.Result

type Result<'t, 'e> = | Ok of t | Error of e

let isOk<'t, 'e>(result: Result<t, e>) : Bool
let isError<'t, 'e>(result: Result<t, e>) : Bool
let withDefault<'t, 'e>(result: Result<t, e>, default: t) : t
let map<'t, 'u, 'e>(result: Result<t, e>, fn: (t) -> u) : Result<u, e>
let mapError<'t, 'e, 'f>(fn: (e) -> f, result: Result<t, e>) : Result<t, f>
let andThen<'t, 'u, 'e>(result: Result<t, e>, fn: (t) -> Result<u, e>) : Result<u, e>
let and<'t, 'e>(result1: Result<t, e>, result2: Result<t, e>) : Result<t, e>
let or<'t, 'e>(result1: Result<t, e>, result2: Result<t, e>) : Result<t, e>
```

## Stdlib.Retry

```dark
module Stdlib.Retry

let withBackoffLoop<'a>(maxAttempts: Int, attempt: Int, delayMs: Float, fn: (Unit) -> Result<a, String>) : Result<a, String>
let withBackoff<'a>(maxAttempts: Int, fn: (Unit) -> Result<a, String>) : Result<a, String>
let withFixedDelayLoop<'a>(maxAttempts: Int, attempt: Int, delayMs: Float, fn: (Unit) -> Result<a, String>) : Result<a, String>
let withFixedDelay<'a>(maxAttempts: Int, delayMs: Float, fn: (Unit) -> Result<a, String>) : Result<a, String>
```

Callbacks run at least once. Retry stops on the first `Ok` or once the current
attempt reaches the maximum, returning the final callback result unchanged.
Delays occur only between attempts; backoff starts at 100.0 ms and doubles,
while fixed delay stays unchanged.

## Stdlib.File (Intrinsic)

```dark
let readText(path: String) : Result<String, String>
let writeText(path: String, content: String) : Result<Unit, String>
let appendText(path: String, content: String) : Result<Unit, String>
let delete(path: String) : Result<Unit, String>
let exists(path: String) : Bool
let setExecutable(path: String) : Result<Unit, String>
```

These generate syscall sequences (open, read/write, close).

## Stdlib.Cli

```dark
def Stdlib.Cli.execute(command: String) : Stdlib.Cli.ExecutionOutcome
def Stdlib.Cli.Process.run(program: String, args: List<String>) : Result<Output, Posix.Error>
def Stdlib.Cli.OS.getOS() : Result<OS, String>
def Stdlib.Cli.Stdin.readKey() : KeyRead
def Stdlib.Cli.Posix.sleep(delayMs: Float) : Unit
```

Portable helpers are Dark source; typed CLI operations lower through
ANF/MIR/LIR to target-native process, host, signal, and terminal primitives.
`Platform.Target` remains an F# compiler-driver type and is not a Dark API.
`Cli.Posix.sleep` lowers through the internal compiler-only
`Stdlib.Cli.__sleep` boundary to a blocking native millisecond delay on Linux
ARM64, Linux x86_64, and macOS ARM64. It does not spawn a shell process.

## Stdlib.Random (Intrinsic)

```dark
let int64() : Int64  // 8 random bytes
```

Uses platform-specific random source:
- macOS: `getentropy()` syscall
- Linux: `getrandom()` syscall

## How Stdlib is Included

1. **Compilation start**: Load intrinsic module signatures from
   `src/DarkCompiler/Stdlib.fs`
   and the ordered Dark stdlib source files from `src/DarkCompiler/stdlib/`
2. **Parse**: Parse stdlib definitions
3. **Combine**: Merge with user program
4. **Type check**: Stdlib + user code together
5. **Compile**: Monomorphize and compile all

Stdlib functions are only included if called (dead code elimination).

## Implementation Files

| File | Purpose |
|------|---------|
| `src/DarkCompiler/Stdlib.fs` | Intrinsic module definitions |
| `src/DarkCompiler/stdlib/*.dark` | Dark stdlib implementations |
| `src/DarkCompiler/CompilerLibrary.fs` | Stdlib loading logic |

## Generic Function Monomorphization

Generic stdlib functions are monomorphized per use:

```dark
Stdlib.List.map<Int64, String>(nums, toString)
```

Creates `Stdlib.List.map_i64_String` specialized function.

## Adding New Stdlib Functions

1. **Dark function**: Add to the appropriate file in `src/DarkCompiler/stdlib/`
2. **Intrinsic**: Add to `src/DarkCompiler/Stdlib.fs` + implement in codegen

See `docs/adding-features.md` for details.
