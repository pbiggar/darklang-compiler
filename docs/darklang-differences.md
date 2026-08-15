# Darklang Differences

## Overview

This compiler aims to match the Darklang interpreter. The interpreter is always correct,
except in places where a developer identifies specific issues.

This document catalogs all known differences between this compiler and the official
Darklang interpreter. It serves as:
- Reference for the validation script (`scripts/validate-darklang.py`)
- Guide for fixing semantic differences
- Documentation of compiler-only features

To validate tests against the interpreter, run:
```bash
python3 scripts/validate-darklang.py --help-full  # See detailed documentation
python3 scripts/validate-darklang.py              # Run validation
```

---

## 1. Syntactic Differences

Features supported in both with different syntax. The validation script automatically
converts from compiler syntax to interpreter syntax.

| Feature | Compiler | Interpreter | Conversion |
|---------|----------|-------------|------------|
| Integer literals | Default syntax: `5` is `Int64`; interpreter syntax: `5` is `Int` | `5` is `Int` | Select the matching parser mode |
| Sized integers | `1y`, `1s`, `1l` | Not supported | Int8, Int16, Int32 suffixes |
| Unsigned integers | `1uy`, `1us`, `1ul` | Not supported | UInt8, UInt16, UInt32 suffixes |
| List separators | `[1, 2]` | `[1L; 2L]` | Comma to semicolon |
| Function calls | `Mod.fn(a, b)` | `Stdlib.Mod.fn a b` | Parentheses to spaces |
| Lambdas | `fun x -> body` | `fun x -> body` | Shared public syntax |
| Type parameters | `List<Int64>` | Different syntax | Generic type notation |
| String interpolation | `$"Hello {x}"` | Not supported | Interpolated strings |

### 1.1 Integer Literals

The compiler's interpreter-syntax parser matches current Dark: an unsuffixed
literal such as `5` is arbitrary-precision `Int`, while `5L` is `Int64`. The
default compiler syntax retains its historical bare-`Int64` meaning. The `I`
suffix explicitly selects `Int` in either compiler parser and is a
compiler-only spelling.

```
# Default compiler syntax
1 + 2 = 3

# Current/interpreter syntax
darklang-interpreter eval "1 + 2"
# Returns: 3
```

### 1.2 List Separators

**Conversion:** `[1, 2, 3]` → `[1L; 2L; 3L]`

This compiler uses commas; Darklang uses semicolons.

```
# This compiler
[1, 2, 3]

# Darklang
darklang-interpreter eval "[1L; 2L; 3L]"
# Returns: [1; 2; 3]
```

### 1.3 Function Call Syntax

**Conversion:** `Module.func(arg1, arg2)` → `Stdlib.Module.func arg1 arg2`

This compiler uses parentheses and commas; Darklang uses space-separated args.

```
# This compiler
Int64.add(1, 2)

# Darklang
darklang-interpreter eval "Stdlib.Int64.add 1L 2L"
# Returns: 3
```

### 1.4 Bindings and lambdas

Let patterns, optional `in`, layout continuations, and `fun` binders now share
the interpreter grammar. See [binding-parity.md](binding-parity.md) for the
revision-pinned behavior matrix and AOT diagnostic differences.

---

## 2. Semantic Bugs

Areas where compiler produces WRONG output. These need to be fixed to match Darklang.

| Bug | Skip Reason | Description | Status |
|-----|-------------|-------------|--------|
| Division `/` | `semantic:division` | Integer vs float division | Needs fix |
| Modulo `%` | `semantic:modulo` | Negative divisor handling | Fixed |
| `Int64.power` | `eval:error_result` | Negative exponent handling | Fixed |
| `Char.isUppercase` | — | Supported non-ASCII uppercase characters were classified as lowercase | Fixed |
| `Dict.setOverridingDuplicates` | — | Official overwriting dictionary update was missing | Fixed |
| `Base64.decode` | — | Valid unpadded final groups were rejected | Fixed |
| `String.reverse` | — | Decomposed grapheme clusters were reversed as individual codepoints | Fixed |
| `Math.degrees` | — | Official degree values could not be converted to radians | Fixed |
| Float precision | `eval:float_precision` | High-precision values now use shortest round-trip rendering | Fixed |

### 2.1 Modulo Operator (`%`)

**Skip reason:** `semantic:modulo`

**Concern:** Negative divisor handling differed.

```
# This compiler
-10 % 3 = 2  (truncated modulo)

# Darklang - negative divisor should error
darklang-interpreter eval "10L % -3L"
```

**Status:** Fixed. Compiler now errors when the divisor is negative, matching the interpreter.

### 2.2 `Int64.power` Negative Exponents

**Skip reason:** `eval:error_result`

The official interpreter reports `Cannot raise integer to a negative exponent`.
The compiler now reports the same runtime error instead of recursively decrementing
the exponent until the generated program crashes.

### 2.3 `Char.isUppercase` Unicode Classification

The compiler now recognizes the non-ASCII uppercase characters supported by its
case-conversion pairs, including `Ż`, matching the official interpreter.

### 2.4 `Dict.setOverridingDuplicates`

The compiler now exposes the official string-keyed dictionary update function.
When the key already exists, the returned immutable dictionary contains the new
value, matching the interpreter.

### 2.5 `Base64.decode` Unpadded Final Groups

The compiler now accepts valid final Base64 groups of two or three characters
without explicit `=` padding, matching the official interpreter. A one-character
input remains invalid because it cannot encode a complete byte.

### 2.6 `String.reverse` Grapheme Clusters

The compiler now reverses strings by grapheme cluster rather than by codepoint,
matching the official interpreter for decomposed text. For example, reversing
`"éx"` now produces `"xé"`, keeping the combining acute accent attached to
its base character.

## 3. Intentional Semantic Divergences

### 3.1 Blob-family parity

The revision-pinned Blob, Base64, Crypto, and X509 public divergence table is
empty. The retained `Bytes`, `Base64.urlDecode`, `Crypto.sha1`,
`Crypto.bytesToHex`, and `Crypto.debug*` names are explicitly compiler-only
extensions over Blob, not alternate parity behavior. See
[blob-parity.md](blob-parity.md).

### 3.2 CLI host failure paths

The CLI surface follows the pinned interpreter contract. One explicit static
repair remains: Linux architecture discovery returns `Error(stderr)` when
`uname -m` exits nonzero. The interpreter source has no corresponding match
arm, while the compiler requires a total, statically typed expression. Public
Windows/legacy-ARM/PowerShell variants are represented but unproduced on the
supported native targets. See
[CLI/process/host/input parity](cli-process-host-input-parity.md).

### 3.3 Conditional and sequence type timing

Conditional selection and statement execution order match the interpreter, as
documented in the [conditional and sequence parity matrix](conditional-sequence-parity.md).
Two intentional static-compiler differences remain: the compiler rejects
heterogeneous conditional arms during type checking, and rejects a non-Unit
non-final sequence expression during type checking. The pinned interpreter
accepts both source shapes and applies only its dynamic condition/statement
checks while evaluating the selected path.

### 3.3 Temporal AOT boundaries

DateTime and Duration public values, signatures, ranges, parsing, arithmetic,
and errors are aligned with the pinned interpreter. The remaining deliberate
differences are the AOT compiler's earlier static rejection phase and direct
UTC host-clock syscall boundary. See [temporal-parity.md](temporal-parity.md).

### 3.4 Html and Http value modules

The active Html and Http surface has no intentional semantic divergences from
the pinned interpreter. `Html.s` is a compiler-only constructor extension;
`Cookie` is data-only, and `responseWithJson` accepts an already serialized
string. See the revision-pinned [Html/Http parity matrix](html-http-parity.md).

### 3.5 Option, Result, Retry, and sleep

The added control combinators and Retry functions have no intentional public
behavior divergence from the pinned interpreter. `Stdlib.Cli.__sleep` is an
internal compiler-only typed execution boundary beneath public
`Stdlib.Cli.Posix.sleep`, not a public extension. Ahead-of-time type-error
timing remains the sole retained divergence for this surface. See the
[Option/Result/Retry parity matrix](option-result-retry-parity.md).

### 3.6 Equality and explicit output prelude

`Stdlib.equals`, `Stdlib.notEquals`, `Stdlib.print`, `Stdlib.printLine`, and
`Stdlib.printLines` now match the pinned interpreter's public behavior. The AOT
compiler rejects mixed and distinct nominal equality operands during inference
instead of evaluating them and raising an interpreter runtime error. RawPtr and
RuntimeError are rejected compiler-internal categories. Interpreter DDB values
have no compiled representation and remain unsupported.

The compiler's `Uuid = String` model is an extension, not parity with the
interpreter's distinct DUuid value. Blob and function equality retain their
pinned identity rules. Explicit printing accepts String only and is independent
of implicit final-result rendering; final Unit adds no text. See
[comparison-parity.md](comparison-parity.md) and
[cli-presentation-parity.md](cli-presentation-parity.md).

### 3.7 Stream lifecycle

The public Stream API and values match the pinned interpreter. The compiler
adds deterministic last-owner close for abandoned handles and rejects invalid
ordering during AOT type checking. It omits the interpreter's chunked byte
fast path because `toBlob` through the existing Blob constructor is
behaviorally equivalent. See the revision-pinned
[Stream parity contract](stream-parity.md).

## 4. Tooling Differences

Acceptable differences due to compilation vs interpretation model.
These tests check error conditions or output that can't be validated with the interpreter.

| Skip Reason | Description |
|-------------|-------------|
| `eval:compile_error` | Tests expecting compile-time errors (e.g., `expect_compile_error`) |
| `eval:error_result` | Tests expecting runtime errors (e.g., `= error` or `error="message"`) |
| `eval:stdout` | Tests checking stdout output (e.g., `stdout=...`) |
| `eval:stderr` | Tests checking stderr output (e.g., `stderr=...`) |
| `eval:exit_code` | Tests checking exit codes (e.g., `exit=...`) |
| `eval:builtin_test` | Tests using `Builtin.test` functions (internal test infrastructure) |

---

## 5. Compiler-Only Features

Features in compiler not in interpreter. These are skipped during validation.

### Internal Features

| Feature | Skip Reason | Description |
|---------|-------------|-------------|
| Integer division | `extension:integer_division` | `/` operator works on integers (Darklang requires `Int64.divide`) |
| Internal functions | `internal:helper_function` | Functions like `__digitToString`, `__findFrom` are implementation helpers |
| SkewList/HAMT | `internal:data_structure` | `Stdlib.Internal.SkewList` and `Stdlib.Internal.HAMT` are internal implementations |

### 5.3 Integer Division Operator

**Skip reason:** `extension:integer_division`

This compiler extends `/` to work on integers (truncating toward zero). Darklang only supports `/` for floats; integer division requires `Stdlib.Int64.divide`.

```
# This compiler
10 / 3 = 3  (integer division, truncates)
10.0 / 3.0 = 3.333...  (float division)

# Darklang
10L / 3L  → Error: floatDivide expects Float
Stdlib.Int64.divide 10L 3L  → 3
10.0 / 3.0  → 3.333...
```

---

## 6. Missing from Interpreter

Features implemented in this compiler that should be added to the Darklang interpreter.

| Feature | Skip Reason | Functions |
|---------|-------------|-----------|
| Bitwise operators | `semantic:bitwise` | `<<`, `>>`, `&`, `\|`, `^`, `~` |
| Boolean not | `semantic:boolean_not` | `!` |
| Random | `stdlib:random` | `Random.int64` |
| Byte operations | `stdlib:byte_ops` | `String.getByteAt` |
| Int64 math | `stdlib:int64_math` | `Int64.sub`, `Int64.mul`, `Int64.div`, `Int64.isEven`, `Int64.isOdd` |
| Float operations | `stdlib:float_ops` | `Float.toBits`, `Float.toInt`, `Float.abs` |
| Math conveniences | `stdlib:math_extensions` | `Math.e`, `Math.abs`, `Math.sqrt`, and Int64-returning `Math.truncate`, `floor`, `ceiling`, `round` |
| List functions | `stdlib:missing` | `List.take`, `List.drop` |
| String functions | `stdlib:missing` | `String.substring`, `String.take`, `String.drop` |

Float/Math parity behavior, the shortest-formatting divergence, and retained
extensions are revision-pinned in [float-math-parity.md](float-math-parity.md).
