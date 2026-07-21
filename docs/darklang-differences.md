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
| Integer literals | `5` | `5L` | Add L suffix |
| Sized integers | `1y`, `1s`, `1l` | Not supported | Int8, Int16, Int32 suffixes |
| Unsigned integers | `1uy`, `1us`, `1ul` | Not supported | UInt8, UInt16, UInt32 suffixes |
| List separators | `[1, 2]` | `[1L; 2L]` | Comma to semicolon |
| Function calls | `Mod.fn(a, b)` | `Stdlib.Mod.fn a b` | Parentheses to spaces |
| Lambdas | `(x: T) => body` | `fun x -> body` | Different arrow syntax |
| Type parameters | `List<Int64>` | Different syntax | Generic type notation |
| String interpolation | `$"Hello {x}"` | Not supported | Interpolated strings |

### 1.1 Integer Literals

**Conversion:** `5` → `5L`

This compiler allows bare integers; Darklang requires the `L` suffix.

```
# This compiler
1 + 2 = 3

# Darklang
darklang-interpreter eval "1L + 2L"
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

### 1.4 Lambda Syntax

**Conversion:** `(x: Int64) => body` → `fun x -> body`

Different arrow syntax between compilers.

---

## 2. Semantic Bugs

Areas where compiler produces WRONG output. These need to be fixed to match Darklang.

| Bug | Skip Reason | Description | Status |
|-----|-------------|-------------|--------|
| Division `/` | `semantic:division` | Integer vs float division | Needs fix |
| Modulo `%` | `semantic:modulo` | Negative divisor handling | Fixed |
| Float precision | `eval:float_precision` | High-precision floats have different representation | Needs fix |

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

## 3. Tooling Differences

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

## 4. Compiler-Only Features

Features in compiler not in interpreter. These are skipped during validation.

### Internal Features

| Feature | Skip Reason | Description |
|---------|-------------|-------------|
| Integer division | `extension:integer_division` | `/` operator works on integers (Darklang requires `Int64.divide`) |
| Internal functions | `internal:helper_function` | Functions like `__digitToString`, `__findFrom` are implementation helpers |
| FingerTree/HAMT | `internal:data_structure` | `Stdlib.__FingerTree` and `Stdlib.__HAMT` are internal implementations |

### 4.3 Integer Division Operator

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

## 5. Missing from Interpreter

Features implemented in this compiler that should be added to the Darklang interpreter.

| Feature | Skip Reason | Functions |
|---------|-------------|-----------|
| Bitwise operators | `semantic:bitwise` | `<<`, `>>`, `&`, `\|`, `^`, `~` |
| Boolean not | `semantic:boolean_not` | `!` |
| Random | `stdlib:random` | `Random.int64` |
| Byte operations | `stdlib:byte_ops` | `String.getByteAt` |
| Int64 math | `stdlib:int64_math` | `Int64.sub`, `Int64.mul`, `Int64.div`, `Int64.isEven`, `Int64.isOdd` |
| Float operations | `stdlib:float_ops` | `Float.toBits`, `Float.toString`, `Float.toInt`, `Float.abs`, `Float.negate`, `Float.sqrt` |
| List functions | `stdlib:missing` | `List.take`, `List.drop` |
| String functions | `stdlib:missing` | `String.substring`, `String.take`, `String.drop` |
