# Known Bugs and Issues

This document tracks known bugs that haven't been fixed yet. For each bug, we include reproduction steps, analysis of the likely cause, and workarounds.

---

## Chained String Concatenation Bug

**Status**: Fixed
**Severity**: Medium
**Discovered**: During string interpolation implementation
**Fixed**: Bug-finding loop session (fix was already in place, added regression tests)

### Original Reproduction

```dark
let x = "one" in let y = "two" in x ++ " and " ++ y
```

This now works correctly and outputs `one and two`.

### Regression Tests Added

Tests added to `src/Tests/e2e/strings.e2e`:
- `let x = "one" in let y = "two" in x ++ " and " ++ y`
- `let a = "hello" in let b = "world" in let c = "!" in a ++ " " ++ b ++ c`

---

## List of Tuples Bug

**Status**: Open
**Severity**: High (blocks List.zip)
**Discovered**: Bug-finding loop investigation

### Reproduction

```dark
def test(n: Int64) : List<(Int64, Int64)> =
    let pair = (5, 6) in
    [pair]

let result = test(1) in
match result with
| [(a, b)] -> a  // Returns 0 instead of 5
| _ -> 999
```

**Expected output**: `5`
**Actual output**: `0`

### Key Observations

- Bug occurs ONLY when function has parameters (even unused)
- Without parameters, the code works correctly
- The FIRST element of the tuple is always 0
- The SECOND element is correct
- Affects `List<(Int64, Int64)>` and likely other tuple types
- Works correctly for `List<Int64>` and for tuples not in lists

### Minimal Reproduction

```dark
// Works:
def test() : List<(Int64, Int64)> = let pair = (5, 6) in [pair]

// Broken (just adding an unused parameter breaks it):
def test(n: Int64) : List<(Int64, Int64)> = let pair = (5, 6) in [pair]
```

### Analysis

The bug is in register allocation or code generation. When a function has parameters:
- The first element of tuples in lists becomes 0
- This happens even with literal values `(5, 6)`
- The list structure is correct, only the tuple's first element is wrong

Likely causes:
1. Parameter vreg presence affects interference graph construction
2. Something clobbers tuple[0] during list construction
3. HeapStore for first tuple element uses a temp register incorrectly

### Workaround

Currently none known. This blocks `List.zip` and other functions returning `List<tuple>`.

---

## Register Spilling Edge Cases

**Status**: Partially documented
**Severity**: Low (rare in practice)
**Documented in**: `KNOWN_ISSUES.md`

### Description

Some complex expressions with very high register pressure may produce incorrect code. The register allocator's spill handling has edge cases that aren't fully covered.

### Workaround

Break complex expressions into smaller parts with intermediate let bindings.

---

## Notes for Bug Hunters

When investigating bugs:

1. **Minimize the test case**: Find the smallest program that reproduces the issue
2. **Add a failing test**: Put it in the appropriate `.e2e` file with a comment
3. **Use verbose output**: `./compile --verbose program.dark` shows intermediate representations
4. **Check each pass**: The bug is usually in the transformation between two passes
5. **Pattern match exhaustiveness**: If adding new features, F# will warn about missing cases

### Debugging Commands

```bash
# Run with verbose output
./compile --verbose program.dark

# Run tests for specific file
dotnet test --filter "FullyQualifiedName~strings"

# Build and run a single program
dotnet run --project src/DarkCompiler -- program.dark && ./a.out
```

---

## Reporting New Bugs

When you find a new bug:

1. Create the minimal reproduction case
2. Document expected vs actual behavior
3. Add to this file with your analysis
4. Add a test case (can be commented out if it crashes)
5. Note any workarounds you've found
