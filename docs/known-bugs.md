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

**Status**: Fixed
**Severity**: High (blocks List.zip)
**Discovered**: Bug-finding loop investigation
**Fixed**: Pattern matching for list cons patterns inside tuple patterns was using TupleGet
         (assuming cons-cell structure) instead of proper FingerTree head/tail calls.

### Original Reproduction

```dark
def test(n: Int64) : List<(Int64, Int64)> =
    let pair = (5, 6) in
    [pair]

let result = test(1) in
match result with
| [(a, b)] -> a  // Previously returned 0 instead of 5
| _ -> 999
```

This now works correctly and outputs `5`.

### Root Cause

In `2_AST_to_ANF.fs`, the `collectPatternBindings` function for `PListCons` patterns inside
`PTuple` patterns was incorrectly using `TupleGet(list, 1)` for head and `TupleGet(list, 2)`
for tail, assuming lists are simple cons cells. However, lists in this compiler are FingerTrees.

The fix was to use proper FingerTree operations:
- `ANF.Call ("Stdlib.FingerTree.headUnsafe_i64", [currentList])` for head
- `ANF.Call ("Stdlib.FingerTree.tail_i64", [currentList])` for tail

### List.zip Now Works

```dark
let zipped = List.zip<Int64, Int64>([1, 2, 3], [4, 5, 6]) in
match zipped with
| [(a, b), (c, d), (e, f)] -> a + b + c + d + e + f  // Returns 21
| _ -> 999
```

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
