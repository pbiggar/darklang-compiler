# Known Bugs and Issues

This document tracks currently known bugs and resolved historical bugs. For
each open bug, include reproduction steps, analysis of the likely cause, and
workarounds.

---

## Resolved Historical Bugs

---

## FingerTree Deep Operation Chain Bug

**Status**: Superseded; FingerTree was replaced by the direct-payload skew RAL
**Severity**: Previously Medium
**Discovered**: Investigation of Dict.fromList failures
**Revalidated**: Public Dict.fromList and List.fold cases with 5 elements now pass

### Original Description

The original investigation reported incorrect values after 4 or more chained
FingerTree tail operations followed by element extraction. The notes pointed at
register allocation or stack management for deeply nested function calls.

The old internal reproduction used `Stdlib.FingerTree.tail` and
`Stdlib.FingerTree.headUnsafe`, but those names are not public language symbols.
The corresponding internal helpers are also rejected from user code because
internal identifiers cannot be referenced directly.

### Public Regression Coverage

The originally reported user-visible impact is now covered by public behavior:

```dark
Stdlib.Dict.size(Stdlib.Dict.fromListOverwritingDuplicates([("1", 10), ("2", 20), ("3", 30), ("4", 40), ("5", 50)]))

Stdlib.Dict.get(Stdlib.Dict.fromListOverwritingDuplicates([("1", 10), ("2", 20), ("3", 30), ("4", 40), ("5", 50)]), "5")

List.fold([1, 2, 3, 4, 5], 0, fun acc x -> acc + x)
```

These currently return `5I`, `Some(50)`, and `15` respectively.

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
let test(n: Int64) : List<(Int64, Int64)> =
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

## Open Bugs

---

## Register Spilling Edge Cases

**Status**: Partially documented
**Severity**: Low (rare in practice)
**Related investigation**: `docs/register-allocation-bug-investigation.md`

### Description

Some complex expressions with very high register pressure may produce incorrect code. The register allocator's spill handling has edge cases that aren't fully covered.

### Workaround

Break complex expressions into smaller parts with intermediate let bindings.

---

## Notes for Bug Hunters

When investigating bugs:

1. **Minimize the test case**: Find the smallest program that reproduces the issue
2. **Add a failing test**: Put it in the appropriate `.e2e` file with a comment
3. **Use IR dumps**: `./dark --dump-anf`, `./dark --dump-mir`, and
   `./dark --dump-lir` show intermediate representations
4. **Check each pass**: The bug is usually in the transformation between two passes
5. **Pattern match exhaustiveness**: If adding new features, F# will warn about missing cases

### Debugging Commands

```bash
# Dump intermediate representations
./dark --dump-anf prog.dark
./dark --dump-mir prog.dark
./dark --dump-lir prog.dark

# Run the full test suite with AI-friendly progress output
./run-tests --ai

# Build and run a single program
./dark -r prog.dark
```

---

## Reporting New Bugs

When you find a new bug:

1. Create the minimal reproduction case
2. Document expected vs actual behavior
3. Add to this file with your analysis
4. Add a failing test case before fixing the bug
5. Note any workarounds you've found
