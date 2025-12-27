# Known Bugs and Issues

This document tracks known bugs that haven't been fixed yet. For each bug, we include reproduction steps, analysis of the likely cause, and workarounds.

---

## Chained String Concatenation Bug

**Status**: Open
**Severity**: Medium (has workaround)
**Discovered**: During string interpolation implementation

### Reproduction

```dark
let x = "one" in let y = "two" in x ++ " and " ++ y
```

**Expected output**: `one and two`
**Actual output**: Garbage characters or segfault

### Analysis

The bug is NOT in parsing or type checking - those work correctly. The issue is in code generation, likely in one of:

1. **Register allocation**: Variables `x` and `y` may be getting the same register
2. **String memory management**: Reference counting may free strings prematurely
3. **Evaluation order**: Left-to-right concatenation may have issues with multiple variables

### Workaround

Use intermediate let bindings to avoid chained concatenation with variables:

```dark
// Instead of:
let x = "one" in let y = "two" in x ++ " and " ++ y

// Use:
let x = "one" in
let y = "two" in
let temp = x ++ " and " in
temp ++ y
```

### Investigation Notes

- Single concatenation works: `x ++ y` is fine
- Literal-only chains work: `"a" ++ "b" ++ "c"` is fine
- Issue appears when mixing variables with chained `++`

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
