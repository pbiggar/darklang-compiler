# Adding Features to the Dark Compiler

This guide provides step-by-step instructions for common extension patterns. Use the F# compiler's exhaustiveness warnings as your guide - they'll show you every location that needs updating.

## Model States Before Implementing

Before adding fields, cases, or representations, make invalid states
unrepresentable. Use precise domain types and group structurally related values
together. Reserve `Option` for genuine semantic absence; for every optional
field, review whether a discriminated union, a required value in the relevant
case, or removing the field would model the domain more accurately.

When the feature changes a representation, plan the migration to completion:
identify the single authoritative representation and remove superseded types,
conversion paths, and callers. Do not add defaults, shims, or special-case
workarounds to bridge unsupported states. If compatibility is required, define
and document it as first-class supported behavior, with appropriate tests.

## Adding a New Binary Operator

**Example**: Adding the modulo operator `%`

### Step 1: AST Definition (`src/DarkCompiler/AST.fs`)

Add the operator to the `BinOp` type:

```fsharp
type BinOp =
    | Add
    | Sub
    | Mul
    | Div
    | Mod  // <- Add here
    // ...
```

### Step 2: Lexer (`src/DarkCompiler/frontend/interpreter/Parser.fs`)

Extend the copied interpreter parser's tokenization and syntax only when the
language itself gains a new operator. Keep the parser's upstream structure and
behavior; add the corresponding `WrittenTypes` operator case.

### Step 3: Parser (`src/DarkCompiler/frontend/interpreter/Parser.fs`)

Add precedence and parsing in the interpreter parser, returning a written
operator node. Its syntax tests should cover precedence and source roundtrips.

### Step 4: Type Checking (`src/DarkCompiler/frontend/WrittenChecking.fs`)

Resolve the written operator to its checked AST case after checking both
operands and the result type.

### Step 5: ANF Representation (`src/DarkCompiler/ir/anf/ANF.fs`)

Add to ANF.BinOp:

```fsharp
type BinOp =
    | Add | Sub | Mul | Div
    | Mod  // <- Add here
```

### Step 6: AST to ANF (`src/DarkCompiler/passes/anf/AST_to_ANF.fs`)

Add conversion in `convertBinOp`:

```fsharp
| AST.Mod -> ANF.Mod
```

Also update any functions that pattern match on BinOp (the compiler will warn you).

### Step 7: MIR Representation (`src/DarkCompiler/ir/mir/MIR.fs`)

Add to MIR.BinOp:

```fsharp
type BinOp = Add | Sub | Mul | Div | Mod
```

### Step 8: ANF to MIR (`src/DarkCompiler/passes/anf/ANF_to_MIR.fs`)

Add conversion - usually straightforward:

```fsharp
| ANF.Mod -> MIR.Mod
```

### Step 9: LIR Representation (`src/DarkCompiler/ir/lir/LIR.fs`)

For ARM64, modulo requires special handling (no native instruction):

```fsharp
type Instr =
    // ...
    | Msub of dest:Operand * minuend:Operand * multiplicand:Operand * multiplier:Operand
```

### Step 10: MIR to LIR (`src/DarkCompiler/passes/mir/MIR_to_LIR.fs`)

Emit the instruction sequence. For modulo: `a % b = a - (a / b) * b`

```fsharp
| MIR.Mod ->
    // Emit: SDIV tmp, left, right
    // Emit: MSUB dest, tmp, right, left
```

### Step 11: Register Allocation (`src/DarkCompiler/passes/lir/RegisterAllocation.fs`)

Update liveness analysis and allocation for new instruction:

```fsharp
| LIR.Msub (dest, minuend, multiplicand, multiplier) ->
    // Define: dest
    // Use: minuend, multiplicand, multiplier
```

### Step 12: Code Generation (`src/DarkCompiler/backend/arm64/CodeGen.fs`, `src/DarkCompiler/backend/x64/CodeGen.fs`)

Generate backend-specific instructions. ARM64 can lower modulo directly to
`MSUB`; x64 needs the equivalent target-specific sequence.

```fsharp
| LIR.Msub (dest, minuend, multiplicand, multiplier) ->
    ARM64.MSUB (toReg dest, toReg minuend, toReg multiplicand, toReg multiplier)
```

### Step 13: Encoding (`src/DarkCompiler/backend/arm64/Encoding.fs`, `src/DarkCompiler/backend/x64/Encoding.fs`)

Encode any new backend instruction forms to machine code bytes. For ARM64
`MSUB`:

```fsharp
| ARM64.MSUB (rd, rn, rm, ra) ->
    // Encode per ARM64 specification
    0x9B008000u ||| (rm <<< 16) ||| (ra <<< 10) ||| (rn <<< 5) ||| rd
```

### Step 14: Tests (`src/Tests/e2e/`)

Add end-to-end tests:

```
// In integers.e2e
10 % 3 = stdout="1\n"
7 % 2 = stdout="1\n"
```

### Completion Review

- Does every new optional field represent genuine semantic absence, rather than
  an unmodeled state or invalid field combination?
- If this changed a representation, has the migration removed old types,
  conversions, and callers so that one authoritative representation remains?
- Are unsupported states exposed clearly, and is any retained compatibility
  explicitly documented and tested as supported behavior?

---

## Adding a New AST Node Type

**Example**: Adding `InterpolatedString` for `$"Hello {name}"`

### Step 1: Define in AST (`src/DarkCompiler/AST.fs`)

```fsharp
/// Part of an interpolated string
type StringPart =
    | StringText of string
    | StringExpr of Expr

/// Expression nodes
and Expr =
    // ...
    | InterpolatedString of StringPart list
```

### Step 2: Update ALL AST Traversal Functions

This is the tedious part. Search for functions that match on `Expr` and add cases. Common locations in `AST_to_ANF.fs`:

- `applySubstToExpr` - apply type substitutions
- `collectTypeApps` - collect generic instantiations
- `replaceTypeApps` - replace type applications
- `varOccursInExpr` - check variable occurrence
- `inlineLambdas` - inline lambda expressions
- `freeVars` - collect free variables
- `liftLambdasInExpr` - lambda lifting
- `inferType` - type inference
- `toANF` - main ANF conversion
- `toAtom` - convert to atom

In `TypeChecking.fs`:
- `checkExpr` - type check expression
- `collectFreeVars` - collect free variables for closures

### Step 3: Decide Desugaring Strategy

**Option A: Desugar in Parser** (simple but loses source info)
```fsharp
// In parser, convert immediately to StringConcat chain
```

**Option B: Desugar in ANF Pass** (recommended)
```fsharp
// In toANF:
| AST.InterpolatedString parts ->
    // Convert to BinOp(StringConcat, ...) chain
    let desugared = partsToConcat parts
    toANF desugared varGen env ...
```

### Step 4: Add Type Checking

```fsharp
| InterpolatedString parts ->
    // Check each expression part is a String
    // Return TString
```

---

## Adding a New Stdlib Function

### Option A: Implement in Dark (Preferred)

Add to the appropriate module file under `src/DarkCompiler/stdlib/`, such as
`src/DarkCompiler/stdlib/Int64.dark`:

```dark
module Stdlib.Int64

let abs (n: Int64) : Int64 =
    if n < 0 then 0 - n else n
```

This is preferred for pure functions because:
- Written in the language itself
- Automatically gets all compiler optimizations
- Easier to understand and modify

### Option B: Implement as Builtin

For functions requiring runtime primitives, add to `src/DarkCompiler/Stdlib.fs`:

```fsharp
let builtinFunctions = [
    ("Stdlib.Int64.abs", [AST.TInt64], AST.TInt64)
]
```

Then handle in code generation to emit special instructions.

---

## Adding a New Type

### Record Type

Records are straightforward - just define in Dark:

```dark
type Point = { x: Int64, y: Int64 }
```

The compiler handles nominal field access and construction automatically.
Record destructuring patterns are intentionally unsupported.

### Sum Type (Algebraic Data Type)

```dark
type Option<'T> = Some of T | None
```

The compiler generates:
- Tag-based representation (None=0, Some=[tag=1, payload])
- Pattern matching support
- Constructor functions

---

## Debugging Tips

1. **Use timing output**: `./dark -vv program.dark`; reserve `-vvv` for cases
   that genuinely require every representation.
2. **Check intermediate representations**: Use `--dump-anf`, `--dump-mir`, or
   `--dump-lir` with `--dump-function=TEXT`; add `--dump-ir-summary` for an
   inventory or `--dump-ir-output=FILE` for complete retained evidence.
3. **Write minimal test case**: Reduce to smallest failing example
4. **Check exhaustiveness warnings**: F# compiler shows all missing cases
5. **Build, then run tests frequently**: `./build --ai && ./run-tests --ai`
   catches regressions early without rebuilding inside the test launcher.
