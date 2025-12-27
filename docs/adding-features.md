# Adding Features to the Dark Compiler

This guide provides step-by-step instructions for common extension patterns. Use the F# compiler's exhaustiveness warnings as your guide - they'll show you every location that needs updating.

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

### Step 2: Lexer (`src/DarkCompiler/passes/1_Parser.fs`)

Add a token type and lexer case:

```fsharp
type Token =
    | TPercent  // <- Add token
    // ...

// In the lexer function, add:
| '%' -> (TPercent, rest)
```

### Step 3: Parser (`src/DarkCompiler/passes/1_Parser.fs`)

Add operator precedence and parsing. For `%`, it has same precedence as `*` and `/`:

```fsharp
// In parseMulDiv or equivalent:
| TPercent :: rest ->
    // Parse right operand and build BinOp
    AST.BinOp (AST.Mod, left, right)
```

### Step 4: Type Checking (`src/DarkCompiler/passes/1.5_TypeChecking.fs`)

Add type rules for the operator:

```fsharp
| AST.BinOp (AST.Mod, left, right) ->
    // Both operands must be Int64, result is Int64
    checkBinOp left right AST.TInt64 AST.TInt64
```

### Step 5: ANF Representation (`src/DarkCompiler/ANF.fs`)

Add to ANF.BinOp:

```fsharp
type BinOp =
    | Add | Sub | Mul | Div
    | Mod  // <- Add here
```

### Step 6: AST to ANF (`src/DarkCompiler/passes/2_AST_to_ANF.fs`)

Add conversion in `convertBinOp`:

```fsharp
| AST.Mod -> ANF.Mod
```

Also update any functions that pattern match on BinOp (the compiler will warn you).

### Step 7: MIR Representation (`src/DarkCompiler/MIR.fs`)

Add to MIR.BinOp:

```fsharp
type BinOp = Add | Sub | Mul | Div | Mod
```

### Step 8: ANF to MIR (`src/DarkCompiler/passes/3_ANF_to_MIR.fs`)

Add conversion - usually straightforward:

```fsharp
| ANF.Mod -> MIR.Mod
```

### Step 9: LIR Representation (`src/DarkCompiler/LIR.fs`)

For ARM64, modulo requires special handling (no native instruction):

```fsharp
type Instr =
    // ...
    | Msub of dest:Operand * minuend:Operand * multiplicand:Operand * multiplier:Operand
```

### Step 10: MIR to LIR (`src/DarkCompiler/passes/4_MIR_to_LIR.fs`)

Emit the instruction sequence. For modulo: `a % b = a - (a / b) * b`

```fsharp
| MIR.Mod ->
    // Emit: SDIV tmp, left, right
    // Emit: MSUB dest, tmp, right, left
```

### Step 11: Register Allocation (`src/DarkCompiler/passes/5_RegisterAllocation.fs`)

Update liveness analysis and allocation for new instruction:

```fsharp
| LIR.Msub (dest, minuend, multiplicand, multiplier) ->
    // Define: dest
    // Use: minuend, multiplicand, multiplier
```

### Step 12: Code Generation (`src/DarkCompiler/passes/6_CodeGen.fs`)

Generate ARM64 instructions:

```fsharp
| LIR.Msub (dest, minuend, multiplicand, multiplier) ->
    ARM64.MSUB (toReg dest, toReg minuend, toReg multiplicand, toReg multiplier)
```

### Step 13: ARM64 Encoding (`src/DarkCompiler/passes/7_ARM64_Encoding.fs`)

Encode to machine code bytes:

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

This is the tedious part. Search for functions that match on `Expr` and add cases. Common locations in `2_AST_to_ANF.fs`:

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

In `1.5_TypeChecking.fs`:
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

Add to `src/DarkCompiler/stdlib.dark`:

```dark
def Stdlib.Int64.abs(n: Int64) : Int64 =
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

The compiler handles field access, pattern matching, and construction automatically.

### Sum Type (Algebraic Data Type)

```dark
type Option<T> = Some of T | None
```

The compiler generates:
- Tag-based representation (None=0, Some=[tag=1, payload])
- Pattern matching support
- Constructor functions

---

## Debugging Tips

1. **Use verbose output**: `./compile --verbose program.dark`
2. **Check intermediate representations**: Add print statements in passes
3. **Write minimal test case**: Reduce to smallest failing example
4. **Check exhaustiveness warnings**: F# compiler shows all missing cases
5. **Run tests frequently**: `dotnet test` catches regressions early
