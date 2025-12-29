# Result Handling Patterns

This codebase uses F#'s standard `Result<T, E>` type extensively for error handling. No exceptions, no `failwith`, no success flags.

## Error Types

**Type Checking Errors** (`1.5_TypeChecking.fs`):
```fsharp
type TypeError =
    | TypeMismatch of expected:Type * actual:Type * context:string
    | UndefinedVariable of name:string
    | MissingTypeAnnotation of context:string
    | InvalidOperation of op:string * types:Type list
    | GenericError of string
```

**Other Passes**: Use `string` for error messages.

## Pattern 1: Nested Match (Most Common)

Used when chaining multiple Result-returning operations:

```fsharp
match loadStdlib() with
| Error e -> Error e
| Ok stdlibAst ->
    match TypeChecking.checkProgramWithEnv withMain with
    | Error e -> Error (TypeChecking.typeErrorToString e)
    | Ok (_, typedStdlib, typeCheckEnv) ->
        match AST_to_ANF.convertProgramWithTypes typedStdlib with
        | Error e -> Error e
        | Ok anfProgram ->
            // Continue processing...
            Ok result
```

This pattern is verbose but explicit. Use it when:
- You need to transform errors between different types
- Each step may need different error handling
- You want maximum clarity about error flow

## Pattern 2: Result.bind Chaining

For simpler sequential operations:

```fsharp
lex input
|> Result.bind parse
|> Result.bind validateOptions
```

Or with inline lambdas:

```fsharp
checkExpr leftExpr env
|> Result.bind (fun (leftType, left') ->
    checkExpr rightExpr env
    |> Result.bind (fun (rightType, right') ->
        validateBinaryOp op leftType rightType
        |> Result.map (fun resultType ->
            (resultType, BinOp(op, left', right'))
        )
    )
)
```

Use `Result.bind` when:
- Error types are consistent (no transformation needed)
- Operations are simple and sequential
- You want more compact code

## Pattern 3: Exception Wrapper

Wrap .NET methods that throw exceptions:

```fsharp
let tryStartProcess (info: ProcessStartInfo) : Result<Process, string> =
    try Ok (Process.Start(info))
    with ex -> Error ex.Message

let tryReadFile (path: string) : Result<string, string> =
    try Ok (File.ReadAllText(path))
    with ex -> Error $"Failed to read {path}: {ex.Message}"
```

Always create wrappers for any external function that might throw.

## Pattern 4: Error Context Addition

Add context when propagating errors:

```fsharp
loadDarkFile filename
|> Result.mapError (fun err -> $"Error parsing {filename}: {err}")

TypeChecking.checkProgram ast
|> Result.mapError TypeChecking.typeErrorToString
```

Use `Result.mapError` to:
- Convert error types (e.g., `TypeError` to `string`)
- Add file/line/context information
- Make errors more user-friendly

## Pattern 5: Folding Over Collections

Process a list where each element might fail:

```fsharp
bindings
|> List.fold (fun acc binding ->
    acc |> Result.bind (fun m ->
        match Map.tryFind name m with
        | None -> Ok (Map.add name typ m)
        | Some existing -> Error $"Duplicate binding: {name}"
    )
) (Ok Map.empty)
```

## Pattern 6: Parallel Results (Tuple Matching)

When you have multiple independent operations:

```fsharp
match checkExpr cond env, checkExpr thenBr env, checkExpr elseBr env with
| Ok condResult, Ok thenResult, Ok elseResult ->
    Ok (combine condResult thenResult elseResult)
| Error e, _, _ -> Error e
| _, Error e, _ -> Error e
| _, _, Error e -> Error e
```

## Key Files Using Result

| File | Result Usage | Error Type |
|------|-------------|------------|
| `CompilerLibrary.fs` | Main orchestration | `string` |
| `1.5_TypeChecking.fs` | Type validation | `TypeError` |
| `1_Parser.fs` | Lexing/parsing | `string` |
| `2_AST_to_ANF.fs` | ANF conversion | `string` |

## Things to Avoid

**Don't use exceptions:**
```fsharp
// BAD
let result = riskyFunction()  // might throw

// GOOD
match tryRiskyFunction() with
| Ok result -> ...
| Error e -> ...
```

**Don't use Option when you need error info:**
```fsharp
// BAD - loses error information
let result = Map.tryFind key map

// GOOD - when you need to report the error
match Map.tryFind key map with
| Some v -> Ok v
| None -> Error $"Key not found: {key}"
```

**Don't ignore errors:**
```fsharp
// BAD
let _ = riskyOperation()

// GOOD
match riskyOperation() with
| Ok _ -> ()
| Error e -> // handle or propagate
```
