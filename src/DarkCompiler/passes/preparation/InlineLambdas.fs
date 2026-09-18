// InlineLambdas.fs - Inline lexical lambda bindings before closure conversion.

module InlineLambdas

open MemoryModel
open ANF
open SpecializationIdentity

type LambdaEnv = Map<string, CheckedAST.Expr>

/// Check if a variable occurs in an expression (for dead code elimination)
let rec varOccursInExpr (name: string) (expr: CheckedAST.Expr) : bool =
    match expr with
    | CheckedAST.BoundaryRender (_, value) -> varOccursInExpr name value
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _ | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _ | CheckedAST.RuntimeError _ -> false
    | CheckedAST.Var n -> n = name
    | CheckedAST.BinOp (_, left, right) -> varOccursInExpr name left || varOccursInExpr name right
    | CheckedAST.UnaryOp (_, inner) -> varOccursInExpr name inner
    | CheckedAST.Let (pattern, value, body) ->
        varOccursInExpr name value
        || (not (CheckedAST.letPatternBindings pattern |> List.contains name)
            && varOccursInExpr name body)
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        if CheckedAST.recursiveBindingName recursion = name then false
        else varOccursInExpr name value || varOccursInExpr name body
    | CheckedAST.If (cond, thenBranch, elseBranch) ->
        varOccursInExpr name cond || varOccursInExpr name thenBranch || varOccursInExpr name elseBranch
    | CheckedAST.Sequence (first, next) ->
        varOccursInExpr name first || varOccursInExpr name next
    | CheckedAST.Call (funcName, args) ->
        // funcName could be a lambda variable reference (parser can't distinguish)
        funcName = name || (args |> exprArgsToList |> List.exists (varOccursInExpr name))
    | CheckedAST.TypeApp (_, _, args) -> args |> exprArgsToList |> List.exists (varOccursInExpr name)
    | CheckedAST.TupleLiteral elements -> List.exists (varOccursInExpr name) elements
    | CheckedAST.TupleAccess (tuple, _) -> varOccursInExpr name tuple
    | CheckedAST.DictLiteral (_, _, entries) ->
        List.exists (fun (key, value) -> varOccursInExpr name key || varOccursInExpr name value) entries
    | CheckedAST.RecordLiteral (_, fields) -> List.exists (fun (_, e) -> varOccursInExpr name e) fields
    | CheckedAST.RecordUpdate (record, updates) ->
        varOccursInExpr name record || List.exists (fun (_, e) -> varOccursInExpr name e) updates
    | CheckedAST.RecordAccess (record, _) -> varOccursInExpr name record
    | CheckedAST.Constructor (_, _, fields) -> List.exists (varOccursInExpr name) fields
    | CheckedAST.Match (scrutinee, cases) ->
        varOccursInExpr name scrutinee ||
        List.exists (fun (mc: CheckedAST.MatchCase) ->
            (mc.Guard |> Option.map (varOccursInExpr name) |> Option.defaultValue false) ||
            varOccursInExpr name mc.Body) cases
    | CheckedAST.ListLiteral elements -> List.exists (varOccursInExpr name) elements
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        // If name is shadowed by a parameter, it doesn't occur
        let paramNames =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect (fun parameter -> CheckedAST.letPatternBindings parameter.Pattern)
            |> Set.ofList
        if Set.contains name paramNames then false
        else varOccursInExpr name body
    | CheckedAST.Apply (func, args)
    | CheckedAST.IndirectApply (func, args) ->
        varOccursInExpr name func || (args |> exprArgsToList |> List.exists (varOccursInExpr name))
    | CheckedAST.FuncRef _ ->
        false  // Function references don't contain variable references
    | CheckedAST.Closure (_, captures) ->
        // Check if name occurs in captured expressions
        List.exists (varOccursInExpr name) captures
    | CheckedAST.InterpolatedString parts ->
        parts |> List.exists (fun part ->
            match part with
            | CheckedAST.StringText _ -> false
            | CheckedAST.StringExpr e -> varOccursInExpr name e)

/// Inline lambdas at Apply sites
/// lambdaEnv: maps variable names to their lambda expressions
let rec inlineLambdas (expr: CheckedAST.Expr) (lambdaEnv: LambdaEnv) : CheckedAST.Expr =
    match expr with
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _ | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _ | CheckedAST.RuntimeError _ ->
        expr
    | CheckedAST.BoundaryRender (renderer, value) ->
        CheckedAST.BoundaryRender (renderer, inlineLambdas value lambdaEnv)
    | CheckedAST.Var _ -> expr  // Variable references stay as-is (not at call position)
    | CheckedAST.BinOp (op, left, right) ->
        CheckedAST.BinOp (op, inlineLambdas left lambdaEnv, inlineLambdas right lambdaEnv)
    | CheckedAST.UnaryOp (op, inner) ->
        CheckedAST.UnaryOp (op, inlineLambdas inner lambdaEnv)
    | CheckedAST.Let (pattern, value, body) ->
        let value' = inlineLambdas value lambdaEnv
        let childEnv =
            CheckedAST.letPatternBindings pattern
            |> List.fold (fun current name -> Map.remove name current) lambdaEnv
        // If the value is a lambda, make the name callable only in the body.
        let lambdaEnv' =
            match pattern, value' with
            | CheckedAST.LPVariable name, CheckedAST.Lambda _ -> Map.add name value' childEnv
            | _ -> childEnv
        let body' = inlineLambdas body lambdaEnv'
        CheckedAST.Let (pattern, value', body')
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        let childEnv = Map.remove (CheckedAST.recursiveBindingName recursion) lambdaEnv
        CheckedAST.RecursiveLet (recursion, inlineLambdas value childEnv, inlineLambdas body childEnv)
    | CheckedAST.If (cond, thenBranch, elseBranch) ->
        CheckedAST.If (inlineLambdas cond lambdaEnv, inlineLambdas thenBranch lambdaEnv, inlineLambdas elseBranch lambdaEnv)
    | CheckedAST.Sequence (first, next) ->
        CheckedAST.Sequence (inlineLambdas first lambdaEnv, inlineLambdas next lambdaEnv)
    | CheckedAST.Call (funcName, args) ->
        let args' = AST.NonEmptyList.map (fun a -> inlineLambdas a lambdaEnv) args
        // Check if funcName is actually a lambda variable (parser can't distinguish)
        match Map.tryFind funcName lambdaEnv with
        | Some _ -> CheckedAST.Apply (CheckedAST.Var funcName, args')
        | None -> CheckedAST.Call (funcName, args')
    | CheckedAST.TypeApp (funcName, typeArgs, args) ->
        CheckedAST.TypeApp (funcName, typeArgs, AST.NonEmptyList.map (fun a -> inlineLambdas a lambdaEnv) args)
    | CheckedAST.TupleLiteral elements ->
        CheckedAST.TupleLiteral (List.map (fun e -> inlineLambdas e lambdaEnv) elements)
    | CheckedAST.TupleAccess (tuple, index) ->
        CheckedAST.TupleAccess (inlineLambdas tuple lambdaEnv, index)
    | CheckedAST.DictLiteral (keyType, valueType, entries) ->
        CheckedAST.DictLiteral (
            keyType,
            valueType,
            entries
            |> List.map (fun (key, value) ->
                (inlineLambdas key lambdaEnv, inlineLambdas value lambdaEnv))
        )
    | CheckedAST.RecordLiteral (typeName, fields) ->
        CheckedAST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, inlineLambdas e lambdaEnv)) fields)
    | CheckedAST.RecordUpdate (record, updates) ->
        CheckedAST.RecordUpdate (inlineLambdas record lambdaEnv, List.map (fun (n, e) -> (n, inlineLambdas e lambdaEnv)) updates)
    | CheckedAST.RecordAccess (record, fieldName) ->
        CheckedAST.RecordAccess (inlineLambdas record lambdaEnv, fieldName)
    | CheckedAST.Constructor (typeName, variantName, fields) ->
        CheckedAST.Constructor (typeName, variantName, List.map (fun e -> inlineLambdas e lambdaEnv) fields)
    | CheckedAST.Match (scrutinee, cases) ->
        let cases' =
            cases
            |> List.map (fun mc ->
                let caseBoundNames =
                    mc.Patterns
                    |> AST.NonEmptyList.toList
                    |> List.collect (fun pattern ->
                        AST.validateBinders (AST.MatchBinderPattern pattern)
                        |> Result.defaultValue [])
                let caseEnv =
                    caseBoundNames
                    |> List.fold (fun current name -> Map.remove name current) lambdaEnv
                { mc with
                    Guard = mc.Guard |> Option.map (fun g -> inlineLambdas g caseEnv)
                    Body = inlineLambdas mc.Body caseEnv })
        CheckedAST.Match (inlineLambdas scrutinee lambdaEnv, cases')
    | CheckedAST.ListLiteral elements ->
        CheckedAST.ListLiteral (List.map (fun e -> inlineLambdas e lambdaEnv) elements)
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        let bodyEnv =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect (fun parameter -> CheckedAST.letPatternBindings parameter.Pattern)
            |> List.fold (fun current name -> Map.remove name current) lambdaEnv
        CheckedAST.Lambda (parameters, returnAnnotation, inlineLambdas body bodyEnv)
    | CheckedAST.Apply (func, args) ->
        let args' = AST.NonEmptyList.map (fun a -> inlineLambdas a lambdaEnv) args
        match func with
        | CheckedAST.Var name ->
            // Check if this variable is a known lambda
            match Map.tryFind name lambdaEnv with
            | Some _ ->
                CheckedAST.Apply (CheckedAST.Var name, args')
            | None ->
                // Unknown function variable - keep as-is (will error later if not valid)
                CheckedAST.Apply (CheckedAST.Var name, args')
        | _ ->
            // Non-variable function (could be lambda or other expr)
            CheckedAST.Apply (inlineLambdas func lambdaEnv, args')
    | CheckedAST.IndirectApply (func, args) ->
        CheckedAST.IndirectApply (
            inlineLambdas func lambdaEnv,
            AST.NonEmptyList.map (fun arg -> inlineLambdas arg lambdaEnv) args
        )
    | CheckedAST.FuncRef _ ->
        // Function references don't need lambda inlining
        expr
    | CheckedAST.Closure (funcName, captures) ->
        // Inline lambdas in captured expressions
        CheckedAST.Closure (funcName, List.map (fun c -> inlineLambdas c lambdaEnv) captures)
    | CheckedAST.InterpolatedString parts ->
        let inlinePart part =
            match part with
            | CheckedAST.StringText s -> CheckedAST.StringText s
            | CheckedAST.StringExpr e -> CheckedAST.StringExpr (inlineLambdas e lambdaEnv)
        CheckedAST.InterpolatedString (List.map inlinePart parts)

/// Inline lambdas in a function definition
let inlineLambdasInFunc (funcDef: CheckedAST.FunctionDef) : CheckedAST.FunctionDef =
    { funcDef with Body = inlineLambdas funcDef.Body Map.empty }

/// Inline lambdas in a program
let inlineLambdasInProgram (program: CheckedAST.Program) : CheckedAST.Program =
    let (CheckedAST.Program topLevels) = program
    let topLevels' =
        topLevels
        |> List.map (function
            | CheckedAST.FunctionDef f -> CheckedAST.FunctionDef (inlineLambdasInFunc f)
            | CheckedAST.Expression e -> CheckedAST.Expression (inlineLambdas e Map.empty)
            | CheckedAST.ValueDef valueDef ->
                let body = inlineLambdas (CheckedAST.valueDefBody valueDef) Map.empty
                CheckedAST.ValueDef { valueDef with Body = body }
            | CheckedAST.TypeDef t -> CheckedAST.TypeDef t)
    CheckedAST.Program topLevels'

// ============================================================================
// Lambda Lifting: Convert Lambdas to Top-Level Functions with Closures
// ============================================================================
//
// Lambda lifting transforms nested lambda expressions into top-level functions.
// The process handles both capturing and non-capturing lambdas uniformly.
//
// Algorithm:
// 1. Identify lambdas in argument positions (function calls, let bindings)
// 2. Collect free variables (captures) from each lambda body
// 3. Generate a lifted function with signature: (closure_tuple, original_params...) -> result
// 4. Replace the lambda with a ClosureAlloc expression containing the function and captures
//
// Closure representation at runtime:
//   [func_ptr, cap1, cap2, ...]  -- heap-allocated tuple
//
// The lifted function extracts captures from the closure tuple:
//   let __closure_N(__closure, x, y) =
//       let cap1 = __closure.1
//       let cap2 = __closure.2
//       in <original body with captures replaced>
//
// All function values use closures for uniform calling convention, even non-capturing
// lambdas and function references. This simplifies higher-order function support.
//
// See docs/compiler/frontend/closures.md for detailed documentation.
// ============================================================================

/// State for lambda lifting - tracks generated functions and counter
