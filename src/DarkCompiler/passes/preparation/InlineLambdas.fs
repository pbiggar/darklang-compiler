// InlineLambdas.fs - Inline lexical lambda bindings before closure conversion.

module InlineLambdas

open MemoryModel
open ANF
open SpecializationIdentity

type LambdaEnv = Map<string, AST.Expr>

/// Check if a variable occurs in an expression (for dead code elimination)
let rec varOccursInExpr (name: string) (expr: AST.Expr) : bool =
    match expr with
    | AST.BoundaryRender (_, value) -> varOccursInExpr name value
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _ | AST.UInt128Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.RuntimeError _ -> false
    | AST.Var n -> n = name
    | AST.BinOp (_, left, right) -> varOccursInExpr name left || varOccursInExpr name right
    | AST.UnaryOp (_, inner) -> varOccursInExpr name inner
    | AST.Let (pattern, value, body) ->
        varOccursInExpr name value
        || (not (AST.letPatternBindings pattern |> List.contains name)
            && varOccursInExpr name body)
    | AST.RecursiveLet (recursion, value, body) ->
        if AST.recursiveBindingName recursion = name then false
        else varOccursInExpr name value || varOccursInExpr name body
    | AST.If (cond, thenBranch, elseBranch) ->
        varOccursInExpr name cond || varOccursInExpr name thenBranch || varOccursInExpr name elseBranch
    | AST.Sequence (first, next) ->
        varOccursInExpr name first || varOccursInExpr name next
    | AST.Call (funcName, args) ->
        // funcName could be a lambda variable reference (parser can't distinguish)
        funcName = name || (args |> exprArgsToList |> List.exists (varOccursInExpr name))
    | AST.TypeApp (_, _, args) -> args |> exprArgsToList |> List.exists (varOccursInExpr name)
    | AST.TupleLiteral elements -> List.exists (varOccursInExpr name) elements
    | AST.TupleAccess (tuple, _) -> varOccursInExpr name tuple
    | AST.DictLiteral (_, _, entries) -> List.exists (fun (key, value) -> varOccursInExpr name key || varOccursInExpr name value) entries
    | AST.RecordLiteral (_, fields) -> List.exists (fun (_, e) -> varOccursInExpr name e) fields
    | AST.RecordUpdate (record, updates) ->
        varOccursInExpr name record || List.exists (fun (_, e) -> varOccursInExpr name e) updates
    | AST.RecordAccess (record, _) -> varOccursInExpr name record
    | AST.Constructor (_, _, payload) -> Option.exists (varOccursInExpr name) payload
    | AST.Match (scrutinee, cases) ->
        varOccursInExpr name scrutinee ||
        List.exists (fun (mc: AST.MatchCase) ->
            (mc.Guard |> Option.map (varOccursInExpr name) |> Option.defaultValue false) ||
            varOccursInExpr name mc.Body) cases
    | AST.ListLiteral elements -> List.exists (varOccursInExpr name) elements
    | AST.Lambda (parameters, returnAnnotation, body) ->
        // If name is shadowed by a parameter, it doesn't occur
        let paramNames =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect (fun parameter -> AST.letPatternBindings parameter.Pattern)
            |> Set.ofList
        if Set.contains name paramNames then false
        else varOccursInExpr name body
    | AST.Apply (func, args)
    | AST.IndirectApply (func, args) ->
        varOccursInExpr name func || (args |> exprArgsToList |> List.exists (varOccursInExpr name))
    | AST.FuncRef _ ->
        false  // Function references don't contain variable references
    | AST.Closure (_, captures) ->
        // Check if name occurs in captured expressions
        List.exists (varOccursInExpr name) captures
    | AST.InterpolatedString parts ->
        parts |> List.exists (fun part ->
            match part with
            | AST.StringText _ -> false
            | AST.StringExpr e -> varOccursInExpr name e)

/// Inline lambdas at Apply sites
/// lambdaEnv: maps variable names to their lambda expressions
let rec inlineLambdas (expr: AST.Expr) (lambdaEnv: LambdaEnv) : AST.Expr =
    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _ | AST.UInt128Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.RuntimeError _ ->
        expr
    | AST.BoundaryRender (renderer, value) ->
        AST.BoundaryRender (renderer, inlineLambdas value lambdaEnv)
    | AST.Var _ -> expr  // Variable references stay as-is (not at call position)
    | AST.BinOp (op, left, right) ->
        AST.BinOp (op, inlineLambdas left lambdaEnv, inlineLambdas right lambdaEnv)
    | AST.UnaryOp (op, inner) ->
        AST.UnaryOp (op, inlineLambdas inner lambdaEnv)
    | AST.Let (pattern, value, body) ->
        let value' = inlineLambdas value lambdaEnv
        let childEnv =
            AST.letPatternBindings pattern
            |> List.fold (fun current name -> Map.remove name current) lambdaEnv
        // If the value is a lambda, make the name callable only in the body.
        let lambdaEnv' =
            match pattern, value' with
            | AST.LPVariable name, AST.Lambda _ -> Map.add name value' childEnv
            | _ -> childEnv
        let body' = inlineLambdas body lambdaEnv'
        AST.Let (pattern, value', body')
    | AST.RecursiveLet (recursion, value, body) ->
        let childEnv = Map.remove (AST.recursiveBindingName recursion) lambdaEnv
        AST.RecursiveLet (recursion, inlineLambdas value childEnv, inlineLambdas body childEnv)
    | AST.If (cond, thenBranch, elseBranch) ->
        AST.If (inlineLambdas cond lambdaEnv, inlineLambdas thenBranch lambdaEnv, inlineLambdas elseBranch lambdaEnv)
    | AST.Sequence (first, next) ->
        AST.Sequence (inlineLambdas first lambdaEnv, inlineLambdas next lambdaEnv)
    | AST.Call (funcName, args) ->
        let args' = AST.NonEmptyList.map (fun a -> inlineLambdas a lambdaEnv) args
        // Check if funcName is actually a lambda variable (parser can't distinguish)
        match Map.tryFind funcName lambdaEnv with
        | Some _ -> AST.Apply (AST.Var funcName, args')
        | None -> AST.Call (funcName, args')
    | AST.TypeApp (funcName, typeArgs, args) ->
        AST.TypeApp (funcName, typeArgs, AST.NonEmptyList.map (fun a -> inlineLambdas a lambdaEnv) args)
    | AST.TupleLiteral elements ->
        AST.TupleLiteral (List.map (fun e -> inlineLambdas e lambdaEnv) elements)
    | AST.TupleAccess (tuple, index) ->
        AST.TupleAccess (inlineLambdas tuple lambdaEnv, index)
    | AST.DictLiteral (keyType, valueType, entries) ->
        AST.DictLiteral (keyType, valueType, entries |> List.map (fun (key, value) -> (inlineLambdas key lambdaEnv, inlineLambdas value lambdaEnv)))
    | AST.RecordLiteral (typeName, fields) ->
        AST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, inlineLambdas e lambdaEnv)) fields)
    | AST.RecordUpdate (record, updates) ->
        AST.RecordUpdate (inlineLambdas record lambdaEnv, List.map (fun (n, e) -> (n, inlineLambdas e lambdaEnv)) updates)
    | AST.RecordAccess (record, fieldName) ->
        AST.RecordAccess (inlineLambdas record lambdaEnv, fieldName)
    | AST.Constructor (typeName, variantName, payload) ->
        AST.Constructor (typeName, variantName, Option.map (fun e -> inlineLambdas e lambdaEnv) payload)
    | AST.Match (scrutinee, cases) ->
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
        AST.Match (inlineLambdas scrutinee lambdaEnv, cases')
    | AST.ListLiteral elements ->
        AST.ListLiteral (List.map (fun e -> inlineLambdas e lambdaEnv) elements)
    | AST.Lambda (parameters, returnAnnotation, body) ->
        let bodyEnv =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect (fun parameter -> AST.letPatternBindings parameter.Pattern)
            |> List.fold (fun current name -> Map.remove name current) lambdaEnv
        AST.Lambda (parameters, returnAnnotation, inlineLambdas body bodyEnv)
    | AST.Apply (func, args) ->
        let args' = AST.NonEmptyList.map (fun a -> inlineLambdas a lambdaEnv) args
        match func with
        | AST.Var name ->
            // Check if this variable is a known lambda
            match Map.tryFind name lambdaEnv with
            | Some _ ->
                AST.Apply (AST.Var name, args')
            | None ->
                // Unknown function variable - keep as-is (will error later if not valid)
                AST.Apply (AST.Var name, args')
        | _ ->
            // Non-variable function (could be lambda or other expr)
            AST.Apply (inlineLambdas func lambdaEnv, args')
    | AST.IndirectApply (func, args) ->
        AST.IndirectApply (
            inlineLambdas func lambdaEnv,
            AST.NonEmptyList.map (fun arg -> inlineLambdas arg lambdaEnv) args
        )
    | AST.FuncRef _ ->
        // Function references don't need lambda inlining
        expr
    | AST.Closure (funcName, captures) ->
        // Inline lambdas in captured expressions
        AST.Closure (funcName, List.map (fun c -> inlineLambdas c lambdaEnv) captures)
    | AST.InterpolatedString parts ->
        let inlinePart part =
            match part with
            | AST.StringText s -> AST.StringText s
            | AST.StringExpr e -> AST.StringExpr (inlineLambdas e lambdaEnv)
        AST.InterpolatedString (List.map inlinePart parts)

/// Inline lambdas in a function definition
let inlineLambdasInFunc (funcDef: AST.FunctionDef) : AST.FunctionDef =
    { funcDef with Body = inlineLambdas funcDef.Body Map.empty }

/// Inline lambdas in a program
let inlineLambdasInProgram (program: AST.Program) : AST.Program =
    let (AST.Program topLevels) = program
    let topLevels' =
        topLevels
        |> List.map (function
            | AST.FunctionDef f -> AST.FunctionDef (inlineLambdasInFunc f)
            | AST.Expression e -> AST.Expression (inlineLambdas e Map.empty)
            | AST.ValueDef valueDef ->
                let body = inlineLambdas (AST.valueDefBody valueDef) Map.empty
                match valueDef with
                | AST.UncheckedValueDef (name, _) -> AST.ValueDef (AST.UncheckedValueDef (name, body))
                | AST.CheckedValueDef (name, typ, _) -> AST.ValueDef (AST.CheckedValueDef (name, typ, body))
            | AST.TypeDef t -> AST.TypeDef t)
    AST.Program topLevels'

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
