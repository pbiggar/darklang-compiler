// FreeVariables.fs - Collect expression and pattern binding dependencies.

module CheckedFreeVariables

open AST
open CheckingDiagnostics

// =============================================================================
// Free Variable Analysis for Closures
// =============================================================================
// When compiling lambdas, we need to identify which variables from the
// enclosing scope are referenced in the lambda body (free variables).
// Only these need to be captured in the closure.

/// Collect free variables in an expression.
/// Returns the set of variable names that are referenced but not bound locally.
/// bound: Set of names that are currently in scope (not free)
let rec collectFreeVars (expr: Expr) (bound: Set<string>) : Set<string> =
    match expr with
    | BoundaryRender (_, value) -> collectFreeVars value bound
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | RuntimeError _ ->
        Set.empty
    | Var name ->
        if Set.contains name bound || isBuiltinTestNanName name || isBuiltinTestInfinityName name then
            Set.empty
        else
            Set.singleton name
    | BinOp (_, left, right) ->
        Set.union (collectFreeVars left bound) (collectFreeVars right bound)
    | UnaryOp (_, inner) ->
        collectFreeVars inner bound
    | Let (pattern, value, body) ->
        let valueFree = collectFreeVars value bound
        let names = letPatternBindings pattern |> Set.ofList
        let bodyFree = collectFreeVars body (Set.union names bound)
        Set.union valueFree bodyFree
    | RecursiveLet (recursion, value, body) ->
        let name = recursiveBindingName recursion
        let recursiveBound = Set.add name bound
        Set.union (collectFreeVars value recursiveBound) (collectFreeVars body recursiveBound)
    | If (cond, thenBranch, elseBranch) ->
        let condFree = collectFreeVars cond bound
        let thenFree = collectFreeVars thenBranch bound
        let elseFree = collectFreeVars elseBranch bound
        Set.union condFree (Set.union thenFree elseFree)
    | Sequence (first, next) ->
        Set.union (collectFreeVars first bound) (collectFreeVars next bound)
    | Call (_, args) ->
        args
        |> NonEmptyList.toList
        |> List.map (fun e -> collectFreeVars e bound)
        |> List.fold Set.union Set.empty
    | TypeApp (_, _, args) ->
        args
        |> NonEmptyList.toList
        |> List.map (fun e -> collectFreeVars e bound)
        |> List.fold Set.union Set.empty
    | TupleLiteral elements ->
        elements |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | TupleAccess (tuple, _) ->
        collectFreeVars tuple bound
    | DictLiteral (_, _, entries) ->
        entries
        |> List.collect (fun (key, value) -> [collectFreeVars key bound; collectFreeVars value bound])
        |> List.fold Set.union Set.empty
    | RecordLiteral (_, fields) ->
        fields |> List.map (fun (_, e) -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | RecordUpdate (record, updates) ->
        let recordFree = collectFreeVars record bound
        let updatesFree = updates |> List.map (fun (_, e) -> collectFreeVars e bound) |> List.fold Set.union Set.empty
        Set.union recordFree updatesFree
    | RecordAccess (record, _) ->
        collectFreeVars record bound
    | Constructor (_, _, fields) ->
        fields |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | Match (scrutinee, cases) ->
        let scrutineeFree = collectFreeVars scrutinee bound
        let casesFree = cases |> List.map (fun matchCase ->
            // Collect bindings from all patterns (all patterns in a group bind same vars)
            let patternBindings =
                matchCase.Patterns
                |> NonEmptyList.toList
                |> List.map collectPatternBindings
                |> List.fold Set.union Set.empty
            let bodyBound = Set.union bound patternBindings
            // Include guard free vars if present
            let guardFree = matchCase.Guard |> Option.map (fun g -> collectFreeVars g bodyBound) |> Option.defaultValue Set.empty
            let bodyFree = collectFreeVars matchCase.Body bodyBound
            Set.union guardFree bodyFree)
        Set.union scrutineeFree (casesFree |> List.fold Set.union Set.empty)
    | ListLiteral elements ->
        elements |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | Lambda (parameters, returnAnnotation, body) ->
        let paramNames =
            parameters
            |> NonEmptyList.toList
            |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
            |> Set.ofList
        collectFreeVars body (Set.union bound paramNames)
    | Apply (func, args)
    | IndirectApply (func, args) ->
        let funcFree = collectFreeVars func bound
        let argsFree =
            args
            |> NonEmptyList.toList
            |> List.map (fun e -> collectFreeVars e bound)
            |> List.fold Set.union Set.empty
        Set.union funcFree argsFree
    | FuncRef _ ->
        // Function references don't contribute free variables
        Set.empty
    | Closure (_, captures) ->
        // Closures capture expressions which may have free variables
        captures |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | InterpolatedString parts ->
        parts |> List.choose (fun part ->
            match part with
            | StringText _ -> None
            | StringExpr e -> Some (collectFreeVars e bound))
        |> List.fold Set.union Set.empty

/// Collect variable names bound by a pattern
and collectPatternBindings (pattern: Pattern) : Set<string> =
    match pattern with
    | PUnit -> Set.empty
    | PWildcard -> Set.empty
    | PVar name -> Set.singleton name
    | PInt64 _
    | PBigInt _
    | PInt128Literal _
    | PInt8Literal _
    | PInt16Literal _
    | PInt32Literal _
    | PUInt8Literal _
    | PUInt16Literal _
    | PUInt32Literal _
    | PUInt64Literal _
    | PUInt128Literal _
    | PBool _
    | PString _
    | PChar _
    | PFloat _ -> Set.empty
    | PConstructor (_, fields) ->
        fields |> List.map collectPatternBindings |> List.fold Set.union Set.empty
    | PResolvedConstructor (_, _, _, fields) ->
        fields |> List.map collectPatternBindings |> List.fold Set.union Set.empty
    | PTuple patterns ->
        patterns |> List.map collectPatternBindings |> List.fold Set.union Set.empty
    | PList patterns ->
        patterns |> List.map collectPatternBindings |> List.fold Set.union Set.empty
    | PListCons (headPatterns, tailPattern) ->
        let headBindings = headPatterns |> List.map collectPatternBindings |> List.fold Set.union Set.empty
        let tailBindings = collectPatternBindings tailPattern
        Set.union headBindings tailBindings
    | POr alternatives ->
        alternatives |> NonEmptyList.head |> collectPatternBindings
