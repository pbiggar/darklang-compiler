// 1.5_TypeChecking.fs - Type Checking Pass (Phase 0)
//
// Simple top-down type checker for the Dark compiler.
//
// Design:
// - Function parameters and return types REQUIRE explicit type signatures (Phase 4+)
// - Let bindings have optional type annotations (Phase 1+)
// - Type checking proceeds top-down (expression context known from surrounding code)
// - No type inference - when type cannot be determined from context, require annotation
//
// Current Phase 0 implementation:
// - Only integers supported (TInt64)
// - All operations must be on integers
// - Returns Result<Type, TypeError> for functional error handling
//
// Example:
//   Input:  2 + 3 * 4
//   Output: Ok TInt64

module TypeChecking

open AST

/// Type errors
type TypeError =
    | TypeMismatch of expected:Type * actual:Type * context:string
    | UndefinedVariable of name:string
    | MissingTypeAnnotation of context:string
    | InvalidOperation of op:string * types:Type list
    | GenericError of string

/// Pretty-print a type for error messages
let rec typeToString (t: Type) : string =
    match t with
    | TInt8 -> "Int8"
    | TInt16 -> "Int16"
    | TInt32 -> "Int32"
    | TInt64 -> "Int64"
    | TUInt8 -> "UInt8"
    | TUInt16 -> "UInt16"
    | TUInt32 -> "UInt32"
    | TUInt64 -> "UInt64"
    | TBool -> "Bool"
    | TFloat64 -> "Float"
    | TString -> "String"
    | TUnit -> "Unit"
    | TFunction (params', ret) ->
        let paramStr = params' |> List.map typeToString |> String.concat ", "
        $"({paramStr}) -> {typeToString ret}"
    | TTuple elemTypes ->
        let elemsStr = elemTypes |> List.map typeToString |> String.concat ", "
        $"({elemsStr})"
    | TRecord name -> name
    | TSum (name, []) -> name
    | TSum (name, typeArgs) ->
        let argsStr = typeArgs |> List.map typeToString |> String.concat ", "
        $"{name}<{argsStr}>"
    | TList elemType -> $"List<{typeToString elemType}>"
    | TVar name -> name  // Type variable (for generics)
    | TRawPtr -> "RawPtr"  // Internal raw pointer type
    | TDict (keyType, valueType) -> $"Dict<{typeToString keyType}, {typeToString valueType}>"

/// Pretty-print a type error
let typeErrorToString (err: TypeError) : string =
    match err with
    | TypeMismatch (expected, actual, context) ->
        $"Type mismatch in {context}: expected {typeToString expected}, got {typeToString actual}"
    | UndefinedVariable name ->
        $"Undefined variable: {name}"
    | MissingTypeAnnotation context ->
        $"Missing type annotation: {context}"
    | InvalidOperation (op, types) ->
        let typesStr = types |> List.map typeToString |> String.concat ", "
        $"Invalid operation '{op}' on types: {typesStr}"
    | GenericError msg ->
        msg

/// Type environment - maps variable names to their types
type TypeEnv = Map<string, Type>

/// Type registry - maps record type names to their field definitions
type TypeRegistry = Map<string, (string * Type) list>

/// Sum type registry - maps sum type names to their variant lists (name, tag, payload)
type SumTypeRegistry = Map<string, (string * int * Type option) list>

/// Variant lookup - maps variant names to (type name, type params, tag index, payload type)
/// Type params are the generic type parameters of the containing sum type
type VariantLookup = Map<string, (string * string list * int * Type option)>

/// Generic function registry - maps function names to their type parameters
/// Only contains entries for functions that have type parameters
type GenericFuncRegistry = Map<string, string list>

/// Alias registry - maps type alias names to (type params, target type)
/// Example: type Id = String -> ("Id", ([], TString))
/// Example: type Outer<a> = Inner<a, Int64> -> ("Outer", (["a"], TSum("Inner", [TVar "a"; TInt64])))
type AliasRegistry = Map<string, (string list * Type)>

/// Type substitution - maps type variable names to concrete types
type Substitution = Map<string, Type>

/// Collected type checking environment - can be passed to compile user code with stdlib
type TypeCheckEnv = {
    TypeReg: TypeRegistry
    VariantLookup: VariantLookup
    FuncEnv: TypeEnv
    GenericFuncReg: GenericFuncRegistry
    ModuleRegistry: ModuleRegistry
    AliasReg: AliasRegistry
}

/// Merge two TypeCheckEnv, with overlay taking precedence on conflicts
/// Used for separate compilation: merge stdlib env with user env
let mergeTypeCheckEnv (baseEnv: TypeCheckEnv) (overlay: TypeCheckEnv) : TypeCheckEnv =
    let mergeMap m1 m2 = Map.fold (fun acc k v -> Map.add k v acc) m1 m2
    {
        TypeReg = mergeMap baseEnv.TypeReg overlay.TypeReg
        VariantLookup = mergeMap baseEnv.VariantLookup overlay.VariantLookup
        FuncEnv = mergeMap baseEnv.FuncEnv overlay.FuncEnv
        GenericFuncReg = mergeMap baseEnv.GenericFuncReg overlay.GenericFuncReg
        ModuleRegistry = baseEnv.ModuleRegistry  // Module registry is constant, use base
        AliasReg = mergeMap baseEnv.AliasReg overlay.AliasReg
    }

/// Resolve a type name through the alias registry
/// If the name is an alias, recursively resolve to the underlying type name
let rec resolveTypeName (aliasReg: AliasRegistry) (typeName: string) : string =
    match Map.tryFind typeName aliasReg with
    | Some ([], TRecord targetName) -> resolveTypeName aliasReg targetName
    | _ -> typeName

/// Apply a substitution to a type, replacing type variables with concrete types
let rec applySubst (subst: Substitution) (typ: Type) : Type =
    match typ with
    | TVar name ->
        match Map.tryFind name subst with
        | Some concreteType -> concreteType
        | None -> typ  // Unbound type variable remains as-is
    | TFunction (paramTypes, returnType) ->
        TFunction (List.map (applySubst subst) paramTypes, applySubst subst returnType)
    | TTuple elemTypes ->
        TTuple (List.map (applySubst subst) elemTypes)
    | TList elemType ->
        TList (applySubst subst elemType)
    | TSum (name, typeArgs) ->
        TSum (name, List.map (applySubst subst) typeArgs)
    | TDict (keyType, valueType) ->
        TDict (applySubst subst keyType, applySubst subst valueType)
    | TInt8 | TInt16 | TInt32 | TInt64 | TUInt8 | TUInt16 | TUInt32 | TUInt64
    | TBool | TFloat64 | TString | TUnit | TRecord _ | TRawPtr ->
        typ  // Concrete types are unchanged

/// Build a substitution from type parameters and type arguments
let buildSubstitution (typeParams: string list) (typeArgs: Type list) : Result<Substitution, string> =
    if List.length typeParams <> List.length typeArgs then
        Error $"Expected {List.length typeParams} type arguments, got {List.length typeArgs}"
    else
        Ok (List.zip typeParams typeArgs |> Map.ofList)

/// Resolve a type by expanding any type aliases (recursively)
/// Returns the fully resolved type with all aliases replaced by their targets
let rec resolveType (aliasReg: AliasRegistry) (typ: Type) : Type =
    match typ with
    | TRecord name ->
        // Check if this record name is actually a type alias
        match Map.tryFind name aliasReg with
        | Some ([], targetType) ->
            // Non-generic alias, resolve the target type too
            resolveType aliasReg targetType
        | Some (typeParams, _) ->
            // This alias expects type arguments but none provided
            // Return as-is (error will be caught elsewhere)
            typ
        | None ->
            // Not an alias, it's a real record type
            typ
    | TSum (name, typeArgs) ->
        // Check if this sum type name is actually a type alias
        match Map.tryFind name aliasReg with
        | Some (typeParams, targetType) ->
            // Type alias with (possibly) type arguments
            if List.length typeParams <> List.length typeArgs then
                // Mismatched type args, return as-is (error caught elsewhere)
                typ
            else
                // Build substitution and apply to target type
                let subst = List.zip typeParams typeArgs |> Map.ofList
                let substituted = applySubst subst targetType
                // Recursively resolve in case target is also an alias
                resolveType aliasReg substituted
        | None ->
            // Not an alias, resolve type arguments recursively
            TSum (name, List.map (resolveType aliasReg) typeArgs)
    | TFunction (paramTypes, returnType) ->
        TFunction (List.map (resolveType aliasReg) paramTypes, resolveType aliasReg returnType)
    | TTuple elemTypes ->
        TTuple (List.map (resolveType aliasReg) elemTypes)
    | TList elemType ->
        TList (resolveType aliasReg elemType)
    | TDict (keyType, valueType) ->
        TDict (resolveType aliasReg keyType, resolveType aliasReg valueType)
    | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TUInt8 | TUInt16 | TUInt32 | TUInt64
    | TBool | TFloat64 | TString | TUnit | TRawPtr ->
        typ  // Primitive types and type variables are unchanged

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
    | UnitLiteral | IntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _
    | BoolLiteral _ | StringLiteral _ | FloatLiteral _ ->
        Set.empty
    | Var name ->
        if Set.contains name bound then Set.empty else Set.singleton name
    | BinOp (_, left, right) ->
        Set.union (collectFreeVars left bound) (collectFreeVars right bound)
    | UnaryOp (_, inner) ->
        collectFreeVars inner bound
    | Let (name, value, body) ->
        let valueFree = collectFreeVars value bound
        let bodyFree = collectFreeVars body (Set.add name bound)
        Set.union valueFree bodyFree
    | If (cond, thenBranch, elseBranch) ->
        let condFree = collectFreeVars cond bound
        let thenFree = collectFreeVars thenBranch bound
        let elseFree = collectFreeVars elseBranch bound
        Set.union condFree (Set.union thenFree elseFree)
    | Call (_, args) ->
        args |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | TypeApp (_, _, args) ->
        args |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | TupleLiteral elements ->
        elements |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | TupleAccess (tuple, _) ->
        collectFreeVars tuple bound
    | RecordLiteral (_, fields) ->
        fields |> List.map (fun (_, e) -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | RecordUpdate (record, updates) ->
        let recordFree = collectFreeVars record bound
        let updatesFree = updates |> List.map (fun (_, e) -> collectFreeVars e bound) |> List.fold Set.union Set.empty
        Set.union recordFree updatesFree
    | RecordAccess (record, _) ->
        collectFreeVars record bound
    | Constructor (_, _, payload) ->
        payload |> Option.map (fun e -> collectFreeVars e bound) |> Option.defaultValue Set.empty
    | Match (scrutinee, cases) ->
        let scrutineeFree = collectFreeVars scrutinee bound
        let casesFree = cases |> List.map (fun matchCase ->
            // Collect bindings from all patterns (all patterns in a group bind same vars)
            let patternBindings =
                matchCase.Patterns
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
    | ListCons (headElements, tail) ->
        let headsFree = headElements |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
        let tailFree = collectFreeVars tail bound
        Set.union headsFree tailFree
    | Lambda (parameters, body) ->
        let paramNames = parameters |> List.map fst |> Set.ofList
        collectFreeVars body (Set.union bound paramNames)
    | Apply (func, args) ->
        let funcFree = collectFreeVars func bound
        let argsFree = args |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
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
    | PLiteral _ | PBool _ | PString _ | PFloat _ -> Set.empty
    | PConstructor (_, None) -> Set.empty
    | PConstructor (_, Some payload) -> collectPatternBindings payload
    | PTuple patterns ->
        patterns |> List.map collectPatternBindings |> List.fold Set.union Set.empty
    | PRecord (_, fields) ->
        fields |> List.map (fun (_, p) -> collectPatternBindings p) |> List.fold Set.union Set.empty
    | PList patterns ->
        patterns |> List.map collectPatternBindings |> List.fold Set.union Set.empty
    | PListCons (headPatterns, tailPattern) ->
        let headBindings = headPatterns |> List.map collectPatternBindings |> List.fold Set.union Set.empty
        let tailBindings = collectPatternBindings tailPattern
        Set.union headBindings tailBindings

// =============================================================================
// Type Inference for Generic Function Calls
// =============================================================================
// When a generic function is called without explicit type arguments, we infer
// the type arguments from the actual argument types. For example:
//   def identity<T>(x: T) : T = x
//   identity(42)  // Infers T=int from argument type

/// Match a pattern type against an actual type, extracting type variable bindings.
/// Returns a list of (typeVarName, concreteType) pairs.
/// Example: matchTypes (TVar "T") TInt64 = Ok [("T", TInt64)]
/// Helper for matching concrete types - also handles when actual is a TVar
let matchConcrete (expectedType: Type) (actual: Type) : Result<(string * Type) list, string> =
    match actual with
    | t when t = expectedType -> Ok []
    | TVar name -> Ok [(name, expectedType)]  // Bind TVar to concrete type
    | _ -> Error $"Expected {typeToString expectedType}, got {typeToString actual}"

let rec matchTypes (pattern: Type) (actual: Type) : Result<(string * Type) list, string> =
    match pattern with
    | TVar name ->
        // Type variable matches anything - record the binding
        Ok [(name, actual)]
    | TInt8 -> matchConcrete TInt8 actual
    | TInt16 -> matchConcrete TInt16 actual
    | TInt32 -> matchConcrete TInt32 actual
    | TInt64 -> matchConcrete TInt64 actual
    | TUInt8 -> matchConcrete TUInt8 actual
    | TUInt16 -> matchConcrete TUInt16 actual
    | TUInt32 -> matchConcrete TUInt32 actual
    | TUInt64 -> matchConcrete TUInt64 actual
    | TBool -> matchConcrete TBool actual
    | TFloat64 -> matchConcrete TFloat64 actual
    | TString -> matchConcrete TString actual
    | TUnit -> matchConcrete TUnit actual
    | TRawPtr -> matchConcrete TRawPtr actual
    | TList patternElem ->
        match actual with
        | TList actualElem -> matchTypes patternElem actualElem
        | TVar name -> Ok [(name, pattern)]  // Bind TVar to List type
        | _ -> Error $"Expected List<...>, got {typeToString actual}"
    | TRecord name ->
        match actual with
        | TRecord n when n = name -> Ok []
        | TVar varName -> Ok [(varName, pattern)]  // Bind TVar to Record type
        | _ -> Error $"Expected {name}, got {typeToString actual}"
    | TSum (name, patternArgs) ->
        match actual with
        | TSum (actualName, actualArgs) when name = actualName ->
            if List.length patternArgs <> List.length actualArgs then
                Error $"Sum type arity mismatch for {name}"
            else
                List.zip patternArgs actualArgs
                |> List.map (fun (p, a) -> matchTypes p a)
                |> List.fold (fun acc res ->
                    match acc, res with
                    | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e) (Ok [])
        | TVar varName -> Ok [(varName, pattern)]  // Bind TVar to Sum type
        | _ -> Error $"Expected {name}, got {typeToString actual}"
    | TFunction (patternParams, patternRet) ->
        match actual with
        | TFunction (actualParams, actualRet) ->
            if List.length patternParams <> List.length actualParams then
                Error $"Function arity mismatch: expected {List.length patternParams} params, got {List.length actualParams}"
            else
                // Match each parameter type and return type
                let paramResults =
                    List.zip patternParams actualParams
                    |> List.map (fun (p, a) -> matchTypes p a)
                let retResult = matchTypes patternRet actualRet
                // Combine all results
                List.fold (fun acc res ->
                    match acc, res with
                    | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e) retResult paramResults
        | TVar varName -> Ok [(varName, pattern)]  // Bind TVar to Function type
        | _ -> Error $"Expected function, got {typeToString actual}"
    | TTuple patternElems ->
        match actual with
        | TTuple actualElems ->
            if List.length patternElems <> List.length actualElems then
                Error $"Tuple size mismatch: expected {List.length patternElems}, got {List.length actualElems}"
            else
                List.zip patternElems actualElems
                |> List.map (fun (p, a) -> matchTypes p a)
                |> List.fold (fun acc res ->
                    match acc, res with
                    | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e) (Ok [])
        | TVar varName -> Ok [(varName, pattern)]  // Bind TVar to Tuple type
        | _ -> Error $"Expected tuple, got {typeToString actual}"
    | TDict (patternKey, patternValue) ->
        match actual with
        | TDict (actualKey, actualValue) ->
            // Match both key and value types
            match matchTypes patternKey actualKey, matchTypes patternValue actualValue with
            | Ok keyBindings, Ok valueBindings -> Ok (keyBindings @ valueBindings)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        | TVar varName -> Ok [(varName, pattern)]  // Bind TVar to Dict type
        | _ -> Error $"Expected Dict<...>, got {typeToString actual}"

/// Consolidate bindings, checking for conflicts where the same type variable
/// is bound to different types. Returns a map from type var name to concrete type.
let consolidateBindings (bindings: (string * Type) list) : Result<Map<string, Type>, string> =
    bindings
    |> List.fold (fun acc (name, typ) ->
        acc |> Result.bind (fun m ->
            match Map.tryFind name m with
            | None -> Ok (Map.add name typ m)
            | Some existingType ->
                if existingType = typ then Ok m
                else Error $"Type variable {name} has conflicting inferences: {typeToString existingType} vs {typeToString typ}"))
        (Ok Map.empty)

/// Unify a type pattern (may contain TVar) with a concrete type.
/// Returns a substitution mapping type variables to concrete types.
/// Example: unifyTypes (TVar "t") TInt64 = Ok (Map.ofList [("t", TInt64)])
let unifyTypes (pattern: Type) (actual: Type) : Result<Substitution, string> =
    matchTypes pattern actual
    |> Result.bind consolidateBindings

/// Check if a type contains type variables
let rec containsTVar (typ: Type) : bool =
    match typ with
    | TVar _ -> true
    | TList elemType -> containsTVar elemType
    | TTuple elemTypes -> List.exists containsTVar elemTypes
    | TSum (_, typeArgs) -> List.exists containsTVar typeArgs
    | TFunction (paramTypes, retType) ->
        List.exists containsTVar paramTypes || containsTVar retType
    | _ -> false

/// Reconcile two types where one might contain type variables.
/// If one type is concrete and the other has type variables that can unify with it,
/// returns the concrete type. If both are concrete and equal, returns the type.
/// If both are concrete and different, returns None.
let reconcileTypes (t1: Type) (t2: Type) : Type option =
    if t1 = t2 then
        Some t1
    elif containsTVar t1 && not (containsTVar t2) then
        // t2 is concrete, check if t1 can unify with it
        match unifyTypes t1 t2 with
        | Ok _ -> Some t2  // Return the concrete type
        | Error _ -> None
    elif not (containsTVar t1) && containsTVar t2 then
        // t1 is concrete, check if t2 can unify with it
        match unifyTypes t2 t1 with
        | Ok _ -> Some t1  // Return the concrete type
        | Error _ -> None
    elif containsTVar t1 && containsTVar t2 then
        // Both have type variables - try to unify
        match unifyTypes t1 t2 with
        | Ok subst -> Some (applySubst subst t1)
        | Error _ -> None
    else
        None

/// Infer type arguments for a generic function call.
/// Given type parameters, parameter types (with type variables), and actual argument types,
/// returns the inferred type arguments in order matching typeParams.
let inferTypeArgs (typeParams: string list) (paramTypes: Type list) (argTypes: Type list) : Result<Type list, string> =
    if List.length paramTypes <> List.length argTypes then
        Error $"Argument count mismatch: expected {List.length paramTypes}, got {List.length argTypes}"
    else
        // Match each parameter type against argument type
        let matchResults =
            List.zip paramTypes argTypes
            |> List.map (fun (paramT, argT) -> matchTypes paramT argT)

        // Combine all bindings
        matchResults
        |> List.fold (fun acc res ->
            match acc, res with
            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
            | Error e, _ -> Error e
            | _, Error e -> Error e) (Ok [])
        |> Result.bind consolidateBindings
        |> Result.bind (fun bindingMap ->
            // Extract type arguments in order of type parameters
            typeParams
            |> List.fold (fun acc paramName ->
                acc |> Result.bind (fun args ->
                    match Map.tryFind paramName bindingMap with
                    | Some typ -> Ok (args @ [typ])
                    | None -> Error $"Cannot infer type for {paramName}: no arguments constrain it"))
                (Ok []))

/// Check expression type top-down, potentially transforming the expression.
/// Parameters:
///   - expr: Expression to type-check
///   - env: Type environment (variable name -> type mappings)
///   - typeReg: Type registry (record type name -> field definitions)
///   - variantLookup: Maps variant names to (type name, tag index)
///   - genericFuncReg: Registry of generic functions (function name -> type params)
///   - expectedType: Optional expected type from context (for checking)
/// Returns: Result<Type * Expr, TypeError>
///   - Type: The type of the expression
///   - Expr: The (possibly transformed) expression
let rec checkExpr (expr: Expr) (env: TypeEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (genericFuncReg: GenericFuncRegistry) (moduleRegistry: ModuleRegistry) (aliasReg: AliasRegistry) (expectedType: Type option) : Result<Type * Expr, TypeError> =
    match expr with
    | UnitLiteral ->
        // Unit literal is always TUnit
        match expectedType with
        | Some TUnit | None -> Ok (TUnit, expr)
        | Some other -> Error (TypeMismatch (other, TUnit, "unit literal"))

    | IntLiteral _ ->
        // Integer literals are always TInt64
        match expectedType with
        | Some TInt64 | None -> Ok (TInt64, expr)
        | Some other ->
            // Handle type variables (e.g., when expected is TVar "t")
            match reconcileTypes other TInt64 with
            | Some TInt64 -> Ok (TInt64, expr)
            | _ -> Error (TypeMismatch (other, TInt64, "integer literal"))

    | Int8Literal _ ->
        match expectedType with
        | Some TInt8 | None -> Ok (TInt8, expr)
        | Some other -> Error (TypeMismatch (other, TInt8, "Int8 literal"))

    | Int16Literal _ ->
        match expectedType with
        | Some TInt16 | None -> Ok (TInt16, expr)
        | Some other -> Error (TypeMismatch (other, TInt16, "Int16 literal"))

    | Int32Literal _ ->
        match expectedType with
        | Some TInt32 | None -> Ok (TInt32, expr)
        | Some other -> Error (TypeMismatch (other, TInt32, "Int32 literal"))

    | UInt8Literal _ ->
        match expectedType with
        | Some TUInt8 | None -> Ok (TUInt8, expr)
        | Some other -> Error (TypeMismatch (other, TUInt8, "UInt8 literal"))

    | UInt16Literal _ ->
        match expectedType with
        | Some TUInt16 | None -> Ok (TUInt16, expr)
        | Some other -> Error (TypeMismatch (other, TUInt16, "UInt16 literal"))

    | UInt32Literal _ ->
        match expectedType with
        | Some TUInt32 | None -> Ok (TUInt32, expr)
        | Some other -> Error (TypeMismatch (other, TUInt32, "UInt32 literal"))

    | UInt64Literal _ ->
        match expectedType with
        | Some TUInt64 | None -> Ok (TUInt64, expr)
        | Some other -> Error (TypeMismatch (other, TUInt64, "UInt64 literal"))

    | BoolLiteral _ ->
        // Boolean literals are always TBool
        match expectedType with
        | Some TBool | None -> Ok (TBool, expr)
        | Some other -> Error (TypeMismatch (other, TBool, "boolean literal"))

    | StringLiteral _ ->
        // String literals are always TString
        match expectedType with
        | Some TString | None -> Ok (TString, expr)
        | Some other -> Error (TypeMismatch (other, TString, "string literal"))

    | InterpolatedString parts ->
        // Interpolated strings are always TString
        // Check that all expression parts are strings
        let rec checkParts (parts: StringPart list) (checkedParts: StringPart list) : Result<StringPart list, TypeError> =
            match parts with
            | [] -> Ok (List.rev checkedParts)
            | StringText s :: rest ->
                checkParts rest (StringText s :: checkedParts)
            | StringExpr e :: rest ->
                checkExpr e env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some TString)
                |> Result.bind (fun (partType, checkedExpr) ->
                    if partType = TString then
                        checkParts rest (StringExpr checkedExpr :: checkedParts)
                    else
                        Error (TypeMismatch (TString, partType, "interpolated expression")))
        match checkParts parts [] with
        | Ok checkedParts ->
            match expectedType with
            | Some TString | None -> Ok (TString, InterpolatedString checkedParts)
            | Some other -> Error (TypeMismatch (other, TString, "interpolated string"))
        | Error err -> Error err

    | FloatLiteral _ ->
        // Float literals are always TFloat64
        match expectedType with
        | Some TFloat64 | None -> Ok (TFloat64, expr)
        | Some other -> Error (TypeMismatch (other, TFloat64, "float literal"))

    | BinOp (op, left, right) ->
        match op with
        // Arithmetic operators: T -> T -> T (where T is int or float)
        | Add | Sub | Mul | Div | Mod ->
            let opName =
                match op with
                | Add -> "+"
                | Sub -> "-"
                | Mul -> "*"
                | Div -> "/"
                | Mod -> "%"
                | _ -> "?"

            // Check left operand to determine numeric type
            checkExpr left env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
            |> Result.bind (fun (leftType, left') ->
                match leftType with
                | TInt64 | TFloat64 ->
                    // Right operand must be same type
                    checkExpr right env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some leftType)
                    |> Result.bind (fun (rightType, right') ->
                        if rightType <> leftType then
                            Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some expected when expected <> leftType ->
                                Error (TypeMismatch (expected, leftType, $"result of {opName}"))
                            | _ -> Ok (leftType, BinOp (op, left', right')))
                | other ->
                    Error (InvalidOperation (opName, [other])))

        // Comparison operators: T -> T -> bool
        // Eq and Neq: work on any type (structural equality for complex types)
        // Lt, Gt, Lte, Gte: only work on numeric types
        | Eq | Neq | Lt | Gt | Lte | Gte ->
            let opName =
                match op with
                | Eq -> "=="
                | Neq -> "!="
                | Lt -> "<"
                | Gt -> ">"
                | Lte -> "<="
                | Gte -> ">="
                | _ -> "?"

            // Check left operand to determine type
            checkExpr left env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
            |> Result.bind (fun (leftType, left') ->
                match op with
                | Eq | Neq ->
                    // Equality works on any type - both operands must be same type
                    checkExpr right env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some leftType)
                    |> Result.bind (fun (rightType, right') ->
                        // Check types are compatible (same structure)
                        if rightType <> leftType then
                            Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some TBool | None -> Ok (TBool, BinOp (op, left', right'))
                            | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}")))
                | Lt | Gt | Lte | Gte ->
                    // Ordering only works on numeric types
                    match leftType with
                    | TInt64 | TFloat64 ->
                        checkExpr right env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some leftType)
                        |> Result.bind (fun (rightType, right') ->
                            if rightType <> leftType then
                                Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                            else
                                match expectedType with
                                | Some TBool | None -> Ok (TBool, BinOp (op, left', right'))
                                | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}")))
                    | other ->
                        Error (InvalidOperation (opName, [other]))
                | _ ->
                    Error (GenericError $"Unexpected comparison operator: {opName}"))

        // Boolean operators: bool -> bool -> bool
        | And | Or ->
            let opName = if op = And then "&&" else "||"

            checkExpr left env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some TBool)
            |> Result.bind (fun (leftType, left') ->
                if leftType <> TBool then
                    Error (TypeMismatch (TBool, leftType, $"left operand of {opName}"))
                else
                    checkExpr right env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some TBool)
                    |> Result.bind (fun (rightType, right') ->
                        if rightType <> TBool then
                            Error (TypeMismatch (TBool, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some TBool | None -> Ok (TBool, BinOp (op, left', right'))
                            | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}"))))

        // Bitwise operators: Int64 -> Int64 -> Int64
        | Shl | Shr | BitAnd | BitOr | BitXor ->
            let opName =
                match op with
                | Shl -> "<<"
                | Shr -> ">>"
                | BitAnd -> "&"
                | BitOr -> "|"
                | BitXor -> "^"
                | _ -> "?"

            checkExpr left env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some TInt64)
            |> Result.bind (fun (leftType, left') ->
                if leftType <> TInt64 then
                    Error (InvalidOperation (opName, [leftType]))
                else
                    checkExpr right env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some TInt64)
                    |> Result.bind (fun (rightType, right') ->
                        if rightType <> TInt64 then
                            Error (TypeMismatch (TInt64, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some TInt64 | None -> Ok (TInt64, BinOp (op, left', right'))
                            | Some other -> Error (TypeMismatch (other, TInt64, $"result of {opName}"))))

        // String concatenation: string -> string -> string
        | StringConcat ->
            checkExpr left env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some TString)
            |> Result.bind (fun (leftType, left') ->
                if leftType <> TString then
                    Error (InvalidOperation ("++", [leftType]))
                else
                    checkExpr right env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some TString)
                    |> Result.bind (fun (rightType, right') ->
                        if rightType <> TString then
                            Error (TypeMismatch (TString, rightType, "right operand of ++"))
                        else
                            match expectedType with
                            | Some TString | None -> Ok (TString, BinOp (op, left', right'))
                            | Some other -> Error (TypeMismatch (other, TString, "result of ++"))))

    | UnaryOp (op, inner) ->
        match op with
        | Neg ->
            // Negation works on integers and floats
            checkExpr inner env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
            |> Result.bind (fun (innerType, inner') ->
                match innerType with
                | TInt64 | TFloat64 ->
                    match expectedType with
                    | Some expected when expected <> innerType ->
                        Error (TypeMismatch (expected, innerType, "result of negation"))
                    | _ -> Ok (innerType, UnaryOp (op, inner'))
                | other ->
                    Error (InvalidOperation ("-", [other])))

        | Not ->
            // Boolean not works on booleans and returns booleans
            checkExpr inner env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some TBool)
            |> Result.bind (fun (innerType, inner') ->
                if innerType <> TBool then
                    Error (TypeMismatch (TBool, innerType, "operand of !"))
                else
                    match expectedType with
                    | Some TBool | None -> Ok (TBool, UnaryOp (op, inner'))
                    | Some other -> Error (TypeMismatch (other, TBool, "result of !")))

    | Let (name, value, body) ->
        // Let binding: check value, extend environment, check body
        checkExpr value env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
        |> Result.bind (fun (valueType, value') ->
            let env' = Map.add name valueType env
            checkExpr body env' typeReg variantLookup genericFuncReg moduleRegistry aliasReg expectedType
            |> Result.map (fun (bodyType, body') -> (bodyType, Let (name, value', body'))))

    | Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some varType ->
            match expectedType with
            | Some expected ->
                // Use reconcileTypes to handle type variables
                match reconcileTypes expected varType with
                | Some reconciledType -> Ok (reconciledType, expr)
                | None -> Error (TypeMismatch (expected, varType, $"variable {name}"))
            | None -> Ok (varType, expr)
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match Stdlib.tryGetFunction moduleRegistry name with
            | Some moduleFunc ->
                let funcType = Stdlib.getFunctionType moduleFunc
                match expectedType with
                | Some expected ->
                    match reconcileTypes expected funcType with
                    | Some reconciledType -> Ok (reconciledType, expr)
                    | None -> Error (TypeMismatch (expected, funcType, $"variable {name}"))
                | None -> Ok (funcType, expr)
            | None ->
                Error (UndefinedVariable name)

    | If (cond, thenBranch, elseBranch) ->
        // If expression: condition must be bool, branches must have same type
        checkExpr cond env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some TBool)
        |> Result.bind (fun (condType, cond') ->
            if condType <> TBool then
                Error (TypeMismatch (TBool, condType, "if condition"))
            else
                checkExpr thenBranch env typeReg variantLookup genericFuncReg moduleRegistry aliasReg expectedType
                |> Result.bind (fun (thenType, then') ->
                    // Pass expectedType (not thenType) to else branch - reconcileTypes will unify them
                    // This allows e.g. if true then Some(42) else None to work even when None has unbound TVars
                    checkExpr elseBranch env typeReg variantLookup genericFuncReg moduleRegistry aliasReg expectedType
                    |> Result.bind (fun (elseType, else') ->
                        match reconcileTypes thenType elseType with
                        | None ->
                            Error (TypeMismatch (thenType, elseType, "if branches must have same type"))
                        | Some reconciledType ->
                            match expectedType with
                            | Some expected when expected <> reconciledType ->
                                Error (TypeMismatch (expected, reconciledType, "if expression"))
                            | _ -> Ok (reconciledType, If (cond', then', else')))))

    | Call (funcName, args) ->
        // Function call: look up function signature, check arguments match
        match Map.tryFind funcName env with
        | Some (TFunction (paramTypes, returnType)) ->
            // Check if this is a generic function - if so, infer type arguments
            match Map.tryFind funcName genericFuncReg with
            | Some typeParams ->
                // Generic function called without explicit type args: infer them
                // First, type-check all arguments to get their types
                let rec checkArgs remaining accTypes accExprs =
                    match remaining with
                    | [] -> Ok (List.rev accTypes, List.rev accExprs)
                    | arg :: rest ->
                        checkExpr arg env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
                        |> Result.bind (fun (argType, arg') ->
                            checkArgs rest (argType :: accTypes) (arg' :: accExprs))

                checkArgs args [] []
                |> Result.bind (fun (argTypes, args') ->
                    // Infer type arguments from parameter types and argument types
                    inferTypeArgs typeParams paramTypes argTypes
                    |> Result.mapError GenericError
                    |> Result.bind (fun inferredTypeArgs ->
                        // Build substitution and compute concrete types
                        buildSubstitution typeParams inferredTypeArgs
                        |> Result.mapError GenericError
                        |> Result.bind (fun subst ->
                            let concreteReturnType = applySubst subst returnType
                            match expectedType with
                            | Some expected when expected <> concreteReturnType ->
                                Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                            | _ ->
                                // Transform Call to TypeApp with inferred type arguments
                                Ok (concreteReturnType, TypeApp (funcName, inferredTypeArgs, args')))))

            | None ->
                // Non-generic function: regular call
                // Check argument count
                if List.length args <> List.length paramTypes then
                    Error (GenericError $"Function {funcName} expects {List.length paramTypes} arguments, got {List.length args}")
                else
                    // Check each argument type and collect transformed args
                    let rec checkArgsWithTypes remaining paramTys accArgs =
                        match remaining, paramTys with
                        | [], [] -> Ok (List.rev accArgs)
                        | arg :: restArgs, paramT :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some paramT)
                            |> Result.bind (fun (argType, arg') ->
                                if argType = paramT then
                                    checkArgsWithTypes restArgs restParams (arg' :: accArgs)
                                else
                                    Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                        | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                    checkArgsWithTypes args paramTypes []
                    |> Result.bind (fun args' ->
                        match expectedType with
                        | Some expected when expected <> returnType ->
                            Error (TypeMismatch (expected, returnType, $"result of call to {funcName}"))
                        | _ -> Ok (returnType, Call (funcName, args')))
        | Some other ->
            Error (GenericError $"{funcName} is not a function (has type {typeToString other})")
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add, __raw_get)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match Stdlib.tryGetFunction moduleRegistry funcName with
            | Some moduleFunc ->
                let paramTypes = moduleFunc.ParamTypes
                let returnType = moduleFunc.ReturnType
                let typeParams = moduleFunc.TypeParams
                // Check argument count
                if List.length args <> List.length paramTypes then
                    Error (GenericError $"Function {funcName} expects {List.length paramTypes} arguments, got {List.length args}")
                else if not (List.isEmpty typeParams) then
                    // Generic module function: infer type arguments from actual argument types
                    // First, type-check all arguments to get their types
                    let rec checkArgs remaining accTypes accExprs =
                        match remaining with
                        | [] -> Ok (List.rev accTypes, List.rev accExprs)
                        | arg :: rest ->
                            checkExpr arg env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
                            |> Result.bind (fun (argType, arg') ->
                                checkArgs rest (argType :: accTypes) (arg' :: accExprs))

                    checkArgs args [] []
                    |> Result.bind (fun (argTypes, args') ->
                        // Infer type arguments from parameter types and argument types
                        inferTypeArgs typeParams paramTypes argTypes
                        |> Result.mapError GenericError
                        |> Result.bind (fun inferredTypeArgs ->
                            // Build substitution and compute concrete types
                            buildSubstitution typeParams inferredTypeArgs
                            |> Result.mapError GenericError
                            |> Result.bind (fun subst ->
                                let concreteReturnType = applySubst subst returnType
                                match expectedType with
                                | Some expected when expected <> concreteReturnType ->
                                    Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                                | _ ->
                                    // Transform Call to TypeApp with inferred type arguments
                                    Ok (concreteReturnType, TypeApp (funcName, inferredTypeArgs, args')))))
                else
                    // Non-generic module function: regular call
                    // Check each argument type and collect transformed args
                    let rec checkArgsWithTypes remaining paramTys accArgs =
                        match remaining, paramTys with
                        | [], [] -> Ok (List.rev accArgs)
                        | arg :: restArgs, paramT :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some paramT)
                            |> Result.bind (fun (argType, arg') ->
                                if argType = paramT then
                                    checkArgsWithTypes restArgs restParams (arg' :: accArgs)
                                else
                                    Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                        | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                    checkArgsWithTypes args paramTypes []
                    |> Result.bind (fun args' ->
                        match expectedType with
                        | Some expected when expected <> returnType ->
                            Error (TypeMismatch (expected, returnType, $"result of call to {funcName}"))
                        | _ -> Ok (returnType, Call (funcName, args')))
            | None ->
                Error (UndefinedVariable funcName)

    | TypeApp (funcName, typeArgs, args) ->
        // Generic function call with explicit type arguments: func<Type1, Type2>(args)
        // 1. Look up function signature
        match Map.tryFind funcName env with
        | Some (TFunction (paramTypes, returnType)) ->
            // 2. Look up type parameters
            match Map.tryFind funcName genericFuncReg with
            | Some typeParams ->
                // 3. Build substitution from type params to type args
                buildSubstitution typeParams typeArgs
                |> Result.mapError GenericError
                |> Result.bind (fun subst ->
                    // 4. Apply substitution to get concrete types
                    let concreteParamTypes = List.map (applySubst subst) paramTypes
                    let concreteReturnType = applySubst subst returnType

                    // 5. Check argument count
                    if List.length args <> List.length concreteParamTypes then
                        Error (GenericError $"Function {funcName} expects {List.length concreteParamTypes} arguments, got {List.length args}")
                    else
                        // 6. Type check each argument and collect transformed args
                        let rec checkArgsWithTypes remaining paramTys accArgs =
                            match remaining, paramTys with
                            | [], [] -> Ok (List.rev accArgs)
                            | arg :: restArgs, paramT :: restParams ->
                                checkExpr arg env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some paramT)
                                |> Result.bind (fun (argType, arg') ->
                                    if argType = paramT then
                                        checkArgsWithTypes restArgs restParams (arg' :: accArgs)
                                    else
                                        Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                            | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                        checkArgsWithTypes args concreteParamTypes []
                        |> Result.bind (fun args' ->
                            // 7. Return the concrete return type
                            match expectedType with
                            | Some expected when expected <> concreteReturnType ->
                                Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                            | _ -> Ok (concreteReturnType, TypeApp (funcName, typeArgs, args'))))
            | None ->
                Error (GenericError $"Function {funcName} is not generic, use regular call syntax")
        | Some other ->
            Error (GenericError $"{funcName} is not a function (has type {typeToString other})")
        | None ->
            // Check if it's a generic module function (e.g., __raw_get<v>)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match Stdlib.tryGetFunction moduleRegistry funcName with
            | Some moduleFunc when not (List.isEmpty moduleFunc.TypeParams) ->
                let typeParams = moduleFunc.TypeParams
                let paramTypes = moduleFunc.ParamTypes
                let returnType = moduleFunc.ReturnType
                // Build substitution from type params to type args
                buildSubstitution typeParams typeArgs
                |> Result.mapError GenericError
                |> Result.bind (fun subst ->
                    // Apply substitution to get concrete types
                    let concreteParamTypes = List.map (applySubst subst) paramTypes
                    let concreteReturnType = applySubst subst returnType

                    // Check argument count
                    if List.length args <> List.length concreteParamTypes then
                        Error (GenericError $"Function {funcName} expects {List.length concreteParamTypes} arguments, got {List.length args}")
                    else
                        // Type check each argument and collect transformed args
                        let rec checkArgsWithTypes remaining paramTys accArgs =
                            match remaining, paramTys with
                            | [], [] -> Ok (List.rev accArgs)
                            | arg :: restArgs, paramT :: restParams ->
                                checkExpr arg env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some paramT)
                                |> Result.bind (fun (argType, arg') ->
                                    if argType = paramT then
                                        checkArgsWithTypes restArgs restParams (arg' :: accArgs)
                                    else
                                        Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                            | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                        checkArgsWithTypes args concreteParamTypes []
                        |> Result.bind (fun args' ->
                            match expectedType with
                            | Some expected when expected <> concreteReturnType ->
                                Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                            | _ -> Ok (concreteReturnType, TypeApp (funcName, typeArgs, args'))))
            | Some _ ->
                Error (GenericError $"Function {funcName} is not generic, use regular call syntax")
            | None ->
                Error (UndefinedVariable funcName)

    | TupleLiteral elements ->
        // Type-check each element and build tuple type
        let rec checkElements elems accTypes accExprs =
            match elems with
            | [] -> Ok (List.rev accTypes, List.rev accExprs)
            | e :: rest ->
                checkExpr e env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
                |> Result.bind (fun (elemType, e') ->
                    checkElements rest (elemType :: accTypes) (e' :: accExprs))

        checkElements elements [] []
        |> Result.bind (fun (elemTypes, elements') ->
            let tupleType = TTuple elemTypes
            match expectedType with
            | Some (TTuple expectedElemTypes) when expectedElemTypes <> elemTypes ->
                Error (TypeMismatch (TTuple expectedElemTypes, tupleType, "tuple literal"))
            | Some other when other <> tupleType ->
                Error (TypeMismatch (other, tupleType, "tuple literal"))
            | _ -> Ok (tupleType, TupleLiteral elements'))

    | TupleAccess (tupleExpr, index) ->
        // Check the tuple expression
        checkExpr tupleExpr env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
        |> Result.bind (fun (tupleType, tupleExpr') ->
            match tupleType with
            | TTuple elemTypes ->
                if index < 0 || index >= List.length elemTypes then
                    Error (GenericError $"Tuple index {index} out of bounds (tuple has {List.length elemTypes} elements)")
                else
                    let elemType = List.item index elemTypes
                    match expectedType with
                    | Some expected when expected <> elemType ->
                        Error (TypeMismatch (expected, elemType, $"tuple access .{index}"))
                    | _ -> Ok (elemType, TupleAccess (tupleExpr', index))
            | other ->
                Error (GenericError $"Cannot access .{index} on non-tuple type {typeToString other}"))

    | RecordLiteral (typeName, fields) ->
        // Type name is required (parser enforces this, but check for safety)
        if typeName = "" then
            Error (GenericError "Record literal requires type name: use 'TypeName { field = value, ... }'")
        else
            // Resolve type alias if present
            let resolvedTypeName = resolveTypeName aliasReg typeName
            match Map.tryFind resolvedTypeName typeReg with
            | None ->
                Error (GenericError $"Unknown record type: {typeName}")
            | Some expectedFields ->
                // Check that all fields are present and have correct types
                let fieldMap = Map.ofList fields

                // Check for missing fields
                let missingFields =
                    expectedFields
                    |> List.filter (fun (fname, _) -> not (Map.containsKey fname fieldMap))
                    |> List.map fst

                if not (List.isEmpty missingFields) then
                    let missingStr = String.concat ", " missingFields
                    Error (GenericError $"Missing fields in record literal: {missingStr}")
                else
                    // Check for extra fields
                    let expectedFieldNames = expectedFields |> List.map fst |> Set.ofList
                    let extraFields =
                        fields
                        |> List.filter (fun (fname, _) -> not (Set.contains fname expectedFieldNames))
                        |> List.map fst

                    if not (List.isEmpty extraFields) then
                        let extraStr = String.concat ", " extraFields
                        Error (GenericError $"Unknown fields in record literal: {extraStr}")
                    else
                        // Type check each field and collect transformed fields
                        let rec checkFieldsInOrder remaining accFields =
                            match remaining with
                            | [] -> Ok (List.rev accFields)
                            | (fname, expectedFieldType) :: rest ->
                                match Map.tryFind fname fieldMap with
                                | Some fieldExpr ->
                                    checkExpr fieldExpr env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some expectedFieldType)
                                    |> Result.bind (fun (actualType, fieldExpr') ->
                                        if actualType = expectedFieldType then
                                            checkFieldsInOrder rest ((fname, fieldExpr') :: accFields)
                                        else
                                            Error (TypeMismatch (expectedFieldType, actualType, $"field {fname}")))
                                | None -> checkFieldsInOrder rest accFields // Already checked for missing fields

                        checkFieldsInOrder expectedFields []
                        |> Result.map (fun fields' -> (TRecord resolvedTypeName, RecordLiteral (resolvedTypeName, fields')))

    | RecordUpdate (recordExpr, updates) ->
        // Check the record expression to get its type
        checkExpr recordExpr env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
        |> Result.bind (fun (recordType, recordExpr') ->
            match recordType with
            | TRecord typeName ->
                match Map.tryFind typeName typeReg with
                | None ->
                    Error (GenericError $"Unknown record type: {typeName}")
                | Some expectedFields ->
                    // Check for unknown fields in update
                    let expectedFieldNames = expectedFields |> List.map fst |> Set.ofList
                    let unknownFields =
                        updates
                        |> List.filter (fun (fname, _) -> not (Set.contains fname expectedFieldNames))
                        |> List.map fst

                    if not (List.isEmpty unknownFields) then
                        let unknownStr = String.concat ", " unknownFields
                        Error (GenericError $"Unknown fields in record update: {unknownStr}")
                    else
                        // Build a map from field name to expected type
                        let fieldTypeMap = expectedFields |> Map.ofList

                        // Type check each update field
                        let rec checkUpdates remaining accUpdates =
                            match remaining with
                            | [] -> Ok (List.rev accUpdates)
                            | (fname, updateExpr) :: rest ->
                                match Map.tryFind fname fieldTypeMap with
                                | Some expectedFieldType ->
                                    checkExpr updateExpr env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some expectedFieldType)
                                    |> Result.bind (fun (actualType, updateExpr') ->
                                        if actualType = expectedFieldType then
                                            checkUpdates rest ((fname, updateExpr') :: accUpdates)
                                        else
                                            Error (TypeMismatch (expectedFieldType, actualType, $"field {fname} in record update")))
                                | None ->
                                    Error (GenericError $"Field {fname} not found in record type {typeName}")

                        checkUpdates updates []
                        |> Result.map (fun updates' -> (TRecord typeName, RecordUpdate (recordExpr', updates')))
            | other ->
                Error (GenericError $"Cannot use record update syntax on non-record type {typeToString other}"))

    | RecordAccess (recordExpr, fieldName) ->
        // Check the record expression
        checkExpr recordExpr env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
        |> Result.bind (fun (recordType, recordExpr') ->
            match recordType with
            | TRecord typeName ->
                match Map.tryFind typeName typeReg with
                | None ->
                    Error (GenericError $"Unknown record type: {typeName}")
                | Some fields ->
                    match List.tryFind (fun (name, _) -> name = fieldName) fields with
                    | None ->
                        Error (GenericError $"Record type {typeName} has no field '{fieldName}'")
                    | Some (_, fieldType) ->
                        match expectedType with
                        | Some expected when expected <> fieldType ->
                            Error (TypeMismatch (expected, fieldType, $"field access .{fieldName}"))
                        | _ -> Ok (fieldType, RecordAccess (recordExpr', fieldName))
            | other ->
                Error (GenericError $"Cannot access .{fieldName} on non-record type {typeToString other}"))

    | Constructor (constrTypeName, variantName, payload) ->
        // Look up the variant to find its type and expected payload
        match Map.tryFind variantName variantLookup with
        | None ->
            Error (GenericError $"Unknown constructor: {variantName}")
        | Some (typeName, typeParams, _tag, expectedPayload) ->
            match expectedPayload, payload with
            | None, None ->
                // Variant without payload, no payload provided - OK
                if List.isEmpty typeParams then
                    // Non-generic type - simple case
                    let sumType = TSum (typeName, [])
                    match expectedType with
                    | Some expected when expected <> sumType ->
                        Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                    | _ -> Ok (sumType, expr)
                else
                    // Generic type with nullary constructor (e.g., None in Option<t>)
                    // Try to get type arguments from expectedType
                    match expectedType with
                    | Some (TSum (expectedName, args)) when expectedName = typeName && List.length args = List.length typeParams ->
                        // Use type args from expected type
                        let sumType = TSum (typeName, args)
                        Ok (sumType, expr)
                    | Some expected ->
                        // Expected type doesn't match - error
                        let sumTypeWithVars = TSum (typeName, typeParams |> List.map TVar)
                        Error (TypeMismatch (expected, sumTypeWithVars, $"constructor {variantName}"))
                    | None ->
                        // No expected type - return type with unresolved type variables
                        // This allows type inference to resolve them later from context
                        let sumType = TSum (typeName, typeParams |> List.map TVar)
                        Ok (sumType, expr)
            | None, Some _ ->
                // Variant doesn't take payload but one was provided
                Error (GenericError $"Constructor {variantName} does not take a payload")
            | Some _, None ->
                // Variant requires payload but none provided
                Error (GenericError $"Constructor {variantName} requires a payload")
            | Some payloadType, Some payloadExpr ->
                // Variant with payload - check payload type
                // For generic types, infer type variables from the payload
                if List.isEmpty typeParams then
                    // Non-generic type - check payload has exact type
                    checkExpr payloadExpr env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some payloadType)
                    |> Result.bind (fun (actualPayloadType, payloadExpr') ->
                        if actualPayloadType <> payloadType then
                            Error (TypeMismatch (payloadType, actualPayloadType, $"payload of {variantName}"))
                        else
                            let sumType = TSum (typeName, [])
                            match expectedType with
                            | Some expected when expected <> sumType ->
                                Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                            | _ -> Ok (sumType, Constructor (constrTypeName, variantName, Some payloadExpr')))
                else
                    // Generic type - infer type variables from payload
                    // First, check the payload expression without expected type
                    checkExpr payloadExpr env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
                    |> Result.bind (fun (actualPayloadType, payloadExpr') ->
                        // Try to unify payloadType (may contain TVar) with actualPayloadType
                        match unifyTypes payloadType actualPayloadType with
                        | Error msg ->
                            Error (GenericError $"Type mismatch in {variantName} payload: {msg}")
                        | Ok subst ->
                            // Apply substitution to verify all type vars are resolved
                            let concretePayloadType = applySubst subst payloadType
                            if concretePayloadType <> actualPayloadType then
                                Error (TypeMismatch (concretePayloadType, actualPayloadType, $"payload of {variantName}"))
                            else
                                // Build concrete type arguments from substitution
                                // For unresolved type vars, try to get them from expectedType
                                let expectedArgs =
                                    match expectedType with
                                    | Some (TSum (expectedName, args)) when expectedName = typeName && List.length args = List.length typeParams ->
                                        Some args
                                    | _ -> None
                                let typeArgs = typeParams |> List.mapi (fun i p ->
                                    match Map.tryFind p subst with
                                    | Some t -> t
                                    | None ->
                                        // Try to get from expected type args
                                        match expectedArgs with
                                        | Some args -> List.item i args
                                        | None -> TVar p)
                                let sumType = TSum (typeName, typeArgs)
                                match expectedType with
                                | Some expected when expected <> sumType ->
                                    Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                                | _ -> Ok (sumType, Constructor (constrTypeName, variantName, Some payloadExpr')))

    | Match (scrutinee, cases) ->
        // Type check the scrutinee first
        checkExpr scrutinee env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
        |> Result.bind (fun (scrutineeType, scrutinee') ->
            // Extract bindings from a pattern based on scrutinee type
            let rec extractPatternBindings (pattern: Pattern) (patternType: Type) : Result<(string * Type) list, TypeError> =
                match pattern with
                | PUnit ->
                    match patternType with
                    | TUnit -> Ok []
                    | _ -> Error (GenericError "Unit pattern can only match unit type")
                | PWildcard -> Ok []
                | PLiteral _ -> Ok []
                | PBool _ -> Ok []
                | PString _ -> Ok []
                | PFloat _ -> Ok []
                | PVar name -> Ok [(name, patternType)]
                | PConstructor (variantName, payloadPattern) ->
                    match Map.tryFind variantName variantLookup with
                    | None -> Error (GenericError $"Unknown variant in pattern: {variantName}")
                    | Some (typeName, typeParams, _, payloadType) ->
                        // Get type arguments from scrutinee type to substitute into payload type
                        let typeArgs =
                            match patternType with
                            | TSum (_, args) -> args
                            | _ -> []
                        // Build substitution from type params to type args
                        let subst =
                            if List.length typeParams = List.length typeArgs then
                                List.zip typeParams typeArgs |> Map.ofList
                            else
                                Map.empty
                        match payloadPattern, payloadType with
                        | None, _ -> Ok []
                        | Some innerPattern, Some pType ->
                            // Apply substitution to get concrete payload type
                            let concretePayloadType = applySubst subst pType
                            extractPatternBindings innerPattern concretePayloadType
                        | Some _, None ->
                            Error (GenericError $"Variant {variantName} has no payload but pattern expects one")
                | PTuple patterns ->
                    match patternType with
                    | TTuple elementTypes when List.length patterns = List.length elementTypes ->
                        List.zip patterns elementTypes
                        |> List.map (fun (p, t) -> extractPatternBindings p t)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])
                    | TTuple elementTypes ->
                        Error (GenericError $"Tuple pattern has {List.length patterns} elements but type has {List.length elementTypes}")
                    | _ -> Error (GenericError "Tuple pattern used on non-tuple type")
                | PRecord (_, fieldPatterns) ->
                    match patternType with
                    | TRecord recordName ->
                        match Map.tryFind recordName typeReg with
                        | Some fields ->
                            fieldPatterns
                            |> List.map (fun (fieldName, pat) ->
                                match List.tryFind (fun (n, _) -> n = fieldName) fields with
                                | Some (_, fieldType) -> extractPatternBindings pat fieldType
                                | None -> Error (GenericError $"Unknown field in pattern: {fieldName}"))
                            |> List.fold (fun acc res ->
                                match acc, res with
                                | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                                | Error e, _ -> Error e
                                | _, Error e -> Error e) (Ok [])
                        | None -> Error (GenericError $"Unknown record type: {recordName}")
                    | _ -> Error (GenericError "Record pattern used on non-record type")
                | PList patterns ->
                    match patternType with
                    | TList elemType ->
                        // Each element pattern binds variables of the list's element type
                        patterns
                        |> List.map (fun p -> extractPatternBindings p elemType)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])
                    | _ -> Error (GenericError "List pattern used on non-list type")
                | PListCons (headPatterns, tailPattern) ->
                    match patternType with
                    | TList elemType ->
                        // Head patterns bind to element type
                        let headBindings =
                            headPatterns
                            |> List.map (fun p -> extractPatternBindings p elemType)
                            |> List.fold (fun acc res ->
                                match acc, res with
                                | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                                | Error e, _ -> Error e
                                | _, Error e -> Error e) (Ok [])
                        // Tail pattern binds to List<elemType>
                        let tailBindings = extractPatternBindings tailPattern (TList elemType)
                        match headBindings, tailBindings with
                        | Ok hb, Ok tb -> Ok (hb @ tb)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e
                    | _ -> Error (GenericError "List cons pattern used on non-list type")

            // Type check each case and ensure they all return the same type
            // Returns (resultType, transformedCases)
            let rec checkCases (remaining: MatchCase list) (resultType: Type option) (accCases: MatchCase list) : Result<Type * MatchCase list, TypeError> =
                match remaining with
                | [] ->
                    match resultType with
                    | Some t -> Ok (t, List.rev accCases)
                    | None -> Error (GenericError "Match expression must have at least one case")
                | matchCase :: rest ->
                    // Extract bindings from first pattern (all patterns in group bind same vars)
                    // For pattern grouping, we check the first pattern's bindings
                    let firstPattern = List.head matchCase.Patterns
                    extractPatternBindings firstPattern scrutineeType
                    |> Result.bind (fun bindings ->
                        let caseEnv = List.fold (fun e (name, ty) -> Map.add name ty e) env bindings
                        // Type check guard if present (must be Bool)
                        let guardResult =
                            match matchCase.Guard with
                            | None -> Ok None
                            | Some guardExpr ->
                                checkExpr guardExpr caseEnv typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some TBool)
                                |> Result.bind (fun (guardType, guard') ->
                                    if guardType = TBool then
                                        Ok (Some guard')
                                    else
                                        Error (TypeMismatch (TBool, guardType, "guard clause")))
                        guardResult
                        |> Result.bind (fun guard' ->
                            checkExpr matchCase.Body caseEnv typeReg variantLookup genericFuncReg moduleRegistry aliasReg resultType
                            |> Result.bind (fun (bodyType, body') ->
                                let newCase = { Patterns = matchCase.Patterns; Guard = guard'; Body = body' }
                                match resultType with
                                | None -> checkCases rest (Some bodyType) (newCase :: accCases)
                                | Some expected ->
                                    // Use reconcileTypes to handle type variables
                                    match reconcileTypes expected bodyType with
                                    | Some reconciledType ->
                                        // Update resultType to the reconciled (concrete) type
                                        checkCases rest (Some reconciledType) (newCase :: accCases)
                                    | None ->
                                        Error (TypeMismatch (expected, bodyType, "match case")))))

            // Pass expectedType to first case so empty lists, None, etc. get the right type
            checkCases cases expectedType []
            |> Result.bind (fun (matchType, cases') ->
                match expectedType with
                | Some expected ->
                    // Use reconcileTypes for expected type check too
                    match reconcileTypes expected matchType with
                    | Some reconciledType -> Ok (reconciledType, Match (scrutinee', cases'))
                    | None -> Error (TypeMismatch (expected, matchType, "match expression"))
                | None -> Ok (matchType, Match (scrutinee', cases'))))

    | ListLiteral elements ->
        // Type-check elements and infer element type from first element
        match elements with
        | [] ->
            // Empty list: use expected type or default to List<int> for backward compatibility
            match expectedType with
            | Some (TList elemType) -> Ok (TList elemType, ListLiteral [])
            | Some other -> Error (TypeMismatch (other, TList TInt64, "empty list"))
            | None -> Ok (TList TInt64, ListLiteral [])  // Default to List<int>
        | first :: rest ->
            // Infer element type from first element
            checkExpr first env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
            |> Result.bind (fun (elemType, first') ->
                // Check remaining elements match the inferred type
                let rec checkRest remaining acc =
                    match remaining with
                    | [] -> Ok (List.rev acc)
                    | e :: rs ->
                        checkExpr e env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some elemType)
                        |> Result.bind (fun (eType, e') ->
                            if eType = elemType then checkRest rs (e' :: acc)
                            else Error (TypeMismatch (elemType, eType, "list element")))
                checkRest rest [first']
                |> Result.bind (fun elements' ->
                    let listType = TList elemType
                    match expectedType with
                    | Some (TList expectedElem) when expectedElem <> elemType ->
                        Error (TypeMismatch (TList expectedElem, listType, "list literal"))
                    | Some other when other <> listType ->
                        Error (TypeMismatch (other, listType, "list literal"))
                    | _ -> Ok (listType, ListLiteral elements')))

    | ListCons (headElements, tail) ->
        // Type-check tail first to get element type
        checkExpr tail env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
        |> Result.bind (fun (tailType, tail') ->
            match tailType with
            | TList elemType ->
                // Type-check each head element with the inferred element type
                let rec checkHeads elems acc =
                    match elems with
                    | [] -> Ok (List.rev acc)
                    | h :: rest ->
                        checkExpr h env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some elemType)
                        |> Result.bind (fun (hType, h') ->
                            if hType <> elemType then
                                Error (TypeMismatch (elemType, hType, "list cons element"))
                            else
                                checkHeads rest (h' :: acc))
                checkHeads headElements []
                |> Result.bind (fun heads' ->
                    let listType = TList elemType
                    match expectedType with
                    | Some expected when expected <> listType ->
                        Error (TypeMismatch (expected, listType, "list cons"))
                    | _ -> Ok (listType, ListCons (heads', tail')))
            | other -> Error (TypeMismatch (TList (TVar "t"), other, "list cons tail must be a list")))

    | Lambda (parameters, body) ->
        // Build environment with lambda parameters
        let paramEnv =
            parameters
            |> List.fold (fun e (name, ty) -> Map.add name ty e) env

        // Type-check the lambda body
        checkExpr body paramEnv typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
        |> Result.bind (fun (bodyType, body') ->
            let paramTypes = parameters |> List.map snd
            let funcType = TFunction (paramTypes, bodyType)
            match expectedType with
            | Some (TFunction (expectedParams, expectedRet)) ->
                // Verify parameter types match
                if List.length expectedParams <> List.length paramTypes then
                    Error (TypeMismatch (TFunction (expectedParams, expectedRet), funcType, "lambda parameter count"))
                else
                    let paramMismatch =
                        List.zip expectedParams paramTypes
                        |> List.tryFind (fun (expected, actual) -> expected <> actual)
                    match paramMismatch with
                    | Some (expected, actual) ->
                        Error (TypeMismatch (expected, actual, "lambda parameter type"))
                    | None ->
                        // Use reconcileTypes to handle generic type unification
                        // e.g., Option<t> should unify with Option<Int64>
                        match reconcileTypes expectedRet bodyType with
                        | None ->
                            Error (TypeMismatch (expectedRet, bodyType, "lambda return type"))
                        | Some reconciledRetType ->
                            let reconciledFuncType = TFunction (paramTypes, reconciledRetType)
                            Ok (reconciledFuncType, Lambda (parameters, body'))
            | Some other ->
                Error (TypeMismatch (other, funcType, "lambda"))
            | None ->
                Ok (funcType, Lambda (parameters, body')))

    | Apply (func, args) ->
        // Type-check the function expression
        checkExpr func env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
        |> Result.bind (fun (funcType, func') ->
            match funcType with
            | TFunction (paramTypes, returnType) ->
                // Check argument count
                if List.length args <> List.length paramTypes then
                    Error (GenericError $"Function expects {List.length paramTypes} arguments, got {List.length args}")
                else
                    // Check each argument against expected param type
                    let rec checkArgs (argExprs: Expr list) (paramTys: Type list) (checkedArgs: Expr list) : Result<Expr list, TypeError> =
                        match argExprs, paramTys with
                        | [], [] -> Ok (List.rev checkedArgs)
                        | arg :: restArgs, paramTy :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some paramTy)
                            |> Result.bind (fun (argType, arg') ->
                                if argType = paramTy then
                                    checkArgs restArgs restParams (arg' :: checkedArgs)
                                else
                                    Error (TypeMismatch (paramTy, argType, "function argument")))
                        | _ -> Error (GenericError "Argument count mismatch")
                    checkArgs args paramTypes []
                    |> Result.bind (fun args' ->
                        match expectedType with
                        | Some expected when expected <> returnType ->
                            Error (TypeMismatch (expected, returnType, "function application result"))
                        | _ -> Ok (returnType, Apply (func', args')))
            | _ ->
                Error (GenericError $"Cannot apply non-function type: {typeToString funcType}"))

    | FuncRef funcName ->
        // Function reference: look up function signature
        match Map.tryFind funcName env with
        | Some funcType ->
            match expectedType with
            | Some expected when expected <> funcType ->
                Error (TypeMismatch (expected, funcType, $"function reference {funcName}"))
            | _ -> Ok (funcType, expr)
        | None ->
            Error (UndefinedVariable funcName)

    | Closure (funcName, captures) ->
        // Closure: function with captured values
        // The closure has the same type as the underlying function (minus closure param)
        // For now, just check the captures and return function type
        let checkCapture (cap: Expr) : Result<Expr, TypeError> =
            checkExpr cap env typeReg variantLookup genericFuncReg moduleRegistry aliasReg None
            |> Result.map snd
        let rec checkCaptures (caps: Expr list) (acc: Expr list) : Result<Expr list, TypeError> =
            match caps with
            | [] -> Ok (List.rev acc)
            | cap :: rest ->
                checkCapture cap |> Result.bind (fun cap' -> checkCaptures rest (cap' :: acc))
        checkCaptures captures []
        |> Result.bind (fun captures' ->
            // Look up closure function type
            match Map.tryFind funcName env with
            | Some (TFunction (_ :: restParams, returnType)) ->
                // The closure type is the function type without the closure param
                let closureType = TFunction (restParams, returnType)
                match expectedType with
                | Some expected when expected <> closureType ->
                    Error (TypeMismatch (expected, closureType, $"closure {funcName}"))
                | _ -> Ok (closureType, Closure (funcName, captures'))
            | Some funcType -> Ok (funcType, Closure (funcName, captures'))
            | None -> Error (UndefinedVariable funcName))

/// Type-check a function definition
/// Returns the transformed function body (with Call -> TypeApp transformations)
let checkFunctionDef (funcDef: FunctionDef) (env: TypeEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (genericFuncReg: GenericFuncRegistry) (moduleRegistry: ModuleRegistry) (aliasReg: AliasRegistry) : Result<FunctionDef, TypeError> =
    // Build environment with parameters
    let paramEnv =
        funcDef.Params
        |> List.fold (fun e (name, ty) -> Map.add name ty e) env

    // Check body has return type
    checkExpr funcDef.Body paramEnv typeReg variantLookup genericFuncReg moduleRegistry aliasReg (Some funcDef.ReturnType)
    |> Result.bind (fun (bodyType, body') ->
        // For generic functions, we need to compare types considering type variables
        // For now, if the body type contains type variables, just check structural equality
        if bodyType = funcDef.ReturnType then
            Ok { funcDef with Body = body' }
        else
            Error (TypeMismatch (funcDef.ReturnType, bodyType, $"function {funcDef.Name} body")))

/// Internal: Type-check a program and return the type checking environment
/// This is the core implementation used by checkProgram, checkProgramWithEnv, and checkProgramWithBaseEnv
/// When baseEnv is provided, registries are merged with it (for separate compilation)
let private checkProgramInternal (baseEnv: TypeCheckEnv option) (program: Program) : Result<Type * Program * TypeCheckEnv, TypeError> =
    let (Program topLevels) = program

    // First pass: collect all type definitions (records) from THIS program
    // Note: typeParams are stored but not fully used yet (future: generic type instantiation)
    let programTypeReg : TypeRegistry =
        topLevels
        |> List.choose (function
            | TypeDef (RecordDef (name, _typeParams, fields)) -> Some (name, fields)
            | _ -> None)
        |> Map.ofList

    // Collect type aliases
    let aliasReg : AliasRegistry =
        topLevels
        |> List.choose (function
            | TypeDef (TypeAlias (name, typeParams, targetType)) -> Some (name, (typeParams, targetType))
            | _ -> None)
        |> Map.ofList

    // Collect sum type definitions and build variant lookup from THIS program
    // Maps variant name -> (type name, type params, tag index, payload type)
    // Type params are included for generic type instantiation at constructor call sites
    let programVariantLookup : VariantLookup =
        topLevels
        |> List.choose (function
            | TypeDef (SumTypeDef (typeName, typeParams, variants)) ->
                Some (typeName, typeParams, variants)
            | _ -> None)
        |> List.collect (fun (typeName, typeParams, variants) ->
            variants
            |> List.mapi (fun idx variant -> (variant.Name, (typeName, typeParams, idx, variant.Payload))))
        |> Map.ofList

    // Second pass: collect all function signatures from THIS program
    let funcSigs =
        topLevels
        |> List.choose (function
            | FunctionDef funcDef ->
                Some (funcDef.Name, (List.map snd funcDef.Params, funcDef.ReturnType))
            | _ -> None)
        |> Map.ofList

    // Build environment with function signatures from THIS program
    let programFuncEnv =
        funcSigs
        |> Map.map (fun _ (paramTypes, returnType) -> TFunction (paramTypes, returnType))

    // Build generic function registry from THIS program - maps function names to type parameters
    let programGenericFuncReg : GenericFuncRegistry =
        topLevels
        |> List.choose (function
            | FunctionDef funcDef when not (List.isEmpty funcDef.TypeParams) ->
                Some (funcDef.Name, funcDef.TypeParams)
            | _ -> None)
        |> Map.ofList

    // Build module registry once (or reuse from base environment)
    let moduleRegistry =
        match baseEnv with
        | Some existingEnv -> existingEnv.ModuleRegistry
        | None -> Stdlib.buildModuleRegistry ()

    // Build the type check environment for THIS program
    let programEnv : TypeCheckEnv = {
        TypeReg = programTypeReg
        VariantLookup = programVariantLookup
        FuncEnv = programFuncEnv
        GenericFuncReg = programGenericFuncReg
        ModuleRegistry = moduleRegistry
        AliasReg = aliasReg
    }

    // Merge with base environment if provided (for separate compilation)
    let typeCheckEnv =
        match baseEnv with
        | Some existingEnv -> mergeTypeCheckEnv existingEnv programEnv
        | None -> programEnv

    // Extract the merged registries for use in type checking
    let typeReg = typeCheckEnv.TypeReg
    let variantLookup = typeCheckEnv.VariantLookup
    let funcEnv = typeCheckEnv.FuncEnv
    let genericFuncReg = typeCheckEnv.GenericFuncReg
    let mergedAliasReg = typeCheckEnv.AliasReg

    // Third pass: type check all function definitions and collect transformed top-levels
    let rec checkAllTopLevels remaining accTopLevels =
        match remaining with
        | [] -> Ok (List.rev accTopLevels)
        | topLevel :: rest ->
            match topLevel with
            | FunctionDef funcDef ->
                checkFunctionDef funcDef funcEnv typeReg variantLookup genericFuncReg moduleRegistry mergedAliasReg
                |> Result.bind (fun funcDef' ->
                    checkAllTopLevels rest (FunctionDef funcDef' :: accTopLevels))
            | TypeDef _ ->
                // Type definitions pass through unchanged
                checkAllTopLevels rest (topLevel :: accTopLevels)
            | Expression expr ->
                // Main expression - check and transform
                checkExpr expr funcEnv typeReg variantLookup genericFuncReg moduleRegistry mergedAliasReg None
                |> Result.bind (fun (_, expr') ->
                    checkAllTopLevels rest (Expression expr' :: accTopLevels))

    // Type check all top-levels
    checkAllTopLevels topLevels []
    |> Result.bind (fun topLevels' ->
        // Determine result type from main expression or main function
        let mainExpr = topLevels' |> List.tryPick (function Expression e -> Some e | _ -> None)
        match mainExpr with
        | Some expr ->
            // Re-check to get type (we already have transformed expr in topLevels')
            checkExpr expr funcEnv typeReg variantLookup genericFuncReg moduleRegistry mergedAliasReg None
            |> Result.map (fun (typ, _) -> (typ, Program topLevels', typeCheckEnv))
        | None ->
            // No main expression - just functions
            // For now, require a "main" function with signature () -> int
            match Map.tryFind "main" funcSigs with
            | Some ([], TInt64) -> Ok (TInt64, Program topLevels', typeCheckEnv)
            | Some _ -> Error (GenericError "main function must have signature () -> int")
            | None -> Error (GenericError "Program must have either a main expression or a main() : int function"))

/// Type-check a program
/// Returns the type of the main expression and the transformed program
/// The transformed program has Call nodes converted to TypeApp where type inference was applied
let checkProgram (program: Program) : Result<Type * Program, TypeError> =
    checkProgramInternal None program
    |> Result.map (fun (typ, prog, _env) -> (typ, prog))

/// Type-check a program and return the type checking environment
/// Use this when you need to reuse the environment (e.g., for stdlib caching)
let checkProgramWithEnv (program: Program) : Result<Type * Program * TypeCheckEnv, TypeError> =
    checkProgramInternal None program

/// Type-check a program with a pre-populated base environment (for separate compilation)
/// The program's definitions are merged with the base environment, allowing lookups
/// of types/functions from both the base (e.g., stdlib) and the program (e.g., user code)
let checkProgramWithBaseEnv (baseEnv: TypeCheckEnv) (program: Program) : Result<Type * Program * TypeCheckEnv, TypeError> =
    checkProgramInternal (Some baseEnv) program
