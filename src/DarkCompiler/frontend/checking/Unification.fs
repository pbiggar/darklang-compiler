// Unification.fs - Unify source types and infer concrete generic arguments.

module TypeUnification

open AST
open CheckingDiagnostics
open CheckingTypes
open CheckedFreeVariables

// =============================================================================
// Type Inference for Generic Function Calls
// =============================================================================
// When a generic function is called without explicit type arguments, we infer
// the type arguments from the actual argument types. For example:
//   let identity<'T>(x: T) : T = x
//   identity(42)  // Infers T=int from argument type

/// Match a pattern type against an actual type, extracting type variable bindings.
/// Returns a list of (typeVarName, concreteType) pairs.
/// Example: matchTypes (TVar "T") TInt64 = Ok [("T", TInt64)]
/// Helper for matching concrete types - also handles when actual is a TVar
let matchConcrete (expectedType: Type) (actual: Type) : Result<(string * Type) list, string> =
    if expectedType = TRuntimeError || actual = TRuntimeError then
        // Runtime-error expressions are bottom-like and can inhabit any expected type.
        Ok []
    else
        match actual with
        | t when t = expectedType -> Ok []
        | TVar name -> Ok [(name, expectedType)]  // Bind TVar to concrete type
        | _ -> Error $"Expected {typeToString expectedType}, got {typeToString actual}"

let rec matchTypes (pattern: Type) (actual: Type) : Result<(string * Type) list, string> =
    match pattern with
    | TVar name ->
        // Type variable matches anything - record the binding
        match actual with
        | TVar actualName when actualName = name -> Ok []  // Same var, no binding needed
        | _ -> Ok [(name, actual)]
    | TInt8 -> matchConcrete TInt8 actual
    | TInt16 -> matchConcrete TInt16 actual
    | TInt32 -> matchConcrete TInt32 actual
    | TInt64 -> matchConcrete TInt64 actual
    | TInt128 -> matchConcrete TInt128 actual
    | TInt -> matchConcrete TInt actual
    | TUInt8 -> matchConcrete TUInt8 actual
    | TUInt16 -> matchConcrete TUInt16 actual
    | TUInt32 -> matchConcrete TUInt32 actual
    | TUInt64 -> matchConcrete TUInt64 actual
    | TUInt128 -> matchConcrete TUInt128 actual
    | TBool -> matchConcrete TBool actual
    | TFloat64 -> matchConcrete TFloat64 actual
    | TString ->
        // Char and String share runtime representation.
        match actual with
        | TChar -> Ok []
        | _ -> matchConcrete TString actual
    | TBlob -> matchConcrete TBlob actual
    | TDateTime -> matchConcrete TDateTime actual
    | TChar ->
        match actual with
        | TString -> Ok []
        | _ -> matchConcrete TChar actual
    | TUnit -> matchConcrete TUnit actual
    | TRuntimeError -> matchConcrete TRuntimeError actual
    | TRawPtr -> matchConcrete TRawPtr actual
    | TList patternElem ->
        match actual with
        | TList actualElem -> matchTypes patternElem actualElem
        | TVar name -> Ok [(name, pattern)]  // Bind TVar to List type
        | _ -> Error $"Expected List<...>, got {typeToString actual}"
    | TStream patternElem ->
        match actual with
        | TStream actualElem -> matchTypes patternElem actualElem
        | TVar name -> Ok [(name, pattern)]
        | _ -> Error $"Expected Stream<...>, got {typeToString actual}"
    | TRecord (name, patternArgs) ->
        match actual with
        | TRecord (n, actualArgs) when n = name ->
            // Unify type arguments if both have them
            if List.length patternArgs <> List.length actualArgs then
                Error $"Record type arity mismatch for {name}"
            else
                List.zip patternArgs actualArgs
                |> List.map (fun (p, a) -> matchTypes p a)
                |> List.fold (fun acc res ->
                    match acc, res with
                    | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e) (Ok [])
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
    | TEnumFields patternFields ->
        match actual with
        | TTuple actualFields when List.length patternFields = List.length actualFields ->
            List.zip patternFields actualFields
            |> List.map (fun (patternField, actualField) -> matchTypes patternField actualField)
            |> List.fold (fun acc result ->
                match acc, result with
                | Ok bindings, Ok more -> Ok (bindings @ more)
                | Error err, _ -> Error err
                | _, Error err -> Error err) (Ok [])
        | TTuple actualFields ->
            Error $"Enum field arity mismatch: expected {List.length patternFields}, got {List.length actualFields}"
        | _ -> Error $"Expected {List.length patternFields} enum fields, got {typeToString actual}"
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

/// Check if a type contains type variables
/// The element variable the checker gives an empty list literal that nothing
/// has typed yet (Expressions.fs, CheckMatches.fs). Spelled like a freshened
/// parameter so that inference may bind it; a declared `'t` must stay rigid.
let emptyListElementVar = "t$empty"

/// A variable inference may bind: a callee's freshened parameter, the open
/// element of an empty list literal, or one of the checker's own placeholders
/// (an untyped lambda binding, a call result through a variable, a pattern's
/// element). A declared type parameter is not one.
let isInferenceVar (name: string) : bool =
    name.Contains "$"
    || name.StartsWith "binding_"
    || name.StartsWith "__"
    || name.StartsWith "recursiveParameter"

let rec containsTVar (typ: Type) : bool =
    match typ with
    | TVar _ -> true
    | TList elemType -> containsTVar elemType
    | TStream elemType -> containsTVar elemType
    | TDict (keyType, valueType) -> containsTVar keyType || containsTVar valueType
    | TTuple elemTypes -> List.exists containsTVar elemTypes
    | TRecord (_, typeArgs) -> List.exists containsTVar typeArgs
    | TSum (_, typeArgs) -> List.exists containsTVar typeArgs
    | TFunction (paramTypes, retType) ->
        List.exists containsTVar paramTypes || containsTVar retType
    | _ -> false

/// Check if two types are compatible (can be unified)
/// Type variables in either type can match concrete types
let typesCompatible (expected: Type) (actual: Type) : bool =
    match matchTypes expected actual with
    | Ok _ -> true
    | Error _ -> false

/// Check if two types are compatible after resolving type aliases
/// Combines alias resolution with type variable unification
let typesCompatibleWithAliases (aliasReg: AliasRegistry) (expected: Type) (actual: Type) : bool =
    let resolvedExpected = resolveType aliasReg expected
    let resolvedActual = resolveType aliasReg actual
    let rec nominalComparisonType typ =
        match typ with
        | TSum (name, typeArgs)
        | TRecord (name, typeArgs) -> TRecord (name, List.map nominalComparisonType typeArgs)
        | TFunction (parameters, result) ->
            TFunction (List.map nominalComparisonType parameters, nominalComparisonType result)
        | TTuple types -> TTuple (List.map nominalComparisonType types)
        | TEnumFields types -> TEnumFields (List.map nominalComparisonType types)
        | TList inner -> TList (nominalComparisonType inner)
        | TDict (keyType, valueType) ->
            TDict (nominalComparisonType keyType, nominalComparisonType valueType)
        | other -> other
    typesCompatible resolvedExpected resolvedActual
    || typesCompatible (nominalComparisonType resolvedExpected) (nominalComparisonType resolvedActual)

/// Consolidate bindings, checking for conflicts where the same type variable
/// is bound to different types. Returns a map from type var name to concrete type.
/// When a type var is bound to both a type containing TVars and a concrete type, prefer the concrete type.
let consolidateBindings (bindings: (string * Type) list) : Result<Map<string, Type>, string> =
    bindings
    |> List.fold (fun acc (name, typ) ->
        acc |> Result.bind (fun m ->
            match Map.tryFind name m with
            | None -> Ok (Map.add name typ m)
            | Some existingType ->
                if existingType = typ then
                    Ok m
                elif containsTVar existingType && not (containsTVar typ) then
                    // existing contains TVars, new is concrete - prefer new
                    Ok (Map.add name typ m)
                elif containsTVar typ && not (containsTVar existingType) then
                    // new contains TVars, existing is concrete - keep existing
                    Ok m
                elif containsTVar existingType && containsTVar typ then
                    // Both contain TVars. Where one side's variables are ones
                    // inference may bind (a generic seed passed to a generic
                    // fold: b$0 = Parser<a$1> from the seed and Parser<a> from
                    // the expected result; or `([], 0)` as a seed next to the
                    // (List<a>, Int) the lambda returns), bind those to the
                    // other side and keep the other side; the caller applies
                    // the map transitively. Otherwise keep the first.
                    let freshenedOnly (bindings: (string * Type) list) =
                        bindings |> List.forall (fun (n, _) -> isInferenceVar n)
                    match matchTypes existingType typ with
                    | Ok extra when freshenedOnly extra ->
                        Ok (extra |> List.fold (fun m' (n, t) ->
                                if Map.containsKey n m' then m' else Map.add n t m') (Map.add name typ m))
                    | _ ->
                        match matchTypes typ existingType with
                        | Ok extra when freshenedOnly extra ->
                            Ok (extra |> List.fold (fun m' (n, t) ->
                                    if Map.containsKey n m' then m' else Map.add n t m') m)
                        | _ -> Ok m
                else
                    // Both are concrete but different - that's an error
                    Error $"Type variable {name} has conflicting inferences: {typeToString existingType} vs {typeToString typ}"))
        (Ok Map.empty)

/// Unify a type pattern (may contain TVar) with a concrete type.
/// Returns a substitution mapping type variables to concrete types.
/// Example: unifyTypes (TVar "t") TInt64 = Ok (Map.ofList [("t", TInt64)])
let unifyTypes (pattern: Type) (actual: Type) : Result<Substitution, string> =
    matchTypes pattern actual
    |> Result.bind consolidateBindings

/// Reconcile two types where one might contain type variables.
/// If one type is concrete and the other has type variables that can unify with it,
/// returns the concrete type. If both are concrete and equal, returns the type.
/// If both are concrete and different, returns None.
/// The optional aliasReg parameter allows type alias resolution before comparison.
let reconcileTypes (aliasReg: AliasRegistry option) (t1: Type) (t2: Type) : Type option =
    // Resolve type aliases if registry is provided
    let t1' = aliasReg |> Option.map (fun reg -> resolveType reg t1) |> Option.defaultValue t1
    let t2' = aliasReg |> Option.map (fun reg -> resolveType reg t2) |> Option.defaultValue t2

    // Alias declarations are collected before constructor lookup has
    // canonicalized a bare nominal reference from TRecord to TSum. Treat the
    // two internal spellings as equivalent when their fully-qualified names
    // and arguments agree. A source program cannot declare a record and sum
    // with the same fully-qualified name, so this does not erase a meaningful
    // nominal distinction.
    let rec nominalComparisonType (typ: Type) : Type =
        match typ with
        | TSum (name, typeArgs)
        | TRecord (name, typeArgs) ->
            TRecord (name, List.map nominalComparisonType typeArgs)
        | TFunction (parameterTypes, returnType) ->
            TFunction (
                List.map nominalComparisonType parameterTypes,
                nominalComparisonType returnType)
        | TTuple elementTypes -> TTuple (List.map nominalComparisonType elementTypes)
        | TEnumFields fieldTypes -> TEnumFields (List.map nominalComparisonType fieldTypes)
        | TList elementType -> TList (nominalComparisonType elementType)
        | TDict (keyType, valueType) ->
            TDict (nominalComparisonType keyType, nominalComparisonType valueType)
        | other -> other

    if nominalComparisonType t1' = nominalComparisonType t2' then
        Some t1'
    elif t1' = TRuntimeError then
        Some t2'
    elif t2' = TRuntimeError then
        Some t1'
    elif t1' = TString && t2' = TChar then
        Some TString
    elif t1' = TChar && t2' = TString then
        Some TChar
    elif containsTVar t1' && not (containsTVar t2') then
        // t2 is concrete, check if t1 can unify with it
        match unifyTypes t1' t2' with
        | Ok _ -> Some t2'  // Return the concrete type
        | Error _ -> None
    elif not (containsTVar t1') && containsTVar t2' then
        // t1 is concrete, check if t2 can unify with it
        match unifyTypes t2' t1' with
        | Ok _ -> Some t1'  // Return the concrete type
        | Error _ -> None
    elif containsTVar t1' && containsTVar t2' then
        // Both have type variables. Bind the side whose variables inference
        // may bind and keep the other: `(List<t>, Int)` from a `([], 0)` seed
        // meets `(List<a>, Int)` from the other arm, and the answer is the
        // arm's, not the seed's, whichever came first.
        let bindsInferenceVarsOnly (subst: Substitution) =
            subst |> Map.forall (fun name _ -> isInferenceVar name)
        match unifyTypes t1' t2' with
        | Ok subst when bindsInferenceVarsOnly subst -> Some (applySubst subst t1')
        | firstDirection ->
            match unifyTypes t2' t1' with
            | Ok subst when bindsInferenceVarsOnly subst -> Some (applySubst subst t2')
            | _ ->
                match firstDirection with
                | Ok subst -> Some (applySubst subst t1')
                | Error _ -> None
    else
        None

/// Infer type arguments for a generic function call.
/// Given type parameters, parameter types (with type variables), and actual argument types,
/// returns the inferred type arguments in order matching typeParams.
/// Also takes optional function return type and expected return type for additional inference.
let inferTypeArgs (typeParams: string list) (paramTypes: Type list) (argTypes: Type list) (returnType: Type option) (expectedReturnType: Type option) : Result<Type list, string> =
    if List.length paramTypes <> List.length argTypes then
        Error $"Argument count mismatch: expected {List.length paramTypes}, got {List.length argTypes}"
    else
        // Match each parameter type against argument type
        let argMatchResults =
            List.zip paramTypes argTypes
            |> List.map (fun (paramT, argT) -> matchTypes paramT argT)

        // Also match return type against expected return type if both are provided
        let returnMatchResult =
            match returnType, expectedReturnType with
            | Some retT, Some expT -> matchTypes retT expT
            | _ -> Ok []

        // Combine all bindings
        (argMatchResults @ [returnMatchResult])
        |> List.fold (fun acc res ->
            match acc, res with
            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
            | Error e, _ -> Error e
            | _, Error e -> Error e) (Ok [])
        |> Result.bind consolidateBindings
        |> Result.bind (fun bindingMap ->
            // Extract type arguments in order of type parameters, preserving unresolved type vars.
            typeParams
            |> List.fold (fun acc paramName ->
                acc |> Result.bind (fun args ->
                    match Map.tryFind paramName bindingMap with
                    | Some typ -> Ok (args @ [applySubst bindingMap typ])
                    | None -> Ok (args @ [TVar paramName])))
                (Ok []))

/// Look up a name already resolved and canonicalized by the semantic boundary.
let tryLookupResolved (name: string) (m: Map<string, 'a>) : ('a * string) option =
    Map.tryFind name m |> Option.map (fun value -> (value, name))
