// Types.fs - Define checking environments and resolve declared source types.

module CheckingTypes

open AST
open CheckingDiagnostics

/// Type environment - maps variable names to their types
type TypeEnv = Map<string, Type>

/// Function parameter-name registry - maps function names to ordered parameter names
type FuncParamNameRegistry = Map<string, string list>

/// Type registry - maps record type names to their ordered field definitions.
type TypeRegistry = Map<string, (string * Type) list>

/// Precomputed record metadata reused across separate type-checking invocations.
type RecordTypeInfo = {
    Fields: (string * Type) list
    FieldTypes: Map<string, Type>
    TypeParams: string list
}

/// Indexed view retained in TypeCheckEnv for reuse by separate compilations.
type IndexedTypeRegistry = Map<string, RecordTypeInfo>

/// Sum type registry - maps sum type names to their variant lists (name, tag, fields)
type SumTypeRegistry = Map<string, (string * int * Type list) list>

type SumVariantInfo = { Name: string; Tag: int; Fields: Type list }

type SumTypeInfo = {
    TypeParams: string list
    Variants: SumVariantInfo list
}

/// Indexed sum metadata retained in TypeCheckEnv so separate compilations do
/// not rebuild it from the complete constructor lookup.
type IndexedSumTypeRegistry = Map<string, SumTypeInfo>

/// Variant lookup - maps variant names to (type name, type params, tag index, field types)
/// Type params are the generic type parameters of the containing sum type
type VariantLookup = Map<string, (string * string list * int * Type list)>

let internal tryFindVariant
    (constructorReference: ConstructorReference)
    (variantName: string)
    (variantLookup: VariantLookup)
    : (string * string list * int * Type list) option =
    match constructorReferenceTypeName constructorReference with
    | None -> Map.tryFind variantName variantLookup
    | Some constructorTypeName ->
        Map.tryFind $"{constructorTypeName}.{variantName}" variantLookup

let internal unqualifiedVariantOwnerCount
    (variantName: string)
    (variantLookup: VariantLookup)
    : int =
    variantLookup
    |> Map.toList
    |> List.choose (fun (lookupName, (typeName, _, _, _)) ->
        if lookupName = $"{typeName}.{variantName}" then Some typeName else None)
    |> List.distinct
    |> List.length

/// Generic function registry and call-site policy controls.
/// `Functions` contains entries only for functions that have type parameters.
type GenericFuncRegistry = {
    Functions: Map<string, string list>
    RequireExplicitTypeArgsForBareCalls: bool
}

/// Alias registry - maps type alias names to (type params, target type)
/// Example: type Id = String -> ("Id", ([], TString))
/// Example: type Outer<a> = Inner<a, Int64> -> ("Outer", (["a"], TSum("Inner", [TVar "a"; TInt64])))
type AliasRegistry = Map<string, (string list * Type)>

/// Type substitution - maps type variable names to concrete types
type Substitution = Map<string, Type>

/// Collected type checking environment - can be passed to compile user code with stdlib
type TypeCheckEnv = {
    TypeReg: TypeRegistry
    IndexedTypeReg: IndexedTypeRegistry
    RecordTypeNames: Set<string>
    VariantLookup: VariantLookup
    IndexedSumTypeReg: IndexedSumTypeRegistry
    SumTypeNames: Set<string>
    FuncEnv: TypeEnv
    Values: Map<string, Type * Expr>
    FuncParamNames: FuncParamNameRegistry
    GenericFuncReg: GenericFuncRegistry
    GenericFuncDefs: Map<string, FunctionDef>
    ModuleRegistry: ModuleRegistry
    AliasReg: AliasRegistry
    ResolutionEnv: NameResolution.ResolutionEnvironment
}

/// Merge two TypeCheckEnv, with overlay taking precedence on conflicts
/// Used for separate compilation: merge stdlib env with user env
let mergeTypeCheckEnv (baseEnv: TypeCheckEnv) (overlay: TypeCheckEnv) : TypeCheckEnv =
    let mergeMap m1 m2 = Map.fold (fun acc k v -> Map.add k v acc) m1 m2
    {
        TypeReg = mergeMap baseEnv.TypeReg overlay.TypeReg
        IndexedTypeReg = mergeMap baseEnv.IndexedTypeReg overlay.IndexedTypeReg
        RecordTypeNames = Set.union baseEnv.RecordTypeNames overlay.RecordTypeNames
        VariantLookup = mergeMap baseEnv.VariantLookup overlay.VariantLookup
        IndexedSumTypeReg = mergeMap baseEnv.IndexedSumTypeReg overlay.IndexedSumTypeReg
        SumTypeNames = Set.union baseEnv.SumTypeNames overlay.SumTypeNames
        FuncEnv = mergeMap baseEnv.FuncEnv overlay.FuncEnv
        Values = mergeMap baseEnv.Values overlay.Values
        FuncParamNames = mergeMap baseEnv.FuncParamNames overlay.FuncParamNames
        GenericFuncReg = {
            Functions = mergeMap baseEnv.GenericFuncReg.Functions overlay.GenericFuncReg.Functions
            RequireExplicitTypeArgsForBareCalls =
                baseEnv.GenericFuncReg.RequireExplicitTypeArgsForBareCalls
                || overlay.GenericFuncReg.RequireExplicitTypeArgsForBareCalls
        }
        GenericFuncDefs = mergeMap baseEnv.GenericFuncDefs overlay.GenericFuncDefs
        ModuleRegistry = baseEnv.ModuleRegistry  // Module registry is constant, use base
        AliasReg = mergeMap baseEnv.AliasReg overlay.AliasReg
        ResolutionEnv = NameResolution.merge baseEnv.ResolutionEnv overlay.ResolutionEnv
    }

/// Resolve a type name through the alias registry
/// If the name is an alias, recursively resolve to the underlying type name
let rec resolveTypeName (aliasReg: AliasRegistry) (typeName: string) : string =
    match Map.tryFind typeName aliasReg with
    // A name-only projection cannot preserve instantiated target arguments.
    // Leave those aliases to resolveAliasTargetType, which carries substitution.
    | Some ([], TRecord (targetName, [])) -> resolveTypeName aliasReg targetName
    | _ -> typeName

/// Apply a substitution to a type, replacing type variables with concrete types
let rec private applySubstWithSeen (seen: Set<string>) (subst: Substitution) (typ: Type) : Type =
    match typ with
    | TVar name ->
        if Set.contains name seen then
            typ
        else
            let seen' = Set.add name seen
            match Map.tryFind name subst with
            | Some concreteType ->
                applySubstWithSeen seen' subst concreteType
            | None ->
                typ  // Unbound type variable remains as-is
    | TFunction (paramTypes, returnType) ->
        TFunction (List.map (applySubstWithSeen seen subst) paramTypes, applySubstWithSeen seen subst returnType)
    | TTuple elemTypes ->
        TTuple (List.map (applySubstWithSeen seen subst) elemTypes)
    | TRecord (name, typeArgs) ->
        TRecord (name, List.map (applySubstWithSeen seen subst) typeArgs)
    | TList elemType ->
        TList (applySubstWithSeen seen subst elemType)
    | TStream elemType ->
        TStream (applySubstWithSeen seen subst elemType)
    | TSum (name, typeArgs) ->
        TSum (name, List.map (applySubstWithSeen seen subst) typeArgs)
    | TDict (keyType, valueType) ->
        TDict (applySubstWithSeen seen subst keyType, applySubstWithSeen seen subst valueType)
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr ->
        typ  // Concrete types are unchanged

/// Apply a substitution to a type, replacing type variables with concrete types
let applySubst (subst: Substitution) (typ: Type) : Type =
    applySubstWithSeen Set.empty subst typ

/// Instantiate declared type parameters simultaneously. A replacement may use
/// the same name as a parameter in a nested declaration and must not itself be
/// substituted (for example Outer<'a> = Inner<String, 'a>).
let rec internal applyTypeArguments (subst: Substitution) (typ: Type) : Type =
    match typ with
    | TVar name -> Map.tryFind name subst |> Option.defaultValue typ
    | TFunction (paramTypes, returnType) ->
        TFunction (List.map (applyTypeArguments subst) paramTypes, applyTypeArguments subst returnType)
    | TTuple elemTypes -> TTuple (List.map (applyTypeArguments subst) elemTypes)
    | TRecord (name, typeArgs) -> TRecord (name, List.map (applyTypeArguments subst) typeArgs)
    | TList elemType -> TList (applyTypeArguments subst elemType)
    | TStream elemType -> TStream (applyTypeArguments subst elemType)
    | TSum (name, typeArgs) -> TSum (name, List.map (applyTypeArguments subst) typeArgs)
    | TDict (keyType, valueType) ->
        TDict (applyTypeArguments subst keyType, applyTypeArguments subst valueType)
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr -> typ

/// Collect type variable names in first-seen order.
let rec collectTypeVarsInType (typ: Type) (acc: string list) : string list =
    let add name =
        if List.contains name acc then acc else acc @ [name]

    match typ with
    | TVar name -> add name
    | TFunction (paramTypes, returnType) ->
        let withParams = paramTypes |> List.fold (fun a t -> collectTypeVarsInType t a) acc
        collectTypeVarsInType returnType withParams
    | TTuple elemTypes ->
        elemTypes |> List.fold (fun a t -> collectTypeVarsInType t a) acc
    | TRecord (_, typeArgs) ->
        typeArgs |> List.fold (fun a t -> collectTypeVarsInType t a) acc
    | TSum (_, typeArgs) ->
        typeArgs |> List.fold (fun a t -> collectTypeVarsInType t a) acc
    | TList elemType ->
        collectTypeVarsInType elemType acc
    | TStream elemType ->
        collectTypeVarsInType elemType acc
    | TDict (keyType, valueType) ->
        let withKey = collectTypeVarsInType keyType acc
        collectTypeVarsInType valueType withKey
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr ->
        acc

let private recordTypeInfo (typeParams: string list) (fields: (string * Type) list) : RecordTypeInfo =
    let (firstDeclaredFieldsRev, firstDeclaredFieldTypes) =
        fields
        |> List.fold (fun (orderedFields, fieldTypes) ((name, fieldType) as field) ->
            if Map.containsKey name fieldTypes then
                (orderedFields, fieldTypes)
            else
                (field :: orderedFields, Map.add name fieldType fieldTypes)) ([], Map.empty)
    {
        Fields = List.rev firstDeclaredFieldsRev
        FieldTypes = firstDeclaredFieldTypes
        TypeParams = typeParams
    }

let internal buildRecordFieldSubstitutionFromParams
    (typeParams: string list)
    (typeArgs: Type list)
    : Result<Substitution, string> =
    if List.length typeParams <> List.length typeArgs then
        Error
            $"Record type argument arity mismatch: expected {List.length typeParams}, got {List.length typeArgs}"
    else
        Ok (List.zip typeParams typeArgs |> Map.ofList)

/// Build a substitution for generic record fields from concrete type arguments.
let rec internal resolveAliasTargetType (aliasReg: AliasRegistry) (typ: Type) : Type =
    match typ with
    | TRecord (name, typeArgs) ->
        match Map.tryFind name aliasReg with
        | Some (typeParams, targetType) when List.length typeArgs <= List.length typeParams ->
            let subst = List.zip (List.take (List.length typeArgs) typeParams) typeArgs |> Map.ofList
            targetType |> applyTypeArguments subst |> resolveAliasTargetType aliasReg
        | _ ->
            TRecord (name, List.map (resolveAliasTargetType aliasReg) typeArgs)
    | TSum (name, typeArgs) ->
        match Map.tryFind name aliasReg with
        | Some (typeParams, targetType) when List.length typeArgs <= List.length typeParams ->
            let subst = List.zip (List.take (List.length typeArgs) typeParams) typeArgs |> Map.ofList
            targetType |> applyTypeArguments subst |> resolveAliasTargetType aliasReg
        | _ ->
            TSum (name, List.map (resolveAliasTargetType aliasReg) typeArgs)
    | TFunction (paramTypes, returnType) ->
        TFunction (List.map (resolveAliasTargetType aliasReg) paramTypes, resolveAliasTargetType aliasReg returnType)
    | TTuple elemTypes ->
        TTuple (List.map (resolveAliasTargetType aliasReg) elemTypes)
    | TList elemType ->
        TList (resolveAliasTargetType aliasReg elemType)
    | TStream elemType ->
        TStream (resolveAliasTargetType aliasReg elemType)
    | TDict (keyType, valueType) ->
        TDict (resolveAliasTargetType aliasReg keyType, resolveAliasTargetType aliasReg valueType)
    | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr ->
        typ

let private tryResolveGenericRecordAliasFields
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (typeName: string)
    : (string * (string * Type) list) option =
    match resolveAliasTargetType aliasReg (TRecord (typeName, [])) with
    | TRecord (targetName, targetTypeArgs)
    | TSum (targetName, targetTypeArgs) when targetName <> typeName || not (List.isEmpty targetTypeArgs) ->
        match Map.tryFind targetName typeReg with
        | Some targetInfo ->
            match buildRecordFieldSubstitutionFromParams targetInfo.TypeParams targetTypeArgs with
            | Ok subst ->
                let fields =
                    targetInfo.Fields
                    |> List.map (fun (fieldName, fieldType) -> (fieldName, applyTypeArguments subst fieldType))
                Some (targetName, fields)
            | Error _ ->
                None
        | None ->
            None
    | _ ->
        None

let internal tryResolveRecordLiteralInfo
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (reference: RecordReference)
    : (string * Type list * RecordTypeInfo) option =
    match
        resolveAliasTargetType
            aliasReg
            (TRecord (reference.SourceTypeName, reference.TypeArgs))
    with
    | TRecord (resolvedTypeName, aliasTypeArgs)
    | TSum (resolvedTypeName, aliasTypeArgs) ->
        Map.tryFind resolvedTypeName typeReg
        |> Option.map (fun info -> (resolvedTypeName, aliasTypeArgs, info))
    | _ -> None

/// Build a substitution from type parameters and type arguments
let buildSubstitution (typeParams: string list) (typeArgs: Type list) : Result<Substitution, string> =
    if List.length typeParams <> List.length typeArgs then
        Error $"Expected {List.length typeParams} type arguments, got {List.length typeArgs}"
    else
        Ok (List.zip typeParams typeArgs |> Map.ofList)

let private typeArgumentLabel (count: int) : string =
    if count = 1 then
        "type argument"
    else
        "type arguments"

let private argumentLabel (count: int) : string =
    if count = 1 then
        "argument"
    else
        "arguments"

let internal formatTypeArgumentArityError (funcName: string) (expectedCount: int) (actualCount: int) : string =
    $"{funcName} expects {expectedCount} {typeArgumentLabel expectedCount}, but got {actualCount} {typeArgumentLabel actualCount}"

let internal formatValueArgumentArityError (funcName: string) (expectedCount: int) (actualCount: int) : string =
    $"{funcName} expects {expectedCount} {argumentLabel expectedCount}, but got {actualCount} {argumentLabel actualCount}"

/// Apply a type substitution to an expression
/// This is used to propagate concrete types through nested TypeApp nodes
let rec applySubstToExpr (subst: Substitution) (expr: Expr) : Expr =
    match expr with
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ -> expr
    | BoundaryRender (renderer, value) -> BoundaryRender (renderer, applySubstToExpr subst value)
    | BinOp (op, left, right) ->
        BinOp (op, applySubstToExpr subst left, applySubstToExpr subst right)
    | UnaryOp (op, inner) ->
        UnaryOp (op, applySubstToExpr subst inner)
    | Let (pattern, value, body) ->
        Let (pattern, applySubstToExpr subst value, applySubstToExpr subst body)
    | RecursiveLet (recursion, value, body) ->
        RecursiveLet (recursion, applySubstToExpr subst value, applySubstToExpr subst body)
    | If (cond, thenBr, elseBr) ->
        If (applySubstToExpr subst cond, applySubstToExpr subst thenBr, applySubstToExpr subst elseBr)
    | Sequence (first, next) ->
        Sequence (applySubstToExpr subst first, applySubstToExpr subst next)
    | Call (funcName, args) ->
        Call (funcName, NonEmptyList.map (applySubstToExpr subst) args)
    | TypeApp (funcName, typeArgs, args) ->
        // Apply substitution to both type arguments and value arguments
        TypeApp (funcName, List.map (applySubst subst) typeArgs, NonEmptyList.map (applySubstToExpr subst) args)
    | TupleLiteral elements ->
        TupleLiteral (List.map (applySubstToExpr subst) elements)
    | TupleAccess (tuple, index) ->
        TupleAccess (applySubstToExpr subst tuple, index)
    | DictLiteral (keyType, valueType, entries) ->
        DictLiteral (
            applySubst subst keyType,
            applySubst subst valueType,
            entries |> List.map (fun (key, value) -> (applySubstToExpr subst key, applySubstToExpr subst value)))
    | RecordLiteral (reference, fields) ->
        RecordLiteral (
            { reference with TypeArgs = List.map (applySubst subst) reference.TypeArgs },
            List.map (fun (n, e) -> (n, applySubstToExpr subst e)) fields
        )
    | RecordUpdate (record, updates) ->
        RecordUpdate (applySubstToExpr subst record, List.map (fun (n, e) -> (n, applySubstToExpr subst e)) updates)
    | RecordAccess (record, fieldName) ->
        RecordAccess (applySubstToExpr subst record, fieldName)
    | Constructor (typeName, variantName, payload) ->
        Constructor (typeName, variantName, List.map (applySubstToExpr subst) payload)
    | Match (scrutinee, cases) ->
        Match (applySubstToExpr subst scrutinee,
               cases |> List.map (fun mc ->
                   { mc with Guard = mc.Guard |> Option.map (applySubstToExpr subst)
                             Body = applySubstToExpr subst mc.Body }))
    | ListLiteral elements ->
        ListLiteral (List.map (applySubstToExpr subst) elements)
    | Lambda (params', returnAnnotation, body) ->
        let concreteParams =
            params'
            |> NonEmptyList.map (fun parameter ->
                { parameter with
                    SourceAnnotation = parameter.SourceAnnotation |> Option.map (applySubst subst)
                    InferredType = parameter.InferredType |> Option.map (applySubst subst) })
        Lambda (concreteParams, returnAnnotation |> Option.map (applySubst subst), applySubstToExpr subst body)
    | Apply (func, args) ->
        Apply (applySubstToExpr subst func, NonEmptyList.map (applySubstToExpr subst) args)
    | IndirectApply (func, args) ->
        IndirectApply (applySubstToExpr subst func, NonEmptyList.map (applySubstToExpr subst) args)
    | Closure (funcName, captures) ->
        Closure (funcName, List.map (applySubstToExpr subst) captures)
    | InterpolatedString parts ->
        InterpolatedString (parts |> List.map (function
            | StringText s -> StringText s
            | StringExpr e -> StringExpr (applySubstToExpr subst e)))

/// Resolve a type by expanding any type aliases (recursively)
/// Returns the fully resolved type with all aliases replaced by their targets
let rec resolveType (aliasReg: AliasRegistry) (typ: Type) : Type =
    match typ with
    | TRecord (name, typeArgs) ->
        // Resolve type arguments first.
        let resolvedArgs = List.map (resolveType aliasReg) typeArgs
        // Check if this record name is actually a type alias.
        match Map.tryFind name aliasReg with
        | Some (typeParams, targetType) ->
            if List.length typeParams <> List.length resolvedArgs then
                // Mismatched type args, return as-is (error caught elsewhere)
                TRecord (name, resolvedArgs)
            else
                // Build substitution and apply to target type
                let subst = List.zip typeParams resolvedArgs |> Map.ofList
                let substituted = applyTypeArguments subst targetType
                // Recursively resolve in case target is also an alias
                resolveType aliasReg substituted
        | None ->
            // Not an alias, it's a real record type
            TRecord (name, resolvedArgs)
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
                let substituted = applyTypeArguments subst targetType
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
    | TStream elemType ->
        TStream (resolveType aliasReg elemType)
    | TDict (keyType, valueType) ->
        TDict (resolveType aliasReg keyType, resolveType aliasReg valueType)
    | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr ->
        typ  // Primitive types and type variables are unchanged

let resolveAliasesInTypeRegistry (aliasReg: AliasRegistry) (typeReg: TypeRegistry) : TypeRegistry =
    typeReg
    |> Map.map (fun _ fields ->
        fields
        |> List.map (fun (fieldName, fieldType) -> (fieldName, resolveType aliasReg fieldType)))

let internal sumTypeNamesFromVariantLookup (variantLookup: VariantLookup) : Set<string> =
    variantLookup
    |> Map.fold (fun names _ (typeName, _, _, _) -> Set.add typeName names) Set.empty

let internal indexSumTypeRegistry
    (variantLookup: VariantLookup)
    : IndexedSumTypeRegistry =
    variantLookup
    |> Map.fold
        (fun indexed lookupName (typeName, typeParams, tag, fields) ->
            let qualifiedPrefix = $"{typeName}."
            if not (lookupName.StartsWith qualifiedPrefix) then
                indexed
            else
                let variant = {
                    Name = lookupName.Substring qualifiedPrefix.Length
                    Tag = tag
                    Fields = fields
                }
                match Map.tryFind typeName indexed with
                | None ->
                    Map.add
                        typeName
                        { TypeParams = typeParams; Variants = [variant] }
                        indexed
                | Some info when
                    info.Variants |> List.exists (fun existing -> existing.Tag = tag) ->
                    indexed
                | Some info ->
                    Map.add
                        typeName
                        { info with Variants = info.Variants @ [variant] }
                        indexed)
        Map.empty
    |> Map.map (fun _ info ->
        { info with Variants = info.Variants |> List.sortBy (fun variant -> variant.Tag) })

let internal canonicalizeBareSumTypeRefsWithNames
    (sumTypeNames: Set<string>)
    (typ: Type)
    : Type =
    let rec canonicalize typ =
        match typ with
        | TRecord (name, []) when Set.contains name sumTypeNames ->
            TSum (name, [])
        | TRecord (name, typeArgs) ->
            TRecord (name, List.map canonicalize typeArgs)
        | TSum (name, typeArgs) ->
            TSum (name, List.map canonicalize typeArgs)
        | TFunction (paramTypes, returnType) ->
            TFunction (List.map canonicalize paramTypes, canonicalize returnType)
        | TTuple elemTypes ->
            TTuple (List.map canonicalize elemTypes)
        | TList elemType ->
            TList (canonicalize elemType)
        | TStream elemType ->
            TStream (canonicalize elemType)
        | TDict (keyType, valueType) ->
            TDict (canonicalize keyType, canonicalize valueType)
        | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
        | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
        | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr ->
            typ

    canonicalize typ

/// Resolve the parser's provisional generic named-type shape against nominal
/// declarations. Generic spellings initially use TSum because parsing happens
/// before the record and sum registries are available.
let internal canonicalizeDeclaredTypeRefsWithSumTypeNames
    (typeReg: Map<string, 'recordInfo>)
    (sumTypeNames: Set<string>)
    (typ: Type)
    : Type =
    let rec canonicalize current =
        match current with
        | TSum (name, typeArgs) when Map.containsKey name typeReg && not (Set.contains name sumTypeNames) ->
            TRecord (name, List.map canonicalize typeArgs)
        | TRecord (name, typeArgs) when Set.contains name sumTypeNames ->
            TSum (name, List.map canonicalize typeArgs)
        | TRecord (name, typeArgs) -> TRecord (name, List.map canonicalize typeArgs)
        | TSum (name, typeArgs) -> TSum (name, List.map canonicalize typeArgs)
        | TFunction (parameterTypes, returnType) ->
            TFunction (List.map canonicalize parameterTypes, canonicalize returnType)
        | TTuple elementTypes -> TTuple (List.map canonicalize elementTypes)
        | TList elementType -> TList (canonicalize elementType)
        | TStream elementType -> TStream (canonicalize elementType)
        | TDict (keyType, valueType) -> TDict (canonicalize keyType, canonicalize valueType)
        | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
        | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
        | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr -> current

    canonicalize typ

/// Build the indexed record metadata used by checking and generated helpers.
/// Separate-compilation callers use this for user records referenced by a
/// concrete stdlib specialization.
let indexTypeRegistry
    (variantLookup: VariantLookup)
    (recordTypeParams: Map<string, string list>)
    (typeReg: TypeRegistry)
    : IndexedTypeRegistry =
    let sumTypeNames = sumTypeNamesFromVariantLookup variantLookup
    typeReg
    |> Map.map (fun typeName fields ->
        fields
        |> List.map (fun (fieldName, fieldType) ->
            (fieldName,
             canonicalizeDeclaredTypeRefsWithSumTypeNames
                 typeReg
                 sumTypeNames
                 fieldType))
        |> recordTypeInfo
            (match Map.tryFind typeName recordTypeParams with
             | Some declared -> declared
             | None -> Crash.crash $"Missing declared record parameters for '{typeName}'"))

/// Compare two types for equality, resolving type aliases first
/// This allows "Vec" and "Point" to be considered equal when Vec aliases Point
let typesEqual (aliasReg: AliasRegistry) (t1: Type) (t2: Type) : bool =
    resolveType aliasReg t1 = resolveType aliasReg t2

let private truncateLegacyRecordValueText (text: string) : string =
    if text.Length > 10 then
        $"{text.Substring(0, 10)}..."
    else
        text

let internal formatLegacyRecordFieldTypeError
    (aliasReg: AliasRegistry)
    (fieldName: string)
    (expectedType: Type)
    (actualType: Type)
    (actualExpr: Expr)
    : string =
    let expectedText = expectedType |> resolveType aliasReg |> typeToString
    let actualText = actualType |> resolveType aliasReg |> typeToString
    let valueText =
        match tryFormatLiteralValue actualExpr with
        | Some value ->
            truncateLegacyRecordValueText value
        | None ->
            actualText

    $"Failed to create record. Expected {expectedText} for field `{fieldName}`, but got {valueText} ({withIndefiniteArticle actualText})"
