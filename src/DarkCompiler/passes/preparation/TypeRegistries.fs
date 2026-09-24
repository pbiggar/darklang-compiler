// TypeRegistries.fs - Describe source record, alias, and variable registries used during preparation.

module TypeRegistries

open MemoryModel
open ANF
open LoweringPrimitives

type RecordTypeInfo = {
    TypeParams: string list
    Fields: (string * AST.SemanticType) list
}

/// Type registry - maps nominal record identities to declared metadata.
type TypeRegistry = Map<string, RecordTypeInfo>

let recordFieldsRegistry (typeReg: TypeRegistry) : Map<string, (string * AST.SemanticType) list> =
    typeReg |> Map.map (fun _ info -> info.Fields)

let recordTypeParamsRegistry (typeReg: TypeRegistry) : Map<string, string list> =
    typeReg |> Map.map (fun _ info -> info.TypeParams)

let rcSumShapeRegistryFromVariantLookup (variantLookup: VariantLookup) : MemoryModel.RcSumShapeRegistry =
    let sumTypeNames =
        variantLookup
        |> Map.toList
        |> List.map (fun (_, (typeName, _, _, _)) -> typeName)
        |> Set.ofList

    let rec canonicalizePayloadType typ =
        match typ with
        | AST.TRecord (name, []) when Set.contains name sumTypeNames -> AST.TSum (name, [])
        | AST.TRecord (name, typeArgs) -> AST.TRecord (name, List.map canonicalizePayloadType typeArgs)
        | AST.TSum (name, typeArgs) -> AST.TSum (name, List.map canonicalizePayloadType typeArgs)
        | AST.TFunction (paramTypes, returnType) ->
            AST.TFunction (List.map canonicalizePayloadType paramTypes, canonicalizePayloadType returnType)
        | AST.TTuple elementTypes -> AST.TTuple (List.map canonicalizePayloadType elementTypes)
        | AST.TList elementType -> AST.TList (canonicalizePayloadType elementType)
        | AST.TDict (keyType, valueType) ->
            AST.TDict (canonicalizePayloadType keyType, canonicalizePayloadType valueType)
        | _ -> typ

    let addVariant
        (acc: Map<string, string list * (int * AST.SemanticType option) list>)
        (_variantName: string, (typeName, typeParams, tag, fieldTypes))
        =
        let payloadType =
            match fieldTypes with
            | [] -> None
            | [fieldType] -> Some fieldType
            | _ -> Some (AST.TTuple fieldTypes)
        match Map.tryFind typeName acc with
        | None ->
            Map.add typeName (typeParams, [(tag, payloadType)]) acc
        | Some (existingTypeParams, variants) ->
            if existingTypeParams = typeParams then
                Map.add typeName (typeParams, (tag, payloadType) :: variants) acc
            else
                Map.add typeName (existingTypeParams, (tag, payloadType) :: variants) acc

    let toSumShapeInfo _typeName (typeParams, variants) =
        { MemoryModel.TypeParams = typeParams
          MemoryModel.Payloads =
            variants
            |> List.sortBy fst
            |> List.map (fun (tag, payload) ->
                (tag, Option.map canonicalizePayloadType payload)) }

    variantLookup
    |> Map.toList
    |> List.filter (fun (variantName, (typeName, _, _, _)) ->
        // Qualified constructor identities are collision-free. Bare names can
        // be overwritten when several sum types expose cases such as
        // ParseError.BadFormat, which previously dropped whole enum shapes.
        variantName.StartsWith($"{typeName}."))
    |> List.fold addVariant Map.empty
    |> Map.map toSumShapeInfo

/// Function registry keyed by semantic identity. Names are retained as
/// definition metadata for diagnostics and backend symbol emission.
type FunctionRegistry = Map<AST.FunctionId, string * AST.SemanticType>

/// Display metadata for every resolved function identity, including compiler
/// intrinsics that do not have ordinary checked definitions.
type FunctionNameRegistry = Map<AST.FunctionId, string>

/// Resolve the small set of explicitly named lowering conventions to their
/// canonical semantic identities without rebuilding the inverse name table at
/// every recursive expression-lowering step.
type FunctionIdRegistry = Map<string, AST.FunctionId>

let functionIdsFromNames (names: FunctionNameRegistry) : FunctionIdRegistry =
    names
    |> Map.toSeq
    |> Seq.map (fun (id, name) -> name, id)
    |> Map.ofSeq

type TypeNameRegistry = CheckedAST.SemanticMetadata

let emptyTypeNames : TypeNameRegistry = {
    TypeNames = Map.empty
}

let typeNamesFromSymbols (symbols: CheckedAST.Symbols) : TypeNameRegistry =
    CheckedAST.semanticMetadata symbols

let tryFindConstructorTag id (_registry: TypeNameRegistry) = Some (AST.constructorRuntimeTag id)
let tryFindFieldIndex id (_registry: TypeNameRegistry) = Some (AST.fieldRuntimeIndex id)

let private listHeadUnsafeFunction
    (functionIds: FunctionIdRegistry)
    (elementType: AST.SemanticType)
    : AST.FunctionId * bool =
    let resolve name =
        Map.tryFind name functionIds
        |> Option.defaultWith (fun () ->
            Crash.crash $"List pattern helper '{name}' is absent from the function registry")
    let valueViewType = AST.TString
    let jsonAccessor =
        match elementType with
        | typ when typ = valueViewType -> Some "Darklang.Stdlib.Json.__viewListHead"
        | AST.TTuple [AST.TString; typ] when typ = valueViewType ->
            Some "Darklang.Stdlib.Json.__viewFieldListHead"
        | _ -> None
    match jsonAccessor with
    | Some name when Map.containsKey name functionIds -> (resolve name, false)
    | _ when elementType = AST.TFloat64 -> (resolve "Darklang.Stdlib.List.__headUnsafeFloat", false)
    | _ -> (resolve "Darklang.Stdlib.List.__headUnsafe_i64", true)

/// Pattern matching reads list payloads without taking an ownership edge.
/// Typed accessors materialize owned return values in their callee; the erased
/// i64 accessor cannot, because its compiled return type carries no managed
/// payload shape for reference-count insertion.
let internal listHeadUnsafeExpr
    (functionIds: FunctionIdRegistry)
    (elementType: AST.SemanticType)
    (listAtom: ANF.Atom)
    : ANF.CExpr =
    let functionId, borrowed = listHeadUnsafeFunction functionIds elementType
    if borrowed then
        ANF.BorrowedCall (functionId, [listAtom])
    else
        ANF.Call (functionId, [listAtom])

/// Alias registry - maps type alias names to their type params and target types
/// For simple record aliases: "Vec" -> ([], TRecord "Point")
type AliasRegistry = Map<string, string list * AST.SemanticType>

let private canonicalizeBareSumTypeRefsWithPredicate
    (isSumTypeName: string -> bool)
    (typ: AST.SemanticType)
    : AST.SemanticType =
    let rec canonicalize typ =
        match typ with
        | AST.TRecord (name, []) when isSumTypeName name ->
            AST.TSum (name, [])
        | AST.TRecord (name, typeArgs) ->
            AST.TRecord (name, List.map canonicalize typeArgs)
        | AST.TSum (name, typeArgs) ->
            AST.TSum (name, List.map canonicalize typeArgs)
        | AST.TFunction (paramTypes, returnType) ->
            AST.TFunction (List.map canonicalize paramTypes, canonicalize returnType)
        | AST.TTuple elemTypes ->
            AST.TTuple (List.map canonicalize elemTypes)
        | AST.TList elemType ->
            AST.TList (canonicalize elemType)
        | AST.TStream elemType ->
            AST.TStream (canonicalize elemType)
        | AST.TDict (keyType, valueType) ->
            AST.TDict (canonicalize keyType, canonicalize valueType)
        | AST.TVar _ | AST.TInferenceVar _ | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TInt
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
        | AST.TBool | AST.TFloat64 | AST.TString | AST.TBlob | AST.TChar | AST.TDateTime
        | AST.TUnit | AST.TInternalRawPtr | AST.TNever ->
            typ

    canonicalize typ

let internal canonicalizeBareSumTypeRefsWithNames
    (sumTypeNames: Set<string>)
    (typ: AST.SemanticType)
    : AST.SemanticType =
    canonicalizeBareSumTypeRefsWithPredicate (fun name -> Set.contains name sumTypeNames) typ

let internal canonicalizeBareSumTypeRefs (variantLookup: VariantLookup) (typ: AST.SemanticType) : AST.SemanticType =
    let isSumTypeName name =
        variantLookup
        |> Map.exists (fun _ (typeName, _, _, _) -> typeName = name)
    canonicalizeBareSumTypeRefsWithPredicate isSumTypeName typ

let internal canonicalizeNamedTypeRefs
    (recordNames: Set<string>)
    (sumTypeNames: Set<string>)
    (typ: AST.SemanticType)
    : AST.SemanticType =
    let rec canonicalize current =
        match current with
        | AST.TSum (name, args) when Set.contains name recordNames && not (Set.contains name sumTypeNames) ->
            AST.TRecord (name, List.map canonicalize args)
        | AST.TRecord (name, args) when Set.contains name sumTypeNames ->
            AST.TSum (name, List.map canonicalize args)
        | AST.TRecord (name, args) -> AST.TRecord (name, List.map canonicalize args)
        | AST.TSum (name, args) -> AST.TSum (name, List.map canonicalize args)
        | AST.TFunction (args, result) ->
            AST.TFunction (List.map canonicalize args, canonicalize result)
        | AST.TTuple elements -> AST.TTuple (List.map canonicalize elements)
        | AST.TList element -> AST.TList (canonicalize element)
        | AST.TDict (key, value) -> AST.TDict (canonicalize key, canonicalize value)
        | _ -> current
    canonicalize typ

/// Resolve a type name through the alias registry
/// If the name is an alias for a record type, returns the resolved record name
/// Otherwise returns the original name
let rec resolveRecordTypeName (aliasReg: AliasRegistry) (typeName: string) : string =
    match Map.tryFind typeName aliasReg with
    | Some ([], AST.TRecord (targetName, _)) -> resolveRecordTypeName aliasReg targetName
    | Some ([], AST.TSum (targetName, _)) -> resolveRecordTypeName aliasReg targetName
    | _ -> typeName

let rec private resolveAliasTypeForRegistry (aliasReg: AliasRegistry) (typ: AST.SemanticType) : AST.SemanticType =
    match typ with
    | AST.TRecord (name, []) ->
        match Map.tryFind name aliasReg with
        | Some ([], targetType) -> resolveAliasTypeForRegistry aliasReg targetType
        | _ -> AST.TRecord (name, [])
    | AST.TRecord (name, typeArgs) ->
        AST.TRecord (name, List.map (resolveAliasTypeForRegistry aliasReg) typeArgs)
    | AST.TSum (name, []) ->
        match Map.tryFind name aliasReg with
        | Some ([], targetType) -> resolveAliasTypeForRegistry aliasReg targetType
        | _ -> AST.TSum (name, [])
    | AST.TSum (name, typeArgs) ->
        AST.TSum (name, List.map (resolveAliasTypeForRegistry aliasReg) typeArgs)
    | AST.TTuple elemTypes ->
        AST.TTuple (List.map (resolveAliasTypeForRegistry aliasReg) elemTypes)
    | AST.TList elemType ->
        AST.TList (resolveAliasTypeForRegistry aliasReg elemType)
    | AST.TStream elemType ->
        AST.TStream (resolveAliasTypeForRegistry aliasReg elemType)
    | AST.TDict (keyType, valueType) ->
        AST.TDict (resolveAliasTypeForRegistry aliasReg keyType, resolveAliasTypeForRegistry aliasReg valueType)
    | AST.TFunction (paramTypes, returnType) ->
        AST.TFunction (
            List.map (resolveAliasTypeForRegistry aliasReg) paramTypes,
            resolveAliasTypeForRegistry aliasReg returnType
        )
    | AST.TVar _
    | AST.TInferenceVar _
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TInt128
    | AST.TInt
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128
    | AST.TBool
    | AST.TFloat64
    | AST.TString
    | AST.TBlob
    | AST.TChar
    | AST.TDateTime
    | AST.TUnit
    | AST.TInternalRawPtr
    | AST.TNever ->
        typ

let private resolveRegistryFields (aliasReg: AliasRegistry) (fields: (string * AST.SemanticType) list) : (string * AST.SemanticType) list =
    fields
    |> List.map (fun (fieldName, fieldType) ->
        (fieldName, resolveAliasTypeForRegistry aliasReg fieldType))

/// Expand a type registry to include alias entries
/// If "Vec" aliases to "Point" and "Point" has fields [x, y], then "Vec" also gets [x, y]
let expandTypeRegWithAliases (typeReg: TypeRegistry) (aliasReg: AliasRegistry) : TypeRegistry =
    let resolvedTypeReg =
        typeReg
        |> Map.map (fun _ info ->
            { info with Fields = resolveRegistryFields aliasReg info.Fields })

    aliasReg
    |> Map.fold (fun accReg aliasName (typeParams, targetType) ->
        match typeParams, targetType with
        | [], AST.TRecord (targetName, _) ->
            let resolvedName = resolveRecordTypeName aliasReg targetName
            match Map.tryFind resolvedName resolvedTypeReg with
            | Some targetInfo ->
                Map.add aliasName { targetInfo with TypeParams = typeParams } accReg
            | None -> accReg  // Target not found, skip
        | _ -> accReg  // Not a non-generic record alias, skip
    ) resolvedTypeReg

/// Variable environment - maps variable names to their TempIds and types
/// The type information is used for type-directed field lookup in record access
type VarEnv = Map<AST.BindingId, ANF.TempId * AST.SemanticType>

/// Extract just the type environment from VarEnv for use with inferType
let typeEnvFromVarEnv (varEnv: VarEnv) : Map<AST.BindingId, AST.SemanticType> =
    varEnv |> Map.map (fun _ (_, t) -> t)

// ============================================================================
// Monomorphization Support for Generic Functions
// ============================================================================
//
// The Dark compiler uses monomorphization to handle generics - each generic
// function instantiation becomes a separate specialized function with a
// mangled name (e.g., identity<Int64> → identity_i64).
//
// Algorithm:
// 1. Collect all generic function definitions (functions with TypeParams)
// 2. Scan for TypeApp expressions (calls to generic functions with type args)
// 3. For each unique (funcName, [typeArgs]) pair:
//    - Substitute type parameters with concrete types in the function body
//    - Generate a specialized function with mangled name
// 4. Replace all TypeApp calls with regular Calls to mangled names
// 5. Iterate until fixed-point (new specializations may contain more TypeApps)
//
// Key design decisions:
// - No runtime type info: all types resolved at compile time
// - Name mangling encodes types: identity_i64, swap_str_bool
// - Iterative: handles nested generics like List<Option<T>>
//
// See docs/compiler/frontend/generics.md for detailed documentation.
// ============================================================================
