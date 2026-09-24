// Declarations.fs - Validate type declarations and summarize source inventories.

module CheckDeclarations

open AST
open CheckingDiagnostics
open CheckingTypes
open ComparisonPlanning
open ResolveDeclarations

let internal typeDefName (typeDef: TypeDef) : string =
    match typeDef with
    | RecordDef (name, _, _)
    | SumTypeDef (name, _, _)
    | TypeAlias (name, _, _) -> name

let private typeDefTypeParams (typeDef: TypeDef) : string list =
    match typeDef with
    | RecordDef (_, typeParams, _)
    | SumTypeDef (_, typeParams, _)
    | TypeAlias (_, typeParams, _) -> typeParams

/// Validate the nominal declaration namespace before building lookup maps.
/// Every type name is visible during this pure validation phase, which permits
/// recursive references without allowing later declarations to overwrite an
/// earlier identity.
let internal validateTopLevelTypeDeclarations
    (baseEnv: TypeCheckEnv option)
    (topLevels: TopLevel list)
    : Result<unit, TypeError> =
    let typeDefs =
        topLevels
        |> List.choose (function
            | TypeDef typeDef -> Some typeDef
            | _ -> None)
        |> List.rev
        |> List.distinctBy typeDefName
        |> List.rev

    let baseTypeArities =
        match baseEnv, typeDefs with
        | _, [] -> Map.empty
        | None, _ -> Map.empty
        | Some env, _ ->
            let recordArities =
                env.IndexedTypeReg
                |> Map.map (fun _ info -> List.length info.TypeParams)
            let aliasArities =
                env.AliasReg
                |> Map.map (fun _ (typeParams, _) -> List.length typeParams)
            let namedArities =
                Map.fold (fun acc name arity -> Map.add name arity acc) recordArities aliasArities
            env.VariantLookup
            |> Map.fold (fun acc _ (typeName, typeParams, _, _) ->
                Map.add typeName (List.length typeParams) acc) namedArities

    let typeArities =
        typeDefs
        |> List.fold (fun arities typeDef ->
            Map.add (typeDefName typeDef) (List.length (typeDefTypeParams typeDef)) arities) baseTypeArities

    let declaringModule owner =
        match NameResolution.tryQualifiedName owner with
        | None -> []
        | Some qualified ->
            qualified
            |> NameResolution.qualifiedNameSegments
            |> List.rev
            |> List.tail
            |> List.rev

    let resolveTypeArity owner name =
        NameResolution.candidateSpellings
            NameResolution.ResolutionContext.Type
            (declaringModule owner)
            name
        |> List.tryPick (fun candidate ->
            Map.tryFind candidate typeArities
            |> Option.map (fun arity -> (candidate, arity)))

    let rec validateTypeReference (owner: string) (typ: SemanticType) : Result<unit, TypeError> =
        let validateAll types =
            types
            |> List.fold (fun result item ->
                result |> Result.bind (fun () -> validateTypeReference owner item)) (Ok ())

        match typ with
        | TRecord (name, typeArgs)
        | TSum (name, typeArgs) ->
            match resolveTypeArity owner name with
            | None -> Error (GenericError $"Unknown type reference: {name} in {owner}")
            | Some (_, expectedArity) when expectedArity <> List.length typeArgs ->
                Error (
                    GenericError
                        $"Type argument arity mismatch: {name} expects {expectedArity}, got {List.length typeArgs} in {owner}"
                )
            | Some _ -> validateAll typeArgs
        | TFunction (parameters, result) -> validateAll (parameters @ [result])
        | TTuple types -> validateAll types
        | TList element -> validateTypeReference owner element
        | TStream element -> validateTypeReference owner element
        | TDict (key, value) -> validateAll [key; value]
        | TVar _ | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
        | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
        | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TNever | TInternalRawPtr -> Ok ()

    let duplicateTypeName =
        typeDefs
        |> List.map typeDefName
        |> List.countBy id
        |> List.tryPick (fun (name, count) -> if count > 1 then Some name else None)

    let validateAliasCycles () : Result<unit, TypeError> =
        let aliasNames =
            typeDefs
            |> List.choose (function TypeAlias (name, _, _) -> Some name | _ -> None)
            |> Set.ofList
        let rec referencedAliases typ =
            let combine types =
                types
                |> List.map referencedAliases
                |> List.fold Set.union Set.empty
            match typ with
            | TRecord (name, args) | TSum (name, args) ->
                let nested = combine args
                if Set.contains name aliasNames then Set.add name nested else nested
            | TFunction (parameters, result) -> combine (result :: parameters)
            | TTuple types -> combine types
            | TList element
            | TStream element -> referencedAliases element
            | TDict (key, value) -> combine [key; value]
            | TVar _ | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
            | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
            | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit
            | TNever | TInternalRawPtr -> Set.empty
        let graph =
            typeDefs
            |> List.choose (function
                | TypeAlias (name, _, target) -> Some (name, referencedAliases target)
                | _ -> None)
            |> Map.ofList
        let rec visit name states =
            match Map.tryFind name states with
            | Some AliasValidated -> Ok states
            | Some AliasVisiting ->
                Error (GenericError $"Invalid recursive type alias cycle involving: {name}")
            | None ->
                let visiting = Map.add name AliasVisiting states
                Map.find name graph
                |> Set.toList
                |> List.fold (fun result dependency ->
                    result |> Result.bind (visit dependency)) (Ok visiting)
                |> Result.map (Map.add name AliasValidated)
        graph
        |> Map.toList
        |> List.map fst
        |> List.fold (fun result name -> result |> Result.bind (visit name)) (Ok Map.empty)
        |> Result.map (fun _ -> ())

    match duplicateTypeName with
    | Some name ->
        Error (GenericError $"Duplicate type declaration: {name}")
    | None ->
        let collidingCaseNames = collidingConstructorCaseNames typeDefs
        let constructorIdentityCollision =
            typeDefs
            |> List.collect (function
                | SumTypeDef (typeName, _, variants) ->
                    variants
                    |> List.filter (fun variant -> Set.contains variant.Name collidingCaseNames)
                    |> List.map (fun variant ->
                        (constructorRuntimeIdentity typeName variant.Name, $"{typeName}.{variant.Name}"))
                | _ -> [])
            |> List.groupBy fst
            |> List.tryPick (fun (identity, entries) ->
                match entries |> List.map snd |> List.distinct with
                | _ :: _ :: _ as names -> Some (identity, names)
                | _ -> None)

        match constructorIdentityCollision with
        | Some (identity, names) ->
            let joinedNames = String.concat ", " names
            Error (
                GenericError
                    $"Constructor identity collision {identity}: {joinedNames}"
            )
        | None ->
        let rec validate remaining =
            match remaining with
            | [] -> Ok ()
            | typeDef :: rest ->
                let typeName = typeDefName typeDef
                let duplicateTypeParam =
                    typeDefTypeParams typeDef
                    |> List.countBy id
                    |> List.tryPick (fun (name, count) -> if count > 1 then Some name else None)

                match duplicateTypeParam with
                | Some param ->
                    Error (GenericError $"Duplicate type parameter: {param} in {typeName}")
                | None ->
                    let declaredTypeParams = typeDefTypeParams typeDef |> Set.ofList
                    let referencedTypeParams =
                        match typeDef with
                        | RecordDef (_, _, fields) ->
                            fields
                            |> List.fold (fun acc (_, fieldType) -> collectTypeVarsInType fieldType acc) []
                        | SumTypeDef (_, _, variants) ->
                            variants
                            |> List.fold (fun acc variant ->
                                variant.Fields
                                |> List.fold (fun fieldsAcc fieldType ->
                                    collectTypeVarsInType fieldType fieldsAcc) acc) []
                        | TypeAlias (_, _, targetType) ->
                            collectTypeVarsInType targetType []
                    let undeclaredTypeParam =
                        referencedTypeParams
                        |> List.tryFind (fun name -> not (Set.contains name declaredTypeParams))

                    match undeclaredTypeParam with
                    | Some param ->
                        Error (GenericError $"Undeclared type parameter: '{param} in {typeName}")
                    | None ->
                    let referencedTypes =
                        match typeDef with
                        | RecordDef (_, _, fields) -> List.map snd fields
                        | SumTypeDef (_, _, variants) -> variants |> List.collect (fun variant -> variant.Fields)
                        | TypeAlias (_, _, targetType) -> [targetType]
                    let referenceResult =
                        referencedTypes
                        |> List.fold (fun result referencedType ->
                            result
                            |> Result.bind (fun () -> validateTypeReference typeName referencedType)) (Ok ())
                    let declarationResult =
                        match typeDef with
                        | RecordDef (_, _, []) ->
                            Error (GenericError $"Record declaration must contain at least one field: {typeName}")
                        // The interpreter preserves duplicate declarations and
                        // resolves lookup against the first declaration.
                        | RecordDef _ -> Ok ()
                        | SumTypeDef (_, _, []) ->
                            Error (GenericError $"Enum declaration must contain at least one case: {typeName}")
                        | SumTypeDef (_, _, variants) ->
                            variants
                            |> List.map (fun variant -> variant.Name)
                            |> List.countBy id
                            |> List.tryPick (fun (name, count) -> if count > 1 then Some name else None)
                            |> function
                                | Some caseName ->
                                    Error (GenericError $"Duplicate constructor declaration: {typeName}.{caseName}")
                                | None -> Ok ()
                        | TypeAlias _ -> Ok ()
                    referenceResult
                    |> Result.bind (fun () -> declarationResult)
                    |> Result.bind (fun () -> validate rest)

        validateAliasCycles () |> Result.bind (fun () -> validate typeDefs)

/// Build all declaration registries after validation and name resolution have
/// established unique nominal type and constructor identities.
let internal summarizeTopLevelDeclarations
    (topLevels: TopLevel list)
    : TopLevelDeclarationSummary =
    let empty : TopLevelDeclarationSummary = {
        TypeReg = Map.empty
        RecordTypeParams = Map.empty
        AliasReg = Map.empty
        VariantLookup = Map.empty
        FuncSigs = Map.empty
        FuncParamNames = Map.empty
        GenericFuncs = Map.empty
    }

    let typeDefs =
        topLevels
        |> List.choose (function | TypeDef typeDef -> Some typeDef | _ -> None)
        |> List.rev
        |> List.distinctBy typeDefName
        |> List.rev
    let collidingCaseNames = collidingConstructorCaseNames typeDefs

    let winningIndices =
        topLevels
        |> List.indexed
        |> List.choose (fun (index, topLevel) ->
            match topLevel with
            | FunctionDef definition -> Some (("function", definition.Name), index)
            | ValueDef definition -> Some (("value", valueDefName definition), index)
            | TypeDef definition -> Some (("type", typeDefName definition), index)
            | Expression _ -> None)
        |> Map.ofList

    let declarationsToSummarize =
        topLevels
        |> List.indexed
        |> List.choose (fun (index, topLevel) ->
            let key =
                match topLevel with
                | FunctionDef definition -> Some ("function", definition.Name)
                | ValueDef definition -> Some ("value", valueDefName definition)
                | TypeDef definition -> Some ("type", typeDefName definition)
                | Expression _ -> None
            match key with
            | None -> Some topLevel
            | Some declarationKey when Map.tryFind declarationKey winningIndices = Some index -> Some topLevel
            | Some _ -> None)

    declarationsToSummarize
    |> List.fold (fun summary topLevel ->
        match topLevel with
        | TypeDef (RecordDef (name, typeParams, fields)) ->
            { summary with
                TypeReg = Map.add name fields summary.TypeReg
                RecordTypeParams = Map.add name typeParams summary.RecordTypeParams }
        | TypeDef (TypeAlias (name, typeParams, targetType)) ->
            { summary with AliasReg = Map.add name (typeParams, targetType) summary.AliasReg }
        | TypeDef (SumTypeDef (typeName, typeParams, variants)) ->
            let variantLookup =
                variants
                |> List.indexed
                |> List.fold (fun lookup (ordinal, variant) ->
                    let tag =
                        if Set.contains variant.Name collidingCaseNames then
                            constructorRuntimeIdentity typeName variant.Name
                        else
                            ordinal
                    let info = (typeName, typeParams, tag, variant.Fields)
                    lookup
                    |> Map.add variant.Name info
                    |> Map.add $"{typeName}.{variant.Name}" info) summary.VariantLookup
            { summary with VariantLookup = variantLookup }
        | FunctionDef funcDef ->
            let parameters = NonEmptyList.toList funcDef.Params
            let genericFuncs =
                match funcDef.TypeParams with
                | [] -> summary.GenericFuncs
                | typeParams -> Map.add funcDef.Name typeParams summary.GenericFuncs
            {
                summary with
                    FuncSigs =
                        Map.add
                            funcDef.Name
                            (List.map snd parameters, funcDef.ReturnType)
                            summary.FuncSigs
                    FuncParamNames =
                        Map.add funcDef.Name (List.map fst parameters) summary.FuncParamNames
                    GenericFuncs = genericFuncs
            }
        | ValueDef _ -> summary
        | Expression _ ->
            summary) empty
