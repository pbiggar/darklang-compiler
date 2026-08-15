// 1.6_ValueRendering.fs - Interpreter-compatible result rendering
//
// Builds monomorphic Dark functions which render values at the eval boundary.
// Keeping recursion in ordinary Dark code gives tuples, lists, records, and sums
// one renderer on every native target instead of backend-specific shape switches.

module ValueRendering

open AST

type private SumVariant = {
    Name: string
    Tag: int
    Payload: Type option
}

type private SumInfo = {
    TypeParams: string list
    Variants: SumVariant list
}

type private RecordInfo = {
    TypeParams: string list
    Fields: (string * Type) list
}

type private RenderEnv = {
    Records: Map<string, RecordInfo>
    Sums: Map<string, SumInfo>
    NamedFunctions: Set<string>
}

type private RenderState = {
    Functions: Map<string, FunctionDef>
}

let private firstDeclaredRecordFields (fields: (string * Type) list) : (string * Type) list =
    fields
    |> List.fold (fun (seen, retainedRev) ((name, _) as field) ->
        if Set.contains name seen then (seen, retainedRev)
        else (Set.add name seen, field :: retainedRev)) (Set.empty, [])
    |> snd
    |> List.rev

let private args (values: Expr list) : NonEmptyList<Expr> =
    NonEmptyList.fromList values

let private call (name: string) (values: Expr list) : Expr =
    Call (name, args values)

let private concat (parts: Expr list) : Expr =
    match parts with
    | [] -> StringLiteral ""
    | first :: rest ->
        // Every renderer fragment has an ASCII delimiter at each join: quotes,
        // punctuation, separators, or the edge of a canonical numeric value.
        // Those boundaries cannot compose under NFC, so retain the native raw
        // concat used before public StringConcat acquired normalization.
        List.fold (fun acc part -> call "__string_concat_raw" [acc; part]) first rest

let private stableHash (value: string) : uint64 =
    value
    |> Seq.fold
        (fun hash ch -> (hash ^^^ uint64 (int ch)) * 1099511628211UL)
        14695981039346656037UL

let private rendererName (typ: Type) : string =
    let text = TypeChecking.typeToString typ
    $"__dark_render_value_{stableHash text:x16}"

let private listItemsRendererName (typ: Type) : string =
    let text = TypeChecking.typeToString typ
    $"__dark_render_list_items_{stableHash text:x16}"

let private dictItemsRendererName (typ: Type) : string =
    let text = TypeChecking.typeToString typ
    $"__dark_render_dict_items_{stableHash text:x16}"

let private applySubstitution (subst: Map<string, Type>) (typ: Type) : Type =
    let rec apply typ =
        match typ with
        | TVar name -> Map.tryFind name subst |> Option.defaultValue typ
        | TList elemType -> TList (apply elemType)
        | TStream elemType -> TStream (apply elemType)
        | TDict (keyType, valueType) -> TDict (apply keyType, apply valueType)
        | TFunction (paramTypes, returnType) -> TFunction (List.map apply paramTypes, apply returnType)
        | TTuple elemTypes -> TTuple (List.map apply elemTypes)
        | TEnumFields fieldTypes -> TEnumFields (List.map apply fieldTypes)
        | TRecord (name, typeArgs) -> TRecord (name, List.map apply typeArgs)
        | TSum (name, typeArgs) -> TSum (name, List.map apply typeArgs)
        | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
        | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
        | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit
        | TRuntimeError | TRawPtr -> typ
    apply typ

let private typeSubstitution (typeParams: string list) (typeArgs: Type list) : Map<string, Type> =
    if List.length typeParams = List.length typeArgs then
        List.zip typeParams typeArgs |> Map.ofList
    else
        Crash.crash
            $"Value renderer type argument mismatch: params={List.length typeParams}, args={List.length typeArgs}"

let private escapedString (quote: string) (value: Expr) : Expr =
    let replace oldValue newValue input =
        call "Stdlib.String.replace" [input; StringLiteral oldValue; StringLiteral newValue]

    let escaped =
        value
        |> replace "\\" "\\\\"
        |> replace "\n" "\\n"
        |> replace "\r" "\\r"
        |> replace "\t" "\\t"
        |> replace quote ($"\\{quote}")

    concat [StringLiteral quote; escaped; StringLiteral quote]

let private makeCase (pattern: Pattern) (body: Expr) : MatchCase =
    { Patterns = NonEmptyList.singleton pattern; Guard = None; Body = body }

let rec private canonicalRenderType (env: RenderEnv) (typ: Type) : Type =
    let canonical = canonicalRenderType env
    match typ with
    | TRecord (name, typeArgs) when Map.containsKey name env.Sums ->
        TSum (name, List.map canonical typeArgs)
    | TRecord (name, typeArgs) -> TRecord (name, List.map canonical typeArgs)
    | TSum (name, typeArgs) -> TSum (name, List.map canonical typeArgs)
    | TTuple elementTypes -> TTuple (List.map canonical elementTypes)
    | TEnumFields fieldTypes -> TEnumFields (List.map canonical fieldTypes)
    | TList elementType -> TList (canonical elementType)
    | TDict (keyType, valueType) -> TDict (canonical keyType, canonical valueType)
    | TFunction (parameterTypes, returnType) ->
        TFunction (List.map canonical parameterTypes, canonical returnType)
    | _ -> typ

let rec private ensureRenderer
    (env: RenderEnv)
    (typ: Type)
    (state: RenderState)
    : string * RenderState =
    let typ = canonicalRenderType env typ
    let name = rendererName typ
    match Map.tryFind name state.Functions with
    | Some _ -> (name, state)
    | None ->
        // Reserve the name before descending so recursive sum types terminate.
        let placeholder = {
            Name = name
            TypeParams = []
            Params = NonEmptyList.singleton ("__value", typ)
            ReturnType = TString
            Body = StringLiteral ""
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let (body, withDependencies) = renderBody env typ (Var "__value") reserved
        let completed = { placeholder with Body = body }
        (name, { withDependencies with Functions = Map.add name completed withDependencies.Functions })

and private renderCall
    (env: RenderEnv)
    (typ: Type)
    (value: Expr)
    (state: RenderState)
    : Expr * RenderState =
    let (name, nextState) = ensureRenderer env typ state
    (call name [value], nextState)

and private renderDelimited
    (env: RenderEnv)
    (items: (Type * Expr) list)
    (state: RenderState)
    : Expr list * RenderState =
    let rec loop remaining currentState acc =
        match remaining with
        | [] -> (List.rev acc, currentState)
        | (itemType, itemExpr) :: rest ->
            let (rendered, nextState) = renderCall env itemType itemExpr currentState
            loop rest nextState (rendered :: acc)
    loop items state []

and private ensureListItemsRenderer
    (env: RenderEnv)
    (elemType: Type)
    (state: RenderState)
    : string * RenderState =
    let listType = TList elemType
    let name = listItemsRendererName listType
    match Map.tryFind name state.Functions with
    | Some _ -> (name, state)
    | None ->
        let placeholder = {
            Name = name
            TypeParams = []
            Params = NonEmptyList.singleton ("__items", listType)
            ReturnType = TString
            Body = StringLiteral ""
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let (renderedHead, withElemRenderer) = renderCall env elemType (Var "__head") reserved
        let tailBody =
            Match (
                Var "__tail",
                [ makeCase (PList []) (StringLiteral "")
                  makeCase PWildcard (concat [StringLiteral ", "; call name [Var "__tail"]]) ]
            )
        let body =
            Match (
                Var "__items",
                [ makeCase (PList []) (StringLiteral "")
                  makeCase
                      (PListCons ([PVar "__head"], PVar "__tail"))
                      (concat [renderedHead; tailBody]) ]
            )
        let completed = { placeholder with Body = body }
        (name, { withElemRenderer with Functions = Map.add name completed withElemRenderer.Functions })

and private ensureDictItemsRenderer
    (env: RenderEnv)
    (valueType: Type)
    (state: RenderState)
    : string * RenderState =
    let entryType = TTuple [TString; valueType]
    let listType = TList entryType
    let name = dictItemsRendererName (TDict (TString, valueType))
    match Map.tryFind name state.Functions with
    | Some _ -> (name, state)
    | None ->
        let placeholder = {
            Name = name
            TypeParams = []
            Params = NonEmptyList.singleton ("__entries", listType)
            ReturnType = TString
            Body = StringLiteral ""
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let entryValue = TupleAccess (Var "__entry", 1)
        let (renderedValue, withValueRenderer) = renderCall env valueType entryValue reserved
        let renderedEntry =
            concat [
                call "Stdlib.Dict.__renderKey" [TupleAccess (Var "__entry", 0)]
                StringLiteral " = "
                renderedValue
            ]
        let tailBody =
            Match (
                Var "__tail",
                [ makeCase (PList []) (StringLiteral "")
                  makeCase PWildcard (concat [StringLiteral "; "; call name [Var "__tail"]]) ]
            )
        let body =
            Match (
                Var "__entries",
                [ makeCase (PList []) (StringLiteral "")
                  makeCase
                      (PListCons ([PVar "__entry"], PVar "__tail"))
                      (concat [renderedEntry; tailBody]) ]
            )
        let completed = { placeholder with Body = body }
        (name, { withValueRenderer with Functions = Map.add name completed withValueRenderer.Functions })

and private renderBody
    (env: RenderEnv)
    (typ: Type)
    (value: Expr)
    (state: RenderState)
    : Expr * RenderState =
    match typ with
    | TUnit -> (StringLiteral "()", state)
    | TBool -> (If (value, StringLiteral "true", StringLiteral "false"), state)
    | TInt8 -> (call "Stdlib.Int8.toString" [value], state)
    | TInt16 -> (call "Stdlib.Int16.toString" [value], state)
    | TInt32 -> (call "Stdlib.Int32.toString" [value], state)
    | TInt64 -> (call "Stdlib.Int64.toString" [value], state)
    | TInt -> (call "Stdlib.Int.toString" [value], state)
    | TUInt8 -> (call "Stdlib.UInt8.toString" [value], state)
    | TUInt16 -> (call "Stdlib.UInt16.toString" [value], state)
    | TUInt32 -> (call "Stdlib.UInt32.toString" [value], state)
    | TUInt64 -> (call "Stdlib.UInt64.toString" [value], state)
    // The current compiler stores supported 128-bit results as canonical decimal strings.
    | TInt128 | TUInt128 -> (value, state)
    | TFloat64 -> (call "Stdlib.Float.toString" [value], state)
    | TString -> (escapedString "\"" value, state)
    | TChar -> (escapedString "'" value, state)
    | TDateTime -> (call "Stdlib.DateTime.toString" [value], state)
    | TTuple elemTypes ->
        let items = elemTypes |> List.mapi (fun index elemType -> (elemType, TupleAccess (value, index)))
        let (rendered, nextState) = renderDelimited env items state
        let separated =
            rendered
            |> List.mapi (fun index expr -> if index = 0 then [expr] else [StringLiteral ", "; expr])
            |> List.concat
        (concat (StringLiteral "(" :: separated @ [StringLiteral ")"]), nextState)
    | TEnumFields _ ->
        Crash.crash "TEnumFields is declaration metadata and cannot be rendered as a value type"
    | TList elemType ->
        let (itemsName, nextState) = ensureListItemsRenderer env elemType state
        let typeName = TypeChecking.typeToString typ
        let body =
            Match (
                value,
                [ makeCase (PList []) (StringLiteral $"{typeName} []")
                  makeCase PWildcard (concat [StringLiteral "["; call itemsName [value]; StringLiteral "]"]) ]
            )
        (body, nextState)
    | TStream _ ->
        (StringLiteral "<stream>", state)
    // An unconstrained Dict value can only be the polymorphic empty literal;
    // no value renderer is needed because there are no entries to inspect.
    | TDict (TString, TVar _) -> (StringLiteral "Dict { }", state)
    | TDict (TString, valueType) ->
        let (itemsName, nextState) = ensureDictItemsRenderer env valueType state
        let entries = TypeApp ("Stdlib.Dict.toList", [valueType], NonEmptyList.singleton value)
        let body =
            Let (
                LPVariable "__dict_entries",
                entries,
                Match (
                    Var "__dict_entries",
                    [ makeCase (PList []) (StringLiteral "Dict { }")
                      makeCase
                          PWildcard
                          (concat
                              [ StringLiteral "Dict { "
                                call itemsName [Var "__dict_entries"]
                                StringLiteral " }" ]) ]
                )
            )
        (body, nextState)
    | TRecord (typeName, typeArgs) ->
        match Map.tryFind typeName env.Records with
        | None ->
            Crash.crash $"Missing record metadata for value renderer: {typeName}"
        | Some recordInfo ->
            let fallbackTypeParams =
                recordInfo.Fields
                |> List.collect (fun (_, fieldType) ->
                    let rec collect typ =
                        match typ with
                        | TVar name -> [name]
                        | TList elem -> collect elem
                        | TDict (key, value) -> collect key @ collect value
                        | TFunction (parameters, result) -> List.collect collect parameters @ collect result
                        | TTuple elems -> List.collect collect elems
                        | TEnumFields fields -> List.collect collect fields
                        | TRecord (_, args) | TSum (_, args) -> List.collect collect args
                        | _ -> []
                    collect fieldType)
                |> List.distinct
            let typeParams =
                if List.isEmpty recordInfo.TypeParams then fallbackTypeParams
                else recordInfo.TypeParams
            let subst = typeSubstitution typeParams typeArgs
            let sortedFields = recordInfo.Fields |> List.sortBy fst
            let rec renderFields remaining currentState acc =
                match remaining with
                | [] -> (List.rev acc, currentState)
                | (fieldName, fieldType) :: rest ->
                    let concreteType = applySubstitution subst fieldType
                    let (rendered, nextState) = renderCall env concreteType (RecordAccess (value, fieldName)) currentState
                    renderFields rest nextState ((fieldName, rendered) :: acc)
            let (renderedFields, nextState) = renderFields sortedFields state []
            let shortParts =
                renderedFields
                |> List.mapi (fun index (fieldName, rendered) ->
                    let prefix = if index = 0 then "" else ", "
                    [StringLiteral $"{prefix}{fieldName}: "; rendered])
                |> List.concat
            let typeText = TypeChecking.typeToString typ
            let short = concat (StringLiteral $"{typeText} {{ " :: shortParts @ [StringLiteral " }"])
            let longParts =
                renderedFields
                |> List.mapi (fun index (fieldName, rendered) ->
                    let prefix = if index = 0 then "" else ",\n  "
                    [StringLiteral $"{prefix}{fieldName}: "; rendered])
                |> List.concat
            let long = concat (StringLiteral $"{typeText} {{\n  " :: longParts @ [StringLiteral "\n}"])
            let shortName = "__record_short"
            (Let (
                LPVariable shortName,
                short,
                If (
                    BinOp (
                        Lte,
                        call "Stdlib.String.length" [Var shortName],
                        BigIntLiteral (System.Numerics.BigInteger 80)
                    ),
                    Var shortName,
                    long
                )
             ), nextState)
    | TSum (typeName, typeArgs) ->
        match Map.tryFind typeName env.Sums with
        | None -> Crash.crash $"Missing sum metadata for value renderer: {typeName}"
        | Some sumInfo ->
            let subst = typeSubstitution sumInfo.TypeParams typeArgs
            let typeText = TypeChecking.typeToString typ
            let rec buildCases remaining currentState acc =
                match remaining with
                | [] -> (List.rev acc, currentState)
                | variant :: rest ->
                    match variant.Payload with
                    | None ->
                        let case = makeCase (PConstructor (variant.Name, None)) (StringLiteral $"{typeText}.{variant.Name}")
                        buildCases rest currentState (case :: acc)
                    | Some payloadType ->
                        let concreteType = applySubstitution subst payloadType
                        let payloadName = $"__payload_{variant.Tag}"
                        let (renderedFields, nextState) =
                            match concreteType with
                            | TEnumFields fieldTypes ->
                                fieldTypes
                                |> List.mapi (fun index fieldType -> (fieldType, TupleAccess (Var payloadName, index)))
                                |> fun items -> renderDelimited env items currentState
                            | _ ->
                                let (rendered, nextState) = renderCall env concreteType (Var payloadName) currentState
                                ([rendered], nextState)
                        let separated =
                            renderedFields
                            |> List.mapi (fun index rendered ->
                                if index = 0 then [rendered] else [StringLiteral ", "; rendered])
                            |> List.concat
                        let body =
                            concat (StringLiteral $"{typeText}.{variant.Name}(" :: separated @ [StringLiteral ")"])
                        let case = makeCase (PConstructor (variant.Name, Some (PVar payloadName))) body
                        buildCases rest nextState (case :: acc)
            let (cases, nextState) = buildCases (List.sortBy (fun variant -> variant.Tag) sumInfo.Variants) state []
            (Match (value, cases), nextState)
    | TFunction _ -> (StringLiteral "(lambda)", state)
    | TBlob ->
        // The interpreter deliberately does not expose ephemeral Blob payloads
        // or process-local identities through value rendering.
        (StringLiteral "<Blob: ephemeral>", state)
    | TRawPtr ->
        (call "Stdlib.Int64.toString" [value], state)
    | TDict (keyType, _) ->
        Crash.crash $"Public Dict renderer received non-String key type {TypeChecking.typeToString keyType}"
    | TRuntimeError -> (StringLiteral "()", state)
    | TVar name -> Crash.crash $"Unresolved type variable in value renderer: {name}"

let private sumRegistryFromVariants
    (variantLookup: Map<string, string * string list * int * Type option>)
    : Map<string, SumInfo> =
    variantLookup
    |> Map.toList
    |> List.map (fun (_, (typeName, typeParams, tag, payload)) ->
        (typeName, typeParams, { Name = ""; Tag = tag; Payload = payload }))
    |> List.groupBy (fun (typeName, _, _) -> typeName)
    |> List.map (fun (typeName, entries) ->
        let typeParams = entries |> List.head |> fun (_, parameters, _) -> parameters
        let variants =
            variantLookup
            |> Map.toList
            |> List.choose (fun (lookupName, (owner, _, tag, payload)) ->
                let qualifiedPrefix = $"{typeName}."
                if owner <> typeName || not (lookupName.StartsWith qualifiedPrefix) then None
                else
                    Some {
                        Name = lookupName.Substring(qualifiedPrefix.Length)
                        Tag = tag
                        Payload = payload
                    })
            |> List.distinctBy (fun variant -> variant.Tag)
        (typeName, { TypeParams = typeParams; Variants = variants }))
    |> Map.ofList

let rewriteProgram
    (baseRecords: Map<string, (string * Type) list>)
    (recordMetadata: TypeChecking.IndexedTypeRegistry)
    (baseVariants: Map<string, string * string list * int * Type option>)
    (baseFunctions: Map<string, Type>)
    (programType: Type)
    (Program topLevels)
    : Program =
    let localRecords =
        topLevels
        |> List.choose (function
            | TypeDef (RecordDef (name, typeParams, fields)) ->
                Some (name, { TypeParams = typeParams; Fields = firstDeclaredRecordFields fields })
            | _ -> None)
        |> Map.ofList
    let baseRecordInfo =
        baseRecords
        |> Map.map (fun name fields ->
            {
                TypeParams =
                    recordMetadata
                    |> Map.tryFind name
                    |> Option.map (fun info -> info.TypeParams)
                    |> Option.defaultValue []
                Fields = firstDeclaredRecordFields fields
            })
    let allCheckedRecordInfo =
        recordMetadata
        |> Map.map (fun _ (info: TypeChecking.RecordTypeInfo) ->
            ({ TypeParams = info.TypeParams; Fields = info.Fields }: RecordInfo))
    let records =
        baseRecordInfo
        |> Map.fold (fun acc name info -> Map.add name info acc) allCheckedRecordInfo
        |> fun checkedRecords ->
            Map.fold (fun acc name info -> Map.add name info acc) checkedRecords localRecords

    let baseSums = sumRegistryFromVariants baseVariants
    let localSums =
        topLevels
        |> List.choose (function
            | TypeDef (SumTypeDef (name, typeParams, variants)) ->
                Some (
                    name,
                    { TypeParams = typeParams
                      Variants =
                        variants
                        |> List.mapi (fun tag variant -> { Name = variant.Name; Tag = tag; Payload = variant.Payload }) }
                )
            | _ -> None)
        |> Map.ofList
    let sums = Map.fold (fun acc name info -> Map.add name info acc) baseSums localSums
    let namedFunctions =
        topLevels
        |> List.choose (function FunctionDef fn -> Some fn.Name | _ -> None)
        |> Set.ofList
        |> Set.union (baseFunctions |> Map.keys |> Set.ofSeq)
    let env = { Records = records; Sums = sums; NamedFunctions = namedFunctions }
    let (renderName, state) =
        match programType with
        | TDateTime -> (None, { Functions = Map.empty })
        | _ ->
            let (name, generatedState) = ensureRenderer env programType { Functions = Map.empty }
            (Some name, generatedState)

    let tryNamedPartialName expr =
        match expr with
        | Lambda (parameters, returnAnnotation, body) ->
            let parameterNames =
                parameters
                |> NonEmptyList.toList
                |> List.choose (fun parameter ->
                    match parameter.Pattern with
                    | LPVariable name -> Some name
                    | _ -> None)
            let generatedPartial =
                List.length parameterNames = NonEmptyList.length parameters
                && (parameterNames |> List.forall (fun name -> name.StartsWith "__partial_"))
            let callNameAndArgs =
                match body with
                | Call (name, callArgs) -> Some (name, NonEmptyList.toList callArgs)
                | TypeApp (name, _, callArgs) -> Some (name, NonEmptyList.toList callArgs)
                | _ -> None
            match generatedPartial, callNameAndArgs with
            | true, Some (name, callArgs) when List.length callArgs > List.length parameterNames ->
                let trailingArgs = callArgs |> List.skip (List.length callArgs - List.length parameterNames)
                if List.forall2 (fun arg parameterName -> arg = Var parameterName) trailingArgs parameterNames then
                    Some name
                else
                    None
            | _ -> None
        | _ -> None

    let rewriteExpression expr =
        let rendered =
            match programType, expr, tryNamedPartialName expr with
            | TDateTime, _, _ ->
                BoundaryRender ("Stdlib.DateTime.toString", expr)
            | TFunction _, _, Some name ->
                Let (LPVariable "__rendered_named_partial", expr, StringLiteral name)
            | TFunction _, Var name, _ when Set.contains name namedFunctions ->
                Let (LPVariable "__rendered_named_function", expr, StringLiteral name)
            | TFunction _, FuncRef name, _ ->
                Let (LPVariable "__rendered_named_function", expr, StringLiteral name)
            | TFunction _, Lambda _, _ ->
                Let (LPVariable "__rendered_lambda", expr, StringLiteral "(lambda)")
            | _ ->
                BoundaryRender (
                    renderName |> Option.defaultWith (fun () -> Crash.crash "Missing boundary value renderer"),
                    expr
                )
        Expression rendered

    let rewrittenTopLevels =
        topLevels
        |> List.map (function Expression expr -> rewriteExpression expr | other -> other)
    let generatedFunctions = state.Functions |> Map.toList |> List.map (snd >> FunctionDef)
    Program (generatedFunctions @ rewrittenTopLevels)
