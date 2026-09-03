// 1.7_JsonPlanning.fs - monomorphic, type-directed JSON conversion plans.
//
// Json.serialize and Json.parse are public generic intrinsics. This pass runs
// after type checking, when every explicit type argument is concrete, and
// replaces those calls with ordinary Dark functions. Backends therefore see
// only statically shaped values and use the normal retain/release machinery.

module JsonPlanning

open AST

type private SumVariant = { Name: string; Tag: int; Payload: Type option }
type private SumInfo = { TypeParams: string list; Variants: SumVariant list }

type private Env = {
    Records: TypeChecking.IndexedTypeRegistry
    Sums: Map<string, SumInfo>
    Aliases: TypeChecking.AliasRegistry
}

type private State = { Functions: Map<string, FunctionDef> }

let private args values = NonEmptyList.fromList values
let private call name values = Call (name, args values)
let private listPush elementType list value =
    TypeApp ("Stdlib.List.push", [elementType], args [list; value])

let private stableHash (value: string) : uint64 =
    value
    |> Seq.fold (fun hash ch -> (hash ^^^ uint64 (int ch)) * 1099511628211UL) 14695981039346656037UL

// Generated plan names must distinguish structurally different types whose
// public spelling is intentionally flattened (notably nested tuples).
let private structuralTypeKey (typ: Type) : string = $"{typ}"

let private serializeName typ =
    $"__dark_json_serialize_{stableHash (structuralTypeKey typ):x16}"

let private listName typ =
    $"__dark_json_serialize_list_{stableHash (structuralTypeKey typ):x16}"

let private dictName typ =
    $"__dark_json_serialize_dict_{stableHash (structuralTypeKey typ):x16}"

let private decoderName typ =
    $"__dark_json_decode_{stableHash (structuralTypeKey typ):x16}"

let private decodeListName typ =
    $"__dark_json_decode_list_{stableHash (structuralTypeKey typ):x16}"

let private decodeDictName typ =
    $"__dark_json_decode_dict_{stableHash (structuralTypeKey typ):x16}"

let private makeCase pattern body =
    { Patterns = NonEmptyList.singleton pattern; Guard = None; Body = body }

let private constructor owner caseName payload =
    Constructor (UnresolvedConstructor (Some owner), caseName, payload)

let private tuplePayload values = TupleLiteral values |> Some
let private ok value = constructor "Stdlib.Result.Result" "Ok" (Some value)
let private error value = constructor "Stdlib.Result.Result" "Error" (Some value)
let private none = constructor "Stdlib.Option.Option" "None" None
let private some value = constructor "Stdlib.Option.Option" "Some" (Some value)

let private jsonErrorType = TSum ("Stdlib.Json.ParseError.ParseError", [])
let private valueViewType = TInt64
let private pathPartType = TSum ("Stdlib.Json.ParseError.JsonPath.Part.Part", [])
let private pathType = TList pathPartType
let private resultType okType = TSum ("Stdlib.Result.Result", [okType; jsonErrorType])
let private writerType = TString

let private writerEmpty = call "Stdlib.Json.__writerEmpty" [UnitLiteral]
let private writerFinish writer = call "Stdlib.Json.__writerFinish" [writer]
let private writerRaw writer value = call "Stdlib.Json.__writerWriteRaw" [writer; value]
let private writerString writer value = call "Stdlib.Json.__writerWriteString" [writer; value]
let private writerBeginArray writer = call "Stdlib.Json.__writerBeginArray" [writer]
let private writerEndArray writer = call "Stdlib.Json.__writerEndArray" [writer]
let private writerBeginObject writer = call "Stdlib.Json.__writerBeginObject" [writer]
let private writerEndObject writer = call "Stdlib.Json.__writerEndObject" [writer]
let private writerSeparator writer = call "Stdlib.Json.__writerSeparator" [writer]
let private writerFieldName writer name = call "Stdlib.Json.__writerFieldName" [writer; name]

let private viewHeadName = "Stdlib.Json.__viewListHead"
let private viewTailName = "Stdlib.Json.__viewListTail"
let private viewIsEmptyName = "Stdlib.Json.__viewListIsEmpty"

let rec private typeReference typ =
    let owner = "Darklang.LanguageTools.RuntimeTypes.TypeReference"
    let nullary caseName = constructor owner caseName None
    let unary caseName value = constructor owner caseName (Some value)
    let custom (name: string) typeArgs =
        let originalName =
            name.Split('.') |> Array.toList |> List.map StringLiteral |> ListLiteral
        let hash = constructor "Darklang.LanguageTools.RuntimeTypes.Hash" "Hash" (Some (StringLiteral ""))
        let fqNameType = TSum ("Darklang.LanguageTools.RuntimeTypes.FQTypeName.FQTypeName", [])
        let fqName = constructor "Darklang.LanguageTools.RuntimeTypes.FQTypeName.FQTypeName" "Package" (Some hash)
        let resolved = ok fqName
        let resolution =
            RecordLiteral (
                unresolvedRecordReference
                    "Darklang.LanguageTools.RuntimeTypes.NameResolution"
                    [fqNameType],
                ["originalName", originalName; "resolved", resolved])
        constructor owner "TCustomType" (tuplePayload [resolution; ListLiteral (List.map typeReference typeArgs)])
    match typ with
    | TUnit -> nullary "TUnit"
    | TBool -> nullary "TBool"
    | TInt8 -> nullary "TInt8"
    | TUInt8 -> nullary "TUInt8"
    | TInt16 -> nullary "TInt16"
    | TUInt16 -> nullary "TUInt16"
    | TInt32 -> nullary "TInt32"
    | TUInt32 -> nullary "TUInt32"
    | TInt64 -> nullary "TInt64"
    | TUInt64 -> nullary "TUInt64"
    | TInt128 -> nullary "TInt128"
    | TUInt128 -> nullary "TUInt128"
    | TInt -> nullary "TInt"
    | TFloat64 -> nullary "TFloat"
    | TChar -> nullary "TChar"
    | TString -> nullary "TString"
    | TBlob -> nullary "TBlob"
    | TSum ("Uuid", []) -> nullary "TUuid"
    | TDateTime -> nullary "TDateTime"
    | TList elementType -> unary "TList" (typeReference elementType)
    | TDict (TString, valueType) -> unary "TDict" (typeReference valueType)
    | TTuple (first :: second :: rest) ->
        constructor owner "TTuple" (tuplePayload [typeReference first; typeReference second; ListLiteral (List.map typeReference rest)])
    | TFunction (parameters, result) ->
        constructor owner "TFn" (tuplePayload [ListLiteral (List.map typeReference parameters); typeReference result])
    | TStream elementType -> custom "Stdlib.Stream.Stream" [elementType]
    | TRecord (name, typeArgs)
    | TSum (name, typeArgs) -> custom name typeArgs
    | TVar name -> unary "TVariable" (StringLiteral name)
    | TTuple [] | TTuple [_] | TEnumFields _ | TRawPtr | TRuntimeError | TDict _ ->
        unary "TVariable" (StringLiteral (TypeChecking.typeToString typ))

let private cantMatch typ raw path =
    constructor
        "Stdlib.Json.ParseError.ParseError"
        "CantMatchWithType"
        (tuplePayload
            [ typeReference typ
              raw
              call "Stdlib.Json.ParseError.__copyPath" [path] ])
    |> error

let private rawSource source raw = call "Stdlib.Json.__copyRaw" [source; raw]

let private resultCases okName okBody errorName =
    [ makeCase (PConstructor ("Ok", Some (PVar okName))) okBody
      makeCase (PConstructor ("Error", Some (PVar errorName))) (error (Var errorName)) ]

let private applySubstitution subst typ =
    let rec apply typ =
        match typ with
        | TVar name -> Map.tryFind name subst |> Option.defaultValue typ
        | TList inner -> TList (apply inner)
        | TDict (keyType, valueType) -> TDict (apply keyType, apply valueType)
        | TTuple types -> TTuple (List.map apply types)
        | TEnumFields types -> TEnumFields (List.map apply types)
        | TFunction (parameters, result) -> TFunction (List.map apply parameters, apply result)
        | TRecord (name, typeArgs) -> TRecord (name, List.map apply typeArgs)
        | TSum (name, typeArgs) -> TSum (name, List.map apply typeArgs)
        | other -> other
    apply typ

let private substitution typeParams typeArgs =
    if List.length typeParams = List.length typeArgs then
        Ok (List.zip typeParams typeArgs |> Map.ofList)
    else
        Error
            $"JSON type argument mismatch: expected {List.length typeParams}, got {List.length typeArgs}"

let private resolveJsonType (env: Env) typ =
    // Preserve semantic aliases recursively before ordinary alias expansion.
    let rec resolve typ =
        let resolveNamed makeType name typeArgs =
            let resolvedArgs = List.map resolve typeArgs
            match name, resolvedArgs, Map.tryFind name env.Aliases with
            | ("Uuid" | "DateTime"), [], _ -> makeType name []
            | _, _, Some (typeParams, target) when List.length typeParams = List.length resolvedArgs ->
                applySubstitution (List.zip typeParams resolvedArgs |> Map.ofList) target |> resolve
            | _, _, None when Map.containsKey name env.Records -> TRecord (name, resolvedArgs)
            | _, _, None when Map.containsKey name env.Sums -> TSum (name, resolvedArgs)
            | _ -> makeType name resolvedArgs
        match typ with
        | TRecord (name, typeArgs) -> resolveNamed (fun n args -> TRecord (n, args)) name typeArgs
        | TSum (name, typeArgs) -> resolveNamed (fun n args -> TSum (n, args)) name typeArgs
        | TTuple types -> TTuple (List.map resolve types)
        | TEnumFields types -> TEnumFields (List.map resolve types)
        | TList inner -> TList (resolve inner)
        | TDict (keyType, valueType) -> TDict (resolve keyType, resolve valueType)
        | TFunction (parameters, result) -> TFunction (List.map resolve parameters, resolve result)
        | other -> other
    resolve typ

let rec private ensureSerializer (env: Env) typ state : Result<string * State, string> =
    let typ = resolveJsonType env typ
    let name = serializeName typ
    match Map.tryFind name state.Functions with
    | Some _ -> Ok (name, state)
    | None ->
        let placeholder = {
            Name = name
            TypeParams = []
            Params = args [("__writer", writerType); ("__value", typ)]
            ReturnType = writerType
            Body = Var "__writer"
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        serializeBody env typ (Var "__value") (Var "__writer") reserved
        |> Result.map (fun (body, nextState) ->
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private serializeCall env typ writer value state =
    ensureSerializer env typ state
    |> Result.map (fun (name, nextState) -> (call name [writer; value], nextState))

and private serializeItems env items writer state =
    let rec loop remaining currentWriter currentState =
        match remaining with
        | [] -> Ok (currentWriter, currentState)
        | (typ, value) :: rest ->
            serializeCall env typ currentWriter value currentState
            |> Result.bind (fun (nextWriter, nextState) -> loop rest nextWriter nextState)
    loop items writer state

and private ensureListSerializer env elemType state =
    let elemType = resolveJsonType env elemType
    let typ = TList elemType
    let name = listName typ
    match Map.tryFind name state.Functions with
    | Some _ -> Ok (name, state)
    | None ->
        let placeholder = {
            Name = name
            TypeParams = []
            Params =
                args
                    [("__items", typ)
                     ("__writer", writerType)
                     ("__first", TBool)]
            ReturnType = writerType
            Body = Var "__writer"
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let separated =
            If (Var "__first", Var "__writer", writerSeparator (Var "__writer"))
        serializeCall env elemType separated (Var "__head") reserved
        |> Result.map (fun (encoded, nextState) ->
            let body =
                Match (
                    Var "__items",
                    [ makeCase (PList []) (Var "__writer")
                      makeCase
                          (PListCons ([PVar "__head"], PVar "__tail"))
                          (call name [Var "__tail"; encoded; BoolLiteral false]) ])
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private ensureDictSerializer env valueType state =
    let valueType = resolveJsonType env valueType
    let dictType = TDict (TString, valueType)
    let entryType = TTuple [TString; valueType]
    let listType = TList entryType
    let name = dictName dictType
    match Map.tryFind name state.Functions with
    | Some _ -> Ok (name, state)
    | None ->
        let placeholder = {
            Name = name
            TypeParams = []
            Params =
                args
                    [("__entries", listType)
                     ("__writer", writerType)
                     ("__first", TBool)]
            ReturnType = writerType
            Body = Var "__writer"
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let separated =
            If (Var "__first", Var "__writer", writerSeparator (Var "__writer"))
        let withName = writerFieldName separated (TupleAccess (Var "__entry", 0))
        serializeCall env valueType withName (TupleAccess (Var "__entry", 1)) reserved
        |> Result.map (fun (encoded, nextState) ->
            let body =
                Match (
                    Var "__entries",
                    [ makeCase (PList []) (Var "__writer")
                      makeCase
                          (PListCons ([PVar "__entry"], PVar "__tail"))
                          (call name [Var "__tail"; encoded; BoolLiteral false]) ])
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private serializeBody env typ value writer state : Result<Expr * State, string> =
    match typ with
    | TUnit -> Ok (writerRaw writer (StringLiteral "null"), state)
    | TBool ->
        Ok (writerRaw writer (If (value, StringLiteral "true", StringLiteral "false")), state)
    | TInt8 -> Ok (writerRaw writer (call "Stdlib.Int8.toString" [value]), state)
    | TInt16 -> Ok (writerRaw writer (call "Stdlib.Int16.toString" [value]), state)
    | TInt32 -> Ok (writerRaw writer (call "Stdlib.Int32.toString" [value]), state)
    | TInt64 -> Ok (writerRaw writer (call "Stdlib.Int64.toString" [value]), state)
    | TInt -> Ok (writerRaw writer (call "Stdlib.Int.toString" [value]), state)
    | TUInt8 -> Ok (writerRaw writer (call "Stdlib.UInt8.toString" [value]), state)
    | TUInt16 -> Ok (writerRaw writer (call "Stdlib.UInt16.toString" [value]), state)
    | TUInt32 -> Ok (writerRaw writer (call "Stdlib.UInt32.toString" [value]), state)
    | TUInt64 -> Ok (writerRaw writer (call "Stdlib.UInt64.toString" [value]), state)
    // The native representation of 128-bit values is already canonical text.
    | TInt128 | TUInt128 -> Ok (writerRaw writer value, state)
    | TFloat64 -> Ok (writerRaw writer (call "Stdlib.Json.__serializeFloat" [value]), state)
    | TString | TChar -> Ok (writerString writer value, state)
    | TSum ("Uuid", []) -> Ok (writerString writer (call "Stdlib.Uuid.toString" [value]), state)
    | TDateTime ->
        Ok (writerString writer (call "Stdlib.DateTime.toString" [value]), state)
    | TTuple elementTypes ->
        elementTypes
        |> List.mapi (fun index elemType -> (elemType, TupleAccess (value, index)))
        |> List.mapi (fun index item -> (index, item))
        |> List.fold (fun result (index, item) ->
            result
            |> Result.bind (fun (currentWriter, currentState) ->
                let separated = if index = 0 then currentWriter else writerSeparator currentWriter
                serializeItems env [item] separated currentState))
            (Ok (writerBeginArray writer, state))
        |> Result.map (fun (encoded, nextState) -> (writerEndArray encoded, nextState))
    | TList elemType ->
        ensureListSerializer env elemType state
        |> Result.map (fun (name, nextState) ->
            let encoded = call name [value; writerBeginArray writer; BoolLiteral true]
            (writerEndArray encoded, nextState))
    | TDict (TString, valueType) ->
        ensureDictSerializer env valueType state
        |> Result.map (fun (name, nextState) ->
            let entries = TypeApp ("Stdlib.Dict.toList", [valueType], NonEmptyList.singleton value)
            let encoded = call name [Var "__entries"; writerBeginObject writer; BoolLiteral true]
            (Let (LPVariable "__entries", entries, writerEndObject encoded),
             nextState))
    | TRecord (typeName, typeArgs) ->
        match Map.tryFind typeName env.Records with
        | None -> Error $"Unsupported type in JSON: {TypeChecking.typeToString typ}"
        | Some recordInfo ->
            substitution recordInfo.TypeParams typeArgs
            |> Result.bind (fun subst ->
                let rec loop remaining index currentWriter currentState =
                    match remaining with
                    | [] -> Ok (currentWriter, currentState)
                    | (fieldName, fieldType) :: rest ->
                        let concrete = applySubstitution subst fieldType |> resolveJsonType env
                        let separated = if index = 0 then currentWriter else writerSeparator currentWriter
                        let named = writerFieldName separated (StringLiteral fieldName)
                        serializeCall env concrete named (RecordAccess (value, fieldName)) currentState
                        |> Result.bind (fun (encoded, nextState) ->
                            loop rest (index + 1) encoded nextState)
                recordInfo.Fields
                |> List.sortBy fst
                |> fun fields -> loop fields 0 (writerBeginObject writer) state
                |> Result.map (fun (encoded, nextState) ->
                    (writerEndObject encoded, nextState)))
    | TSum (typeName, typeArgs) ->
        match Map.tryFind typeName env.Sums with
        | None -> Error $"Unsupported type in JSON: {TypeChecking.typeToString typ}"
        | Some sumInfo ->
            substitution sumInfo.TypeParams typeArgs
            |> Result.bind (fun subst ->
                let rec loop remaining current acc =
                    match remaining with
                    | [] -> Ok (List.rev acc, current)
                    | variant :: rest ->
                        match variant.Payload with
                        | None ->
                            let body =
                                writer
                                |> writerBeginObject
                                |> fun current -> writerFieldName current (StringLiteral variant.Name)
                                |> writerBeginArray
                                |> writerEndArray
                                |> writerEndObject
                            loop rest current (makeCase (PConstructor (variant.Name, None)) body :: acc)
                        | Some payloadType ->
                            let concrete = applySubstitution subst payloadType |> resolveJsonType env
                            let payloadName = $"__payload_{variant.Tag}"
                            let fields =
                                match concrete with
                                | TEnumFields fieldTypes ->
                                    fieldTypes
                                    |> List.mapi (fun index fieldType ->
                                        (fieldType, TupleAccess (Var payloadName, index)))
                                | _ -> [(concrete, Var payloadName)]
                            let initialWriter =
                                writer
                                |> writerBeginObject
                                |> fun current -> writerFieldName current (StringLiteral variant.Name)
                                |> writerBeginArray
                            fields
                            |> List.mapi (fun index item -> (index, item))
                            |> List.fold (fun result (index, item) ->
                                result
                                |> Result.bind (fun (currentWriter, currentState) ->
                                    let separated = if index = 0 then currentWriter else writerSeparator currentWriter
                                    serializeItems env [item] separated currentState))
                                (Ok (initialWriter, current))
                            |> Result.bind (fun (encoded, next) ->
                                let body = encoded |> writerEndArray |> writerEndObject
                                loop rest next (makeCase (PConstructor (variant.Name, Some (PVar payloadName))) body :: acc))
                loop (List.sortBy (fun variant -> variant.Tag) sumInfo.Variants) state []
                |> Result.map (fun (cases, nextState) -> (Match (value, cases), nextState)))
    | TFunction _ | TBlob | TRawPtr | TRuntimeError | TStream _ | TVar _ | TEnumFields _
    | TDict _ ->
        Error
            $"Unsupported type in JSON: {TypeChecking.typeToString typ}. Some types are not supported in Json serialization"

let private optionDecoder typ functionName =
    let failure = cantMatch typ (rawSource (Var "__source") (Var "__view")) (Var "__path")
    Match (
        call functionName [Var "__source"; Var "__view"],
        [ makeCase (PConstructor ("Some", Some (PVar "__value"))) (ok (Var "__value"))
          makeCase (PConstructor ("None", None)) failure ])

let rec private ensureDecoder (env: Env) typ state : Result<string * State, string> =
    let typ = resolveJsonType env typ
    let name = decoderName typ
    match Map.tryFind name state.Functions with
    | Some _ -> Ok (name, state)
    | None ->
        let placeholder = {
            Name = name
            TypeParams = []
            Params = NonEmptyList.fromList ["__source", TString; "__view", valueViewType; "__path", pathType]
            ReturnType = resultType typ
            Body = RuntimeError "unfinished JSON decoder"
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        decodeBody env typ reserved
        |> Result.map (fun (body, nextState) ->
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private decodeCall env typ view path state =
    ensureDecoder env typ state
    |> Result.map (fun (name, nextState) -> (call name [Var "__source"; view; path], nextState))

and private sequenceDecoded env items build state =
    let rec loop remaining current bindings =
        match remaining with
        | [] -> Ok (build (List.rev bindings), current)
        | (typ, view, path, bindingName) :: rest ->
            decodeCall env typ view path current
            |> Result.bind (fun (decoded, next) ->
                loop rest next ((bindingName, typ) :: bindings)
                |> Result.map (fun (tail, finalState) ->
                    (Match (decoded, resultCases bindingName tail "__decode_error"), finalState)))
    loop items state []

and private ensureListDecoder env elemType state =
    let elemType = resolveJsonType env elemType
    let listType = TList elemType
    let name = decodeListName listType
    match Map.tryFind name state.Functions with
    | Some _ -> Ok (name, state)
    | None ->
        let placeholder = {
            Name = name
            TypeParams = []
            Params =
                NonEmptyList.fromList
                    ["__source", TString
                     "__array_view", valueViewType
                     "__next_index", TInt64
                     "__path", pathType
                     "__index", TInt64]
            ReturnType = resultType listType
            Body = RuntimeError "unfinished JSON list decoder"
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let itemPath =
            listPush
                pathPartType
                (Var "__path")
                (constructor "Stdlib.Json.ParseError.JsonPath.Part.Part" "Index" (Some (call "Stdlib.Int.fromInt64" [Var "__index"])))
        decodeCall env elemType (Var "__head") itemPath reserved
        |> Result.map (fun (decodedHead, nextState) ->
            let decodedTail =
                call
                    name
                    [ Var "__source"
                      Var "__array_view"
                      Var "__after_item"
                      Var "__path"
                      BinOp (Add, Var "__index", Int64Literal 1L) ]
            let invalid =
                cantMatch
                    listType
                    (rawSource (Var "__source") (Var "__array_view"))
                    (Var "__path")
            let body =
                Let (
                    LPVariable "__head",
                    call "Stdlib.Json.__arrayNext" [Var "__source"; Var "__array_view"; Var "__next_index"],
                    If (
                        BinOp (Eq, Var "__head", Int64Literal -1L),
                        ok (ListLiteral []),
                        If (
                            BinOp (Eq, Var "__head", Int64Literal -2L),
                            invalid,
                            Let (
                                LPVariable "__after_item",
                                call "Stdlib.Json.__arrayAfter" [Var "__source"; Var "__array_view"; Var "__head"],
                                If (
                                    BinOp (Lt, Var "__after_item", Int64Literal 0L),
                                    invalid,
                                    Match (
                                        decodedHead,
                                        resultCases
                                            "__decoded_head"
                                            (Match (
                                                decodedTail,
                                                resultCases
                                                    "__decoded_tail"
                                                    (ok (listPush elemType (Var "__decoded_tail") (Var "__decoded_head")))
                                                    "__tail_error"))
                                            "__head_error"))))))
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private ensureDictDecoder env valueType state =
    let valueType = resolveJsonType env valueType
    let dictType = TDict (TString, valueType)
    let name = decodeDictName dictType
    match Map.tryFind name state.Functions with
    | Some _ -> Ok (name, state)
    | None ->
        let viewFieldsType = TList (TTuple [TString; valueViewType])
        let placeholder = {
            Name = name
            TypeParams = []
            Params =
                NonEmptyList.fromList
                    ["__source", TString
                     "__fields", viewFieldsType
                     "__path", pathType
                     "__dict", dictType]
            ReturnType = resultType dictType
            Body = RuntimeError "unfinished JSON dictionary decoder"
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let key = call "Stdlib.Json.__viewFieldName" [Var "__entry"]
        let fieldView = call "Stdlib.Json.__viewFieldValue" [Var "__entry"]
        let fieldPath =
            listPush
                pathPartType
                (Var "__path")
                (constructor "Stdlib.Json.ParseError.JsonPath.Part.Part" "Field" (Some key))
        decodeCall env valueType fieldView fieldPath reserved
        |> Result.map (fun (decoded, nextState) ->
            let withValue =
                TypeApp (
                    "Stdlib.Dict.setOverridingDuplicates",
                    [valueType],
                    args [Var "__dict"; key; Var "__decoded_value"])
            let body =
                Match (
                    Var "__fields",
                    [ makeCase (PList []) (ok (Var "__dict"))
                      makeCase
                          (PListCons ([PVar "__entry"], PVar "__tail"))
                          (Match (
                              decoded,
                              resultCases
                                  "__decoded_value"
                                  (Match (
                                      call name [Var "__source"; Var "__tail"; Var "__path"; withValue],
                                      resultCases
                                          "__decoded_dict"
                                          (ok (Var "__decoded_dict"))
                                          "__dict_tail_error"))
                                  "__dict_error")) ])
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private decodeEnumCase env typ typeName subst variant state =
    let casePath =
        listPush
            pathPartType
            (Var "__path")
            (constructor
                "Stdlib.Json.ParseError.JsonPath.Part.Part"
                "Field"
                (Some (StringLiteral variant.Name)))
    let fieldTypes =
        match variant.Payload with
        | None -> []
        | Some payload ->
            match applySubstitution subst payload |> resolveJsonType env with
            | TEnumFields fields -> fields
            | field -> [field]
    let rawNames = fieldTypes |> List.mapi (fun index _ -> $"__enum_raw_{variant.Tag}_{index}")
    let valueNames = fieldTypes |> List.mapi (fun index _ -> $"__enum_value_{variant.Tag}_{index}")
    let decodedItems count =
        fieldTypes
        |> List.take count
        |> List.mapi (fun index fieldType ->
            let argumentPath =
                listPush
                    pathPartType
                    casePath
                    (constructor
                        "Stdlib.Json.ParseError.JsonPath.Part.Part"
                        "Index"
                        (Some (BigIntLiteral (bigint index))))
            (fieldType, Var rawNames[index], argumentPath, valueNames[index]))
    let constructed =
        let values = valueNames |> List.map Var
        let payload =
            match values with
            | [] -> None
            | [value] -> Some value
            | _ -> Some (TupleLiteral values)
        constructor typeName variant.Name payload |> ok
    let exactResult = sequenceDecoded env (decodedItems fieldTypes.Length) (fun _ -> constructed) state
    exactResult
    |> Result.bind (fun (exactBody, exactState) ->
        let rec missingCases count current acc =
            if count >= fieldTypes.Length then Ok (List.rev acc, current)
            else
                let missing =
                    constructor
                        "Stdlib.Json.ParseError.ParseError"
                        "EnumMissingField"
                        (tuplePayload
                            [typeReference fieldTypes[count]
                             BigIntLiteral (bigint count)
                             casePath])
                    |> error
                sequenceDecoded env (decodedItems count) (fun _ -> missing) current
                |> Result.bind (fun (body, next) ->
                    missingCases (count + 1) next (makeCase (PList (rawNames |> List.take count |> List.map PVar)) body :: acc))
        missingCases 0 exactState []
        |> Result.bind (fun (missing, missingState) ->
            let extraName = $"__enum_extra_{variant.Tag}"
            let extraPath =
                listPush
                    pathPartType
                    casePath
                    (constructor
                        "Stdlib.Json.ParseError.JsonPath.Part.Part"
                        "Index"
                        (Some (BigIntLiteral (bigint fieldTypes.Length))))
            let extra =
                constructor
                    "Stdlib.Json.ParseError.ParseError"
                    "EnumExtraField"
                    (tuplePayload [rawSource (Var "__source") (Var extraName); extraPath])
                |> error
            sequenceDecoded env (decodedItems fieldTypes.Length) (fun _ -> extra) missingState
            |> Result.map (fun (extraBody, finalState) ->
                let exact = makeCase (PList (List.map PVar rawNames)) exactBody
                let extraPattern =
                    PListCons (List.map PVar rawNames @ [PVar extraName], PWildcard)
                let arrayBody =
                    Match (
                        Var "__enum_args",
                        missing @ [exact; makeCase extraPattern extraBody])
                let body =
                    Match (
                        call "Stdlib.Json.__arrayItems" [Var "__source"; Var "__case_raw"],
                        [ makeCase
                              (PConstructor ("Some", Some (PVar "__enum_args")))
                              arrayBody
                          makeCase PWildcard (cantMatch typ (rawSource (Var "__source") (Var "__case_raw")) casePath) ])
                (body, finalState))))

and private decodeBody env typ state : Result<Expr * State, string> =
    let failure = cantMatch typ (rawSource (Var "__source") (Var "__view")) (Var "__path")
    match typ with
    | TUnit ->
        Ok (If (call "Stdlib.Json.__isNull" [Var "__source"; Var "__view"], ok UnitLiteral, failure), state)
    | TBool -> Ok (optionDecoder typ "Stdlib.Json.__boolValue", state)
    | TString -> Ok (optionDecoder typ "Stdlib.Json.__stringValue", state)
    | TChar -> Ok (optionDecoder typ "Stdlib.Json.__viewChar", state)
    | TInt8 -> Ok (optionDecoder typ "Stdlib.Json.__viewInt8", state)
    | TInt16 -> Ok (optionDecoder typ "Stdlib.Json.__viewInt16", state)
    | TInt32 -> Ok (optionDecoder typ "Stdlib.Json.__viewInt32", state)
    | TInt64 -> Ok (optionDecoder typ "Stdlib.Json.__viewInt64", state)
    | TInt128 -> Ok (optionDecoder typ "Stdlib.Json.__viewInt128", state)
    | TInt -> Ok (optionDecoder typ "Stdlib.Json.__viewInt", state)
    | TUInt8 -> Ok (optionDecoder typ "Stdlib.Json.__viewUInt8", state)
    | TUInt16 -> Ok (optionDecoder typ "Stdlib.Json.__viewUInt16", state)
    | TUInt32 -> Ok (optionDecoder typ "Stdlib.Json.__viewUInt32", state)
    | TUInt64 -> Ok (optionDecoder typ "Stdlib.Json.__viewUInt64", state)
    | TUInt128 -> Ok (optionDecoder typ "Stdlib.Json.__viewUInt128", state)
    | TFloat64 -> Ok (optionDecoder typ "Stdlib.Json.__viewFloat", state)
    | TSum ("Uuid", []) -> Ok (optionDecoder typ "Stdlib.Json.__viewUuid", state)
    | TDateTime -> Ok (optionDecoder typ "Stdlib.Json.__viewDateTime", state)
    | TList elemType ->
        ensureListDecoder env elemType state
        |> Result.map (fun (listDecoder, nextState) ->
            (Let (
                LPVariable "__array_start",
                call "Stdlib.Json.__arrayStart" [Var "__source"; Var "__view"],
                If (
                    BinOp (Lt, Var "__array_start", Int64Literal 0L),
                    failure,
                    call listDecoder [Var "__source"; Var "__view"; Var "__array_start"; Var "__path"; Int64Literal 0L])),
             nextState))
    | TTuple elementTypes ->
        let names = elementTypes |> List.mapi (fun index _ -> $"__tuple_raw_{index}")
        let patterns = names |> List.map PVar
        let items = elementTypes |> List.mapi (fun index elemType ->
            let path =
                listPush
                    pathPartType
                    (Var "__path")
                    (constructor "Stdlib.Json.ParseError.JsonPath.Part.Part" "Index" (Some (BigIntLiteral (bigint index))))
            (elemType, Var names[index], path, $"__tuple_value_{index}"))
        sequenceDecoded env items (fun bindings -> ok (TupleLiteral (bindings |> List.map (fst >> Var)))) state
        |> Result.map (fun (decoded, nextState) ->
            (Match (
                call "Stdlib.Json.__arrayItems" [Var "__source"; Var "__view"],
                [ makeCase (PConstructor ("Some", Some (PList patterns))) decoded
                  makeCase PWildcard failure ]),
             nextState))
    | TRecord (typeName, typeArgs) ->
        match Map.tryFind typeName env.Records with
        | None -> Error $"Unsupported type in JSON: {TypeChecking.typeToString typ}"
        | Some recordInfo ->
            substitution recordInfo.TypeParams typeArgs
            |> Result.bind (fun subst ->
                // Conversion checks required fields in declaration order; wire
                // serialization is independently ordinal-by-name.
                let fields = recordInfo.Fields
                let viewListType = TList valueViewType
                let rec build remaining current decodedFields =
                    match remaining with
                    | [] ->
                        Ok (
                            ok (
                                RecordLiteral (
                                    unresolvedRecordReference typeName typeArgs,
                                    List.rev decodedFields
                                )
                            ),
                            current
                        )
                    | (fieldName, fieldType) :: rest ->
                        let concrete = applySubstitution subst fieldType |> resolveJsonType env
                        let matches =
                            TypeApp (
                                "Stdlib.Dict.get",
                                [viewListType],
                                args [Var "__object_field_map"; StringLiteral fieldName])
                        let fieldPath =
                            listPush
                                pathPartType
                                (Var "__path")
                                (constructor "Stdlib.Json.ParseError.JsonPath.Part.Part" "Field" (Some (StringLiteral fieldName)))
                        decodeCall env concrete (Var "__field_raw") fieldPath current
                        |> Result.bind (fun (decoded, next) ->
                            build rest next ((fieldName, Var $"__field_{fieldName}") :: decodedFields)
                            |> Result.map (fun (tail, finalState) ->
                                let missing = constructor "Stdlib.Json.ParseError.ParseError" "RecordMissingField" (tuplePayload [StringLiteral fieldName; Var "__path"]) |> error
                                let duplicate = constructor "Stdlib.Json.ParseError.ParseError" "RecordDuplicateField" (tuplePayload [StringLiteral fieldName; Var "__path"]) |> error
                                let one = Match (decoded, resultCases $"__field_{fieldName}" tail "__field_error")
                                (Match (
                                    matches,
                                    [ makeCase (PConstructor ("None", None)) missing
                                      makeCase
                                        (PConstructor ("Some", Some (PVar "__matches")))
                                        (Let (
                                            LPVariable "__field_raw",
                                            call viewHeadName [Var "__matches"],
                                            Let (
                                                LPVariable "__field_rest",
                                                call viewTailName [Var "__matches"],
                                                If (
                                                    call viewIsEmptyName [Var "__field_rest"],
                                                    one,
                                                    duplicate)))) ]),
                                 finalState)))
                build fields state []
                |> Result.map (fun (decoded, nextState) ->
                    (Match (
                        call "Stdlib.Json.__objectFieldMap" [Var "__source"; Var "__view"],
                        [ makeCase (PConstructor ("Some", Some (PVar "__object_field_map"))) decoded
                          makeCase PWildcard failure ]),
                     nextState)))
    | TDict (TString, valueType) ->
        ensureDictDecoder env valueType state
        |> Result.map (fun (dictDecoder, nextState) ->
            let empty = DictLiteral (valueType, [])
            (Match (
                call "Stdlib.Json.__objectFields" [Var "__source"; Var "__view"],
                [ makeCase
                      (PConstructor ("Some", Some (PVar "__object_fields")))
                      (call dictDecoder [Var "__source"; Var "__object_fields"; Var "__path"; empty])
                  makeCase PWildcard failure ]),
             nextState))
    | TSum (typeName, typeArgs) ->
        match Map.tryFind typeName env.Sums with
        | None -> Error $"Unsupported type in JSON: {TypeChecking.typeToString typ}"
        | Some sumInfo ->
            substitution sumInfo.TypeParams typeArgs
            |> Result.bind (fun subst ->
                let rec buildCases remaining current acc =
                    match remaining with
                    | [] -> Ok (List.rev acc, current)
                    | variant :: rest ->
                        decodeEnumCase env typ typeName subst variant current
                        |> Result.bind (fun (body, next) ->
                            buildCases rest next (makeCase (PString variant.Name) body :: acc))
                buildCases (List.sortBy (fun variant -> variant.Tag) sumInfo.Variants) state []
                |> Result.map (fun (caseMatches, nextState) ->
                    let invalidCase =
                        constructor
                            "Stdlib.Json.ParseError.ParseError"
                            "EnumInvalidCasename"
                            (tuplePayload [typeReference typ; Var "__case_name"; Var "__path"])
                        |> error
                    let oneField =
                        Match (
                            Var "__case_name",
                            caseMatches @ [makeCase PWildcard invalidCase])
                    let tooMany =
                        constructor
                            "Stdlib.Json.ParseError.ParseError"
                            "EnumTooManyCases"
                            (tuplePayload [typeReference typ; Var "__case_names"; Var "__path"])
                        |> error
                    let checkedOneField = oneField
                    let objectBody =
                        Match (
                            call "Stdlib.Json.__enumCandidate" [Var "__source"; Var "__view"],
                            [ makeCase (PConstructor ("EnumNoFields", None)) failure
                              makeCase
                                  (PConstructor (
                                      "EnumOneField",
                                      Some (PTuple [PVar "__case_name"; PVar "__case_raw"])))
                                  checkedOneField
                              makeCase
                                  (PConstructor ("EnumManyFields", Some (PVar "__case_names")))
                                  tooMany
                              makeCase PWildcard failure ])
                    (objectBody, nextState)))
    | TFunction _ | TBlob | TRawPtr | TRuntimeError | TStream _ | TVar _ | TEnumFields _ | TDict _ ->
        Error $"Unsupported type in JSON: {TypeChecking.typeToString typ}. Some types are not supported in Json serialization"

let private sumRegistry (variantLookup: TypeChecking.VariantLookup) =
    variantLookup
    |> Map.toList
    |> List.map (fun (_, (typeName, typeParams, _, _)) -> (typeName, typeParams))
    |> List.distinct
    |> List.map (fun (typeName, typeParams) ->
        let variants =
            variantLookup
            |> Map.toList
            |> List.choose (fun (lookupName, (owner, _, tag, payload)) ->
                if owner <> typeName then None
                else
                    let prefix = $"{typeName}."
                    let caseName = if lookupName.StartsWith prefix then lookupName.Substring prefix.Length else lookupName
                    Some { Name = caseName; Tag = tag; Payload = payload })
            |> List.distinctBy (fun variant -> variant.Tag)
        (typeName, { TypeParams = typeParams; Variants = variants }))
    |> Map.ofList

let rec private mapExpr rewrite expr =
    let recurse = mapExpr rewrite
    let mapped =
        match expr with
        | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _
        | Int16Literal _ | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _
        | UInt64Literal _ | UInt128Literal _ | BoolLiteral _ | StringLiteral _ | CharLiteral _
        | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ -> expr
        | InterpolatedString parts ->
            InterpolatedString (parts |> List.map (function StringText text -> StringText text | StringExpr e -> StringExpr (recurse e)))
        | BinOp (op, left, right) -> BinOp (op, recurse left, recurse right)
        | UnaryOp (op, inner) -> UnaryOp (op, recurse inner)
        | Let (pattern, value, body) -> Let (pattern, recurse value, recurse body)
        | RecursiveLet (recursion, value, body) ->
            RecursiveLet (recursion, recurse value, recurse body)
        | If (condition, thenBranch, elseBranch) -> If (recurse condition, recurse thenBranch, recurse elseBranch)
        | Sequence (first, next) -> Sequence (recurse first, recurse next)
        | Call (name, values) -> Call (name, NonEmptyList.map recurse values)
        | TypeApp (name, types, values) -> TypeApp (name, types, NonEmptyList.map recurse values)
        | TupleLiteral values -> TupleLiteral (List.map recurse values)
        | TupleAccess (value, index) -> TupleAccess (recurse value, index)
        | DictLiteral (typ, entries) -> DictLiteral (typ, entries |> List.map (fun (key, value) -> (key, recurse value)))
        | RecordLiteral (name, fields) -> RecordLiteral (name, fields |> List.map (fun (field, value) -> (field, recurse value)))
        | RecordUpdate (record, fields) -> RecordUpdate (recurse record, fields |> List.map (fun (field, value) -> (field, recurse value)))
        | RecordAccess (record, field) -> RecordAccess (recurse record, field)
        | Constructor (reference, name, payload) -> Constructor (reference, name, Option.map recurse payload)
        | Match (value, cases) ->
            Match (recurse value, cases |> List.map (fun case -> { case with Guard = Option.map recurse case.Guard; Body = recurse case.Body }))
        | ListLiteral values -> ListLiteral (List.map recurse values)
        | Lambda (parameters, annotation, body) -> Lambda (parameters, annotation, recurse body)
        | Apply (fn, values) -> Apply (recurse fn, NonEmptyList.map recurse values)
        | IndirectApply (fn, values) -> IndirectApply (recurse fn, NonEmptyList.map recurse values)
        | Closure (name, captures) -> Closure (name, List.map recurse captures)
        | BoundaryRender (renderer, value) -> BoundaryRender (renderer, recurse value)
    rewrite mapped

let rewriteProgram
    (env: TypeChecking.TypeCheckEnv)
    (Program topLevels)
    : Program =
    let planningEnv = {
        Records = env.IndexedTypeReg
        Sums = sumRegistry env.VariantLookup
        Aliases = env.AliasReg
    }
    let (serializerTypes, parserTypes) =
        let collect expr acc =
            let initialResult = acc
            // mapExpr provides a compact complete traversal; the fold result is
            // threaded functionally through this local recursive collector.
            let rec walk current collected =
                let collected =
                    match current with
                    | TypeApp ("Stdlib.Json.serialize", [typ], _) -> (typ :: fst collected, snd collected)
                    | TypeApp ("Stdlib.Json.parse", [typ], _) -> (fst collected, typ :: snd collected)
                    | _ -> collected
                let capture child = walk child
                match current with
                | BinOp (_, a, b) | Sequence (a, b) -> capture b (capture a collected)
                | UnaryOp (_, a) | TupleAccess (a, _) | RecordAccess (a, _) | BoundaryRender (_, a) -> capture a collected
                | Let (_, a, b) | RecursiveLet (_, a, b) -> capture b (capture a collected)
                | If (a, b, c) -> capture c (capture b (capture a collected))
                | Call (_, values) | TypeApp (_, _, values) -> NonEmptyList.toList values |> List.fold (fun s e -> capture e s) collected
                | TupleLiteral values | ListLiteral values | Closure (_, values) -> List.fold (fun s e -> capture e s) collected values
                | DictLiteral (_, entries) -> entries |> List.fold (fun s (_, e) -> capture e s) collected
                | RecordLiteral (_, fields) -> fields |> List.fold (fun s (_, e) -> capture e s) collected
                | RecordUpdate (record, fields) -> fields |> List.fold (fun s (_, e) -> capture e s) (capture record collected)
                | Constructor (_, _, payload) -> payload |> Option.map (fun e -> capture e collected) |> Option.defaultValue collected
                | Match (value, cases) -> cases |> List.fold (fun s case -> capture case.Body (case.Guard |> Option.map (fun g -> capture g s) |> Option.defaultValue s)) (capture value collected)
                | Lambda (_, _, body) -> capture body collected
                | Apply (fn, values) | IndirectApply (fn, values) -> NonEmptyList.toList values |> List.fold (fun s e -> capture e s) (capture fn collected)
                | InterpolatedString parts -> parts |> List.fold (fun s part -> match part with StringText _ -> s | StringExpr e -> capture e s) collected
                | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _
                | Int16Literal _ | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _
                | UInt64Literal _ | UInt128Literal _ | BoolLiteral _ | StringLiteral _ | CharLiteral _
                | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ -> collected
            walk expr initialResult
        topLevels
        |> List.fold (fun acc topLevel ->
            match topLevel with
            | FunctionDef fn -> collect fn.Body acc
            | Expression expr -> collect expr acc
            | TypeDef _ -> acc) ([], [])
        |> fun (serializers, parsers) -> (List.distinct serializers, List.distinct parsers)

    let planned =
        let serializersPlanned =
            serializerTypes
            |> List.fold (fun result typ ->
                result
                |> Result.bind (fun state -> ensureSerializer planningEnv typ state |> Result.map snd))
                (Ok { Functions = Map.empty })
        parserTypes
        |> List.fold (fun result typ ->
            result
            |> Result.bind (fun state -> ensureDecoder planningEnv typ state |> Result.map snd)) serializersPlanned

    match planned with
    | Error error ->
        let rewrite expr =
            match expr with
            | TypeApp ("Stdlib.Json.serialize", _, _)
            | TypeApp ("Stdlib.Json.parse", _, _) -> RuntimeError error
            | _ -> expr
        Program (
            topLevels
            |> List.map (function
                | FunctionDef fn -> FunctionDef { fn with Body = mapExpr rewrite fn.Body }
                | Expression expr -> Expression (mapExpr rewrite expr)
                | other -> other))
    | Ok state ->
        let rewrite expr =
            match expr with
            | TypeApp ("Stdlib.Json.serialize", [typ], values) ->
                let written =
                    call
                        (serializeName (resolveJsonType planningEnv typ))
                        (writerEmpty :: NonEmptyList.toList values)
                writerFinish written
            | TypeApp ("Stdlib.Json.parse", [typ], values) ->
                let concrete = resolveJsonType planningEnv typ
                let source = NonEmptyList.head values
                let parsed = call "Stdlib.Json.__parseRoot" [Var "__json_source"]
                let rootPath = ListLiteral [constructor "Stdlib.Json.ParseError.JsonPath.Part.Part" "Root" None]
                Let (
                    LPVariable "__json_source",
                    source,
                    Let (
                        LPVariable "__json_parse_result",
                        parsed,
                        Match (
                            Var "__json_parse_result",
                            [ makeCase
                                  (PConstructor ("Ok", Some (PVar "__json_view")))
                                  (call (decoderName concrete) [Var "__json_source"; Var "__json_view"; rootPath])
                              makeCase
                                  (PConstructor ("Error", Some PWildcard))
                                  (constructor "Stdlib.Json.ParseError.ParseError" "NotJson" None |> error) ])))
            | _ -> expr
        let rewritten =
            topLevels
            |> List.map (function
                | FunctionDef fn -> FunctionDef { fn with Body = mapExpr rewrite fn.Body }
                | Expression expr -> Expression (mapExpr rewrite expr)
                | other -> other)
        let generated = state.Functions |> Map.toList |> List.map (snd >> FunctionDef)
        Program (generated @ rewritten)
