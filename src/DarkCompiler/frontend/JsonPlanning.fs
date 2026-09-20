// JsonPlanning.fs - monomorphic, type-directed JSON conversion plans.
//
// Json.serialize and Json.parse are public generic intrinsics. This pass runs
// after type checking, when every explicit type argument is concrete, and
// replaces those calls with ordinary Dark functions. Backends therefore see
// only statically shaped values and use the normal retain/release machinery.

module JsonPlanning

open AST
open CheckedAST
open System.Collections.Generic

/// Bounded, caller-owned cache of generated typed JSON codec declarations.
/// Entries are keyed by direction plus the complete reachable shape of the
/// resolved root type, so unrelated declarations do not prevent reuse while
/// same-named local declarations with different shapes remain isolated.
type PlanningSession() =
    let artifacts = Dictionary<string, FunctionDef list>()
    let mutable disposed = false
    let mutable hitCount = 0
    let mutable missCount = 0

    member _.TryFind(key: string) : FunctionDef list option =
        if disposed then
            None
        else
            match artifacts.TryGetValue key with
            | true, functions ->
                hitCount <- hitCount + 1
                Some functions
            | false, _ ->
                missCount <- missCount + 1
                None

    member _.Store(key: string, functions: FunctionDef list) : unit =
        if not disposed then
            artifacts.[key] <- functions

    member _.Count = if disposed then 0 else artifacts.Count
    member _.HitCount = hitCount
    member _.MissCount = missCount

    interface System.IDisposable with
        member _.Dispose() =
            artifacts.Clear()
            disposed <- true

type private SumVariant = CheckingTypes.SumVariantInfo
type private SumInfo = CheckingTypes.SumTypeInfo

type private Env = {
    Records: CheckingTypes.IndexedTypeRegistry
    Sums: Map<string, SumInfo>
    Aliases: CheckingTypes.AliasRegistry
    Symbols: Symbols
}

type private State = {
    Functions: Map<string, FunctionDef>
    Symbols: Symbols
}

let private freshBinding name state =
    let (id, symbols) = allocateBinding name state.Symbols
    (id, { state with Symbols = symbols })

let private reserveFunction name state =
    let (id, symbols) = internFunction name state.Symbols
    (id, { state with Symbols = symbols })

let private freshBindings names state =
    names
    |> List.mapFold (fun current name ->
        let (id, next) = freshBinding name current
        ((name, id), next)) state
    |> fun (bindings, next) -> (Map.ofList bindings, next)

let private local name bindings =
    match Map.tryFind name bindings with
    | Some id -> Local id
    | None -> Crash.crash $"Generated JSON binding was not allocated: {name}"

let private patternLocal name bindings =
    match Map.tryFind name bindings with
    | Some id -> PVariable id
    | None -> Crash.crash $"Generated JSON pattern binding was not allocated: {name}"

let private args values = NonEmptyList.fromList values
let private resolveFunction (env: Env) name =
    tryFindFunctionId name env.Symbols
    |> Option.defaultWith (fun () -> Crash.crash $"Generated JSON function was not interned: {name}")
let private call env name values = Call (resolveFunction env name, args values)
let private listPush env elementType list value =
    TypeApp (resolveFunction env "Darklang.Stdlib.List.push", [elementType], args [list; value])

let private stableHash (value: string) : uint64 =
    value
    |> Seq.fold (fun hash ch -> (hash ^^^ uint64 (int ch)) * 1099511628211UL) 14695981039346656037UL

// Generated plan names must distinguish structurally different types whose
// public spelling is intentionally flattened (notably nested tuples). Encode
// the union directly: F#'s default union formatting uses reflection, which is
// disproportionately expensive when the same primitive codecs are requested
// by many separate compilations.
let rec private structuralTypeKey (typ: Type) : string =
    let encodeText tag (value: string) = $"{tag}{value.Length}:{value}"
    let encodeTypes tag types =
        let encoded =
            types
            |> List.map structuralTypeKey
            |> List.map (fun value -> $"{value.Length}:{value}")
            |> String.concat ""
        $"{tag}{List.length types}:{encoded}"
    match typ with
    | TInt8 -> "i8"
    | TInt16 -> "i16"
    | TInt32 -> "i32"
    | TInt64 -> "i64"
    | TInt128 -> "i128"
    | TInt -> "int"
    | TUInt8 -> "u8"
    | TUInt16 -> "u16"
    | TUInt32 -> "u32"
    | TUInt64 -> "u64"
    | TUInt128 -> "u128"
    | TBool -> "bool"
    | TFloat64 -> "float64"
    | TString -> "string"
    | TBlob -> "blob"
    | TChar -> "char"
    | TDateTime -> "datetime"
    | TUnit -> "unit"
    | TRuntimeError -> "runtime-error"
    | TRawPtr -> "raw-ptr"
    | TVar name -> encodeText "var" name
    | TList elementType -> encodeTypes "list" [elementType]
    | TStream elementType -> encodeTypes "stream" [elementType]
    | TDict (keyType, valueType) -> encodeTypes "dict" [keyType; valueType]
    | TTuple elementTypes -> encodeTypes "tuple" elementTypes
    | TRecord (name, typeArgs) ->
        let encodedName = encodeText "record" name
        let encodedArgs = encodeTypes "args" typeArgs
        $"{encodedName}{encodedArgs}"
    | TSum (name, typeArgs) ->
        let encodedName = encodeText "sum" name
        let encodedArgs = encodeTypes "args" typeArgs
        $"{encodedName}{encodedArgs}"
    | TFunction (paramTypes, returnType) ->
        let encodedParams = encodeTypes "function" paramTypes
        let encodedReturn = encodeTypes "returns" [returnType]
        $"{encodedParams}{encodedReturn}"

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

let private typeId (env: Env) name =
    tryFindTypeId name env.Symbols
    |> Option.defaultWith (fun () -> Crash.crash $"Generated JSON type was not interned: {name}")

let private constructor (env: Env) owner caseName payload =
    match tryFindConstructorId owner caseName env.Symbols with
    | Some id ->
        Constructor ({ TypeId = typeId env owner; ConstructorId = id }, Option.toList payload)
    | None -> Crash.crash $"JSON constructor was not interned: {owner}.{caseName}"

let private constructorPattern (env: Env) owner caseName fields =
    match tryFindConstructorId owner caseName env.Symbols with
    | Some id -> PConstructor (id, fields)
    | None -> Crash.crash $"JSON constructor pattern was not interned: {owner}.{caseName}"

let private fieldId (env: Env) owner fieldName =
    match tryFindFieldId owner fieldName env.Symbols with
    | Some id -> id
    | None -> Crash.crash $"Generated JSON record field was not interned: {owner}.{fieldName}"

let private tuplePayload values = TupleLiteral values |> Some
let private ok env value = constructor env "Darklang.Stdlib.Result.Result" "Ok" (Some value)
let private error env value = constructor env "Darklang.Stdlib.Result.Result" "Error" (Some value)
let private none env = constructor env "Darklang.Stdlib.Option.Option" "None" None
let private some env value = constructor env "Darklang.Stdlib.Option.Option" "Some" (Some value)

let private jsonErrorType = TSum ("Darklang.Stdlib.Json.ParseError.ParseError", [])
let private valueViewType = TInt64
let private pathPartType = TSum ("Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part", [])
let private pathType = TList pathPartType
let private resultType okType = TSum ("Darklang.Stdlib.Result.Result", [okType; jsonErrorType])
let private writerType = TString

let private writerEmpty env = call env "Darklang.Stdlib.Json.__writerEmpty" [UnitLiteral]
let private writerFinish env writer = call env "Darklang.Stdlib.Json.__writerFinish" [writer]
let private writerRaw env writer value = call env "Darklang.Stdlib.Json.__writerWriteRaw" [writer; value]
let private writerString env writer value = call env "Darklang.Stdlib.Json.__writerWriteString" [writer; value]
let private writerBeginArray env writer = call env "Darklang.Stdlib.Json.__writerBeginArray" [writer]
let private writerEndArray env writer = call env "Darklang.Stdlib.Json.__writerEndArray" [writer]
let private writerBeginObject env writer = call env "Darklang.Stdlib.Json.__writerBeginObject" [writer]
let private writerEndObject env writer = call env "Darklang.Stdlib.Json.__writerEndObject" [writer]
let private writerSeparator env writer = call env "Darklang.Stdlib.Json.__writerSeparator" [writer]
let private writerFieldName env writer name = call env "Darklang.Stdlib.Json.__writerFieldName" [writer; name]

let rec private typeReference env typ =
    let owner = "Darklang.LanguageTools.RuntimeTypes.TypeReference"
    let nullary caseName = constructor env owner caseName None
    let unary caseName value = constructor env owner caseName (Some value)
    let custom (name: string) typeArgs =
        let originalName =
            name.Split('.') |> Array.toList |> List.map StringLiteral |> ListLiteral
        let hash = constructor env "Darklang.LanguageTools.RuntimeTypes.Hash" "Hash" (Some (StringLiteral ""))
        let fqNameType = TSum ("Darklang.LanguageTools.RuntimeTypes.FQTypeName.FQTypeName", [])
        let fqName = constructor env "Darklang.LanguageTools.RuntimeTypes.FQTypeName.FQTypeName" "Package" (Some hash)
        let resolved = ok env fqName
        let resolution =
            RecordLiteral (
                {
                    TypeId = typeId env "Darklang.LanguageTools.RuntimeTypes.NameResolution"
                    TypeArgs = [fqNameType]
                },
                [ fieldId env "Darklang.LanguageTools.RuntimeTypes.NameResolution" "originalName", originalName
                  fieldId env "Darklang.LanguageTools.RuntimeTypes.NameResolution" "resolved", resolved ])
        constructor env owner "TCustomType" (tuplePayload [resolution; ListLiteral (List.map (typeReference env) typeArgs)])
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
    | TList elementType -> unary "TList" (typeReference env elementType)
    | TDict (TString, valueType) -> unary "TDict" (typeReference env valueType)
    | TTuple (first :: second :: rest) ->
        constructor env owner "TTuple" (tuplePayload [typeReference env first; typeReference env second; ListLiteral (List.map (typeReference env) rest)])
    | TFunction (parameters, result) ->
        constructor env owner "TFn" (tuplePayload [ListLiteral (List.map (typeReference env) parameters); typeReference env result])
    | TStream elementType -> custom "Darklang.Stdlib.Stream.Stream" [elementType]
    | TRecord (name, typeArgs)
    | TSum (name, typeArgs) -> custom name typeArgs
    | TVar name -> unary "TVariable" (StringLiteral name)
    | TTuple [] | TTuple [_] | TRawPtr | TRuntimeError | TDict _ ->
        unary "TVariable" (StringLiteral (CheckingDiagnostics.typeToString typ))

let private cantMatch env typ raw path =
    constructor env
        "Darklang.Stdlib.Json.ParseError.ParseError"
        "CantMatchWithType"
        (tuplePayload
            [ typeReference env typ
              raw
              call env "Darklang.Stdlib.Json.ParseError.__copyPath" [path] ])
    |> error env

let private rawSource env source raw = call env "Darklang.Stdlib.Json.__copyRaw" [source; raw]

let private resultCases env okId okBody errorId =
    [ makeCase (constructorPattern env "Darklang.Stdlib.Result.Result" "Ok" [PVariable okId]) okBody
      makeCase
          (constructorPattern env "Darklang.Stdlib.Result.Result" "Error" [PVariable errorId])
          (error env (Local errorId)) ]

let private applySubstitution subst typ =
    let rec apply typ =
        match typ with
        | TVar name -> Map.tryFind name subst |> Option.defaultValue typ
        | TList inner -> TList (apply inner)
        | TDict (keyType, valueType) -> TDict (apply keyType, apply valueType)
        | TTuple types -> TTuple (List.map apply types)
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
        | TList inner -> TList (resolve inner)
        | TDict (keyType, valueType) -> TDict (resolve keyType, resolve valueType)
        | TFunction (parameters, result) -> TFunction (List.map resolve parameters, resolve result)
        | other -> other
    resolve typ

let private canonicalCodecTypeKey (env: Env) (rootType: Type) : string =
    // Include only declarations reachable from the requested type. This is
    // deliberately narrower than fingerprinting the complete type-checking
    // environment: most E2E files add unrelated declarations, and those must
    // not defeat reuse of primitive and standard-library codecs.
    let rec encode (visiting: Set<string>) typ =
        let typ = resolveJsonType env typ
        match typ with
        | TList elementType -> $"list({encode visiting elementType})"
        | TDict (keyType, valueType) ->
            $"dict({encode visiting keyType},{encode visiting valueType})"
        | TTuple elementTypes ->
            elementTypes
            |> List.map (encode visiting)
            |> String.concat ","
            |> fun elements -> $"tuple({elements})"
        | TFunction (parameters, result) ->
            let parameters = parameters |> List.map (encode visiting) |> String.concat ","
            $"fn({parameters})->{encode visiting result}"
        | TRecord (name, typeArgs) ->
            let identity = $"record:{structuralTypeKey typ}"
            if Set.contains identity visiting then
                $"ref({identity})"
            else
                let visiting = Set.add identity visiting
                let args = typeArgs |> List.map (encode visiting) |> String.concat ","
                match Map.tryFind name env.Records with
                | None -> $"{identity}<{args}>"
                | Some info ->
                    match substitution info.TypeParams typeArgs with
                    | Error _ -> $"{identity}<{args}>:invalid-arity"
                    | Ok subst ->
                        let fields =
                            info.Fields
                            |> List.map (fun (fieldName, fieldType) ->
                                let concrete = applySubstitution subst fieldType
                                $"{fieldName}:{encode visiting concrete}")
                            |> String.concat ","
                        $"{identity}<{args}>{{{fields}}}"
        | TSum (name, typeArgs) ->
            let identity = $"sum:{structuralTypeKey typ}"
            if Set.contains identity visiting then
                $"ref({identity})"
            else
                let visiting = Set.add identity visiting
                let args = typeArgs |> List.map (encode visiting) |> String.concat ","
                match Map.tryFind name env.Sums with
                | None -> $"{identity}<{args}>"
                | Some info ->
                    match substitution info.TypeParams typeArgs with
                    | Error _ -> $"{identity}<{args}>:invalid-arity"
                    | Ok subst ->
                        let variants =
                            info.Variants
                            |> List.sortBy (fun variant -> variant.Tag)
                            |> List.map (fun variant ->
                                let fields =
                                    variant.Fields
                                    |> List.map (applySubstitution subst >> encode visiting)
                                    |> String.concat ","
                                $"{variant.Tag}:{variant.Name}:[{fields}]")
                            |> String.concat ","
                        $"{identity}<{args}>[{variants}]"
        | other -> structuralTypeKey other

    encode Set.empty rootType

let rec private ensureSerializer (env: Env) typ state : Result<string * State, string> =
    let typ = resolveJsonType env typ
    let name = serializeName typ
    match Map.tryFind name state.Functions with
    | Some _ -> Ok (name, state)
    | None ->
        let (functionId, state) = reserveFunction name state
        let (bindings, state) = freshBindings ["__writer"; "__value"] state
        let env = { env with Symbols = state.Symbols }
        let writerId =
            match Map.tryFind "__writer" bindings with
            | Some id -> id
            | None -> Crash.crash "JSON serializer writer binding was not allocated"
        let valueId =
            match Map.tryFind "__value" bindings with
            | Some id -> id
            | None -> Crash.crash "JSON serializer value binding was not allocated"
        let placeholder = {
            Id = functionId
            Name = name
            TypeParams = []
            Params = args [(writerId, writerType); (valueId, typ)]
            ReturnType = writerType
            Body = Local writerId
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        serializeBody env typ (Local valueId) (Local writerId) reserved
        |> Result.map (fun (body, nextState) ->
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private serializeCall env typ writer value state =
    ensureSerializer env typ state
    |> Result.map (fun (name, nextState) ->
        let currentEnv = { env with Symbols = nextState.Symbols }
        (call currentEnv name [writer; value], nextState))

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
        let (functionId, state) = reserveFunction name state
        let (bindings, state) =
            freshBindings ["__items"; "__writer"; "__first"; "__head"; "__tail"] state
        let env = { env with Symbols = state.Symbols }
        let binding name =
            match Map.tryFind name bindings with
            | Some id -> id
            | None -> Crash.crash $"JSON list serializer binding was not allocated: {name}"
        let placeholder = {
            Id = functionId
            Name = name
            TypeParams = []
            Params =
                args
                    [(binding "__items", typ)
                     (binding "__writer", writerType)
                     (binding "__first", TBool)]
            ReturnType = writerType
            Body = local "__writer" bindings
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let separated =
            If (local "__first" bindings, local "__writer" bindings, writerSeparator env (local "__writer" bindings))
        serializeCall env elemType separated (local "__head" bindings) reserved
        |> Result.map (fun (encoded, nextState) ->
            let body =
                Match (
                    local "__items" bindings,
                    [ makeCase (PList []) (local "__writer" bindings)
                      makeCase
                          (PListCons ([patternLocal "__head" bindings], patternLocal "__tail" bindings))
                          (call env name [local "__tail" bindings; encoded; BoolLiteral false]) ])
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
        let (functionId, state) = reserveFunction name state
        let (bindings, state) =
            freshBindings ["__entries"; "__writer"; "__first"; "__entry"; "__tail"] state
        let env = { env with Symbols = state.Symbols }
        let binding name =
            match Map.tryFind name bindings with
            | Some id -> id
            | None -> Crash.crash $"JSON dictionary serializer binding was not allocated: {name}"
        let placeholder = {
            Id = functionId
            Name = name
            TypeParams = []
            Params =
                args
                    [(binding "__entries", listType)
                     (binding "__writer", writerType)
                     (binding "__first", TBool)]
            ReturnType = writerType
            Body = local "__writer" bindings
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let separated =
            If (local "__first" bindings, local "__writer" bindings, writerSeparator env (local "__writer" bindings))
        let withName = writerFieldName env separated (TupleAccess (local "__entry" bindings, 0))
        serializeCall env valueType withName (TupleAccess (local "__entry" bindings, 1)) reserved
        |> Result.map (fun (encoded, nextState) ->
            let body =
                Match (
                    local "__entries" bindings,
                    [ makeCase (PList []) (local "__writer" bindings)
                      makeCase
                          (PListCons ([patternLocal "__entry" bindings], patternLocal "__tail" bindings))
                          (call env name [local "__tail" bindings; encoded; BoolLiteral false]) ])
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private serializeBody env typ value writer state : Result<Expr * State, string> =
    match typ with
    | TUnit -> Ok (writerRaw env writer (StringLiteral "null"), state)
    | TBool ->
        Ok (writerRaw env writer (If (value, StringLiteral "true", StringLiteral "false")), state)
    | TInt8 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.Int8.toString" [value]), state)
    | TInt16 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.Int16.toString" [value]), state)
    | TInt32 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.Int32.toString" [value]), state)
    | TInt64 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.Int64.toString" [value]), state)
    | TInt -> Ok (writerRaw env writer (call env "Darklang.Stdlib.Int.toString" [value]), state)
    | TUInt8 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.UInt8.toString" [value]), state)
    | TUInt16 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.UInt16.toString" [value]), state)
    | TUInt32 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.UInt32.toString" [value]), state)
    | TUInt64 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.UInt64.toString" [value]), state)
    | TInt128 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.Int128.toString" [value]), state)
    | TUInt128 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.UInt128.toString" [value]), state)
    | TFloat64 -> Ok (writerRaw env writer (call env "Darklang.Stdlib.Json.__serializeFloat" [value]), state)
    | TString | TChar -> Ok (writerString env writer value, state)
    | TSum ("Uuid", []) -> Ok (writerString env writer (call env "Darklang.Stdlib.Uuid.toString" [value]), state)
    | TDateTime ->
        Ok (writerString env writer (call env "Darklang.Stdlib.DateTime.toString" [value]), state)
    | TTuple elementTypes ->
        elementTypes
        |> List.mapi (fun index elemType -> (elemType, TupleAccess (value, index)))
        |> List.mapi (fun index item -> (index, item))
        |> List.fold (fun result (index, item) ->
            result
            |> Result.bind (fun (currentWriter, currentState) ->
                let separated = if index = 0 then currentWriter else writerSeparator env currentWriter
                serializeItems env [item] separated currentState))
            (Ok (writerBeginArray env writer, state))
        |> Result.map (fun (encoded, nextState) -> (writerEndArray env encoded, nextState))
    | TList elemType ->
        ensureListSerializer env elemType state
        |> Result.map (fun (name, nextState) ->
            let currentEnv = { env with Symbols = nextState.Symbols }
            let encoded = call currentEnv name [value; writerBeginArray currentEnv writer; BoolLiteral true]
            (writerEndArray currentEnv encoded, nextState))
    | TDict (TString, valueType) ->
        ensureDictSerializer env valueType state
        |> Result.map (fun (name, nextState) ->
            let (entriesId, nextState) = freshBinding "__entries" nextState
            let currentEnv = { env with Symbols = nextState.Symbols }
            let entries =
                TypeApp (
                    resolveFunction currentEnv "Darklang.Stdlib.Dict.toList",
                    [TString; valueType],
                    NonEmptyList.singleton value
                )
            let encoded = call currentEnv name [Local entriesId; writerBeginObject currentEnv writer; BoolLiteral true]
            (Let (LPVariable entriesId, entries, writerEndObject currentEnv encoded),
             nextState))
    | TRecord (typeName, typeArgs) ->
        match Map.tryFind typeName env.Records with
        | None -> Error $"Unsupported type in JSON: {CheckingDiagnostics.typeToString typ}"
        | Some recordInfo ->
            substitution recordInfo.TypeParams typeArgs
            |> Result.bind (fun subst ->
                let rec loop remaining index currentWriter currentState =
                    match remaining with
                    | [] -> Ok (currentWriter, currentState)
                    | (fieldIndex, fieldName, fieldType) :: rest ->
                        let concrete = applySubstitution subst fieldType |> resolveJsonType env
                        let separated = if index = 0 then currentWriter else writerSeparator env currentWriter
                        let named = writerFieldName env separated (StringLiteral fieldName)
                        let (fieldId, symbols) =
                            CheckedAST.internField typeName fieldName fieldIndex currentState.Symbols
                        serializeCall
                            env
                            concrete
                            named
                            (RecordAccess (value, fieldId))
                            { currentState with Symbols = symbols }
                        |> Result.bind (fun (encoded, nextState) ->
                            loop rest (index + 1) encoded nextState)
                recordInfo.Fields
                |> List.mapi (fun index (name, typ) -> (index, name, typ))
                |> List.sortBy (fun (_, name, _) -> name)
                |> fun fields -> loop fields 0 (writerBeginObject env writer) state
                |> Result.map (fun (encoded, nextState) ->
                    (writerEndObject env encoded, nextState)))
    | TSum (typeName, typeArgs) ->
        match Map.tryFind typeName env.Sums with
        | None -> Error $"Unsupported type in JSON: {CheckingDiagnostics.typeToString typ}"
        | Some sumInfo ->
            substitution sumInfo.TypeParams typeArgs
            |> Result.bind (fun subst ->
                let rec loop (remaining: SumVariant list) current acc =
                    match remaining with
                    | [] -> Ok (List.rev acc, current)
                    | variant :: rest ->
                        match variant.Fields with
                        | [] ->
                            let body =
                                writer
                                |> writerBeginObject env
                                |> fun current -> writerFieldName env current (StringLiteral variant.Name)
                                |> writerBeginArray env
                                |> writerEndArray env
                                |> writerEndObject env
                            loop rest current
                                (makeCase (constructorPattern env typeName variant.Name []) body :: acc)
                        | fieldTypes ->
                            let concreteFields = fieldTypes |> List.map (applySubstitution subst >> resolveJsonType env)
                            let fieldNames = fieldTypes |> List.mapi (fun index _ -> $"__field_{variant.Tag}_{index}")
                            let (fieldIds, current) =
                                fieldNames
                                |> List.mapFold (fun state fieldName -> freshBinding fieldName state) current
                            let fields = List.zip concreteFields (List.map Local fieldIds)
                            let initialWriter =
                                writer
                                |> writerBeginObject env
                                |> fun current -> writerFieldName env current (StringLiteral variant.Name)
                                |> writerBeginArray env
                            fields
                            |> List.mapi (fun index item -> (index, item))
                            |> List.fold (fun result (index, item) ->
                                result
                                |> Result.bind (fun (currentWriter, currentState) ->
                                    let separated = if index = 0 then currentWriter else writerSeparator env currentWriter
                                    serializeItems env [item] separated currentState))
                                (Ok (initialWriter, current))
                            |> Result.bind (fun (encoded, next) ->
                                let body = encoded |> writerEndArray env |> writerEndObject env
                                loop rest next
                                    (makeCase
                                        (constructorPattern env typeName variant.Name (List.map PVariable fieldIds))
                                        body
                                     :: acc))
                loop (List.sortBy (fun variant -> variant.Tag) sumInfo.Variants) state []
                |> Result.map (fun (cases, nextState) -> (Match (value, cases), nextState)))
    | TFunction _ | TBlob | TRawPtr | TRuntimeError | TStream _ | TVar _
    | TDict _ ->
        Error
            $"Unsupported type in JSON: {CheckingDiagnostics.typeToString typ}. Some types are not supported in Json serialization"

let private optionDecoder env typ functionName source view path state =
    let (valueId, state) = freshBinding "__value" state
    let failure = cantMatch env typ (rawSource env source view) path
    (Match (
        call env functionName [source; view],
        [ makeCase
              (constructorPattern env "Darklang.Stdlib.Option.Option" "Some" [PVariable valueId])
              (ok env (Local valueId))
          makeCase
              (constructorPattern env "Darklang.Stdlib.Option.Option" "None" [])
              failure ]),
     state)

let rec private ensureDecoder (env: Env) typ state : Result<string * State, string> =
    let typ = resolveJsonType env typ
    let name = decoderName typ
    match Map.tryFind name state.Functions with
    | Some _ -> Ok (name, state)
    | None ->
        let (functionId, state) = reserveFunction name state
        let (bindings, state) = freshBindings ["__source"; "__view"; "__path"] state
        let env = { env with Symbols = state.Symbols }
        let binding name =
            match Map.tryFind name bindings with
            | Some id -> id
            | None -> Crash.crash $"JSON decoder binding was not allocated: {name}"
        let placeholder = {
            Id = functionId
            Name = name
            TypeParams = []
            Params =
                NonEmptyList.fromList
                    [binding "__source", TString
                     binding "__view", valueViewType
                     binding "__path", pathType]
            ReturnType = resultType typ
            Body = RuntimeError "unfinished JSON decoder"
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        decodeBody
            env
            typ
            (local "__source" bindings)
            (local "__view" bindings)
            (local "__path" bindings)
            reserved
        |> Result.map (fun (body, nextState) ->
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private decodeCall env typ source view path state =
    ensureDecoder env typ state
    |> Result.map (fun (name, nextState) ->
        let currentEnv = { env with Symbols = nextState.Symbols }
        (call currentEnv name [source; view; path], nextState))

and private sequenceDecoded env source items build state =
    let rec loop remaining current bindings =
        match remaining with
        | [] -> Ok (build (List.rev bindings), current)
        | (typ, view, path, bindingName) :: rest ->
            decodeCall env typ source view path current
            |> Result.bind (fun (decoded, next) ->
                let (errorId, next) = freshBinding "__decode_error" next
                loop rest next ((bindingName, typ) :: bindings)
                |> Result.map (fun (tail, finalState) ->
                    (Match (decoded, resultCases env bindingName tail errorId), finalState)))
    loop items state []

and private ensureListDecoder env elemType state =
    let elemType = resolveJsonType env elemType
    let listType = TList elemType
    let name = decodeListName listType
    match Map.tryFind name state.Functions with
    | Some _ -> Ok (name, state)
    | None ->
        let (functionId, state) = reserveFunction name state
        let (bindings, state) =
            freshBindings
                ["__source"; "__array_view"; "__next_index"; "__path"; "__index"
                 "__head"; "__after_item"; "__decoded_head"; "__decoded_tail"
                 "__tail_error"; "__head_error"]
                state
        let env = { env with Symbols = state.Symbols }
        let binding name =
            match Map.tryFind name bindings with
            | Some id -> id
            | None -> Crash.crash $"JSON list decoder binding was not allocated: {name}"
        let placeholder = {
            Id = functionId
            Name = name
            TypeParams = []
            Params =
                NonEmptyList.fromList
                    [binding "__source", TString
                     binding "__array_view", valueViewType
                     binding "__next_index", TInt64
                     binding "__path", pathType
                     binding "__index", TInt64]
            ReturnType = resultType listType
            Body = RuntimeError "unfinished JSON list decoder"
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let itemPath =
            listPush env
                pathPartType
                (local "__path" bindings)
                (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Index" (Some (call env "Darklang.Stdlib.Int.fromInt64" [local "__index" bindings])))
        decodeCall env elemType (local "__source" bindings) (local "__head" bindings) itemPath reserved
        |> Result.map (fun (decodedHead, nextState) ->
            let decodedTail =
                call env
                    name
                    [ local "__source" bindings
                      local "__array_view" bindings
                      local "__after_item" bindings
                      local "__path" bindings
                      BinOp (Add, local "__index" bindings, Int64Literal 1L) ]
            let invalid =
                cantMatch
                    env
                    listType
                    (rawSource env (local "__source" bindings) (local "__array_view" bindings))
                    (local "__path" bindings)
            let decodedTailResult =
                Match (
                    decodedTail,
                    resultCases
                        env
                        (binding "__decoded_tail")
                        (ok env (listPush env elemType (local "__decoded_tail" bindings) (local "__decoded_head" bindings)))
                        (binding "__tail_error"))
            let decodedHeadResult =
                Match (
                    decodedHead,
                    resultCases
                        env
                        (binding "__decoded_head")
                        decodedTailResult
                        (binding "__head_error"))
            let body =
                Let (
                    LPVariable (binding "__head"),
                    call env "Darklang.Stdlib.Json.__arrayNext" [local "__source" bindings; local "__array_view" bindings; local "__next_index" bindings],
                    If (
                        BinOp (Eq, local "__head" bindings, Int64Literal -1L),
                        ok env (ListLiteral []),
                        If (
                            BinOp (Eq, local "__head" bindings, Int64Literal -2L),
                            invalid,
                            Let (
                                LPVariable (binding "__after_item"),
                                call env "Darklang.Stdlib.Json.__arrayAfter" [local "__source" bindings; local "__array_view" bindings; local "__head" bindings],
                                If (
                                    BinOp (Lt, local "__after_item" bindings, Int64Literal 0L),
                                    invalid,
                                    decodedHeadResult)))))
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private ensureDictDecoder env valueType state =
    let valueType = resolveJsonType env valueType
    let dictType = TDict (TString, valueType)
    let name = decodeDictName dictType
    match Map.tryFind name state.Functions with
    | Some _ -> Ok (name, state)
    | None ->
        let (functionId, state) = reserveFunction name state
        let viewFieldsType = TList (TTuple [TString; valueViewType])
        let (bindings, state) =
            freshBindings
                ["__source"; "__fields"; "__path"; "__dict"; "__entry"; "__tail"
                 "__decoded_value"; "__decoded_dict"; "__dict_tail_error"; "__dict_error"]
                state
        let env = { env with Symbols = state.Symbols }
        let binding name =
            match Map.tryFind name bindings with
            | Some id -> id
            | None -> Crash.crash $"JSON dictionary decoder binding was not allocated: {name}"
        let placeholder = {
            Id = functionId
            Name = name
            TypeParams = []
            Params =
                NonEmptyList.fromList
                    [binding "__source", TString
                     binding "__fields", viewFieldsType
                     binding "__path", pathType
                     binding "__dict", dictType]
            ReturnType = resultType dictType
            Body = RuntimeError "unfinished JSON dictionary decoder"
            Recursion = None
        }
        let reserved = { state with Functions = Map.add name placeholder state.Functions }
        let key = call env "Darklang.Stdlib.Json.__viewFieldName" [local "__entry" bindings]
        let fieldView = call env "Darklang.Stdlib.Json.__viewFieldValue" [local "__entry" bindings]
        let fieldPath =
            listPush env
                pathPartType
                (local "__path" bindings)
                (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Field" (Some key))
        decodeCall env valueType (local "__source" bindings) fieldView fieldPath reserved
        |> Result.map (fun (decoded, nextState) ->
            let withValue =
                TypeApp (
                    resolveFunction env "Darklang.Stdlib.Dict.setOverridingDuplicates",
                    [TString; valueType],
                    args [local "__dict" bindings; key; local "__decoded_value" bindings])
            let body =
                Match (
                    local "__fields" bindings,
                    [ makeCase (PList []) (ok env (local "__dict" bindings))
                      makeCase
                          (PListCons ([patternLocal "__entry" bindings], patternLocal "__tail" bindings))
                          (Match (
                              decoded,
                              resultCases
                                  env
                                  (binding "__decoded_value")
                                  (Match (
                                      call env name [local "__source" bindings; local "__tail" bindings; local "__path" bindings; withValue],
                                      resultCases
                                          env
                                          (binding "__decoded_dict")
                                          (ok env (local "__decoded_dict" bindings))
                                          (binding "__dict_tail_error")))
                                  (binding "__dict_error"))) ])
            let completed = { placeholder with Body = body }
            (name, { nextState with Functions = Map.add name completed nextState.Functions }))

and private decodeEnumCase
    env
    typ
    typeName
    subst
    source
    path
    caseRaw
    (variant: SumVariant)
    state =
    let casePath =
        listPush env
            pathPartType
            path
            (constructor env
                "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part"
                "Field"
                (Some (StringLiteral variant.Name)))
    let fieldTypes =
        variant.Fields |> List.map (applySubstitution subst >> resolveJsonType env)
    let rawNames = fieldTypes |> List.mapi (fun index _ -> $"__enum_raw_{variant.Tag}_{index}")
    let valueNames = fieldTypes |> List.mapi (fun index _ -> $"__enum_value_{variant.Tag}_{index}")
    let extraName = $"__enum_extra_{variant.Tag}"
    let (bindings, state) = freshBindings (rawNames @ valueNames @ [extraName; "__enum_args"]) state
    let decodedItems count =
        fieldTypes
        |> List.take count
        |> List.mapi (fun index fieldType ->
            let argumentPath =
                listPush env
                    pathPartType
                    casePath
                    (constructor env
                        "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part"
                        "Index"
                        (Some (BigIntLiteral (bigint index))))
            let valueId =
                match Map.tryFind valueNames[index] bindings with
                | Some id -> id
                | None -> Crash.crash "JSON enum value binding was not allocated"
            (fieldType, local rawNames[index] bindings, argumentPath, valueId))
    let constructed =
        let values = valueNames |> List.map (fun name -> local name bindings)
        match tryFindConstructorId typeName variant.Name env.Symbols with
        | Some id ->
            Constructor (
                { TypeId = typeId env typeName; ConstructorId = id },
                values
            )
            |> ok env
        | None -> Crash.crash $"JSON enum constructor was not interned: {typeName}.{variant.Name}"
    let exactResult = sequenceDecoded env source (decodedItems fieldTypes.Length) (fun _ -> constructed) state
    exactResult
    |> Result.bind (fun (exactBody, exactState) ->
        let rec missingCases count current acc =
            if count >= fieldTypes.Length then Ok (List.rev acc, current)
            else
                let missing =
                    constructor env
                        "Darklang.Stdlib.Json.ParseError.ParseError"
                        "EnumMissingField"
                        (tuplePayload
                            [typeReference env fieldTypes[count]
                             BigIntLiteral (bigint count)
                             casePath])
                    |> error env
                sequenceDecoded env source (decodedItems count) (fun _ -> missing) current
                |> Result.bind (fun (body, next) ->
                    missingCases
                        (count + 1)
                        next
                        (makeCase
                            (PList (rawNames |> List.take count |> List.map (fun name -> patternLocal name bindings)))
                            body
                         :: acc))
        missingCases 0 exactState []
        |> Result.bind (fun (missing, missingState) ->
            let extraPath =
                listPush env
                    pathPartType
                    casePath
                    (constructor env
                        "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part"
                        "Index"
                        (Some (BigIntLiteral (bigint fieldTypes.Length))))
            let extra =
                constructor env
                    "Darklang.Stdlib.Json.ParseError.ParseError"
                    "EnumExtraField"
                    (tuplePayload [rawSource env source (local extraName bindings); extraPath])
                |> error env
            sequenceDecoded env source (decodedItems fieldTypes.Length) (fun _ -> extra) missingState
            |> Result.map (fun (extraBody, finalState) ->
                let exact =
                    makeCase (PList (rawNames |> List.map (fun name -> patternLocal name bindings))) exactBody
                let extraPattern =
                    PListCons (
                        (rawNames |> List.map (fun name -> patternLocal name bindings))
                        @ [patternLocal extraName bindings],
                        PWildcard)
                let arrayBody =
                    Match (
                        local "__enum_args" bindings,
                        missing @ [exact; makeCase extraPattern extraBody])
                let body =
                    Match (
                        call env "Darklang.Stdlib.Json.__arrayItems" [source; caseRaw],
                        [ makeCase
                              (constructorPattern
                                  env
                                  "Darklang.Stdlib.Option.Option"
                                  "Some"
                                  [patternLocal "__enum_args" bindings])
                              arrayBody
                          makeCase PWildcard (cantMatch env typ (rawSource env source caseRaw) casePath) ])
                (body, finalState))))

and private decodeBody env typ source view path state : Result<Expr * State, string> =
    let failure = cantMatch env typ (rawSource env source view) path
    match typ with
    | TUnit ->
        Ok (If (call env "Darklang.Stdlib.Json.__isNull" [source; view], ok env UnitLiteral, failure), state)
    | TBool -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__boolValue" source view path state)
    | TString -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__stringValue" source view path state)
    | TChar -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewChar" source view path state)
    | TInt8 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewInt8" source view path state)
    | TInt16 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewInt16" source view path state)
    | TInt32 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewInt32" source view path state)
    | TInt64 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewInt64" source view path state)
    | TInt128 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewInt128" source view path state)
    | TInt -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewInt" source view path state)
    | TUInt8 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewUInt8" source view path state)
    | TUInt16 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewUInt16" source view path state)
    | TUInt32 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewUInt32" source view path state)
    | TUInt64 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewUInt64" source view path state)
    | TUInt128 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewUInt128" source view path state)
    | TFloat64 -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewFloat" source view path state)
    | TSum ("Uuid", []) -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewUuid" source view path state)
    | TDateTime -> Ok (optionDecoder env typ "Darklang.Stdlib.Json.__viewDateTime" source view path state)
    | TList elemType ->
        ensureListDecoder env elemType state
        |> Result.map (fun (listDecoder, nextState) ->
            let (arrayStartId, nextState) = freshBinding "__array_start" nextState
            let currentEnv = { env with Symbols = nextState.Symbols }
            (Let (
                LPVariable arrayStartId,
                call currentEnv "Darklang.Stdlib.Json.__arrayStart" [source; view],
                If (
                    BinOp (Lt, Local arrayStartId, Int64Literal 0L),
                    failure,
                    call currentEnv listDecoder [source; view; Local arrayStartId; path; Int64Literal 0L])),
             nextState))
    | TTuple elementTypes ->
        let names = elementTypes |> List.mapi (fun index _ -> $"__tuple_raw_{index}")
        let valueNames = elementTypes |> List.mapi (fun index _ -> $"__tuple_value_{index}")
        let (bindings, state) = freshBindings (names @ valueNames) state
        let patterns = names |> List.map (fun name -> patternLocal name bindings)
        let items = elementTypes |> List.mapi (fun index elemType ->
            let itemPath =
                listPush env
                    pathPartType
                    path
                    (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Index" (Some (BigIntLiteral (bigint index))))
            let valueId =
                match Map.tryFind valueNames[index] bindings with
                | Some id -> id
                | None -> Crash.crash "JSON tuple value binding was not allocated"
            (elemType, local names[index] bindings, itemPath, valueId))
        sequenceDecoded env source items (fun decoded -> ok env (TupleLiteral (decoded |> List.map (fst >> Local)))) state
        |> Result.map (fun (decoded, nextState) ->
            (Match (
                call env "Darklang.Stdlib.Json.__arrayItems" [source; view],
                [ makeCase
                      (constructorPattern
                          env
                          "Darklang.Stdlib.Option.Option"
                          "Some"
                          [PList patterns])
                      decoded
                  makeCase PWildcard failure ]),
             nextState))
    | TRecord (typeName, typeArgs) ->
        match Map.tryFind typeName env.Records with
        | None -> Error $"Unsupported type in JSON: {CheckingDiagnostics.typeToString typ}"
        | Some recordInfo ->
            substitution recordInfo.TypeParams typeArgs
            |> Result.bind (fun subst ->
                let (objectMapId, state) = freshBinding "__object_field_map" state
                // Conversion checks required fields in declaration order; wire
                // serialization is independently ordinal-by-name.
                let fields = recordInfo.Fields
                let rec build remaining current decodedFields =
                    match remaining with
                    | [] ->
                        Ok (
                            ok env (
                                RecordLiteral (
                                    { TypeId = typeId env typeName; TypeArgs = typeArgs },
                                    List.rev decodedFields
                                )
                            ),
                            current
                        )
                    | (fieldName, fieldType) :: rest ->
                        let concrete = applySubstitution subst fieldType |> resolveJsonType env
                        let (fieldRawId, current) = freshBinding "__field_raw" current
                        let (fieldValueId, current) = freshBinding $"__field_{fieldName}" current
                        let (fieldErrorId, current) = freshBinding "__field_error" current
                        let matches =
                            TypeApp (
                                resolveFunction env "Darklang.Stdlib.Dict.get",
                                [TString; valueViewType],
                                args [Local objectMapId; StringLiteral fieldName])
                        let fieldPath =
                            listPush env
                                pathPartType
                                path
                                (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Field" (Some (StringLiteral fieldName)))
                        decodeCall env concrete source (Local fieldRawId) fieldPath current
                        |> Result.bind (fun (decoded, next) ->
                            let id = fieldId env typeName fieldName
                            build rest next ((id, Local fieldValueId) :: decodedFields)
                            |> Result.map (fun (tail, finalState) ->
                                let missing = constructor env "Darklang.Stdlib.Json.ParseError.ParseError" "RecordMissingField" (tuplePayload [StringLiteral fieldName; path]) |> error env
                                let duplicate = constructor env "Darklang.Stdlib.Json.ParseError.ParseError" "RecordDuplicateField" (tuplePayload [StringLiteral fieldName; path]) |> error env
                                let one = Match (decoded, resultCases env fieldValueId tail fieldErrorId)
                                (Match (
                                    matches,
                                    [ makeCase
                                          (constructorPattern
                                              env
                                              "Darklang.Stdlib.Option.Option"
                                              "None"
                                              [])
                                          missing
                                      makeCase
                                        (constructorPattern
                                            env
                                            "Darklang.Stdlib.Option.Option"
                                            "Some"
                                            [PVariable fieldRawId])
                                        (If (
                                            call env "Darklang.Stdlib.Json.__viewIsDuplicate" [Local fieldRawId],
                                            duplicate,
                                            one)) ]),
                                 finalState)))
                build fields state []
                |> Result.map (fun (decoded, nextState) ->
                    (Match (
                        call env "Darklang.Stdlib.Json.__objectFieldMap" [source; view],
                        [ makeCase
                              (constructorPattern
                                  env
                                  "Darklang.Stdlib.Option.Option"
                                  "Some"
                                  [PVariable objectMapId])
                              decoded
                          makeCase PWildcard failure ]),
                     nextState)))
    | TDict (TString, valueType) ->
        ensureDictDecoder env valueType state
        |> Result.map (fun (dictDecoder, nextState) ->
            let (objectFieldsId, nextState) = freshBinding "__object_fields" nextState
            let currentEnv = { env with Symbols = nextState.Symbols }
            let empty = DictLiteral (TString, valueType, [])
            (Match (
                call currentEnv "Darklang.Stdlib.Json.__objectFields" [source; view],
                [ makeCase
                      (constructorPattern
                          currentEnv
                          "Darklang.Stdlib.Option.Option"
                          "Some"
                          [PVariable objectFieldsId])
                      (call currentEnv dictDecoder [source; Local objectFieldsId; path; empty])
                  makeCase PWildcard failure ]),
             nextState))
    | TSum (typeName, typeArgs) ->
        match Map.tryFind typeName env.Sums with
        | None -> Error $"Unsupported type in JSON: {CheckingDiagnostics.typeToString typ}"
        | Some sumInfo ->
            substitution sumInfo.TypeParams typeArgs
            |> Result.bind (fun subst ->
                let (caseNameId, state) = freshBinding "__case_name" state
                let (caseRawId, state) = freshBinding "__case_raw" state
                let (caseNamesId, state) = freshBinding "__case_names" state
                let rec buildCases (remaining: SumVariant list) current acc =
                    match remaining with
                    | [] -> Ok (List.rev acc, current)
                    | variant :: rest ->
                        decodeEnumCase env typ typeName subst source path (Local caseRawId) variant current
                        |> Result.bind (fun (body, next) ->
                            buildCases rest next (makeCase (PString variant.Name) body :: acc))
                buildCases (List.sortBy (fun variant -> variant.Tag) sumInfo.Variants) state []
                |> Result.map (fun (caseMatches, nextState) ->
                    let invalidCase =
                        constructor env
                            "Darklang.Stdlib.Json.ParseError.ParseError"
                            "EnumInvalidCasename"
                            (tuplePayload [typeReference env typ; Local caseNameId; path])
                        |> error env
                    let oneField =
                        Match (
                            Local caseNameId,
                            caseMatches @ [makeCase PWildcard invalidCase])
                    let tooMany =
                        constructor env
                            "Darklang.Stdlib.Json.ParseError.ParseError"
                            "EnumTooManyCases"
                            (tuplePayload [typeReference env typ; Local caseNamesId; path])
                        |> error env
                    let checkedOneField = oneField
                    let objectBody =
                        Match (
                            call env "Darklang.Stdlib.Json.__enumCandidate" [source; view],
                            [ makeCase
                                  (constructorPattern
                                      env
                                      "Darklang.Stdlib.Json.InternalEnumObject"
                                      "EnumNoFields"
                                      [])
                                  failure
                              makeCase
                                  (constructorPattern
                                      env
                                      "Darklang.Stdlib.Json.InternalEnumObject"
                                      "EnumOneField"
                                      [PVariable caseNameId; PVariable caseRawId])
                                  checkedOneField
                              makeCase
                                  (constructorPattern
                                      env
                                      "Darklang.Stdlib.Json.InternalEnumObject"
                                      "EnumManyFields"
                                      [PVariable caseNamesId])
                                  tooMany
                              makeCase PWildcard failure ])
                    (objectBody, nextState)))
    | TFunction _ | TBlob | TRawPtr | TRuntimeError | TStream _ | TVar _ | TDict _ ->
        Error $"Unsupported type in JSON: {CheckingDiagnostics.typeToString typ}. Some types are not supported in Json serialization"

let rec private mapExpr rewrite symbols expr =
    let mapList values state =
        values
        |> List.mapFold (fun current value -> mapExpr rewrite current value) state
    let mapNonEmpty values state =
        let (mapped, next) = mapList (NonEmptyList.toList values) state
        (NonEmptyList.fromList mapped, next)
    let mapPair first second state =
        let (first', afterFirst) = mapExpr rewrite state first
        let (second', next) = mapExpr rewrite afterFirst second
        (first', second', next)
    let (mapped, symbols) =
        match expr with
        | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _
        | Int16Literal _ | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _
        | UInt64Literal _ | UInt128Literal _ | BoolLiteral _ | StringLiteral _ | BlobLiteral _ | CharLiteral _
        | FloatLiteral _ | Local _ | FuncRef _ | RuntimeError _ -> (expr, symbols)
        | InterpolatedString parts ->
            let (parts', next) =
                parts
                |> List.mapFold (fun current part ->
                    match part with
                    | StringText _ -> (part, current)
                    | StringExpr value ->
                        let (value', following) = mapExpr rewrite current value
                        (StringExpr value', following)) symbols
            (InterpolatedString parts', next)
        | BinOp (op, left, right) ->
            let (left', right', next) = mapPair left right symbols
            (BinOp (op, left', right'), next)
        | UnaryOp (op, inner) ->
            let (inner', next) = mapExpr rewrite symbols inner
            (UnaryOp (op, inner'), next)
        | Let (pattern, value, body) ->
            let (value', body', next) = mapPair value body symbols
            (Let (pattern, value', body'), next)
        | RecursiveLet (recursion, value, body) ->
            let (value', body', next) = mapPair value body symbols
            (RecursiveLet (recursion, value', body'), next)
        | If (condition, thenBranch, elseBranch) ->
            let (condition', afterCondition) = mapExpr rewrite symbols condition
            let (thenBranch', elseBranch', next) = mapPair thenBranch elseBranch afterCondition
            (If (condition', thenBranch', elseBranch'), next)
        | Sequence (first, nextExpr) ->
            let (first', next', next) = mapPair first nextExpr symbols
            (Sequence (first', next'), next)
        | Call (name, values) ->
            let (values', next) = mapNonEmpty values symbols
            (Call (name, values'), next)
        | TypeApp (name, types, values) ->
            let (values', next) = mapNonEmpty values symbols
            (TypeApp (name, types, values'), next)
        | TupleLiteral values ->
            let (values', next) = mapList values symbols
            (TupleLiteral values', next)
        | TupleAccess (value, index) ->
            let (value', next) = mapExpr rewrite symbols value
            (TupleAccess (value', index), next)
        | DictLiteral (keyType, valueType, entries) ->
            let (entries', next) =
                entries
                |> List.mapFold (fun current (key, value) ->
                    let (key', value', following) = mapPair key value current
                    ((key', value'), following)) symbols
            (DictLiteral (keyType, valueType, entries'), next)
        | RecordLiteral (name, fields) ->
            let (fields', next) =
                fields
                |> List.mapFold (fun current (field, value) ->
                    let (value', following) = mapExpr rewrite current value
                    ((field, value'), following)) symbols
            (RecordLiteral (name, fields'), next)
        | RecordUpdate (record, fields) ->
            let (record', afterRecord) = mapExpr rewrite symbols record
            let (fields', next) =
                fields
                |> List.mapFold (fun current (field, value) ->
                    let (value', following) = mapExpr rewrite current value
                    ((field, value'), following)) afterRecord
            (RecordUpdate (record', fields'), next)
        | RecordAccess (record, field) ->
            let (record', next) = mapExpr rewrite symbols record
            (RecordAccess (record', field), next)
        | Constructor (reference, fields) ->
            let (fields', next) = mapList fields symbols
            (Constructor (reference, fields'), next)
        | Match (value, cases) ->
            let (value', afterValue) = mapExpr rewrite symbols value
            let (cases', next) =
                cases
                |> List.mapFold (fun current case ->
                    let (guard', afterGuard) =
                        match case.Guard with
                        | None -> (None, current)
                        | Some guard ->
                            let (guard', following) = mapExpr rewrite current guard
                            (Some guard', following)
                    let (body', following) = mapExpr rewrite afterGuard case.Body
                    ({ case with Guard = guard'; Body = body' }, following)) afterValue
            (Match (value', cases'), next)
        | ListLiteral values ->
            let (values', next) = mapList values symbols
            (ListLiteral values', next)
        | Lambda (parameters, annotation, body) ->
            let (body', next) = mapExpr rewrite symbols body
            (Lambda (parameters, annotation, body'), next)
        | Apply (fn, values) | IndirectApply (fn, values) ->
            let (fn', afterFn) = mapExpr rewrite symbols fn
            let (values', next) = mapNonEmpty values afterFn
            match expr with
            | Apply _ -> (Apply (fn', values'), next)
            | _ -> (IndirectApply (fn', values'), next)
        | Closure (name, captures) ->
            let (captures', next) = mapList captures symbols
            (Closure (name, captures'), next)
        | BoundaryRender (renderer, value) ->
            let (value', next) = mapExpr rewrite symbols value
            (BoundaryRender (renderer, value'), next)
    rewrite symbols mapped

let rewriteProgramWithSession
    (session: PlanningSession option)
    (env: CheckingTypes.TypeCheckEnv)
    (Program (symbols, topLevels))
    : Program =
    let serializeId = tryFindFunctionId "Darklang.Stdlib.Json.serialize" symbols
    let parseId = tryFindFunctionId "Darklang.Stdlib.Json.parse" symbols
    let (serializerTypes, parserTypes) =
        let collect expr acc =
            let initialResult = acc
            // mapExpr provides a compact complete traversal; the fold result is
            // threaded functionally through this local recursive collector.
            let rec walk current collected =
                let collected =
                    match current with
                    | TypeApp (id, [typ], _) when Some id = serializeId ->
                        (typ :: fst collected, snd collected)
                    | TypeApp (id, [typ], _) when Some id = parseId ->
                        (fst collected, typ :: snd collected)
                    | _ -> collected
                let capture child = walk child
                match current with
                | BinOp (_, a, b) | Sequence (a, b) -> capture b (capture a collected)
                | UnaryOp (_, a) | TupleAccess (a, _) | RecordAccess (a, _) | BoundaryRender (_, a) -> capture a collected
                | Let (_, a, b) | RecursiveLet (_, a, b) -> capture b (capture a collected)
                | If (a, b, c) -> capture c (capture b (capture a collected))
                | Call (_, values) | TypeApp (_, _, values) -> NonEmptyList.toList values |> List.fold (fun s e -> capture e s) collected
                | TupleLiteral values | ListLiteral values | Closure (_, values) -> List.fold (fun s e -> capture e s) collected values
                | DictLiteral (_, _, entries) ->
                    entries |> List.fold (fun state (key, value) -> capture value (capture key state)) collected
                | RecordLiteral (_, fields) -> fields |> List.fold (fun s (_, e) -> capture e s) collected
                | RecordUpdate (record, fields) -> fields |> List.fold (fun s (_, e) -> capture e s) (capture record collected)
                | Constructor (_, fields) -> fields |> List.fold (fun state field -> capture field state) collected
                | Match (value, cases) -> cases |> List.fold (fun s case -> capture case.Body (case.Guard |> Option.map (fun g -> capture g s) |> Option.defaultValue s)) (capture value collected)
                | Lambda (_, _, body) -> capture body collected
                | Apply (fn, values) | IndirectApply (fn, values) -> NonEmptyList.toList values |> List.fold (fun s e -> capture e s) (capture fn collected)
                | InterpolatedString parts -> parts |> List.fold (fun s part -> match part with StringText _ -> s | StringExpr e -> capture e s) collected
                | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _
                | Int16Literal _ | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _
                | UInt64Literal _ | UInt128Literal _ | BoolLiteral _ | StringLiteral _ | BlobLiteral _ | CharLiteral _
                | FloatLiteral _ | Local _ | FuncRef _ | RuntimeError _ -> collected
            walk expr initialResult
        topLevels
        |> List.fold (fun acc topLevel ->
            match topLevel with
            | FunctionDef fn -> collect fn.Body acc
            | ValueDef valueDef -> collect (valueDefBody valueDef) acc
            | Expression expr -> collect expr acc
            | TypeDef _ -> acc) ([], [])
        |> fun (serializers, parsers) -> (List.distinct serializers, List.distinct parsers)

    let hasJsonCalls = not (List.isEmpty serializerTypes && List.isEmpty parserTypes)
    let symbols =
        if hasJsonCalls then
            [ "Darklang.Stdlib.DateTime.toString"
              "Darklang.Stdlib.Int.fromInt64"
              "Darklang.Stdlib.Int.toString"
              "Darklang.Stdlib.Int128.toString"
              "Darklang.Stdlib.Int16.toString"
              "Darklang.Stdlib.Int32.toString"
              "Darklang.Stdlib.Int64.toString"
              "Darklang.Stdlib.Int8.toString"
              "Darklang.Stdlib.Json.ParseError.__copyPath"
              "Darklang.Stdlib.Json.__arrayAfter"
              "Darklang.Stdlib.Json.__arrayItems"
              "Darklang.Stdlib.Json.__arrayNext"
              "Darklang.Stdlib.Json.__arrayStart"
              "Darklang.Stdlib.Json.__boolValue"
              "Darklang.Stdlib.Json.__copyRaw"
              "Darklang.Stdlib.Json.__enumCandidate"
              "Darklang.Stdlib.Json.__isNull"
              "Darklang.Stdlib.Json.__objectFieldMap"
              "Darklang.Stdlib.Json.__objectFields"
              "Darklang.Stdlib.Json.__parseRoot"
              "Darklang.Stdlib.Json.__serializeFloat"
              "Darklang.Stdlib.Json.__stringValue"
              "Darklang.Stdlib.Json.__viewChar"
              "Darklang.Stdlib.Json.__viewDateTime"
              "Darklang.Stdlib.Json.__viewFieldName"
              "Darklang.Stdlib.Json.__viewFieldValue"
              "Darklang.Stdlib.Json.__viewFloat"
              "Darklang.Stdlib.Json.__viewInt"
              "Darklang.Stdlib.Json.__viewInt128"
              "Darklang.Stdlib.Json.__viewInt16"
              "Darklang.Stdlib.Json.__viewInt32"
              "Darklang.Stdlib.Json.__viewInt64"
              "Darklang.Stdlib.Json.__viewInt8"
              "Darklang.Stdlib.Json.__viewIsDuplicate"
              "Darklang.Stdlib.Json.__viewUInt128"
              "Darklang.Stdlib.Json.__viewUInt16"
              "Darklang.Stdlib.Json.__viewUInt32"
              "Darklang.Stdlib.Json.__viewUInt64"
              "Darklang.Stdlib.Json.__viewUInt8"
              "Darklang.Stdlib.Json.__viewUuid"
              "Darklang.Stdlib.Json.__writerBeginArray"
              "Darklang.Stdlib.Json.__writerBeginObject"
              "Darklang.Stdlib.Json.__writerEmpty"
              "Darklang.Stdlib.Json.__writerEndArray"
              "Darklang.Stdlib.Json.__writerEndObject"
              "Darklang.Stdlib.Json.__writerFieldName"
              "Darklang.Stdlib.Json.__writerFinish"
              "Darklang.Stdlib.Json.__writerSeparator"
              "Darklang.Stdlib.Json.__writerWriteRaw"
              "Darklang.Stdlib.Json.__writerWriteString"
              "Darklang.Stdlib.List.push"
              "Darklang.Stdlib.Dict.toList"
              "Darklang.Stdlib.Dict.setOverridingDuplicates"
              "Darklang.Stdlib.Dict.get"
              "Darklang.Stdlib.UInt128.toString"
              "Darklang.Stdlib.UInt16.toString"
              "Darklang.Stdlib.UInt32.toString"
              "Darklang.Stdlib.UInt64.toString"
              "Darklang.Stdlib.UInt8.toString"
              "Darklang.Stdlib.Uuid.toString" ]
            |> List.fold (fun current name -> CheckedAST.internFunction name current |> snd) symbols
        else
            symbols
    let planningSymbols =
        env.IndexedTypeReg
        |> Map.fold (fun current typeName recordInfo ->
            recordInfo.Fields
            |> List.indexed
            |> List.fold (fun current (index, (fieldName, _)) ->
                internField typeName fieldName index current |> snd) current) symbols
    let planningEnv = {
        Records = env.IndexedTypeReg
        Sums = if hasJsonCalls then env.IndexedSumTypeReg else Map.empty
        Aliases = env.AliasReg
        Symbols = planningSymbols
    }

    let mergeArtifact (state: State) (functions: FunctionDef list) : Result<State, string> =
        functions
        |> List.fold
            (fun result fn ->
                result
                |> Result.bind (fun current ->
                    match Map.tryFind fn.Name current.Functions with
                    | Some existing when existing <> fn ->
                        Error $"JSON codec name collision for canonical plan '{fn.Name}'"
                    | Some _ -> Ok current
                    | None ->
                        Ok { current with Functions = Map.add fn.Name fn current.Functions }))
            (Ok state)

    let planCached
        (direction: string)
        (ensure: Env -> Type -> State -> Result<string * State, string>)
        (typ: Type)
        (state: State)
        : Result<State, string> =
        let concrete = resolveJsonType planningEnv typ
        let key = $"{direction}|{canonicalCodecTypeKey planningEnv concrete}"
        match session |> Option.bind (fun current -> current.TryFind key) with
        | Some _ -> ensure planningEnv concrete state |> Result.map snd
        | None ->
            let existingNames = state.Functions |> Map.keys |> Set.ofSeq
            ensure planningEnv concrete state
            |> Result.bind (fun (_, artifactState) ->
                let functions =
                    artifactState.Functions
                    |> Map.toList
                    |> List.choose (fun (name, fn) ->
                        if Set.contains name existingNames then None else Some fn)
                session |> Option.iter (fun current -> current.Store(key, functions))
                Ok artifactState)

    let planned =
        match session with
        | None ->
            // A one-shot caller can share dependencies directly in one state
            // without paying for cache keys or artifact merging.
            let serializersPlanned =
                serializerTypes
                |> List.fold (fun result typ ->
                    result
                    |> Result.bind (fun state -> ensureSerializer planningEnv typ state |> Result.map snd))
                    (Ok { Functions = Map.empty; Symbols = planningSymbols })
            parserTypes
            |> List.fold (fun result typ ->
                result
                |> Result.bind (fun state -> ensureDecoder planningEnv typ state |> Result.map snd)) serializersPlanned
        | Some _ ->
            let serializersPlanned =
                serializerTypes
                |> List.fold (fun result typ ->
                    result |> Result.bind (planCached "serialize" ensureSerializer typ))
                    (Ok { Functions = Map.empty; Symbols = planningSymbols })
            parserTypes
            |> List.fold (fun result typ ->
                result |> Result.bind (planCached "parse" ensureDecoder typ)) serializersPlanned

    if not hasJsonCalls then
        Program (symbols, topLevels)
    else
        match planned with
        | Error error ->
            let rewrite currentSymbols expr =
                match expr with
                | TypeApp (id, _, _)
                    when Some id = serializeId || Some id = parseId ->
                    (RuntimeError error, currentSymbols)
                | _ -> (expr, currentSymbols)
            let (rewritten, symbols') =
                topLevels
                |> List.mapFold (fun currentSymbols topLevel ->
                    match topLevel with
                    | FunctionDef fn ->
                        let (body, next) = mapExpr rewrite currentSymbols fn.Body
                        (FunctionDef { fn with Body = body }, next)
                    | Expression expr ->
                        let (expr', next) = mapExpr rewrite currentSymbols expr
                        (Expression expr', next)
                    | other -> (other, currentSymbols)) symbols
            Program (symbols', rewritten)
        | Ok state ->
            let finalPlanningEnv = { planningEnv with Symbols = state.Symbols }
            let rewrite currentSymbols expr =
                match expr with
                | TypeApp (id, [typ], values) when Some id = serializeId ->
                    let written =
                        call finalPlanningEnv
                            (serializeName (resolveJsonType finalPlanningEnv typ))
                            (writerEmpty finalPlanningEnv :: NonEmptyList.toList values)
                    (writerFinish finalPlanningEnv written, currentSymbols)
                | TypeApp (id, [typ], values) when Some id = parseId ->
                    let concrete = resolveJsonType finalPlanningEnv typ
                    let source = NonEmptyList.head values
                    let (sourceId, symbols1) = allocateBinding "__json_source" currentSymbols
                    let (parseResultId, symbols2) = allocateBinding "__json_parse_result" symbols1
                    let (viewId, symbols3) = allocateBinding "__json_view" symbols2
                    let parsed = call finalPlanningEnv "Darklang.Stdlib.Json.__parseRoot" [Local sourceId]
                    let rootPath = ListLiteral [constructor finalPlanningEnv "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Root" None]
                    (Let (
                        LPVariable sourceId,
                        source,
                        Let (
                            LPVariable parseResultId,
                            parsed,
                            Match (
                                Local parseResultId,
                                [ makeCase
                                      (constructorPattern
                                          planningEnv
                                          "Darklang.Stdlib.Result.Result"
                                          "Ok"
                                          [PVariable viewId])
                                      (call finalPlanningEnv (decoderName concrete) [Local sourceId; Local viewId; rootPath])
                                  makeCase
                                      (constructorPattern
                                          planningEnv
                                          "Darklang.Stdlib.Result.Result"
                                          "Error"
                                          [PWildcard])
                                      (constructor finalPlanningEnv "Darklang.Stdlib.Json.ParseError.ParseError" "NotJson" None |> error finalPlanningEnv) ]))),
                     symbols3)
                | _ -> (expr, currentSymbols)
            let (rewritten, finalSymbols) =
                topLevels
                |> List.mapFold (fun currentSymbols topLevel ->
                    match topLevel with
                    | FunctionDef fn ->
                        let (body, next) = mapExpr rewrite currentSymbols fn.Body
                        (FunctionDef { fn with Body = body }, next)
                    | Expression expr ->
                        let (expr', next) = mapExpr rewrite currentSymbols expr
                        (Expression expr', next)
                    | other -> (other, currentSymbols)) state.Symbols
            let generated = state.Functions |> Map.toList |> List.map (snd >> FunctionDef)
            Program (finalSymbols, generated @ rewritten)

let rewriteProgram (env: CheckingTypes.TypeCheckEnv) (program: Program) : Program =
    rewriteProgramWithSession None env program
