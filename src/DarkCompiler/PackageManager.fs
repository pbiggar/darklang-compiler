// PackageManager.fs - Hosted package resolution and persistent response cache.
//
// Resolves the same ProgramTypes package declarations exposed by Matter's
// package-manager HTTP API and renders them as compiler package source units.

module PackageManager

open System
open System.IO
open System.Net
open System.Net.Http
open System.Text.Json
open Microsoft.Data.Sqlite

type Config = {
    Server: Uri
    CachePath: string
}

type ResolvedSource = {
    Name: string
    Source: string
}

type private ItemKind = PackageType | PackageValue | PackageFunction

type private LocatedEntity = {
    Kind: ItemKind
    Hash: string
    Location: string
    Json: string
}

type private FetchResult = Found of string | Missing

// ProgramTypes encodes expression trees as nested tagged arrays. Real package
// functions exceed System.Text.Json's conservative default depth of 64, while
// retaining a finite bound protects the compiler from unbounded payloads.
let private packageJsonOptions = JsonDocumentOptions(MaxDepth = 512)

let defaultServer = Uri "https://matter.darklang.com"

let defaultCachePath () : string =
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
        "dark-compiler",
        "packages.sqlite3")

let defaultConfig () : Config = {
    Server = defaultServer
    CachePath = defaultCachePath ()
}

let private kindPath kind =
    match kind with
    | PackageType -> "type"
    | PackageValue -> "value"
    | PackageFunction -> "function"

let private allKinds = [PackageType; PackageValue; PackageFunction]

let private withCache (config: Config) (action: SqliteConnection -> Result<'a, string>) : Result<'a, string> =
    try
        let directory = Path.GetDirectoryName config.CachePath
        if not (String.IsNullOrEmpty directory) then Directory.CreateDirectory directory |> ignore
        use connection = new SqliteConnection($"Data Source={config.CachePath}")
        connection.Open()
        use settings = connection.CreateCommand()
        settings.CommandText <- "PRAGMA busy_timeout = 5000"
        settings.ExecuteNonQuery() |> ignore
        use schema = connection.CreateCommand()
        schema.CommandText <-
            "CREATE TABLE IF NOT EXISTS package_responses "
            + "(cache_key TEXT PRIMARY KEY, status INTEGER NOT NULL, body TEXT NOT NULL)"
        schema.ExecuteNonQuery() |> ignore
        action connection
    with ex -> Error $"Package cache '{config.CachePath}' failed: {ex.Message}"

let private cacheRead (config: Config) (key: string) : Result<FetchResult option, string> =
    withCache config (fun connection ->
        use command = connection.CreateCommand()
        command.CommandText <- "SELECT status, body FROM package_responses WHERE cache_key = $key"
        command.Parameters.AddWithValue("$key", key) |> ignore
        use reader = command.ExecuteReader()
        if reader.Read() then
            match reader.GetInt32 0 with
            | 200 -> Ok (Some (Found (reader.GetString 1)))
            | 404 -> Ok (Some Missing)
            | status -> Error $"Package cache contains unsupported HTTP status {status} for {key}"
        else Ok None)

let private cacheWrite (config: Config) (key: string) (result: FetchResult) : Result<unit, string> =
    withCache config (fun connection ->
        let status, body =
            match result with
            | Found body -> 200, body
            | Missing -> 404, ""
        use command = connection.CreateCommand()
        command.CommandText <-
            "INSERT INTO package_responses(cache_key, status, body) VALUES ($key, $status, $body) "
            + "ON CONFLICT(cache_key) DO UPDATE SET status = excluded.status, body = excluded.body"
        command.Parameters.AddWithValue("$key", key) |> ignore
        command.Parameters.AddWithValue("$status", status) |> ignore
        command.Parameters.AddWithValue("$body", body) |> ignore
        command.ExecuteNonQuery() |> ignore
        Ok ())

let private requestNetwork (client: HttpClient) (config: Config) (path: string) : Result<FetchResult, string> =
    try
        let uri = Uri(config.Server, path)
        use response = client.GetAsync(uri).Result
        let body = response.Content.ReadAsStringAsync().Result
        if response.IsSuccessStatusCode then Ok (Found body)
        else if response.StatusCode = HttpStatusCode.NotFound then Ok Missing
        else Error $"Package server GET {uri} returned {int response.StatusCode}: {body}"
    with ex -> Error $"Package server GET {Uri(config.Server, path)} failed: {ex.Message}"

let private fetchByHash (client: HttpClient) (config: Config) (path: string) : Result<FetchResult, string> =
    let cacheKey = config.Server.AbsoluteUri.TrimEnd('/') + path
    match cacheRead config cacheKey with
    | Error error -> Error error
    | Ok (Some result) -> Ok result
    | Ok None ->
        requestNetwork client config path
        |> Result.bind (fun result -> cacheWrite config cacheKey result |> Result.map (fun () -> result))

let private findByName (client: HttpClient) (config: Config) (path: string) : Result<FetchResult, string> =
    let cacheKey = config.Server.AbsoluteUri.TrimEnd('/') + path
    match requestNetwork client config path with
    | Ok result -> cacheWrite config cacheKey result |> Result.map (fun () -> result)
    | Error networkError ->
        match cacheRead config cacheKey with
        | Ok (Some result) -> Ok result
        | Ok None -> Error networkError
        | Error cacheError -> Error $"{networkError}; {cacheError}"

let private objectFields (element: JsonElement) : (string * JsonElement) list =
    if element.ValueKind = JsonValueKind.Object then
        element.EnumerateObject() |> Seq.map (fun property -> property.Name, property.Value) |> Seq.toList
    else []

let private tryField name element =
    objectFields element
    |> List.tryPick (fun (fieldName, value) -> if fieldName = name then Some value else None)

let private arrayItems (element: JsonElement) : JsonElement list =
    if element.ValueKind = JsonValueKind.Array then element.EnumerateArray() |> Seq.toList else []

let private enumCase (element: JsonElement) : Result<string * JsonElement list, string> =
    match objectFields element with
    | [(caseName, fields)] when fields.ValueKind = JsonValueKind.Array -> Ok (caseName, arrayItems fields)
    | _ -> Error $"Expected a package enum, got {element.GetRawText()}"

let private stringValue (element: JsonElement) : Result<string, string> =
    match element.ValueKind with
    | JsonValueKind.String -> Ok (element.GetString())
    | _ -> Error $"Expected a string, got {element.GetRawText()}"

let private numberText (element: JsonElement) : Result<string, string> =
    match element.ValueKind with
    | JsonValueKind.Number -> Ok (element.GetRawText())
    | _ -> Error $"Expected a number, got {element.GetRawText()}"

let private escapedStringContents (value: string) =
    value
        .Replace("\\", "\\\\")
        .Replace("\"", "\\\"")
        .Replace("\n", "\\n")
        .Replace("\r", "\\r")
        .Replace("\t", "\\t")

let private quoted (value: string) = "\"" + escapedStringContents value + "\""

let private parseHashJson (json: string) : Result<string, string> =
    try
        use document = JsonDocument.Parse(json, packageJsonOptions)
        enumCase document.RootElement
        |> Result.bind (function
            | "Hash", [value] -> stringValue value
            | _ -> Error $"Package find returned an invalid hash: {json}")
    with ex -> Error $"Package find returned invalid JSON: {ex.Message}"

let private locationName (element: JsonElement) : Result<string, string> =
    match tryField "owner" element, tryField "modules" element, tryField "name" element with
    | Some owner, Some modules, Some name ->
        stringValue owner
        |> Result.bind (fun ownerText ->
            ResultList.mapResults stringValue (arrayItems modules)
            |> Result.bind (fun moduleNames ->
                stringValue name
                |> Result.map (fun leaf -> String.concat "." (ownerText :: moduleNames @ [leaf]))))
    | _ -> Error $"Invalid package location: {element.GetRawText()}"

let private resolvedName (element: JsonElement) : Result<string, string> =
    match tryField "resolved" element, tryField "originalName" element with
    | Some resolved, Some originalName ->
        enumCase resolved
        |> Result.bind (function
            | "Ok", [resolvedValue] ->
                match tryField "location" resolvedValue with
                | Some location ->
                    enumCase location
                    |> Result.bind (function
                        | "Some", [value] -> locationName value
                        | "None", [] ->
                            ResultList.mapResults stringValue (arrayItems originalName)
                            |> Result.map (String.concat ".")
                        | _ -> Error $"Invalid resolved package location: {location.GetRawText()}")
                | None -> Error $"Invalid resolved package name: {resolvedValue.GetRawText()}"
            | "Error", _ ->
                ResultList.mapResults stringValue (arrayItems originalName)
                |> Result.map (String.concat ".")
            | _ -> Error $"Invalid package resolution: {resolved.GetRawText()}")
    | _ -> Error $"Invalid package name resolution: {element.GetRawText()}"

let rec private renderType (element: JsonElement) : Result<string, string> =
    enumCase element
    |> Result.bind (fun (caseName, fields) ->
        let unary name =
            match fields with
            | [inner] -> renderType inner |> Result.map (fun rendered -> $"{name}<{rendered}>")
            | _ -> Error $"Invalid {caseName} type"
        match caseName, fields with
        | "TVariable", [name] -> stringValue name
        | "TUnit", [] -> Ok "Unit"
        | "TBool", [] -> Ok "Bool"
        | "TInt8", [] -> Ok "Int8"
        | "TUInt8", [] -> Ok "UInt8"
        | "TInt16", [] -> Ok "Int16"
        | "TUInt16", [] -> Ok "UInt16"
        | "TInt32", [] -> Ok "Int32"
        | "TUInt32", [] -> Ok "UInt32"
        | "TInt64", [] -> Ok "Int64"
        | "TUInt64", [] -> Ok "UInt64"
        | "TInt128", [] -> Ok "Int128"
        | "TUInt128", [] -> Ok "UInt128"
        | "TInt", [] -> Ok "Int"
        | "TFloat", [] -> Ok "Float"
        | "TChar", [] -> Ok "Char"
        | "TString", [] -> Ok "String"
        | "TDateTime", [] -> Ok "DateTime"
        | "TUuid", [] -> Ok "Uuid"
        | "TBlob", [] -> Ok "Blob"
        | "TStream", _ -> unary "Stream"
        | "TList", _ -> unary "List"
        | "TDB", _ -> unary "DB"
        | "TDict", [key; value] ->
            renderType key |> Result.bind (fun k -> renderType value |> Result.map (fun v -> $"Dict<{k}, {v}>"))
        | "TTuple", [first; second; rest] ->
            ResultList.mapResults renderType (first :: second :: arrayItems rest)
            |> Result.map (String.concat " * " >> fun value -> $"({value})")
        | "TFn", [parameters; returnType] ->
            ResultList.mapResults renderType (arrayItems parameters)
            |> Result.bind (fun args ->
                renderType returnType
                |> Result.map (fun result -> String.concat " -> " (args @ [result])))
        | "TCustomType", [name; typeArgs] ->
            resolvedName name
            |> Result.bind (fun renderedName ->
                ResultList.mapResults renderType (arrayItems typeArgs)
                |> Result.map (function
                    | [] -> renderedName
                    | args -> renderedName + "<" + String.concat ", " args + ">"))
        | _ -> Error $"Unsupported package type {caseName}")

let rec private renderLetPattern (element: JsonElement) : Result<string, string> =
    enumCase element
    |> Result.bind (function
        | "LPUnit", [_] -> Ok "()"
        | "LPWildcard", [_] -> Ok "_"
        | "LPVariable", [_; name] -> stringValue name
        | "LPTuple", [_; first; second; rest] ->
            ResultList.mapResults renderLetPattern (first :: second :: arrayItems rest)
            |> Result.map (String.concat ", " >> fun value -> $"({value})")
        | caseName, _ -> Error $"Unsupported package let pattern {caseName}")

let rec private renderMatchPattern (element: JsonElement) : Result<string, string> =
    enumCase element
    |> Result.bind (fun (caseName, fields) ->
        let scalar suffix value = numberText value |> Result.map (fun number -> number + suffix)
        match caseName, fields with
        | "MPVariable", [_; name] -> stringValue name
        | "MPUnit", [_] -> Ok "()"
        | "MPBool", [_; value] -> Ok (if value.GetBoolean() then "true" else "false")
        | "MPInt8", [_; value] -> scalar "y" value
        | "MPUInt8", [_; value] -> scalar "uy" value
        | "MPInt16", [_; value] -> scalar "s" value
        | "MPUInt16", [_; value] -> scalar "us" value
        | "MPInt32", [_; value] -> scalar "l" value
        | "MPUInt32", [_; value] -> scalar "ul" value
        | "MPInt64", [_; value] -> scalar "L" value
        | "MPUInt64", [_; value] -> scalar "UL" value
        | "MPInt128", [_; value] -> scalar "Q" value
        | "MPUInt128", [_; value] -> scalar "Z" value
        | "MPInt", [_; value] -> numberText value
        | "MPString", [_; value] -> stringValue value |> Result.map quoted
        | "MPChar", [_; value] -> stringValue value |> Result.map (fun text -> "'" + text.Replace("'", "\\'") + "'")
        | "MPList", [_; values] ->
            ResultList.mapResults renderMatchPattern (arrayItems values)
            |> Result.map (String.concat ", " >> fun value -> $"[{value}]")
        | "MPListCons", [_; head; tail] ->
            renderMatchPattern head
            |> Result.bind (fun h -> renderMatchPattern tail |> Result.map (fun t -> $"({h} :: {t})"))
        | "MPTuple", [_; first; second; rest] ->
            ResultList.mapResults renderMatchPattern (first :: second :: arrayItems rest)
            |> Result.map (String.concat ", " >> fun value -> $"({value})")
        | "MPEnum", [_; name; values] ->
            stringValue name
            |> Result.bind (fun enumName ->
                ResultList.mapResults renderMatchPattern (arrayItems values)
                |> Result.map (function
                    | [] -> enumName
                    | rendered -> enumName + "(" + String.concat ", " rendered + ")"))
        | "MPOr", [_; values] ->
            ResultList.mapResults renderMatchPattern (arrayItems values)
            |> Result.map (String.concat " | ")
        | _ -> Error $"Unsupported package match pattern {caseName}")

let private infixText element =
    enumCase element
    |> Result.bind (function
        | "BinOp", [operation] ->
            enumCase operation
            |> Result.bind (function
                | "BinOpAnd", [] -> Ok "&&"
                | "BinOpOr", [] -> Ok "||"
                | name, _ -> Error $"Unsupported binary operation {name}")
        | "InfixFnCall", [operation] ->
            enumCase operation
            |> Result.bind (fun (name, _) ->
                match name with
                | "ArithmeticPlus" -> Ok "+"
                | "ArithmeticMinus" -> Ok "-"
                | "ArithmeticMultiply" -> Ok "*"
                | "ArithmeticDivide" -> Ok "/"
                | "ArithmeticModulo" -> Ok "%"
                | "ArithmeticPower" -> Ok "^"
                | "ComparisonGreaterThan" -> Ok ">"
                | "ComparisonGreaterThanOrEqual" -> Ok ">="
                | "ComparisonLessThan" -> Ok "<"
                | "ComparisonLessThanOrEqual" -> Ok "<="
                | "ComparisonEquals" -> Ok "=="
                | "ComparisonNotEquals" -> Ok "!="
                | "StringConcat" -> Ok "++"
                | other -> Error $"Unsupported infix function {other}")
        | name, _ -> Error $"Unsupported infix {name}")

let rec private renderExpr (parameters: string list) (selfName: string) (element: JsonElement) : Result<string, string> =
    let recurse = renderExpr parameters selfName
    let renderMany values = ResultList.mapResults recurse values
    let application fnName typeArgs args =
        ResultList.mapResults renderType (arrayItems typeArgs)
        |> Result.bind (fun types ->
            renderMany (arrayItems args)
            |> Result.map (fun renderedArgs ->
                let appliedName = if List.isEmpty types then fnName else fnName + "<" + String.concat ", " types + ">"
                appliedName + " (" + String.concat ") (" renderedArgs + ")"))
    enumCase element
    |> Result.bind (fun (caseName, fields) ->
        let scalar suffix value = numberText value |> Result.map (fun number -> number + suffix)
        match caseName, fields with
        | "EUnit", [_] -> Ok "()"
        | "EBool", [_; value] -> Ok (if value.GetBoolean() then "true" else "false")
        | "EInt8", [_; value] -> scalar "y" value
        | "EUInt8", [_; value] -> scalar "uy" value
        | "EInt16", [_; value] -> scalar "s" value
        | "EUInt16", [_; value] -> scalar "us" value
        | "EInt32", [_; value] -> scalar "l" value
        | "EUInt32", [_; value] -> scalar "ul" value
        | "EInt64", [_; value] -> scalar "L" value
        | "EUInt64", [_; value] -> scalar "UL" value
        | "EInt128", [_; value] -> scalar "Q" value
        | "EUInt128", [_; value] -> scalar "Z" value
        | "EInt", [_; value] -> numberText value
        | "EFloat", [_; sign; whole; part] ->
            enumCase sign
            |> Result.bind (fun (signName, _) ->
                stringValue whole
                |> Result.bind (fun w ->
                    stringValue part
                    |> Result.map (fun p -> (if signName = "Negative" then "-" else "") + w + "." + p)))
        | "EChar", [_; value] -> stringValue value |> Result.map (fun text -> "'" + text.Replace("'", "\\'") + "'")
        | "EString", [_; segments] ->
            let renderSegment segment =
                enumCase segment
                |> Result.bind (function
                    | "StringText", [text] -> stringValue text |> Result.map escapedStringContents
                    | "StringInterpolation", [expr] -> recurse expr |> Result.map (fun value -> $"{{{value}}}")
                    | name, _ -> Error $"Unsupported string segment {name}")
            ResultList.mapResults renderSegment (arrayItems segments)
            |> Result.map (String.concat "" >> fun value -> $"$\"{value}\"")
        | "EVariable", [_; name] -> stringValue name
        | "EArg", [_; index] ->
            match index.TryGetInt32() with
            | true, i ->
                parameters |> List.tryItem i |> Option.map Ok |> Option.defaultValue (Error $"Invalid package argument index {i}")
            | _ -> Error "Invalid package argument index"
        | "ESelf", [_] -> Ok selfName
        | "EFnName", [_; name]
        | "EValue", [_; name] -> resolvedName name
        | "EList", [_; values] -> renderMany (arrayItems values) |> Result.map (String.concat ", " >> fun value -> $"[{value}]")
        | "ETuple", [_; first; second; rest] ->
            renderMany (first :: second :: arrayItems rest)
            |> Result.map (String.concat ", " >> fun value -> $"({value})")
        | "EDict", [_; entries] ->
            let renderEntry entry =
                match arrayItems entry with
                | [key; value] -> recurse key |> Result.bind (fun k -> recurse value |> Result.map (fun v -> $"{k}: {v}"))
                | _ -> Error "Invalid package dictionary entry"
            ResultList.mapResults renderEntry (arrayItems entries)
            |> Result.map (String.concat "; " >> fun value -> $"Dict {{ {value} }}")
        | "ELet", [_; pattern; value; body] ->
            renderLetPattern pattern
            |> Result.bind (fun pat -> recurse value |> Result.bind (fun v -> recurse body |> Result.map (fun b -> $"(let {pat} = {v} in {b})")))
        | "EIf", [_; condition; yes; no] ->
            recurse condition
            |> Result.bind (fun c ->
                recurse yes
                |> Result.bind (fun y ->
                    enumCase no
                    |> Result.bind (function
                        | "Some", [value] -> recurse value |> Result.map (fun n -> $"(if {c} then {y} else {n})")
                        | "None", [] -> Ok $"(if {c} then {y})"
                        | name, _ -> Error $"Invalid optional else case {name}")))
        | "EInfix", [_; infix; left; right] ->
            infixText infix |> Result.bind (fun op -> recurse left |> Result.bind (fun l -> recurse right |> Result.map (fun r -> $"({l} {op} {r})")))
        | "EApply", [_; fn; typeArgs; args] ->
            enumCase fn
            |> Result.bind (function
                | "EFnName", [_; name] -> resolvedName name |> Result.bind (fun n -> application n typeArgs args)
                | _ -> recurse fn |> Result.bind (fun renderedFn -> application $"({renderedFn})" typeArgs args))
        | "ELambda", [_; patterns; body] ->
            ResultList.mapResults renderLetPattern (arrayItems patterns)
            |> Result.bind (fun pats -> recurse body |> Result.map (fun b -> "(fun " + String.concat " " pats + " -> " + b + ")"))
        | "ERecord", [_; name; typeArgs; values] ->
            resolvedName name
            |> Result.bind (fun typeName ->
                ResultList.mapResults renderType (arrayItems typeArgs)
                |> Result.bind (fun types ->
                    let renderField field =
                        match arrayItems field with
                        | [fieldName; value] -> stringValue fieldName |> Result.bind (fun n -> recurse value |> Result.map (fun v -> $"{n} = {v}"))
                        | _ -> Error "Invalid package record field"
                    ResultList.mapResults renderField (arrayItems values)
                    |> Result.map (fun rendered ->
                        let nameWithTypes = if List.isEmpty types then typeName else typeName + "<" + String.concat ", " types + ">"
                        nameWithTypes + " { " + String.concat "; " rendered + " }")))
        | "ERecordFieldAccess", [_; record; fieldName] ->
            recurse record |> Result.bind (fun r -> stringValue fieldName |> Result.map (fun f -> $"({r}).{f}"))
        | "ERecordUpdate", [_; record; updates] ->
            let renderUpdate update =
                match arrayItems update with
                | [fieldName; value] -> stringValue fieldName |> Result.bind (fun n -> recurse value |> Result.map (fun v -> $"{n} = {v}"))
                | _ -> Error "Invalid package record update"
            recurse record
            |> Result.bind (fun r -> ResultList.mapResults renderUpdate (arrayItems updates) |> Result.map (fun us -> "{ " + r + " with " + String.concat "; " us + " }"))
        | "EEnum", [_; name; typeArgs; enumName; values] ->
            resolvedName name
            |> Result.bind (fun typeName ->
                stringValue enumName
                |> Result.bind (fun leaf ->
                    renderMany (arrayItems values)
                    |> Result.map (function
                        | [] -> $"{typeName}.{leaf}"
                        | rendered -> typeName + "." + leaf + "(" + String.concat ", " rendered + ")")))
        | "EMatch", [_; argument; cases] ->
            let renderCase matchCase =
                match tryField "pat" matchCase, tryField "whenCondition" matchCase, tryField "rhs" matchCase with
                | Some pattern, Some guard, Some body ->
                    renderMatchPattern pattern
                    |> Result.bind (fun p ->
                        enumCase guard
                        |> Result.bind (function
                            | "None", [] -> recurse body |> Result.map (fun b -> $"| {p} -> {b}")
                            | "Some", [condition] -> recurse condition |> Result.bind (fun g -> recurse body |> Result.map (fun b -> $"| {p} when {g} -> {b}"))
                            | name, _ -> Error $"Invalid match guard {name}"))
                | _ -> Error "Invalid package match case"
            recurse argument
            |> Result.bind (fun arg -> ResultList.mapResults renderCase (arrayItems cases) |> Result.map (fun cs -> "(match " + arg + " with " + String.concat " " cs + ")"))
        | "EStatement", [_; first; next] -> recurse first |> Result.bind (fun f -> recurse next |> Result.map (fun n -> $"({f}; {n})"))
        | "EPipe", [_; initial; parts] ->
            let renderPipePart part =
                enumCase part
                |> Result.bind (function
                    | "EPipeVariable", [_; name; args] ->
                        stringValue name
                        |> Result.bind (fun fnName ->
                            renderMany (arrayItems args)
                            |> Result.map (function
                                | [] -> fnName
                                | rendered -> fnName + " (" + String.concat ") (" rendered + ")"))
                    | "EPipeLambda", [_; patterns; body] ->
                        ResultList.mapResults renderLetPattern (arrayItems patterns)
                        |> Result.bind (fun pats -> recurse body |> Result.map (fun b -> "fun " + String.concat " " pats + " -> " + b))
                    | "EPipeInfix", [_; infix; argument] ->
                        infixText infix |> Result.bind (fun op -> recurse argument |> Result.map (fun arg -> $"({op}) ({arg})"))
                    | "EPipeFnCall", [_; name; typeArgs; args] ->
                        resolvedName name |> Result.bind (fun fnName -> application fnName typeArgs args)
                    | "EPipeEnum", [_; name; caseName; values] ->
                        resolvedName name
                        |> Result.bind (fun typeName ->
                            stringValue caseName
                            |> Result.bind (fun leaf ->
                                renderMany (arrayItems values)
                                |> Result.map (function
                                    | [] -> typeName + "." + leaf
                                    | rendered -> typeName + "." + leaf + " (" + String.concat ") (" rendered + ")")))
                    | name, _ -> Error $"Unsupported package pipe part {name}")
            recurse initial
            |> Result.bind (fun first ->
                ResultList.mapResults renderPipePart (arrayItems parts)
                |> Result.map (fun rendered -> "(" + String.concat " |> " (first :: rendered) + ")"))
        | _ -> Error $"Unsupported package expression {caseName}")

let private parseLocatedEntity (kind: ItemKind) (hash: string) (json: string) : Result<LocatedEntity, string> =
    try
        use document = JsonDocument.Parse(json, packageJsonOptions)
        match tryField "entity" document.RootElement, tryField "location" document.RootElement with
        | Some _, Some location ->
            locationName location
            |> Result.map (fun name -> { Kind = kind; Hash = hash; Location = name; Json = json })
        | _ -> Error $"Package server returned an invalid located entity for {hash}"
    with ex -> Error $"Package server returned invalid JSON for {hash}: {ex.Message}"

let private tryPackageHash element : (string * string) option =
    match tryField "name" element, tryField "location" element with
    | Some name, Some location ->
        match enumCase name, enumCase location with
        | Ok ("Package", [hash]), Ok ("Some", [located]) ->
            match enumCase hash, locationName located with
            | Ok ("Hash", [hashText]), Ok name -> stringValue hashText |> Result.toOption |> Option.map (fun value -> value, name)
            | _ -> None
        | _ -> None
    | _ -> None

let private dependencyRefs (json: string) : Result<(string * string) list, string> =
    try
        use document = JsonDocument.Parse(json, packageJsonOptions)
        let rec collect element =
            let own = tryPackageHash element |> Option.toList
            match element.ValueKind with
            | JsonValueKind.Object -> own @ (element.EnumerateObject() |> Seq.collect (fun property -> collect property.Value) |> Seq.toList)
            | JsonValueKind.Array -> own @ (element.EnumerateArray() |> Seq.collect collect |> Seq.toList)
            | _ -> own
        Ok (collect document.RootElement |> List.distinct)
    with ex -> Error $"Could not inspect package dependencies: {ex.Message}"

let private renderEntity (entity: LocatedEntity) : Result<ResolvedSource, string> =
    try
        use document = JsonDocument.Parse(entity.Json, packageJsonOptions)
        match tryField "entity" document.RootElement with
        | None -> Error $"Missing package entity {entity.Hash}"
        | Some value ->
            let parts = entity.Location.Split('.') |> Array.toList
            match List.rev parts with
            | [] | [_] -> Error $"Invalid package location {entity.Location}"
            | leaf :: reversedModule ->
                let moduleName = reversedModule |> List.rev |> String.concat "."
                let prefix body = { Name = $"package:{entity.Location}@{entity.Hash}"; Source = $"module {moduleName}\n{body}" }
                match entity.Kind with
                | PackageFunction ->
                    match tryField "parameters" value, tryField "typeParams" value, tryField "returnType" value, tryField "body" value with
                    | Some parameters, Some typeParams, Some returnType, Some body ->
                        let renderParameter parameter =
                            match tryField "name" parameter, tryField "typ" parameter with
                            | Some name, Some typ ->
                                stringValue name
                                |> Result.bind (fun rawName ->
                                    let sourceName =
                                        rawName
                                        |> NameSyntax.identifierFromText
                                        |> NameSyntax.formatIdentifier
                                    renderType typ
                                    |> Result.map (fun renderedType ->
                                        sourceName, $"({sourceName}: {renderedType})"))
                            | _ -> Error "Invalid package function parameter"
                        ResultList.mapResults renderParameter (arrayItems parameters)
                        |> Result.bind (fun renderedParameters ->
                            ResultList.mapResults stringValue (arrayItems typeParams)
                            |> Result.bind (fun types ->
                                renderType returnType
                                |> Result.bind (fun resultType ->
                                    renderExpr (List.map fst renderedParameters) entity.Location body
                                    |> Result.map (fun renderedBody ->
                                        let typeDecl = if List.isEmpty types then "" else "<" + (types |> List.map (fun t -> "'" + t) |> String.concat ", ") + ">"
                                        let renderedParams = renderedParameters |> List.map snd |> String.concat " "
                                        prefix $"let {leaf}{typeDecl} {renderedParams} : {resultType} = {renderedBody}"))))
                    | _ -> Error $"Invalid package function {entity.Location}"
                | PackageValue ->
                    match tryField "body" value with
                    | Some body -> renderExpr [] entity.Location body |> Result.map (fun rendered -> prefix $"val {leaf} = {rendered}")
                    | None -> Error $"Invalid package value {entity.Location}"
                | PackageType ->
                    match tryField "declaration" value with
                    | None -> Error $"Invalid package type {entity.Location}"
                    | Some declaration ->
                        match tryField "typeParams" declaration, tryField "definition" declaration with
                        | Some typeParams, Some definition ->
                            ResultList.mapResults stringValue (arrayItems typeParams)
                            |> Result.bind (fun types ->
                                enumCase definition
                                |> Result.bind (fun (definitionCase, fields) ->
                                    let typeDecl = if List.isEmpty types then "" else "<" + (types |> List.map (fun t -> "'" + t) |> String.concat ", ") + ">"
                                    match definitionCase, fields with
                                    | "Alias", [target] -> renderType target |> Result.map (fun rendered -> prefix $"type {leaf}{typeDecl} = {rendered}")
                                    | "Record", [recordFields] ->
                                        let renderField field =
                                            match tryField "name" field, tryField "typ" field with
                                            | Some name, Some typ -> stringValue name |> Result.bind (fun n -> renderType typ |> Result.map (fun t -> $"{n}: {t}"))
                                            | _ -> Error "Invalid package record field"
                                        ResultList.mapResults renderField (arrayItems recordFields)
                                        |> Result.map (fun rendered -> prefix ("type " + leaf + typeDecl + " = { " + String.concat "; " rendered + " }"))
                                    | "Enum", [cases] ->
                                        let renderCase caseValue =
                                            match tryField "name" caseValue, tryField "fields" caseValue with
                                            | Some name, Some caseFields ->
                                                stringValue name
                                                |> Result.bind (fun caseName ->
                                                    let renderEnumField field =
                                                        match tryField "typ" field with
                                                        | Some typ -> renderType typ
                                                        | None -> Error "Invalid package enum field"
                                                    ResultList.mapResults renderEnumField (arrayItems caseFields)
                                                    |> Result.map (function
                                                        | [] -> caseName
                                                        | rendered -> caseName + " of " + String.concat " * " rendered))
                                            | _ -> Error "Invalid package enum case"
                                        ResultList.mapResults renderCase (arrayItems cases)
                                        |> Result.map (fun rendered -> prefix ("type " + leaf + typeDecl + " = " + String.concat " | " rendered))
                                    | _ -> Error $"Unsupported package type definition {definitionCase}"))
                        | _ -> Error $"Invalid package type declaration {entity.Location}"
    with ex -> Error $"Could not render package {entity.Location}: {ex.Message}"

let rec private typeNames (typ: AST.Type) : string list =
    match typ with
    | AST.TFunction (parameters, result) -> List.collect typeNames parameters @ typeNames result
    | AST.TTuple elements -> List.collect typeNames elements
    | AST.TRecord (name, arguments)
    | AST.TSum (name, arguments) -> name :: List.collect typeNames arguments
    | AST.TList inner
    | AST.TStream inner -> typeNames inner
    | AST.TDict (key, value) -> typeNames key @ typeNames value
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TInt
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
    | AST.TBool | AST.TFloat64 | AST.TString | AST.TBlob | AST.TChar | AST.TDateTime
    | AST.TUnit | AST.TRuntimeError | AST.TVar _ | AST.TRawPtr -> []

let rec private patternNames (pattern: AST.Pattern) : string list =
    match pattern with
    | AST.PConstructor (name, fields) -> name :: List.collect patternNames fields
    | AST.PTuple patterns | AST.PList patterns -> List.collect patternNames patterns
    | AST.PListCons (head, tail) -> List.collect patternNames head @ patternNames tail
    | AST.POr alternatives -> alternatives |> AST.NonEmptyList.toList |> List.collect patternNames
    | _ -> []

let rec private expressionNames (expr: AST.Expr) : string list =
    let many expressions = List.collect expressionNames expressions
    match expr with
    | AST.BinOp (_, left, right) -> expressionNames left @ expressionNames right
    | AST.UnaryOp (_, inner) -> expressionNames inner
    | AST.Let (_, value, body)
    | AST.RecursiveLet (_, value, body)
    | AST.Sequence (value, body) -> expressionNames value @ expressionNames body
    | AST.If (condition, yes, no) -> expressionNames condition @ expressionNames yes @ expressionNames no
    | AST.Call (name, arguments) -> name :: (arguments |> AST.NonEmptyList.toList |> many)
    | AST.TypeApp (name, types, arguments) -> name :: List.collect typeNames types @ (arguments |> AST.NonEmptyList.toList |> many)
    | AST.TupleLiteral values | AST.ListLiteral values -> many values
    | AST.TupleAccess (value, _) | AST.RecordAccess (value, _) -> expressionNames value
    | AST.DictLiteral (keyType, valueType, entries) ->
        typeNames keyType
        @ typeNames valueType
        @ (entries
           |> List.collect (fun (key, value) -> expressionNames key @ expressionNames value))
    | AST.RecordLiteral (reference, fields) ->
        reference.SourceTypeName :: List.collect typeNames reference.TypeArgs @ (fields |> List.map snd |> many)
    | AST.RecordUpdate (record, updates) -> expressionNames record @ (updates |> List.map snd |> many)
    | AST.Constructor (reference, _, fields) ->
        let declaringType = AST.constructorReferenceTypeName reference |> Option.toList
        declaringType @ many fields
    | AST.Match (scrutinee, cases) ->
        expressionNames scrutinee
        @ (cases
           |> List.collect (fun matchCase ->
               (matchCase.Patterns |> AST.NonEmptyList.toList |> List.collect patternNames)
               @ (matchCase.Guard |> Option.map expressionNames |> Option.defaultValue [])
               @ expressionNames matchCase.Body))
    | AST.Lambda (parameters, returnType, body) ->
        (parameters
         |> AST.NonEmptyList.toList
         |> List.collect (fun parameter -> parameter.SourceAnnotation |> Option.map typeNames |> Option.defaultValue []))
        @ (returnType |> Option.map typeNames |> Option.defaultValue [])
        @ expressionNames body
    | AST.Apply (fn, arguments) | AST.IndirectApply (fn, arguments) ->
        expressionNames fn @ (arguments |> AST.NonEmptyList.toList |> many)
    | AST.FuncRef name -> [name]
    | AST.Closure (name, captures) -> name :: many captures
    | AST.BoundaryRender (_, value) -> expressionNames value
    | AST.InterpolatedString parts ->
        parts
        |> List.collect (function AST.StringText _ -> [] | AST.StringExpr value -> expressionNames value)
    | AST.Var name when name.Contains '.' -> [name]
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.Int8Literal _
    | AST.Int16Literal _ | AST.Int32Literal _ | AST.UInt8Literal _ | AST.UInt16Literal _
    | AST.UInt32Literal _ | AST.UInt64Literal _ | AST.UInt128Literal _ | AST.BigIntLiteral _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _
    | AST.Var _ | AST.RuntimeError _ -> []

let private sourceCandidates (AST.Program topLevels) : string list =
    topLevels
    |> List.collect (function
        | AST.FunctionDef definition ->
            (definition.Params |> AST.NonEmptyList.toList |> List.collect (snd >> typeNames))
            @ typeNames definition.ReturnType
            @ expressionNames definition.Body
        | AST.ValueDef definition -> expressionNames (AST.valueDefBody definition)
        | AST.TypeDef (AST.RecordDef (_, _, fields)) -> fields |> List.collect (snd >> typeNames)
        | AST.TypeDef (AST.SumTypeDef (_, _, variants)) ->
            variants |> List.collect (fun variant -> List.collect typeNames variant.Fields)
        | AST.TypeDef (AST.TypeAlias (_, _, target)) -> typeNames target
        | AST.Expression (_, expression) -> expressionNames expression)
    |> List.filter (fun name -> name.Contains '.')
    |> List.collect (fun name ->
        let parts = name.Split('.') |> Array.toList
        [2 .. List.length parts] |> List.rev |> List.map (fun length -> parts |> List.take length |> String.concat "."))
    |> List.distinct

let resolve (config: Config) (program: AST.Program) : Result<ResolvedSource list, string> =
    use client = new HttpClient()
    client.Timeout <- TimeSpan.FromSeconds 30.0
    let fetchLocated (kind: ItemKind) (hash: string) =
        let path = $"/{kindPath kind}/get/with-location/{Uri.EscapeDataString hash}"
        fetchByHash client config path
        |> Result.bind (function
            | Missing -> Ok None
            | Found json -> parseLocatedEntity kind hash json |> Result.map Some)
    let findRoots () =
        sourceCandidates program
        |> ResultList.collectResults (fun name ->
            allKinds
            |> ResultList.collectResults (fun kind ->
                let path = $"/{kindPath kind}/find/{Uri.EscapeDataString name}"
                findByName client config path
                |> Result.bind (function
                    | Missing -> Ok []
                    | Found json -> parseHashJson json |> Result.map (fun hash -> [kind, hash]))))
        |> Result.map List.distinct
    let rec load pending visited loaded =
        match pending with
        | [] -> Ok (List.rev loaded)
        | (_, hash) :: rest when Set.contains hash visited -> load rest visited loaded
        | (Some kind, hash) :: rest ->
            fetchLocated kind hash
            |> Result.bind (function
                | None -> Error $"Package {kindPath kind} {hash} was not found"
                | Some entity ->
                    dependencyRefs entity.Json
                    |> Result.bind (fun dependencies ->
                        let next = dependencies |> List.map (fun (dependencyHash, _) -> None, dependencyHash)
                        load (next @ rest) (Set.add hash visited) (entity :: loaded)))
        | (None, hash) :: rest ->
            allKinds
            |> ResultList.mapResults (fun kind -> fetchLocated kind hash |> Result.map (fun found -> kind, found))
            |> Result.bind (fun attempts ->
                match attempts |> List.tryPick (fun (kind, found) -> found |> Option.map (fun entity -> kind, entity)) with
                | None -> Error $"Package dependency {hash} was not found"
                | Some (_, entity) ->
                    dependencyRefs entity.Json
                    |> Result.bind (fun dependencies ->
                        let next = dependencies |> List.map (fun (dependencyHash, _) -> None, dependencyHash)
                        load (next @ rest) (Set.add hash visited) (entity :: loaded)))
    findRoots ()
    |> Result.bind (fun roots -> roots |> List.map (fun (kind, hash) -> Some kind, hash) |> fun pending -> load pending Set.empty [])
    |> Result.bind (ResultList.mapResults renderEntity)
