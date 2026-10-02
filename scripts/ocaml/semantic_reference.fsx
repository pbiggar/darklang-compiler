// semantic_reference.fsx - Serialize complete reference semantic values for migration.
#r "../../bin/DarkCompiler/Debug/net11.0/DarkCompiler.dll"

open System
open System.Globalization
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.FSharp.Reflection

let scalar (kind: string) (value: string) : JsonNode =
    let node = JsonObject()
    node["kind"] <- JsonValue.Create kind
    node["value"] <- JsonValue.Create value
    node

let namedArray (name: string) (values: JsonNode array) : JsonNode =
    let node = JsonObject()
    node[name] <- JsonArray(values)
    node

let rec encode (typ: Type) (value: obj) : JsonNode =
    if typ = typeof<unit> then null
    elif typ = typeof<string> then JsonValue.Create (unbox<string> value)
    elif typ = typeof<bool> then JsonValue.Create (unbox<bool> value)
    elif typ = typeof<float> then
        scalar "float64" ((uint64 (BitConverter.DoubleToInt64Bits (unbox<float> value))).ToString("x16"))
    elif typ = typeof<single> then
        scalar "float32" ((uint32 (BitConverter.SingleToInt32Bits (unbox<single> value))).ToString("x8"))
    elif typ = typeof<char> then scalar "utf16" ((int (unbox<char> value)).ToString("x4"))
    elif typ = typeof<sbyte> then scalar "int8" (string value)
    elif typ = typeof<int16> then scalar "int16" (string value)
    elif typ = typeof<int> then scalar "int32" (string value)
    elif typ = typeof<int64> then scalar "int64" (string value)
    elif typ = typeof<Int128> then scalar "int128" (string value)
    elif typ = typeof<byte> then scalar "uint8" (string value)
    elif typ = typeof<uint16> then scalar "uint16" (string value)
    elif typ = typeof<uint32> then scalar "uint32" (string value)
    elif typ = typeof<uint64> then scalar "uint64" (string value)
    elif typ = typeof<UInt128> then scalar "uint128" (string value)
    elif typ = typeof<System.Numerics.BigInteger> then scalar "bigint" (string value)
    elif typ.IsArray then
        let elementType = typ.GetElementType()
        let values = (value :?> System.Array) |> Seq.cast<obj> |> Seq.map (encode elementType) |> Seq.toArray
        JsonArray(values)
    elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<list<_>> then
        let elementType = typ.GetGenericArguments()[0]
        let values = (value :?> System.Collections.IEnumerable) |> Seq.cast<obj> |> Seq.map (encode elementType) |> Seq.toArray
        JsonArray(values)
    elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
        let entries = value :?> System.Collections.IEnumerable
        entries |> Seq.cast<obj> |> Seq.map (fun entry -> encode (entry.GetType()) entry) |> Seq.toArray |> namedArray "map"
    elif FSharpType.IsTuple typ then
        Array.map2 encode (FSharpType.GetTupleElements typ) (FSharpValue.GetTupleFields value) |> namedArray "tuple"
    elif FSharpType.IsUnion typ then
        let case, fields = FSharpValue.GetUnionFields(value, typ)
        let node = JsonObject()
        node["type"] <- JsonValue.Create (typ.Name.Split('`')[0])
        node["case"] <- JsonValue.Create case.Name
        node["fields"] <- JsonArray(Array.map2 (fun (field: Reflection.PropertyInfo) fieldValue -> encode field.PropertyType fieldValue) (case.GetFields()) fields)
        node
    elif FSharpType.IsRecord typ then
        let node = JsonObject()
        node["record"] <- JsonValue.Create (typ.Name.Split('`')[0])
        node["fields"] <- JsonArray(Array.map2 (fun (field: Reflection.PropertyInfo) fieldValue ->
            JsonArray([| JsonValue.Create(field.Name) :> JsonNode; encode field.PropertyType fieldValue |]) :> JsonNode)
            (FSharpType.GetRecordFields typ) (FSharpValue.GetRecordFields value))
        node
    elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Collections.Generic.KeyValuePair<_, _>> then
        let key = typ.GetProperty("Key")
        let item = typ.GetProperty("Value")
        namedArray "tuple" [| encode key.PropertyType (key.GetValue value); encode item.PropertyType (item.GetValue value) |]
    else failwith $"Missing semantic comparison encoder for {typ.FullName}"

CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
CultureInfo.CurrentUICulture <- CultureInfo.InvariantCulture
let rec requests () =
    match Console.ReadLine() with
    | null -> ()
    | line ->
        let request = JsonNode.Parse line
        let stage = request["stage"].GetValue<string>()
        let source = request["source"].GetValue<string>()
        let result =
            match stage with
            | "tokens" ->
                let value = LibParser.Lexer.tokenize source
                encode (typeof<Result<LibParser.Lexer.SpannedToken list * (LibParser.Tokenizer.TokenRange * string) list, string>>) (box value)
            | _ -> failwith $"Unsupported reference observation stage: {stage}"
        let response = JsonObject()
        response["schema"] <- JsonValue.Create 1
        response["stage"] <- JsonValue.Create stage
        response["value"] <- result
        Console.WriteLine(response.ToJsonString())
        requests ()
requests ()
