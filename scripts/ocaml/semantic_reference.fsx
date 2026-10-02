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

let encodeString (text: string) : JsonNode =
    let rec unpaired index =
        if index >= text.Length then false
        elif Char.IsHighSurrogate text[index] then
            if index + 1 < text.Length && Char.IsLowSurrogate text[index + 1] then unpaired (index + 2)
            else true
        elif Char.IsLowSurrogate text[index] then true
        else unpaired (index + 1)
    if unpaired 0 then
        text |> Seq.map (fun unit -> JsonValue.Create((int unit).ToString("x4")) :> JsonNode) |> Seq.toArray |> namedArray "utf16String"
    else JsonValue.Create text

// Cache reflection readers; complete observations of the source corpus contain
// millions of repeated record/union values. Caching changes only instrumentation.
let encoders = Collections.Generic.Dictionary<Type, obj -> JsonNode>()
let rec encoder (typ: Type) : obj -> JsonNode =
    match encoders.TryGetValue typ with
    | true, fn -> fn
    | _ ->
        let fn : obj -> JsonNode =
            if typ = typeof<unit> then fun _ -> null
            elif typ = typeof<string> then fun value -> encodeString (unbox<string> value)
            elif typ = typeof<bool> then fun value -> JsonValue.Create (unbox<bool> value)
            elif typ = typeof<float> then fun value -> scalar "float64" ((uint64 (BitConverter.DoubleToInt64Bits (unbox<float> value))).ToString("x16"))
            elif typ = typeof<single> then fun value -> scalar "float32" ((uint32 (BitConverter.SingleToInt32Bits (unbox<single> value))).ToString("x8"))
            elif typ = typeof<char> then fun value -> scalar "utf16" ((int (unbox<char> value)).ToString("x4"))
            elif typ = typeof<sbyte> then fun value -> scalar "int8" (string value)
            elif typ = typeof<int16> then fun value -> scalar "int16" (string value)
            elif typ = typeof<int> then fun value -> scalar "int32" (string value)
            elif typ = typeof<int64> then fun value -> scalar "int64" (string value)
            elif typ = typeof<Int128> then fun value -> scalar "int128" (string value)
            elif typ = typeof<byte> then fun value -> scalar "uint8" (string value)
            elif typ = typeof<uint16> then fun value -> scalar "uint16" (string value)
            elif typ = typeof<uint32> then fun value -> scalar "uint32" (string value)
            elif typ = typeof<uint64> then fun value -> scalar "uint64" (string value)
            elif typ = typeof<UInt128> then fun value -> scalar "uint128" (string value)
            elif typ = typeof<System.Numerics.BigInteger> then fun value -> scalar "bigint" (string value)
            elif typ.IsArray then
                let elementType = typ.GetElementType()
                fun value -> JsonArray((value :?> System.Array) |> Seq.cast<obj> |> Seq.map (encode elementType) |> Seq.toArray)
            elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<list<_>> then
                let elementType = typ.GetGenericArguments()[0]
                fun value -> JsonArray((value :?> Collections.IEnumerable) |> Seq.cast<obj> |> Seq.map (encode elementType) |> Seq.toArray)
            elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
                fun value ->
                    (value :?> Collections.IEnumerable) |> Seq.cast<obj> |> Seq.map (fun entry -> encode (entry.GetType()) entry) |> Seq.toArray |> namedArray "map"
            elif FSharpType.IsTuple typ then
                let types = FSharpType.GetTupleElements typ
                let reader = FSharpValue.PreComputeTupleReader typ
                fun value -> Array.map2 encode types (reader value) |> namedArray "tuple"
            elif FSharpType.IsUnion typ then
                let tag = FSharpValue.PreComputeUnionTagReader typ
                let cases = FSharpType.GetUnionCases typ |> Array.map (fun case ->
                    case.Name, (case.GetFields() |> Array.map (fun field -> field.PropertyType)), FSharpValue.PreComputeUnionReader case)
                fun value ->
                    let name, types, reader = cases[tag value]
                    let node = JsonObject()
                    node["type"] <- JsonValue.Create (typ.Name.Split('`')[0])
                    node["case"] <- JsonValue.Create name
                    node["fields"] <- JsonArray(Array.map2 encode types (reader value))
                    node
            elif FSharpType.IsRecord typ then
                let fields = FSharpType.GetRecordFields typ
                let reader = FSharpValue.PreComputeRecordReader typ
                fun value ->
                    let node = JsonObject()
                    node["record"] <- JsonValue.Create (typ.Name.Split('`')[0])
                    node["fields"] <- JsonArray(Array.map2 (fun (field: Reflection.PropertyInfo) fieldValue ->
                        JsonArray([| JsonValue.Create(field.Name) :> JsonNode; encode field.PropertyType fieldValue |]) :> JsonNode)
                        fields (reader value))
                    node
            elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Collections.Generic.KeyValuePair<_, _>> then
                let key, item = typ.GetProperty("Key"), typ.GetProperty("Value")
                fun value -> namedArray "tuple" [| encode key.PropertyType (key.GetValue value); encode item.PropertyType (item.GetValue value) |]
            else failwith $"Missing semantic comparison encoder for {typ.FullName}"
        encoders[typ] <- fn
        fn
and encode (typ: Type) (value: obj) : JsonNode = (encoder typ) value

CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
CultureInfo.CurrentUICulture <- CultureInfo.InvariantCulture
let reader : IO.TextReader =
    match fsi.CommandLineArgs with
    | [| _; path |] -> new IO.StreamReader(path)
    | _ -> Console.In
let rec requests () =
    match reader.ReadLine() with
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
