// semantic_reference.fsx - Serialize complete reference semantic values for migration.
#r "../../bin/DarkCompiler/Debug/net11.0/DarkCompiler.dll"
#r "../../bin/Tests/Debug/net11.0/Tests.dll"

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

let parserModule = typeof<LibParser.Parser.ParserState>.Assembly.GetType("LibParser.Parser")
let classificationMethods =
    ["isIntLit"; "canStartAtom"; "canStartPattern"; "closesOrSeparates"; "isRecoveryBarrier"]
    |> List.map (fun name -> parserModule.GetMethod(name, Reflection.BindingFlags.Static ||| Reflection.BindingFlags.NonPublic))
let infixMethod = parserModule.GetMethod("infixOf", Reflection.BindingFlags.Static ||| Reflection.BindingFlags.NonPublic)
let dsl source =
    let node = JsonObject()
    let file = TestDSL.Common.parseTestFile source
    node["sections"] <- encode typeof<TestDSL.Common.Section list> (box (TestDSL.Common.parseSections source))
    node["file"] <- encode typeof<(string * string) list> (box (Map.toList file.Sections))
    node["required"] <- encode typeof<Result<string,string> list> (box (["NAME"; "SOURCE"; "EXPECTED"; "BODY"] |> List.map (fun name -> TestDSL.Common.getRequiredSection name file)))
    node["optional"] <- encode typeof<string option list> (box (["NAME"; "SOURCE"; "EXPECTED"; "BODY"] |> List.map (fun name -> TestDSL.Common.getOptionalSection name file)))
    node["stripped"] <- encode typeof<string list> (box (TestDSL.Common.stripCommentsAndEmpty source))
    node["normalized"] <- encodeString (TestDSL.Common.normalizeLineEndings source)
    node["escaped"] <- encode typeof<Result<string,string>> (box (TestDSL.Common.parseEscapedText source))
    node["syntax"] <- encode typeof<Result<TestDSL.SyntaxFormat.SyntaxTest list,string>> (box (TestDSL.SyntaxFormat.parseSyntaxFileContent "probe" source))
    node :> JsonNode

let astHelpers (source:string) =
    let node = JsonObject()
    let spellings = [source; "a"; "z"; "\uE000"; "\U00010000"; "a"]
    let allocated = [0UL; 1UL; 9223372036854775807UL; 9223372036854775808UL; 18446744073709551600UL]
                    |> List.map (fun first -> AST.allocateFunctionIdsFromOrdinal first spellings |> Map.toList |> List.map (fun (name,id) -> name,AST.functionIdValue id))
    node["allocated"] <- encode typeof<(string * uint64) list list> (box allocated)
    let existing = AST.allocateFunctionIds [AST.functionId 0UL; AST.functionId 10UL; AST.functionId 9223372036854775807UL] spellings
                   |> Map.toList |> List.map (fun (name,id) -> name,AST.functionIdValue id)
    node["existing"] <- encode typeof<(string * uint64) list> (box existing)
    let ordered = [0UL; 1UL; 9223372036854775807UL; 9223372036854775808UL; 18446744073709551615UL]
                  |> List.map AST.functionId |> List.sort |> List.map AST.functionIdValue
    node["ordered"] <- encode typeof<uint64 list> (box ordered)
    node["hashes"] <- encode typeof<int list> (box (["Some"; "None"; "Ok"; "Error"; source] |> List.map (AST.constructorRuntimeIdentity source)))
    let ref = AST.resolvedConstructorReferenceWithTypeArgs source [AST.TInt64; AST.TList AST.TString]
    node["reference"] <- encode typeof<AST.ConstructorReference> (box ref)
    node["typeName"] <- encode typeof<string option> (box (AST.constructorReferenceTypeName ref))
    let letPattern = AST.LPTuple (AST.LPVariable source, AST.LPVariable "x", [AST.LPWildcard; AST.LPVariable "_ignored"])
    node["bindings"] <- encode typeof<string list> (box (AST.letPatternBindings letPattern))
    node["mapped"] <- encode typeof<AST.LetPattern> (box (AST.mapLetPatternBindings (fun name -> name + "!") letPattern))
    let patterns = [AST.LetBinderPatterns [letPattern]; AST.LetBinderPatterns [AST.LPVariable source; AST.LPVariable source];
                    AST.MatchBinderPattern (AST.POr (AST.NonEmptyList.fromList [AST.PVar source; AST.PVar "other"]));
                    AST.MatchBinderPattern (AST.PListCons ([AST.PVar source; AST.PVar "x"], AST.PVar "tail"));
                    AST.MatchBinderPattern (AST.PResolvedConstructor (source, "Case", 2, [AST.PVar source; AST.PVar "x"]))]
    node["validated"] <- encode typeof<Result<string list,string> list> (box (List.map AST.validateBinders patterns))
    let definitions : AST.TypeDef list = [AST.SumTypeDef ("A", [], [({Name=source; Fields=[]} : AST.Variant); ({Name="Case"; Fields=[]} : AST.Variant)]);
                       AST.SumTypeDef ("B", [], [({Name=source; Fields=[]} : AST.Variant)]);
                       AST.SumTypeDef ("A", [], [({Name="Case"; Fields=[]} : AST.Variant)])]
    node["collisions"] <- encode typeof<string list> (box (AST.collidingConstructorCaseNames definitions |> Set.toList))
    let id = AST.constructorId (AST.typeId 1) source 7
    let field = AST.fieldId (AST.typeId 2) 3
    node["identityProjections"] <- encode typeof<bool * string * int * bool * int * string option * string option * string option>
        (box (AST.constructorIdOwner id = AST.typeId 1, AST.constructorIdValue id, AST.constructorRuntimeTag id,
              AST.fieldIdOwner field = AST.typeId 2, AST.fieldRuntimeIndex field,
              AST.bindingDisplayName (AST.bindingId 4), AST.bindingDisplayName (AST.namedBindingId 4 source),
              AST.bindingDisplayName (AST.topLevelValueId source)))
    node :> JsonNode

let names source =
    let node = JsonObject()
    let identifier = NameSyntax.identifierFromText source
    node["identifier"] <- encode typeof<NameSyntax.Identifier> (box identifier)
    node["classify"] <- encode typeof<NameSyntax.IdentifierToken> (box (NameSyntax.classify source))
    node["bare"] <- JsonValue.Create(NameSyntax.isBareIdentifier identifier)
    node["format"] <- encodeString (NameSyntax.formatIdentifier identifier)
    let qualified = NameSyntax.tryParseLegacySpelling source |> Option.map (fun name ->
        NameSyntax.formatQualifiedName name, NameSyntax.segments name,
        NameSyntax.trySplitLast name |> Option.map (fun (prefix, last) -> NameSyntax.formatQualifiedName prefix, last))
    node["qualified"] <- encode typeof<(string * NameSyntax.Identifier list * (string * NameSyntax.Identifier) option) option> (box qualified)
    let header = NameSyntax.tryExtractModuleHeader source |> Option.map (fun (name, body) -> NameSyntax.formatQualifiedName name, body)
    node["header"] <- encode typeof<(string * string) option> (box header)
    node["sourceUnit"] <- encode typeof<Result<string,string>> (box (NameSyntax.sourceUnitName source |> Result.map NameSyntax.sourceUnitNameText))
    let scan = if source.Length = 0 then None else Some (NameSyntax.scanOrdinary source 0)
    node["scan"] <- encode typeof<(NameSyntax.Identifier * int) option> (box scan)
    let quoted = if source.StartsWith "``" then Some (NameSyntax.scanQuoted source 0) else None
    node["quoted"] <- encode typeof<Result<NameSyntax.Identifier * int,string> option> (box quoted)
    node :> JsonNode

let writtenSource source =
    match WrittenParsing.parse LibParser.Validation.Script source with
    | Error error -> encode typeof<Result<unit,string>> (box (Error error : Result<unit,string>))
    | Ok validated ->
        let node = JsonObject()
        node["items"] <- encode typeof<Result<WrittenSource.Item list,string>> (box (WrittenSource.items validated))
        node["names"] <- encode typeof<Result<string list,string>> (box (WrittenSource.qualifiedNames [validated]))
        let units = [false; true] |> List.collect (fun entryRequired ->
            [NameSyntax.SourceUnitPurpose.Executable; NameSyntax.SourceUnitPurpose.Library; NameSyntax.SourceUnitPurpose.Package]
            |> List.map (fun purpose ->
                let checkedUnits = WrittenSource.validateSourceUnits entryRequired [("probe", purpose, validated)]
                checkedUnits |> Result.map (List.map LibParser.Validation.ValidatedSourceFile.toWrittenTypes)))
        node["units"] <- encode typeof<Result<LibParser.WrittenTypes.SourceFile list,string> list> (box units)
        node :> JsonNode

let parserSupport stage source =
    match LibParser.Lexer.tokenize source with
    | Error error -> encode typeof<Result<unit,string>> (box (Error error : Result<unit,string>))
    | Ok (tokens, _) ->
        let tokens = List.toArray tokens
        let scopes = Collections.Generic.Stack<LibParser.Parser.OffsideScope>()
        scopes.Push { stmtCol = -1; stmtExact = false }
        let state : LibParser.Parser.ParserState = {
            toks = tokens; tokenCount = tokens.Length; diagnostics = Collections.Generic.List<LibParser.Parser.Diagnostic>()
            scopes = scopes; matchArms = []; pendingGt = 0
            pendingGtRange = LibParser.WrittenTypes.synthRange; declAnchor = -1
            depth = 0; abandoned = false; steps = 0; interpDepth = 0 }
        if stage = "patterns" then
            let pattern, next = LibParser.Parser.parseMatchPattern state 0
            let result = JsonObject()
            result["pattern"] <- encode typeof<LibParser.WrittenTypes.MatchPattern> (box pattern)
            result["next"] <- scalar "int32" (string next)
            result["diagnostics"] <- encode typeof<LibParser.Parser.Diagnostic list> (box (List.ofSeq state.diagnostics))
            result :> JsonNode
        elif stage = "parameters" || stage = "effects" then
            let value, next =
                if stage = "parameters" then
                    let value, next = LibParser.Parser.parseParam state 0
                    encode typeof<LibParser.WrittenTypes.FnParam> (box value), next
                else
                    let value, next = LibParser.Parser.parseEffectRow state 0
                    encode typeof<LibParser.WrittenTypes.Identifier list option> (box value), next
            let result = JsonObject()
            result["value"] <- value
            result["next"] <- scalar "int32" (string next)
            result["diagnostics"] <- encode typeof<LibParser.Parser.Diagnostic list> (box (List.ofSeq state.diagnostics))
            result :> JsonNode
        elif stage = "bindings" then
            let pattern, next = LibParser.Parser.parseLetPattern state 0
            let result = JsonObject()
            result["pattern"] <- encode typeof<LibParser.WrittenTypes.LetPattern> (box pattern)
            result["next"] <- scalar "int32" (string next)
            result["diagnostics"] <- encode typeof<LibParser.Parser.Diagnostic list> (box (List.ofSeq state.diagnostics))
            result :> JsonNode
        elif stage = "types" then
            let value, next = LibParser.Parser.parseTypeRef state 0
            let result = JsonObject()
            result["type"] <- encode typeof<LibParser.WrittenTypes.TypeReference> (box value)
            result["next"] <- scalar "int32" (string next)
            result["diagnostics"] <- encode typeof<LibParser.Parser.Diagnostic list> (box (List.ofSeq state.diagnostics))
            result :> JsonNode
        else
          let observations = tokens |> Array.mapi (fun index token ->
            let node = JsonObject()
            node["index"] <- scalar "int32" (string index)
            node["found"] <- encodeString (LibParser.Parser.foundDesc state index)
            node["flags"] <- JsonArray(classificationMethods |> List.map (fun method -> JsonValue.Create(unbox<bool> (method.Invoke(null, [|box token.token|]))) :> JsonNode) |> List.toArray)
            node["infix"] <- encode infixMethod.ReturnType (infixMethod.Invoke(null, [|box token.token|]))
            let parts = match token.token with LibParser.Tokenizer.TFloat value -> Some (LibParser.Parser.floatParts state index value) | _ -> None
            node["floatParts"] <- encode typeof<Option<string * string>> (box parts)
            let qualified = match token.token with LibParser.Tokenizer.TIdent _ -> Some (LibParser.Parser.parseQualified state index) | _ -> None
            node["qualified"] <- encode typeof<Option<(LibParser.WrittenTypes.Identifier * LibParser.Tokenizer.TokenRange) list * LibParser.WrittenTypes.Identifier * int>> (box qualified)
            let parameters = if token.token = LibParser.Tokenizer.TLt then Some (LibParser.Parser.parseTypeParams state index) else None
            node["typeParams"] <- encode typeof<Option<(string * LibParser.Tokenizer.TokenRange) list * int>> (box parameters)
            let gt =
                if token.token = LibParser.Tokenizer.TGt || token.token = LibParser.Tokenizer.TShr then
                    state.pendingGt <- 0
                    let first = LibParser.Parser.expectGt state index
                    let second = if state.pendingGt > 0 then Some (LibParser.Parser.expectGt state (snd first)) else None
                    Some (first, second)
                else None
            node["gt"] <- encode typeof<Option<(LibParser.Tokenizer.TokenRange * int) * Option<LibParser.Tokenizer.TokenRange * int>>> (box gt)
            LibParser.Parser.checkBareMinMagnitude state index
            node :> JsonNode)
          LibParser.Parser.validateLiterals state
          let result = JsonObject()
          result["tokens"] <- JsonArray observations
          result["diagnostics"] <- encode typeof<LibParser.Parser.Diagnostic list> (box (List.ofSeq state.diagnostics))
          result :> JsonNode

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
            | "validated" ->
                [LibParser.Validation.Script; LibParser.Validation.Package; LibParser.Validation.Test]
                |> List.map (fun mode -> LibParser.Parser.parseFor mode source |> Result.map LibParser.Validation.ValidatedSourceFile.toWrittenTypes)
                |> box |> encode typeof<Result<LibParser.WrittenTypes.SourceFile, LibParser.Parser.Diagnostic list> list>
            | "rendered" ->
                (LibParser.Parser.parse source).diagnostics |> List.map (LibParser.Parser.renderDiagnostic source)
                |> box |> encode typeof<string list>
            | "ast-helpers" -> astHelpers source
            | "dsl" -> dsl source
            | "formatter" ->
                let formatted = WrittenParsing.parse LibParser.Validation.Script source |> Result.map (fun validated ->
                    let parsed = LibParser.Validation.ValidatedSourceFile.toWrittenTypes validated
                    let printed = WrittenFormatter.format source parsed
                    let reparsed = WrittenParsing.parse LibParser.Validation.Script printed |> Result.toOption |> Option.map (fun value ->
                        let parsed = LibParser.Validation.ValidatedSourceFile.toWrittenTypes value
                        WrittenFormatter.syntaxKey parsed, WrittenFormatter.format printed parsed)
                    WrittenFormatter.syntaxKey parsed, printed, reparsed)
                encode typeof<Result<string * string * (string * string) option,string>> (box formatted)
            | "names" -> names source
            | "written-source" -> writtenSource source
            | "ast" -> encode typeof<LibParser.Parser.ParseResult> (box (LibParser.Parser.parse source))
            | "parser-support" | "patterns" | "types" | "bindings" | "parameters" | "effects" -> parserSupport stage source
            | _ -> failwith $"Unsupported reference observation stage: {stage}"
        let response = JsonObject()
        response["schema"] <- JsonValue.Create 1
        response["stage"] <- JsonValue.Create stage
        response["value"] <- result
        Console.WriteLine(response.ToJsonString())
        requests ()
requests ()
