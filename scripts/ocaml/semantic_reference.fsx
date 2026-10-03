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
            elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Set<_>> then
                let elementType = typ.GetGenericArguments()[0]
                fun value -> (value :?> Collections.IEnumerable) |> Seq.cast<obj> |> Seq.map (encode elementType) |> Seq.toArray |> namedArray "set"
            elif FSharpType.IsTuple typ then
                let types = FSharpType.GetTupleElements typ
                let reader = FSharpValue.PreComputeTupleReader typ
                fun value -> Array.map2 encode types (reader value) |> namedArray "tuple"
            elif FSharpType.IsUnion(typ, Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic) then
                let tag = FSharpValue.PreComputeUnionTagReader(typ, Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
                let cases = FSharpType.GetUnionCases(typ, Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic) |> Array.map (fun case ->
                    case.Name, (case.GetFields() |> Array.map (fun field -> field.PropertyType)), FSharpValue.PreComputeUnionReader(case, Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic))
                fun value ->
                    let name, types, reader = cases[tag value]
                    let node = JsonObject()
                    node["type"] <- JsonValue.Create (typ.Name.Split('`')[0])
                    node["case"] <- JsonValue.Create name
                    node["fields"] <- JsonArray(Array.map2 encode types (reader value))
                    node
            elif FSharpType.IsRecord(typ, Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic) then
                let fields = FSharpType.GetRecordFields(typ, Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
                let reader = FSharpValue.PreComputeRecordReader(typ, Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
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

let resolution source =
    let spellingsMethod = typeof<AST.SemanticType>.Assembly.GetType("NameResolution").GetMethod("candidateSpellings", Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
    let candidateSpellings context scope query = spellingsMethod.Invoke(null,[|box context;box scope;box query|]) :?> string list
    let namespaceKey = function
        | NameResolution.RootNamespace -> ["RootNamespace"]
        | NameResolution.ModuleNamespace path -> "ModuleNamespace" :: AST.NonEmptyList.toList path
        | NameResolution.PackageNamespace (owner, modules) -> "PackageNamespace" :: owner :: modules
        | NameResolution.BuiltinNamespace -> ["BuiltinNamespace"]
    let identityKey = function
        | NameResolution.LocalValue name -> ["LocalValue"; name]
        | NameResolution.ModuleValue (ns, name) -> "ModuleValue" :: name :: namespaceKey ns
        | NameResolution.PackageValue (ns, name) -> "PackageValue" :: name :: namespaceKey ns
        | NameResolution.BuiltinValue (name, version) -> ["BuiltinValue"; name; string version]
        | NameResolution.ModuleFunction (ns, name, id) -> "ModuleFunction" :: name :: id :: namespaceKey ns
        | NameResolution.PackageFunction (ns, name, id) -> "PackageFunction" :: name :: id :: namespaceKey ns
        | NameResolution.BuiltinFunction (name, version) -> ["BuiltinFunction"; name; string version]
        | NameResolution.ConstructorSymbol (owner, caseName) -> ["ConstructorSymbol"; owner; caseName]
        | NameResolution.UserType name -> ["UserType"; name]
        | NameResolution.BuiltinType name -> ["BuiltinType"; name]
    let provenanceKey = function
        | NameResolution.LexicalBinding s -> ["LexicalBinding"; s]
        | NameResolution.SourceDeclaration s -> ["SourceDeclaration"; s]
        | NameResolution.ModuleDeclaration s -> ["ModuleDeclaration"; s]
        | NameResolution.PackageDeclaration s -> ["PackageDeclaration"; s]
        | NameResolution.BuiltinRegistration s -> ["BuiltinRegistration"; s]
        | NameResolution.CompilerExtension s -> ["CompilerExtension"; s]
    let candidateValue (c:NameResolution.Candidate) = NameResolution.qualifiedNameSegments c.VisibleName, identityKey c.Identity, provenanceKey c.Provenance
    let identities = [NameResolution.LocalValue "x"; NameResolution.ModuleValue (NameResolution.RootNamespace,"x"); NameResolution.PackageValue (NameResolution.PackageNamespace ("Owner",["Module"]),"x"); NameResolution.BuiltinValue ("x",-1); NameResolution.ModuleFunction (NameResolution.ModuleNamespace (AST.NonEmptyList.fromList ["A";"B"]),"x",source); NameResolution.PackageFunction (NameResolution.RootNamespace,"x","decl2"); NameResolution.BuiltinFunction ("x",2); NameResolution.ConstructorSymbol ("First","x"); NameResolution.ConstructorSymbol ("Second","x"); NameResolution.UserType "X"; NameResolution.BuiltinType "X"]
    let provenances = [NameResolution.LexicalBinding source; NameResolution.SourceDeclaration source; NameResolution.ModuleDeclaration source; NameResolution.PackageDeclaration source; NameResolution.BuiltinRegistration source; NameResolution.CompilerExtension source]
    let spellings = ["x";"A.x";"A.B.x";source;"Darklang.Stdlib.Option.Option";"Darklang.Stdlib.Option.Option.Some"]
    let all = spellings |> List.collect (fun spelling -> identities |> List.collect (fun identity -> provenances |> List.choose (NameResolution.candidate spelling identity)))
    let baseEnv = NameResolution.addCandidates all NameResolution.empty
    let overlay = NameResolution.addCandidates (all |> List.filter (fun c -> match c.Provenance with NameResolution.SourceDeclaration _ | NameResolution.ModuleDeclaration _ -> true | _ -> false)) NameResolution.empty
    let environments = [NameResolution.empty; baseEnv; overlay; NameResolution.merge baseEnv overlay; NameResolution.filterCandidates (fun c -> c.Identity = NameResolution.ConstructorSymbol ("First","x")) baseEnv; NameResolution.filterCandidates (fun _ -> false) baseEnv; NameResolution.merge baseEnv NameResolution.empty]
    let contexts = [NameResolution.ResolutionContext.Value;NameResolution.ResolutionContext.Callable;NameResolution.ResolutionContext.Constructor;NameResolution.ResolutionContext.Type]
    let queries = [source;"x";"A.x";"A.B.x";"Option";"Option.Some";"Result.Ok";"Stdlib.Option.Option";"missing";"A..B"]
    let scopes = [[];["A"];["A";"B"]]
    let values = environments |> List.map (fun env ->
        NameResolution.candidates env |> List.map candidateValue,
        contexts |> List.collect (fun context -> scopes |> List.collect (fun scope -> queries |> List.map (fun query ->
            candidateSpellings context scope query,
            NameResolution.resolveInModule context scope query env
            |> Result.map (fun r -> NameResolution.qualifiedNameSegments r.OriginalName, NameResolution.contextToString r.Context, identityKey r.Identity, provenanceKey r.Provenance, NameResolution.canonicalSpelling r.Identity)
            |> Result.mapError NameResolution.errorToString))))
    let value = NameResolution.tryQualifiedName source |> Option.map NameResolution.qualifiedNameSegments, values
    encode (value.GetType()) (box value)

let checkingCall<'a> name args : 'a =
    let methodInfo = typeof<AST.SemanticType>.Assembly.GetType("CheckingDiagnostics").GetMethod(name, Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
    methodInfo.Invoke(null,args) :?> 'a
let checkingDiagnostics source =
    let types = [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TChar;AST.TDateTime;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TVar source;AST.TInferenceVar (source,"#infer:id:fixed");AST.TFunction ([AST.TInt64;AST.TVar source],AST.TDict (AST.TString,AST.TVar source));AST.TTuple [AST.TUnit;AST.TList AST.TString];AST.TRecord (source,[]);AST.TRecord (source,[AST.TDict (AST.TChar,AST.TStream AST.TInt64)]);AST.TSum (source,[]);AST.TSum (source,[AST.TVar source]);AST.TList (AST.TDict (AST.TInt64,AST.TString));AST.TStream AST.TFloat64;AST.TDict (AST.TBool,AST.TInt64)]
    let expressions: AST.Expr list = [AST.UnitLiteral;AST.Int64Literal Int64.MinValue;AST.Int128Literal Int128.MinValue;AST.BigIntLiteral (Numerics.BigInteger.One <<< 256);AST.Int8Literal -128y;AST.Int16Literal -32768s;AST.Int32Literal Int32.MinValue;AST.UInt8Literal 255uy;AST.UInt16Literal 65535us;AST.UInt32Literal UInt32.MaxValue;AST.UInt64Literal UInt64.MaxValue;AST.UInt128Literal UInt128.MaxValue;AST.BoolLiteral true;AST.BoolLiteral false;AST.StringLiteral source;AST.CharLiteral source;AST.FloatLiteral -0.0;AST.FloatLiteral infinity;AST.FloatLiteral nan;AST.FloatLiteral 1e16;AST.FloatLiteral 1e-5;AST.TupleLiteral [AST.StringLiteral source;AST.FloatLiteral 1.0];AST.TupleLiteral [AST.Var source];AST.ListLiteral [];AST.ListLiteral [AST.FloatLiteral 1.0];AST.ListLiteral [AST.Var source;AST.TupleLiteral [AST.Int64Literal 1L;AST.StringLiteral source]];AST.Var source]
    let patterns = [AST.LPUnit;AST.LPWildcard;AST.LPVariable source;AST.LPTuple (AST.LPVariable source,AST.LPUnit,[AST.LPTuple (AST.LPWildcard,AST.LPVariable "nested",[])])]
    let errors = (types |> List.collect (fun typ -> [CheckingDiagnostics.TypeMismatch (AST.TBool,typ,source);CheckingDiagnostics.IfBranchTypeMismatch (AST.TBool,typ);CheckingDiagnostics.InvalidOperation (source,[typ;AST.TString]);CheckingDiagnostics.IncompatibleEqualityOperands (typ,AST.TString);CheckingDiagnostics.IncompatibleOrderingOperands (typ,AST.TInt64)])) @ [CheckingDiagnostics.UndefinedVariable source;CheckingDiagnostics.UndefinedCallTarget source;CheckingDiagnostics.MissingTypeAnnotation source;CheckingDiagnostics.PolymorphicRecursion source;CheckingDiagnostics.ResolutionFailure (NameResolution.InvalidQualifiedName (source,NameResolution.ResolutionContext.Callable));CheckingDiagnostics.GenericError source]
    let bound : Map<string,AST.Expr> = Map.ofList [("message",AST.StringLiteral source)]
    let calls : AST.Expr list = [AST.applyNamed "Builtin.unwrap" (AST.NonEmptyList.singleton (AST.Constructor (AST.UnresolvedConstructor None,"Option.None",[])));AST.applyNamed "Builtin.crash" (AST.NonEmptyList.singleton (AST.Var "message"));AST.Let (AST.LPVariable "message",AST.StringLiteral source,AST.applyNamed "Builtin.testRuntimeError" (AST.NonEmptyList.singleton (AST.Var "message")));AST.Var "absent"]
    let names = [source;"Builtin.unwrap";"Builtin.testRuntimeError";"Builtin.crash";"Builtin.testNan";"Builtin.testInfinity";"Builtin.blobEmpty"]
    let typeValues = types |> List.map (fun typ -> CheckingDiagnostics.typeToString typ,CheckingDiagnostics.typeToHelperIdentityString typ,checkingCall<bool> "isNeverType" [|box typ|])
    let errorValues = List.map CheckingDiagnostics.typeErrorToString errors
    let expressionValues = expressions |> List.map (fun expr ->
        checkingCall<string option> "tryFormatLiteralValue" [|box expr|],
        checkingCall<string option> "formatPatternMismatchValue" [|box expr|],
        checkingCall<string> "formatListLiteralForNoMatch" [|box [expr]|],
        types |> List.map (fun typ -> [checkingCall<string> "ifConditionTypeMismatchMessage" [|box expr;box typ|];checkingCall<string> "interpolationTypeMismatchMessage" [|box expr;box typ|];checkingCall<string> "formatPatternMismatchError" [|box expr;box typ;box AST.TString;box (None:string option)|];checkingCall<string> "formatPatternMismatchError" [|box expr;box typ;box AST.TUnit;box (Some source)|];checkingCall<string> "formatLegacyParamTypeError" [|box source;box 4;box "param";box AST.TString;box typ;box expr|]]))
    let patternValues = patterns |> List.map (fun pattern ->
        checkingCall<string> "formatLetDeconstructionPattern" [|box pattern|],
        checkingCall<AST.SemanticType> "inferredLetPatternType" [|box source;box pattern|] |> CheckingDiagnostics.typeToHelperIdentityString,
        types |> List.map (fun typ -> checkingCall<(string * AST.SemanticType) list option> "bindLetPatternTypes" [|box pattern;box typ|] |> Option.map (List.map (fun (name,typ) -> name,CheckingDiagnostics.typeToHelperIdentityString typ))))
    let callValues = calls |> List.map (fun call -> checkingCall<bool> "isKnownFailureConstructorExpr" [|box call|],checkingCall<bool> "isKnownUnwrapFailureExpr" [|box bound;box call|],checkingCall<bool> "isKnownTestRuntimeErrorExpr" [|box bound;box call|],checkingCall<string option> "tryExtractKnownTestRuntimeErrorMessage" [|box bound;box call|])
    let nameValues = names |> List.map (fun name -> checkingCall<string> "withIndefiniteArticle" [|box name|],["isBuiltinUnwrapName";"isBuiltinTestRuntimeErrorName";"isRuntimeFailureName";"isBuiltinTestNanName";"isBuiltinTestInfinityName";"isBuiltinBlobEmptyName"] |> List.map (fun test -> checkingCall<bool> test [|box name|]))
    let freshValues = [None;Some source] |> List.map (fun scope ->
        let fresh,subst = CheckingDiagnostics.freshenTypeParams scope [source;"a";source]
        let keys = fresh |> List.map (fun key -> match CheckingDiagnostics.inferenceVarForKey key with AST.TInferenceVar (display,_) -> display | _ -> failwith "Freshened key is not an inference variable")
        keys,List.distinct fresh |> List.length = 3,fresh |> List.forall (fun key -> let guid = key.Substring(key.Length - 32) in guid.Length = 32 && guid[12] = '4' && List.contains guid[16] ['8';'9';'a';'b']),subst |> Map.toList |> List.map (fun (name,key) -> name,CheckingDiagnostics.typeToString (CheckingDiagnostics.inferenceVarForKey key)))
    let value = typeValues,errorValues,expressionValues,patternValues,callValues,nameValues,freshValues
    encode (value.GetType()) (box value)

let freeVariables source =
    let x = AST.Var source
    let y = AST.Var "y"
    let field = AST.unresolvedRecordFieldReference "field"
    let patterns = [AST.PUnit;AST.PWildcard;AST.PVar source;AST.PConstructor ("C",[AST.PVar source;AST.PVar "y"]);AST.PResolvedConstructor ("M.T","C",3,[AST.PVar source]);AST.PInt64 1L;AST.PBigInt 1I;AST.PInt128Literal (Int128.Parse "1");AST.PInt8Literal 1y;AST.PInt16Literal 1s;AST.PInt32Literal 1;AST.PUInt8Literal 1uy;AST.PUInt16Literal 1us;AST.PUInt32Literal 1ul;AST.PUInt64Literal 1UL;AST.PUInt128Literal (UInt128.Parse "1");AST.PBool true;AST.PString source;AST.PChar source;AST.PFloat 1.0;AST.PTuple [AST.PVar source;AST.PVar "y"];AST.PList [AST.PVar source];AST.PListCons ([AST.PVar source],AST.PVar "tail");AST.POr (AST.NonEmptyList.fromList [AST.PVar source;AST.PVar "other"])]
    let literals: AST.Expr list = [AST.UnitLiteral;AST.Int64Literal 1L;AST.Int128Literal (Int128.Parse "1");AST.BigIntLiteral 1I;AST.Int8Literal 1y;AST.Int16Literal 1s;AST.Int32Literal 1;AST.UInt8Literal 1uy;AST.UInt16Literal 1us;AST.UInt32Literal 1ul;AST.UInt64Literal 1UL;AST.UInt128Literal (UInt128.Parse "1");AST.BoolLiteral true;AST.StringLiteral source;AST.CharLiteral source;AST.FloatLiteral 1.0;AST.RuntimeError source]
    let expressions = literals @ [x;AST.Var "Builtin.testNan";AST.Var "Builtin.testInfinity";AST.BoundaryRender (source,x);AST.BinOp (AST.Add,x,y);AST.UnaryOp (AST.Neg,x);
        AST.Let (AST.LPVariable source,x,AST.TupleLiteral [x;y]);AST.Let (AST.LPTuple (AST.LPVariable source,AST.LPVariable "y",[]),AST.Var "value",AST.TupleLiteral [x;y]);
        AST.RecursiveLet (AST.RecursiveBindingCandidate {SourceName=source;Kind=AST.NamedLocalFunctionMember},AST.Apply (x,[],AST.NonEmptyList.singleton y),AST.TupleLiteral [x;y]);
        AST.If (x,y,AST.Var "z");AST.Sequence (x,y);AST.Apply (x,[],AST.NonEmptyList.fromList [y;AST.Var "z"]);AST.TupleLiteral [x;y];AST.TupleAccess (x,1);
        AST.DictLiteral (AST.TString,AST.TString,[(x,y)]);AST.RecordLiteral (AST.unresolvedRecordReference "R" [],[(field,x)]);AST.RecordUpdate (x,[(field,y)]);AST.RecordAccess (x,field);
        AST.Constructor (AST.UnresolvedConstructor None,"C",[x;y]);AST.ListLiteral [x;y];AST.Lambda (AST.NonEmptyList.singleton (AST.lambdaParameter (AST.LPVariable source)),None,AST.TupleLiteral [x;y]);
        AST.Apply (AST.TupleAccess (x,0),[],AST.NonEmptyList.singleton y);AST.IndirectApply (x,AST.NonEmptyList.singleton y);AST.Closure (source,[x;y]);
        AST.InterpolatedString [AST.StringText source;AST.StringExpr x;AST.StringExpr y]] @ (patterns |> List.map (fun pattern -> AST.Match (AST.Var "scrutinee",[{Patterns=AST.NonEmptyList.singleton pattern;Guard=Some (AST.Var "guard");Body=AST.TupleLiteral [x;y;AST.Var "tail"]}])))
    let scopes = [Set.empty;Set.singleton source;Set.ofList [source;"y";"guard"]]
    let value = patterns |> List.map (CheckedFreeVariables.collectPatternBindings >> Set.toList), expressions |> List.map (fun expr -> scopes |> List.map (fun bound -> CheckedFreeVariables.collectFreeVars expr bound |> Set.toList))
    encode (value.GetType()) (box value)

let functionIdMap source =
    let ids = [0UL;1UL;uint64 Int64.MaxValue;1UL <<< 63;UInt64.MaxValue;1UL] |> List.map AST.functionId
    let entries = ids |> List.mapi (fun index id -> id,source + string index)
    let table = FunctionIdMap.ofList entries
    let overlay = FunctionIdMap.ofArray [|AST.functionId 1UL,"overlay";AST.functionId 2UL,source|]
    let tables = [FunctionIdMap.empty;table;FunctionIdMap.ofSeq entries;FunctionIdMap.remove (AST.functionId 1UL) table;FunctionIdMap.change (AST.functionId 0UL) (fun _ -> None) table;FunctionIdMap.change (AST.functionId 3UL) (fun previous -> Some (Option.defaultValue source previous)) table;FunctionIdMap.merge table overlay;FunctionIdMap.map (fun id value -> string (AST.functionIdValue id) + value) table;FunctionIdMap.filter (fun id _ -> AST.functionIdValue id >= (1UL <<< 63)) table]
    let ordinalEntries entries = entries |> List.map (fun (id,value) -> AST.functionIdValue id,value)
    let values = tables |> List.map (fun table ->
        let iterated = ResizeArray<_>()
        FunctionIdMap.iter (fun id value -> iterated.Add (id,value)) table
        ordinalEntries (FunctionIdMap.toList table),ordinalEntries (FunctionIdMap.toSeq table |> Seq.toList),
        FunctionIdMap.keys table |> Seq.map AST.functionIdValue |> Seq.toList,FunctionIdMap.values table |> Seq.toList,
        FunctionIdMap.count table,FunctionIdMap.isEmpty table,
        ids |> List.map (fun id -> FunctionIdMap.tryFind id table,FunctionIdMap.containsKey id table),
        ordinalEntries (FunctionIdMap.fold (fun state id value -> state @ [(id,value)]) [] table),ordinalEntries (List.ofSeq iterated),
        FunctionIdMap.exists (fun id _ -> AST.functionIdValue id = UInt64.MaxValue) table,FunctionIdMap.forall (fun _ value -> value <> "") table,
        if FunctionIdMap.isEmpty table then None else let id,value = FunctionIdMap.maxKeyValue table in Some (AST.functionIdValue id,value,FunctionIdMap.find id table))
    encode (values.GetType()) (box values)

let checkedAst source =
    let one value = AST.NonEmptyList.singleton value
    let var : AST.Expr = AST.Var "x"
    let unit : AST.Expr = AST.UnitLiteral
    let field index = AST.resolvedRecordFieldReference "R" ("field" + string index) index
    let record : AST.RecordReference = {SourceTypeName = "R"; ResolvedTypeName = "R"; TypeArgs = [AST.TInferenceVar (source,"fixed")]}
    let parameter = AST.inferredLambdaVariable "x" AST.TInt64
    let patterns = [AST.PUnit; AST.PWildcard; AST.PVar "x";
        AST.PResolvedConstructor ("S", "Choice", 17, [AST.PVar "x"]); AST.PInt64 Int64.MinValue;
        AST.PBigInt (Numerics.BigInteger.One <<< 256); AST.PInt128Literal Int128.MinValue;
        AST.PInt8Literal -128y; AST.PInt16Literal -32768s; AST.PInt32Literal Int32.MinValue;
        AST.PUInt8Literal 255uy; AST.PUInt16Literal 65535us; AST.PUInt32Literal UInt32.MaxValue;
        AST.PUInt64Literal UInt64.MaxValue; AST.PUInt128Literal UInt128.MaxValue;
        AST.PBool true; AST.PString source; AST.PChar source; AST.PFloat -0.0;
        AST.PTuple [AST.PVar "x"; AST.PWildcard]; AST.PList [AST.PVar "x"];
        AST.PListCons ([AST.PWildcard], AST.PVar "x"); AST.POr (one (AST.PVar "x"))]
    let expressions : AST.Expr list =
        [unit; AST.Int64Literal Int64.MinValue; AST.Int128Literal Int128.MinValue;
        AST.Int8Literal -128y; AST.Int16Literal -32768s; AST.Int32Literal Int32.MinValue; AST.UInt8Literal 255uy;
        AST.UInt16Literal 65535us; AST.UInt32Literal UInt32.MaxValue; AST.UInt64Literal UInt64.MaxValue; AST.UInt128Literal UInt128.MaxValue;
        AST.BigIntLiteral (Numerics.BigInteger.One <<< 256); AST.BoolLiteral true; AST.StringLiteral source; AST.CharLiteral source; AST.FloatLiteral -0.0;
        AST.InterpolatedString [AST.StringText source; AST.StringExpr var]; AST.BinOp (AST.Add, var, unit); AST.UnaryOp (AST.Not, var);
        AST.Let (AST.LPTuple (AST.LPVariable "x", AST.LPWildcard, [AST.LPUnit]), unit, var);
        AST.Var source; AST.Var "Builtin.testNan"; AST.Var "Builtin.testInfinity"; AST.Var "Builtin.blobEmpty";
        AST.If (var, unit, var); AST.Sequence (unit, var); AST.Apply (var, [], one unit); AST.Apply (var, [AST.TInt64], one unit);
        AST.TupleLiteral [unit; var; unit]; AST.TupleAccess (var, 2); AST.DictLiteral (AST.TString, AST.TInt64, [AST.StringLiteral source, var]);
        AST.RecordLiteral (record, [field 1, var; field 0, unit]); AST.RecordUpdate (var, [field 1, unit]); AST.RecordAccess (var, field 1);
        AST.Constructor (AST.ResolvedConstructor (["a"], "S", [AST.TInt64]), "Choice", [var]);
        AST.ListLiteral [var; unit]; AST.Lambda (one parameter, Some AST.TInt64, var);
        AST.Apply (AST.Lambda (one parameter, None, var), [], one unit); AST.IndirectApply (var, one unit);
        AST.Closure (source, [var]); AST.RuntimeError source; AST.BoundaryRender (source, var);
        AST.TupleLiteral []; AST.TupleLiteral [unit]; AST.Match (unit, []);
        AST.RecordLiteral (record, []); AST.RecordAccess (var, AST.unresolvedRecordFieldReference "field");
        AST.Constructor (AST.UnresolvedConstructor None, "Absent", []); AST.Lambda (one (AST.lambdaParameter (AST.LPVariable "x")), None, var);
        AST.Apply (unit, [AST.TUnit], one unit)] @
        (patterns |> List.map (fun pattern -> AST.Match (var, [{Patterns = one pattern; Guard = Some var; Body = var}])))
    let lookup : Map<string,string * string list * int * AST.SemanticType list> = Map.ofList ["S.Choice", ("S", [], 17, [])]
    let catalog = CheckedAST.includeFunctionNames [source; "z"; "aa"; "z"] CheckedAST.emptyFunctionCatalog
    let methodInfo = typeof<AST.SemanticType>.Assembly.GetType("CheckedAST").GetMethod("ofTypedProgram", Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
    let convert (topLevels : AST.TopLevel list) : Result<CheckedAST.Program,string> =
        methodInfo.Invoke(null,[|box lookup; box (Set.singleton "external"); box CheckedAST.emptyTypeCatalog; box catalog; box (fun name -> if name = "R" then Some 2 else None); box (AST.Program (AST.TypeDef (AST.RecordDef ("R",[],["field0",AST.TUnit; "field1",AST.TInt64])) :: AST.TypeDef (AST.SumTypeDef ("S",[],[{Name = "Choice"; Fields = []}])) :: topLevels))|]) :?> Result<CheckedAST.Program,string>
    let converted = expressions |> List.map (fun expr -> convert [AST.Expression ([],expr)])
    let definition : AST.FunctionDef = {Name = source; TypeParams = ["a"]; Params = one ("x",AST.TInferenceVar (source,"fixed")); ReturnType = AST.TInt64; Body = AST.Let (AST.LPVariable "local",unit,var); Recursion = None}
    let declarations = convert [AST.TypeDef (AST.RecordDef ("R",["a"],["field0",AST.TVar "a"; "field1",AST.TInt64]));
        AST.TypeDef (AST.SumTypeDef ("S",[],[{Name = "Choice"; Fields = []}])); AST.TypeDef (AST.TypeAlias ("A",[],AST.TInferenceVar (source,"fixed")));
        AST.FunctionDef definition; AST.ValueDef (AST.CheckedValueDef ("v",AST.TInt64,AST.Var source)); AST.Expression ([],AST.Var "v")]
    let owner = AST.typeId 5
    let recordCases = [0;1;2;64;65] |> List.collect (fun count ->
        let entries = List.init count (fun index -> AST.fieldId owner index,index)
        [CheckedAST.completeRecordFields owner count entries; CheckedAST.completeRecordFields owner count (List.rev entries);
         CheckedAST.completeRecordFields owner count ((AST.fieldId owner 0,99)::entries);
         CheckedAST.completeRecordFields owner count [AST.fieldId (AST.typeId 6) 0,99]])
    let orderingTypes = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TFunction ([AST.TVar source], AST.TInt64); AST.TTuple []; AST.TRecord (source, []); AST.TSum (source, []); AST.TList AST.TInt64; AST.TStream AST.TInt64; AST.TVar source; AST.TInferenceVar (source, "fixed"); AST.TInternalRawPtr; AST.TDict (AST.TString, AST.TInt64); AST.TRecord ("aa", []); AST.TRecord ("z", []); AST.TTuple [AST.TInt64]]
    let ordering = orderingTypes |> List.collect (fun left -> orderingTypes |> List.map (fun right -> sign (compare left right)))
    let value = ordering,CheckedAST.semanticMetadata (CheckedAST.emptySymbols()),catalog,converted,declarations,convert [AST.ValueDef (AST.UncheckedValueDef (source,unit))],recordCases
    encode (value.GetType()) (box value)

let typesCall<'a> name args : 'a =
    let methodInfo = typeof<AST.SemanticType>.Assembly.GetType("CheckingTypes").GetMethod(name, Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
    methodInfo.Invoke(null,args) :?> 'a
let additionalTypedExpressions source =
    let x = AST.Var source
    let y = AST.Var "y"
    let field = AST.unresolvedRecordFieldReference "field"
    let patterns = [AST.PUnit;AST.PWildcard;AST.PVar source;AST.PConstructor ("C",[AST.PVar source;AST.PVar "y"]);AST.PResolvedConstructor ("M.T","C",3,[AST.PVar source]);AST.PInt64 1L;AST.PBigInt 1I;AST.PInt128Literal (Int128.Parse "1");AST.PInt8Literal 1y;AST.PInt16Literal 1s;AST.PInt32Literal 1;AST.PUInt8Literal 1uy;AST.PUInt16Literal 1us;AST.PUInt32Literal 1ul;AST.PUInt64Literal 1UL;AST.PUInt128Literal (UInt128.Parse "1");AST.PBool true;AST.PString source;AST.PChar source;AST.PFloat 1.0;AST.PTuple [AST.PVar source;AST.PVar "y"];AST.PList [AST.PVar source];AST.PListCons ([AST.PVar source],AST.PVar "tail");AST.POr (AST.NonEmptyList.fromList [AST.PVar source;AST.PVar "other"])]
    let literals: AST.Expr list = [AST.UnitLiteral;AST.Int64Literal 1L;AST.Int128Literal (Int128.Parse "1");AST.BigIntLiteral 1I;AST.Int8Literal 1y;AST.Int16Literal 1s;AST.Int32Literal 1;AST.UInt8Literal 1uy;AST.UInt16Literal 1us;AST.UInt32Literal 1ul;AST.UInt64Literal 1UL;AST.UInt128Literal (UInt128.Parse "1");AST.BoolLiteral true;AST.StringLiteral source;AST.CharLiteral source;AST.FloatLiteral 1.0;AST.RuntimeError source]
    let expressions = literals @ [x;AST.Var "Builtin.testNan";AST.Var "Builtin.testInfinity";AST.BoundaryRender (source,x);AST.BinOp (AST.Add,x,y);AST.UnaryOp (AST.Neg,x);
        AST.Let (AST.LPVariable source,x,AST.TupleLiteral [x;y]);AST.Let (AST.LPTuple (AST.LPVariable source,AST.LPVariable "y",[]),AST.Var "value",AST.TupleLiteral [x;y]);
        AST.RecursiveLet (AST.RecursiveBindingCandidate {SourceName=source;Kind=AST.NamedLocalFunctionMember},AST.Apply (x,[],AST.NonEmptyList.singleton y),AST.TupleLiteral [x;y]);
        AST.If (x,y,AST.Var "z");AST.Sequence (x,y);AST.Apply (x,[],AST.NonEmptyList.fromList [y;AST.Var "z"]);AST.TupleLiteral [x;y];AST.TupleAccess (x,1);
        AST.DictLiteral (AST.TString,AST.TString,[(x,y)]);AST.RecordLiteral (AST.unresolvedRecordReference "R" [],[(field,x)]);AST.RecordUpdate (x,[(field,y)]);AST.RecordAccess (x,field);
        AST.Constructor (AST.UnresolvedConstructor None,"C",[x;y]);AST.ListLiteral [x;y];AST.Lambda (AST.NonEmptyList.singleton (AST.lambdaParameter (AST.LPVariable source)),None,AST.TupleLiteral [x;y]);
        AST.Apply (AST.TupleAccess (x,0),[],AST.NonEmptyList.singleton y);AST.IndirectApply (x,AST.NonEmptyList.singleton y);AST.Closure (source,[x;y]);
        AST.InterpolatedString [AST.StringText source;AST.StringExpr x;AST.StringExpr y]] @ (patterns |> List.map (fun pattern -> AST.Match (AST.Var "scrutinee",[{Patterns=AST.NonEmptyList.singleton pattern;Guard=Some (AST.Var "guard");Body=AST.TupleLiteral [x;y;AST.Var "tail"]}])))
    expressions
let checkingTypes source =
    let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
        AST.TVar source; AST.TVar "a"; AST.TInferenceVar (source,"a"); AST.TFunction ([AST.TVar "a"],AST.TVar "b"); AST.TTuple [AST.TVar "b";AST.TVar "a"];
        AST.TRecord ("Outer",[AST.TVar "b"]); AST.TSum ("Outer",[AST.TVar "b"]); AST.TRecord ("Outer",[]); AST.TSum ("Outer",[]);
        AST.TRecord ("Outer",[AST.TUnit;AST.TBool]); AST.TSum ("Outer",[AST.TUnit;AST.TBool]); AST.TList (AST.TVar "a"); AST.TStream (AST.TVar "a"); AST.TDict (AST.TVar "a",AST.TVar "b"); AST.TRecord ("S",[]); AST.TSum ("R",[AST.TVar "a"])]
    let substitutions = [Map.empty; Map.ofList ["a",AST.TVar "b";"b",AST.TInt64]; Map.ofList ["a",AST.TVar "b";"b",AST.TVar "a"]; Map.ofList ["a",AST.TList (AST.TVar "a")]]
    let aliases = Map.ofList ["Outer",(["a"],AST.TRecord ("Inner",[AST.TString;AST.TVar "a"])); "Inner",(["a";"b"],AST.TRecord ("R",[AST.TVar "a";AST.TVar "b"])); "Plain",([],AST.TRecord ("R",[])); "Number",([],AST.TInt64)]
    let lookup : CheckingTypes.VariantLookup = Map.ofList ["S.Second",("S",[],2,[AST.TString]); "S.First",("S",[],0,[]); "S.FirstAlias",("S",[],0,[AST.TBool]); "First",("S",[],0,[]); "T.First",("T",[],0,[AST.TInt64])]
    let registry = Map.ofList ["R",["x",AST.TSum ("R",[AST.TVar "a"]); "x",AST.TBool; "y",AST.TRecord ("S",[AST.TVar "b"])]]
    let indexed = CheckingTypes.indexTypeRegistry lookup (Map.ofList ["R",["a";"b"]]) registry
    let perType = samples |> List.map (fun typ ->
        (substitutions |> List.map (fun subst -> CheckingTypes.applySubst subst typ,typesCall<AST.SemanticType> "applyTypeArguments" [|box subst;box typ|])),
        CheckingTypes.collectTypeVarsInType typ ["existing"],typesCall<AST.SemanticType> "resolveAliasTargetType" [|box aliases;box typ|],
        CheckingTypes.resolveType aliases typ,typesCall<AST.SemanticType> "canonicalizeBareSumTypeRefsWithNames" [|box (Set.singleton "S");box typ|],
        (let methodInfo = typeof<AST.SemanticType>.Assembly.GetType("CheckingTypes").GetMethod("canonicalizeDeclaredTypeRefsWithSumTypeNames", Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
         methodInfo.MakeGenericMethod([|typeof<(string * AST.SemanticType) list>|]).Invoke(null,[|box registry;box (Set.singleton "S");box typ|]) :?> AST.SemanticType),
        (samples |> List.map (fun other -> CheckingTypes.typesEqual aliases typ other)))
    let exprs : AST.Expr list = additionalTypedExpressions source @ [AST.Apply (AST.Var source,[AST.TVar "a"],AST.NonEmptyList.singleton (AST.Var "x"));
        AST.Lambda (AST.NonEmptyList.singleton {Pattern = AST.LPVariable "x";SourceAnnotation = Some (AST.TVar "a");InferredType = Some (AST.TVar "b")},Some (AST.TVar "a"),AST.Var "x");
        AST.RecordLiteral ({SourceTypeName = "Outer";ResolvedTypeName = "R";TypeArgs = [AST.TVar "a"]},[]);
        AST.DictLiteral (AST.TVar "a",AST.TVar "b",[AST.Var source,AST.UnitLiteral]);
        AST.Constructor (AST.ResolvedConstructor ([],"S",[AST.TVar "a"]),"First",[])]
    let arities = [0;1;2;3] |> List.collect (fun expected -> [0;1;2;3] |> List.map (fun actual ->
        let parameters = List.init expected (fun index -> "a" + string index)
        let args = List.init actual (fun _ -> AST.TInt64)
        typesCall<Result<CheckingTypes.Substitution,string>> "buildRecordFieldSubstitutionFromParams" [|box parameters;box args|],CheckingTypes.buildSubstitution parameters args,
        typesCall<string> "formatTypeArgumentArityError" [|box source;box expected;box actual|],typesCall<string> "formatValueArgumentArityError" [|box source;box expected;box actual|]))
    let reference : AST.RecordReference = {SourceTypeName = "Outer";ResolvedTypeName = "ignored";TypeArgs = [AST.TInt64]}
    let legacyExprs : AST.Expr list = [AST.StringLiteral source;AST.Var source;AST.FloatLiteral -0.0]
    let value = perType,indexed,typesCall<CheckingTypes.IndexedSumTypeRegistry> "indexSumTypeRegistry" [|box lookup|],
                CheckingTypes.resolveAliasesInTypeRegistry aliases registry,arities,(exprs |> List.map (CheckingTypes.applySubstToExpr (List.item 1 substitutions))),
                typesCall<(string * (string * AST.SemanticType) list) option> "tryResolveGenericRecordAliasFields" [|box aliases;box indexed;box "Outer"|],
                typesCall<(string * AST.SemanticType list * CheckingTypes.RecordTypeInfo) option> "tryResolveRecordLiteralInfo" [|box aliases;box indexed;box reference|],
                CheckingTypes.resolveTypeName aliases "Plain",typesCall<int> "unqualifiedVariantOwnerCount" [|box "First";box lookup|],
                (legacyExprs |> List.map (fun expr -> typesCall<string> "formatLegacyRecordFieldTypeError" [|box aliases;box source;box AST.TString;box (AST.TRecord ("Number",[]));box expr|]))
    encode (value.GetType()) (box value)

let unification source =
    let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
        AST.TVar source; AST.TInferenceVar (source,"fixed"); AST.TVar "t$empty"; AST.TFunction ([AST.TVar "a"],AST.TVar "b"); AST.TTuple [AST.TVar "a";AST.TInt64];
        AST.TRecord ("R",[AST.TVar "a"]); AST.TSum ("R",[AST.TVar "a"]); AST.TList (AST.TVar "a"); AST.TStream (AST.TVar "a"); AST.TDict (AST.TVar "a",AST.TVar "b");
        AST.TFunction ([AST.TInt64],AST.TBool); AST.TTuple [AST.TList (AST.TVar "t$empty");AST.TInt]; AST.TTuple [AST.TList (AST.TVar "a");AST.TInt]; AST.TList AST.TInt64; AST.TStream AST.TInt64;
        AST.TRecord ("R",[]); AST.TSum ("S",[]); AST.TFunction ([],AST.TUnit)]
    let aliases : CheckingTypes.AliasRegistry = Map.ofList ["Alias",([],AST.TRecord ("R",[]))]
    let pairs = samples |> List.collect (fun left -> samples |> List.map (fun right ->
        TypeUnification.matchConcrete left right,TypeUnification.matchTypes left right,
        TypeUnification.unifyTypes left right,TypeUnification.typesCompatible left right,TypeUnification.typesCompatibleWithAliases aliases left right,
        TypeUnification.reconcileTypes None left right,TypeUnification.reconcileTypes (Some aliases) left right))
    let cases = (List.zip samples (List.rev samples) |> List.map (fun (left,right) -> [source,left;source,right;"tail",AST.TInt64])) @
                [["a",AST.TList (AST.TVar "b$0");"a",AST.TList (AST.TVar "b")];
                 ["a",AST.TList (AST.TVar "b");"a",AST.TList (AST.TVar "b$0")];
                 ["a",AST.TRecord ("R",[AST.TVar "b$0"]);"a",AST.TRecord ("R",[AST.TVar "b"])];
                 ["a",AST.TInt64;"a",AST.TString]]
    let inference = (samples |> List.map (fun actual -> TypeUnification.inferTypeArgs ["a";"b";source] [AST.TVar "a"] [actual] (Some (AST.TVar "b")) (Some AST.TString))) @
                    [TypeUnification.inferTypeArgs ["a"] [AST.TVar "a"] [] None None;
                     TypeUnification.inferTypeArgs ["a"] [AST.TVar "a";AST.TVar "a"] [AST.TInt64;AST.TString] None None]
    let value = TypeUnification.emptyListElementVar,([source;"#infer:fixed";"t$empty";"binding_x";"__x";"recursiveParameter0";"a"] |> List.map TypeUnification.isInferenceVar),
                (samples |> List.map (fun typ -> (match typ with TypeUnification.UnificationVar name -> Some name | _ -> None),TypeUnification.containsTVar typ)),
                pairs,(cases |> List.map TypeUnification.consolidateBindings),inference,
                TypeUnification.tryLookupResolved source (Map.ofList [source,AST.TInt64]),TypeUnification.tryLookupResolved source (Map.empty<string,AST.SemanticType>),
                ([-2147483648; -1; 0; 1; 2; 3; 2147483647] |> List.map (fun index ->
                    let methodInfo = typeof<AST.SemanticType>.Assembly.GetType("CheckExpressionSupport").GetMethod("paramNameForLegacyError", Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
                    methodInfo.Invoke(null,[|box (Map.ofList [source,["first";"second"]]);box source;box index|]) :?> string))
    encode (value.GetType()) (box value)

let structuralFormat source =
    let scalar = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr]
    let rec nested depth value = if depth = 0 then value else nested (depth - 1) (AST.TList value)
    let samples =
        scalar @ [AST.TVar source; AST.TInferenceVar (source,"fixed"); AST.TFunction ([AST.TVar source;AST.TInt64],AST.TList AST.TString);
        AST.TTuple scalar; AST.TRecord (source,scalar); AST.TSum (source,scalar); AST.TList (AST.TVar source); AST.TStream (AST.TVar source);
        AST.TDict (AST.TRecord (source,[]),AST.TSum (source,[AST.TVar source]));
        AST.TTuple (List.init 100 (fun _ -> AST.TUnit)); AST.TTuple (List.init 101 (fun _ -> AST.TUnit));
        nested 99 AST.TUnit; nested 100 AST.TUnit; nested 101 AST.TUnit] @
        (if source = "" then [AST.TTuple (List.init 100 (fun _ -> AST.TTuple (List.init 100 (fun _ -> AST.TUnit))))] else []) @
        ([60;61;62;63;64;65;70;75;79;80;81] |> List.collect (fun length -> [AST.TRecord (String('x',length),[AST.TUnit;AST.TList AST.TInt64]); AST.TFunction ([AST.TVar (String('x',length))],AST.TString)]))
    samples |> List.map (sprintf "%A") |> box |> encode typeof<string list>

let comparisonCall name args =
    let methodInfo = typeof<AST.SemanticType>.Assembly.GetType("ComparisonPlanning").GetMethod(name, Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
    encode methodInfo.ReturnType (methodInfo.Invoke(null,args))
let comparison source =
    let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
        AST.TVar source; AST.TInferenceVar (source,"fixed"); AST.TFunction ([AST.TInt64],AST.TString); AST.TFunction ([AST.TBlob],AST.TNever); AST.TTuple [AST.TInt64;AST.TString];
        AST.TRecord ("R",[]); AST.TRecord ("R",[AST.TUnit]); AST.TRecord ("Recursive",[]); AST.TRecord ("Opaque",[]); AST.TRecord ("Missing",[]); AST.TRecord ("S",[]);
        AST.TSum ("S",[]); AST.TSum ("BadSum",[]); AST.TSum ("Generic",[AST.TString]); AST.TSum ("Generic",[]); AST.TSum ("Uuid",[]);
        AST.TList AST.TInt64; AST.TStream AST.TNever; AST.TDict (AST.TString,AST.TInt64); AST.TDict (AST.TInt64,AST.TString); AST.TDict (AST.TBlob,AST.TString)]
    let lookup : CheckingTypes.VariantLookup = Map.ofList ["S.C",("S",[],0,[AST.TInt64]); "BadSum.C",("BadSum",[],0,[AST.TInternalRawPtr]); "Generic.C",("Generic",["a"],0,[AST.TVar "a"])]
    let raw = Map.ofList ["R",["field",AST.TInt64]; "Recursive",["next",AST.TRecord ("Recursive",[])]; "Opaque",["field",AST.TStream AST.TInt64]]
    let registry = CheckingTypes.indexTypeRegistry lookup (Map.ofList ["R",[];"Recursive",[];"Opaque",[]]) raw
    let sums : CheckingTypes.IndexedSumTypeRegistry = typesCall "indexSumTypeRegistry" [|box lookup|]
    let aliases : CheckingTypes.AliasRegistry = Map.ofList ["Alias",([],AST.TRecord ("R",[]))]
    let left : AST.Expr = AST.Var source
    let right : AST.Expr = AST.UnitLiteral
    let baseArgs = [|box aliases;box registry;box sums|]
    let array values = JsonArray(Array.ofList values) :> JsonNode
    let tuple values = namedArray "tuple" (Array.ofList values)
    let encodeExpr value = encode typeof<AST.Expr> (box value)
    let planType = typeof<AST.SemanticType>.Assembly.GetType("ComparisonPlanning+InternalTypeApp")
    let case = FSharpType.GetUnionCases(planType,Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)[0]
    let perType = samples |> List.map (fun typ ->
        let dispatch = FSharpValue.MakeUnion(case,[|box typ;box left;box right|],Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
        let methodInfo = typeof<AST.SemanticType>.Assembly.GetType("ComparisonPlanning").GetMethod("makeInternalTypeApp",Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
        let internalExpr = methodInfo.Invoke(null,[|dispatch|]) :?> AST.Expr
        tuple [comparisonCall "canonicalEqualityType" [|box lookup;box typ|]; comparisonCall "needsEqHelperForResolvedType" [|box lookup;box typ|];
            encodeString (ComparisonPlanning.eqHelperName typ); encodeString (ComparisonPlanning.compareHelperName typ); encodeExpr internalExpr;
            comparisonCall "tryDecodeInternalTypeApp" [|box internalExpr|]; comparisonCall "buildEqExprForType" [|box aliases;box lookup;box typ;box left;box right|];
            comparisonCall "canonicalSortableType" (Array.append baseArgs [|box typ|]); comparisonCall "dictKeyAdmissibleType" (Array.append baseArgs [|box typ|]);
            comparisonCall "validateJsonTargetType" [|box aliases;box registry;box lookup;box sums;box typ|];
            [source;"Dict.set";"Dict.__internal";"Darklang.Stdlib.Dict.set";"Darklang.Stdlib.Dict.__internal"] |> List.map (fun name -> comparisonCall "validateDictKeyCall" (Array.append baseArgs [|box name;box [typ]|])) |> array;
            [source;"__compare";"Darklang.Stdlib.List.sort";"Darklang.Stdlib.List.unique"] |> List.map (fun name -> comparisonCall "validateCanonicalSortableCall" (Array.append baseArgs [|box name;box [typ]|])) |> array;
            [AST.Lt;AST.Gt;AST.Lte;AST.Gte] |> List.map (fun op -> comparisonCall "buildOrderingExprForType" [|box op;box typ;box left;box right|]) |> array])
    let varying = [AST.TVar source; AST.TInferenceVar (source,"fixed")]
    let pairs = if source = "" then samples |> List.collect (fun left -> samples |> List.map (fun right -> left,right))
                else varying |> List.collect (fun left -> samples |> List.collect (fun right -> [left,right;right,left]))
    let comparisons = pairs |> List.map (fun (left,right) ->
        [AST.Eq;AST.Neq;AST.Lt;AST.Gt;AST.Lte;AST.Gte] |> List.map (fun op -> comparisonCall "classifyComparison" [|box aliases;box registry;box lookup;box sums;box op;box left;box right|]) |> array)
    tuple [array perType;array comparisons;comparisonCall "chainAndExpr" [|box ([]:AST.Expr list)|];comparisonCall "chainAndExpr" [|box [left;right;left]|];
        comparisonCall "sumTypeHasPayload" [|box lookup;box "S"|];comparisonCall "sumTypeHasPayload" [|box lookup;box "Absent"|];comparisonCall "tryDecodeInternalTypeApp" [|box left|];
        comparisonCall "validateCanonicalSortableCall" (Array.append baseArgs [|box "Darklang.Stdlib.List.sortBy";box [AST.TInt64;AST.TBlob]|]);
        comparisonCall "validateCanonicalSortableCall" (Array.append baseArgs [|box "Darklang.Stdlib.List.uniqueBy";box [AST.TInt64;AST.TBlob]|])]

let structuralHelpers source =
    let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
        AST.TVar source; AST.TInferenceVar (source,"fixed"); AST.TFunction ([AST.TInt64],AST.TString); AST.TFunction ([AST.TBlob],AST.TNever); AST.TTuple [AST.TInt64;AST.TString];
        AST.TRecord ("R",[]); AST.TRecord ("R",[AST.TUnit]); AST.TRecord ("Recursive",[]); AST.TRecord ("Opaque",[]); AST.TRecord ("Missing",[]); AST.TRecord ("S",[]);
        AST.TSum ("S",[]); AST.TSum ("BadSum",[]); AST.TSum ("Generic",[AST.TString]); AST.TSum ("Generic",[]); AST.TSum ("Uuid",[]);
        AST.TList AST.TInt64; AST.TStream AST.TNever; AST.TDict (AST.TString,AST.TInt64); AST.TDict (AST.TInt64,AST.TString); AST.TDict (AST.TBlob,AST.TString)]
    let lookup : CheckingTypes.VariantLookup = Map.ofList ["S.C",("S",[],0,[AST.TInt64]); "BadSum.C",("BadSum",[],0,[AST.TInternalRawPtr]); "Generic.C",("Generic",["a"],0,[AST.TVar "a"])]
    let raw = Map.ofList ["R",["field",AST.TInt64]; "Recursive",["next",AST.TRecord ("Recursive",[])]; "Opaque",["field",AST.TStream AST.TInt64]]
    let aliases : CheckingTypes.AliasRegistry = Map.ofList ["Alias",([],AST.TRecord ("R",[]))]
    let left : AST.Expr = AST.Var source
    let right : AST.Expr = AST.UnitLiteral
    let samples = samples @ [AST.TRecord ("GenericRecord",[AST.TInt64;AST.TString]); AST.TRecord ("GenericRecord",[]); AST.TSum ("Multiple",[AST.TInt64])]
    let raw = Map.add "GenericRecord" ["z",AST.TVar "a"; "aa",AST.TVar "b"; source,AST.TList (AST.TVar "a")] raw
    let registry = CheckingTypes.indexTypeRegistry lookup (Map.ofList ["R",[];"Recursive",[];"Opaque",[];"GenericRecord",["a";"b"]]) raw
    let lookup = lookup |> Map.add ("Multiple." + source) ("Multiple",["a"],7,[AST.TVar "a";AST.TList AST.TString])
                        |> Map.add "Multiple.z" ("Multiple",["a"],3,[]) |> Map.add "Multiple.aa" ("Multiple",["a"],1,[AST.TString])
    let sums : CheckingTypes.IndexedSumTypeRegistry = typesCall "indexSumTypeRegistry" [|box lookup|]
    let assembly = typeof<AST.SemanticType>.Assembly
    let modeType = assembly.GetType("EqualityHelpers+EqHelperExprMode")
    let modes = FSharpType.GetUnionCases(modeType,Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
                |> Array.map (fun case -> FSharpValue.MakeUnion(case,[||],Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic))
    samples |> List.map (fun typ ->
        modes |> Array.map (fun mode ->
            [|"EqualityHelpers","buildEqHelperExpr"; "OrderingHelpers","buildCompareHelperExpr"|]
            |> Array.map (fun (moduleName,methodName) ->
                let methodInfo = assembly.GetType(moduleName).GetMethod(methodName,Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
                encode methodInfo.ReturnType (methodInfo.Invoke(null,[|box aliases;box registry;box lookup;box sums;mode;box typ;box left;box right|])))
            |> namedArray "tuple") |> fun values -> JsonArray(values) :> JsonNode)
    |> Array.ofList |> fun values -> JsonArray(values) :> JsonNode

let helperDependencies source =
    let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
        AST.TVar source; AST.TInferenceVar (source,"fixed"); AST.TFunction ([AST.TInt64],AST.TString); AST.TFunction ([AST.TBlob],AST.TNever); AST.TTuple [AST.TInt64;AST.TString];
        AST.TRecord ("R",[]); AST.TRecord ("R",[AST.TUnit]); AST.TRecord ("Recursive",[]); AST.TRecord ("Opaque",[]); AST.TRecord ("Missing",[]); AST.TRecord ("S",[]);
        AST.TSum ("S",[]); AST.TSum ("BadSum",[]); AST.TSum ("Generic",[AST.TString]); AST.TSum ("Generic",[]); AST.TSum ("Uuid",[]);
        AST.TList AST.TInt64; AST.TStream AST.TNever; AST.TDict (AST.TString,AST.TInt64); AST.TDict (AST.TInt64,AST.TString); AST.TDict (AST.TBlob,AST.TString)]
    let lookup : CheckingTypes.VariantLookup = Map.ofList ["S.C",("S",[],0,[AST.TInt64]); "BadSum.C",("BadSum",[],0,[AST.TInternalRawPtr]); "Generic.C",("Generic",["a"],0,[AST.TVar "a"])]
    let raw = Map.ofList ["R",["field",AST.TInt64]; "Recursive",["next",AST.TRecord ("Recursive",[])]; "Opaque",["field",AST.TStream AST.TInt64]]
    let aliases : CheckingTypes.AliasRegistry = Map.ofList ["Alias",([],AST.TRecord ("R",[]))]
    let left : AST.Expr = AST.Var source
    let right : AST.Expr = AST.UnitLiteral
    let samples = samples @ [AST.TRecord ("GenericRecord",[AST.TInt64;AST.TString]); AST.TRecord ("GenericRecord",[]); AST.TSum ("Multiple",[AST.TInt64])]
    let raw = Map.add "GenericRecord" ["z",AST.TVar "a"; "aa",AST.TVar "b"; source,AST.TList (AST.TVar "a")] raw
    let registry = CheckingTypes.indexTypeRegistry lookup (Map.ofList ["R",[];"Recursive",[];"Opaque",[];"GenericRecord",["a";"b"]]) raw
    let lookup = lookup |> Map.add ("Multiple." + source) ("Multiple",["a"],7,[AST.TVar "a";AST.TList AST.TString])
                        |> Map.add "Multiple.z" ("Multiple",["a"],3,[]) |> Map.add "Multiple.aa" ("Multiple",["a"],1,[AST.TString])
    let sums : CheckingTypes.IndexedSumTypeRegistry = typesCall "indexSumTypeRegistry" [|box lookup|]
    let assembly = typeof<AST.SemanticType>.Assembly
    let dependencyModule = assembly.GetType("HelperDependencies")
    let eqType = assembly.GetType("HelperDependencies+EqHelperGenerationState")
    let compareType = assembly.GetType("HelperDependencies+CompareHelperGenerationState")
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic
    let empty typ = FSharpValue.MakeRecord(typ,[|box (Set.empty<string>);box (Map.empty<string,AST.FunctionDef>)|],flags)
    let ensure name value state = dependencyModule.GetMethod(name,flags ||| Reflection.BindingFlags.Static).Invoke(null,[|box aliases;box registry;box lookup;box sums;box value;state|])
    let eq value state = ensure "ensureEqHelperForType" value state
    let compare value state = ensure "ensureCompareHelperForType" value state
    let perType = samples |> List.map (fun value -> namedArray "tuple" [|encode eqType (eq value (empty eqType));encode compareType (compare value (empty compareType))|])
    let call name args : AST.Expr = AST.applyNamedWithTypes name args (AST.NonEmptyList.fromList [left;right])
    let modeType = assembly.GetType("ComparisonPlanning+InternalTypeApp")
    let case = FSharpType.GetUnionCases(modeType,flags)[0]
    let dispatch = FSharpValue.MakeUnion(case,[|box (AST.TVar source);box left;box right|],flags)
    let internalExpr = assembly.GetType("ComparisonPlanning").GetMethod("makeInternalTypeApp",flags ||| Reflection.BindingFlags.Static).Invoke(null,[|dispatch|]) :?> AST.Expr
    let expressions = [internalExpr;call "__compare" [AST.TInt64];call "__compare" [AST.TVar source];call "Darklang.Stdlib.List.sort" [AST.TString];
        call "Darklang.Stdlib.List.unique" [AST.TString];call "Darklang.Stdlib.List.uniqueBy" [AST.TInt64;AST.TVar source];
        call "Darklang.Stdlib.List.sortBy" [AST.TString;AST.TInt64];call source [AST.TString;AST.TVar source]]
    let expressions = expressions @ [AST.TupleLiteral expressions;AST.If (left,AST.TupleLiteral expressions,right);
        AST.Match (left,[{Patterns = AST.NonEmptyList.singleton AST.PWildcard;Guard = Some (List.head expressions);Body = AST.TupleLiteral expressions}]);
        AST.InterpolatedString (expressions |> List.map AST.StringExpr)]
    let collect expression =
        [|"collectEqHelperTypesFromExpr";"collectCompareHelperTypesFromExpr"|] |> Array.map (fun name ->
            let value = dependencyModule.GetMethod(name,flags ||| Reflection.BindingFlags.Static).Invoke(null,[|box aliases;box expression|]) :?> Set<AST.SemanticType>
            encode typeof<AST.SemanticType list> (box (Set.toList value))) |> namedArray "tuple"
    namedArray "tuple" [|JsonArray(Array.ofList perType) :> JsonNode;encode eqType (List.fold (fun state value -> eq value state) (empty eqType) samples);
        encode compareType (List.fold (fun state value -> compare value state) (empty compareType) samples);
        JsonArray(expressions |> List.map collect |> Array.ofList) :> JsonNode|]

let materializeHelpers source =
    let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
        AST.TVar source; AST.TInferenceVar (source,"fixed"); AST.TFunction ([AST.TInt64],AST.TString); AST.TFunction ([AST.TBlob],AST.TNever); AST.TTuple [AST.TInt64;AST.TString];
        AST.TRecord ("R",[]); AST.TRecord ("R",[AST.TUnit]); AST.TRecord ("Recursive",[]); AST.TRecord ("Opaque",[]); AST.TRecord ("Missing",[]); AST.TRecord ("S",[]);
        AST.TSum ("S",[]); AST.TSum ("BadSum",[]); AST.TSum ("Generic",[AST.TString]); AST.TSum ("Generic",[]); AST.TSum ("Uuid",[]);
        AST.TList AST.TInt64; AST.TStream AST.TNever; AST.TDict (AST.TString,AST.TInt64); AST.TDict (AST.TInt64,AST.TString); AST.TDict (AST.TBlob,AST.TString)]
    let lookup : CheckingTypes.VariantLookup = Map.ofList ["S.C",("S",[],0,[AST.TInt64]); "BadSum.C",("BadSum",[],0,[AST.TInternalRawPtr]); "Generic.C",("Generic",["a"],0,[AST.TVar "a"])]
    let raw = Map.ofList ["R",["field",AST.TInt64]; "Recursive",["next",AST.TRecord ("Recursive",[])]; "Opaque",["field",AST.TStream AST.TInt64]]
    let aliases : CheckingTypes.AliasRegistry = Map.ofList ["Alias",([],AST.TRecord ("R",[]))]
    let left : AST.Expr = AST.Var source
    let right : AST.Expr = AST.UnitLiteral
    let samples = samples @ [AST.TRecord ("GenericRecord",[AST.TInt64;AST.TString]); AST.TRecord ("GenericRecord",[]); AST.TSum ("Multiple",[AST.TInt64])]
    let raw = Map.add "GenericRecord" ["z",AST.TVar "a"; "aa",AST.TVar "b"; source,AST.TList (AST.TVar "a")] raw
    let registry = CheckingTypes.indexTypeRegistry lookup (Map.ofList ["R",[];"Recursive",[];"Opaque",[];"GenericRecord",["a";"b"]]) raw
    let lookup = lookup |> Map.add ("Multiple." + source) ("Multiple",["a"],7,[AST.TVar "a";AST.TList AST.TString])
                        |> Map.add "Multiple.z" ("Multiple",["a"],3,[]) |> Map.add "Multiple.aa" ("Multiple",["a"],1,[AST.TString])
    let sums : CheckingTypes.IndexedSumTypeRegistry = typesCall "indexSumTypeRegistry" [|box lookup|]
    let assembly = typeof<AST.SemanticType>.Assembly
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let planning = assembly.GetType("ComparisonPlanning")
    let internalType = assembly.GetType("ComparisonPlanning+InternalTypeApp")
    let case = FSharpType.GetUnionCases(internalType, flags)[0]
    let dispatch value = planning.GetMethod("makeInternalTypeApp",flags).Invoke(null,[|FSharpValue.MakeUnion(case,[|box value;box left;box right|],flags)|]) :?> AST.Expr
    let compare value = AST.applyNamedWithTypes "__compare" [value] (AST.NonEmptyList.fromList [left;right])
    let bodies = samples |> List.map (fun value -> AST.TupleLiteral [dispatch value;compare value])
    let definition name typeParams body : AST.FunctionDef = {Name=name;TypeParams=typeParams;Params=AST.NonEmptyList.singleton ("arg",AST.TUnit);ReturnType=AST.TUnit;Body=body;Recursion=None}
    let helperName = planning.GetMethod("eqHelperName",flags).Invoke(null,[|box (AST.TList AST.TInt64)|]) :?> string
    let topLevels : AST.TopLevel list = [AST.FunctionDef (definition source [] (AST.TupleLiteral bodies));
        AST.FunctionDef (definition "template" ["a"] (AST.TupleLiteral bodies));AST.FunctionDef (definition helperName [] AST.UnitLiteral);
        AST.ValueDef (AST.UncheckedValueDef ("unchecked",AST.TupleLiteral bodies));AST.ValueDef (AST.CheckedValueDef ("checked",AST.TUnit,AST.TupleLiteral bodies));
        AST.TypeDef (AST.RecordDef ("R",[],["field",AST.TInt64]));AST.Expression ([source],AST.TupleLiteral bodies)]
    let moduleType = assembly.GetType("MaterializeHelpers")
    let call name parameters =
        let method = moduleType.GetMethod(name,flags)
        encode method.ReturnType (method.Invoke(null,parameters))
    namedArray "tuple" [|call "materializeEqHelpersInTopLevelsWithIndexedSums" [|box aliases;box registry;box lookup;box sums;box topLevels|];
        call "materializeEqHelpersInTopLevels" [|box aliases;box registry;box lookup;box topLevels|];
        call "materializeCompareHelpersInTopLevels" [|box aliases;box registry;box lookup;box topLevels|]|]

let declarations source =
    let x = AST.Var source
    let y = AST.Var "y"
    let field = AST.unresolvedRecordFieldReference "field"
    let patterns = [AST.PUnit;AST.PWildcard;AST.PVar source;AST.PConstructor ("C",[AST.PVar source;AST.PVar "y"]);AST.PResolvedConstructor ("M.T","C",3,[AST.PVar source]);AST.PInt64 1L;AST.PBigInt 1I;AST.PInt128Literal (Int128.Parse "1");AST.PInt8Literal 1y;AST.PInt16Literal 1s;AST.PInt32Literal 1;AST.PUInt8Literal 1uy;AST.PUInt16Literal 1us;AST.PUInt32Literal 1ul;AST.PUInt64Literal 1UL;AST.PUInt128Literal (UInt128.Parse "1");AST.PBool true;AST.PString source;AST.PChar source;AST.PFloat 1.0;AST.PTuple [AST.PVar source;AST.PVar "y"];AST.PList [AST.PVar source];AST.PListCons ([AST.PVar source],AST.PVar "tail");AST.POr (AST.NonEmptyList.fromList [AST.PVar source;AST.PVar "other"])]
    let literals: AST.Expr list = [AST.UnitLiteral;AST.Int64Literal 1L;AST.Int128Literal (Int128.Parse "1");AST.BigIntLiteral 1I;AST.Int8Literal 1y;AST.Int16Literal 1s;AST.Int32Literal 1;AST.UInt8Literal 1uy;AST.UInt16Literal 1us;AST.UInt32Literal 1ul;AST.UInt64Literal 1UL;AST.UInt128Literal (UInt128.Parse "1");AST.BoolLiteral true;AST.StringLiteral source;AST.CharLiteral source;AST.FloatLiteral 1.0;AST.RuntimeError source]
    let expressions = literals @ [x;AST.Var "Builtin.testNan";AST.Var "Builtin.testInfinity";AST.BoundaryRender (source,x);AST.BinOp (AST.Add,x,y);AST.UnaryOp (AST.Neg,x);
        AST.Let (AST.LPVariable source,x,AST.TupleLiteral [x;y]);AST.Let (AST.LPTuple (AST.LPVariable source,AST.LPVariable "y",[]),AST.Var "value",AST.TupleLiteral [x;y]);
        AST.RecursiveLet (AST.ParsedRecursiveBinding {Binding=AST.bindingId 1;Boundary=AST.scopeBoundaryId 2;Member=AST.recursiveMemberId 3;SourceName=source;Kind=AST.NamedLocalFunctionMember},AST.Apply (x,[],AST.NonEmptyList.singleton y),AST.TupleLiteral [x;y]);
        AST.If (x,y,AST.Var "z");AST.Sequence (x,y);AST.Apply (x,[],AST.NonEmptyList.fromList [y;AST.Var "z"]);AST.TupleLiteral [x;y];AST.TupleAccess (x,1);
        AST.DictLiteral (AST.TString,AST.TString,[(x,y)]);AST.RecordLiteral (AST.unresolvedRecordReference "R" [],[(field,x)]);AST.RecordUpdate (x,[(field,y)]);AST.RecordAccess (x,field);
        AST.Constructor (AST.UnresolvedConstructor None,"C",[x;y]);AST.ListLiteral [x;y];AST.Lambda (AST.NonEmptyList.singleton (AST.lambdaParameter (AST.LPVariable source)),None,AST.TupleLiteral [x;y]);
        AST.Apply (AST.TupleAccess (x,0),[],AST.NonEmptyList.singleton y);AST.IndirectApply (x,AST.NonEmptyList.singleton y);AST.Closure (source,[x;y]);
        AST.InterpolatedString [AST.StringText source;AST.StringExpr x;AST.StringExpr y]] @ (patterns |> List.map (fun pattern -> AST.Match (AST.Var "scrutinee",[{Patterns=AST.NonEmptyList.singleton pattern;Guard=Some (AST.Var "guard");Body=AST.TupleLiteral [x;y;AST.Var "tail"]}])))
    let parsed name ordinal : AST.ParsedRecursiveMember = {Binding=AST.bindingId ordinal;Boundary=AST.scopeBoundaryId 0;Member=AST.recursiveMemberId ordinal;SourceName=name;Kind=AST.TopLevelFunctionMember}
    let definition name ordinal body : AST.FunctionDef = {Name=name;TypeParams=[];Params=AST.NonEmptyList.fromList ([source;"y";"z";"value";"scrutinee";"guard";"tail"] |> List.map (fun name -> name,AST.TUnit));ReturnType=AST.TUnit;Body=body;Recursion=Some (AST.ParsedRecursiveBinding (parsed name ordinal))}
    let declarations : AST.TopLevel list = [AST.TypeDef (AST.RecordDef ("R",[],["field",AST.TInt64]));AST.TypeDef (AST.SumTypeDef ("S",[],[{Name="C";Fields=[AST.TInt64]}]));
        AST.TypeDef (AST.TypeAlias ("Alias",[],AST.TRecord ("R",[])));AST.TypeDef (AST.RecordDef ("M.T",[],["field",AST.TList AST.TInt64]));
        AST.FunctionDef (definition "M.f" 0 (AST.applyNamed "g" (AST.NonEmptyList.singleton AST.UnitLiteral)));AST.FunctionDef (definition "M.g" 1 (AST.applyNamed "f" (AST.NonEmptyList.singleton AST.UnitLiteral)));
        AST.FunctionDef (definition "loop" 2 (AST.applyNamed "loop" (AST.NonEmptyList.singleton AST.UnitLiteral)));AST.FunctionDef (definition "ordinary" 3 AST.UnitLiteral);AST.ValueDef (AST.UncheckedValueDef ("M.value",AST.StringLiteral source))]
    let registry : AST.ModuleRegistry = Map.ofList ["Intrinsic.fn",{Name="Intrinsic.fn";TypeParams=[];ParamTypes=[AST.TUnit];ReturnType=AST.TUnit};"M.f",{Name="M.f";TypeParams=[];ParamTypes=[AST.TUnit];ReturnType=AST.TUnit}]
    let aliases : CheckingTypes.AliasRegistry = Map.ofList ["Alias",([],AST.TRecord ("R",[]))]
    let assembly = typeof<AST.SemanticType>.Assembly
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let resolver = assembly.GetType("ResolveDeclarations")
    let checking = assembly.GetType("CheckDeclarations")
    let environment includeIntrinsic = resolver.GetMethod("declarationResolutionEnvironment",flags).Invoke(null,[|box declarations;box registry;box includeIntrinsic|]) :?> NameResolution.ResolutionEnvironment
    let env = environment true
    let call (moduleType:Type) name args = let method = moduleType.GetMethod(name,flags) in encode method.ReturnType (method.Invoke(null,args))
    let resolve (topLevels:AST.TopLevel list) = call resolver "resolveProgramNames" [|box env;box aliases;box (Set.ofList ["R";"M.T"]);box (AST.Program topLevels)|]
    let validationCases : AST.TopLevel list list = [declarations;[];[AST.TypeDef (AST.RecordDef ("Empty",[],[]))];[AST.TypeDef (AST.SumTypeDef ("Empty",[],[]))];
        [AST.TypeDef (AST.RecordDef ("Unknown",[],["field",AST.TRecord (source,[])]))];[AST.TypeDef (AST.RecordDef ("Undeclared",[],["field",AST.TVar source]))];
        [AST.TypeDef (AST.RecordDef ("Duplicate",[source;source],["field",AST.TVar source]))];[AST.TypeDef (AST.SumTypeDef ("Duplicate",[],[{Name=source;Fields=[]};{Name=source;Fields=[]}]))];
        [AST.TypeDef (AST.TypeAlias ("Cycle",[],AST.TRecord ("Cycle",[])))];[AST.TypeDef (AST.TypeAlias ("A",[],AST.TRecord ("B",[])));AST.TypeDef (AST.TypeAlias ("B",[],AST.TRecord ("A",[])))];
        [AST.TypeDef (AST.RecordDef ("R",["a"],["field",AST.TVar "a"]));AST.TypeDef (AST.RecordDef ("Owner",[],["field",AST.TRecord ("R",[])]))];
        [AST.TypeDef (AST.RecordDef ("R",[],["field",AST.TInt64]));AST.TypeDef (AST.RecordDef ("R",[],["other",AST.TString]))];
        [AST.TypeDef (AST.SumTypeDef ("A",[],[{Name="C";Fields=[]}]));AST.TypeDef (AST.SumTypeDef ("B",[],[{Name="C";Fields=[]}]))]]
    namedArray "tuple" [|encode typeof<NameResolution.Candidate list> (box (NameResolution.candidates env));encode typeof<NameResolution.Candidate list> (box (NameResolution.candidates (environment false)));
        call resolver "resolveRecursiveDeclarationGroups" [|box declarations|];resolve declarations;
        JsonArray(expressions |> List.map (fun expression -> resolve [AST.FunctionDef (definition "probe" 4 expression)]) |> Array.ofList) :> JsonNode;
        JsonArray(validationCases |> List.map (fun values -> namedArray "tuple" [|call checking "validateTopLevelTypeDeclarations" [|box (None:CheckingTypes.TypeCheckEnv option);box values|];call checking "summarizeTopLevelDeclarations" [|box values|]|]) |> Array.ofList) :> JsonNode|]

let recordChecking source =
    let registry = CheckingTypes.indexTypeRegistry Map.empty (Map.ofList ["R",[];"Generic",["a"]]) (Map.ofList ["R",["first",AST.TInt64;"second",AST.TString];"Generic",["first",AST.TVar "a";"second",AST.TVar "a"]])
    let aliases : CheckingTypes.AliasRegistry = Map.ofList ["Alias",([],AST.TRecord ("R",[]));"GAlias",(["a"],AST.TRecord ("Generic",[AST.TVar "a"]))]
    let generic : CheckingTypes.GenericFuncRegistry = {Functions=Map.empty;RequireExplicitTypeArgsForBareCalls=false}
    let trace = ResizeArray<AST.Expr * AST.SemanticType option>()
    let callback (args:obj list) : obj =
        let value = args[0] :?> AST.Expr
        let expected = args[8] :?> AST.SemanticType option
        trace.Add (value,expected)
        let result : Result<AST.SemanticType * AST.Expr,CheckingDiagnostics.TypeError> =
            match value with
            | AST.RuntimeError message -> Error (CheckingDiagnostics.GenericError message)
            | AST.Var "mismatch" -> Error (CheckingDiagnostics.TypeMismatch (AST.TString,AST.TInt64,"callback"))
            | AST.Int64Literal _ -> Ok (AST.TInt64,AST.BoundaryRender ("checked",value))
            | AST.StringLiteral _ -> Ok (AST.TString,AST.BoundaryRender ("checked",value))
            | AST.Var "generic" -> Ok (AST.TVar source,AST.BoundaryRender ("checked",value))
            | _ -> Ok (AST.TUnit,AST.BoundaryRender ("checked",value))
        box result
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let method = typeof<AST.SemanticType>.Assembly.GetType("CheckRecordLiterals").GetMethod("check",flags)
    let rec curry (typ:Type) args = FSharpValue.MakeFunction(typ,fun arg -> let args = args @ [arg] in if args.Length = 9 then callback args else curry (typ.GetGenericArguments().[1]) args)
    let checker = curry (method.GetParameters().[0].ParameterType) []
    let field name value = AST.unresolvedRecordFieldReference name,value
    let fields : (AST.RecordFieldReference * AST.Expr) list list = [[field "first" (AST.Int64Literal 1L);field "second" (AST.StringLiteral source)];
        [field "second" (AST.StringLiteral source);field "first" (AST.Int64Literal 1L)];[];[field "first" AST.UnitLiteral];[field "first" (AST.Int64Literal 1L);field "first" AST.UnitLiteral];
        [field "___" AST.UnitLiteral];[field "" AST.UnitLiteral];[field "first" (AST.Int64Literal 1L);field "second" (AST.StringLiteral source);field source AST.UnitLiteral];
        [field "first" (AST.RuntimeError source);field "second" (AST.RuntimeError "second")];[field "second" (AST.RuntimeError "second");field "first" (AST.RuntimeError source)];
        [field "first" (AST.Var "mismatch");field "second" (AST.StringLiteral source)];[field "first" (AST.Var "generic");field "second" (AST.Var "generic")];[field "first" (AST.Int64Literal 1L);field "second" (AST.Int64Literal 2L)]]
    let references = ["",[];"Unknown",[];"R",[];"R",[AST.TUnit];"Generic",[];"Generic",[AST.TString];"Generic",[AST.TString;AST.TInt64];"Alias",[];"GAlias",[AST.TInt64]] |> List.map (fun (name,args) -> AST.unresolvedRecordReference name args)
    let expected = [None;Some AST.TUnit;Some (AST.TRecord ("R",[]));Some (AST.TRecord ("Generic",[AST.TInt64]));Some (AST.TRecord ("Generic",[AST.TString]))]
    let cases = if source = "" then references |> List.collect (fun reference -> fields |> List.collect (fun fields -> expected |> List.map (fun expected -> reference,fields,expected)))
                else fields |> List.collect (fun fields -> [AST.unresolvedRecordReference "R" [],fields,None;AST.unresolvedRecordReference "Generic" [],fields,Some (AST.TRecord ("Generic",[AST.TInt64]))])
    cases |> List.map (fun (reference,fields,expected) ->
        trace.Clear()
        let result = method.Invoke(null,[|checker;box (Map.empty<string,AST.SemanticType>);box registry;box (Map.empty<string,string * string list * int * AST.SemanticType list>);box generic;box AST.defaultWarningSettings;box (Map.empty<string,AST.ModuleFunc>);box aliases;box expected;box reference;box fields|])
        namedArray "tuple" [|encode method.ReturnType result;encode typeof<(AST.Expr * AST.SemanticType option) list> (box (List.ofSeq trace))|]) |> List.toArray |> fun values -> JsonArray(values) :> JsonNode

let binaryChecking source =
    let samples = [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TFloat64;AST.TBool;AST.TString;AST.TChar;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TBlob;AST.TDateTime;AST.TVar source;AST.TInferenceVar (source,"fixed");AST.TList AST.TInt64;AST.TRecord ("R",[]);AST.TSum ("S",[]);AST.TTuple [AST.TInt64;AST.TString];AST.TFunction ([AST.TInt64],AST.TString)]
    let pairs = if source = "" then samples |> List.collect (fun left -> samples |> List.map (fun right -> left,right)) else
                    [AST.TInt64,AST.TInt64;AST.TInt128,AST.TString;AST.TBool,AST.TBool;AST.TString,AST.TChar;AST.TVar source,AST.TInt64;AST.TInt64,AST.TVar source;AST.TSum ("S",[]),AST.TSum ("Other",[])]
    let ops = [AST.Add;AST.Sub;AST.Mul;AST.Div;AST.Mod;AST.Eq;AST.Neq;AST.Lt;AST.Gt;AST.Lte;AST.Gte;AST.And;AST.Or;AST.Pow;AST.Shl;AST.Shr;AST.BitAnd;AST.BitOr;AST.BitXor;AST.StringConcat]
    let registry = CheckingTypes.indexTypeRegistry Map.empty (Map.ofList ["R",[]]) (Map.ofList ["R",["field",AST.TInt64]])
    let generic : CheckingTypes.GenericFuncRegistry = {Functions=Map.empty;RequireExplicitTypeArgsForBareCalls=false}
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let method = typeof<AST.SemanticType>.Assembly.GetType("CheckBinaryOperations").GetMethod("check",flags)
    let run leftType rightType (op:AST.BinOp) (expected:AST.SemanticType option) (left:AST.Expr) (right:AST.Expr) =
        let trace = ResizeArray<AST.Expr * AST.SemanticType option>()
        let callback (args:obj list) =
            let value = args[0] :?> AST.Expr
            let expected = args[8] :?> AST.SemanticType option
            trace.Add (value,expected)
            let typ = match value with AST.Var "left" -> leftType | AST.Var "right" -> rightType | AST.BoolLiteral _ -> AST.TBool | AST.StringLiteral _ -> AST.TString | _ -> AST.TUnit
            let result : Result<AST.SemanticType * AST.Expr,CheckingDiagnostics.TypeError> =
                match value with
                | AST.Var "error" when expected = None -> Error (CheckingDiagnostics.GenericError source)
                | AST.RuntimeError message -> Error (CheckingDiagnostics.GenericError message)
                | _ -> let typ = if TypeUnification.containsTVar typ then Option.defaultValue typ expected else typ in Ok (typ,value)
            box result
        let rec curry (typ:Type) args = FSharpValue.MakeFunction(typ,fun arg -> let args = args @ [arg] in if args.Length = 9 then callback args else curry (typ.GetGenericArguments().[1]) args)
        let checker = curry (method.GetParameters().[0].ParameterType) []
        let result = method.Invoke(null,[|checker;box (Map.empty<string,CheckingTypes.SumTypeInfo>);box (Map.empty<string,AST.SemanticType>);box registry;box (Map.empty<string,string * string list * int * AST.SemanticType list>);box generic;box AST.defaultWarningSettings;box (Map.empty<string,AST.ModuleFunc>);box (Map.empty<string,string list * AST.SemanticType>);box expected;box op;box left;box right|])
        namedArray "tuple" [|encode method.ReturnType result;encode typeof<(AST.Expr * AST.SemanticType option) list> (box (List.ofSeq trace))|]
    let ordinary = pairs |> List.collect (fun (left,right) -> ops |> List.collect (fun op -> [None;Some AST.TBool;Some AST.TInt64] |> List.map (fun expected -> run left right op expected (AST.Var "left") (AST.Var "right"))))
    let runtime = AST.applyNamed "Builtin.testRuntimeError" (AST.NonEmptyList.singleton (AST.StringLiteral source))
    let special = ops |> List.collect (fun op -> [runtime,AST.Var "right";AST.Var "left",runtime;AST.BoolLiteral false,runtime;AST.BoolLiteral true,runtime;AST.Var "left",AST.Var "error"] |> List.map (fun (left,right) -> run AST.TInt64 (AST.TVar source) op None left right))
    JsonArray(Array.ofList (ordinary @ special)) :> JsonNode

let stdlibCatalog source =
    let registry = Stdlib.buildModuleRegistry ()
    let names = source :: "missing" :: "Builtin.print_v0" :: (registry |> Map.toList |> List.map fst)
    let lookup = names |> List.map (Stdlib.tryGetFunction registry)
    namedArray "tuple" [|encode typeof<AST.ModuleDef list> (box Stdlib.allModules);encode typeof<AST.ModuleFunc list> (box Stdlib.rawMemoryIntrinsics);
        encode typeof<AST.ModuleRegistry> (box registry);encode typeof<(AST.ModuleFunc * string) option list> (box lookup);
        encode typeof<AST.SemanticType list> (box (registry |> Map.toList |> List.map (snd >> Stdlib.getFunctionType)));
        encode typeof<AST.SemanticType> (box (Stdlib.resultType (AST.TVar source)))|]

let lambdaChecking source =
    let x = AST.Var source
    let y = AST.Var "y"
    let field = AST.unresolvedRecordFieldReference "field"
    let patterns = [AST.PUnit;AST.PWildcard;AST.PVar source;AST.PConstructor ("C",[AST.PVar source;AST.PVar "y"]);AST.PResolvedConstructor ("M.T","C",3,[AST.PVar source]);AST.PInt64 1L;AST.PBigInt 1I;AST.PInt128Literal (Int128.Parse "1");AST.PInt8Literal 1y;AST.PInt16Literal 1s;AST.PInt32Literal 1;AST.PUInt8Literal 1uy;AST.PUInt16Literal 1us;AST.PUInt32Literal 1ul;AST.PUInt64Literal 1UL;AST.PUInt128Literal (UInt128.Parse "1");AST.PBool true;AST.PString source;AST.PChar source;AST.PFloat 1.0;AST.PTuple [AST.PVar source;AST.PVar "y"];AST.PList [AST.PVar source];AST.PListCons ([AST.PVar source],AST.PVar "tail");AST.POr (AST.NonEmptyList.fromList [AST.PVar source;AST.PVar "other"])]
    let literals: AST.Expr list = [AST.UnitLiteral;AST.Int64Literal 1L;AST.Int128Literal (Int128.Parse "1");AST.BigIntLiteral 1I;AST.Int8Literal 1y;AST.Int16Literal 1s;AST.Int32Literal 1;AST.UInt8Literal 1uy;AST.UInt16Literal 1us;AST.UInt32Literal 1ul;AST.UInt64Literal 1UL;AST.UInt128Literal (UInt128.Parse "1");AST.BoolLiteral true;AST.StringLiteral source;AST.CharLiteral source;AST.FloatLiteral 1.0;AST.RuntimeError source]
    let expressions = literals @ [x;AST.Var "Builtin.testNan";AST.Var "Builtin.testInfinity";AST.BoundaryRender (source,x);AST.BinOp (AST.Add,x,y);AST.UnaryOp (AST.Neg,x);
        AST.Let (AST.LPVariable source,x,AST.TupleLiteral [x;y]);AST.Let (AST.LPTuple (AST.LPVariable source,AST.LPVariable "y",[]),AST.Var "value",AST.TupleLiteral [x;y]);
        AST.RecursiveLet (AST.RecursiveBindingCandidate {SourceName=source;Kind=AST.NamedLocalFunctionMember},AST.Apply (x,[],AST.NonEmptyList.singleton y),AST.TupleLiteral [x;y]);
        AST.If (x,y,AST.Var "z");AST.Sequence (x,y);AST.Apply (x,[],AST.NonEmptyList.fromList [y;AST.Var "z"]);AST.TupleLiteral [x;y];AST.TupleAccess (x,1);
        AST.DictLiteral (AST.TString,AST.TString,[(x,y)]);AST.RecordLiteral (AST.unresolvedRecordReference "R" [],[(field,x)]);AST.RecordUpdate (x,[(field,y)]);AST.RecordAccess (x,field);
        AST.Constructor (AST.UnresolvedConstructor None,"C",[x;y]);AST.ListLiteral [x;y];AST.Lambda (AST.NonEmptyList.singleton (AST.lambdaParameter (AST.LPVariable source)),None,AST.TupleLiteral [x;y]);
        AST.Apply (AST.TupleAccess (x,0),[],AST.NonEmptyList.singleton y);AST.IndirectApply (x,AST.NonEmptyList.singleton y);AST.Closure (source,[x;y]);
        AST.InterpolatedString [AST.StringText source;AST.StringExpr x;AST.StringExpr y]] @ (patterns |> List.map (fun pattern -> AST.Match (AST.Var "scrutinee",[{Patterns=AST.NonEmptyList.singleton pattern;Guard=Some (AST.Var "guard");Body=AST.TupleLiteral [x;y;AST.Var "tail"]}])))
    let groups = [AST.NonEmptyList.singleton (AST.lambdaParameter (AST.LPVariable source));AST.NonEmptyList.singleton (AST.typedLambdaVariable source AST.TString);
        AST.NonEmptyList.singleton (AST.lambdaParameter (AST.LPTuple (AST.LPVariable source,AST.LPVariable "y",[])));
        AST.NonEmptyList.fromList [AST.lambdaParameter (AST.LPVariable source);AST.lambdaParameter (AST.LPVariable "y")]]
    let expectations = [None;Some AST.TInt64;Some (AST.TFunction ([],AST.TUnit));Some (AST.TFunction ([AST.TInt64],AST.TInt64));Some (AST.TFunction ([AST.TVar source],AST.TVar source));Some (AST.TFunction ([AST.TInt64;AST.TString],AST.TBool))]
    let annotations = [None;Some AST.TUnit;Some AST.TInt64;Some (AST.TVar source)]
    let env : CheckingTypes.TypeEnv = Map.ofList ["y",AST.TString;"f",AST.TFunction ([AST.TInt64],AST.TString);"value",AST.TInt64]
    let modules = Stdlib.buildModuleRegistry ()
    let generic : CheckingTypes.GenericFuncRegistry = {Functions=Map.empty;RequireExplicitTypeArgsForBareCalls=false}
    let bodies = expressions @ [AST.applyNamed "f" (AST.NonEmptyList.singleton x);AST.applyNamed "Darklang.Stdlib.Int64.toFloat" (AST.NonEmptyList.singleton x)]
    let cases = if source = "" then groups |> List.collect (fun parameters -> expectations |> List.collect (fun expected -> annotations |> List.collect (fun annotation -> bodies |> List.map (fun body -> parameters,expected,annotation,body))))
                else bodies |> List.mapi (fun index body -> groups[index % groups.Length],expectations[index % expectations.Length],annotations[index % annotations.Length],body)
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let method = typeof<AST.SemanticType>.Assembly.GetType("CheckLambdas").GetMethod("check",flags)
    cases |> List.map (fun (parameters,expected,annotation,body) ->
        let trace = ResizeArray<AST.Expr * CheckingTypes.TypeEnv * AST.SemanticType option>()
        let callback (args:obj list) =
            let value = args[0] :?> AST.Expr
            let env = args[1] :?> CheckingTypes.TypeEnv
            let expected = args[8] :?> AST.SemanticType option
            trace.Add (value,env,expected)
            let typ = match value with AST.Var name -> Map.tryFind name env |> Option.defaultValue AST.TUnit | AST.RuntimeError _ -> AST.TNever | AST.Int64Literal _ -> AST.TInt64 | AST.StringLiteral _ -> AST.TString | AST.BoolLiteral _ -> AST.TBool | _ -> Option.defaultValue AST.TUnit expected
            box (Ok (typ,value):Result<AST.SemanticType * AST.Expr,CheckingDiagnostics.TypeError>)
        let rec curry (typ:Type) args = FSharpValue.MakeFunction(typ,fun arg -> let args = args @ [arg] in if args.Length = 9 then callback args else curry (typ.GetGenericArguments().[1]) args)
        let checker = curry (method.GetParameters().[0].ParameterType) []
        let result = method.Invoke(null,[|checker;box env;box (Map.empty<string,CheckingTypes.RecordTypeInfo>);box (Map.empty<string,string * string list * int * AST.SemanticType list>);box generic;box AST.defaultWarningSettings;box modules;box (Map.empty<string,string list * AST.SemanticType>);box expected;box parameters;box annotation;box body|])
        namedArray "tuple" [|encode method.ReturnType result;encode typeof<(AST.Expr * CheckingTypes.TypeEnv * AST.SemanticType option) list> (box (List.ofSeq trace))|]) |> List.toArray |> fun values -> JsonArray(values) :> JsonNode

let callChecking source =
    let env : CheckingTypes.TypeEnv = Map.ofList ["f",AST.TFunction ([AST.TInt64;AST.TString],AST.TBool);"g",AST.TFunction ([AST.TVar "a";AST.TVar "a"],AST.TVar "a");"higher",AST.TVar "a";"inferred",AST.TInferenceVar ("a","fixed");"nonfunction",AST.TInt64;"nullary",AST.TFunction ([],AST.TUnit);"Darklang.Stdlib.List.sort",AST.TFunction ([AST.TList (AST.TVar "a")],AST.TList (AST.TVar "a"))]
    let generic : CheckingTypes.GenericFuncRegistry = {Functions=Map.ofList ["g",["a"];"Darklang.Stdlib.List.sort",["a"]];RequireExplicitTypeArgsForBareCalls=true}
    let modules = Stdlib.buildModuleRegistry () |> Map.add "module.generic" {Name="module.generic";TypeParams=["a"];ParamTypes=[AST.TVar "a";AST.TVar "a"];ReturnType=AST.TVar "a"}
                                             |> Map.add "module.concrete" {Name="module.concrete";TypeParams=[];ParamTypes=[AST.TInt64;AST.TString];ReturnType=AST.TBool}
    let names = ["f";"g";"higher";"inferred";"nonfunction";"nullary";"missing";"module.generic";"module.concrete";"Builtin.unwrap";"Builtin.crash";"Builtin.testRuntimeError";"Darklang.Stdlib.List.sort";"__compare";"__empty_dict"]
    let argLists : AST.Expr list list = [[AST.UnitLiteral];[AST.Int64Literal 1L];[AST.StringLiteral source];[AST.Int64Literal 1L;AST.StringLiteral source];[AST.Int64Literal 1L;AST.Int64Literal 2L];[AST.StringLiteral source;AST.StringLiteral source];[AST.Int64Literal 1L;AST.StringLiteral source;AST.UnitLiteral];[AST.Var "generic"];[AST.Var "error"];[AST.Var "option"];[AST.Var "result"];[AST.Var "list"]]
    let expectations = [None;Some AST.TUnit;Some AST.TBool;Some AST.TInt64;Some AST.TString;Some (AST.TFunction ([AST.TString],AST.TBool));Some (AST.TVar source)]
    let cases = if source = "" then names |> List.collect (fun name -> argLists |> List.collect (fun args -> expectations |> List.map (fun expected -> name,args,expected)))
                else names |> List.collect (fun name -> argLists |> List.mapi (fun index args -> name,args,expectations[index % expectations.Length]))
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let method = typeof<AST.SemanticType>.Assembly.GetType("CheckCalls").GetMethod("check",flags)
    cases |> List.map (fun (name,args,expected) ->
        let trace = ResizeArray<AST.Expr * AST.SemanticType option>()
        let callback (parameters:obj list) =
            let value = parameters[0] :?> AST.Expr
            let expected = parameters[8] :?> AST.SemanticType option
            trace.Add (value,expected)
            let typ = match value with AST.UnitLiteral -> AST.TUnit | AST.Int64Literal _ -> AST.TInt64 | AST.StringLiteral _ -> AST.TString
                                       | AST.Var "option" -> AST.TSum ("Darklang.Stdlib.Option.Option",[AST.TInt64]) | AST.Var "result" -> AST.TSum ("Darklang.Stdlib.Result.Result",[AST.TInt64;AST.TString])
                                       | AST.Var "list" -> AST.TList AST.TInt64 | AST.Var "generic" -> Option.defaultValue (AST.TVar source) expected | _ -> AST.TUnit
            let result : Result<AST.SemanticType * AST.Expr,CheckingDiagnostics.TypeError> = match value with AST.Var "error" -> Error (CheckingDiagnostics.TypeMismatch (Option.defaultValue AST.TString expected,AST.TInt64,"callback")) | _ -> Ok (typ,value)
            box result
        let rec curry (typ:Type) args = FSharpValue.MakeFunction(typ,fun arg -> let args = args @ [arg] in if args.Length = 9 then callback args else curry (typ.GetGenericArguments().[1]) args)
        let checker = curry (method.GetParameters().[0].ParameterType) []
        let result = method.Invoke(null,[|checker;box (Map.ofList ["f",["first";"second"]]);box (Map.empty<string,CheckingTypes.SumTypeInfo>);box env;box (Map.empty<string,CheckingTypes.RecordTypeInfo>);box (Map.empty<string,string * string list * int * AST.SemanticType list>);box generic;box AST.defaultWarningSettings;box modules;box (Map.empty<string,string list * AST.SemanticType>);box expected;box name;box (AST.NonEmptyList.fromList args)|])
        namedArray "tuple" [|encode method.ReturnType result;encode typeof<(AST.Expr * AST.SemanticType option) list> (box (List.ofSeq trace))|]) |> List.toArray |> fun values -> JsonArray(values) :> JsonNode

let matchChecking source =
    let patterns = [AST.PUnit;AST.PWildcard;AST.PVar source;AST.PConstructor ("C",[AST.PVar source;AST.PVar "y"]);AST.PResolvedConstructor ("M.T","C",3,[AST.PVar source]);AST.PInt64 1L;AST.PBigInt 1I;AST.PInt128Literal (Int128.Parse "1");AST.PInt8Literal 1y;AST.PInt16Literal 1s;AST.PInt32Literal 1;AST.PUInt8Literal 1uy;AST.PUInt16Literal 1us;AST.PUInt32Literal 1ul;AST.PUInt64Literal 1UL;AST.PUInt128Literal (UInt128.Parse "1");AST.PBool true;AST.PString source;AST.PChar source;AST.PFloat 1.0;AST.PTuple [AST.PVar source;AST.PVar "y"];AST.PList [AST.PVar source];AST.PListCons ([AST.PVar source],AST.PVar "tail");AST.POr (AST.NonEmptyList.fromList [AST.PVar source;AST.PVar "other"])]
    let lookup : CheckingTypes.VariantLookup = Map.ofList ["C",("S",[],0,[AST.TInt64]);"S.C",("S",[],0,[AST.TInt64]);"D",("S",[],1,[]);"S.D",("S",[],1,[]);
        "Ok",("Outer",[],0,[AST.TSum ("Inner",[])]);"Outer.Ok",("Outer",[],0,[AST.TSum ("Inner",[])]);"I1",("Inner",[],0,[]);"Inner.I1",("Inner",[],0,[]);"I2",("Inner",[],1,[]);"Inner.I2",("Inner",[],1,[])]
    let sums : CheckingTypes.IndexedSumTypeRegistry = typesCall "indexSumTypeRegistry" [|box lookup|]
    let names = Set.ofList ["S";"Outer";"Inner"]
    let generic : CheckingTypes.GenericFuncRegistry = {Functions=Map.empty;RequireExplicitTypeArgsForBareCalls=false}
    let scrutinees : (AST.SemanticType * AST.Expr) list = [AST.TUnit,AST.UnitLiteral;AST.TInt64,AST.Int64Literal 1L;AST.TInt64,AST.Var "scrutinee";AST.TInt128,AST.Int128Literal (Int128.Parse "1");AST.TInt,AST.BigIntLiteral 1I;
        AST.TInt8,AST.Int8Literal 1y;AST.TInt16,AST.Int16Literal 1s;AST.TInt32,AST.Int32Literal 1;AST.TUInt8,AST.UInt8Literal 1uy;AST.TUInt16,AST.UInt16Literal 1us;AST.TUInt32,AST.UInt32Literal 1ul;AST.TUInt64,AST.UInt64Literal 1UL;AST.TUInt128,AST.UInt128Literal (UInt128.Parse "1");
        AST.TBool,AST.BoolLiteral false;AST.TBool,AST.Var "scrutinee";AST.TString,AST.StringLiteral source;AST.TChar,AST.CharLiteral source;AST.TFloat64,AST.FloatLiteral 1.0;
        AST.TTuple [AST.TInt64;AST.TString],AST.TupleLiteral [AST.Int64Literal 1L;AST.StringLiteral source];AST.TTuple [AST.TBool;AST.TBool],AST.Var "scrutinee";
        AST.TList AST.TInt64,AST.ListLiteral [];AST.TList AST.TInt64,AST.ListLiteral [AST.Int64Literal 1L;AST.Int64Literal 1L];AST.TList AST.TString,AST.ListLiteral [AST.StringLiteral source];
        AST.TSum ("S",[]),AST.Constructor (AST.UnresolvedConstructor None,"C",[AST.Int64Literal 1L]);AST.TSum ("S",[]),AST.Var "scrutinee";AST.TSum ("Outer",[]),AST.Var "scrutinee";
        AST.TNever,AST.RuntimeError source;AST.TVar source,AST.Var "scrutinee";AST.TInferenceVar (source,"fixed"),AST.Var "scrutinee"]
    let case patterns guard body : AST.MatchCase = {Patterns=AST.NonEmptyList.fromList patterns;Guard=guard;Body=body}
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let method = typeof<AST.SemanticType>.Assembly.GetType("CheckMatches").GetMethod("check",flags)
    let run typ (scrutinee:AST.Expr) (cases:AST.MatchCase list) (expected:AST.SemanticType option) =
        let trace = ResizeArray<AST.Expr * CheckingTypes.TypeEnv * AST.SemanticType option>()
        let callback (args:obj list) =
            let value = args[0] :?> AST.Expr
            let env = args[1] :?> CheckingTypes.TypeEnv
            let expected = args[8] :?> AST.SemanticType option
            let first = trace.Count = 0
            trace.Add (value,env,expected)
            let result : Result<AST.SemanticType * AST.Expr,CheckingDiagnostics.TypeError> =
                if first then Ok (typ,value) else
                match value with
                | AST.Var "undefined" -> Error (CheckingDiagnostics.UndefinedVariable "undefined")
                | AST.BoolLiteral _ -> match expected with Some expected when expected <> AST.TBool -> Error (CheckingDiagnostics.TypeMismatch (expected,AST.TBool,"boolean literal")) | _ -> Ok (AST.TBool,value)
                | AST.Var "genericBody" -> Ok (Option.defaultValue (AST.TVar source) expected,value)
                | AST.Var name -> Ok (Map.tryFind name env |> Option.defaultValue AST.TUnit,value)
                | AST.UnitLiteral -> Ok (AST.TUnit,value) | AST.Int64Literal _ -> Ok (AST.TInt64,value) | AST.StringLiteral _ -> Ok (AST.TString,value) | AST.RuntimeError _ -> Ok (AST.TNever,value)
                | _ -> Ok (Option.defaultValue AST.TUnit expected,value)
            box result
        let rec curry (typ:Type) args = FSharpValue.MakeFunction(typ,fun arg -> let args = args @ [arg] in if args.Length = 9 then callback args else curry (typ.GetGenericArguments().[1]) args)
        let checker = curry (method.GetParameters().[0].ParameterType) []
        let result = method.Invoke(null,[|checker;box names;box sums;box (Map.empty<string,AST.SemanticType>);box (Map.empty<string,CheckingTypes.RecordTypeInfo>);box lookup;box generic;box AST.defaultWarningSettings;box (Map.empty<string,AST.ModuleFunc>);box (Map.empty<string,string list * AST.SemanticType>);box expected;box scrutinee;box cases|])
        namedArray "tuple" [|encode method.ReturnType result;encode typeof<(AST.Expr * CheckingTypes.TypeEnv * AST.SemanticType option) list> (box (List.ofSeq trace))|]
    let configurations pattern = [[case [pattern] None AST.UnitLiteral];[case [pattern] None AST.UnitLiteral;case [AST.PWildcard] None AST.UnitLiteral];
        [case [pattern] (Some (AST.BoolLiteral true)) AST.UnitLiteral;case [AST.PWildcard] None AST.UnitLiteral];[case [pattern;AST.PWildcard] None AST.UnitLiteral];
        [case [pattern] (Some (AST.Var "undefined")) AST.UnitLiteral];[case [pattern] None (AST.Var "genericBody");case [AST.PWildcard] None (AST.Int64Literal 1L)];
        [case [pattern] None (AST.Int64Literal 1L);case [AST.PWildcard] None (AST.BoolLiteral true)]]
    let ordinary = if source = "" then scrutinees |> List.collect (fun (typ,value) -> patterns |> List.collect (fun pattern -> configurations pattern |> List.map (fun cases -> run typ value cases None)))
                   else patterns |> List.mapi (fun index pattern -> let typ,value = scrutinees[index % scrutinees.Length] in run typ value ((configurations pattern).[index % 7]) None)
    let special : (AST.SemanticType * AST.Expr * AST.MatchCase list) list = [AST.TBool,AST.Var "scrutinee",[case [AST.PBool true] None AST.UnitLiteral;case [AST.PBool false] None AST.UnitLiteral];
        AST.TList AST.TInt64,AST.Var "scrutinee",[case [AST.PList []] None AST.UnitLiteral;case [AST.PListCons ([AST.PWildcard],AST.PWildcard)] None AST.UnitLiteral];
        AST.TTuple [AST.TBool;AST.TBool],AST.Var "scrutinee",[case [AST.PTuple [AST.PBool true;AST.PWildcard]] None AST.UnitLiteral;case [AST.PTuple [AST.PBool false;AST.PBool true]] None AST.UnitLiteral;case [AST.PTuple [AST.PBool false;AST.PBool false]] None AST.UnitLiteral];
        AST.TSum ("Outer",[]),AST.Var "scrutinee",[case [AST.PConstructor ("Ok",[AST.PConstructor ("I1",[])])] None AST.UnitLiteral;case [AST.PConstructor ("Ok",[AST.PConstructor ("I2",[])])] None AST.UnitLiteral];
        AST.TSum ("S",[]),AST.Var "scrutinee",[case [AST.PConstructor ("C",[AST.PWildcard])] None AST.UnitLiteral;case [AST.PConstructor ("D",[])] None AST.UnitLiteral];
        AST.TUnit,AST.UnitLiteral,[];AST.TUnit,AST.UnitLiteral,[case [AST.PWildcard] None AST.UnitLiteral]]
    let special = special |> List.collect (fun (typ,value,cases) -> [None;Some AST.TInt64;Some (AST.TVar source)] |> List.map (fun expected -> run typ value cases expected))
    JsonArray(Array.ofList (ordinary @ special)) :> JsonNode

let additionalExpressions source =
    let x = AST.Var source
    let y = AST.Var "y"
    let field = AST.unresolvedRecordFieldReference "field"
    let patterns = [AST.PUnit;AST.PWildcard;AST.PVar source;AST.PConstructor ("C",[AST.PVar source;AST.PVar "y"]);AST.PResolvedConstructor ("M.T","C",3,[AST.PVar source]);AST.PInt64 1L;AST.PBigInt 1I;AST.PInt128Literal (Int128.Parse "1");AST.PInt8Literal 1y;AST.PInt16Literal 1s;AST.PInt32Literal 1;AST.PUInt8Literal 1uy;AST.PUInt16Literal 1us;AST.PUInt32Literal 1ul;AST.PUInt64Literal 1UL;AST.PUInt128Literal (UInt128.Parse "1");AST.PBool true;AST.PString source;AST.PChar source;AST.PFloat 1.0;AST.PTuple [AST.PVar source;AST.PVar "y"];AST.PList [AST.PVar source];AST.PListCons ([AST.PVar source],AST.PVar "tail");AST.POr (AST.NonEmptyList.fromList [AST.PVar source;AST.PVar "other"])]
    let literals: AST.Expr list = [AST.UnitLiteral;AST.Int64Literal 1L;AST.Int128Literal (Int128.Parse "1");AST.BigIntLiteral 1I;AST.Int8Literal 1y;AST.Int16Literal 1s;AST.Int32Literal 1;AST.UInt8Literal 1uy;AST.UInt16Literal 1us;AST.UInt32Literal 1ul;AST.UInt64Literal 1UL;AST.UInt128Literal (UInt128.Parse "1");AST.BoolLiteral true;AST.StringLiteral source;AST.CharLiteral source;AST.FloatLiteral 1.0;AST.RuntimeError source]
    let expressions = literals @ [x;AST.Var "Builtin.testNan";AST.Var "Builtin.testInfinity";AST.BoundaryRender (source,x);AST.BinOp (AST.Add,x,y);AST.UnaryOp (AST.Neg,x);
        AST.Let (AST.LPVariable source,x,AST.TupleLiteral [x;y]);AST.Let (AST.LPTuple (AST.LPVariable source,AST.LPVariable "y",[]),AST.Var "value",AST.TupleLiteral [x;y]);
        AST.RecursiveLet (AST.RecursiveBindingCandidate {SourceName=source;Kind=AST.NamedLocalFunctionMember},AST.Apply (x,[],AST.NonEmptyList.singleton y),AST.TupleLiteral [x;y]);
        AST.If (x,y,AST.Var "z");AST.Sequence (x,y);AST.Apply (x,[],AST.NonEmptyList.fromList [y;AST.Var "z"]);AST.TupleLiteral [x;y];AST.TupleAccess (x,1);
        AST.DictLiteral (AST.TString,AST.TString,[(x,y)]);AST.RecordLiteral (AST.unresolvedRecordReference "R" [],[(field,x)]);AST.RecordUpdate (x,[(field,y)]);AST.RecordAccess (x,field);
        AST.Constructor (AST.UnresolvedConstructor None,"C",[x;y]);AST.ListLiteral [x;y];AST.Lambda (AST.NonEmptyList.singleton (AST.lambdaParameter (AST.LPVariable source)),None,AST.TupleLiteral [x;y]);
        AST.Apply (AST.TupleAccess (x,0),[],AST.NonEmptyList.singleton y);AST.IndirectApply (x,AST.NonEmptyList.singleton y);AST.Closure (source,[x;y]);
        AST.InterpolatedString [AST.StringText source;AST.StringExpr x;AST.StringExpr y]] @ (patterns |> List.map (fun pattern -> AST.Match (AST.Var "scrutinee",[{Patterns=AST.NonEmptyList.singleton pattern;Guard=Some (AST.Var "guard");Body=AST.TupleLiteral [x;y;AST.Var "tail"]}])))
    expressions

let expressionChecking source =
    let lookup : CheckingTypes.VariantLookup = Map.ofList ["C",("S",[],0,[AST.TInt64]);"S.C",("S",[],0,[AST.TInt64]);"D",("S",[],1,[]);"S.D",("S",[],1,[]);
        "None",("Darklang.Stdlib.Option.Option",["a"],0,[]);"Darklang.Stdlib.Option.Option.None",("Darklang.Stdlib.Option.Option",["a"],0,[]);
        "Some",("Darklang.Stdlib.Option.Option",["a"],1,[AST.TVar "a"]);"Darklang.Stdlib.Option.Option.Some",("Darklang.Stdlib.Option.Option",["a"],1,[AST.TVar "a"])]
    let names = Set.ofList ["S";"Darklang.Stdlib.Option.Option"]
    let sums : CheckingTypes.IndexedSumTypeRegistry = typesCall "indexSumTypeRegistry" [|box lookup|]
    let registry = CheckingTypes.indexTypeRegistry lookup (Map.ofList ["R",[];"Generic",["a"]]) (Map.ofList ["R",["field",AST.TInt64;"field",AST.TString];"Generic",["field",AST.TVar "a"]])
    let aliases : CheckingTypes.AliasRegistry = Map.ofList ["AliasInt",([],AST.TInt64);"AliasString",([],AST.TString);"AliasRecord",([],AST.TRecord ("R",[]));"Pair",(["a"],AST.TTuple [AST.TVar "a";AST.TVar "a"])]
    let env : CheckingTypes.TypeEnv = Map.ofList [source,AST.TInt64;"y",AST.TString;"z",AST.TBool;"value",AST.TTuple [AST.TInt64;AST.TString];"scrutinee",AST.TSum ("S",[]);"guard",AST.TBool;"tail",AST.TString;
        "record",AST.TRecord ("R",[]);"g",AST.TFunction ([AST.TVar "a";AST.TVar "a"],AST.TVar "a");"f",AST.TFunction ([AST.TInt64;AST.TString],AST.TBool);
        "Dict.fn",AST.TFunction ([AST.TVar "k";AST.TVar "v"],AST.TVar "v");"closure",AST.TFunction ([AST.TUnit;AST.TInt64],AST.TString)]
    let generic : CheckingTypes.GenericFuncRegistry = {Functions=Map.ofList ["g",["a"];"Dict.fn",["k";"v"]];RequireExplicitTypeArgsForBareCalls=true}
    let parsed : AST.ParsedRecursiveMember = {Binding=AST.bindingId 1;Boundary=AST.scopeBoundaryId 2;Member=AST.recursiveMemberId 3;SourceName="recursive";Kind=AST.NamedLocalFunctionMember}
    let recursive = AST.ResolvedRecursiveBinding {Parsed=parsed;Group=AST.singletonRecursiveGroupId parsed.Member;GroupIndex=0;Availability=AST.SelfRecursiveMember}
    let variable name = AST.lambdaParameter (AST.LPVariable name)
    let field value = AST.unresolvedRecordFieldReference "field",value
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let internalApp =
        let planType = typeof<AST.SemanticType>.Assembly.GetType("ComparisonPlanning+InternalTypeApp")
        let case = FSharpType.GetUnionCases(planType,flags)[0]
        let dispatch = FSharpValue.MakeUnion(case,[|box AST.TInt64;box (AST.Int64Literal 1L : AST.Expr);box (AST.Int64Literal 2L : AST.Expr)|],flags)
        typeof<AST.SemanticType>.Assembly.GetType("ComparisonPlanning").GetMethod("makeInternalTypeApp",flags).Invoke(null,[|dispatch|]) :?> AST.Expr
    let ops = [AST.Add; AST.Sub; AST.Mul; AST.Div; AST.Mod; AST.Eq; AST.Neq; AST.Lt; AST.Gt; AST.Lte; AST.Gte; AST.And; AST.Or; AST.Pow; AST.Shl; AST.Shr; AST.BitAnd; AST.BitOr; AST.BitXor; AST.StringConcat]
    let extra : AST.Expr list =
        (ops |> List.collect (fun op -> [AST.BinOp (op, AST.Int64Literal 1L, AST.Int64Literal 2L); AST.BinOp (op, AST.BoolLiteral true, AST.BoolLiteral false)]))
        @ [AST.ListLiteral []; AST.ListLiteral [AST.Int64Literal 1L; AST.Int64Literal 2L]; AST.ListLiteral [AST.Int64Literal 1L; AST.StringLiteral source];
          AST.Let (AST.LPVariable "n", AST.Int64Literal 1L, AST.InterpolatedString [AST.StringExpr (AST.Var "n")]);
          AST.Let (AST.LPTuple (AST.LPVariable "a", AST.LPVariable "b", []), AST.Int64Literal 1L, AST.Var "missing");
          AST.Let (AST.LPVariable "fn", AST.Lambda (AST.NonEmptyList.singleton (variable "x"), None, AST.BinOp (AST.Add, AST.Var "x", AST.Int64Literal 1L)), AST.applyNamed "fn" (AST.NonEmptyList.singleton (AST.Int64Literal 2L)));
          AST.RecursiveLet (recursive, AST.Lambda (AST.NonEmptyList.singleton {Pattern = AST.LPVariable "x"; SourceAnnotation = Some AST.TString; InferredType = Some AST.TInt64}, Some AST.TInt64, AST.Var "x"), AST.applyNamed "recursive" (AST.NonEmptyList.singleton (AST.Int64Literal 2L)));
          AST.Apply (AST.Var "g", [AST.TInt64], AST.NonEmptyList.fromList [AST.Int64Literal 1L; AST.Int64Literal 2L]); AST.Apply (AST.Var "g", [AST.TString], AST.NonEmptyList.singleton (AST.StringLiteral source));
          AST.Apply (AST.Var "Dict.fn", [AST.TInt64], AST.NonEmptyList.fromList [AST.StringLiteral source; AST.Int64Literal 1L]);
          AST.Apply (AST.Var "__raw_get", [AST.TInt64], AST.NonEmptyList.fromList [AST.RuntimeError source; AST.Int64Literal 0L]);
          internalApp;
          AST.RecordLiteral (AST.unresolvedRecordReference "R" [], [field (AST.Int64Literal 1L)]); AST.RecordUpdate (AST.Var "record", [field (AST.Int64Literal 1L); field (AST.Int64Literal 2L)]);
          AST.RecordAccess (AST.Var "record", AST.unresolvedRecordFieldReference "field"); AST.RecordAccess (AST.Var "record", AST.unresolvedRecordFieldReference "___");
          AST.Constructor (AST.UnresolvedConstructor None, "C", [AST.Int64Literal 1L]); AST.Constructor (AST.UnresolvedConstructor None, "None", []);
          AST.applyNamed "Builtin.unwrap" (AST.NonEmptyList.singleton (AST.Constructor (AST.UnresolvedConstructor None, "None", [])));
          AST.If (AST.BoolLiteral true, AST.ListLiteral [], AST.ListLiteral [AST.Int64Literal 1L]); AST.If (AST.Int64Literal 1L, AST.UnitLiteral, AST.UnitLiteral);
          AST.TupleLiteral [AST.applyNamed "Builtin.testRuntimeError" (AST.NonEmptyList.singleton (AST.StringLiteral source)); AST.Int64Literal 1L];
          AST.DictLiteral (AST.TUnit, AST.TUnit, [AST.StringLiteral source, AST.Int64Literal 1L; AST.StringLiteral source, AST.Int64Literal 2L]);
          AST.DictLiteral (AST.TUnit, AST.TUnit, [AST.FloatLiteral (BitConverter.Int64BitsToDouble (int64 0xfff8000000000000UL)), AST.UnitLiteral; AST.FloatLiteral (BitConverter.Int64BitsToDouble 0x7ff8000000000001L), AST.UnitLiteral]);
          AST.DictLiteral (AST.TUnit, AST.TUnit, []); AST.DictLiteral (AST.TUnit, AST.TUnit, [AST.StringLiteral source, AST.UnitLiteral]);
          AST.Apply (AST.Lambda (AST.NonEmptyList.fromList [variable "x"; variable "y"], None, AST.Var "x"), [], AST.NonEmptyList.singleton (AST.Int64Literal 1L)); AST.Closure ("closure", [AST.UnitLiteral]);
          AST.Match (AST.BoolLiteral true, [{Patterns = AST.NonEmptyList.singleton (AST.PBool true); Guard = None; Body = AST.Int64Literal 1L}; {Patterns = AST.NonEmptyList.singleton (AST.PBool false); Guard = None; Body = AST.Int64Literal 2L}])]
   
    let expressions = additionalExpressions source @ extra
    let expectations = [None; Some AST.TUnit; Some AST.TInt64; Some AST.TInt128; Some AST.TInt; Some AST.TBool; Some AST.TString; Some AST.TChar; Some AST.TFloat64; Some (AST.TVar source);
        Some (AST.TRecord ("AliasInt", [])); Some (AST.TRecord ("AliasString", [])); Some (AST.TRecord ("R", [])); Some (AST.TSum ("S", [])); Some (AST.TList AST.TInt64); Some (AST.TTuple [AST.TInt64; AST.TString]); Some (AST.TFunction ([AST.TInt64], AST.TInt64)); Some (AST.TDict (AST.TString, AST.TInt64))]
    let cases = if source = "" then expressions |> List.collect (fun value -> expectations |> List.map (fun expected -> value,expected)) else expressions |> List.mapi (fun index value -> value,expectations[index % expectations.Length])
    let method = typeof<AST.SemanticType>.Assembly.GetType("CheckExpressions").GetMethod("checkExprWithParamNamesAndSumTypeNames",flags)
    cases |> List.map (fun (value,expected) ->
        try
            let result = method.Invoke(null,[|box (Map.ofList ["f",["first";"second"]]);box names;box sums;box value;box env;box registry;box lookup;box generic;box AST.defaultWarningSettings;box (Stdlib.buildModuleRegistry ());box aliases;box expected|])
            let node = JsonObject()
            node["result"] <- encode method.ReturnType result
            node :> JsonNode
        with :? Reflection.TargetInvocationException as error ->
            let node = JsonObject()
            node["crash"] <- encodeString error.InnerException.Message
            node :> JsonNode) |> List.toArray |> fun values -> JsonArray(values) :> JsonNode

let functionChecking source =
    let lookup : CheckingTypes.VariantLookup = Map.ofList ["C",("S",[],0,[AST.TInt64]);"S.C",("S",[],0,[AST.TInt64]);"D",("S",[],1,[]);"S.D",("S",[],1,[]);
        "None",("Darklang.Stdlib.Option.Option",["a"],0,[]);"Darklang.Stdlib.Option.Option.None",("Darklang.Stdlib.Option.Option",["a"],0,[]);
        "Some",("Darklang.Stdlib.Option.Option",["a"],1,[AST.TVar "a"]);"Darklang.Stdlib.Option.Option.Some",("Darklang.Stdlib.Option.Option",["a"],1,[AST.TVar "a"])]
    let names = Set.ofList ["S";"Darklang.Stdlib.Option.Option"]
    let sums : CheckingTypes.IndexedSumTypeRegistry = typesCall "indexSumTypeRegistry" [|box lookup|]
    let registry = CheckingTypes.indexTypeRegistry lookup (Map.ofList ["R",[];"Generic",["a"]]) (Map.ofList ["R",["field",AST.TInt64;"field",AST.TString];"Generic",["field",AST.TVar "a"]])
    let aliases : CheckingTypes.AliasRegistry = Map.ofList ["AliasInt",([],AST.TInt64);"AliasString",([],AST.TString);"AliasRecord",([],AST.TRecord ("R",[]));"Pair",(["a"],AST.TTuple [AST.TVar "a";AST.TVar "a"])]
    let env : CheckingTypes.TypeEnv = Map.ofList [source,AST.TInt64;"y",AST.TString;"z",AST.TBool;"value",AST.TTuple [AST.TInt64;AST.TString];"scrutinee",AST.TSum ("S",[]);"guard",AST.TBool;"tail",AST.TString;
        "record",AST.TRecord ("R",[]);"g",AST.TFunction ([AST.TVar "a";AST.TVar "a"],AST.TVar "a");"f",AST.TFunction ([AST.TInt64;AST.TString],AST.TBool);
        "Dict.fn",AST.TFunction ([AST.TVar "k";AST.TVar "v"],AST.TVar "v");"closure",AST.TFunction ([AST.TUnit;AST.TInt64],AST.TString)]
    let generic : CheckingTypes.GenericFuncRegistry = {Functions=Map.ofList ["g",["a"];"Dict.fn",["k";"v"]];RequireExplicitTypeArgsForBareCalls=true}
    let parsed : AST.ParsedRecursiveMember = {Binding=AST.bindingId 1;Boundary=AST.scopeBoundaryId 2;Member=AST.recursiveMemberId 3;SourceName="recursive";Kind=AST.NamedLocalFunctionMember}
    let recursive = AST.ResolvedRecursiveBinding {Parsed=parsed;Group=AST.singletonRecursiveGroupId parsed.Member;GroupIndex=0;Availability=AST.SelfRecursiveMember}
    let functions : AST.FunctionDef list = [
        {Name=source;TypeParams=[];Params=AST.NonEmptyList.singleton ("x",AST.TInt64);ReturnType=AST.TInt64;Body=AST.Var "x";Recursion=None};
        {Name=source;TypeParams=[];Params=AST.NonEmptyList.singleton ("x",AST.TInt64);ReturnType=AST.TString;Body=AST.Int64Literal 1L;Recursion=None};
        {Name=source;TypeParams=["a"];Params=AST.NonEmptyList.singleton ("x",AST.TVar "a");ReturnType=AST.TVar "a";Body=AST.Var "x";Recursion=None};
        {Name=source;TypeParams=["a"];Params=AST.NonEmptyList.singleton ("x",AST.TVar "a");ReturnType=AST.TVar "a";Body=AST.StringLiteral source;Recursion=None};
        {Name=source;TypeParams=[];Params=AST.NonEmptyList.singleton ("x",AST.TRecord ("AliasInt",[]));ReturnType=AST.TRecord ("AliasInt",[]);Body=AST.Var "x";Recursion=Some recursive};
        {Name=source;TypeParams=[];Params=AST.NonEmptyList.singleton ("x",AST.TRecord ("S",[]));ReturnType=AST.TSum ("S",[]);Body=AST.Var "x";Recursion=Some (AST.TypedRecursiveBinding {Resolved=(match recursive with AST.ResolvedRecursiveBinding value -> value | _ -> failwith "fixture");MonomorphicType=AST.TUnit})};
        {Name=source;TypeParams=[];Params=AST.NonEmptyList.singleton ("x",AST.TUnit);ReturnType=AST.TList AST.TInt64;Body=AST.ListLiteral [];Recursion=None};
        {Name=source;TypeParams=[];Params=AST.NonEmptyList.singleton ("x",AST.TUnit);ReturnType=AST.TString;Body=AST.RuntimeError source;Recursion=None}]
    let specs = [[];[AST.TInt64];[AST.TString];[AST.TVar source];[AST.TInt64;AST.TString]]
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let moduleType = typeof<AST.SemanticType>.Assembly.GetType("CheckFunctions")
    let checkMethod = moduleType.GetMethod("checkFunctionDefWithSumTypeNames",flags)
    let specializeMethod = moduleType.GetMethod("specializeFunctionForTypeCheck",flags)
    let collectMethod = moduleType.GetMethod("collectTypeAppSpecs",flags)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let tuple values = namedArray "tuple" (Array.ofList values)
    tuple [
        functions |> List.map (fun func -> tuple [
            [false;true] |> List.map (fun explicit -> let result = checkMethod.Invoke(null,[|box (Map.ofList ["f",["first";"second"]]);box names;box sums;box func;box env;box registry;box lookup;box {generic with RequireExplicitTypeArgsForBareCalls=explicit};box AST.defaultWarningSettings;box (Stdlib.buildModuleRegistry ());box aliases|]) in encode checkMethod.ReturnType result) |> list;
            specs |> List.map (fun args -> encode specializeMethod.ReturnType (specializeMethod.Invoke(null,[|box func;box args|]))) |> list]) |> list;
        (additionalExpressions source @ [AST.Apply (AST.Var source,[AST.TInt64;AST.TVar "a"],AST.NonEmptyList.singleton (AST.Apply (AST.Var "g",[AST.TString],AST.NonEmptyList.singleton AST.UnitLiteral)));AST.Closure ("f",[AST.Apply (AST.Var "g",[AST.TInt64],AST.NonEmptyList.singleton AST.UnitLiteral)])])
        |> List.map (fun expr -> let specs = collectMethod.Invoke(null,[|box expr|]) :?> Set<string * AST.SemanticType list> in encode typeof<(string * AST.SemanticType list) list> (box (Set.toList specs))) |> list]

let programChecking source =
    let func name parameters ret body : AST.FunctionDef = {Name=name;TypeParams=[];Params=AST.NonEmptyList.fromList parameters;ReturnType=ret;Body=body;Recursion=None}
    let generic = {func "identity" ["x",AST.TVar "a"] (AST.TVar "a") (AST.Var "x") with TypeParams=["a"]}
    let declarations : AST.TopLevel list = [AST.TypeDef (AST.RecordDef ("R",[],["field",AST.TInt64]));AST.TypeDef (AST.SumTypeDef ("S",[],[{Name="C";Fields=[AST.TInt64]};{Name="D";Fields=[]}]));
        AST.TypeDef (AST.TypeAlias ("Alias",[],AST.TInt64));AST.FunctionDef generic;AST.FunctionDef (func "constant" ["x",AST.TUnit] AST.TString (AST.StringLiteral source));
        AST.ValueDef (AST.UncheckedValueDef ("value",AST.Int64Literal 1L))]
    let programs : AST.Program list = [AST.Program [];AST.Program [AST.Expression ([],AST.UnitLiteral)];AST.Program declarations;AST.Program (declarations @ [AST.Expression ([],AST.applyNamed "identity" (AST.NonEmptyList.singleton (AST.Int64Literal 1L)))]);
        AST.Program (declarations @ [AST.Expression ([],AST.Apply (AST.Var "identity",[AST.TString],AST.NonEmptyList.singleton (AST.StringLiteral source)))]);
        AST.Program (declarations @ [AST.Expression ([],AST.RecordLiteral (AST.unresolvedRecordReference "R" [],[AST.unresolvedRecordFieldReference "field",AST.Int64Literal 1L]))]);
        AST.Program (declarations @ [AST.Expression ([],AST.BinOp (AST.Eq,AST.Var "value",AST.Int64Literal 1L))]);
        AST.Program [AST.Expression ([],AST.StringLiteral source);AST.Expression ([],AST.UnitLiteral)];
        AST.Program [AST.FunctionDef (func "broken" ["x",AST.TUnit] AST.TString (AST.Int64Literal 1L));AST.Expression ([],AST.UnitLiteral)];
        AST.Program [AST.ValueDef (AST.UncheckedValueDef ("first",AST.Int64Literal 1L));AST.ValueDef (AST.UncheckedValueDef ("second",AST.Var "first"));AST.Expression ([],AST.Var "second")]]
    let baseResult = TypeChecking.checkDeclarationProgramWithEnv (AST.Program declarations)
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let method = typeof<AST.SemanticType>.Assembly.GetType("CheckResolvedProgram").GetMethod("checkResolvedProgramInternal",flags)
    let fullType = typeof<Result<AST.SemanticType * CheckedAST.Program * CheckingTypes.TypeCheckEnv,CheckingDiagnostics.TypeError>>
    let full value = encode fullType (box value)
    let tuple values = namedArray "tuple" (Array.ofList values)
    let perProgram program =
        let result = baseResult |> Result.map (fun (_,_,baseEnv) ->
            TypeChecking.checkProgramWithBaseEnv baseEnv program,TypeChecking.checkDeclarationProgramWithBaseEnv baseEnv program,
            TypeChecking.checkPublicProgramWithBaseEnvAndSettings baseEnv true AST.defaultWarningSettings program,TypeChecking.checkSyntheticPreambleWithBaseEnvAndSettings baseEnv false AST.defaultWarningSettings program)
        tuple [full (TypeChecking.checkProgramWithEnv program);full (TypeChecking.checkDeclarationProgramWithEnv program);
        encode typeof<Result<AST.SemanticType * CheckedAST.Program,CheckingDiagnostics.TypeError>> (box (TypeChecking.checkPublicProgram program));
        encode method.ReturnType (method.Invoke(null,[|box (None:CheckingTypes.TypeCheckEnv option);box false;box AST.defaultWarningSettings;box true;box program|]));
        encode typeof<Result<(Result<AST.SemanticType * CheckedAST.Program * CheckingTypes.TypeCheckEnv,CheckingDiagnostics.TypeError> * Result<AST.SemanticType * CheckedAST.Program * CheckingTypes.TypeCheckEnv,CheckingDiagnostics.TypeError> * Result<AST.SemanticType * CheckedAST.Program * CheckingTypes.TypeCheckEnv,CheckingDiagnostics.TypeError> * Result<AST.SemanticType * CheckedAST.Program * CheckingTypes.TypeCheckEnv,CheckingDiagnostics.TypeError>),CheckingDiagnostics.TypeError>> (box result)]
    tuple [full baseResult;JsonArray(programs |> List.map perProgram |> List.toArray) :> JsonNode]

// BEGIN GENERATED IR FIXTURES
let irFixtures source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    tuple [tuple [
        encode typeof<MemoryModel.CanonicalBufferKind list> (box [(MemoryModel.Utf8String); (MemoryModel.NullableUtf8String); (MemoryModel.GraphemeCluster); (MemoryModel.NullableGraphemeCluster)]);
        encode typeof<MemoryModel.RcKind list> (box [(MemoryModel.GenericHeap); (MemoryModel.StreamHeap); (MemoryModel.TaggedList); (MemoryModel.DictHeap); (MemoryModel.ClosureHeap)]);
        encode typeof<MemoryModel.RcShape list> (box [(MemoryModel.Immediate); (MemoryModel.FixedBlock ((3), [(MemoryModel.Immediate); (MemoryModel.Immediate)])); (MemoryModel.StreamRoot); (MemoryModel.BoxedSum ((3), [((3), (MemoryModel.Immediate)); ((3), (MemoryModel.Immediate))], [({MemoryModel.RcBoxedSumVariantShape.Tag = (3); MemoryModel.RcBoxedSumVariantShape.FieldShapes = [((3), (MemoryModel.Immediate)); ((3), (MemoryModel.Immediate))]} : MemoryModel.RcBoxedSumVariantShape); ({MemoryModel.RcBoxedSumVariantShape.Tag = (3); MemoryModel.RcBoxedSumVariantShape.FieldShapes = [((3), (MemoryModel.Immediate)); ((3), (MemoryModel.Immediate))]} : MemoryModel.RcBoxedSumVariantShape)])); (MemoryModel.RecursiveNominalRef ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (MemoryModel.TaggedListShape ((MemoryModel.Immediate))); (MemoryModel.DictRoot ((MemoryModel.Immediate), (MemoryModel.Immediate))); (MemoryModel.DynamicString); (MemoryModel.DynamicBlob); (MemoryModel.DynamicInt); (MemoryModel.ClosureShape ([(MemoryModel.Immediate); (MemoryModel.Immediate)])); (MemoryModel.StaticString); (MemoryModel.RawUnmanaged)]);
        encode typeof<MemoryModel.RcBoxedSumVariantShape list> (box [({MemoryModel.RcBoxedSumVariantShape.Tag = (3); MemoryModel.RcBoxedSumVariantShape.FieldShapes = [((3), (MemoryModel.Immediate)); ((3), (MemoryModel.Immediate))]} : MemoryModel.RcBoxedSumVariantShape)]);
        encode typeof<MemoryModel.RcSumShapeInfo list> (box [({MemoryModel.RcSumShapeInfo.TypeParams = [(source); (source)]; MemoryModel.RcSumShapeInfo.Payloads = [((3), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))); ((3), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))))]; MemoryModel.RcSumShapeInfo.UnaryPayloadTags = (Set.ofList [3; 1])} : MemoryModel.RcSumShapeInfo)]);
        encode typeof<MemoryModel.RcSumShapeRegistry list> (box [(Map.ofList [(source, ({MemoryModel.RcSumShapeInfo.TypeParams = [(source); (source)]; MemoryModel.RcSumShapeInfo.Payloads = [((3), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))); ((3), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))))]; MemoryModel.RcSumShapeInfo.UnaryPayloadTags = (Set.ofList [3; 1])} : MemoryModel.RcSumShapeInfo))])]);
        encode typeof<MemoryModel.RcOperation list> (box [(MemoryModel.FixedSizeRoot ((3), (MemoryModel.GenericHeap))); (MemoryModel.DynamicStringBuffer); (MemoryModel.DynamicBlobBuffer); (MemoryModel.DynamicIntBuffer)]);
        encode typeof<MemoryModel.RcStorageClass list> (box [(MemoryModel.UnmanagedStorage); (MemoryModel.ManagedDynamicBuffer ((MemoryModel.FixedSizeRoot ((3), (MemoryModel.GenericHeap))))); (MemoryModel.ManagedRcRoot ((3), (MemoryModel.GenericHeap)))]);
        encode typeof<MemoryModel.RcReleasePlan list> (box [(MemoryModel.NoReleasePlan); (MemoryModel.DynamicBufferRelease ((MemoryModel.FixedSizeRoot ((3), (MemoryModel.GenericHeap))))); (MemoryModel.RecursiveRelease ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (MemoryModel.RootRelease ((3), (MemoryModel.GenericHeap), (MemoryModel.NoPayloadRelease)))]);
        encode typeof<MemoryModel.RcPayloadReleasePlan list> (box [(MemoryModel.NoPayloadRelease); (MemoryModel.FixedBlockPayloadRelease ((3), [(MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan))); (MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan)))])); (MemoryModel.BoxedSumPayloadRelease ((3), [(MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan))); (MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan)))], [({MemoryModel.RcBoxedSumVariantRelease.Tag = (3); MemoryModel.RcBoxedSumVariantRelease.FieldReleases = [(MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan))); (MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan)))]} : MemoryModel.RcBoxedSumVariantRelease); ({MemoryModel.RcBoxedSumVariantRelease.Tag = (3); MemoryModel.RcBoxedSumVariantRelease.FieldReleases = [(MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan))); (MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan)))]} : MemoryModel.RcBoxedSumVariantRelease)])); (MemoryModel.TaggedListPayloadRelease ((MemoryModel.NoReleasePlan))); (MemoryModel.DictPayloadRelease ((MemoryModel.NoReleasePlan), (MemoryModel.NoReleasePlan))); (MemoryModel.ClosurePayloadRelease ([(MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan))); (MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan)))]))]);
        encode typeof<MemoryModel.RcFieldRelease list> (box [(MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan)))]);
        encode typeof<MemoryModel.RcBoxedSumVariantRelease list> (box [({MemoryModel.RcBoxedSumVariantRelease.Tag = (3); MemoryModel.RcBoxedSumVariantRelease.FieldReleases = [(MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan))); (MemoryModel.FieldRelease ((3), (MemoryModel.NoReleasePlan)))]} : MemoryModel.RcBoxedSumVariantRelease)]);
        encode typeof<MemoryModel.RcMetadata list> (box [({MemoryModel.RcMetadata.ReleasePlanCacheKey = (Some ((source))); MemoryModel.RcMetadata.ReleasePlan = (Some ((MemoryModel.NoReleasePlan))); MemoryModel.RcMetadata.SourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : MemoryModel.RcMetadata)]);
        encode typeof<ANF.TempId list> (box [(ANF.TempId ((3)))]);
        encode typeof<ANF.TypedParam list> (box [({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam)]);
        encode typeof<ANF.SizedInt list> (box [(ANF.Int8 (3y)); (ANF.Int16 (3s)); (ANF.Int32 (3)); (ANF.Int64 (3L)); (ANF.UInt8 (3uy)); (ANF.UInt16 (3us)); (ANF.UInt32 (3ul)); (ANF.UInt64 (3UL))]);
        encode typeof<ANF.Atom list> (box [(ANF.UnitLiteral); (ANF.IntLiteral ((ANF.Int8 (3y)))); (ANF.BoolLiteral ((true))); (ANF.StringLiteral ((source))); (ANF.FloatLiteral ((-0.0))); (ANF.Var ((ANF.TempId ((3))))); (ANF.FuncRef ((AST.functionId System.UInt64.MaxValue)))]);
        encode typeof<ANF.BinOp list> (box [(ANF.Add); (ANF.Sub); (ANF.Mul); (ANF.Div); (ANF.Mod); (ANF.Shl); (ANF.Shr); (ANF.BitAnd); (ANF.BitOr); (ANF.BitXor); (ANF.Eq); (ANF.Neq); (ANF.Lt); (ANF.Gt); (ANF.Lte); (ANF.Gte); (ANF.And); (ANF.Or)]);
        encode typeof<ANF.UnaryOp list> (box [(ANF.Neg); (ANF.Not); (ANF.BitNot)]);
        encode typeof<ANF.ReturnOwnership list> (box [(ANF.OwnedReturn); (ANF.BorrowedReturn)]);
        encode typeof<ANF.CliOperation list> (box [(ANF.Execute); (ANF.RunProcess); (ANF.HostOS); (ANF.HostArchitecture); (ANF.Hostname); (ANF.GetEnv); (ANF.GetEnvironmentPacked); (ANF.SetEnv); (ANF.UnsetEnv); (ANF.DirectoryCurrent); (ANF.DirectoryListPacked); (ANF.FileIsDirectory); (ANF.FileCreateExclusive); (ANF.GetArgv); (ANF.Kill); (ANF.GetPid); (ANF.GetUid); (ANF.CpuCount); (ANF.SpawnProcess); (ANF.ProcessIO); (ANF.TerminateProcess); (ANF.SocketTcp4); (ANF.SocketTcp6); (ANF.SocketUdp4); (ANF.SocketUdp6); (ANF.SocketConnect4); (ANF.SocketConnect6); (ANF.SocketSend); (ANF.SocketReceive); (ANF.SocketReceiveTimeout); (ANF.SocketSendTimeout); (ANF.SocketClose); (ANF.SecureRandomFill)]);
        encode typeof<ANF.RecordDescriptor list> (box [({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor)]);
        encode typeof<ANF.CExpr list> (box [(ANF.Atom ((ANF.StringLiteral source))); (ANF.TypedAtom ((ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (ANF.Prim ((ANF.Add), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.UnaryPrim ((ANF.Neg), (ANF.StringLiteral source))); (ANF.IfValue ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.Call ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.BorrowedCall ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.TailCall ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.IndirectCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.IndirectTailCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.ClosureAlloc ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.ClosureCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.ClosureTailCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.TupleAlloc ([(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.TupleGet ((ANF.StringLiteral source), (3))); (ANF.RecordAlloc (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.RecordGet (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.StringLiteral source), (3))); (ANF.RecordClone (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.RecordReuse (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), ({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.StringConcat ((ANF.StringLiteral source), (ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.CanonicalBufferEq ((MemoryModel.Utf8String), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.RefCountInc ((ANF.StringLiteral source), (3), (MemoryModel.GenericHeap), (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey = (Some ((source))); MemoryModel.RcMetadata.ReleasePlan = (Some ((MemoryModel.NoReleasePlan))); MemoryModel.RcMetadata.SourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : MemoryModel.RcMetadata))))); (ANF.RefCountDec ((ANF.StringLiteral source), (3), (MemoryModel.GenericHeap), (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey = (Some ((source))); MemoryModel.RcMetadata.ReleasePlan = (Some ((MemoryModel.NoReleasePlan))); MemoryModel.RcMetadata.SourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : MemoryModel.RcMetadata))))); (ANF.Print ((ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (ANF.StdoutWrite ((ANF.StringLiteral source), (true))); (ANF.StdinReadLine); (ANF.RuntimeError ((source))); (ANF.RuntimeErrorString ((ANF.StringLiteral source))); (ANF.FileReadBlob ((ANF.StringLiteral source))); (ANF.FileExists ((ANF.StringLiteral source))); (ANF.FileWriteBlob ((ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.FileAppendText ((ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.FileDelete ((ANF.StringLiteral source))); (ANF.FileCreateDirectory ((ANF.StringLiteral source))); (ANF.FileSetExecutable ((ANF.StringLiteral source))); (ANF.FileWriteFromPtr ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.FloatSqrt ((ANF.StringLiteral source))); (ANF.FloatAbs ((ANF.StringLiteral source))); (ANF.FloatNeg ((ANF.StringLiteral source))); (ANF.Int64ToFloat ((ANF.StringLiteral source))); (ANF.FloatToInt64 ((ANF.StringLiteral source))); (ANF.FloatToBits ((ANF.StringLiteral source))); (ANF.RawAlloc ((ANF.StringLiteral source))); (ANF.MappedAlloc ((ANF.StringLiteral source))); (ANF.RawFree ((ANF.StringLiteral source))); (ANF.MappedFree ((ANF.StringLiteral source))); (ANF.RawGet ((ANF.StringLiteral source), (ANF.StringLiteral source), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))))); (ANF.RawTake ((ANF.StringLiteral source), (ANF.StringLiteral source), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))))); (ANF.RawGetByte ((ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.RawWriteWord ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.RawWriteByte ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.RawSlotInit ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (ANF.StringToRawPtr ((ANF.StringLiteral source))); (ANF.RawPtrToString ((ANF.StringLiteral source))); (ANF.BlobToRawPtr ((ANF.StringLiteral source))); (ANF.RawPtrToBlob ((ANF.StringLiteral source))); (ANF.RawPtrToInt128 ((ANF.StringLiteral source))); (ANF.RawPtrToUInt128 ((ANF.StringLiteral source))); (ANF.DictToRawPtr ((ANF.StringLiteral source))); (ANF.RawPtrToDict ((ANF.StringLiteral source), (ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (ANF.ListToRawPtr ((ANF.StringLiteral source))); (ANF.FixedBlockToRawPtr ((ANF.StringLiteral source))); (ANF.RawPtrToList ((ANF.StringLiteral source), (ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (ANF.RefCountIncString ((ANF.StringLiteral source))); (ANF.RefCountDecString ((ANF.StringLiteral source))); (ANF.RefCountIncBlob ((ANF.StringLiteral source))); (ANF.RefCountDecBlob ((ANF.StringLiteral source))); (ANF.RefCountIncInt ((ANF.StringLiteral source))); (ANF.RefCountDecInt ((ANF.StringLiteral source))); (ANF.RandomInt64); (ANF.DateTimeNow); (ANF.Sleep ((ANF.StringLiteral source))); (ANF.CliNative ((ANF.Execute), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.FloatToString ((ANF.StringLiteral source)))]);
        encode typeof<ANF.AExpr list> (box [(ANF.Let ((ANF.TempId ((3))), (ANF.Atom (ANF.StringLiteral source)), (ANF.Return ANF.UnitLiteral))); (ANF.Return ((ANF.StringLiteral source))); (ANF.If ((ANF.StringLiteral source), (ANF.Return ANF.UnitLiteral), (ANF.Return ANF.UnitLiteral))); (ANF.Join (({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam), (ANF.Return ANF.UnitLiteral), (ANF.Return ANF.UnitLiteral))); (ANF.Jump ((ANF.TempId ((3))), (ANF.StringLiteral source)))]);
        encode typeof<ANF.Function list> (box [({ANF.Function.Id = (AST.functionId System.UInt64.MaxValue); ANF.Function.Name = (source); ANF.Function.TypedParams = [({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam); ({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam)]; ANF.Function.ReturnType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); ANF.Function.ReturnOwnership = (ANF.OwnedReturn); ANF.Function.Body = (ANF.Return ANF.UnitLiteral)} : ANF.Function)]);
        encode typeof<ANF.Program list> (box [(ANF.Program ([({ANF.Function.Id = (AST.functionId System.UInt64.MaxValue); ANF.Function.Name = (source); ANF.Function.TypedParams = [({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam); ({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam)]; ANF.Function.ReturnType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); ANF.Function.ReturnOwnership = (ANF.OwnedReturn); ANF.Function.Body = (ANF.Return ANF.UnitLiteral)} : ANF.Function); ({ANF.Function.Id = (AST.functionId System.UInt64.MaxValue); ANF.Function.Name = (source); ANF.Function.TypedParams = [({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam); ({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam)]; ANF.Function.ReturnType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); ANF.Function.ReturnOwnership = (ANF.OwnedReturn); ANF.Function.Body = (ANF.Return ANF.UnitLiteral)} : ANF.Function)], (ANF.Return ANF.UnitLiteral)))]);
        encode typeof<ANF.VarGen list> (box [(ANF.VarGen ((3)))]);
        encode typeof<ANF.TypedProgram list> (box [({ANF.TypedProgram.Program = (ANF.Program ([({ANF.Function.Id = (AST.functionId System.UInt64.MaxValue); ANF.Function.Name = (source); ANF.Function.TypedParams = [({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam); ({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam)]; ANF.Function.ReturnType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); ANF.Function.ReturnOwnership = (ANF.OwnedReturn); ANF.Function.Body = (ANF.Return ANF.UnitLiteral)} : ANF.Function); ({ANF.Function.Id = (AST.functionId System.UInt64.MaxValue); ANF.Function.Name = (source); ANF.Function.TypedParams = [({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam); ({ANF.TypedParam.Id = (ANF.TempId ((3))); ANF.TypedParam.Type = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.TypedParam)]; ANF.Function.ReturnType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); ANF.Function.ReturnOwnership = (ANF.OwnedReturn); ANF.Function.Body = (ANF.Return ANF.UnitLiteral)} : ANF.Function)], (ANF.Return ANF.UnitLiteral))); ANF.TypedProgram.TypeMap = (ANF.TypeMap.empty)} : ANF.TypedProgram)]);
        encode typeof<ANF.ExprIdGen list> (box [(ANF.ExprIdGen ((3)))])];
      tuple [
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<MemoryModel.CanonicalBufferKind>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<MemoryModel.RcKind>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<MemoryModel.RcShape>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<MemoryModel.RcOperation>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<MemoryModel.RcStorageClass>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<MemoryModel.RcReleasePlan>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<MemoryModel.RcPayloadReleasePlan>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<MemoryModel.RcFieldRelease>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.TempId>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.SizedInt>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.Atom>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.BinOp>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.UnaryOp>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.ReturnOwnership>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.CliOperation>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.CExpr>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.AExpr>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.Program>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.VarGen>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList));
        encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<ANF.ExprIdGen>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList))]]
// END GENERATED IR FIXTURES




let checkedAstFixtures source =
    let one value = AST.NonEmptyList.singleton value
    let var : AST.Expr = AST.Var "x"
    let unit : AST.Expr = AST.UnitLiteral
    let field index = AST.resolvedRecordFieldReference "R" ("field" + string index) index
    let record : AST.RecordReference = {SourceTypeName = "R"; ResolvedTypeName = "R"; TypeArgs = [AST.TInferenceVar (source,"fixed")]}
    let parameter = AST.inferredLambdaVariable "x" AST.TInt64
    let patterns = [AST.PUnit; AST.PWildcard; AST.PVar "x";
        AST.PResolvedConstructor ("S", "Choice", 17, [AST.PVar "x"]); AST.PInt64 Int64.MinValue;
        AST.PBigInt (Numerics.BigInteger.One <<< 256); AST.PInt128Literal Int128.MinValue;
        AST.PInt8Literal -128y; AST.PInt16Literal -32768s; AST.PInt32Literal Int32.MinValue;
        AST.PUInt8Literal 255uy; AST.PUInt16Literal 65535us; AST.PUInt32Literal UInt32.MaxValue;
        AST.PUInt64Literal UInt64.MaxValue; AST.PUInt128Literal UInt128.MaxValue;
        AST.PBool true; AST.PString source; AST.PChar source; AST.PFloat -0.0;
        AST.PTuple [AST.PVar "x"; AST.PWildcard]; AST.PList [AST.PVar "x"];
        AST.PListCons ([AST.PWildcard], AST.PVar "x"); AST.POr (one (AST.PVar "x"))]
    let expressions : AST.Expr list =
        [unit; AST.Int64Literal Int64.MinValue; AST.Int128Literal Int128.MinValue;
        AST.Int8Literal -128y; AST.Int16Literal -32768s; AST.Int32Literal Int32.MinValue; AST.UInt8Literal 255uy;
        AST.UInt16Literal 65535us; AST.UInt32Literal UInt32.MaxValue; AST.UInt64Literal UInt64.MaxValue; AST.UInt128Literal UInt128.MaxValue;
        AST.BigIntLiteral (Numerics.BigInteger.One <<< 256); AST.BoolLiteral true; AST.StringLiteral source; AST.CharLiteral source; AST.FloatLiteral -0.0;
        AST.InterpolatedString [AST.StringText source; AST.StringExpr var]; AST.BinOp (AST.Add, var, unit); AST.UnaryOp (AST.Not, var);
        AST.Let (AST.LPTuple (AST.LPVariable "x", AST.LPWildcard, [AST.LPUnit]), unit, var);
        AST.Var source; AST.Var "Builtin.testNan"; AST.Var "Builtin.testInfinity"; AST.Var "Builtin.blobEmpty";
        AST.If (var, unit, var); AST.Sequence (unit, var); AST.Apply (var, [], one unit); AST.Apply (var, [AST.TInt64], one unit);
        AST.TupleLiteral [unit; var; unit]; AST.TupleAccess (var, 2); AST.DictLiteral (AST.TString, AST.TInt64, [AST.StringLiteral source, var]);
        AST.RecordLiteral (record, [field 1, var; field 0, unit]); AST.RecordUpdate (var, [field 1, unit]); AST.RecordAccess (var, field 1);
        AST.Constructor (AST.ResolvedConstructor (["a"], "S", [AST.TInt64]), "Choice", [var]);
        AST.ListLiteral [var; unit]; AST.Lambda (one parameter, Some AST.TInt64, var);
        AST.Apply (AST.Lambda (one parameter, None, var), [], one unit); AST.IndirectApply (var, one unit);
        AST.Closure (source, [var]); AST.RuntimeError source; AST.BoundaryRender (source, var);
        AST.TupleLiteral []; AST.TupleLiteral [unit]; AST.Match (unit, []);
        AST.RecordLiteral (record, []); AST.RecordAccess (var, AST.unresolvedRecordFieldReference "field");
        AST.Constructor (AST.UnresolvedConstructor None, "Absent", []); AST.Lambda (one (AST.lambdaParameter (AST.LPVariable "x")), None, var);
        AST.Apply (unit, [AST.TUnit], one unit)] @
        (patterns |> List.map (fun pattern -> AST.Match (var, [{Patterns = one pattern; Guard = Some var; Body = var}])))
    let lookup : Map<string,string * string list * int * AST.SemanticType list> = Map.ofList ["S.Choice", ("S", [], 17, [])]
    let catalog = CheckedAST.includeFunctionNames [source; "z"; "aa"; "z"] CheckedAST.emptyFunctionCatalog
    let methodInfo = typeof<AST.SemanticType>.Assembly.GetType("CheckedAST").GetMethod("ofTypedProgram", Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
    let convert (topLevels : AST.TopLevel list) : Result<CheckedAST.Program,string> =
        methodInfo.Invoke(null,[|box lookup; box (Set.singleton "external"); box CheckedAST.emptyTypeCatalog; box catalog; box (fun name -> if name = "R" then Some 2 else None); box (AST.Program (AST.TypeDef (AST.RecordDef ("R",[],["field0",AST.TUnit; "field1",AST.TInt64])) :: AST.TypeDef (AST.SumTypeDef ("S",[],[{Name = "Choice"; Fields = []}])) :: topLevels))|]) :?> Result<CheckedAST.Program,string>
    let converted = expressions |> List.map (fun expr -> convert [AST.Expression ([],expr)])
    converted




let closureAnalysisCall<'a> name args : 'a =
    let flags=Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    unbox (typeof<AST.SemanticType>.Assembly.GetType("ClosureAnalysis").GetMethod(name,flags).Invoke(null,args))

let closureAnalysisEncode<'a> (value:'a) = encode typeof<'a> (box value)

let closureComparisonMethod name = typeof<AST.SemanticType>.Assembly.GetType("ClosureComparisons").GetMethod(name,Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
let closureComparisonCall<'a> name args : 'a = unbox ((closureComparisonMethod name).Invoke(null,args))

let closureAnalysisWith mode source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let enc value = closureAnalysisEncode value
    let outcome encoder value =
        match value with
        | Error error -> enc (Error error : Result<unit,string>)
        | Ok value ->
            let node=JsonObject()
            node["type"]<-JsonValue.Create "FSharpResult"
            node["case"]<-JsonValue.Create "Ok"
            node["fields"]<-JsonArray([|encoder value|])
            node :> JsonNode
    let types=[AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr; AST.TVar "a"; AST.TVar "b"; AST.TInferenceVar (source,"fixed"); AST.TRecord ("R",[]); AST.TRecord ("R",[AST.TVar "a"]); AST.TSum ("S",[AST.TVar "a"]); AST.TSum ("S",[]); AST.TTuple [AST.TVar "a";AST.TString]; AST.TList (AST.TVar "a"); AST.TStream (AST.TVar "a"); AST.TDict (AST.TVar "a",AST.TString); AST.TFunction ([AST.TVar "a"],AST.TString)]
    let ids=[AST.bindingId 0;AST.namedBindingId 0 "x";AST.namedBindingId 1 "x";AST.topLevelValueId source]
    let bounds=[Set.empty;Set.ofList ids]
    let program program =
        let symbols=CheckedAST.programSymbols program
        let tops=CheckedAST.programTopLevels program
        let functions=tops |> List.choose (function CheckedAST.FunctionDef value -> Some value | _ -> None)
        let bodies=tops |> List.choose (function CheckedAST.FunctionDef value -> Some value.Body | CheckedAST.ValueDef value -> Some value.Body | CheckedAST.Expression value -> Some value | _ -> None)
        let env=WrittenChecking.typeCheckEnvironment program
        let records : TypeRegistries.TypeRegistry = env.IndexedTypeReg |> Map.map (fun _ (info:CheckingTypes.RecordTypeInfo) -> {TypeParams=info.TypeParams;Fields=info.Fields})
        let typeEnv=CheckedAST.programValues program |> Map.toList |> List.map (fun (name,(typ,_)) -> AST.topLevelValueId name,typ) |> Map.ofList
        let parameters=functions |> List.map (fun func -> func.Id,CheckedAST.functionParameterTypes func |> AST.NonEmptyList.toList |> List.map snd) |> FunctionIdMap.ofList
        let returns=functions |> List.map (fun func -> func.Id,CheckedAST.functionReturnType func) |> FunctionIdMap.ofList
        let generic=functions |> List.map (fun func -> func.Id,(func.TypeParams,CheckedAST.functionReturnType func)) |> FunctionIdMap.ofList
        let initial : ClosureAnalysis.LiftState = {Symbols=symbols;Counter=0;LiftedFunctions=functions;ComparisonFuncs=Map.ofList [(AST.functionId 9UL,[AST.TVar "a"]),source;(AST.functionId UInt64.MaxValue,[AST.TInt64]),"other"];ComparableFunctionParams=Set.ofList [[];[AST.TInt64];[AST.TVar "a"]];TypeEnv=typeEnv;FuncParams=parameters;FuncReturnTypes=returns;GenericFuncDefs=generic;TypeReg=records;VariantLookup=env.VariantLookup;RecursiveSelf=None}
        let parameters : AST.NonEmptyList<CheckedAST.LambdaParameter> = AST.NonEmptyList.singleton {Pattern=CheckedAST.LPVariable (AST.namedBindingId 0 "x");Type=CheckedAST.checkedType AST.TInt64}
        let extra=[CheckedAST.Local (AST.namedBindingId 0 "x");CheckedAST.Closure (AST.functionId 9UL,[CheckedAST.Local (AST.namedBindingId 0 "x")]);CheckedAST.Call (AST.functionId 9UL,AST.NonEmptyList.singleton CheckedAST.UnitLiteral);CheckedAST.TypeApp (AST.functionId 9UL,CheckedAST.checkedTypeArgs [AST.TString],AST.NonEmptyList.singleton CheckedAST.UnitLiteral);CheckedAST.Lambda (parameters,Some (CheckedAST.checkedType AST.TString),CheckedAST.Local (AST.namedBindingId 0 "x"));CheckedAST.If (CheckedAST.BoolLiteral true,CheckedAST.ListLiteral [],CheckedAST.ListLiteral [CheckedAST.Int64Literal 1L])]
        let states=[initial;{initial with TypeEnv=ids |> List.map (fun id -> id,AST.TInt64) |> Map.ofList;FuncParams=FunctionIdMap.add (AST.functionId 9UL) [AST.TInt64;AST.TBool] initial.FuncParams;FuncReturnTypes=FunctionIdMap.add (AST.functionId 9UL) AST.TNever initial.FuncReturnTypes;GenericFuncDefs=FunctionIdMap.add (AST.functionId 9UL) (["a"],AST.TList (AST.TVar "a")) initial.GenericFuncDefs}]
        let infer (state:ClosureAnalysis.LiftState) expr=ClosureAnalysis.simpleInferType expr state.TypeEnv state.FuncParams state.FuncReturnTypes state.GenericFuncDefs state.TypeReg state.VariantLookup (TypeRegistries.typeNamesFromSymbols state.Symbols)
        let attempt encoder action = outcome encoder (try Ok (action ()) with error -> Error (if error :? Reflection.TargetInvocationException && not (isNull error.InnerException) then error.InnerException.Message else error.Message))
        let expression expr=tuple [bounds |> List.map (fun bound -> enc (ClosureAnalysis.freeVars expr bound)) |> list;states |> List.map (fun state -> tuple [attempt enc (fun () -> infer state expr);attempt enc (fun () -> ClosureAnalysis.inferLambdaReturnType expr state)]) |> list]
        let cases=bodies |> List.collect (function CheckedAST.Match (_,cases) -> AST.NonEmptyList.toList cases |> List.collect (fun case -> AST.NonEmptyList.toList case.Patterns) | _ -> [])
        let functionId,symbols=CheckedAST.internFunction "__lift0" symbols
        let fake : CheckedAST.FunctionDef = {Id=functionId;Name="__lift1";TypeParams=[];Params=AST.NonEmptyList.singleton (AST.bindingId 0,CheckedAST.checkedType AST.TInt64);ReturnType=CheckedAST.checkedType AST.TInt64;Body=CheckedAST.UnitLiteral;Recursion=None}
        let collision={initial with Symbols=symbols;FuncParams=FunctionIdMap.add functionId [] initial.FuncParams;LiftedFunctions=fake::initial.LiftedFunctions}
        let fresh state prefix=let name,next=closureAnalysisCall<string * ClosureAnalysis.LiftState> "freshLiftedName" [|box state;box prefix|] in tuple [encodeString name;enc next]
        let lifted expr state=attempt (outcome (fun (expr,next) -> tuple [enc expr;enc next])) (fun () -> LiftExpressions.liftLambdasInExpr expr state)
        let comparisons (state:ClosureAnalysis.LiftState) =
            let names=[None;Some (AST.functionId 9UL);Some (AST.functionId UInt64.MaxValue)] |> List.map (fun identity -> [[];[AST.TInt64];[AST.TVar "a"]] |> List.map (fun args -> let name,add,next=closureComparisonCall<string*bool*ClosureAnalysis.LiftState> "comparisonNameForIdentity" [|box identity;box args;box state|] in tuple [encodeString name;enc add;enc next]) |> list) |> list
            let captureTypes=[[];[AST.TInt64];[AST.TString];[AST.TInt];[AST.TList AST.TInt64];[AST.TFunction ([AST.TInt64],AST.TBool)];[AST.TTuple [AST.TInt64;AST.TString]]]
            let symbols=("Darklang.Stdlib.Int.__equals" :: (captureTypes |> List.collect (List.map ComparisonPlanning.eqHelperName))) |> List.fold (fun symbols name -> CheckedAST.internFunction name symbols |> snd) state.Symbols
            let comparator=[state.Symbols;symbols] |> List.map (fun symbols -> captureTypes |> List.map (fun captures -> [false;true] |> List.map (fun compare -> attempt enc (fun () -> closureComparisonCall<CheckedAST.FunctionDef*CheckedAST.Symbols> "makeClosureComparator" [|box "__comparator";box captures;box compare;box state.VariantLookup;box symbols|])) |> list) |> list) |> list
            let partial,symbols=CheckedAST.allocateBinding "__partial_0" state.Symbols
            let partialParameters : AST.NonEmptyList<CheckedAST.LambdaParameter> = AST.NonEmptyList.singleton {Pattern=CheckedAST.LPVariable partial;Type=CheckedAST.checkedType AST.TBool}
            let partialState={state with Symbols=symbols;FuncParams=FunctionIdMap.add (AST.functionId 9UL) [AST.TInt64;AST.TBool] state.FuncParams}
            let partialBodies=[CheckedAST.Call (AST.functionId 9UL,AST.NonEmptyList.fromList [CheckedAST.Int64Literal 1L;CheckedAST.Local partial]);CheckedAST.TypeApp (AST.functionId 9UL,CheckedAST.checkedTypeArgs [AST.TString],AST.NonEmptyList.fromList [CheckedAST.Int64Literal 1L;CheckedAST.Local partial])]
            let plans parameters state expr=attempt (fun value -> encode (closureComparisonMethod "planLambdaComparison").ReturnType value) (fun () -> (closureComparisonMethod "planLambdaComparison").Invoke(null,[|box parameters;box expr;box state|]))
            tuple [names;comparator;(bodies @ extra) |> List.map (plans parameters state) |> list;partialBodies |> List.map (plans partialParameters partialState) |> list]
        let wrappers=[FunctionIdMap.empty;FunctionIdMap.ofList [AST.functionId 9UL,(AST.functionId 10UL,AST.functionId 11UL);AST.functionId 0UL,(AST.functionId 12UL,AST.functionId 13UL)]]
        let functionsReport (state:ClosureAnalysis.LiftState) =
            let withFuncs : LiftFunctions.LiftStateWithFuncs = {State=state;FuncParams=state.FuncParams;GeneratedWrappers=List.item 1 wrappers}
            let lifted=functions |> List.map (fun func -> attempt enc (fun () -> LiftFunctions.liftLambdasInFunc func state)) |> list
            let wrapped=[AST.functionId 9UL;AST.functionId 0UL;AST.functionId UInt64.MaxValue] |> List.map (fun id -> attempt enc (fun () -> LiftFunctions.generateFuncWrapper id state.FuncParams state.FuncReturnTypes withFuncs)) |> list
            let expressions=(bodies @ extra) |> List.map (fun expr -> tuple [enc (LiftFunctions.collectFuncRefsInExpr expr state.FuncParams);wrappers |> List.map (fun wrappers -> enc (LiftFunctions.replaceInExpr wrappers expr)) |> list]) |> list
            let tops=wrappers |> List.map (fun wrappers -> enc (tops |> List.map (LiftFunctions.replaceFuncRefsWithWrappers wrappers))) |> list
            let registry,variants=LiftFunctions.prepareLambdaLiftBaseTypes state.TypeReg state.VariantLookup
            let prepared=enc {state with TypeReg=registry;VariantLookup=variants}
            let catalog : LiftFunctions.FunctionCatalog = {Params=state.FuncParams;ReturnTypes=state.FuncReturnTypes;GenericDefs=state.GenericFuncDefs}
            let programs=[Map.empty,Map.empty;state.TypeReg,state.VariantLookup] |> List.map (fun (registry,variants) -> attempt enc (fun () -> LiftFunctions.liftLambdasInProgram registry variants catalog program)) |> list
            tuple [lifted;wrapped;expressions;tops;prepared;programs]
        let report =
            if mode="lift-functions" then tuple [enc initial;states |> List.map functionsReport |> list]
            elif mode="lift-expressions" then tuple [enc initial;(bodies @ extra) |> List.map (fun expr -> states |> List.map (lifted expr) |> list) |> list;states |> List.map (fun state -> attempt (outcome (fun (args,next) -> tuple [enc (AST.NonEmptyList.toList args);enc next])) (fun () -> LiftExpressions.liftLambdasInArgs (AST.NonEmptyList.fromList (bodies @ extra)) state)) |> list]
            elif mode="closure-comparisons" then tuple [enc initial;states |> List.map comparisons |> list;(bodies @ extra) |> List.map (fun expr -> ids |> List.map (fun self -> tuple [enc (closureComparisonCall<CheckedAST.Expr> "rewriteRecursiveSelfReferences" [|box self;box (AST.bindingId 1);box expr|]);enc (closureComparisonCall<CheckedAST.Expr> "rewriteLiftedSelfCalls" [|box (AST.functionId 9UL);box self;box expr|])]) |> list) |> list]
            else
                 tuple [enc initial;(bodies @ extra) |> List.map expression |> list;
                cases |> List.map (fun pattern -> types |> List.map (fun typ -> enc (closureAnalysisCall<Map<AST.BindingId,AST.SemanticType>> "matchPatternBindingTypes" [|box records;box env.VariantLookup;box (TypeRegistries.typeNamesFromSymbols initial.Symbols);box pattern;box typ|])) |> list) |> list;
                states |> List.map (fun state -> enc (closureAnalysisCall<bool> "lambdaNeedsComparison" [|box parameters;box state|])) |> list;
                [initial;collision;{initial with Counter=Int32.MaxValue}] |> List.map (fun state -> ["__lift";source] |> List.map (fresh state) |> list) |> list]
        report
    let sourceProgram source=WrittenParsing.parse LibParser.Validation.Script source |> Result.bind (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit]) |> Result.map (fun (_,value,_) -> value) |> outcome program
    tuple [types |> List.map (fun left -> types |> List.map (fun right -> enc (ClosureAnalysis.reconcileBranchTypes left right)) |> list) |> list;checkedAstFixtures source |> List.map (outcome program) |> list;sourceProgram source;
        if source="" then ["let id (x: 'a) : 'a = x\nid 1";"type S<'a> = A of 'a | B\nlet f (x: S<Int64>) : Int64 = match x with | S.A value -> value | S.B -> 0\nf (S.A 1)";"let recur (x: Int64) : Int64 = if x == 0 then x else recur (x - 1)\nrecur 1"] |> List.map sourceProgram |> list else list []]

let closureAnalysis = closureAnalysisWith "closure-analysis"
let closureComparisons = closureAnalysisWith "closure-comparisons"
let liftExpressions = closureAnalysisWith "lift-expressions"
let liftFunctions = closureAnalysisWith "lift-functions"

let monomorphizationCall<'a> name args : 'a =
    let flags=Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    unbox (typeof<AST.SemanticType>.Assembly.GetType("Monomorphization").GetMethod(name,flags).Invoke(null,args))

let checkedProgramFromParts symbols tops : CheckedAST.Program =
    let flags=Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    unbox (typeof<AST.SemanticType>.Assembly.GetType("CheckedAST").GetMethod("programFromCheckedParts",flags).Invoke(null,[|box symbols;box tops|]))

let monomorphization (source:string) =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let enc value=closureAnalysisEncode value
    let outcome encoder value =
        match value with
        | Error error -> enc (Error error : Result<unit,string>)
        | Ok value ->
            let node=JsonObject()
            node["type"]<-JsonValue.Create "FSharpResult"
            node["case"]<-JsonValue.Create "Ok"
            node["fields"]<-JsonArray([|encoder value|])
            node :> JsonNode
    let attempt encoder action=outcome encoder (try Ok (action ()) with error -> Error (if error :? Reflection.TargetInvocationException && not (isNull error.InnerException) then error.InnerException.Message else error.Message))
    let types=[[];[AST.TInt64];[AST.TList AST.TInt64];[AST.TVar "a"];[AST.TStream (AST.TVar "a")];[AST.TFunction ([AST.TInt64],AST.TBool)];[AST.TRecord ("R",[])];[AST.TString;AST.TInt64]]
    let names=["__dark_internal_eq_helper_dispatch";"__compare";"__hash";"__key_eq";"Dict.fromList";"Darklang.Stdlib.Dict.fromList";"Darklang.Stdlib.Dict.empty";"__raw_get";"Builtin.pmEvaluateValue";source.Substring(0,min 32 source.Length);"identity"]
    let program program =
        let original=CheckedAST.programSymbols program
        let additional=names @ ["Builtin.testRuntimeError";"Darklang.Stdlib.Dict.__setOverwriting"] @ (names |> List.collect (fun name -> types |> List.map (SpecializationIdentity.specName name))) @ (types |> List.collect (List.collect (fun typ -> [ComparisonPlanning.eqHelperName typ;ComparisonPlanning.compareHelperName typ])))
        let symbols=additional |> List.fold (fun symbols name -> CheckedAST.internFunction name symbols |> snd) original
        let targets=names |> List.map (fun name -> CheckedAST.tryFindFunctionId name symbols |> Option.get)
        let bodies=CheckedAST.programTopLevels program |> List.choose (function CheckedAST.FunctionDef func -> Some func.Body | CheckedAST.ValueDef value -> Some value.Body | CheckedAST.Expression expr -> Some expr | _ -> None)
        let arguments=[AST.NonEmptyList.singleton CheckedAST.UnitLiteral;AST.NonEmptyList.fromList [CheckedAST.Int64Literal 1L;CheckedAST.Int64Literal 2L];AST.NonEmptyList.singleton (CheckedAST.ListLiteral [])]
        let synthetic=targets |> List.collect (fun target -> types |> List.collect (fun types -> arguments |> List.map (fun args -> CheckedAST.TypeApp (target,CheckedAST.checkedTypeArgs types,args))))
        let registry=names |> List.collect (fun name -> types |> List.map (fun types -> (name,types),SpecializationIdentity.specName name types)) |> Map.ofList
        let registries=[Map.empty;registry]
        let expression expr=tuple [attempt enc (fun () -> Monomorphization.collectTypeApps symbols expr);enc (Monomorphization.collectCalledFunctions expr);attempt enc (fun () -> Monomorphization.replaceTypeApps symbols expr);registries |> List.map (fun registry -> attempt enc (fun () -> Monomorphization.replaceTypeAppsWithRegistry symbols registry expr)) |> list]
        let functions=CheckedAST.programTopLevels program |> List.choose (function CheckedAST.FunctionDef func -> Some func | _ -> None)
        let func func=tuple [attempt enc (fun () -> Monomorphization.collectTypeAppsFromFunc symbols func);attempt enc (fun () -> Monomorphization.replaceTypeAppsInFunc symbols func);registries |> List.map (fun registry -> attempt enc (fun () -> Monomorphization.replaceTypeAppsInFuncWithRegistry symbols registry func)) |> list]
        let definitions=SpecializationIdentity.extractGenericFuncDefs program
        let initial=Set.ofList ["identity",[AST.TInt64];"external",[AST.TVar "a"];"__hash",[AST.TString]]
        let initials=[Set.empty;initial;Set.union initial (bodies |> List.fold (fun specs expr -> try Set.union specs (Monomorphization.collectTypeApps symbols expr) with _ -> specs) Set.empty)]
        let programWithSymbols=checkedProgramFromParts symbols (CheckedAST.programTopLevels program)
        tuple [(bodies @ synthetic) |> List.map expression |> list;functions |> List.map func |> list;initials |> List.map (fun specs -> attempt enc (fun () -> Monomorphization.specializeFromSpecs symbols definitions specs)) |> list;
            registries |> List.map (fun registry -> attempt enc (fun () -> Monomorphization.replaceTypeAppsInProgramWithRegistry registry programWithSymbols)) |> list;
            attempt enc (fun () -> monomorphizationCall<CheckedAST.Program> "monomorphizeWithGenericFuncDefs" [|box definitions;box programWithSymbols|]);attempt enc (fun () -> PrepareFunctions.monomorphize programWithSymbols);attempt enc (fun () -> PrepareFunctions.monomorphizeWithExternalDefs definitions programWithSymbols);
            [Set.empty;Set.ofList names] |> List.map (fun known -> enc (Monomorphization.programNeedsLambdaLowering known program)) |> list]
    let sourceProgram source=WrittenParsing.parse LibParser.Validation.Script source |> Result.bind (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit]) |> Result.map (fun (_,value,_) -> value) |> outcome program
    tuple [checkedAstFixtures source |> List.map (outcome program) |> list;sourceProgram source;
        if source="" then ["let identity (x: 'a) : 'a = x\nidentity 1";"let eq (x: 'a) (y: 'a) : Bool = x == y\neq [1] [2]";"let f = fun (x: Int64) -> x\nf 1";"let a (x: 'a) : 'a = x\nlet b (x: 'a) : 'a = a x\nb 1";"Stdlib.Dict.fromList []"] |> List.map sourceProgram |> list else list []]

let loweringAnalysisTypes=[AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr; AST.TVar "a"; AST.TInferenceVar ("scope","fixed"); AST.TRecord ("R",[]); AST.TRecord ("Generic",[AST.TString]); AST.TSum ("S",[]); AST.TSum ("Nullable",[]); AST.TSum ("Transparent",[]); AST.TSum ("Uuid",[]); AST.TTuple []; AST.TTuple [AST.TInt64;AST.TTuple [AST.TString;AST.TInt128]]; AST.TList AST.TInt64; AST.TStream AST.TInt64; AST.TDict (AST.TString,AST.TInt64); AST.TFunction ([AST.TInt64],AST.TBool)]
let loweringAnalysisOps=[AST.Add;AST.Sub;AST.Mul;AST.Div;AST.Mod;AST.Pow;AST.Shl;AST.Shr;AST.BitAnd;AST.BitOr;AST.BitXor;AST.Eq;AST.Neq;AST.Lt;AST.Gt;AST.Lte;AST.Gte;AST.And;AST.Or;AST.StringConcat]
let loweringOperatorCall<'a> name args : 'a =
    let flags=Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    unbox (typeof<AST.SemanticType>.Assembly.GetType("LoweringOperators").GetMethod(name,flags).Invoke(null,args))
let loweringTypes source =
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let enc value=closureAnalysisEncode value
    let outcome encoder value =
        match value with
        | Error error -> enc (Error error : Result<unit,string>)
        | Ok value ->
            let node=JsonObject()
            node["type"]<-JsonValue.Create "FSharpResult"
            node["case"]<-JsonValue.Create "Ok"
            node["fields"]<-JsonArray([|encoder value|])
            node :> JsonNode
    let attempt action=enc (try Ok (action ()) with error -> Error error.Message)
    let intrinsics=["Builtin.unwrap"; "Builtin.testRuntimeError"; "Builtin.crash"; "Builtin.pmEvaluateValue_i64"; "__raw_get_i64"; "__raw_get_list_i64"; "__raw_take_str"; "__stream_to_rawptr_a"; "__rawptr_to_stream_i64"; "__raw_slot_init_i64"; "__hash_i64"; "__key_eq_str"; "__empty_dict_str_i64"; "__dict_is_null_str_i64"; "__dict_get_tag_str_i64"; "__dict_to_rawptr_str_i64"; "__rawptr_to_dict_str_i64"; "__list_is_null_i64"; "__list_get_tag_i64"; "__list_to_rawptr_i64"; "__rawptr_to_list_i64"; "__list_empty_i64"; "__raw_get_invalid__"; "Darklang.Stdlib.File.exists"; "unknown"]
    let program program =
        let symbols=CheckedAST.programSymbols program
        let tops=CheckedAST.programTopLevels program
        let env=WrittenChecking.typeCheckEnvironment program
        let registry : TypeRegistries.TypeRegistry = env.IndexedTypeReg |> Map.map (fun _ (info:CheckingTypes.RecordTypeInfo) -> {TypeParams=info.TypeParams;Fields=info.Fields})
        let variants=env.VariantLookup
        let sums=LoweringPrimitives.sumMetadataFromVariantLookup variants
        let typeNames=TypeRegistries.typeNamesFromSymbols symbols
        let funcs=tops |> List.choose (function CheckedAST.FunctionDef func -> Some func | _ -> None)
        let functions=funcs |> List.map (fun func -> func.Id,(func.Name,AST.TFunction (CheckedAST.functionParameterTypes func |> AST.NonEmptyList.toList |> List.map snd,CheckedAST.functionReturnType func))) |> FunctionIdMap.ofList
        let names=functions |> FunctionIdMap.map (fun _ (name,_) -> name)
        let environment=CheckedAST.programValues program |> Map.toList |> List.map (fun (name,(typ,_)) -> AST.topLevelValueId name,typ) |> Map.ofList
        let environments=[environment;[AST.bindingId 0,AST.TInt64;AST.bindingId 1,AST.TString;AST.namedBindingId 0 "x",AST.TInt64;AST.namedBindingId 1 "x",AST.TList AST.TString] |> List.fold (fun env (id,typ) -> Map.add id typ env) environment]
        let bodies=tops |> List.choose (function CheckedAST.FunctionDef func -> Some func.Body | CheckedAST.ValueDef value -> Some value.Body | CheckedAST.Expression expr -> Some expr | _ -> None)
        let expression expr=environments |> List.map (fun environment -> [Map.empty;Stdlib.buildModuleRegistry ()] |> List.map (fun modules -> attempt (fun () -> LoweringTypeInference.inferTypeCore sums typeNames expr environment registry variants functions names modules)) |> list) |> list
        let intrinsic name=
            let names=FunctionIdMap.add (AST.functionId 9UL) name names
            [AST.NonEmptyList.singleton CheckedAST.UnitLiteral;AST.NonEmptyList.singleton (CheckedAST.Int64Literal 1L);AST.NonEmptyList.fromList [CheckedAST.Int64Literal 1L;CheckedAST.Int64Literal 2L]] |> List.map (fun args -> [FunctionIdMap.empty;FunctionIdMap.add (AST.functionId 9UL) ("other",AST.TInt64) functions;FunctionIdMap.add (AST.functionId 9UL) (name,AST.TFunction ([AST.TUnit],AST.TBool)) functions] |> List.map (fun functionRegistry -> attempt (fun () -> LoweringTypeInference.inferTypeCore sums typeNames (CheckedAST.Call (AST.functionId 9UL,args)) environment registry variants functionRegistry names (Stdlib.buildModuleRegistry ()))) |> list) |> list
        let numeric typ=
            let environment=environment |> Map.add (AST.bindingId 1) typ |> Map.add (AST.bindingId 0) typ
            let infer expr=attempt (fun () -> LoweringTypeInference.inferTypeCore sums typeNames expr environment registry variants functions names (Stdlib.buildModuleRegistry ()))
            tuple [loweringAnalysisOps |> List.map (fun op -> infer (CheckedAST.BinOp (op,CheckedAST.Local (AST.bindingId 0),CheckedAST.Local (AST.bindingId 1)))) |> list;[AST.Neg;AST.Not;AST.BitNot] |> List.map (fun op -> infer (CheckedAST.UnaryOp (op,CheckedAST.Local (AST.bindingId 0)))) |> list]
        tuple [bodies |> List.map expression |> list;(if source="" then intrinsics |> List.map intrinsic |> list else list []);(if source="" then loweringAnalysisTypes |> List.map numeric |> list else list [])]
    let sourceProgram source=WrittenParsing.parse LibParser.Validation.Script source |> Result.bind (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit]) |> Result.map (fun (_,value,_) -> value) |> outcome program
    tuple [checkedAstFixtures source |> List.map (outcome program) |> list;sourceProgram source;if source="" then ["type R<'a> = { value: 'a }\nR { value = 1 }";"type S<'a> = A of 'a | B\nmatch S.A 1 with | S.A x -> x | S.B -> 0";"if true then [] else [1]";"let f = fun (x: Int64) -> x\nf 1"] |> List.map sourceProgram |> list else list []]

let loweringOperators source =
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let enc value=closureAnalysisEncode value
    let attempt action=enc (try Ok (action ()) with error -> Error (if error :? Reflection.TargetInvocationException && not (isNull error.InnerException) then error.InnerException.Message else error.Message))
    if source<>"" then list [] else
    let registry : TypeRegistries.TypeRegistry = Map.ofList ["R",{TypeParams=[];Fields=["a",AST.TInt64;"b",AST.TTuple [AST.TString;AST.TInt128]]};"Generic",{TypeParams=["a"];Fields=["value",AST.TVar "a"]}]
    let variants=Map.ofList ["S.A",("S",[],0,[AST.TInt64;AST.TBool]);"S.B",("S",[],1,[]);"Nullable.None",("Nullable",[],0,[]);"Nullable.Some",("Nullable",[],1,[AST.TString]);"Transparent.A",("Transparent",[],0,[AST.TChar]);"Uuid.Uuid",("Uuid",[],0,[AST.TUInt128])]
    let cases=LoweringPrimitives.sumRepresentationIndex variants
    let resolved action=
        let requests=ResizeArray<string>()
        let resolve name=requests.Add name;AST.functionId 9UL
        let value=attempt (fun () -> action resolve)
        tuple [value;enc (List.ofSeq requests)]
    tuple [loweringAnalysisOps |> List.map (fun op -> attempt (fun () -> LoweringOperators.convertBinOp op)) |> list;
        loweringAnalysisTypes |> List.map (fun typ -> loweringAnalysisOps |> List.map (fun op -> resolved (fun resolve -> loweringOperatorCall<AST.FunctionId option> "integerFunctionForBinOp" [|box resolve;box typ;box op|])) |> list) |> list;
        enc ([AST.Neg;AST.Not;AST.BitNot] |> List.map LoweringOperators.convertUnaryOp);enc (loweringAnalysisTypes |> List.map LoweringOperators.isCompoundType);
        loweringAnalysisTypes |> List.map (fun typ -> [0;Int32.MinValue;Int32.MaxValue] |> List.map (fun gen -> resolved (fun resolve -> LoweringOperators.generateStructuralEquality resolve (ANF.Var (ANF.TempId 0)) (ANF.Var (ANF.TempId 1)) typ (ANF.VarGen gen) registry variants cases)) |> list) |> list]

let loweringAggregateCall<'a> name args : 'a =
    let flags=Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    unbox (typeof<AST.SemanticType>.Assembly.GetType("LoweringAggregates").GetMethod(name,flags).Invoke(null,args))
let loweringAggregates source =
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let enc value=closureAnalysisEncode value
    let initial=[ANF.TempId -7,ANF.Atom (ANF.StringLiteral source);ANF.TempId -6,ANF.TypedAtom (ANF.UnitLiteral,AST.TUnit)]
    let ids=[AST.bindingId 0;AST.namedBindingId 0 "x";AST.namedBindingId 1 "x";AST.topLevelValueId source]
    let patterns=[CheckedAST.LPUnit;CheckedAST.LPWildcard] @ (ids |> List.map CheckedAST.LPVariable) @ [CheckedAST.LPTuple (CheckedAST.LPVariable ids.Head,CheckedAST.LPVariable ids.Head,[]);CheckedAST.LPTuple (CheckedAST.LPUnit,CheckedAST.LPWildcard,[]);CheckedAST.LPTuple (CheckedAST.LPTuple (CheckedAST.LPVariable ids.Head,CheckedAST.LPWildcard,[]),CheckedAST.LPVariable ids[1],[CheckedAST.LPUnit])]
    let patternTypes=[AST.TUnit;AST.TInt64;AST.TTuple [];AST.TTuple [AST.TInt64;AST.TString];AST.TTuple [AST.TUnit;AST.TString];AST.TTuple [AST.TTuple [AST.TBool;AST.TString];AST.TInt64;AST.TUnit];AST.TTuple [AST.TInt64]]
    let env : TypeRegistries.VarEnv = Map.ofList [ids.Head,(ANF.TempId -2,AST.TBool);ids[1],(ANF.TempId -3,AST.TString)]
    tuple [patterns |> List.map (fun pattern -> patternTypes |> List.map (fun typ -> tuple [enc (loweringAggregateCall<bool> "letPatternAcceptsType" [|box pattern;box typ|]);[0;Int32.MinValue;Int32.MaxValue] |> List.map (fun gen -> enc (loweringAggregateCall<Result<TypeRegistries.VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen,string>> "lowerLetPatternBindings" [|box pattern;box (ANF.Var (ANF.TempId -1));box typ;box env;box initial;box (ANF.VarGen gen)|])) |> list]) |> list) |> list;
        (if source="" then loweringAnalysisTypes |> List.map (fun typ -> [0;1;2;3;6;7;14;15;31;32;64] |> List.map (fun count -> [0;Int32.MinValue;Int32.MaxValue] |> List.map (fun gen -> let elements=List.init count (fun index -> ANF.IntLiteral (ANF.Int64 (int64 index)),typ) in enc (loweringAggregateCall<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen> "buildSkewListLiteral" [|box (AST.TList typ);box elements;box (ANF.VarGen gen);box initial|])) |> list) |> list) |> list else list [])]

let atomLowering source =
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let enc value=closureAnalysisEncode value
    let outcome encoder value =
        match value with
        | Error error -> enc (Error error : Result<unit,string>)
        | Ok value ->
            let node=JsonObject()
            node["type"]<-JsonValue.Create "FSharpResult"
            node["case"]<-JsonValue.Create "Ok"
            node["fields"]<-JsonArray([|encoder value|])
            node :> JsonNode
    let attempt action=enc (try Ok (action ()) with error -> Error error.Message)
    let program program =
        let symbols=CheckedAST.programSymbols program
        let tops=CheckedAST.programTopLevels program
        let types=WrittenChecking.typeCheckEnvironment program
        let registry : TypeRegistries.TypeRegistry = types.IndexedTypeReg |> Map.map (fun _ (info:CheckingTypes.RecordTypeInfo) -> {TypeParams=info.TypeParams;Fields=info.Fields})
        let variants=types.VariantLookup
        let sums=LoweringPrimitives.sumMetadataFromVariantLookup variants
        let typeNames=TypeRegistries.typeNamesFromSymbols symbols
        let funcs=tops |> List.choose (function CheckedAST.FunctionDef func -> Some func | _ -> None)
        let functions=funcs |> List.map (fun func -> func.Id,(func.Name,AST.TFunction (CheckedAST.functionParameterTypes func |> AST.NonEmptyList.toList |> List.map snd,CheckedAST.functionReturnType func))) |> FunctionIdMap.ofList
        let names=CheckedAST.functionNames symbols
        let extras=["Darklang.Stdlib.Int.__value";"Darklang.Stdlib.Int.__equals";"Darklang.Stdlib.Int.bitwiseNot";"Darklang.Stdlib.Int128.__value";"Darklang.Stdlib.UInt128.__value";"Darklang.Stdlib.Int128.__equals";"Darklang.Stdlib.UInt128.__equals";"Darklang.Stdlib.Int128.bitwiseNot";"Darklang.Stdlib.UInt128.bitwiseNot";"Darklang.Stdlib.String.__normalizeAfterConcat"]
        let ids=AST.allocateFunctionIds (names |> FunctionIdMap.toList |> Seq.map fst) (extras |> Seq.filter (fun name -> not (Map.containsKey name (CheckedAST.functionIds symbols))))
        let names=ids |> Map.fold (fun names name id -> FunctionIdMap.add id name names) names
        let ids=TypeRegistries.functionIdsFromNames names
        let globals : TypeRegistries.VarEnv = CheckedAST.programValues program |> Map.toList |> List.mapi (fun index (name,(typ,_)) -> AST.topLevelValueId name,(ANF.TempId (-100-index),typ)) |> Map.ofList
        let environments=[globals;[AST.bindingId 0,(ANF.TempId -2,AST.TInt64);AST.bindingId 1,(ANF.TempId -3,AST.TString);AST.namedBindingId 0 "x",(ANF.TempId -4,AST.TInt64);AST.namedBindingId 1 "x",(ANF.TempId -5,AST.TList AST.TString)] |> List.fold (fun env (id,value) -> Map.add id value env) globals]
        let bodies=tops |> List.choose (function CheckedAST.FunctionDef func -> Some func.Body | CheckedAST.ValueDef value -> Some value.Body | CheckedAST.Expression value -> Some value | _ -> None)
        let expression expr =
            let inEnvironment env =
                let atGenerator gen =
                    let requests=ResizeArray<JsonNode>()
                    let rec atom : LoweringCallbacks.AtomLowerer = fun sums types inert expr gen env registry variants functions names modules ->
                        requests.Add (tuple [enc expr;enc gen;enc env])
                        AtomLowering.lowerAtom anf atom bound ids sums types inert expr gen env registry variants functions names modules
                    and anf : LoweringCallbacks.ExpressionLowerer = fun _ _ _ _ _ _ _ _ _ _ _ -> Error "observation expression callback"
                    and bound : LoweringCallbacks.BoundAtomLowerer = fun _ _ _ _ _ _ _ _ _ _ _ -> Error "observation bound-atom callback"
                    let value=attempt (fun () -> AtomLowering.lowerAtom anf atom bound ids sums typeNames Set.empty expr (ANF.VarGen gen) env registry variants functions names (Stdlib.buildModuleRegistry ()))
                    tuple [value;list (List.ofSeq requests)]
                (if source="" then [0;Int32.MaxValue] else [0]) |> List.map atGenerator |> list
            environments |> List.map inEnvironment |> list
        let extra=if source<>"" then [] else [CheckedAST.StringLiteral "e\u0301";CheckedAST.CharLiteral "e\u0301";CheckedAST.BigIntLiteral (-(1I <<< 62));CheckedAST.BigIntLiteral (1I <<< 62);CheckedAST.Int128Literal Int128.MinValue;CheckedAST.UInt128Literal UInt128.MaxValue;CheckedAST.UnaryOp (AST.Neg,CheckedAST.Int64Literal Int64.MinValue);CheckedAST.ListLiteral (List.init 32 (fun index -> CheckedAST.Int64Literal (int64 index)));CheckedAST.BinOp (AST.StringConcat,CheckedAST.StringLiteral "",CheckedAST.BinOp (AST.StringConcat,CheckedAST.StringLiteral "a",CheckedAST.StringLiteral ""));CheckedAST.If (CheckedAST.BoolLiteral true,CheckedAST.TupleLiteral (CheckedAST.tupleElementsOfList [CheckedAST.UnitLiteral;CheckedAST.UnitLiteral]),CheckedAST.UnitLiteral)]
        bodies @ extra |> List.map expression |> list
    let sourceProgram source=WrittenParsing.parse LibParser.Validation.Script source |> Result.bind (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit]) |> Result.map (fun (_,value,_) -> value) |> outcome program
    tuple [checkedAstFixtures source |> List.map (outcome program) |> list;sourceProgram source;(if source="" then ["type R = { a: Int64; b: String }\nR { b = \"é\"; a = 1 }";"type S = A of Int64 | B\nS.A 1";"let f = fun (x: Int64) -> x\nf 1";"let (x, y) = (1, 2)\nx + y"] |> List.map sourceProgram |> list else list [])]

let checkedFormatWithDisplay display source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let outcome encoder value =
        match value with
        | Error error -> encode typeof<Result<unit,string>> (box (Error error : Result<unit,string>))
        | Ok value ->
            let node=JsonObject()
            node["type"]<-JsonValue.Create "FSharpResult"
            node["case"]<-JsonValue.Create "Ok"
            node["fields"]<-JsonArray([|encoder value|])
            node :> JsonNode
    let program value = CheckedAST.programTopLevels value |> List.choose (function CheckedAST.FunctionDef value -> Some value.Body | CheckedAST.ValueDef value -> Some value.Body | CheckedAST.Expression value -> Some value | CheckedAST.TypeDef _ -> None) |> List.map (fun value -> if display then value.ToString() else sprintf "%A" value) |> List.map encodeString |> list
    let sourceProgram source = WrittenParsing.parse LibParser.Validation.Script source |> Result.bind (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit]) |> Result.map (fun (_,value,_) -> value) |> outcome program
    let seed=source |> Seq.fold (fun seed value -> (seed ^^^ uint64 value) * 1099511628211UL) 14695981039346656037UL
    let mutable bits=seed
    let values=List.init (if source="" then 10016 else 64) (fun _ ->
        bits <- bits ^^^ (bits <<< 13)
        bits <- bits ^^^ (bits >>> 7)
        bits <- bits ^^^ (bits <<< 17)
        BitConverter.Int64BitsToDouble (int64 bits))
    let values=[0.0;-0.0;nan;infinity;-infinity;1.0;1e-5;1e16;1.2345678901234567;1234567890.5;2.2250738585072014e-308;Double.Epsilon] @ values
    tuple [checkedAstFixtures source |> List.map (outcome program) |> list;sourceProgram source;encode typeof<string list> (box (values |> List.map (sprintf "%A")));
           if source="" then ["let recur (x: Int64) : Int64 = if x == 0 then x else recur (x - 1)\nrecur 1";"let f (x: 'a) : 'a = (fun (y: 'a) -> y) x\nf 1"] |> List.map sourceProgram |> list else list []]

let checkedFormat = checkedFormatWithDisplay false
let checkedDisplay = checkedFormatWithDisplay true

let inlineLambdas source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let outcome encoder value =
        match value with
        | Error error -> encode typeof<Result<unit,string>> (box (Error error : Result<unit,string>))
        | Ok value ->
            let node=JsonObject()
            node["type"]<-JsonValue.Create "FSharpResult"
            node["case"]<-JsonValue.Create "Ok"
            node["fields"]<-JsonArray([|encoder value|])
            node :> JsonNode
    let ids = [AST.bindingId 0; AST.bindingId 1; AST.namedBindingId 0 "x"; AST.namedBindingId 1 "x"; AST.namedBindingId 0 source; AST.topLevelValueId source]
    let lambda = CheckedAST.Lambda (AST.NonEmptyList.singleton {CheckedAST.Pattern=CheckedAST.LPVariable (AST.namedBindingId 0 "x");Type=CheckedAST.checkedType AST.TInt64},Some (CheckedAST.checkedType AST.TInt64),CheckedAST.Local (AST.namedBindingId 0 "x"))
    let environments = [Map.empty;ids |> List.map (fun id -> id,lambda) |> Map.ofList]
    let expression expr = tuple [encode typeof<bool list> (box (ids |> List.map (fun id -> InlineLambdas.varOccursInExpr id expr)));encode typeof<CheckedAST.Expr list> (box (environments |> List.map (InlineLambdas.inlineLambdas expr)))]
    let program program =
        let tops=CheckedAST.programTopLevels program
        let bodies=tops |> List.choose (function CheckedAST.FunctionDef value -> Some value.Body | CheckedAST.ValueDef value -> Some value.Body | CheckedAST.Expression value -> Some value | CheckedAST.TypeDef _ -> None)
        let functions=tops |> List.choose (function CheckedAST.FunctionDef value -> Some value | _ -> None)
        tuple [encode typeof<CheckedAST.Program> (box (InlineLambdas.inlineLambdasInProgram program));bodies |> List.map expression |> list;encode typeof<CheckedAST.FunctionDef list> (box (functions |> List.map InlineLambdas.inlineLambdasInFunc))]
    let sourceProgram source = WrittenParsing.parse LibParser.Validation.Script source |> Result.bind (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit]) |> Result.map (fun (_,value,_) -> value) |> outcome program
    let extra = [lambda;CheckedAST.Let (CheckedAST.LPVariable (AST.namedBindingId 0 "x"),CheckedAST.Local (AST.namedBindingId 0 "x"),lambda);
        CheckedAST.Closure (AST.functionId 9UL,[CheckedAST.TypeApp (AST.functionId 8UL,CheckedAST.checkedTypeArgs [AST.TVar "a"],AST.NonEmptyList.singleton (CheckedAST.Local (AST.namedBindingId 0 "x")))]);
        CheckedAST.Match (CheckedAST.UnitLiteral,AST.NonEmptyList.singleton {CheckedAST.Patterns=AST.NonEmptyList.singleton (CheckedAST.PVariable (AST.namedBindingId 0 "x"));Guard=Some (CheckedAST.Local (AST.namedBindingId 0 "x"));Body=CheckedAST.Local (AST.namedBindingId 0 "x")})]
    tuple [checkedAstFixtures source |> List.map (outcome program) |> list;extra |> List.map expression |> list;sourceProgram source;if source="" then ["let f = fun (x: Int64) -> x\nf 1";"let recur (x: Int64) : Int64 = if x == 0 then x else recur (x - 1)\nrecur 1"] |> List.map sourceProgram |> list else list []]

let typeSubstitutionCall<'a> name args : 'a =
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    unbox (typeof<AST.SemanticType>.Assembly.GetType("TypeSubstitution").GetMethod(name,flags).Invoke(null,args))

let typeSubstitution source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let outcome encoder value =
        match value with
        | Error error -> encode typeof<Result<unit,string>> (box (Error error : Result<unit,string>))
        | Ok value ->
            let node=JsonObject()
            node["type"]<-JsonValue.Create "FSharpResult"
            node["case"]<-JsonValue.Create "Ok"
            node["fields"]<-JsonArray([|encoder value|])
            node :> JsonNode
    let types = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr; AST.TVar "a"; AST.TInferenceVar (source, "fixed"); AST.TRecord ("Alias", []); AST.TRecord ("G", [AST.TVar "a"]); AST.TSum ("Alias", []); AST.TSum ("G", [AST.TVar "a"]); AST.TTuple [AST.TVar "a"; AST.TString]; AST.TList (AST.TVar "a"); AST.TStream (AST.TVar "a"); AST.TDict (AST.TVar "a", AST.TInferenceVar (source,"fixed")); AST.TFunction ([AST.TVar "a"], AST.TInferenceVar (source,"fixed"))]
    let substitutions = [Map.empty; Map.ofList ["a", AST.TInt64; "fixed", AST.TString]; Map.ofList ["a", AST.TVar "fixed"; "fixed", AST.TInt64]]
    let aliases = Map.ofList ["Alias", ([], AST.TList AST.TString); "G", (["a"], AST.TTuple [AST.TVar "a"; AST.TRecord ("Alias", [])])]
    let records : TypeRegistries.TypeRegistry = Map.ofList ["R", {TypeRegistries.TypeParams = ["a"; "phantom"]; Fields = [source, AST.TVar "a"; source, AST.TBool; "alias", AST.TRecord ("Alias", [])]}; "Empty", {TypeRegistries.TypeParams = []; Fields = []}]
    let observeProgram program =
        let tops=CheckedAST.programTopLevels program
        let bodies=tops |> List.choose (function CheckedAST.FunctionDef value -> Some value.Body | CheckedAST.ValueDef value -> Some value.Body | CheckedAST.Expression value -> Some value | CheckedAST.TypeDef _ -> None)
        let functions=tops |> List.choose (function CheckedAST.FunctionDef value -> Some value | _ -> None)
        let specialized func args =
            let value = try Ok (TypeSubstitution.specializeFunction (AST.functionId 9UL) func args) with error -> Error error.Message
            encode typeof<Result<CheckedAST.FunctionDef,string>> (box value)
        tuple [encode typeof<CheckedAST.Program> (box program);
               encode typeof<CheckedAST.Expr list list> (box (substitutions |> List.map (fun subst -> bodies |> List.map (TypeSubstitution.applySubstToExpr subst))));
               functions |> List.map (fun func -> tuple [encode typeof<CheckedAST.FunctionDef> (box (TypeSubstitution.resolveAliasesInFunction aliases func));[[];[AST.TInt64];[AST.TString;AST.TBool]] |> List.map (specialized func) |> list]) |> list]
    let sourceProgram source = WrittenParsing.parse LibParser.Validation.Script source |> Result.bind (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit]) |> Result.map (fun (_,program,_) -> program) |> outcome observeProgram
    let fixtures = if source="" then ["let id (x: 'a) : 'a = x\nid 1"; "type Alias = String\nlet f (x: Alias) : Alias = x\nf \"a\""; "let recur (x: Int64) : Int64 = if x == 0 then x else recur (x - 1)\nrecur 1"; "let f (x: 'a) : 'a = (fun (y: 'a) -> y) x\nf 1"] else []
    let bindings = types |> List.collect (fun value -> [["a", AST.TVar "a"; "a", value]; ["a", value; "a", AST.TVar "a"]; ["a", AST.TInt64; "a", value]; ["a", AST.TStream (AST.TVar "a"); "a", value]; ["a", AST.TInferenceVar (source, "first"); "a", AST.TVar "b"; "z", value]])
    tuple [
        encode typeof<AST.SemanticType list list> (box (substitutions |> List.map (fun subst -> types |> List.map (TypeSubstitution.applySubstToType subst))))
        encode typeof<Result<(string * AST.SemanticType) list,string> list list> (box (types |> List.map (fun pattern -> types |> List.map (TypeSubstitution.matchTypePattern pattern))))
        encode typeof<Result<Map<string,AST.SemanticType>,string> list> (box (bindings |> List.map TypeSubstitution.consolidateTypeBindings))
        encode typeof<AST.SemanticType list> (box (types |> List.map (TypeSubstitution.resolveAliasType aliases)))
        encode typeof<TypeRegistries.TypeRegistry> (box (TypeSubstitution.resolveAliasesInTypeRegistry aliases records))
        records |> Map.map (fun _ info -> tuple [
            encode typeof<(string * AST.SemanticType) list> (box (typeSubstitutionCall<(string * AST.SemanticType) list> "firstDeclaredRecordFields" [|box info.Fields|]))
            [[];[AST.TInt64];[AST.TInt64;AST.TBool]] |> List.map (fun args -> tuple [
                encode typeof<Map<string,AST.SemanticType> option> (box (typeSubstitutionCall<Map<string,AST.SemanticType> option> "buildDeclaredRecordFieldSubst" [|box info;box args|]))
                encode typeof<ANF.RecordDescriptor> (box (typeSubstitutionCall<ANF.RecordDescriptor> "recordDescriptor" [|box source;box args;box info|]))
                encode typeof<Result<ANF.RecordDescriptor,string>> (box (typeSubstitutionCall<Result<ANF.RecordDescriptor,string>> "boxedSumDescriptor" [|box source;box info.TypeParams;box args;box (List.map snd info.Fields)|]))]) |> list]) |> Map.toList |> List.map (fun (name,value) -> tuple [encodeString name;value]) |> List.toArray |> namedArray "map"
        checkedAstFixtures source |> List.map (outcome observeProgram) |> list
        sourceProgram source
        fixtures |> List.map sourceProgram |> list]

let loweringPrimitiveCall<'a> name args : 'a =
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let moduleType = typeof<AST.SemanticType>.Assembly.GetType("LoweringPrimitives")
    unbox (moduleType.GetMethod(name,flags).Invoke(null,args))

let loweringPrimitives source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let calls = ResizeArray<string>()
    let resolve name = calls.Add name; AST.functionId 9UL
    let capture (encodeValue : _ -> JsonNode) work =
        calls.Clear()
        try
            let value = work ()
            tuple [encode typeof<string list> (box (List.ofSeq calls)); encodeValue value |> fun value ->
                let node=JsonObject() in node["type"]<-JsonValue.Create "FSharpResult";node["case"]<-JsonValue.Create "Ok";node["fields"]<-JsonArray([|value|]);node :> JsonNode]
        with error ->
            let rec unwrap (error:exn) = match error with :? Reflection.TargetInvocationException when not (isNull error.InnerException) -> unwrap error.InnerException | _ -> error.Message
            tuple [encode typeof<string list> (box (List.ofSeq calls));encode typeof<Result<string,string>> (box (Error (unwrap error) : Result<string,string>))]
    let types = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr; AST.TVar source; AST.TInferenceVar (source,"fixed"); AST.TRecord ("R", []); AST.TSum ("S", []); AST.TTuple [AST.TInt64; AST.TString]; AST.TList AST.TString; AST.TStream AST.TString; AST.TDict (AST.TString, AST.TInt64); AST.TFunction ([AST.TInt64], AST.TString)]
    let variants = Map.ofList ["One.C", ("One", ["a"], 5, [AST.TVar "a"]); "Null.A", ("Null", ["a"], 2, []); "Null.Z", ("Null", ["a"], 7, [AST.TVar "a"]); "Bad.B", ("Bad", [], 3, [AST.TString; AST.TInt64]); "plain", ("Lost", [], 4, [])]
    let sums = LoweringPrimitives.sumRepresentationIndex variants
    let more = Map.ofList ["One.D", ("One", [], 8, []); "Null.Z", ("Null", [], 0, [AST.TBool])]
    let args = [ []; [ANF.UnitLiteral]; [ANF.StringLiteral source]; [ANF.StringLiteral source; ANF.IntLiteral (ANF.Int64 -1L)]; [ANF.StringLiteral source; ANF.IntLiteral (ANF.Int64 0L); ANF.UnitLiteral]; [ANF.UnitLiteral; ANF.UnitLiteral]; [ANF.StringLiteral source; ANF.UnitLiteral; ANF.UnitLiteral; ANF.UnitLiteral]]
    let names = ["Builtin.crash"; "Builtin.print"; "Builtin.printLine"; "Builtin.stdinReadLine"; "Builtin.testRuntimeError"; "Builtin.unwrap"; "Darklang.Stdlib.Bool.not"; "Darklang.Stdlib.Cli.__argv"; "Darklang.Stdlib.Cli.__cpuCount"; "Darklang.Stdlib.Cli.__createExclusive"; "Darklang.Stdlib.Cli.__environmentPacked"; "Darklang.Stdlib.Cli.__execute"; "Darklang.Stdlib.Cli.__getenv"; "Darklang.Stdlib.Cli.__getpid"; "Darklang.Stdlib.Cli.__getuid"; "Darklang.Stdlib.Cli.__hostArchitectureCode"; "Darklang.Stdlib.Cli.__hostOSCode"; "Darklang.Stdlib.Cli.__hostname"; "Darklang.Stdlib.Cli.__kill"; "Darklang.Stdlib.Cli.__processIO"; "Darklang.Stdlib.Cli.__runProcess"; "Darklang.Stdlib.Cli.__setenv"; "Darklang.Stdlib.Cli.__sleep"; "Darklang.Stdlib.Cli.__spawnProcess"; "Darklang.Stdlib.Cli.__terminateProcess"; "Darklang.Stdlib.Cli.__unsetenv"; "Darklang.Stdlib.Crypto.__secureRandomFill"; "Darklang.Stdlib.DateTime.__fromUnixTimeTicks"; "Darklang.Stdlib.DateTime.__now"; "Darklang.Stdlib.DateTime.__toUnixTimeTicks"; "Darklang.Stdlib.File.appendText"; "Darklang.Stdlib.File.createDirectory"; "Darklang.Stdlib.File.currentDirectory"; "Darklang.Stdlib.File.delete"; "Darklang.Stdlib.File.exists"; "Darklang.Stdlib.File.isDirectory"; "Darklang.Stdlib.File.listDirectoryPacked"; "Darklang.Stdlib.File.readBlob"; "Darklang.Stdlib.File.setExecutable"; "Darklang.Stdlib.File.writeBlob"; "Darklang.Stdlib.File.writeFromPtr"; "Darklang.Stdlib.Float.__toBits"; "Darklang.Stdlib.Float.__toInt64Unchecked"; "Darklang.Stdlib.Float.negate"; "Darklang.Stdlib.Float.sqrt"; "Darklang.Stdlib.Int.__equals"; "Darklang.Stdlib.Int.__randomInt64Word"; "Darklang.Stdlib.Int128.__equalsWords"; "Darklang.Stdlib.Int128.__fromInt"; "Darklang.Stdlib.Int128.__fromWords"; "Darklang.Stdlib.Int128.__toInt"; "Darklang.Stdlib.Int16"; "Darklang.Stdlib.Int32"; "Darklang.Stdlib.Int64"; "Darklang.Stdlib.Int64.toFloat"; "Darklang.Stdlib.Int8"; "Darklang.Stdlib.Network.__close"; "Darklang.Stdlib.Network.__connect4"; "Darklang.Stdlib.Network.__connect6"; "Darklang.Stdlib.Network.__receive"; "Darklang.Stdlib.Network.__receiveTimeout"; "Darklang.Stdlib.Network.__send"; "Darklang.Stdlib.Network.__sendTimeout"; "Darklang.Stdlib.Network.__tcp4Socket"; "Darklang.Stdlib.Network.__tcp6Socket"; "Darklang.Stdlib.Network.__udp4Socket"; "Darklang.Stdlib.Network.__udp6Socket"; "Darklang.Stdlib.UInt128.__equalsWords"; "Darklang.Stdlib.UInt128.__fromInt"; "Darklang.Stdlib.UInt128.__fromWords"; "Darklang.Stdlib.UInt128.__toInt"; "Darklang.Stdlib.UInt16"; "Darklang.Stdlib.UInt32"; "Darklang.Stdlib.UInt64"; "Darklang.Stdlib.UInt8"; "__blob_to_rawptr"; "__dark_internal_eq_helper_dispatch"; "__dict_get_tag"; "__dict_is_null"; "__dict_to_rawptr"; "__empty_dict"; "__int128_to_int"; "__int128_to_rawptr"; "__int64_to_int16"; "__int64_to_int32"; "__int64_to_int8"; "__int64_to_uint16"; "__int64_to_uint32"; "__int64_to_uint64_bits"; "__int64_to_uint8"; "__int_to_int128"; "__int_to_rawptr"; "__int_to_uint128"; "__int_to_word"; "__list_array_release_small"; "__list_empty"; "__list_get_tag"; "__list_is_null"; "__list_to_rawptr"; "__mapped_alloc"; "__mapped_free"; "__raw_alloc"; "__raw_free"; "__raw_get"; "__raw_get_byte"; "__raw_slot_init"; "__raw_slot_init requires a concrete slot type"; "__raw_take"; "__raw_write_byte"; "__raw_write_word"; "__rawptr_to_blob"; "__rawptr_to_dict"; "__rawptr_to_int"; "__rawptr_to_int128"; "__rawptr_to_list"; "__rawptr_to_stream"; "__rawptr_to_string"; "__rawptr_to_uint128"; "__refcount_dec_string"; "__refcount_inc_string"; "__stream_to_rawptr"; "__string_concat_raw"; "__string_to_rawptr"; "__uint128_to_int"; "__uint128_to_rawptr"; "__uint16_to_int64"; "__uint32_to_int64"; "__uint64_to_int64_bits"; "__uint8_to_int64"; "__word_to_int"; "missing"; "Darklang.Stdlib.Int64.shiftLeft"; "Darklang.Stdlib.UInt64.bitwiseNot"; "__raw_get_byte_i64"; "__raw_get_str"; "__raw_take_str"; "__raw_slot_init_str"; "__raw_slot_init_fn_i64_to_str"; "__rawptr_to_dict_str_i64"; "__rawptr_to_list_str"; "__rawptr_to_stream_str"; "__raw_get_bad_type"] @ [source]
    let intrinsic name args = capture (fun value -> encode typeof<ANF.CExpr option list> (box value)) (fun () ->
        let file=LoweringPrimitives.tryFileIntrinsic name args
        let cli=LoweringPrimitives.tryCliIntrinsic name args
        let presentation=LoweringPrimitives.tryPresentationIntrinsic name args
        let float=LoweringPrimitives.tryFloatIntrinsic name args
        let canonical=LoweringPrimitives.tryCanonicalPrimitiveIntrinsic name args
        let raw=LoweringPrimitives.tryRawMemoryIntrinsic resolve (Set.singleton "S") name args
        let random=LoweringPrimitives.tryRandomIntrinsic name args
        let date=LoweringPrimitives.tryDateTimeIntrinsic name args
        [file;cli;presentation;float;canonical;raw;random;date])
    let sourceToken = if source.Length <= 64 && source.Split('_').Length <= 5 then source else "source"
    let mangled = sourceToken :: [""; "i64"; "runtime_error"; "rawptr"; "R"; "S"; "S_i64"; "R_i64_str"; "a"; "λ"; "𐐨"; "ᲊ"; "tup"; "tup0"; "tup2_i64_str"; "tup_i64_str"; "tup3_i64"; "fn_i64_to_str"; "fn_to_str"; "fn_i64_to_fn_str_to_bool"; "dict_str_list_i64"; "stream_R_i64"; "a$b"; "R__i64"; "tup2147483648_i64"; "tup+1_i64"]
    let integers = [-(1I <<< 127); -1I; 0I; 1I; (1I <<< 127)-1I; (1I <<< 128)-1I]
    let patterns = [CheckedAST.PInt64 -1L; CheckedAST.PInt8Literal -128y; CheckedAST.PInt16Literal -32768s; CheckedAST.PInt32Literal Int32.MinValue; CheckedAST.PUInt8Literal 255uy; CheckedAST.PUInt16Literal 65535us; CheckedAST.PUInt32Literal UInt32.MaxValue; CheckedAST.PUInt64Literal UInt64.MaxValue; CheckedAST.PWildcard; CheckedAST.PBool true; CheckedAST.PString source]
    let expressions = [CheckedAST.UnitLiteral; CheckedAST.Int64Literal -1L; CheckedAST.Int128Literal Int128.MinValue; CheckedAST.UInt128Literal UInt128.MaxValue; CheckedAST.Int8Literal -128y; CheckedAST.Int16Literal -32768s; CheckedAST.Int32Literal Int32.MinValue; CheckedAST.UInt8Literal 255uy; CheckedAST.UInt16Literal 65535us; CheckedAST.UInt32Literal UInt32.MaxValue; CheckedAST.UInt64Literal UInt64.MaxValue; CheckedAST.BoolLiteral true; CheckedAST.BoolLiteral false; CheckedAST.FloatLiteral -0.0; CheckedAST.FloatLiteral infinity; CheckedAST.StringLiteral source; CheckedAST.CharLiteral source; CheckedAST.RuntimeError source]
    let constructor,symbols = CheckedAST.internConstructor "Null" "Z" 7 (CheckedAST.emptySymbols())
    let owner = AST.constructorIdOwner constructor
    let reference : CheckedAST.ConstructorReference = {TypeId=owner;ConstructorId=constructor;TypeArgs=[]}
    let typeNames = CheckedAST.semanticMetadata symbols
    let variant = encode typeof<(string * string list * int * AST.SemanticType list) option>
    tuple [
        encodeString "__dark_internal_eq_helper_dispatch"
        types |> List.map (fun typ -> encode typeof<AST.SemanticType * string * MemoryModel.CanonicalBufferKind option * bool * bool> (box (typ,LoweringPrimitives.typeToString typ,loweringPrimitiveCall<MemoryModel.CanonicalBufferKind option> "canonicalBufferKindForType" [|box typ|],MemoryPlanning.canUseTransparentSumPayload typ,loweringPrimitiveCall<bool> "canUseNullaryZeroForPayload" [|box typ|]))) |> list
        encode typeof<LoweringPrimitives.SumRepresentationIndex> (box sums)
        encode typeof<LoweringPrimitives.SumMetadata> (box (LoweringPrimitives.mergeSumMetadata (LoweringPrimitives.sumMetadataFromVariantLookup variants) (LoweringPrimitives.sumMetadataFromVariantLookup more)))
        types |> List.map (fun typ -> ["One";"Null";"Bad";"Missing"] |> List.map (fun owner -> capture (fun value -> encode typeof<AST.SemanticType option * AST.SemanticType option * int64 option * ANF.CExpr> (box value)) (fun () ->
            loweringPrimitiveCall<AST.SemanticType option> "transparentSumPayloadType" [|box owner;box [typ];box sums|],loweringPrimitiveCall<AST.SemanticType option> "nullablePointerSumPayloadType" [|box owner;box [typ];box sums|],loweringPrimitiveCall<int64 option> "spareImmediateSumSentinel" [|box owner;box [typ];box sums|],loweringPrimitiveCall<ANF.CExpr> "sumPayloadExpr" [|box (AST.TSum (owner,[typ]));box (ANF.StringLiteral source);box sums|])) |> list) |> list
        names |> List.map (fun name -> tuple [encodeString name;args |> List.map (intrinsic name) |> list;encode typeof<bool> (box (LoweringPrimitives.isBuiltinUnwrapName name));encode typeof<bool> (box (LoweringPrimitives.isRuntimeFailureName name));encode typeof<bool> (box (LoweringPrimitives.isSourceCrashName name));encode typeof<bool> (box (LoweringPrimitives.isBuiltinTestRuntimeErrorName name))]) |> list
        encode typeof<(string * Result<AST.SemanticType,string>) list> (box (mangled |> List.map (fun value -> value,LoweringPrimitives.tryParseMangledType variants value)))
        integers |> List.map (fun value -> capture (fun value -> encode typeof<ANF.CExpr list> (box value)) (fun () ->
            let unsigned = (value + (1I <<< 128)) % (1I <<< 128)
            let signed = if unsigned >= (1I <<< 127) then unsigned-(1I <<< 128) else unsigned
            let signed=Int128.Parse (string signed)
            let unsigned=UInt128.Parse (string unsigned)
            let a=loweringPrimitiveCall<ANF.CExpr> "int128Construction" [|box resolve;box signed|]
            let b=loweringPrimitiveCall<ANF.CExpr> "uint128Construction" [|box resolve;box unsigned|]
            let c=loweringPrimitiveCall<ANF.CExpr> "int128LiteralComparison" [|box resolve;box (ANF.StringLiteral source);box signed|]
            let d=loweringPrimitiveCall<ANF.CExpr> "uint128LiteralComparison" [|box resolve;box (ANF.StringLiteral source);box unsigned|]
            [a;b;c;d])) |> list
        encode typeof<ANF.SizedInt option list> (box (patterns |> List.map LoweringPrimitives.patternLiteralToSizedInt))
        encode typeof<string option list> (box (expressions |> List.map (fun expression -> loweringPrimitiveCall<string option> "unwrapErrorPayloadToString" [|box expression|])))
        [[];[CheckedAST.StringLiteral source];[CheckedAST.StringLiteral source;CheckedAST.UnitLiteral]] |> List.map (fun args -> types |> List.map (fun typ -> capture (fun value -> encode typeof<CheckedAST.Expr> (box value)) (fun () -> loweringPrimitiveCall<CheckedAST.Expr> "materializeComparisonPlan" [|box resolve;box typ;box args|])) |> list) |> list
        [[];[CheckedAST.UnitLiteral];[CheckedAST.StringLiteral source;CheckedAST.UnitLiteral]] |> List.map (fun args -> capture (fun value -> encode typeof<CheckedAST.Expr> (box value)) (fun () -> loweringPrimitiveCall<CheckedAST.Expr> "materializeFunctionComparisonPlan" [|box (AST.bindingId 0);box (AST.bindingId 1);box args|])) |> list
        tuple [encode typeof<string option> (box (loweringPrimitiveCall<string option> "tryFindRecordTypeNameById" [|box owner;box typeNames|]));encode typeof<string option> (box (loweringPrimitiveCall<string option> "tryFindSumTypeNameById" [|box owner;box typeNames|]));
               variant (box (loweringPrimitiveCall<(string * string list * int * AST.SemanticType list) option> "tryFindVariantForType" [|box "Z";box (AST.TSum ("Null",[]));box variants|]));
               variant (box (loweringPrimitiveCall<(string * string list * int * AST.SemanticType list) option> "tryFindVariantByTag" [|box "Null";box 7;box sums|]));
               variant (box (loweringPrimitiveCall<(string * string list * int * AST.SemanticType list) option> "tryFindVariantByConstructorId" [|box owner;box "Null";box constructor;box variants|]));
               variant (box (loweringPrimitiveCall<(string * string list * int * AST.SemanticType list) option> "tryFindVariantForTypeById" [|box constructor;box (AST.TSum ("Null",[]));box typeNames;box variants|]));
               encode typeof<bool> (box (loweringPrimitiveCall<bool> "constructorReferenceMatches" [|box "Null";box "Null.Z";box reference;box typeNames;box variants|]))]]

let memoryPlanning source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let records = Map.ofList ["R",[source,AST.TString;"next",AST.TRecord ("R",[])];"G",["value",AST.TVar "a";"next",AST.TList (AST.TRecord ("G",[AST.TVar "a"]))]]
    let parameters = Map.ofList ["R",[];"G",["a"]]
    let info parameters payloads unary : MemoryModel.RcSumShapeInfo = {TypeParams=parameters;Payloads=payloads;UnaryPayloadTags=Set.ofList unary}
    let sums = Map.ofList ["Empty",info [] [] [];"Null",info ["a"] [4,Some (AST.TVar "a");3,None] [4];"One",info ["a"] [7,Some (AST.TVar "a")] [7];"Many",info [] [5,Some (AST.TTuple [AST.TString;AST.TSum ("Many",[])]);2,None;8,Some AST.TInt128] [8];"Binary",info [] [2,Some (AST.TTuple [AST.TString;AST.TInt64])] [];"Unknown",info [] [0,Some (AST.TVar "unresolved")] [0]]
    let primitives = [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TDateTime;AST.TUnit;AST.TNever;AST.TVar source;AST.TInferenceVar (source,"id");AST.TString;AST.TChar;AST.TBlob;AST.TInternalRawPtr;AST.TFunction ([AST.TString],AST.TInt64);AST.TList AST.TString;AST.TStream AST.TString;AST.TDict (AST.TString,AST.TInt128);AST.TTuple [AST.TString;AST.TInt64];AST.TRecord ("R",[])]
    let sumTypes = (primitives |> List.collect (fun typ -> [AST.TSum ("Null",[typ]);AST.TSum ("One",[typ])])) @ [AST.TSum ("Empty",[]);AST.TSum ("Many",[]);AST.TSum ("Binary",[]);AST.TSum ("Unknown",[]);AST.TRecord ("Many",[]);AST.TSum ("R",[]);AST.TRecord ("G",[AST.TString])]
    let basic = primitives @ [AST.TSum ("X",[]);AST.TSum ("X",[AST.TString]);AST.TSum ("X",[AST.TString;AST.TInt64])]
    let simpleShapes = [MemoryModel.Immediate;MemoryModel.StaticString;MemoryModel.RawUnmanaged;MemoryModel.DynamicString;MemoryModel.DynamicBlob;MemoryModel.DynamicInt;MemoryModel.StreamRoot;MemoryModel.FixedBlock (32,[MemoryModel.Immediate;MemoryModel.DynamicString;MemoryModel.RecursiveNominalRef (AST.TRecord (source,[]));MemoryModel.ClosureShape []]);MemoryModel.BoxedSum (16,[8,MemoryModel.DynamicString;8,MemoryModel.RecursiveNominalRef (AST.TSum (source,[]))],[{MemoryModel.Tag=3;FieldShapes=[8,MemoryModel.RecursiveNominalRef (AST.TSum ("OnlyVariant",[]))]}]);MemoryModel.TaggedListShape (MemoryModel.RecursiveNominalRef (AST.TRecord (source,[])));MemoryModel.DictRoot (MemoryModel.DynamicString,MemoryModel.FixedBlock (8,[MemoryModel.DynamicBlob]));MemoryModel.ClosureShape [MemoryModel.DynamicString;MemoryModel.RecursiveNominalRef (AST.TRecord (source,[]))];MemoryModel.RecursiveNominalRef (AST.TSum (source,[]))]
    let facts value =
        let release = MemoryPlanning.rcShapeReleasePlan value
        tuple [encode typeof<MemoryModel.RcShape> (box value);encode typeof<bool> (box (MemoryPlanning.rcShapeNeedsOwnedScopeRelease value));encode typeof<bool> (box (MemoryPlanning.rcShapeIsRootManaged value));encode typeof<bool> (box (MemoryPlanning.rcShapeNeedsRecursiveRelease value));
               encode typeof<MemoryModel.RcKind option> (box (MemoryPlanning.rcShapeRootKind value));encode typeof<int option> (box (MemoryPlanning.rcShapePayloadSize value));encode typeof<MemoryModel.RcStorageClass> (box (MemoryPlanning.rcShapeStorageClass value));encode typeof<bool> (box (MemoryPlanning.rcShapeIsOwnershipTransferRoot value));encode typeof<MemoryModel.RcOperation option> (box (MemoryPlanning.rcShapeRetainOperation value));encode typeof<MemoryModel.RcOperation option> (box (MemoryPlanning.rcShapeReleaseOperation value));encode typeof<bool> (box (MemoryPlanning.rcShapeNeedsBorrowedRetain value));encode typeof<bool> (box (MemoryPlanning.rcShapeNeedsAutomaticBindingDec value));encode typeof<bool> (box (MemoryPlanning.rcShapeNeedsManagedAliasRootPreservation value));encode typeof<MemoryModel.RcReleasePlan> (box release);encode typeof<Set<AST.SemanticType>> (box (MemoryPlanning.recursiveReleaseTypes release))]
    tuple [
        basic |> List.map (fun typ -> tuple [encode typeof<AST.SemanticType> (box typ);encode typeof<bool> (box (MemoryPlanning.canUseTransparentSumPayload typ));facts (MemoryPlanning.rcShapeOfType records typ);encode typeof<MemoryModel.RcReleasePlan> (box (MemoryPlanning.rcReleasePlanOfType records typ))]) |> List.toArray |> fun values -> JsonArray(values) :> JsonNode
        (primitives @ sumTypes) |> List.map (fun typ -> tuple [encode typeof<AST.SemanticType> (box typ);encode typeof<AST.SemanticType option> (box (MemoryPlanning.nullablePointerSumPayloadType sums typ));encode typeof<bool> (box (MemoryPlanning.isNullablePointerSumType sums typ));encode typeof<bool> (box (MemoryPlanning.isSpareImmediateSumType sums typ));facts (MemoryPlanning.rcShapeOfTypeWithSums records parameters sums typ);encode typeof<MemoryModel.RcReleasePlan> (box (MemoryPlanning.rcReleasePlanOfTypeWithSums records sums typ))]) |> List.toArray |> fun values -> JsonArray(values) :> JsonNode
        simpleShapes |> List.map facts |> List.toArray |> fun values -> JsonArray(values) :> JsonNode
        encode typeof<Map<string,string list>> (box (MemoryPlanning.inferredRecordTypeParamsRegistry records))]

let preparationRegistries source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let records : TypeRegistries.TypeRegistry = Map.ofList ["R",{TypeRegistries.TypeParams=[];Fields=[source,AST.TInt64;"second",AST.TRecord ("Number",[])]};"Generic",{TypeRegistries.TypeParams=["a"];Fields=["value",AST.TVar "a"]}]
    let aliases : TypeRegistries.AliasRegistry = Map.ofList ["Alias",([],AST.TRecord ("R",[]));"Chain",([],AST.TRecord ("Alias",[]));"SumAlias",([],AST.TSum ("S",[]));"Number",([],AST.TInt64);"Unresolved",([],AST.TRecord ("Missing",[]));"Parameterized",(["a"],AST.TRecord ("Generic",[AST.TVar "a"]))]
    let variants = Map.ofList ["S.A",("S",["a"],2,[AST.TVar "a"]);"S.B",("S",["later"],1,[]);"S.C",("S",[],2,[AST.TRecord ("S",[]);AST.TStream (AST.TRecord ("S",[]))]);"A",("S",[],9,[]);"Other.A",("Other",[],0,[AST.TString]);"wrong",("Lost",[],3,[])]
    let types = [AST.TInt64;AST.TString;AST.TFloat64;AST.TVar source;AST.TRecord ("S",[]);AST.TRecord ("R",[]);AST.TSum ("R",[AST.TRecord ("S",[])]);AST.TRecord ("Generic",[AST.TRecord ("S",[])]);AST.TList (AST.TRecord ("S",[]));AST.TStream (AST.TRecord ("S",[]));AST.TDict (AST.TRecord ("S",[]),AST.TSum ("R",[]));AST.TFunction ([AST.TRecord ("S",[])],AST.TSum ("R",[]));AST.TTuple [AST.TString;AST.TInt64]]
    let names = FunctionIdMap.ofList [AST.functionId 0UL,source;AST.functionId 9223372036854775808UL,"middle";AST.functionId UInt64.MaxValue,source;AST.functionId 7UL,"other"]
    let variables : TypeRegistries.VarEnv = Map.ofList [AST.bindingId 0,(ANF.TempId 3,AST.TInt64);AST.bindingId 1,(ANF.TempId 4,AST.TVar source);AST.bindingId 0,(ANF.TempId 5,AST.TString)]
    let constructor,symbols = CheckedAST.internConstructor "S" "A" 2 (CheckedAST.emptySymbols())
    let field,symbols = CheckedAST.internField "R" source 3 symbols
    let metadata = [TypeRegistries.emptyTypeNames;TypeRegistries.typeNamesFromSymbols symbols]
    let helperNames = ["Darklang.Stdlib.List.__headUnsafe_i64";"Darklang.Stdlib.List.__headUnsafeFloat";"Darklang.Stdlib.Json.__viewListHead";"Darklang.Stdlib.Json.__viewFieldListHead"]
    let helpers = helperNames |> List.mapi (fun index name -> name,AST.functionId (uint64 index))
    let helperMaps = [Map.ofList helpers;helpers |> List.filter (fun (name,_) -> not (name.StartsWith "Darklang.Stdlib.Json.")) |> Map.ofList]
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let call name args = let method = typeof<AST.SemanticType>.Assembly.GetType("TypeRegistries").GetMethod(name,flags) in method.Invoke(null,args)
    let canonical value = unbox<AST.SemanticType> value
    tuple [
        encode typeof<Map<string,(string * AST.SemanticType) list>> (box (TypeRegistries.recordFieldsRegistry records))
        encode typeof<Map<string,string list>> (box (TypeRegistries.recordTypeParamsRegistry records))
        encode typeof<MemoryModel.RcSumShapeRegistry> (box (TypeRegistries.rcSumShapeRegistryFromVariantLookup variants))
        encode typeof<TypeRegistries.FunctionIdRegistry> (box (TypeRegistries.functionIdsFromNames names))
        encode typeof<(CheckedAST.SemanticMetadata * int option * int option) list> (box (metadata |> List.map (fun metadata -> metadata,TypeRegistries.tryFindConstructorTag constructor metadata,TypeRegistries.tryFindFieldIndex field metadata)))
        encode typeof<ANF.CExpr list list> (box (helperMaps |> List.map (fun helpers -> types |> List.map (fun typ -> unbox<ANF.CExpr> (call "listHeadUnsafeExpr" [|box helpers;box typ;box (ANF.StringLiteral source)|])))))
        encode typeof<(AST.SemanticType * AST.SemanticType * AST.SemanticType) list> (box (types |> List.map (fun typ -> canonical (call "canonicalizeBareSumTypeRefsWithNames" [|box (Set.singleton "S");box typ|]),canonical (call "canonicalizeBareSumTypeRefs" [|box variants;box typ|]),canonical (call "canonicalizeNamedTypeRefs" [|box (Set.singleton "R");box (Set.singleton "S");box typ|]))))
        encode typeof<string list> (box (["Alias";"Chain";"SumAlias";"Number";source] |> List.map (TypeRegistries.resolveRecordTypeName aliases)))
        encode typeof<TypeRegistries.TypeRegistry> (box (TypeRegistries.expandTypeRegWithAliases records aliases))
        encode typeof<Map<AST.BindingId,AST.SemanticType>> (box (TypeRegistries.typeEnvFromVarEnv variables))]

let anfObservation source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let integers = [ANF.Int8 -128y; ANF.Int16 -32768s; ANF.Int32 Int32.MinValue; ANF.Int64 Int64.MinValue; ANF.UInt8 255uy; ANF.UInt16 65535us; ANF.UInt32 UInt32.MaxValue; ANF.UInt64 UInt64.MaxValue]
    let atoms = [ANF.UnitLiteral; ANF.IntLiteral (ANF.UInt64 UInt64.MaxValue); ANF.BoolLiteral true; ANF.StringLiteral source; ANF.FloatLiteral -0.0; ANF.Var (ANF.TempId 3); ANF.FuncRef (AST.functionId UInt64.MaxValue)]
    let tables = [[]; [0, AST.TUnit]; [3, AST.TString; 1, AST.TInt64; 3, AST.TBool; 5, AST.TList AST.TString]; [2, AST.TVar source]; [Int32.MaxValue, AST.TUnit]] |> List.map (fun entries -> entries |> List.map (fun (id,typ) -> ANF.TempId id,typ) |> ANF.TypeMap.ofSeq)
    let ids = [Int32.MinValue; -1; 0; 1; 2; 3; 4; 5; 6; Int32.MaxValue - 1; Int32.MaxValue]
    let table value = value, ids |> List.map (fun id -> ANF.TypeMap.tryFind (ANF.TempId id) value)
    let generators = ids |> List.map (fun id ->
        let temp,generator = ANF.freshVar (ANF.VarGen id)
        let expr,exprGenerator = ANF.freshExprId (ANF.ExprIdGen id)
        temp,generator,expr,exprGenerator)
    let coverage = [3;1;3;-1;Int32.MaxValue] |> List.fold (fun mapping id -> ANF.addCoverageEntry id source mapping) ANF.emptyCoverageMapping
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let method = typeof<AST.SemanticType>.Assembly.GetType("SpecializationIdentity").GetMethod("normalizeSyntheticNullaryArgAtoms",flags)
    let normalized = [[];[AST.TUnit];[AST.TInt64]] |> List.map (fun parameters ->
        [[];[CheckedAST.UnitLiteral];[CheckedAST.StringLiteral source]] |> List.map (fun expressions ->
            ([] :: (atoms |> List.map (fun atom -> [atom])) @ [atoms]) |> List.map (fun atoms -> unbox<ANF.Atom list> (method.Invoke(null,[|box parameters;box expressions;box atoms|])))))
    tuple [encode typeof<(ANF.SizedInt * int64 * string * AST.SemanticType) list> (box (integers |> List.map (fun value -> value,ANF.sizedIntToInt64 value,ANF.sizedIntToString value,ANF.sizedIntToType value)));
           encode typeof<ANF.Atom list> (box atoms); encode typeof<(ANF.TypeMap * AST.SemanticType option list) list> (box (tables |> List.map table));
           encode typeof<(ANF.TypeMap * AST.SemanticType option list) list list> (box (tables |> List.map (fun earlier -> tables |> List.map (fun later -> table (ANF.TypeMap.merge earlier later)))));
           encode typeof<(ANF.TempId * ANF.VarGen * int * ANF.ExprIdGen) list> (box generators); encode typeof<ANF.CoverageMapping> (box coverage); encode typeof<ANF.Atom list list list list> (box normalized); irFixtures source]

let checkedPreparation source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let types = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr; AST.TVar source; AST.TInferenceVar (source, "id"); AST.TRecord (source, []); AST.TRecord ("R", [AST.TVar source]); AST.TSum ("S", [AST.TInt64]); AST.TTuple [AST.TString; AST.TVar source]; AST.TList (AST.TVar source); AST.TStream (AST.TVar source); AST.TDict (AST.TString, AST.TVar source); AST.TFunction ([AST.TUnit; AST.TVar source], AST.TInt64)]
    let mangled = types |> List.map (fun typ -> SpecializationIdentity.typeToMangledName typ, SpecializationIdentity.containsTypeVar typ, SpecializationIdentity.specName source [typ; AST.TList typ])
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let normalize = typeof<AST.SemanticType>.Assembly.GetType("SpecializationIdentity").GetMethod("normalizeSyntheticNullaryParams",flags)
    let ok payload =
        let node = JsonObject()
        node["type"] <- JsonValue.Create "FSharpResult"
        node["case"] <- JsonValue.Create "Ok"
        node["fields"] <- JsonArray([|payload|])
        node :> JsonNode
    let perSource text =
        match WrittenParsing.parse LibParser.Validation.Script text |> Result.bind (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit]) with
        | Error error -> encode typeof<Result<unit,string>> (box (Error error : Result<unit,string>))
        | Ok (_,program,_) ->
            let symbols = CheckedAST.programSymbols program
            let topLevels = CheckedAST.programTopLevels program
            let env = WrittenChecking.typeCheckEnvironment program
            let generic = SpecializationIdentity.extractGenericFuncDefs program
            let values = generic |> Map.toList |> List.map snd
            let imported target = encode typeof<CheckedAST.Symbols * CheckedAST.FunctionDef list> (box (SpecializationIdentity.importSpecializedFunctions target values))
            let materialize indexed =
                let result = if indexed then CheckedMaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums symbols env.AliasReg env.IndexedTypeReg env.VariantLookup env.IndexedSumTypeReg topLevels
                             else CheckedMaterializeHelpers.materializeEqHelpersInTopLevels symbols env.AliasReg env.IndexedTypeReg env.VariantLookup topLevels
                encode typeof<CheckedAST.Symbols * CheckedAST.TopLevel list> (box result)
            let bodies = topLevels |> List.choose (function CheckedAST.FunctionDef f -> Some f.Body | CheckedAST.ValueDef value -> Some value.Body | CheckedAST.Expression expr -> Some expr | CheckedAST.TypeDef _ -> None)
            let normalized = topLevels |> List.choose (function CheckedAST.FunctionDef f -> Some f | _ -> None) |> List.map (fun f -> normalize.Invoke(null,[|box symbols; box (CheckedAST.functionParameterTypes f |> AST.NonEmptyList.toList)|]))
            let normalized = normalized |> List.map (encode normalize.ReturnType) |> List.toArray
            ok (tuple [encode typeof<CheckedAST.Program> (box program); encode typeof<SpecializationIdentity.GenericFuncDefs> (box generic); imported symbols; imported (CheckedAST.emptySymbols ());
                       encode typeof<Set<AST.FunctionId> list> (box (bodies |> List.map SpecializationIdentity.directDependencies));
                       JsonArray([false;true] |> List.map materialize |> List.toArray) :> JsonNode; JsonArray(normalized) :> JsonNode])
    let fixtures = if source <> "" then [] else ["let id (x: 'a) : 'a = x\nid 1"; "let eq (x: 'a) (y: 'a) : Bool = x == y\neq [1] [2]"; "[1] == [2]"; "(1, true) == (2, false)"; "type R = { x: Int64 }\nR { x = 1 } == R { x = 2 }"; "type S = A of Int64 | B\nS.A 1 == S.B"; "let recurse (x: Int64) : Int64 = if x == 0 then 0 else recurse (x - 1)\nrecurse 3"; "(fun (a, b) -> a + b) (1, 2)"]
    tuple [encode typeof<(string * bool * string) list> (box mangled); perSource source; JsonArray(fixtures |> List.map perSource |> List.toArray) :> JsonNode]

let writtenChecking source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let fullType = typeof<Result<AST.SemanticType * CheckedAST.Program * WrittenChecking.Environment,string>>
    let programType = typeof<Result<AST.SemanticType * CheckedAST.Program,string>>
    let full value = encode fullType (box value)
    let program value = encode programType (box value)
    let parse = WrittenParsing.parse LibParser.Validation.Script
    let baseText = "type R = { field: Int64 }\nlet id (x: 'a) : 'a = x\nval value = 1\n"
    let baseResult = parse baseText |> Result.bind (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit])
    let perSource text =
        match parse text with
        | Error error -> encode typeof<Result<unit,string>> (box (Error error : Result<unit,string>))
        | Ok validated ->
            let cases = [false,false;false,true;true,false;true,true]
            let baseUse =
                match baseResult with
                | Error error -> encode typeof<Result<unit,string>> (box (Error error : Result<unit,string>))
                | Ok (_,_,env) ->
                    let values = cases |> List.map (fun (internal_,require) -> full (WrittenChecking.checkSourceUnitsWithBase (Some env) internal_ require [validated])) |> List.toArray
                    let payload = JsonArray(values) :> JsonNode
                    let node = JsonObject()
                    node["type"] <- JsonValue.Create "FSharpResult"
                    node["case"] <- JsonValue.Create "Ok"
                    node["fields"] <- JsonArray([|payload|])
                    node :> JsonNode
            let result = tuple [program (WrittenChecking.checkClosedProgram validated);
                                JsonArray([false;true] |> List.map (fun require -> program (WrittenChecking.checkSimpleProgram require validated)) |> List.toArray) :> JsonNode;
                                JsonArray(cases |> List.map (fun (internal_,require) -> full (WrittenChecking.checkSourceUnitsWithBase None internal_ require [validated])) |> List.toArray) :> JsonNode;
                                baseUse;
                                encode typeof<Result<CheckingTypes.TypeCheckEnv,string>> (box (WrittenChecking.checkSourceUnitsWithBase None false false [validated] |> Result.map (fun (_,program,_) -> WrittenChecking.typeCheckEnvironment program)))]
            let node = JsonObject()
            node["type"] <- JsonValue.Create "FSharpResult"
            node["case"] <- JsonValue.Create "Ok"
            node["fields"] <- JsonArray([|result|])
            node :> JsonNode
    let fixtures = if source <> "" then [] else ["()"; "1"; "true"; "fun x -> x"; "(fun x -> x) 1"; "let x = 1 in x + 2"; "if true then 1 else 2"; "[1; 2]"; "Dict { \"x\" = 1; \"x\" = 2 }"; "(1, true)"; "match true with | true -> 1 | false -> 2"; "type R = { field: Int64 }\nR { field = 1 }"; "type S = C of Int64 | D\nS.C 1"; "type Box<'a> = { value: 'a }\nBox { value = 1 }"; "let id (x: 'a) : 'a = x\nid 1"; "let fact (n: Int64) : Int64 = if n == 0 then 1 else n * fact (n - 1)\nfact 5"; "val x = 1\nval y = x\ny"; "Builtin.boolNot true"; "Builtin.bitwiseNot 1uy"; "Builtin.testNan"; "1 ++ 'a'"; "let x = [1] in x"; "fun __x -> __x"; "id 1"; "R { field = value }"]
    tuple [full baseResult; perSource source; JsonArray(fixtures |> List.map perSource |> List.toArray) :> JsonNode]

let writtenTypes source =
    let r = LibParser.WrittenTypes.synthRange
    let custom modules name args = LibParser.WrittenTypes.TCustom {range=r;modules=modules |> List.map (fun name -> ({range=r;name=name}:LibParser.WrittenTypes.Identifier),r);typ={range=r;name=name};typeArgs=args}
    let references = [LibParser.WrittenTypes.TUnit r;LibParser.WrittenTypes.TBool r;LibParser.WrittenTypes.TInt r;LibParser.WrittenTypes.TInt8 r;LibParser.WrittenTypes.TUInt8 r;LibParser.WrittenTypes.TInt16 r;LibParser.WrittenTypes.TUInt16 r;LibParser.WrittenTypes.TInt32 r;LibParser.WrittenTypes.TUInt32 r;LibParser.WrittenTypes.TInt64 r;LibParser.WrittenTypes.TUInt64 r;LibParser.WrittenTypes.TInt128 r;LibParser.WrittenTypes.TUInt128 r;LibParser.WrittenTypes.TFloat r;LibParser.WrittenTypes.TChar r;LibParser.WrittenTypes.TString r;LibParser.WrittenTypes.TDateTime r;LibParser.WrittenTypes.TUuid r;LibParser.WrittenTypes.TBlob r;
        LibParser.WrittenTypes.TVariable (r,r,(r,source));LibParser.WrittenTypes.TVariable (r,r,(r,"a"));LibParser.WrittenTypes.TList (r,r,r,LibParser.WrittenTypes.TVariable (r,r,(r,"a")),r);
        LibParser.WrittenTypes.TDict (r,r,r,LibParser.WrittenTypes.TString r,r,LibParser.WrittenTypes.TVariable (r,r,(r,source)),r);
        LibParser.WrittenTypes.TTuple (r,LibParser.WrittenTypes.TVariable (r,r,(r,"a")),r,LibParser.WrittenTypes.TString r,[r,LibParser.WrittenTypes.TVariable (r,r,(r,source))],r,r);
        LibParser.WrittenTypes.TFn (r,[LibParser.WrittenTypes.TVariable (r,r,(r,"a")),r;LibParser.WrittenTypes.TInt64 r,r],custom ["M"] source [LibParser.WrittenTypes.TVariable (r,r,(r,source))]);
        custom [] "RawPtr" [];custom [] "Stream" [LibParser.WrittenTypes.TInt64 r];custom [] "Stream" [];custom ["M"] source [LibParser.WrittenTypes.TString r];custom [] "R" [];custom [] "Alias" [];custom [] "S" [];custom [] "Generic" [LibParser.WrittenTypes.TString r];custom [] "Generic" [];custom [] "Cycle" []]
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let moduleType = typeof<AST.SemanticType>.Assembly.GetType("WrittenChecking")
    let entryType = typeof<AST.SemanticType>.Assembly.GetType("WrittenChecking+TypeEntry")
    let kindType = typeof<AST.SemanticType>.Assembly.GetType("WrittenChecking+TypeKind")
    let kind name = FSharpType.GetUnionCases(kindType,flags) |> Array.find (fun case -> case.Name=name) |> fun case -> FSharpValue.MakeUnion(case,[||],flags)
    let entry name parameters path definition = FSharpValue.MakeRecord(entryType,[|kind name;box parameters;box path;box definition|],flags)
    let field name typ : LibParser.WrittenTypes.RecordFieldSyntax = {range=r;name=r,name;typ=typ;description="";symbolColon=r}
    let enum name : LibParser.WrittenTypes.EnumCaseSyntax = {range=r;name=r,name;fields=[];description="";keywordOf=None}
    let entries = ["R",entry "RecordKind" ([]:string list) ([]:string list) (LibParser.WrittenTypes.TDRecord [field source (LibParser.WrittenTypes.TInt64 r),None]);
        "Alias",entry "AliasKind" ([]:string list) ([]:string list) (LibParser.WrittenTypes.TDAlias (LibParser.WrittenTypes.TInt64 r));"S",entry "SumKind" ([]:string list) ([]:string list) (LibParser.WrittenTypes.TDEnum [r,enum "C"]);
        "Other",entry "SumKind" ([]:string list) ([]:string list) (LibParser.WrittenTypes.TDEnum [r,enum "C"]);"Generic",entry "RecordKind" ["a"] ([]:string list) (LibParser.WrittenTypes.TDRecord [field "value" (LibParser.WrittenTypes.TVariable (r,r,(r,"a"))),None]);
        "Cycle",entry "AliasKind" ([]:string list) ([]:string list) (LibParser.WrittenTypes.TDAlias (custom [] "Cycle" []));"M.R",entry "RecordKind" ([]:string list) ["M"] (LibParser.WrittenTypes.TDRecord [])]
    let mapType = typedefof<Map<_,_>>.MakeGenericType [|typeof<string>;entryType|]
    let emptyEntries = Array.CreateInstance(FSharpType.MakeTupleType [|typeof<string>;entryType|],0)
    let empty = Activator.CreateInstance(mapType,[|box emptyEntries|])
    let inventory = entries |> List.fold (fun state (name,value) -> mapType.GetMethod("Add").Invoke(state,[|box name;value|])) empty
    let scopes = [Set.empty;Set.ofList [source;"a"]]
    let customResolver modules name args = if name="reject" then Error "custom rejected" else Ok (AST.TRecord (String.concat "." (modules @ [name]),args))
    let convertMethod = moduleType.GetMethod("resolveWrittenType",flags)
    let collectMethod = moduleType.GetMethod("collectWrittenTypeParams",flags)
    let conversions = references |> List.map (fun reference ->
        let simple = scopes |> List.map (fun scope -> WrittenChecking.typeReference customResolver scope reference)
        let resolved = scopes |> List.map (fun scope -> [false;true] |> List.map (fun allowInternal -> convertMethod.Invoke(null,[|box allowInternal;inventory;box ["M"];box scope;box reference|]) :?> Result<AST.SemanticType,string>))
        let variables = collectMethod.Invoke(null,[|box ["existing"];box reference|]) :?> string list
        simple,resolved,variables)
    let semTypes = [AST.TUnit;AST.TInt64;AST.TString;AST.TChar;AST.TNever;AST.TVar source;AST.TList (AST.TVar "a");AST.TList AST.TInt64;AST.TRecord ("R",[]);AST.TRecord ("Generic",[AST.TString]);AST.TSum ("S",[])]
    let require = moduleType.GetMethod("requireType",flags)
    let requirements = semTypes |> List.map (fun left -> semTypes |> List.map (fun right -> require.Invoke(null,[|box (Some left);box right|]) :?> Result<unit,string>))
    let collisions = moduleType.GetMethod("collidingCaseNames",flags).Invoke(null,[|inventory|]) :?> Set<string> |> Set.toList
    let floats = [false,"1","0";true,"0","0";false,"1_2","0";false,"1","5e309";false," 1","5 ";false,"NaN","";false,"1","2e-5000";false,source,"0";false,"Infinity","";false,"1","2e+3"]
    let floats = floats |> List.map (fun (negative,whole,fraction) -> let text=(if negative then "-" else "")+whole+"."+fraction in match Double.TryParse(text,Globalization.NumberStyles.Float,CultureInfo.InvariantCulture) with true,value -> Some value | _ -> None)
    let value = conversions,requirements,collisions,floats
    encode (value.GetType()) (box value)

let writtenPatterns source =
    let r = LibParser.WrittenTypes.synthRange
    let custom modules name args = LibParser.WrittenTypes.TCustom {range=r;modules=modules |> List.map (fun name -> ({range=r;name=name}:LibParser.WrittenTypes.Identifier),r);typ={range=r;name=name};typeArgs=args}
    let references = [LibParser.WrittenTypes.TUnit r;LibParser.WrittenTypes.TBool r;LibParser.WrittenTypes.TInt r;LibParser.WrittenTypes.TInt8 r;LibParser.WrittenTypes.TUInt8 r;LibParser.WrittenTypes.TInt16 r;LibParser.WrittenTypes.TUInt16 r;LibParser.WrittenTypes.TInt32 r;LibParser.WrittenTypes.TUInt32 r;LibParser.WrittenTypes.TInt64 r;LibParser.WrittenTypes.TUInt64 r;LibParser.WrittenTypes.TInt128 r;LibParser.WrittenTypes.TUInt128 r;LibParser.WrittenTypes.TFloat r;LibParser.WrittenTypes.TChar r;LibParser.WrittenTypes.TString r;LibParser.WrittenTypes.TDateTime r;LibParser.WrittenTypes.TUuid r;LibParser.WrittenTypes.TBlob r;
        LibParser.WrittenTypes.TVariable (r,r,(r,source));LibParser.WrittenTypes.TVariable (r,r,(r,"a"));LibParser.WrittenTypes.TList (r,r,r,LibParser.WrittenTypes.TVariable (r,r,(r,"a")),r);
        LibParser.WrittenTypes.TDict (r,r,r,LibParser.WrittenTypes.TString r,r,LibParser.WrittenTypes.TVariable (r,r,(r,source)),r);
        LibParser.WrittenTypes.TTuple (r,LibParser.WrittenTypes.TVariable (r,r,(r,"a")),r,LibParser.WrittenTypes.TString r,[r,LibParser.WrittenTypes.TVariable (r,r,(r,source))],r,r);
        LibParser.WrittenTypes.TFn (r,[LibParser.WrittenTypes.TVariable (r,r,(r,"a")),r;LibParser.WrittenTypes.TInt64 r,r],custom ["M"] source [LibParser.WrittenTypes.TVariable (r,r,(r,source))]);
        custom [] "RawPtr" [];custom [] "Stream" [LibParser.WrittenTypes.TInt64 r];custom [] "Stream" [];custom ["M"] source [LibParser.WrittenTypes.TString r];custom [] "R" [];custom [] "Alias" [];custom [] "S" [];custom [] "Generic" [LibParser.WrittenTypes.TString r];custom [] "Generic" [];custom [] "Cycle" []]
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let moduleType = typeof<AST.SemanticType>.Assembly.GetType("WrittenChecking")
    let entryType = typeof<AST.SemanticType>.Assembly.GetType("WrittenChecking+TypeEntry")
    let kindType = typeof<AST.SemanticType>.Assembly.GetType("WrittenChecking+TypeKind")
    let kind name = FSharpType.GetUnionCases(kindType,flags) |> Array.find (fun case -> case.Name=name) |> fun case -> FSharpValue.MakeUnion(case,[||],flags)
    let entry name parameters path definition = FSharpValue.MakeRecord(entryType,[|kind name;box parameters;box path;box definition|],flags)
    let field name typ : LibParser.WrittenTypes.RecordFieldSyntax = {range=r;name=r,name;typ=typ;description="";symbolColon=r}
    let enum name : LibParser.WrittenTypes.EnumCaseSyntax = {range=r;name=r,name;fields=[];description="";keywordOf=None}
    let entries = ["R",entry "RecordKind" ([]:string list) ([]:string list) (LibParser.WrittenTypes.TDRecord [field source (LibParser.WrittenTypes.TInt64 r),None]);
        "Alias",entry "AliasKind" ([]:string list) ([]:string list) (LibParser.WrittenTypes.TDAlias (LibParser.WrittenTypes.TInt64 r));"S",entry "SumKind" ([]:string list) ([]:string list) (LibParser.WrittenTypes.TDEnum [r,enum "C"]);
        "Other",entry "SumKind" ([]:string list) ([]:string list) (LibParser.WrittenTypes.TDEnum [r,enum "C"]);"Generic",entry "RecordKind" ["a"] ([]:string list) (LibParser.WrittenTypes.TDRecord [field "value" (LibParser.WrittenTypes.TVariable (r,r,(r,"a"))),None]);
        "Cycle",entry "AliasKind" ([]:string list) ([]:string list) (LibParser.WrittenTypes.TDAlias (custom [] "Cycle" []));"M.R",entry "RecordKind" ([]:string list) ["M"] (LibParser.WrittenTypes.TDRecord [])]
    let mapType = typedefof<Map<_,_>>.MakeGenericType [|typeof<string>;entryType|]
    let emptyEntries = Array.CreateInstance(FSharpType.MakeTupleType [|typeof<string>;entryType|],0)
    let empty = Activator.CreateInstance(mapType,[|box emptyEntries|])
    let inventory = entries |> List.fold (fun state (name,value) -> mapType.GetMethod("Add").Invoke(state,[|box name;value|])) empty
    let globalsType = typeof<AST.SemanticType>.Assembly.GetType("WrittenChecking+Globals")
    let emptyMap (typ:Type) = let args=typ.GetGenericArguments() in let entries=Array.CreateInstance(FSharpType.MakeTupleType args,0) in Activator.CreateInstance(typ,[|box entries|])
    let colliding = moduleType.GetMethod("collidingCaseNames",flags).Invoke(null,[|inventory|])
    let globals = FSharpValue.MakeRecord(globalsType,FSharpType.GetRecordFields(globalsType,flags) |> Array.map (fun field ->
        match field.Name with "Types" -> inventory | "CollidingCases" -> colliding | "AllowInternal" -> box false | "TypeParams" -> box (Set.empty<string>) | "ModulePath" -> box ["M"] | "CurrentFunction" -> null | _ -> emptyMap field.PropertyType),flags)
    let symbols = CheckedAST.emptySymbols ()
    let patterns = [LibParser.WrittenTypes.MPVariable (r,"_");LibParser.WrittenTypes.MPVariable (r,source);LibParser.WrittenTypes.MPUnit r;LibParser.WrittenTypes.MPBool (r,true);LibParser.WrittenTypes.MPInt (r,(r,1I));LibParser.WrittenTypes.MPInt64 (r,(r,1L),r);
        LibParser.WrittenTypes.MPInt8 (r,(r,1y),r);LibParser.WrittenTypes.MPUInt8 (r,(r,1uy),r);LibParser.WrittenTypes.MPInt16 (r,(r,1s),r);LibParser.WrittenTypes.MPUInt16 (r,(r,1us),r);LibParser.WrittenTypes.MPInt32 (r,(r,1),r);LibParser.WrittenTypes.MPUInt32 (r,(r,1ul),r);LibParser.WrittenTypes.MPUInt64 (r,(r,1UL),r);LibParser.WrittenTypes.MPInt128 (r,(r,Int128.Parse "1"),r);LibParser.WrittenTypes.MPUInt128 (r,(r,UInt128.Parse "1"),r);
        LibParser.WrittenTypes.MPString (r,Some (r,source),r,r);LibParser.WrittenTypes.MPChar (r,Some (r,source),r,r);LibParser.WrittenTypes.MPString (r,None,r,r);LibParser.WrittenTypes.MPFloat (r,false,"1","0");LibParser.WrittenTypes.MPFloat (r,false,"1_2","0");
        LibParser.WrittenTypes.MPTuple (r,LibParser.WrittenTypes.MPVariable (r,source),r,LibParser.WrittenTypes.MPVariable (r,"y"),[],r,r);LibParser.WrittenTypes.MPList (r,[LibParser.WrittenTypes.MPVariable (r,source),None],r,r);LibParser.WrittenTypes.MPListCons (r,LibParser.WrittenTypes.MPVariable (r,source),LibParser.WrittenTypes.MPVariable (r,"tail"),r);
        LibParser.WrittenTypes.MPEnum (r,(r,"C"),[]);LibParser.WrittenTypes.MPEnum (r,(r,"Missing"),[]);LibParser.WrittenTypes.MPOr (r,[]);LibParser.WrittenTypes.MPOr (r,[LibParser.WrittenTypes.MPVariable (r,source);LibParser.WrittenTypes.MPVariable (r,source)]);
        LibParser.WrittenTypes.MPOr (r,[LibParser.WrittenTypes.MPVariable (r,source);LibParser.WrittenTypes.MPVariable (r,"other")]);LibParser.WrittenTypes.MPError r]
    let types = [AST.TUnit;AST.TInt64;AST.TInt;AST.TBool;AST.TString;AST.TChar;AST.TFloat64;AST.TNever;AST.TVar source;AST.TInferenceVar (source,"fixed");AST.TTuple [AST.TInt64;AST.TString];AST.TList AST.TInt64;AST.TSum ("S",[])]
    let cases = if source="" then patterns |> List.collect (fun pattern -> types |> List.map (fun typ -> pattern,typ)) else patterns |> List.mapi (fun index pattern -> pattern,types[index % types.Length])
    let method = moduleType.GetMethod("checkMatchPattern",flags)
    let results = cases |> List.map (fun (pattern,expected) -> encode method.ReturnType (method.Invoke(null,[|globals;box symbols;box (None:Map<string,AST.SemanticType * AST.BindingId> option);box expected;box pattern|])))
    let letPatterns = [LibParser.WrittenTypes.LPUnit r;LibParser.WrittenTypes.LPWildcard r;LibParser.WrittenTypes.LPVariable (r,source);LibParser.WrittenTypes.LPTuple (r,LibParser.WrittenTypes.LPVariable (r,source),r,LibParser.WrittenTypes.LPVariable (r,"y"),[],r,r);LibParser.WrittenTypes.LPTuple (r,LibParser.WrittenTypes.LPVariable (r,source),r,LibParser.WrittenTypes.LPVariable (r,source),[],r,r)]
    let letMethod = moduleType.GetMethod("checkLetPattern",flags)
    let letResults = letPatterns |> List.map (fun pattern -> JsonArray(types |> List.map (fun typ -> encode letMethod.ReturnType (letMethod.Invoke(null,[|box pattern;box typ;box symbols|]))) |> List.toArray) :> JsonNode)
    let boolCase pattern guard : CheckedAST.MatchCase = {Patterns=AST.NonEmptyList.singleton pattern;Guard=guard;Body=CheckedAST.UnitLiteral}
    let witnesses : (AST.SemanticType * CheckedAST.Expr * CheckedAST.MatchCase list) list = [AST.TBool,CheckedAST.BoolLiteral true,[boolCase (CheckedAST.PBool true) None];AST.TBool,CheckedAST.Local (AST.topLevelValueId "value"),[boolCase (CheckedAST.PBool true) None];
        AST.TBool,CheckedAST.Local (AST.topLevelValueId "value"),[boolCase (CheckedAST.PBool true) None;boolCase (CheckedAST.PBool false) None];
        AST.TBool,CheckedAST.BoolLiteral true,[boolCase CheckedAST.PWildcard (Some (CheckedAST.BoolLiteral true))];
        AST.TList AST.TInt64,CheckedAST.Local (AST.topLevelValueId "value"),[boolCase (CheckedAST.PList []) None;boolCase (CheckedAST.PListCons ([CheckedAST.PWildcard],CheckedAST.PWildcard)) None];
        AST.TTuple [AST.TBool;AST.TBool],CheckedAST.Local (AST.topLevelValueId "value"),[boolCase (CheckedAST.PTuple [CheckedAST.PBool true;CheckedAST.PWildcard]) None;boolCase (CheckedAST.PTuple [CheckedAST.PBool false;CheckedAST.PBool true]) None;boolCase (CheckedAST.PTuple [CheckedAST.PBool false;CheckedAST.PBool false]) None]]
    let exhaustiveMethod = moduleType.GetMethod("matchIsExhaustive",flags)
    let exhaustive = witnesses |> List.map (fun (typ,value,cases) -> unbox<bool> (exhaustiveMethod.Invoke(null,[|globals;box symbols;box typ;box value;box cases|])))
    namedArray "tuple" [|JsonArray(Array.ofList results) :> JsonNode;JsonArray(Array.ofList letResults) :> JsonNode;encode typeof<bool list> (box exhaustive)|]

CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
CultureInfo.CurrentUICulture <- CultureInfo.InvariantCulture
let reader : IO.TextReader =
    match fsi.CommandLineArgs with
    | [| _; path |] -> new IO.StreamReader(path)
    | _ -> Console.In
let jsonOutputOptions = System.Text.Json.JsonSerializerOptions(MaxDepth=65536,Encoder=System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)
let anfScalarOptimization source =
    let tuple values = namedArray "tuple" (Array.ofList values)
    let fixtures : ANF.CExpr list = [(ANF.Atom ((ANF.StringLiteral source)));
        (ANF.TypedAtom ((ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
        (ANF.Prim ((ANF.Add), (ANF.StringLiteral source), (ANF.StringLiteral source)));
        (ANF.UnaryPrim ((ANF.Neg), (ANF.StringLiteral source)));
        (ANF.IfValue ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source)));
        (ANF.Call ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.BorrowedCall ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.TailCall ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.IndirectCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.IndirectTailCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.ClosureAlloc ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.ClosureCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.ClosureTailCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.TupleAlloc ([(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.TupleGet ((ANF.StringLiteral source), (3)));
        (ANF.RecordAlloc (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.RecordGet (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.StringLiteral source), (3)));
        (ANF.RecordClone (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.RecordReuse (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), ({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.StringConcat ((ANF.StringLiteral source), (ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.CanonicalBufferEq ((MemoryModel.Utf8String), (ANF.StringLiteral source), (ANF.StringLiteral source)));
        (ANF.RefCountInc ((ANF.StringLiteral source), (3), (MemoryModel.GenericHeap), (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey = (Some ((source))); MemoryModel.RcMetadata.ReleasePlan = (Some ((MemoryModel.NoReleasePlan))); MemoryModel.RcMetadata.SourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : MemoryModel.RcMetadata)))));
        (ANF.RefCountDec ((ANF.StringLiteral source), (3), (MemoryModel.GenericHeap), (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey = (Some ((source))); MemoryModel.RcMetadata.ReleasePlan = (Some ((MemoryModel.NoReleasePlan))); MemoryModel.RcMetadata.SourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : MemoryModel.RcMetadata)))));
        (ANF.Print ((ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
        (ANF.StdoutWrite ((ANF.StringLiteral source), (true)));
        (ANF.StdinReadLine);
        (ANF.RuntimeError ((source)));
        (ANF.RuntimeErrorString ((ANF.StringLiteral source)));
        (ANF.FileReadBlob ((ANF.StringLiteral source)));
        (ANF.FileExists ((ANF.StringLiteral source)));
        (ANF.FileWriteBlob ((ANF.StringLiteral source), (ANF.StringLiteral source)));
        (ANF.FileAppendText ((ANF.StringLiteral source), (ANF.StringLiteral source)));
        (ANF.FileDelete ((ANF.StringLiteral source)));
        (ANF.FileCreateDirectory ((ANF.StringLiteral source)));
        (ANF.FileSetExecutable ((ANF.StringLiteral source)));
        (ANF.FileWriteFromPtr ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source)));
        (ANF.FloatSqrt ((ANF.StringLiteral source)));
        (ANF.FloatAbs ((ANF.StringLiteral source)));
        (ANF.FloatNeg ((ANF.StringLiteral source)));
        (ANF.Int64ToFloat ((ANF.StringLiteral source)));
        (ANF.FloatToInt64 ((ANF.StringLiteral source)));
        (ANF.FloatToBits ((ANF.StringLiteral source)));
        (ANF.RawAlloc ((ANF.StringLiteral source)));
        (ANF.MappedAlloc ((ANF.StringLiteral source)));
        (ANF.RawFree ((ANF.StringLiteral source)));
        (ANF.MappedFree ((ANF.StringLiteral source)));
        (ANF.RawGet ((ANF.StringLiteral source), (ANF.StringLiteral source), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))));
        (ANF.RawTake ((ANF.StringLiteral source), (ANF.StringLiteral source), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))));
        (ANF.RawGetByte ((ANF.StringLiteral source), (ANF.StringLiteral source)));
        (ANF.RawWriteWord ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source)));
        (ANF.RawWriteByte ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source)));
        (ANF.RawSlotInit ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
        (ANF.StringToRawPtr ((ANF.StringLiteral source)));
        (ANF.RawPtrToString ((ANF.StringLiteral source)));
        (ANF.BlobToRawPtr ((ANF.StringLiteral source)));
        (ANF.RawPtrToBlob ((ANF.StringLiteral source)));
        (ANF.RawPtrToInt128 ((ANF.StringLiteral source)));
        (ANF.RawPtrToUInt128 ((ANF.StringLiteral source)));
        (ANF.DictToRawPtr ((ANF.StringLiteral source)));
        (ANF.RawPtrToDict ((ANF.StringLiteral source), (ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
        (ANF.ListToRawPtr ((ANF.StringLiteral source)));
        (ANF.FixedBlockToRawPtr ((ANF.StringLiteral source)));
        (ANF.RawPtrToList ((ANF.StringLiteral source), (ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
        (ANF.RefCountIncString ((ANF.StringLiteral source)));
        (ANF.RefCountDecString ((ANF.StringLiteral source)));
        (ANF.RefCountIncBlob ((ANF.StringLiteral source)));
        (ANF.RefCountDecBlob ((ANF.StringLiteral source)));
        (ANF.RefCountIncInt ((ANF.StringLiteral source)));
        (ANF.RefCountDecInt ((ANF.StringLiteral source)));
        (ANF.RandomInt64);
        (ANF.DateTimeNow);
        (ANF.Sleep ((ANF.StringLiteral source)));
        (ANF.CliNative ((ANF.Execute), [(ANF.StringLiteral source); (ANF.StringLiteral source)]));
        (ANF.FloatToString ((ANF.StringLiteral source)));
        (ANF.Atom ((ANF.Var (ANF.TempId 3))));
        (ANF.TypedAtom ((ANF.Var (ANF.TempId 3)), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
        (ANF.Prim ((ANF.Add), (ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3))));
        (ANF.UnaryPrim ((ANF.Neg), (ANF.Var (ANF.TempId 3))));
        (ANF.IfValue ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3))));
        (ANF.Call ((AST.functionId System.UInt64.MaxValue), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.BorrowedCall ((AST.functionId System.UInt64.MaxValue), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.TailCall ((AST.functionId System.UInt64.MaxValue), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.IndirectCall ((ANF.Var (ANF.TempId 3)), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.IndirectTailCall ((ANF.Var (ANF.TempId 3)), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.ClosureAlloc ((AST.functionId System.UInt64.MaxValue), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.ClosureCall ((ANF.Var (ANF.TempId 3)), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.ClosureTailCall ((ANF.Var (ANF.TempId 3)), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.TupleAlloc ([(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.TupleGet ((ANF.Var (ANF.TempId 3)), (3)));
        (ANF.RecordAlloc (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.RecordGet (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.Var (ANF.TempId 3)), (3)));
        (ANF.RecordClone (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.Var (ANF.TempId 3)), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.RecordReuse (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), ({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.Var (ANF.TempId 3)), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.StringConcat ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.CanonicalBufferEq ((MemoryModel.Utf8String), (ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3))));
        (ANF.RefCountInc ((ANF.Var (ANF.TempId 3)), (3), (MemoryModel.GenericHeap), (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey = (Some ((source))); MemoryModel.RcMetadata.ReleasePlan = (Some ((MemoryModel.NoReleasePlan))); MemoryModel.RcMetadata.SourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : MemoryModel.RcMetadata)))));
        (ANF.RefCountDec ((ANF.Var (ANF.TempId 3)), (3), (MemoryModel.GenericHeap), (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey = (Some ((source))); MemoryModel.RcMetadata.ReleasePlan = (Some ((MemoryModel.NoReleasePlan))); MemoryModel.RcMetadata.SourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : MemoryModel.RcMetadata)))));
        (ANF.Print ((ANF.Var (ANF.TempId 3)), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
        (ANF.StdoutWrite ((ANF.Var (ANF.TempId 3)), (true)));
        (ANF.StdinReadLine);
        (ANF.RuntimeError ((source)));
        (ANF.RuntimeErrorString ((ANF.Var (ANF.TempId 3))));
        (ANF.FileReadBlob ((ANF.Var (ANF.TempId 3))));
        (ANF.FileExists ((ANF.Var (ANF.TempId 3))));
        (ANF.FileWriteBlob ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3))));
        (ANF.FileAppendText ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3))));
        (ANF.FileDelete ((ANF.Var (ANF.TempId 3))));
        (ANF.FileCreateDirectory ((ANF.Var (ANF.TempId 3))));
        (ANF.FileSetExecutable ((ANF.Var (ANF.TempId 3))));
        (ANF.FileWriteFromPtr ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3))));
        (ANF.FloatSqrt ((ANF.Var (ANF.TempId 3))));
        (ANF.FloatAbs ((ANF.Var (ANF.TempId 3))));
        (ANF.FloatNeg ((ANF.Var (ANF.TempId 3))));
        (ANF.Int64ToFloat ((ANF.Var (ANF.TempId 3))));
        (ANF.FloatToInt64 ((ANF.Var (ANF.TempId 3))));
        (ANF.FloatToBits ((ANF.Var (ANF.TempId 3))));
        (ANF.RawAlloc ((ANF.Var (ANF.TempId 3))));
        (ANF.MappedAlloc ((ANF.Var (ANF.TempId 3))));
        (ANF.RawFree ((ANF.Var (ANF.TempId 3))));
        (ANF.MappedFree ((ANF.Var (ANF.TempId 3))));
        (ANF.RawGet ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))));
        (ANF.RawTake ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))));
        (ANF.RawGetByte ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3))));
        (ANF.RawWriteWord ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3))));
        (ANF.RawWriteByte ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3))));
        (ANF.RawSlotInit ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
        (ANF.StringToRawPtr ((ANF.Var (ANF.TempId 3))));
        (ANF.RawPtrToString ((ANF.Var (ANF.TempId 3))));
        (ANF.BlobToRawPtr ((ANF.Var (ANF.TempId 3))));
        (ANF.RawPtrToBlob ((ANF.Var (ANF.TempId 3))));
        (ANF.RawPtrToInt128 ((ANF.Var (ANF.TempId 3))));
        (ANF.RawPtrToUInt128 ((ANF.Var (ANF.TempId 3))));
        (ANF.DictToRawPtr ((ANF.Var (ANF.TempId 3))));
        (ANF.RawPtrToDict ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
        (ANF.ListToRawPtr ((ANF.Var (ANF.TempId 3))));
        (ANF.FixedBlockToRawPtr ((ANF.Var (ANF.TempId 3))));
        (ANF.RawPtrToList ((ANF.Var (ANF.TempId 3)), (ANF.Var (ANF.TempId 3)), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
        (ANF.RefCountIncString ((ANF.Var (ANF.TempId 3))));
        (ANF.RefCountDecString ((ANF.Var (ANF.TempId 3))));
        (ANF.RefCountIncBlob ((ANF.Var (ANF.TempId 3))));
        (ANF.RefCountDecBlob ((ANF.Var (ANF.TempId 3))));
        (ANF.RefCountIncInt ((ANF.Var (ANF.TempId 3))));
        (ANF.RefCountDecInt ((ANF.Var (ANF.TempId 3))));
        (ANF.RandomInt64);
        (ANF.DateTimeNow);
        (ANF.Sleep ((ANF.Var (ANF.TempId 3))));
        (ANF.CliNative ((ANF.Execute), [(ANF.Var (ANF.TempId 3)); (ANF.Var (ANF.TempId 3))]));
        (ANF.FloatToString ((ANF.Var (ANF.TempId 3))))]
    let ints = [Int64.MinValue;Int64.MaxValue;-2L;-1L;0L;1L;2L;3L;4L;63L;64L;65L]
    let floats = [Double.NegativeInfinity;-3.75;-2.0;-1.0;-0.0;0.0;0.5;1.0;2.0;3.75;Double.PositiveInfinity;BitConverter.Int64BitsToDouble 0x7ff8000000001234L]
    let atoms = [ANF.UnitLiteral;ANF.IntLiteral (ANF.Int8 -128y);ANF.IntLiteral (ANF.Int16 -32768s);ANF.IntLiteral (ANF.Int32 Int32.MinValue);ANF.IntLiteral (ANF.UInt8 255uy);ANF.IntLiteral (ANF.UInt16 65535us);ANF.IntLiteral (ANF.UInt32 UInt32.MaxValue);ANF.BoolLiteral true;ANF.BoolLiteral false;ANF.StringLiteral source;ANF.StringLiteral "";ANF.StringLiteral "e";ANF.StringLiteral "\u0301";ANF.Var (ANF.TempId 3);ANF.Var (ANF.TempId 4);ANF.FuncRef (AST.functionId UInt64.MaxValue)] @ (ints |> List.collect (fun n -> [ANF.IntLiteral (ANF.Int64 n);ANF.IntLiteral (ANF.UInt64 (uint64 n))])) @ (floats |> List.map ANF.FloatLiteral)
    let ops = [ANF.Add;ANF.Sub;ANF.Mul;ANF.Div;ANF.Mod;ANF.Shl;ANF.Shr;ANF.BitAnd;ANF.BitOr;ANF.BitXor;ANF.Eq;ANF.Neq;ANF.Lt;ANF.Gt;ANF.Lte;ANF.Gte;ANF.And;ANF.Or]
    let types = [AST.TUnit;AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TList AST.TInt64;AST.TTuple [AST.TString;AST.TInt64]]
    let typeEnvs = Map.empty :: (types |> List.map (fun typ -> Map.ofList [ANF.TempId 3,typ]))
    let envs = [Map.empty;Map.ofList [ANF.TempId 3,ANF.Var (ANF.TempId 3)];Map.ofList [ANF.TempId 3,ANF.IntLiteral (ANF.Int64 7L);ANF.TempId 4,ANF.StringLiteral source]]
    let context : ANFConstants.OptimizeContext = {TypeReg=Map.ofList [source,[("value",AST.TVar "a");("nested",AST.TVar "b")]];RecordTypeParams=Map.ofList [source,["a";"b"]];SumShapeReg=Map.empty;FunctionNames=FunctionIdMap.ofList [AST.functionId 1UL,"Darklang.Stdlib.String.__appendNormalized";AST.functionId 2UL,"Darklang.Stdlib.String.__normalizeAfterConcat"];FunctionIds=Map.empty}
    let options = [ANFConstants.defaultOptimizeOptions;{ANFConstants.defaultOptimizeOptions with EnableConstFolding=false};{ANFConstants.defaultOptimizeOptions with EnableStrengthReduction=false};{ANFConstants.defaultOptimizeOptions with EnableConstFolding=false;EnableStrengthReduction=false}]
    let extra = [ANF.Call (AST.functionId 1UL,[ANF.StringLiteral "e";ANF.StringLiteral "\u0301"]);ANF.Call (AST.functionId 2UL,[ANF.StringLiteral "e\u0301"]);ANF.Call (AST.functionId 1UL,[ANF.Var (ANF.TempId 3);ANF.StringLiteral ""]);ANF.Call (AST.functionId 1UL,[ANF.StringLiteral "";ANF.Var (ANF.TempId 3)]);ANF.TupleGet (ANF.Var (ANF.TempId 3),1);ANF.TupleGet (ANF.Var (ANF.TempId 3),2);ANF.StringConcat (ANF.StringLiteral "",ANF.Var (ANF.TempId 4),[ANF.StringLiteral ""])]
    let all = fixtures @ extra @ (types |> List.map (fun typ -> ANF.TypedAtom (ANF.Var (ANF.TempId 3),typ)))
    let tupleEnv = Map.ofList [ANF.TempId 3,Map.ofList [0,ANF.IntLiteral (ANF.Int64 7L);1,ANF.StringLiteral source]]
    let bodies = [ANF.Return (ANF.Var (ANF.TempId 3));ANF.Let (ANF.TempId 3,ANF.Atom (ANF.Var (ANF.TempId 3)),ANF.Let (ANF.TempId 4,ANF.Prim (ANF.Add,ANF.Var (ANF.TempId 3),ANF.Var (ANF.TempId 4)),ANF.Return (ANF.Var (ANF.TempId 4))));ANF.Join ({ANF.TypedParam.Id=ANF.TempId 3;ANF.TypedParam.Type=AST.TInt64},ANF.Let (ANF.TempId 4,ANF.Atom (ANF.Var (ANF.TempId 3)),ANF.Return (ANF.Var (ANF.TempId 4))),ANF.If (ANF.Var (ANF.TempId 3),ANF.Jump (ANF.TempId 3,ANF.Var (ANF.TempId 4)),ANF.Let (ANF.TempId 4,ANF.Atom (ANF.Var (ANF.TempId 3)),ANF.Jump (ANF.TempId 3,ANF.Var (ANF.TempId 4)))))]
    let function_ id name body : ANF.Function = {Id=AST.functionId id;Name=name;TypedParams=[{ANF.TypedParam.Id=ANF.TempId 3;ANF.TypedParam.Type=AST.TInt64}];ReturnType=AST.TInt64;ReturnOwnership=ANF.OwnedReturn;Body=body}
    let functions = [function_ 0UL source (List.foldBack (fun c body -> ANF.Let (ANF.TempId 4,c,body)) fixtures (ANF.Return (ANF.Var (ANF.TempId 4))));function_ 1UL "one" (ANF.Let (ANF.TempId 4,ANF.Call (AST.functionId 2UL,[]),ANF.Return (ANF.Var (ANF.TempId 4))));function_ 2UL "two" (ANF.Let (ANF.TempId 4,ANF.Call (AST.functionId 1UL,[]),ANF.Return (ANF.Var (ANF.TempId 4))));function_ 3UL "self" (ANF.Let (ANF.TempId 4,ANF.Call (AST.functionId 3UL,[]),ANF.Return (ANF.Var (ANF.TempId 4))));function_ UInt64.MaxValue "Darklang.Stdlib.Json.__test" (ANF.Return ANF.UnitLiteral)]
    let graph = InliningCommon.buildFunctionInfoMap functions |> FunctionIdMap.map (fun _ info -> info.Calls)
    let ids = functions |> List.map (fun f -> f.Id) |> Set.ofList
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let effectMethod name = typeof<AST.SemanticType>.Assembly.GetType("ANFEffects").GetMethod(name,flags)
    let preserve = effectMethod "mustPreserveEvaluation"
    let uses = effectMethod "cexprTempUses"
    let fold = (effectMethod "foldCExprTempIds").MakeGenericMethod [|typeof<ANF.TempId list>|]
    let usesTemp = effectMethod "cexprUsesTemp"
    let forward = effectMethod "canForwardTupleElement"
    let effect expr =
        let add : ANF.TempId -> ANF.TempId list -> ANF.TempId list = fun tid xs -> tid::xs
        unbox<bool> (preserve.Invoke(null,[|box context;box expr|])),
        (unbox<Set<ANF.TempId>> (uses.Invoke(null,[|box expr|])) |> Set.toList),
        (unbox<ANF.TempId list> (fold.Invoke(null,[|box add;box expr;box ([] : ANF.TempId list)|])) |> List.rev),
        ([0;3;4] |> List.map (fun tid -> unbox<bool> (usesTemp.Invoke(null,[|box (ANF.TempId tid);box expr|]))))
    let graphJson value = encode typeof<(AST.FunctionId * AST.FunctionId list) list> (box (value |> FunctionIdMap.toList |> List.map (fun (id,calls) -> id,Set.toList calls)))
    let infoJson (info: InliningCommon.FunctionInfo) = encode typeof<ANF.Function * AST.FunctionId list * int * bool * bool * bool * bool * bool list> (box (info.Func,Set.toList info.Calls,info.Size,info.IsRecursive,info.HasClosures,info.HasTailCalls,info.IsExternal,[-1;0;2;3;4] |> List.map (InliningCommon.shouldInline info InliningCommon.defaultConfig)))
    tuple [
        encode typeof<(int64 option * int64 option) list> (box (ints |> List.map (fun n -> ANFConstants.tryLog2 n,ANFConstants.tryLog2UInt64 (uint64 n))))
        encode typeof<int64 option list> (box (floats |> List.map ANFConstants.tryTruncateFloatToInt64))
        encode typeof<ANF.CExpr option list list list> (box (ops |> List.map (fun op -> atoms |> List.map (fun left -> atoms |> List.map (ANFConstants.foldBinOp op left)))))
        encode typeof<ANF.CExpr option list list list list> (box (typeEnvs |> List.map (fun env -> ops |> List.map (fun op -> atoms |> List.map (fun left -> atoms |> List.map (ANFConstants.tryStrengthReduce env op left))))))
        encode typeof<ANF.CExpr option list list> (box ([ANF.Neg;ANF.Not;ANF.BitNot] |> List.map (fun op -> atoms |> List.map (ANFConstants.foldUnaryOp op))))
        encode typeof<(bool * ANF.TempId list * ANF.TempId list * bool list) list> (box (all |> List.map effect))
        encode typeof<ANF.CExpr list list> (box (envs |> List.map (fun env -> all |> List.map (ANFSubstitution.substCExpr env))))
        encode typeof<(ANF.CExpr * bool) list list list> (box (options |> List.map (fun options -> envs |> List.map (fun env -> all |> List.map (ANFSubstitution.optimizeCExpr context options env Map.empty tupleEnv)))))
        encode typeof<bool list list> (box (typeEnvs |> List.map (fun env -> atoms |> List.map (fun atom -> unbox<bool> (forward.Invoke(null,[|box context;box env;box atom|]))))))
        encode typeof<(ANF.AExpr * ANF.VarGen) list> (box (bodies |> List.map (InliningCommon.renameExpr (Map.ofList [ANF.TempId 3,ANF.TempId 30;ANF.TempId 4,ANF.TempId 40]) (ANF.VarGen 100))))
        encode typeof<ANF.CExpr list> (box (fixtures |> List.map (InliningCommon.renameCExpr (Map.ofList [ANF.TempId 3,ANF.TempId 30]))))
        graphJson graph
        graphJson (InliningCommon.buildReverseCallGraph graph)
        encode typeof<AST.FunctionId list list> (box (InliningCommon.findSCCs ids graph |> List.map Set.toList))
        JsonArray(InliningCommon.buildFunctionInfoMap functions |> FunctionIdMap.toList |> List.map (snd >> infoJson) |> List.toArray) :> JsonNode
        JsonArray(InliningCommon.buildExternalCandidateInfoMap InliningCommon.defaultConfig functions |> FunctionIdMap.toList |> List.map (snd >> infoJson) |> List.toArray) :> JsonNode
        graphJson (ANFDeadCodeElimination.buildCallGraph functions)
        encode typeof<ANF.Function list> (box (ANFDeadCodeElimination.filterReachableFunctions (Set.singleton (AST.functionId 1UL)) functions))
        encode typeof<AST.FunctionId list> (box (ANFDeadCodeElimination.getReachableStdlib (ANFDeadCodeElimination.buildCallGraph functions) [List.head functions] |> Set.toList))]

let rcInternalCall<'a> moduleName name args : 'a =
    let method = typeof<AST.SemanticType>.Assembly.GetType(moduleName).GetMethod(name, Reflection.BindingFlags.Static ||| Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
    try unbox<'a> (method.Invoke(null,args)) with :? Reflection.TargetInvocationException as error -> raise error.InnerException
let lirConstructorFixtures (source:string) =
    let enc (value:'a) = encode typeof<'a> (box value)
    let tuple values = namedArray "tuple" (Array.ofList values)
    let operand = LIR.StringSymbol source
    let reg = LIR.Virtual 3
    let freg = LIR.FVirtual (-1)
    let typ = AST.TRecord (source,[AST.TInt64;AST.TList AST.TString])
    tuple [tuple [enc ([LIR.X0;
LIR.X1;
LIR.X2;
LIR.X3;
LIR.X4;
LIR.X5;
LIR.X6;
LIR.X7;
LIR.X8;
LIR.X9;
LIR.X10;
LIR.X11;
LIR.X12;
LIR.X13;
LIR.X14;
LIR.X15;
LIR.X16;
LIR.X17;
LIR.X19;
LIR.X20;
LIR.X21;
LIR.X22;
LIR.X23;
LIR.X24;
LIR.X25;
LIR.X26;
LIR.X27;
LIR.X29;
LIR.X30;
LIR.SP] : LIR.PhysReg list);
enc ([LIR.D0;
LIR.D1;
LIR.D2;
LIR.D3;
LIR.D4;
LIR.D5;
LIR.D6;
LIR.D7;
LIR.D8;
LIR.D9;
LIR.D10;
LIR.D11;
LIR.D12;
LIR.D13;
LIR.D14;
LIR.D15] : LIR.PhysFPReg list);
enc ([LIR.Physical (LIR.X0);
LIR.Virtual ((3))] : LIR.Reg list);
enc ([LIR.FPhysical (LIR.D0);
LIR.FVirtual ((3))] : LIR.FReg list);
enc ([LIR.Imm ((-3L));
LIR.FloatImm ((-0.0));
LIR.Reg ((reg));
LIR.StackSlot ((3));
LIR.StringSymbol ((source));
LIR.FloatSymbol ((-0.0));
LIR.FuncAddr ((AST.functionId System.UInt64.MaxValue))] : LIR.Operand list);
enc ([LIR.EQ;
LIR.NE;
LIR.LT;
LIR.GT;
LIR.LE;
LIR.GE;
LIR.ULT;
LIR.UGT;
LIR.ULE;
LIR.UGE] : LIR.Condition list);
enc ([LIR.GenericHeap;
LIR.StreamHeap;
LIR.TaggedList;
LIR.DictHeap;
LIR.ClosureHeap] : LIR.RcKind list);
enc ([LIR.Execute;
LIR.RunProcess;
LIR.HostOS;
LIR.HostArchitecture;
LIR.Hostname;
LIR.GetEnv;
LIR.GetEnvironmentPacked;
LIR.SetEnv;
LIR.UnsetEnv;
LIR.DirectoryCurrent;
LIR.DirectoryListPacked;
LIR.FileIsDirectory;
LIR.FileCreateExclusive;
LIR.GetArgv;
LIR.Kill;
LIR.GetPid;
LIR.GetUid;
LIR.CpuCount;
LIR.SpawnProcess;
LIR.ProcessIO;
LIR.TerminateProcess;
LIR.SocketTcp4;
LIR.SocketTcp6;
LIR.SocketUdp4;
LIR.SocketUdp6;
LIR.SocketConnect4;
LIR.SocketConnect6;
LIR.SocketSend;
LIR.SocketReceive;
LIR.SocketReceiveTimeout;
LIR.SocketSendTimeout;
LIR.SocketClose;
LIR.SecureRandomFill] : LIR.CliOperation list);
enc ([LIR.Label ((source))] : LIR.Label list);
enc ([LIR.Mov ((reg), (operand));
LIR.Phi ((reg), [((operand), (LIR.Label source)); ((operand), (LIR.Label source))], (Some ((typ))));
LIR.Store ((3), (reg));
LIR.Add ((reg), (reg), (operand));
LIR.Sub ((reg), (reg), (operand));
LIR.Mul ((reg), (reg), (reg));
LIR.Sdiv ((reg), (reg), (reg));
LIR.Udiv ((reg), (reg), (reg));
LIR.Msub ((reg), (reg), (reg), (reg));
LIR.Madd ((reg), (reg), (reg), (reg));
LIR.Cmp ((reg), (operand));
LIR.Cset ((reg), LIR.EQ);
LIR.Select ((reg), (reg), (reg), LIR.EQ);
LIR.And ((reg), (reg), (reg));
LIR.And_imm ((reg), (reg), (-3L));
LIR.Orr ((reg), (reg), (reg));
LIR.Eor ((reg), (reg), (reg));
LIR.Lsl ((reg), (reg), (reg));
LIR.Lsr ((reg), (reg), (reg));
LIR.Asr ((reg), (reg), (reg));
LIR.Lsl_imm ((reg), (reg), (3));
LIR.Lsr_imm ((reg), (reg), (3));
LIR.Asr_imm ((reg), (reg), (3));
LIR.Neg ((reg), (reg));
LIR.Mvn ((reg), (reg));
LIR.Sxtb ((reg), (reg));
LIR.Sxth ((reg), (reg));
LIR.Sxtw ((reg), (reg));
LIR.Uxtb ((reg), (reg));
LIR.Uxth ((reg), (reg));
LIR.Uxtw ((reg), (reg));
LIR.Call ((reg), (AST.functionId System.UInt64.MaxValue), [(operand); (operand)]);
LIR.TailCall ((AST.functionId System.UInt64.MaxValue), [(operand); (operand)]);
LIR.IndirectCall ((reg), (reg), [(operand); (operand)]);
LIR.IndirectTailCall ((reg), [(operand); (operand)]);
LIR.ClosureAlloc ((reg), (AST.functionId System.UInt64.MaxValue), [(operand); (operand)]);
LIR.ClosureCall ((reg), (reg), [(operand); (operand)]);
LIR.ClosureTailCall ((reg), [(operand); (operand)]);
LIR.SaveRegs ([LIR.X0; LIR.X0], [LIR.D0; LIR.D0]);
LIR.RestoreRegs ([LIR.X0; LIR.X0], [LIR.D0; LIR.D0]);
LIR.ArgMoves ([(LIR.X0, (operand)); (LIR.X0, (operand))]);
LIR.TailArgMoves ([(LIR.X0, (operand)); (LIR.X0, (operand))]);
LIR.FArgMoves ([(LIR.D0, (freg)); (LIR.D0, (freg))]);
LIR.PrintInt64 ((reg));
LIR.PrintUInt64 ((reg));
LIR.PrintBool ((reg));
LIR.PrintInt64NoNewline ((reg));
LIR.PrintUInt64NoNewline ((reg));
LIR.PrintBoolNoNewline ((reg));
LIR.PrintFloat ((freg));
LIR.PrintFloatNoNewline ((freg));
LIR.PrintString ((source));
LIR.StdoutWrite ((3), (operand), (true));
LIR.StdinReadLine ((3), (reg));
LIR.RuntimeError ((source));
LIR.RuntimeErrorString ((reg));
LIR.PrintHeapStringNoNewline ((reg));
LIR.PrintChars ([0uy;127uy;255uy]);
LIR.PrintBlob ((reg));
LIR.PrintList ((reg), (typ));
LIR.PrintSum ((reg), [((source), (3), (Some ((typ)))); ((source), (3), (Some ((typ))))], (true));
LIR.PrintRecord ((reg), (source), [((source), (typ)); ((source), (typ))]);
LIR.Exit;
LIR.FPhi ((freg), [((freg), (LIR.Label source)); ((freg), (LIR.Label source))]);
LIR.FMov ((freg), (freg));
LIR.FLoad ((freg), (-0.0));
LIR.FSpillLoad ((freg), (3));
LIR.FSpillStore ((3), (freg));
LIR.FAdd ((freg), (freg), (freg));
LIR.FSub ((freg), (freg), (freg));
LIR.FMul ((freg), (freg), (freg));
LIR.FMadd ((freg), (freg), (freg), (freg));
LIR.FDiv ((freg), (freg), (freg));
LIR.FNeg ((freg), (freg));
LIR.FAbs ((freg), (freg));
LIR.FSqrt ((freg), (freg));
LIR.FCmp ((freg), (freg));
LIR.Int64ToFloat ((freg), (reg));
LIR.FloatToInt64 ((reg), (freg));
LIR.FloatToBits ((reg), (freg));
LIR.GpToFp ((freg), (reg));
LIR.FpToGp ((reg), (freg));
LIR.HeapAlloc ((reg), (3));
LIR.HeapStore ((reg), (3), (operand), (Some ((typ))));
LIR.HeapLoad ((reg), (reg), (3));
LIR.RefCountInc ((reg), (3), LIR.GenericHeap, (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey=Some source;ReleasePlan=Some (MemoryModel.RecursiveRelease (AST.TList AST.TString));SourceType=Some AST.TString}))));
LIR.RefCountDec ((reg), (3), LIR.GenericHeap, (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey=Some source;ReleasePlan=Some (MemoryModel.RecursiveRelease (AST.TList AST.TString));SourceType=Some AST.TString}))));
LIR.StringConcat ((reg), (operand), (operand), [(operand); (operand)]);
LIR.CanonicalBufferEq ((reg), (MemoryModel.NullableGraphemeCluster), (operand), (operand));
LIR.PrintHeapString ((reg));
LIR.LoadFuncAddr ((reg), (AST.functionId System.UInt64.MaxValue));
LIR.FileReadBlob ((reg), (operand));
LIR.FileExists ((reg), (operand));
LIR.FileWriteBlob ((reg), (operand), (operand));
LIR.FileAppendText ((reg), (operand), (operand));
LIR.FileDelete ((reg), (operand));
LIR.FileCreateDirectory ((reg), (operand));
LIR.FileSetExecutable ((reg), (operand));
LIR.FileWriteFromPtr ((reg), (operand), (reg), (reg));
LIR.RawAlloc ((reg), (reg));
LIR.MappedAlloc ((reg), (reg));
LIR.RawFree ((reg));
LIR.MappedFree ((reg));
LIR.RawGet ((reg), (reg), (reg));
LIR.RawGetByte ((reg), (reg), (reg));
LIR.RawWriteWord ((reg), (reg), (reg));
LIR.RawWriteByte ((reg), (reg), (reg));
LIR.RawSlotInit ((reg), (reg), (reg), (typ));
LIR.RefCountIncString ((operand));
LIR.RefCountDecString ((operand));
LIR.RefCountIncBlob ((operand));
LIR.RefCountDecBlob ((operand));
LIR.RefCountIncInt ((operand));
LIR.RefCountDecInt ((operand));
LIR.RandomInt64 ((reg));
LIR.DateTimeNow ((reg));
LIR.Sleep ((3), (freg));
LIR.CliNative ((reg), LIR.Execute, [(operand); (operand)]);
LIR.FloatToString ((reg), (freg));
LIR.CoverageHit ((3))] : LIR.Instr list);
enc ([LIR.Ret;
LIR.Branch ((reg), (LIR.Label source), (LIR.Label source));
LIR.BranchZero ((reg), (LIR.Label source), (LIR.Label source));
LIR.BranchBitZero ((reg), (3), (LIR.Label source), (LIR.Label source));
LIR.BranchBitNonZero ((reg), (3), (LIR.Label source), (LIR.Label source));
LIR.CondBranch (LIR.EQ, (LIR.Label source), (LIR.Label source));
LIR.Jump ((LIR.Label source))] : LIR.Terminator list);
enc ([LIR.FingerprintedReleasePlan ((source));
LIR.StructuralReleasePlan ((Some ((MemoryModel.RootRelease (16,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (16,[MemoryModel.FieldRelease (8,MemoryModel.RecursiveRelease (AST.TRecord (source,[])))]))))))] : LIR.RcReleasePlanMemoKey list);
enc ([LIR.SlotInitListRootRetain;
LIR.SlotInitDictRootRetain;
LIR.SlotInitDynamicBufferRetain;
LIR.SlotInitClosureRootRetain;
LIR.SlotInitGenericRootRetain ((3))] : LIR.Arm64SlotInitRootRetainTarget list)];tuple [enc (FSharpType.GetUnionCases(typeof<LIR.PhysReg>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.PhysFPReg>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.Reg>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.FReg>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.Operand>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.Condition>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.RcKind>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.CliOperation>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.Label>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.Instr>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.Terminator>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.RcReleasePlanMemoKey>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList);
enc (FSharpType.GetUnionCases(typeof<LIR.Arm64SlotInitRootRetainTarget>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList)]]
let lirInstructionFixturesWithRegisters (source:string) reg freg operand typ : LIR.Instr list = [LIR.Mov ((reg), (operand));
LIR.Phi ((reg), [((operand), (LIR.Label source)); ((operand), (LIR.Label source))], (Some ((typ))));
LIR.Store ((3), (reg));
LIR.Add ((reg), (reg), (operand));
LIR.Sub ((reg), (reg), (operand));
LIR.Mul ((reg), (reg), (reg));
LIR.Sdiv ((reg), (reg), (reg));
LIR.Udiv ((reg), (reg), (reg));
LIR.Msub ((reg), (reg), (reg), (reg));
LIR.Madd ((reg), (reg), (reg), (reg));
LIR.Cmp ((reg), (operand));
LIR.Cset ((reg), LIR.EQ);
LIR.Select ((reg), (reg), (reg), LIR.EQ);
LIR.And ((reg), (reg), (reg));
LIR.And_imm ((reg), (reg), (-3L));
LIR.Orr ((reg), (reg), (reg));
LIR.Eor ((reg), (reg), (reg));
LIR.Lsl ((reg), (reg), (reg));
LIR.Lsr ((reg), (reg), (reg));
LIR.Asr ((reg), (reg), (reg));
LIR.Lsl_imm ((reg), (reg), (3));
LIR.Lsr_imm ((reg), (reg), (3));
LIR.Asr_imm ((reg), (reg), (3));
LIR.Neg ((reg), (reg));
LIR.Mvn ((reg), (reg));
LIR.Sxtb ((reg), (reg));
LIR.Sxth ((reg), (reg));
LIR.Sxtw ((reg), (reg));
LIR.Uxtb ((reg), (reg));
LIR.Uxth ((reg), (reg));
LIR.Uxtw ((reg), (reg));
LIR.Call ((reg), (AST.functionId System.UInt64.MaxValue), [(operand); (operand)]);
LIR.TailCall ((AST.functionId System.UInt64.MaxValue), [(operand); (operand)]);
LIR.IndirectCall ((reg), (reg), [(operand); (operand)]);
LIR.IndirectTailCall ((reg), [(operand); (operand)]);
LIR.ClosureAlloc ((reg), (AST.functionId System.UInt64.MaxValue), [(operand); (operand)]);
LIR.ClosureCall ((reg), (reg), [(operand); (operand)]);
LIR.ClosureTailCall ((reg), [(operand); (operand)]);
LIR.SaveRegs ([LIR.X0; LIR.X0], [LIR.D0; LIR.D0]);
LIR.RestoreRegs ([LIR.X0; LIR.X0], [LIR.D0; LIR.D0]);
LIR.ArgMoves ([(LIR.X0, (operand)); (LIR.X0, (operand))]);
LIR.TailArgMoves ([(LIR.X0, (operand)); (LIR.X0, (operand))]);
LIR.FArgMoves ([(LIR.D0, (freg)); (LIR.D0, (freg))]);
LIR.PrintInt64 ((reg));
LIR.PrintUInt64 ((reg));
LIR.PrintBool ((reg));
LIR.PrintInt64NoNewline ((reg));
LIR.PrintUInt64NoNewline ((reg));
LIR.PrintBoolNoNewline ((reg));
LIR.PrintFloat ((freg));
LIR.PrintFloatNoNewline ((freg));
LIR.PrintString ((source));
LIR.StdoutWrite ((3), (operand), (true));
LIR.StdinReadLine ((3), (reg));
LIR.RuntimeError ((source));
LIR.RuntimeErrorString ((reg));
LIR.PrintHeapStringNoNewline ((reg));
LIR.PrintChars ([0uy;127uy;255uy]);
LIR.PrintBlob ((reg));
LIR.PrintList ((reg), (typ));
LIR.PrintSum ((reg), [((source), (3), (Some ((typ)))); ((source), (3), (Some ((typ))))], (true));
LIR.PrintRecord ((reg), (source), [((source), (typ)); ((source), (typ))]);
LIR.Exit;
LIR.FPhi ((freg), [((freg), (LIR.Label source)); ((freg), (LIR.Label source))]);
LIR.FMov ((freg), (freg));
LIR.FLoad ((freg), (-0.0));
LIR.FSpillLoad ((freg), (3));
LIR.FSpillStore ((3), (freg));
LIR.FAdd ((freg), (freg), (freg));
LIR.FSub ((freg), (freg), (freg));
LIR.FMul ((freg), (freg), (freg));
LIR.FMadd ((freg), (freg), (freg), (freg));
LIR.FDiv ((freg), (freg), (freg));
LIR.FNeg ((freg), (freg));
LIR.FAbs ((freg), (freg));
LIR.FSqrt ((freg), (freg));
LIR.FCmp ((freg), (freg));
LIR.Int64ToFloat ((freg), (reg));
LIR.FloatToInt64 ((reg), (freg));
LIR.FloatToBits ((reg), (freg));
LIR.GpToFp ((freg), (reg));
LIR.FpToGp ((reg), (freg));
LIR.HeapAlloc ((reg), (3));
LIR.HeapStore ((reg), (3), (operand), (Some ((typ))));
LIR.HeapLoad ((reg), (reg), (3));
LIR.RefCountInc ((reg), (3), LIR.GenericHeap, (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey=Some source;ReleasePlan=Some (MemoryModel.RecursiveRelease (AST.TList AST.TString));SourceType=Some AST.TString}))));
LIR.RefCountDec ((reg), (3), LIR.GenericHeap, (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey=Some source;ReleasePlan=Some (MemoryModel.RecursiveRelease (AST.TList AST.TString));SourceType=Some AST.TString}))));
LIR.StringConcat ((reg), (operand), (operand), [(operand); (operand)]);
LIR.CanonicalBufferEq ((reg), (MemoryModel.NullableGraphemeCluster), (operand), (operand));
LIR.PrintHeapString ((reg));
LIR.LoadFuncAddr ((reg), (AST.functionId System.UInt64.MaxValue));
LIR.FileReadBlob ((reg), (operand));
LIR.FileExists ((reg), (operand));
LIR.FileWriteBlob ((reg), (operand), (operand));
LIR.FileAppendText ((reg), (operand), (operand));
LIR.FileDelete ((reg), (operand));
LIR.FileCreateDirectory ((reg), (operand));
LIR.FileSetExecutable ((reg), (operand));
LIR.FileWriteFromPtr ((reg), (operand), (reg), (reg));
LIR.RawAlloc ((reg), (reg));
LIR.MappedAlloc ((reg), (reg));
LIR.RawFree ((reg));
LIR.MappedFree ((reg));
LIR.RawGet ((reg), (reg), (reg));
LIR.RawGetByte ((reg), (reg), (reg));
LIR.RawWriteWord ((reg), (reg), (reg));
LIR.RawWriteByte ((reg), (reg), (reg));
LIR.RawSlotInit ((reg), (reg), (reg), (typ));
LIR.RefCountIncString ((operand));
LIR.RefCountDecString ((operand));
LIR.RefCountIncBlob ((operand));
LIR.RefCountDecBlob ((operand));
LIR.RefCountIncInt ((operand));
LIR.RefCountDecInt ((operand));
LIR.RandomInt64 ((reg));
LIR.DateTimeNow ((reg));
LIR.Sleep ((3), (freg));
LIR.CliNative ((reg), LIR.Execute, [(operand); (operand)]);
LIR.FloatToString ((reg), (freg));
LIR.CoverageHit ((3))]
let lirInstructionFixturesWithOperand source operand = lirInstructionFixturesWithRegisters source (LIR.Virtual 3) (LIR.FVirtual (-1)) operand (AST.TRecord (source,[AST.TInt64;AST.TList AST.TString]))
let lirInstructionFixtures source = lirInstructionFixturesWithOperand source (LIR.StringSymbol source)
let lirAllocationFixtures (source:string) (regs:LIR.Reg array) (fregs:LIR.FReg array) operand typ : LIR.Instr list =
    let freg=fregs[0]
    [LIR.Mov ((regs[0]), (operand));
LIR.Phi ((regs[0]), [((operand), (LIR.Label source)); ((operand), (LIR.Label source))], (Some ((typ))));
LIR.Store ((3), (regs[0]));
LIR.Add ((regs[0]), (regs[1]), (operand));
LIR.Sub ((regs[0]), (regs[1]), (operand));
LIR.Mul ((regs[0]), (regs[1]), (regs[2]));
LIR.Sdiv ((regs[0]), (regs[1]), (regs[2]));
LIR.Udiv ((regs[0]), (regs[1]), (regs[2]));
LIR.Msub ((regs[0]), (regs[1]), (regs[2]), (regs[3]));
LIR.Madd ((regs[0]), (regs[1]), (regs[2]), (regs[3]));
LIR.Cmp ((regs[0]), (operand));
LIR.Cset ((regs[0]), LIR.EQ);
LIR.Select ((regs[0]), (regs[1]), (regs[2]), LIR.EQ);
LIR.And ((regs[0]), (regs[1]), (regs[2]));
LIR.And_imm ((regs[0]), (regs[1]), (-3L));
LIR.Orr ((regs[0]), (regs[1]), (regs[2]));
LIR.Eor ((regs[0]), (regs[1]), (regs[2]));
LIR.Lsl ((regs[0]), (regs[1]), (regs[2]));
LIR.Lsr ((regs[0]), (regs[1]), (regs[2]));
LIR.Asr ((regs[0]), (regs[1]), (regs[2]));
LIR.Lsl_imm ((regs[0]), (regs[1]), (3));
LIR.Lsr_imm ((regs[0]), (regs[1]), (3));
LIR.Asr_imm ((regs[0]), (regs[1]), (3));
LIR.Neg ((regs[0]), (regs[1]));
LIR.Mvn ((regs[0]), (regs[1]));
LIR.Sxtb ((regs[0]), (regs[1]));
LIR.Sxth ((regs[0]), (regs[1]));
LIR.Sxtw ((regs[0]), (regs[1]));
LIR.Uxtb ((regs[0]), (regs[1]));
LIR.Uxth ((regs[0]), (regs[1]));
LIR.Uxtw ((regs[0]), (regs[1]));
LIR.Call ((regs[0]), (AST.functionId System.UInt64.MaxValue), [(operand); (operand)]);
LIR.TailCall ((AST.functionId System.UInt64.MaxValue), [(operand); (operand)]);
LIR.IndirectCall ((regs[0]), (regs[1]), [(operand); (operand)]);
LIR.IndirectTailCall ((regs[0]), [(operand); (operand)]);
LIR.ClosureAlloc ((regs[0]), (AST.functionId System.UInt64.MaxValue), [(operand); (operand)]);
LIR.ClosureCall ((regs[0]), (regs[1]), [(operand); (operand)]);
LIR.ClosureTailCall ((regs[0]), [(operand); (operand)]);
LIR.SaveRegs ([LIR.X0; LIR.X0], [LIR.D0; LIR.D0]);
LIR.RestoreRegs ([LIR.X0; LIR.X0], [LIR.D0; LIR.D0]);
LIR.ArgMoves ([(LIR.X0, (operand)); (LIR.X0, (operand))]);
LIR.TailArgMoves ([(LIR.X0, (operand)); (LIR.X0, (operand))]);
LIR.FArgMoves ([(LIR.D0, (freg)); (LIR.D0, (freg))]);
LIR.PrintInt64 ((regs[0]));
LIR.PrintUInt64 ((regs[0]));
LIR.PrintBool ((regs[0]));
LIR.PrintInt64NoNewline ((regs[0]));
LIR.PrintUInt64NoNewline ((regs[0]));
LIR.PrintBoolNoNewline ((regs[0]));
LIR.PrintFloat ((fregs[0]));
LIR.PrintFloatNoNewline ((fregs[0]));
LIR.PrintString ((source));
LIR.StdoutWrite ((3), (operand), (true));
LIR.StdinReadLine ((3), (regs[0]));
LIR.RuntimeError ((source));
LIR.RuntimeErrorString ((regs[0]));
LIR.PrintHeapStringNoNewline ((regs[0]));
LIR.PrintChars ([0uy;127uy;255uy]);
LIR.PrintBlob ((regs[0]));
LIR.PrintList ((regs[0]), (typ));
LIR.PrintSum ((regs[0]), [((source), (3), (Some ((typ)))); ((source), (3), (Some ((typ))))], (true));
LIR.PrintRecord ((regs[0]), (source), [((source), (typ)); ((source), (typ))]);
LIR.Exit;
LIR.FPhi ((fregs[0]), [((freg), (LIR.Label source)); ((freg), (LIR.Label source))]);
LIR.FMov ((fregs[0]), (fregs[1]));
LIR.FLoad ((fregs[0]), (-0.0));
LIR.FSpillLoad ((fregs[0]), (3));
LIR.FSpillStore ((3), (fregs[0]));
LIR.FAdd ((fregs[0]), (fregs[1]), (fregs[2]));
LIR.FSub ((fregs[0]), (fregs[1]), (fregs[2]));
LIR.FMul ((fregs[0]), (fregs[1]), (fregs[2]));
LIR.FMadd ((fregs[0]), (fregs[1]), (fregs[2]), (fregs[3]));
LIR.FDiv ((fregs[0]), (fregs[1]), (fregs[2]));
LIR.FNeg ((fregs[0]), (fregs[1]));
LIR.FAbs ((fregs[0]), (fregs[1]));
LIR.FSqrt ((fregs[0]), (fregs[1]));
LIR.FCmp ((fregs[0]), (fregs[1]));
LIR.Int64ToFloat ((fregs[0]), (regs[0]));
LIR.FloatToInt64 ((regs[0]), (fregs[0]));
LIR.FloatToBits ((regs[0]), (fregs[0]));
LIR.GpToFp ((fregs[0]), (regs[0]));
LIR.FpToGp ((regs[0]), (fregs[0]));
LIR.HeapAlloc ((regs[0]), (3));
LIR.HeapStore ((regs[0]), (3), (operand), (Some ((typ))));
LIR.HeapLoad ((regs[0]), (regs[1]), (3));
LIR.RefCountInc ((regs[0]), (3), LIR.GenericHeap, (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey=Some source;ReleasePlan=Some (MemoryModel.RecursiveRelease (AST.TList AST.TString));SourceType=Some AST.TString}))));
LIR.RefCountDec ((regs[0]), (3), LIR.GenericHeap, (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey=Some source;ReleasePlan=Some (MemoryModel.RecursiveRelease (AST.TList AST.TString));SourceType=Some AST.TString}))));
LIR.StringConcat ((regs[0]), (operand), (operand), [(operand); (operand)]);
LIR.CanonicalBufferEq ((regs[0]), (MemoryModel.NullableGraphemeCluster), (operand), (operand));
LIR.PrintHeapString ((regs[0]));
LIR.LoadFuncAddr ((regs[0]), (AST.functionId System.UInt64.MaxValue));
LIR.FileReadBlob ((regs[0]), (operand));
LIR.FileExists ((regs[0]), (operand));
LIR.FileWriteBlob ((regs[0]), (operand), (operand));
LIR.FileAppendText ((regs[0]), (operand), (operand));
LIR.FileDelete ((regs[0]), (operand));
LIR.FileCreateDirectory ((regs[0]), (operand));
LIR.FileSetExecutable ((regs[0]), (operand));
LIR.FileWriteFromPtr ((regs[0]), (operand), (regs[1]), (regs[2]));
LIR.RawAlloc ((regs[0]), (regs[1]));
LIR.MappedAlloc ((regs[0]), (regs[1]));
LIR.RawFree ((regs[0]));
LIR.MappedFree ((regs[0]));
LIR.RawGet ((regs[0]), (regs[1]), (regs[2]));
LIR.RawGetByte ((regs[0]), (regs[1]), (regs[2]));
LIR.RawWriteWord ((regs[0]), (regs[1]), (regs[2]));
LIR.RawWriteByte ((regs[0]), (regs[1]), (regs[2]));
LIR.RawSlotInit ((regs[0]), (regs[1]), (regs[2]), (typ));
LIR.RefCountIncString ((operand));
LIR.RefCountDecString ((operand));
LIR.RefCountIncBlob ((operand));
LIR.RefCountDecBlob ((operand));
LIR.RefCountIncInt ((operand));
LIR.RefCountDecInt ((operand));
LIR.RandomInt64 ((regs[0]));
LIR.DateTimeNow ((regs[0]));
LIR.Sleep ((3), (fregs[0]));
LIR.CliNative ((regs[0]), LIR.Execute, [(operand); (operand)]);
LIR.FloatToString ((regs[0]), (fregs[0]));
LIR.CoverageHit ((3))]

let allocationObservation (source:string) =
    let enc (value:'a) = encode typeof<'a> (box value)
    let raw (value:obj) = encode (value.GetType()) value
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let call name args : 'a = rcInternalCall<'a> "AllocationModel" name args
    let liveness name args : 'a = rcInternalCall<'a> "RegisterLiveness" name args
    let facts name args : 'a = rcInternalCall<'a> "RegisterFacts" name args
    let interference name args : 'a = rcInternalCall<'a> "RegisterInterference" name args
    let attempt action =
        let node=JsonObject()
        node["type"] <- JsonValue.Create "FSharpResult"
        try
            node["case"] <- JsonValue.Create "Ok"
            node["fields"] <- JsonArray([|action ()|])
        with ex ->
            node["case"] <- JsonValue.Create "Error"
            node["fields"] <- JsonArray([|enc ex.Message|])
        node :> JsonNode
    let block label instructions terminator : LIR.BasicBlock = {Label=label;Instrs=instructions;Terminator=terminator}
    let cfg entry blocks : LIR.CFG = {Entry=entry;Blocks=blocks |> List.map (fun (b:LIR.BasicBlock) -> b.Label,b) |> Map.ofList}
    let label=LIR.Label source
    let other=LIR.Label "other"
    let missing=LIR.Label "missing"
    let regs=[LIR.Virtual 3;LIR.Virtual (-9);LIR.Physical LIR.X0]
    let fregs=[LIR.FVirtual (-1);LIR.FVirtual 7;LIR.FVirtual (-1000);LIR.FVirtual (-1001);LIR.FVirtual (-1002);LIR.FVirtual (-2000);LIR.FPhysical LIR.D0]
    let instructionCases=regs |> List.collect (fun reg -> fregs |> List.collect (fun freg -> [LIR.Reg reg;LIR.Imm 0L;LIR.FuncAddr (AST.functionId UInt64.MaxValue)] |> List.collect (fun operand -> [AST.TInt64;AST.TFloat64] |> List.collect (fun typ ->
        lirInstructionFixturesWithRegisters source reg freg operand typ |> List.map (fun instr ->
            let b=block label [instr] LIR.Ret
            tuple [enc instr;enc (facts "getUsedVRegs" [|box instr|] : int list);enc (facts "getDefinedVReg" [|box instr|] : int option);enc (facts "getUsedFVRegs" [|box instr|] : int list);enc (facts "getDefinedFVReg" [|box instr|] : int option);raw (facts "classifyBlocks" [|box [|b|]|] : obj)])))))
    let terminatorCases=regs |> List.collect (fun reg -> [LIR.Ret;LIR.Jump other;LIR.Branch (reg,label,other);LIR.BranchZero (reg,label,other);LIR.BranchBitZero (reg,3,label,other);LIR.BranchBitNonZero (reg,3,label,other);LIR.CondBranch (LIR.EQ,label,other)] |> List.map (fun term ->
        tuple [enc term;enc (facts "getTerminatorUsedVRegs" [|box term|] : int list);enc (RegisterLiveness.getSuccessors term)]))
    let idCases=[[];[3];[-9;0;3;3;-9];List.init 63 id;List.init 64 id;List.init 65 id;List.init 129 (fun n -> n*2-129)]
    let domains=idCases |> List.map (fun ids ->
        let d:AllocationModel.VRegDomain=call "buildVRegDomain" [|box ids|]
        let selected=ids |> List.indexed |> List.choose (fun (n,value) -> if n%2=0 then Some value else None)
        let b:AllocationModel.BitSet=call "vregBitsFromList" [|box d;box selected|]
        let before=enc b
        let lookups=(ids @ [Int32.MinValue;Int32.MaxValue;987]) |> List.map (fun value -> tuple [enc value;enc (call "tryIndexOf" [|box d;box value|] : int option);enc (call "vregBitsContains" [|box d;box b;box value|] : bool)])
        let mutations=(ids @ [987]) |> List.map (fun value ->
            let bits=Array.copy b
            let add=attempt (fun () -> call "vregBitsAddInPlace" [|box d;box value;box bits|] |> fun (value:unit) -> enc value)
            let afterAdd=enc bits
            let remove=attempt (fun () -> call "vregBitsRemoveInPlace" [|box d;box value;box bits|] |> fun (value:unit) -> enc value)
            tuple [add;afterAdd;remove;enc bits])
        tuple [enc d;before;list lookups;list mutations;attempt (fun () -> enc (call "vregBitsFromList" [|box d;box [987]|] : AllocationModel.BitSet))])
    let accumulatorType=typeof<AllocationModel.VRegDomain>.Assembly.GetType("AllocationModel+BitSetUnionAccumulator")
    let accumulatorCases=FSharpType.GetUnionCases(accumulatorType,Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
    let unions=[0;1;2;3] |> List.map (fun size ->
        let empty=Bitset.empty size
        let a=Bitset.empty size
        let b=Bitset.empty size
        if size>0 then
            Bitset.addIndexInPlace 0 a
            Bitset.addIndexInPlace (size*64-1) b
        let mutable current=FSharpValue.MakeUnion(accumulatorCases |> Array.find (fun c -> c.Name="NoUnionBits"),[||],Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
        let steps=[empty;a;empty;b;a] |> List.map (fun input ->
            current <- call "bitsetAccumulateUnion" [|current;box input|]
            let finished:AllocationModel.BitSet=call "bitsetFinishUnion" [|box empty;current|]
            tuple [encode accumulatorType current;enc finished;enc (size>0 && obj.ReferenceEquals(finished,empty));enc (size>0 && obj.ReferenceEquals(finished,a));enc (size>0 && obj.ReferenceEquals(finished,b))])
        tuple [list steps;enc empty;enc a;enc b])
    let graphs=[[];[-9;0;3];List.init 65 id;List.init 129 id] |> List.collect (fun ids ->
        let pairs=List.zip ids (match ids with [] -> [] | first::rest -> rest @ [first])
        [[];pairs;pairs @ pairs;ids |> List.collect (fun a -> ids |> List.map (fun b -> a,b));[987,987];[987,3]] |> List.map (fun edges -> attempt (fun () ->
            let g=AllocationModel.buildInterferenceGraphFromEdges ids edges
            let d=g.Domain
            let count=d.Ids.Length
            let spillIds=d.Ids |> Array.indexed |> Array.choose (fun (n,value) -> if n%3=0 then Some value else None) |> Array.toList
            let result:AllocationModel.ColoringResult={Domain=d;Colors=Array.init count (fun n -> if n%3=0 then None else Some (n%4));Spills=call "vregBitsFromList" [|box d;box spillIds|];ChromaticNumber=4}
            tuple [enc g;ids @ [987] |> List.map (fun value -> tuple [enc value;enc (AllocationModel.graphHasVertex g value);enc (AllocationModel.graphNeighbors g value);enc (AllocationModel.colorOf result value);enc (AllocationModel.isSpill result value)]) |> list;enc result;enc (AllocationModel.spillCount result);enc (AllocationModel.coloredCount result)])))
    let instructionCFGs=lirInstructionFixtures source |> List.map (fun instr -> cfg label [block label [instr] (LIR.Jump other);block other [LIR.Add (LIR.Virtual 9,LIR.Virtual 3,LIR.Reg (LIR.Virtual (-9)));LIR.FAdd (LIR.FVirtual 9,LIR.FVirtual 7,LIR.FVirtual (-1))] LIR.Ret])
    let patterns=[cfg label [block label [] LIR.Ret];cfg label [block label [] (LIR.Jump missing)];cfg missing [block label [] LIR.Ret];
        cfg label [block label [LIR.Mov (LIR.Virtual 1,LIR.Reg (LIR.Virtual 2))] (LIR.BranchZero (LIR.Virtual 1,label,other));block other [LIR.Phi (LIR.Virtual 3,[LIR.Reg (LIR.Virtual 1),label;LIR.Reg (LIR.Virtual 2),missing],None);LIR.FPhi (LIR.FVirtual 7,[LIR.FVirtual (-1),label;LIR.FVirtual 8,missing])] (LIR.Jump label)];
        cfg label [block label [] (LIR.CondBranch (LIR.EQ,other,other));block other [LIR.Phi (LIR.Virtual 3,[LIR.Reg (LIR.Virtual 1),label;LIR.Reg (LIR.Virtual 2),label],None);LIR.FPhi (LIR.FVirtual 7,[LIR.FVirtual (-1000),label])] LIR.Ret];
        cfg label [block label (List.init 129 (fun n -> LIR.Mov (LIR.Virtual (n*2-129),LIR.Reg (LIR.Virtual (n*2-127))))) (LIR.Jump other);block other (List.init 65 (fun n -> LIR.FMov (LIR.FVirtual n,LIR.FVirtual (n+1)))) (LIR.BranchZero (LIR.Virtual 1,label,other))]]
    let cfgCases=instructionCFGs @ patterns |> List.collect (fun cfg -> [[];[-9;3;7;129]] |> List.map (fun extra -> attempt (fun () ->
        let idx,blocks:AllocationModel.BlockIndex * LIR.BasicBlock array=call "buildBlockIndex" [|box cfg|]
        let classified:obj=facts "classifyBlocks" [|box blocks|]
        let id,il,fd,fl:AllocationModel.VRegDomain * AllocationModel.BlockLiveness array * AllocationModel.VRegDomain * AllocationModel.BlockLiveness array=liveness "computeCombinedLivenessBitsFromFacts" [|box idx;classified;box extra;box extra|]
        tuple [enc idx;enc blocks;raw classified;
            enc (call "blocksToMap" [|box idx;box blocks|] : Map<LIR.Label,LIR.BasicBlock>);
            enc (id,il,fd,fl);
            enc (liveness "computeLivenessBitsFromFacts" [|box idx;classified;box extra|] : AllocationModel.VRegDomain * AllocationModel.BlockLiveness array);
            enc (liveness "computeFloatLivenessBitsFromFacts" [|box idx;classified;box extra|] : AllocationModel.VRegDomain * AllocationModel.BlockLiveness array);
            enc (RegisterLiveness.computeLivenessBits cfg);
            enc (RegisterLiveness.computeFloatLivenessBits cfg);
            (blocks |> Array.map (fun b -> enc (RegisterLiveness.computeGenKill id b,RegisterLiveness.computeFloatGenKill fd b)) |> fun values -> JsonArray(values) :> JsonNode);
            enc (interference "buildInterferenceGraphBitsetWithLiveness" [|box idx;classified;box id;box il;box (call "vregBitsFromList" [|box id;box extra|] : AllocationModel.BitSet)|] : AllocationModel.InterferenceGraph);
            enc (interference "buildFloatInterferenceGraphBitsetWithLiveness" [|box idx;classified;box fd;box fl;box (call "vregBitsFromList" [|box fd;box extra|] : AllocationModel.BitSet)|] : AllocationModel.InterferenceGraph);
            enc (RegisterInterference.buildInterferenceGraphBitsetFast cfg extra);enc (RegisterInterference.buildInterferenceGraphBitset cfg extra);
            [label;other;missing] |> List.map (fun label -> enc ((call "tryBlockIndex" [|box idx;box label|] : int option),(call "blockIndexOfLabel" [|box idx;box label|] : int option),(call "blockLivenessForLabel" [|box idx;box il;box label|] : AllocationModel.BlockLiveness option))) |> list])))
    let saveCases=[[];[LIR.SaveRegs ([],[]);LIR.Mov (LIR.Virtual 1,LIR.Reg (LIR.Virtual 3));LIR.RestoreRegs ([],[]);LIR.FAdd (LIR.FVirtual 9,LIR.FVirtual 7,LIR.FVirtual (-1))];[LIR.SaveRegs ([],[]);LIR.SaveRegs ([],[]);LIR.RestoreRegs ([],[]);LIR.RestoreRegs ([],[])];[LIR.SaveRegs ([LIR.X0],[LIR.D0]);LIR.RestoreRegs ([LIR.X0],[LIR.D0])];[LIR.SaveRegs ([],[])];[LIR.RestoreRegs ([],[])]] |> List.map (fun instructions ->
        let b=block label instructions (LIR.BranchZero (LIR.Virtual 3,label,other))
        let classified:System.Array=facts "classifyBlocks" [|box [|b|]|]
        let id,il,fd,fl:AllocationModel.VRegDomain * AllocationModel.BlockLiveness array * AllocationModel.VRegDomain * AllocationModel.BlockLiveness array=liveness "computeCombinedLivenessBitsFromFacts" [|box ({Labels=[|label|];EntryIndex=0}:AllocationModel.BlockIndex);box classified;box [1;3;9];box [-1;7;9]|]
        let first=classified.GetValue 0
        let instrFacts=first.GetType().GetProperty("InstrFacts",Reflection.BindingFlags.Instance ||| Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic).GetValue(first)
        tuple [enc instructions;instructions |> List.map (fun instr -> enc (liveness "isEmptySaveRegs" [|box instr|] : bool)) |> list;attempt (fun () -> enc (liveness "computeSaveRegsPreparation" [|box id;box fd;box b;instrFacts;box il[0].LiveOut;box fl[0].LiveOut|] : (AllocationModel.BitSet * AllocationModel.BitSet) list))])
    tuple [list instructionCases;list terminatorCases;list domains;list unions;list graphs;list cfgCases;list saveCases]

let coloringObservation (source:string) =
    let enc (value:'a) = encode typeof<'a> (box value)
    let raw (value:obj) = encode (value.GetType()) value
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let coalesce name args : 'a = rcInternalCall<'a> "RegisterCoalescing" name args
    let color name args : 'a = rcInternalCall<'a> "RegisterColoring" name args
    let field name (value:obj) = value.GetType().GetProperty(name,Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Instance).GetValue(value)
    let attempt action =
        let node=JsonObject()
        node["type"] <- JsonValue.Create "FSharpResult"
        try
            node["case"] <- JsonValue.Create "Ok"
            node["fields"] <- JsonArray([|action ()|])
        with ex ->
            node["case"] <- JsonValue.Create "Error"
            node["fields"] <- JsonArray([|enc ex.Message|])
        node :> JsonNode
    let regs=[LIR.Virtual 3;LIR.Virtual (-9);LIR.Physical LIR.X0]
    let fregs=[LIR.FVirtual 7;LIR.FPhysical LIR.D0]
    let collectors=regs |> List.collect (fun reg -> fregs |> List.collect (fun freg ->
        lirInstructionFixturesWithRegisters source reg freg (LIR.Reg reg) AST.TFloat64 |> List.map (fun instr ->
            let blocks=[|({Label=LIR.Label source;Instrs=[instr];Terminator=LIR.Ret}:LIR.BasicBlock)|]
            tuple [enc instr;enc (RegisterCoalescing.collectMovePairs blocks);enc (RegisterCoalescing.collectPhiPairs blocks);enc (RegisterCoalescing.collectFPhiPairs blocks);enc (RegisterCoalescing.collectFPhiSourceMovePairs blocks);enc (RegisterCoalescing.collectPhiPreferences blocks)])))
    let chain=[|({Label=LIR.Label source;Instrs=[LIR.FMov (LIR.FVirtual 1,LIR.FVirtual 2);LIR.FMov (LIR.FVirtual 2,LIR.FVirtual 3);LIR.FPhi (LIR.FVirtual 7,[LIR.FVirtual 1,LIR.Label source;LIR.FVirtual 2,LIR.Label source]);LIR.Mov (LIR.Virtual 1,LIR.Reg (LIR.Virtual 1));LIR.Phi (LIR.Virtual 7,[LIR.Reg (LIR.Virtual 1),LIR.Label source;LIR.Reg (LIR.Virtual 7),LIR.Label source;LIR.Imm 3L,LIR.Label source],None)];Terminator=LIR.Ret}:LIR.BasicBlock)|]
    let chains=tuple [enc (RegisterCoalescing.collectMovePairs chain);enc (RegisterCoalescing.collectPhiPairs chain);enc (RegisterCoalescing.collectFPhiPairs chain);enc (RegisterCoalescing.collectFPhiSourceMovePairs chain);enc (RegisterCoalescing.collectPhiPreferences chain);enc (coalesce "dedupePairs" [|box [3,1;1,3;0,0;-9,3;3,-9;7,1]|] : (int*int) list)]
    let ids=[-9;0;3;7]
    let allEdges=[-9,0;-9,3;-9,7;0,3;0,7;3,7]
    let smallGraphs=List.init 64 (fun mask -> AllocationModel.buildInterferenceGraphFromEdges ids (allEdges |> List.indexed |> List.choose (fun (n,edge) -> if mask &&& (1 <<< n)<>0 then Some edge else None)))
    let largeGraphs=[65;129] |> List.collect (fun size ->
        let ids=List.init size (fun n -> n*2-129)
        let edges=List.zip (ids |> List.take (size-1)) (List.tail ids) |> List.indexed |> List.choose (fun (n,edge) -> if n%4<>0 then Some edge else None)
        let graph=AllocationModel.buildInterferenceGraphFromEdges ids edges
        let vertices=Bitset.empty graph.Domain.WordCount
        graph.Domain.Ids |> Array.iteri (fun n _ -> if n%2=0 then Bitset.addIndexInPlace n vertices)
        [graph;{graph with Vertices=vertices}])
    let inactive={List.head smallGraphs with Vertices=Bitset.empty 1}
    let graphs=AllocationModel.buildInterferenceGraphFromEdges [] [] :: inactive :: smallGraphs @ largeGraphs
    let cases=graphs |> List.map (fun g ->
        let order,p=RegisterCoalescing.maximumCardinalitySearchWithProfile g
        let variants=[[];[-9,0;7,1];[-9,0;0,1;3,0;987,2];[-9,3;0,0;3,2;7,1];[-9,-1;3,Int32.MaxValue]] |> List.collect (fun precolors -> [[];[-9,3;0,7];[-9,0;0,3;3,7;987,0]] |> List.collect (fun movePairs -> [[];[-9,3;0,7];[-9,0;0,3;3,7;987,0]] |> List.collect (fun prefs -> [0;1;2;4] |> List.map (fun colors ->
            let c:obj=coalesce "coalesceGraphFast" [|box g;box precolors;box movePairs;box prefs|]
            let repGraph=field "Graph" c :?> AllocationModel.InterferenceGraph
            let members=field "RepMembers" c :?> AllocationModel.BitSet array
            let synthetic:AllocationModel.ColoringResult={Domain=g.Domain;Colors=Array.init g.Domain.Ids.Length (fun n -> if n%3=0 then None else Some (n%4));Spills=Array.copy repGraph.Vertices;ChromaticNumber=4}
            let result=RegisterColoring.chordalGraphColor g precolors colors prefs movePairs
            let sw=Diagnostics.Stopwatch.StartNew()
            let timed:obj=color "chordalGraphColorWithTiming" [|box sw;box g;box precolors;box colors;box prefs;box movePairs|]
            let fields=FSharpValue.GetTupleFields timed
            let timing=fields[1]
            let coalesceMs=field "CoalesceMs" timing :?> float
            let mcsMs=field "McsMs" timing :?> float
            let greedyMs=field "GreedyMs" timing :?> float
            let expandMs=field "ExpandMs" timing :?> float
            let timing=tuple [enc (fields[0] :?> AllocationModel.ColoringResult);enc (coalesceMs>=0. && mcsMs>=0. && greedyMs>=0. && expandMs>=0.);enc (if Bitset.isEmpty g.Vertices then coalesceMs=0. && mcsMs=0. && greedyMs=0. && expandMs=0. else true);enc (if List.isEmpty movePairs && List.isEmpty prefs then expandMs=0. else true)]
            tuple [raw c;enc (coalesce "expandColoring" [|box synthetic;box members|] : AllocationModel.ColoringResult);enc result;timing;
                [[];[LIR.X0];[LIR.X19;LIR.X20;LIR.X0];[LIR.X26;LIR.X25;LIR.X24;LIR.X23;LIR.X22;LIR.X21;LIR.X20;LIR.X19]] |> List.map (fun registers -> attempt (fun () -> enc (RegisterColoring.coloringToAllocation result registers))) |> list]))))
        let precolored=Array.init g.Domain.Ids.Length (fun n -> if n%3=0 then Some (n%4) else None)
        let preferences=Array.init g.Domain.Ids.Length (fun n ->
            let bits=Bitset.empty g.Domain.WordCount
            if n>0 then Bitset.addIndexInPlace (n-1) bits
            bits)
        let greedy=[order;List.rev order;order @ order;987::order] |> List.collect (fun ordering -> [0;1;2;4] |> List.map (fun colors -> attempt (fun () -> enc (RegisterColoring.greedyColorReverse g ordering precolored colors preferences))))
        tuple [enc g;enc order;enc p;enc (RegisterCoalescing.maximumCardinalitySearch g);list variants;list greedy])
    tuple [list collectors;chains;list cases]

let floatAllocationObservation (source:string) =
    let enc (value:'a) = encode typeof<'a> (box value)
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let attempt action = enc (try Ok (action ()) with ex -> Error ex.Message)
    let block label instrs terminator : LIR.BasicBlock = {Label=label;Instrs=instrs;Terminator=terminator}
    let cfg entry blocks : LIR.CFG = {Entry=entry;Blocks=blocks |> List.map (fun (b:LIR.BasicBlock) -> b.Label,b) |> Map.ofList}
    let label=LIR.Label source
    let other=LIR.Label "other"
    let values=[Int64.MinValue;0x7ff8000000000001L;0x7ff0000000000000L;0xfff0000000000000L;0x3ff0000000000000L] |> List.map BitConverter.Int64BitsToDouble
    let ids=[-2000;-1002;-1001;-1000;-1;0;1;2;3;4;7;9;19]
    let d:AllocationModel.VRegDomain=rcInternalCall "AllocationModel" "buildVRegDomain" [|box ids|]
    let fregs=[LIR.FVirtual (-1);LIR.FVirtual 7;LIR.FVirtual (-1000);LIR.FVirtual (-1001);LIR.FVirtual (-1002);LIR.FVirtual (-2000);LIR.FVirtual 987;LIR.FPhysical LIR.D4]
    let repairCases=[0;1;2;3;4] |> List.collect (fun mode -> [0;1] |> List.map (fun scratch ->
        let allocations=d.Ids |> Array.mapi (fun n _ ->
            match mode with
            | 0 -> Some (FloatAllocation.FPhysReg (List.item (n%16) FloatAllocation.allocatableFloatRegs))
            | 1 -> Some (FloatAllocation.FStackSlot (-(n+1)*8))
            | 2 -> Some (FloatAllocation.FRematerialized (List.item (n%5) values))
            | 3 -> (match n%4 with 0 -> None | 1 -> Some (FloatAllocation.FPhysReg LIR.D0) | 2 -> Some (FloatAllocation.FStackSlot (-24)) | _ -> Some (FloatAllocation.FRematerialized (-0.)))
            | _ -> None)
        let allocation:FloatAllocation.FAllocationResult={Domain=d;Allocations=allocations;StackSize=48;UsedCalleeSavedF=[LIR.D8;LIR.D15];SpillScratchLeft=(if scratch=0 then LIR.FVirtual (-1000) else LIR.FPhysical LIR.D14);SpillScratchRight=(if scratch=0 then LIR.FVirtual (-1001) else LIR.FPhysical LIR.D15);SpillScratchThird=LIR.FVirtual (-1002)}
        let repairs=fregs |> List.collect (fun freg -> [AST.TInt64;AST.TFloat64] |> List.collect (fun typ ->
            lirInstructionFixturesWithRegisters source (LIR.Virtual 3) freg (LIR.Reg (LIR.Virtual 3)) typ |> List.map (fun instr ->
                let b=block label [instr] LIR.Ret
                let graph=cfg label [b]
                tuple [enc instr;attempt (fun () -> FloatAllocation.applyFloatAllocationToInstrs allocation instr);attempt (fun () -> FloatAllocation.applyFloatAllocationToBlock allocation b);attempt (fun () -> FloatAllocation.applyFloatAllocationToBlocks allocation [|b|]);attempt (fun () -> FloatAllocation.applyFloatAllocationToCFG allocation graph)])))
        let moves=[LIR.FArgMoves [LIR.D0,LIR.FPhysical LIR.D1;LIR.D1,LIR.FPhysical LIR.D0];LIR.FArgMoves [LIR.D0,LIR.FVirtual 1;LIR.D1,LIR.FVirtual 2;LIR.D2,LIR.FVirtual 3];LIR.FArgMoves [LIR.D0,LIR.FPhysical LIR.D1;LIR.D1,LIR.FVirtual 3];LIR.FArgMoves [];LIR.FPhi (LIR.FVirtual 987,[LIR.FVirtual 7,label])] |> List.map (fun instr -> tuple [enc instr;attempt (fun () -> FloatAllocation.applyFloatAllocationToInstrs allocation instr)])
        tuple [enc allocation;ids @ [987] |> List.map (fun value -> enc (value,FloatAllocation.tryFloatAllocation allocation value)) |> list;fregs |> List.map (fun freg -> attempt (fun () -> FloatAllocation.applyFloatAllocationToFReg allocation freg)) |> list;list repairs;list moves]))
    let schedules=[[];[LIR.FLoad (LIR.FVirtual 1,-0.);LIR.Mov (LIR.Virtual 3,LIR.Imm 1L);LIR.FLoad (LIR.FVirtual 2,1.);LIR.FAdd (LIR.FVirtual 3,LIR.FVirtual 2,LIR.FVirtual 1)];[LIR.FLoad (LIR.FVirtual 1,1.);LIR.FLoad (LIR.FVirtual 1,2.);LIR.PrintFloat (LIR.FVirtual 1)];[LIR.PrintFloat (LIR.FVirtual 1);LIR.FLoad (LIR.FVirtual 1,1.)];[LIR.FLoad (LIR.FVirtual 1,1.);LIR.FPhi (LIR.FVirtual 2,[LIR.FVirtual 1,label])];[LIR.FLoad (LIR.FVirtual 2,2.);LIR.FLoad (LIR.FVirtual 1,1.);LIR.FAdd (LIR.FVirtual 3,LIR.FVirtual 1,LIR.FVirtual 2)]]
    let scheduling=schedules |> List.map (fun instructions ->
        let b=block label instructions (LIR.Jump other)
        let graph=cfg label [b;block other [LIR.FPhi (LIR.FVirtual 7,[LIR.FVirtual 1,label])] LIR.Ret]
        enc (FloatAllocation.scheduleFloatLoadsInBlock b,FloatAllocation.scheduleFloatLoadsInCFG graph))
    let pressure literal count =
        let loads=List.init count (fun n -> if literal then LIR.FLoad (LIR.FVirtual n,List.item (n%5) values) else LIR.Int64ToFloat (LIR.FVirtual n,LIR.Virtual 3))
        let uses=List.init count (fun n -> LIR.PrintFloat (LIR.FVirtual n))
        cfg label [block label (loads @ uses) LIR.Ret]
    let fixtureCFGs=lirInstructionFixtures source |> List.map (fun instr -> cfg label [block label [instr] LIR.Ret])
    let cfgs=fixtureCFGs @ (schedules |> List.map (fun instructions -> cfg label [block label instructions LIR.Ret])) @ [pressure true 33;pressure false 33;pressure false 65;cfg label [block label [LIR.FLoad (LIR.FVirtual 1,1.);LIR.FMov (LIR.FVirtual 2,LIR.FVirtual 1)] (LIR.Jump other);block other [LIR.FPhi (LIR.FVirtual 3,[LIR.FVirtual 2,label;LIR.FVirtual 7,other]);LIR.PrintFloat (LIR.FVirtual 3)] (LIR.Jump label)]]
    let allocations=cfgs |> List.map (fun graph ->
        let scheduled=FloatAllocation.scheduleFloatLoadsInCFG graph
        let idx,blocks:AllocationModel.BlockIndex * LIR.BasicBlock array=rcInternalCall "AllocationModel" "buildBlockIndex" [|box scheduled|]
        let facts:obj=rcInternalCall "RegisterFacts" "classifyBlocks" [|box blocks|]
        let variants=[[];[1;3;7]] |> List.collect (fun extras ->
            let domain,liveness:AllocationModel.VRegDomain * AllocationModel.BlockLiveness array=rcInternalCall "RegisterLiveness" "computeFloatLivenessBitsFromFacts" [|box idx;facts;box extras|]
            [[];[LIR.D0];[LIR.D0;LIR.D1];FloatAllocation.allocatableFloatRegs;FloatAllocation.allocatableFloatRegsFor Platform.X86_64] |> List.collect (fun registers -> [0;8;24] |> List.collect (fun initial -> [[];[1,0;3,1;7,0];[1,1;3,0;7,1]] |> List.map (fun precolors ->
                let node=JsonObject()
                node["type"] <- JsonValue.Create "FSharpResult"
                try
                    let entryBits:AllocationModel.BitSet=rcInternalCall "AllocationModel" "vregBitsFromList" [|box domain;box extras|]
                    let allocation:FloatAllocation.FAllocationResult=rcInternalCall "FloatAllocation" "chordalFloatAllocationWithLiveness" [|box registers;box initial;box idx;box blocks;facts;box entryBits;box precolors;box domain;box liveness|]
                    node["case"] <- JsonValue.Create "Ok"
                    node["fields"] <- JsonArray([|tuple [enc allocation;attempt (fun () -> FloatAllocation.applyFloatAllocationToCFG allocation scheduled)]|])
                with ex ->
                    node["case"] <- JsonValue.Create "Error"
                    node["fields"] <- JsonArray([|enc ex.Message|])
                node :> JsonNode))))
        tuple [enc scheduled;list variants;attempt (fun () -> FloatAllocation.chordalFloatAllocation graph []);attempt (fun () -> FloatAllocation.chordalFloatAllocation graph [1;3;7])])
    tuple [enc FloatAllocation.floatCallerSavedRegs;enc FloatAllocation.floatCalleeSavedRegs;enc FloatAllocation.allocatableFloatRegs;
        [Platform.ARM64;Platform.X86_64] |> List.map (fun arch -> enc (FloatAllocation.allocatableFloatRegsFor arch,FloatAllocation.floatCallerSavedRegsFor arch)) |> list;enc (FloatAllocation.allocatableFloatRegs |> List.map FloatAllocation.physFPRegToInt);list repairCases;list scheduling;list allocations]

let spillObservation (source:string) =
    let enc (value:'a) = encode typeof<'a> (box value)
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let attempt action = enc (try Ok (action ()) with ex -> Error ex.Message)
    let internalCall name args : 'a = rcInternalCall<'a> "SpillOperands" name args
    let phys=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP]
    let ids=[-9;0;1;2;3;7;29]
    let d:AllocationModel.VRegDomain=rcInternalCall "AllocationModel" "buildVRegDomain" [|box ids|]
    let regs=(phys |> List.map LIR.Physical) @ ((ids @ [987;Int32.MinValue;Int32.MaxValue]) |> List.map LIR.Virtual)
    let pairRegs=((ids @ [987]) |> List.map LIR.Virtual) @ [LIR.Physical LIR.X0;LIR.Physical LIR.X8;LIR.Physical LIR.X12;LIR.Physical LIR.X19]
    let mappings=[0;1;2;3;4] |> List.map (fun mode ->
        let allocations=d.Ids |> Array.mapi (fun n _ ->
            match mode with
            | 0 -> Some (AllocationModel.PhysReg (List.item n [LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7]))
            | 1 -> Some (AllocationModel.StackSlot (-(n+1)*8))
            | 2 -> Some (if n%2=0 then AllocationModel.PhysReg LIR.X12 else AllocationModel.StackSlot (-24))
            | 3 -> (match n%3 with 0 -> None | 1 -> Some (AllocationModel.PhysReg (List.item (n*4) phys)) | _ -> Some (AllocationModel.StackSlot (-8)))
            | _ -> None)
        let mapping:AllocationModel.AllocationResult={Domain=d;Allocations=allocations;StackSize=64;UsedCalleeSaved=[]}
        let scalar=regs |> List.map (fun reg ->
            let operand=LIR.Reg reg
            tuple [enc reg;(match reg with LIR.Virtual id -> enc (internalCall "tryAllocation" [|box mapping;box id|] : AllocationModel.Allocation option) | LIR.Physical _ -> null);
                enc (SpillOperands.applyToReg mapping reg);enc (SpillOperands.applyToOperandNoLoad mapping operand);
                [LIR.X0;LIR.X12;LIR.X19] |> List.map (fun temp -> let op,loads=SpillOperands.applyToOperand mapping operand temp in enc (op,loads,SpillOperands.loadSpilled mapping reg temp)) |> list])
        let operands=[LIR.Imm Int64.MinValue;LIR.FloatImm (-0.);LIR.StringSymbol source;LIR.FloatSymbol (BitConverter.Int64BitsToDouble 0x7ff8000000000001L);LIR.StackSlot (-24);LIR.FuncAddr (AST.functionId UInt64.MaxValue)] |> List.map (fun op -> let allocated,loads=SpillOperands.applyToOperand mapping op LIR.X12 in enc (op,allocated,loads,SpillOperands.applyToOperandNoLoad mapping op))
        let live=List.init 128 (fun mask ->
            let bits=Bitset.empty d.WordCount
            for n in 0..6 do
                if mask &&& (1 <<< n)<>0 then Bitset.addIndexInPlace n bits
            enc (SpillOperands.getLiveCallerSavedRegs mapping bits))
        let pairs=[Platform.ARM64;Platform.X86_64] |> List.collect (fun arch -> pairRegs |> List.collect (fun left -> pairRegs |> List.collect (fun right -> ((phys |> List.map LIR.Physical) @ [LIR.Virtual 3;LIR.Virtual 987]) |> List.map (fun dest -> attempt (fun () -> internalCall "loadSpilledPair" [|box arch;box mapping;box left;box right;box dest|] : (LIR.Reg*LIR.Instr list)*(LIR.Reg*LIR.Instr list))))))
        tuple [list scalar;list operands;list live;list pairs])
    let candidates=[LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X19;LIR.X20;LIR.X21;LIR.X0;LIR.X1;LIR.X2]
    let exclusion=List.init 2048 (fun mask ->
        let excluded=candidates |> List.indexed |> List.choose (fun (n,reg) -> if mask &&& (1 <<< n)<>0 then Some (LIR.Physical reg) else None)
        attempt (fun () -> internalCall "x86SpillTempExcluding" [|box (LIR.Virtual 3 :: excluded @ excluded)|] : LIR.PhysReg))
    let fd:AllocationModel.VRegDomain=rcInternalCall "AllocationModel" "buildVRegDomain" [|box (List.init 16 id)|]
    let floatSaved=[0;1;2;3;4] |> List.collect (fun mode ->
        let allocations=Array.init 16 (fun n -> match mode with 0 -> Some (FloatAllocation.FPhysReg (List.item n FloatAllocation.allocatableFloatRegs)) | 1 -> Some (FloatAllocation.FStackSlot (-8)) | 2 -> Some (FloatAllocation.FRematerialized (-0.)) | 3 -> (if n%2=0 then Some (FloatAllocation.FPhysReg (List.item (15-n) FloatAllocation.allocatableFloatRegs)) else None) | _ -> None)
        let allocation:FloatAllocation.FAllocationResult={Domain=fd;Allocations=allocations;StackSize=0;UsedCalleeSavedF=[];SpillScratchLeft=LIR.FVirtual (-1000);SpillScratchRight=LIR.FVirtual (-1001);SpillScratchThird=LIR.FVirtual (-1002)}
        0::65535::21845::43690::List.init 16 (fun n -> 1 <<< n) |> List.map (fun mask ->
            let bits=Bitset.empty 1
            for n in 0..15 do
                if mask &&& (1 <<< n)<>0 then Bitset.addIndexInPlace n bits
            [Platform.ARM64;Platform.X86_64] |> List.map (fun arch -> enc (SpillOperands.getLiveCallerSavedFloatRegs arch bits allocation)) |> list))
    tuple [list mappings;phys |> List.map (fun reg -> enc (internalCall "aliasesX86ScratchReg" [|box reg|] : bool)) |> list;list exclusion;list floatSaved;[Platform.ARM64;Platform.X86_64] |> List.map (fun arch -> enc (internalCall "isX86_64" [|box arch|] : bool)) |> list]

let phiObservation (source:string) =
    let enc (value:'a) = encode typeof<'a> (box value)
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let attempt action = enc (try Ok (action ()) with ex -> Error ex.Message)
    let block label instrs terminator : LIR.BasicBlock = {Label=label;Instrs=instrs;Terminator=terminator}
    let cfg entry blocks : LIR.CFG = {Entry=entry;Blocks=blocks |> List.map (fun (b:LIR.BasicBlock) -> b.Label,b) |> Map.ofList}
    let left=LIR.Label "left"
    let right=LIR.Label "right"
    let merge=LIR.Label source
    let missing=LIR.Label "missing"
    let vr id=LIR.Reg (LIR.Virtual id)
    let intMapping (domain:AllocationModel.VRegDomain) mode : AllocationModel.AllocationResult =
        let allocations=domain.Ids |> Array.mapi (fun n _ -> match mode with 0 -> Some (AllocationModel.PhysReg (List.item (n%8) [LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7])) | 1 -> Some (AllocationModel.StackSlot (-(n+1)*8)) | 2 -> Some (if n%2=0 then AllocationModel.PhysReg LIR.X3 else AllocationModel.StackSlot (-(n+1)*8)) | _ -> None)
        {Domain=domain;Allocations=allocations;StackSize=64;UsedCalleeSaved=[]}
    let floatMapping (domain:AllocationModel.VRegDomain) mode scratch : FloatAllocation.FAllocationResult =
        let allocations=domain.Ids |> Array.mapi (fun n _ -> match mode with 0 -> Some (FloatAllocation.FPhysReg (List.item (n%16) FloatAllocation.allocatableFloatRegs)) | 1 -> Some (FloatAllocation.FStackSlot (-(n+1)*8)) | 2 -> Some (FloatAllocation.FRematerialized (BitConverter.Int64BitsToDouble (if n%2=0 then Int64.MinValue else 0x7ff8000000000001L))) | 3 -> (match n%3 with 0 -> Some (FloatAllocation.FPhysReg LIR.D0) | 1 -> Some (FloatAllocation.FStackSlot (-24)) | _ -> None) | _ -> None)
        {Domain=domain;Allocations=allocations;StackSize=64;UsedCalleeSavedF=[];SpillScratchLeft=(if scratch=0 then LIR.FVirtual (-1000) else LIR.FPhysical LIR.D14);SpillScratchRight=(if scratch=0 then LIR.FVirtual (-1001) else LIR.FPhysical LIR.D15);SpillScratchThird=LIR.FVirtual (-1002)}
    let domain:AllocationModel.VRegDomain=rcInternalCall "AllocationModel" "buildVRegDomain" [|box [-9;0;1;2;3;4;5;7]|]
    let fregs=[LIR.FVirtual 1;LIR.FVirtual 2;LIR.FVirtual 7;LIR.FVirtual (-1);LIR.FVirtual 987;LIR.FPhysical LIR.D0;LIR.FPhysical LIR.D1]
    let moveLists=(fregs |> List.collect (fun dest -> fregs |> List.map (fun src -> [dest,src]))) @ [[];[LIR.FVirtual 1,LIR.FVirtual 2;LIR.FVirtual 2,LIR.FVirtual 1];[LIR.FVirtual 1,LIR.FVirtual 2;LIR.FVirtual 2,LIR.FVirtual 3;LIR.FVirtual 3,LIR.FVirtual 1];[LIR.FPhysical LIR.D0,LIR.FPhysical LIR.D1;LIR.FPhysical LIR.D1,LIR.FPhysical LIR.D0];[LIR.FVirtual 1,LIR.FVirtual 2;LIR.FVirtual 2,LIR.FVirtual 7]]
    let moves=[0;1;2;3;4] |> List.collect (fun mode -> [0;1] |> List.map (fun scratch ->
        let allocation=floatMapping domain mode scratch
        moveLists |> List.map (fun moves -> tuple [enc moves;attempt (fun () -> PhiResolution.generateFloatMoveInstrsWithAllocation moves allocation)]) |> list))
    let make instructions term = cfg left [block left [] (LIR.Jump merge);block right [] (LIR.Jump merge);block merge instructions term]
    let operands=[vr 2;LIR.Reg (LIR.Physical LIR.X0);LIR.Imm Int64.MinValue;LIR.StackSlot (-24);LIR.StringSymbol source;LIR.FloatImm (-0.);LIR.FloatSymbol (BitConverter.Int64BitsToDouble 0x7ff8000000000001L);LIR.FuncAddr (AST.functionId UInt64.MaxValue)]
    let operandCFGs=operands |> List.map (fun op -> make [LIR.Phi (LIR.Virtual 1,[op,left;vr 1,right;vr 7,missing],Some AST.TInt64);LIR.PrintInt64 (LIR.Virtual 1)] LIR.Ret)
    let patterns=[make [] LIR.Ret;
        make [LIR.Phi (LIR.Virtual 1,[vr 2,left;vr 1,right],None);LIR.Phi (LIR.Virtual 2,[vr 1,left;vr 2,right],None);LIR.PrintInt64 (LIR.Virtual 1)] LIR.Ret;
        make [LIR.Phi (LIR.Virtual 987,[vr 7,left],None)] LIR.Ret;
        make [LIR.Phi (LIR.Virtual 987,[vr 7,left],None)] (LIR.BranchZero (LIR.Virtual 987,left,right));
        make [LIR.Phi (LIR.Physical LIR.X0,[vr 1,left;vr 7,right],None);LIR.Phi (LIR.Virtual 1,[vr 2,left],None);LIR.Phi (LIR.Virtual 2,[vr 3,right],None)] LIR.Ret;
        make [LIR.Phi (LIR.Virtual 1,[vr 2,missing],None)] (LIR.BranchZero (LIR.Virtual 1,left,right));
        make [LIR.Phi (LIR.Virtual 1,[vr 2,left],None);LIR.Phi (LIR.Virtual 2,[vr 1,right],None)] LIR.Ret;
        make [LIR.FPhi (LIR.FVirtual 1,[LIR.FVirtual 2,left;LIR.FVirtual 1,right]);LIR.FPhi (LIR.FVirtual 2,[LIR.FVirtual 1,left;LIR.FVirtual 2,right])] LIR.Ret;
        make [LIR.FPhi (LIR.FPhysical LIR.D0,[LIR.FVirtual 7,left;LIR.FPhysical LIR.D1,right])] LIR.Ret;
        make [LIR.FPhi (LIR.FVirtual 987,[LIR.FVirtual 7,missing])] LIR.Ret;
        make [LIR.FPhi (LIR.FVirtual 987,[LIR.FVirtual 7,left])] LIR.Ret;
        cfg left [block left [LIR.FArgMoves [LIR.D0,LIR.FVirtual 1];LIR.TailCall (AST.functionId 3UL,[])] (LIR.Jump merge);block merge [LIR.FPhi (LIR.FVirtual 2,[LIR.FVirtual 1,left])] LIR.Ret]]
    let cfgCases=operandCFGs @ patterns |> List.map (fun graph ->
        let idx,blocks:AllocationModel.BlockIndex * LIR.BasicBlock array=rcInternalCall "AllocationModel" "buildBlockIndex" [|box graph|]
        let results=[0;1;2;3] |> List.collect (fun intMode -> [0;1;2;3;4] |> List.collect (fun floatMode -> [0;1] |> List.map (fun scratch -> attempt (fun () -> PhiResolution.resolvePhiNodes idx blocks (intMapping domain intMode) (floatMapping domain floatMode scratch)))))
        tuple [enc graph;list results])
    let chains=[2;65;129] |> List.map (fun count ->
        let domain:AllocationModel.VRegDomain=rcInternalCall "AllocationModel" "buildVRegDomain" [|box (List.init count id)|]
        let phis=List.init (count-1) (fun n -> LIR.Phi (LIR.Virtual n,[vr (n+1),left],None))
        let graph=make (phis @ [LIR.PrintInt64 (LIR.Virtual 0)]) LIR.Ret
        let idx,blocks:AllocationModel.BlockIndex * LIR.BasicBlock array=rcInternalCall "AllocationModel" "buildBlockIndex" [|box graph|]
        [0;1;2;3] |> List.map (fun mode -> attempt (fun () -> PhiResolution.resolvePhiNodes idx blocks (intMapping domain mode) (floatMapping domain 0 0))) |> list)
    tuple [list moves;list cfgCases;list chains]

let instructionAllocationObservation (source:string) =
    let enc (value:'a) = encode typeof<'a> (box value)
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let attempt action = enc (try Ok (action ()) with ex -> Error ex.Message)
    let d:AllocationModel.VRegDomain=rcInternalCall "AllocationModel" "buildVRegDomain" [|box [0;1;2;3]|]
    let regs=[|LIR.Virtual 0;LIR.Virtual 1;LIR.Virtual 2;LIR.Virtual 3|]
    let fregs=[|LIR.FVirtual 7;LIR.FPhysical LIR.D0;LIR.FVirtual (-1);LIR.FVirtual (-1000)|]
    let mapping mask : AllocationModel.AllocationResult =
        let allocations=Array.init 4 (fun n -> match (mask >>> (n*2)) &&& 3 with 0 -> Some (AllocationModel.PhysReg (List.item n [LIR.X19;LIR.X1;LIR.X2;LIR.X3])) | 1 -> Some (AllocationModel.PhysReg LIR.X12) | 2 -> Some (AllocationModel.StackSlot (-(n+1)*8)) | _ -> None)
        {Domain=d;Allocations=allocations;StackSize=32;UsedCalleeSaved=[]}
    let row arch mapping instr = tuple [enc instr;attempt (fun () -> ApplyRegisterAllocation.applyToInstr arch mapping instr)]
    let extras=[LIR.StringConcat (LIR.Virtual 0,LIR.Reg (LIR.Virtual 1),LIR.Reg (LIR.Virtual 2),[]);
        LIR.CanonicalBufferEq (LIR.Virtual 0,MemoryModel.NullableGraphemeCluster,LIR.Reg (LIR.Virtual 1),LIR.Reg (LIR.Virtual 2));
        LIR.CanonicalBufferEq (LIR.Virtual 0,MemoryModel.NullableGraphemeCluster,LIR.Reg (LIR.Virtual 1),LIR.Imm 0L);
        LIR.CanonicalBufferEq (LIR.Virtual 0,MemoryModel.NullableGraphemeCluster,LIR.Imm 0L,LIR.Reg (LIR.Virtual 2));
        LIR.CliNative (LIR.Virtual 0,LIR.Execute,[LIR.Reg (LIR.Virtual 0);LIR.Reg (LIR.Virtual 1);LIR.Reg (LIR.Virtual 2);LIR.Reg (LIR.Virtual 3)]);
        LIR.ArgMoves [LIR.X0,LIR.Reg (LIR.Virtual 1);LIR.X1,LIR.Reg (LIR.Virtual 0);LIR.X2,LIR.Reg (LIR.Virtual 2)];
        LIR.TailArgMoves [LIR.X0,LIR.Reg (LIR.Virtual 1);LIR.X1,LIR.Reg (LIR.Virtual 0);LIR.X2,LIR.Reg (LIR.Virtual 2)];
        LIR.Call (LIR.Virtual 0,AST.functionId UInt64.MaxValue,[LIR.Reg (LIR.Virtual 1);LIR.Reg (LIR.Virtual 2);LIR.Reg (LIR.Virtual 3)]);
        LIR.TailCall (AST.functionId UInt64.MaxValue,[LIR.Reg (LIR.Virtual 1);LIR.Reg (LIR.Virtual 2);LIR.Reg (LIR.Virtual 3)])]
    let allClasses=List.init 256 (fun mask ->
        let mapping=mapping mask
        [Platform.ARM64;Platform.X86_64] |> List.map (fun arch -> lirAllocationFixtures source regs fregs (LIR.Reg (LIR.Virtual 3)) AST.TInt64 @ extras |> List.map (row arch mapping) |> list) |> list)
    let operands=[LIR.Reg (LIR.Virtual 3);LIR.Imm Int64.MinValue;LIR.FloatImm (BitConverter.Int64BitsToDouble 0x7ff8000000000001L);LIR.FuncAddr (AST.functionId UInt64.MaxValue);LIR.StackSlot (-24);LIR.StringSymbol source]
    let registerRoles=[regs;[|LIR.Physical LIR.X0;LIR.Physical LIR.X1;LIR.Physical LIR.X2;LIR.Physical LIR.X3|];[|LIR.Physical LIR.X12;LIR.Physical LIR.X13;LIR.Physical LIR.X14;LIR.Physical LIR.X15|];[|LIR.Virtual 987;LIR.Virtual 1;LIR.Virtual 2;LIR.Virtual 3|];[|LIR.Virtual Int32.MinValue;LIR.Virtual Int32.MaxValue;LIR.Virtual (-9);LIR.Virtual 0|]]
    let varied=[0;85;170;255;27;228;42;99] |> List.collect (fun mask ->
        let mapping=mapping mask
        [Platform.ARM64;Platform.X86_64] |> List.collect (fun arch -> registerRoles |> List.collect (fun roles -> operands |> List.collect (fun operand -> [AST.TInt64;AST.TFloat64] |> List.map (fun typ -> lirAllocationFixtures source roles fregs operand typ |> List.map (row arch mapping) |> list)))))
    tuple [list allClasses;list varied]

let blockAllocationObservation (source:string) =
    let enc (value:'a) = encode typeof<'a> (box value)
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let attempt action = enc (try Ok (action ()) with ex -> Error ex.Message)
    let label=LIR.Label source
    let other=LIR.Label "other"
    let d:AllocationModel.VRegDomain=rcInternalCall "AllocationModel" "buildVRegDomain" [|box [0..15]|]
    let integer mode : AllocationModel.AllocationResult =
        {Domain=d;Allocations=Array.init 16 (fun n ->
            match mode with
            | 0 -> Some (AllocationModel.PhysReg (List.item (n%7) [LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7]))
            | 1 -> Some (AllocationModel.StackSlot (-(n+1)*8))
            | 2 -> (match n%3 with 0 -> None | 1 -> Some (AllocationModel.PhysReg LIR.X19) | _ -> Some (AllocationModel.StackSlot (-24)))
            | _ -> Some (AllocationModel.PhysReg (List.item n [LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26])));StackSize=128;UsedCalleeSaved=[]}
    let floating mode : FloatAllocation.FAllocationResult =
        {Domain=d;Allocations=Array.init 16 (fun n ->
            match mode with
            | 0 -> Some (FloatAllocation.FPhysReg (List.item n FloatAllocation.allocatableFloatRegs))
            | 1 -> Some (FloatAllocation.FStackSlot (-(n+1)*8))
            | 2 -> (match n%3 with 0 -> Some (FloatAllocation.FPhysReg LIR.D15) | 1 -> Some (FloatAllocation.FRematerialized (-0.0)) | _ -> None)
            | _ -> None);StackSize=256;UsedCalleeSavedF=[];SpillScratchLeft=LIR.FVirtual (-1000);SpillScratchRight=LIR.FVirtual (-1001);SpillScratchThird=LIR.FVirtual (-1002)}
    let live mask : AllocationModel.BitSet = [|uint64 mask|]
    let block label instrs term : LIR.BasicBlock = {Label=label;Instrs=instrs;Terminator=term}
    let fixtures=lirAllocationFixtures source [|LIR.Virtual 0;LIR.Virtual 1;LIR.Virtual 2;LIR.Virtual 3|] [|LIR.FVirtual 0;LIR.FVirtual 1;LIR.FVirtual 2;LIR.FVirtual 3|] (LIR.Reg (LIR.Virtual 3)) AST.TFloat64
    let instructions=(fixtures |> List.collect (fun instr -> [[instr];[LIR.SaveRegs ([],[]);instr;LIR.RestoreRegs ([],[]);LIR.Add (LIR.Virtual 5,LIR.Virtual 0,LIR.Reg (LIR.Virtual 1));LIR.FAdd (LIR.FVirtual 5,LIR.FVirtual 0,LIR.FVirtual 1)]])) @ [[];[LIR.SaveRegs ([],[]);LIR.SaveRegs ([],[]);LIR.Call (LIR.Virtual 0,AST.functionId 3UL,[]);LIR.RestoreRegs ([],[]);LIR.RestoreRegs ([],[]);LIR.PrintInt64 (LIR.Virtual 1);LIR.PrintFloat (LIR.FVirtual 1)];[LIR.SaveRegs ([],[])];[LIR.RestoreRegs ([],[])];[LIR.SaveRegs ([LIR.X3],[LIR.D3]);LIR.RestoreRegs ([],[])];[LIR.SaveRegs ([LIR.X3],[LIR.D3]);LIR.RestoreRegs ([LIR.X3],[LIR.D3])]]
    let arches=[Platform.ARM64;Platform.X86_64]
    let blockCases=[0..3] |> List.collect (fun im -> [0..3] |> List.collect (fun fm -> arches |> List.collect (fun arch -> [0;85;65535] |> List.map (fun mask -> instructions |> List.map (fun instrs -> attempt (fun () -> ApplyBlockAllocation.applyToBlockWithLiveness arch (integer im) (floating fm) (live mask) (live (mask ^^^ 65535)) (block label instrs (LIR.BranchZero (LIR.Virtual 3,label,other))))) |> list))))
    let terms=[LIR.Virtual 0;LIR.Virtual 3;LIR.Virtual 987;LIR.Physical LIR.X0;LIR.Physical LIR.X12;LIR.Physical LIR.SP] |> List.collect (fun reg -> [LIR.Ret;LIR.Jump other;LIR.Branch (reg,label,other);LIR.BranchZero (reg,label,other);LIR.BranchBitZero (reg,63,label,other);LIR.BranchBitNonZero (reg,63,label,other);LIR.CondBranch (LIR.NE,label,other)])
    let terminators=[0..3] |> List.map (fun mode -> terms |> List.map (fun term -> let loads,allocated=ApplyBlockAllocation.applyToTerminator (integer mode) term in tuple [enc term;enc loads;enc allocated]) |> list)
    let cfgCases=[0..3] |> List.collect (fun im -> [0..3] |> List.collect (fun fm -> [0;1;2] |> List.map (fun floatCount ->
        let blocks=[|block label [LIR.SaveRegs ([],[]);LIR.Call (LIR.Virtual 0,AST.functionId 3UL,[]);LIR.RestoreRegs ([],[]);LIR.PrintInt64 (LIR.Virtual 1);LIR.PrintFloat (LIR.FVirtual 1)] (LIR.Jump other);block other [LIR.FLoad (LIR.FVirtual 2,-0.0);LIR.SaveRegs ([],[]);LIR.RestoreRegs ([],[])] LIR.Ret|]
        let facts:obj=rcInternalCall "RegisterFacts" "classifyBlocks" [|box blocks|]
        let liveness:AllocationModel.BlockLiveness array=[|{LiveIn=live 85;LiveOut=live 65535};{LiveIn=live 170;LiveOut=live 85}|]
        let floatLiveness:AllocationModel.BlockLiveness array=Array.init floatCount (fun n -> {LiveIn=live 170;LiveOut=live (if n=0 then 43690 else 65535)})
        try
            let prep:obj=rcInternalCall "ApplyBlockAllocation" "prepareCFGAllocation" [|box blocks;box (integer im);box (floating fm);box liveness;box floatLiveness;facts|]
            let output=tuple [encode (prep.GetType()) prep;arches |> List.map (fun arch -> tuple [attempt (fun () -> rcInternalCall<LIR.BasicBlock array> "ApplyBlockAllocation" "applyPreparedCFGAllocation" [|box arch;box blocks;box (integer im);box (floating fm);prep|]);attempt (fun () -> ApplyBlockAllocation.applyToCFGWithLiveness arch blocks (integer im) (floating fm) liveness floatLiveness)]) |> list]
            let node=JsonObject()
            node["type"] <- JsonValue.Create "FSharpResult"
            node["case"] <- JsonValue.Create "Ok"
            node["fields"] <- JsonArray(output)
            node :> JsonNode
        with ex -> enc (Error ex.Message : Result<unit,string>))))
    let prepType=typeof<AST.SemanticType>.Assembly.GetType("ApplyBlockAllocation+BlockAllocationPreparation")
    let preparedCases=[[];[LIR.SaveRegs ([],[])];[LIR.RestoreRegs ([],[])];[LIR.SaveRegs ([],[]);LIR.RestoreRegs ([],[])];[LIR.SaveRegs ([],[]);LIR.SaveRegs ([],[]);LIR.RestoreRegs ([],[]);LIR.RestoreRegs ([],[])]] |> List.collect (fun instrs -> [0..3] |> List.collect (fun count -> arches |> List.map (fun arch ->
        let prep=FSharpValue.MakeRecord(prepType,[|box (List.init count (fun _ -> live 65535,live 65535))|],Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
        let preparations=System.Array.CreateInstance(prepType,1)
        preparations.SetValue(prep,0)
        attempt (fun () -> rcInternalCall<LIR.BasicBlock array> "ApplyBlockAllocation" "applyPreparedCFGAllocation" [|box arch;box [|block label instrs LIR.Ret|];box (integer 0);box (floating 0);box preparations|]))))
    tuple [list blockCases;list terminators;list cfgCases;list preparedCases]

let lirTreeObservation (source:string) =
    let enc (value:'a) = encode typeof<'a> (box value)
    let tuple values = namedArray "tuple" (Array.ofList values)
    let array values = JsonArray(Array.ofList values) :> JsonNode
    let attempt action = enc (try Ok (action ()) with e -> Error e.Message)
    let callGraph (value:FunctionIdMap<Set<AST.FunctionId>>) = namedArray "map" (FunctionIdMap.toList value |> List.map enc |> Array.ofList)
    let makeFunction id name instructions : LIR.Function =
        let label = LIR.Label name
        let block : LIR.BasicBlock = {Label=label;Instrs=instructions;Terminator=LIR.Ret}
        {Id=id;Name=name;TypedParams=[];CFG={Entry=label;Blocks=Map.ofList [label,block]};StackSize=0;UsedCalleeSaved=[];CodegenFacts=None}
    let types = [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TBool;AST.TString;AST.TChar;AST.TFloat64;AST.TUnit;AST.TTuple [AST.TInt64];AST.TList AST.TString;AST.TRecord (source,[])]
    let names = List.choose ListDisplay.getDisplayStringFunc types
    let ids = names |> List.mapi (fun i name -> name,AST.functionId (uint64 (i+10))) |> Map.ofList
    let extra = types |> List.map (fun typ -> LIR.PrintSum (LIR.Virtual 0,["C",0,Some (AST.TList typ);"D",1,Some AST.TInt64;"E",2,None],false))
    let operands = [LIR.FuncAddr (AST.functionId 1UL);LIR.FuncAddr (AST.functionId System.UInt64.MaxValue);LIR.Imm 0L;LIR.StringSymbol source;LIR.Reg (LIR.Virtual 3)]
    let instructionCases = operands |> List.collect (fun operand -> (lirInstructionFixturesWithOperand source operand @ extra) |> List.collect (fun instruction -> [Map.empty;ids] |> List.map (fun ids ->
        let func = makeFunction (AST.functionId 0UL) source [instruction]
        let blocks = func.CFG.Blocks |> Map.toArray |> Array.map snd
        tuple [enc instruction;attempt (fun () -> DeadCodeElimination.getCalledFunctions ids func);enc (DeadCodeElimination.requiresListDisplayHelpers func);enc (RegisterPolicy.isNonTailCall instruction);enc (RegisterPolicy.hasNonTailCalls blocks);array ([Platform.ARM64;Platform.X86_64] |> List.map (fun arch -> tuple [enc (RegisterPolicy.calleeSavedRegsFor arch);enc (RegisterPolicy.getAllocatableRegs arch blocks)]))])))
    let graphCases = [0;1;2;3;4;5] |> List.map (fun variant ->
        let f n name instructions = makeFunction (AST.functionId (uint64 n)) name instructions
        let users = [f 0 "main" [LIR.Call (LIR.Virtual 1,AST.functionId 1UL,[]);LIR.Mov (LIR.Virtual 2,LIR.FuncAddr (AST.functionId 3UL))];f 1 "helper" [LIR.TailCall (AST.functionId (if variant % 2=0 then 0UL else 4UL),[])];f 2 (if variant=3 then "main" else "unused") [LIR.LoadFuncAddr (LIR.Virtual 1,AST.functionId 5UL)]]
        let stdlib = [f 3 "s3" [LIR.TailCall (AST.functionId 4UL,[])];f 4 "s4" [LIR.TailCall (AST.functionId (if variant % 3=0 then 3UL else 99UL),[])];f 5 "s5" [];makeFunction (AST.functionId System.UInt64.MaxValue) "max" [LIR.ClosureAlloc (LIR.Virtual 1,AST.functionId 3UL,[])]]
        let all = users @ stdlib
        let names = all |> List.map (fun func -> func.Name,func.Id) |> Map.ofList
        let userGraph = DeadCodeElimination.buildCallGraph names users
        let stdlibGraph = DeadCodeElimination.buildCallGraph names stdlib
        let incompleteGraph = if variant % 2=0 then FunctionIdMap.remove (AST.functionId 1UL) userGraph else userGraph
        let roots = Set.ofList [AST.functionId 0UL;AST.functionId System.UInt64.MaxValue;AST.functionId 77UL]
        let anfFunc : ANF.Function = {Id=AST.functionId 1UL;Name="helper";TypedParams=[];ReturnType=AST.TUnit;ReturnOwnership=ANF.OwnedReturn;Body=ANF.Let (ANF.TempId 1,ANF.Call (AST.functionId 3UL,[]),ANF.Return ANF.UnitLiteral)}
        let anf = ANF.Program ([anfFunc],ANF.Let (ANF.TempId 2,ANF.ClosureAlloc (AST.functionId (if variant % 2=0 then 1UL else 5UL),[]),ANF.Return ANF.UnitLiteral))
        tuple [callGraph userGraph;callGraph stdlibGraph;enc (DeadCodeElimination.findReachable (FunctionIdMap.merge userGraph stdlibGraph) roots);enc (DeadCodeElimination.directCallsFromFunctions incompleteGraph users);
               array ([None;Some "main";Some "helper";Some "unused";Some "missing"] |> List.map (fun entry -> tuple [attempt (fun () -> FunctionTreeShaking.filterUserFunctionsWithCallGraph entry incompleteGraph users);attempt (fun () -> FunctionTreeShaking.filterUserFunctions entry users)]));
               enc (DeadCodeElimination.filterFunctionsWithUserCallGraph stdlibGraph incompleteGraph users stdlib);enc (DeadCodeElimination.filterFunctions stdlibGraph names users stdlib);enc (FunctionTreeShaking.filterStdlibFunctionsWithUserCallGraph stdlibGraph incompleteGraph users stdlib);enc (FunctionTreeShaking.filterStdlibFunctions stdlibGraph users stdlib);attempt (fun () -> FunctionTreeShaking.getReachableStdlibNames stdlibGraph anf)])
    tuple [array instructionCases;array graphCases;enc RegisterPolicy.callerSavedRegs;enc ([Platform.ARM64;Platform.X86_64] |> List.map (fun arch -> RegisterPolicy.getAllocatableRegs arch [||]))]

let lirObservation (source:string) =
    let enc (value:'a) = encode typeof<'a> (box value)
    let tuple values = namedArray "tuple" (Array.ofList values)
    let label text = LIR.Label text
    let block text instructions terminator : LIR.BasicBlock = {Label=label text;Instrs=instructions;Terminator=terminator}
    let graph entry blocks : LIR.CFG = {Entry=label entry;Blocks=Map.ofList (blocks |> List.map (fun (block:LIR.BasicBlock) -> block.Label,block))}
    let makeFunction cfg typedParams : LIR.Function = {Id=AST.functionId System.UInt64.MaxValue;Name="fixture";TypedParams=typedParams;CFG=cfg;StackSize=32;UsedCalleeSaved=[LIR.X19;LIR.X27];CodegenFacts=None}
    let terminators = [LIR.Ret;LIR.Jump (label "a");LIR.Branch (LIR.Virtual 0,label "b",label "a");LIR.BranchZero (LIR.Virtual 0,label "b",label "a");LIR.BranchBitZero (LIR.Virtual 0,3,label "b",label "a");LIR.BranchBitNonZero (LIR.Virtual 0,3,label "b",label "a");LIR.CondBranch (LIR.LT,label "b",label "a")]
    let layouts = terminators |> List.collect (fun term -> [0;1;2;3;4] |> List.map (fun variant ->
        let aTerm = match variant with 0 -> LIR.Jump (label "ret") | 1 -> LIR.Jump (label source) | 2 -> LIR.Jump (label "missing") | 3 -> LIR.Branch (LIR.Virtual 1,label "ret",label "ret") | _ -> LIR.Ret
        let bTerm = if variant=3 then LIR.Ret else LIR.Jump (label "ret")
        let cfg = graph source [block source [] term;block "a" [] aTerm;block "b" [] bTerm;block "ret" [] LIR.Ret;block "\uE000" [] (LIR.Jump (label "\U00010000"));block "\U00010000" [] LIR.Ret]
        tuple [enc cfg;enc (LIR.layoutBlocks cfg)]))
    let bad = [graph "missing" [block source [] LIR.Ret];graph "missing" [];graph source [block source [] (LIR.Jump (label "missing"))]]
    let planTypes = [AST.TString;AST.TInt64;AST.TList AST.TString;AST.TRecord ("\uE000",[]);AST.TRecord ("\U00010000",[]);AST.TTuple [AST.TString;AST.TList AST.TInt64]]
    let basePlans = [MemoryModel.NoReleasePlan;MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer;MemoryModel.DynamicBufferRelease (MemoryModel.FixedSizeRoot (8,MemoryModel.GenericHeap));MemoryModel.DynamicBufferRelease MemoryModel.DynamicIntBuffer]
    let recursive = List.map MemoryModel.RecursiveRelease planTypes
    let fields = recursive |> List.mapi (fun i plan -> MemoryModel.FieldRelease (i*8,plan))
    let payloads = [MemoryModel.NoPayloadRelease;MemoryModel.FixedBlockPayloadRelease (48,fields);MemoryModel.BoxedSumPayloadRelease (48,fields,[{MemoryModel.RcBoxedSumVariantRelease.Tag=3;FieldReleases=fields};{MemoryModel.RcBoxedSumVariantRelease.Tag=1;FieldReleases=[]}]);MemoryModel.TaggedListPayloadRelease (List.head recursive);MemoryModel.DictPayloadRelease (List.head recursive,recursive[1]);MemoryModel.ClosurePayloadRelease fields]
    let plans = basePlans @ recursive @ (payloads |> List.mapi (fun i payload -> MemoryModel.RootRelease (i*8,MemoryModel.GenericHeap,payload)))
    let metadata : MemoryModel.RcMetadata option list = None :: Some {ReleasePlanCacheKey=None;ReleasePlan=None;SourceType=None} :: (plans |> List.collect (fun plan -> [None;Some source;Some "\uE000";Some "\U00010000"] |> List.map (fun cache -> Some {ReleasePlanCacheKey=cache;ReleasePlan=Some plan;SourceType=Some AST.TString})))
    let kinds = [LIR.GenericHeap;LIR.StreamHeap;LIR.TaggedList;LIR.DictHeap;LIR.ClosureHeap]
    let rcInstructions = kinds |> List.collect (fun kind -> metadata |> List.collect (fun metadata -> [LIR.RefCountDec (LIR.Virtual 1,16,kind,metadata);LIR.RefCountInc (LIR.Virtual 1,16,kind,metadata)]))
    let cliInstructions = [LIR.Execute;LIR.RunProcess;LIR.GetArgv;LIR.SpawnProcess;LIR.ProcessIO;LIR.TerminateProcess] |> List.map (fun operation -> LIR.CliNative (LIR.Virtual 1,operation,[]))
    let instructions = lirInstructionFixtures source
    let paramLists : LIR.TypedLIRParam list list = [[];[{Reg=LIR.Virtual 0;Type=AST.TTuple []}];[{Reg=LIR.Virtual 0;Type=AST.TTuple [AST.TInt64;AST.TString;AST.TList AST.TString]}];[{Reg=LIR.Physical LIR.X0;Type=AST.TInt64}]]
    let factCases = (instructions @ cliInstructions @ rcInstructions) |> List.collect (fun instruction -> paramLists |> List.map (fun parameters ->
        let func = makeFunction (graph source [block source [instruction] LIR.Ret]) parameters
        tuple [enc func;enc (LIR.analyzeFunctionCodegenFacts func);enc (LIR.attachFunctionCodegenFacts func)]))
    let allFunction = makeFunction (graph source [block source instructions LIR.Ret;block "\uE000" rcInstructions LIR.Ret;block "\U00010000" cliInstructions LIR.Ret]) paramLists[2]
    let program = LIR.Program ([allFunction],Map.ofList [source,{LIR.TypeVariants.TypeParams=["a"];Variants=[{LIR.VariantInfo.Name="C";Tag=3;Payload=Some AST.TString;FieldCount=1}]}],Map.ofList [source,["f",AST.TList AST.TInt64]])
    let keys = List.map LIR.rcReleasePlanMemoKey metadata
    let printingInstructions = instructions @ ([0;1;2;3;4;8] |> List.collect (fun size -> [LIR.PrintSum (LIR.Virtual 3,List.init size (fun i -> source + "\n\"",i,if i % 2 = 0 then None else Some (AST.TTuple planTypes)),false);LIR.PrintRecord (LIR.Virtual 3,source,List.init size (fun i -> source + string i,AST.TTuple planTypes))]))
    let printerCases = terminators |> List.collect (fun terminator -> printingInstructions |> List.map (fun instruction ->
        let first = makeFunction (graph source [block source [instruction] terminator;block "a" [] LIR.Ret]) []
        let second = {first with Id=AST.functionId 2UL;Name="Other.\U00010428";CFG=graph "a" [block "a" [] LIR.Ret]}
        let program = LIR.Program ([first;second],Map.empty,Map.empty)
        tuple [enc (LIRPrinter.formatLIR program);JsonArray([None;Some "fixture";Some "TURE";Some "absent";Some "\U00010400"] |> List.map (fun filter -> enc (List.map (fun summary -> LIRPrinter.formatLIRDump filter summary program) [false;true])) |> Array.ofList) :> JsonNode]))
    tuple [lirConstructorFixtures source;JsonArray(Array.ofList layouts) :> JsonNode;enc (List.map LIR.layoutBlocks bad);JsonArray(Array.ofList factCases) :> JsonNode;enc keys;enc (Set.ofList keys |> Set.toList);enc (LIR.attachCodegenFacts program);enc (LIR.countCoverageHits program);JsonArray(Array.ofList printerCases) :> JsonNode;enc (LIRPrinter.formatLIR (LIR.Program ([],Map.empty,Map.empty)))]

let irPrinterObservation (source:string) =
    let enc value=closureAnalysisEncode value
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let fid n=AST.functionId (uint64 n)
    let operations typ operand=[
            MIR.Mov (MIR.VReg 1, operand, Some typ);
            MIR.BinOp (MIR.VReg 1, MIR.Div, operand, operand, typ);
            MIR.UnaryOp (MIR.VReg 1, MIR.Not, operand);
            MIR.Call (MIR.VReg 1, fid 200, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.TailCall (fid 200, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.IndirectCall (MIR.VReg 1, operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.IndirectTailCall (operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.ClosureAlloc (MIR.VReg 1, fid 200, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.ClosureCall (MIR.VReg 1, operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.ClosureTailCall (operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ]);
            MIR.HeapAlloc (MIR.VReg 1, 3);
            MIR.HeapStore (MIR.VReg 1, 3, operand, Some typ);
            MIR.HeapLoad (MIR.VReg 1, MIR.VReg 2, 3, Some typ);
            MIR.StringConcat (MIR.VReg 1, operand, operand, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.CanonicalBufferEq (MIR.VReg 1, MemoryModel.Utf8String, operand, operand);
            MIR.RefCountInc (MIR.VReg 1, 3, MIR.GenericHeap, None);
            MIR.RefCountDec (MIR.VReg 1, 3, MIR.GenericHeap, None);
            MIR.Print (operand, typ);
            MIR.StdoutWrite (3, operand, true);
            MIR.StdinReadLine (MIR.VReg 1);
            MIR.RuntimeError (source);
            MIR.RuntimeErrorString (operand);
            MIR.FileReadBlob (MIR.VReg 1, operand);
            MIR.FileExists (MIR.VReg 1, operand);
            MIR.FileWriteBlob (MIR.VReg 1, operand, operand);
            MIR.FileAppendText (MIR.VReg 1, operand, operand);
            MIR.FileDelete (MIR.VReg 1, operand);
            MIR.FileCreateDirectory (MIR.VReg 1, operand);
            MIR.FileSetExecutable (MIR.VReg 1, operand);
            MIR.FileWriteFromPtr (MIR.VReg 1, operand, operand, operand);
            MIR.FloatSqrt (MIR.VReg 1, operand);
            MIR.FloatAbs (MIR.VReg 1, operand);
            MIR.FloatNeg (MIR.VReg 1, operand);
            MIR.Int64ToFloat (MIR.VReg 1, operand);
            MIR.FloatToInt64 (MIR.VReg 1, operand);
            MIR.FloatToBits (MIR.VReg 1, operand);
            MIR.RawAlloc (MIR.VReg 1, operand);
            MIR.MappedAlloc (MIR.VReg 1, operand);
            MIR.RawFree (operand);
            MIR.MappedFree (operand);
            MIR.RawGet (MIR.VReg 1, operand, operand, Some typ);
            MIR.RawGetByte (MIR.VReg 1, operand, operand);
            MIR.RawWriteWord (operand, operand, operand);
            MIR.RawWriteByte (operand, operand, operand);
            MIR.RawSlotInit (operand, operand, operand, typ);
            MIR.StringToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToString (MIR.VReg 1, operand);
            MIR.BlobToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToBlob (MIR.VReg 1, operand);
            MIR.DictToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToDict (MIR.VReg 1, operand, operand);
            MIR.ListToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToList (MIR.VReg 1, operand, operand);
            MIR.RefCountIncString (operand);
            MIR.RefCountDecString (operand);
            MIR.RefCountIncBlob (operand);
            MIR.RefCountDecBlob (operand);
            MIR.RefCountIncInt (operand);
            MIR.RefCountDecInt (operand);
            MIR.RandomInt64 (MIR.VReg 1);
            MIR.DateTimeNow (MIR.VReg 1);
            MIR.Sleep (3, MIR.VReg 2, operand);
            MIR.CliNative (MIR.VReg 1, MIR.HostOS, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.FloatToString (MIR.VReg 1, operand);
            MIR.Phi (MIR.VReg 1, [operand,MIR.Label source;MIR.Register (MIR.VReg 3),MIR.Label "other"], Some typ);
            MIR.CoverageHit (3)        ]
    let strings=[source;"\\\"\n\r\t\000";"😀";"é";String [|char 0xd800|];String [|char 0xdc00|]]
    let escaping=strings |> List.map (fun text -> enc (rcInternalCall<string> "IRPrinting" "escapeStringContent" [|box text|])) |> list
    let cases=[0x61,0x41;0x62,0x42;0x63,0x43;0x64,0x44;0x65,0x45;0x66,0x46;0x67,0x47;0x68,0x48;0x69,0x49;0x6a,0x4a;0x6b,0x4b;0x6c,0x4c;0x6d,0x4d;0x6e,0x4e;0x6f,0x4f;0x70,0x50;0x71,0x51;0x72,0x52;0x73,0x53;0x74,0x54;0x75,0x55;0x76,0x56;0x77,0x57;0x78,0x58;0x79,0x59;0x7a,0x5a;0xb5,0x39c;0xe0,0xc0;0xe1,0xc1;0xe2,0xc2;0xe3,0xc3;0xe4,0xc4;0xe5,0xc5;0xe6,0xc6;0xe7,0xc7;0xe8,0xc8;0xe9,0xc9;0xea,0xca;0xeb,0xcb;0xec,0xcc;0xed,0xcd;0xee,0xce;0xef,0xcf;0xf0,0xd0;0xf1,0xd1;0xf2,0xd2;0xf3,0xd3;0xf4,0xd4;0xf5,0xd5;0xf6,0xd6;0xf8,0xd8;0xf9,0xd9;0xfa,0xda;0xfb,0xdb;0xfc,0xdc;0xfd,0xdd;0xfe,0xde;0xff,0x178;0x101,0x100;0x103,0x102;0x105,0x104;0x107,0x106;0x109,0x108;0x10b,0x10a;0x10d,0x10c;0x10f,0x10e;0x111,0x110;0x113,0x112;0x115,0x114;0x117,0x116;0x119,0x118;0x11b,0x11a;0x11d,0x11c;0x11f,0x11e;0x121,0x120;0x123,0x122;0x125,0x124;0x127,0x126;0x129,0x128;0x12b,0x12a;0x12d,0x12c;0x12f,0x12e;0x133,0x132;0x135,0x134;0x137,0x136;0x13a,0x139;0x13c,0x13b;0x13e,0x13d;0x140,0x13f;0x142,0x141;0x144,0x143;0x146,0x145;0x148,0x147;0x14b,0x14a;0x14d,0x14c;0x14f,0x14e;0x151,0x150;0x153,0x152;0x155,0x154;0x157,0x156;0x159,0x158;0x15b,0x15a;0x15d,0x15c;0x15f,0x15e;0x161,0x160;0x163,0x162;0x165,0x164;0x167,0x166;0x169,0x168;0x16b,0x16a;0x16d,0x16c;0x16f,0x16e;0x171,0x170;0x173,0x172;0x175,0x174;0x177,0x176;0x17a,0x179;0x17c,0x17b;0x17e,0x17d;0x180,0x243;0x183,0x182;0x185,0x184;0x188,0x187;0x18c,0x18b;0x192,0x191;0x195,0x1f6;0x199,0x198;0x19a,0x23d;0x19e,0x220;0x1a1,0x1a0;0x1a3,0x1a2;0x1a5,0x1a4;0x1a8,0x1a7;0x1ad,0x1ac;0x1b0,0x1af;0x1b4,0x1b3;0x1b6,0x1b5;0x1b9,0x1b8;0x1bd,0x1bc;0x1bf,0x1f7;0x1c5,0x1c4;0x1c6,0x1c4;0x1c8,0x1c7;0x1c9,0x1c7;0x1cb,0x1ca;0x1cc,0x1ca;0x1ce,0x1cd;0x1d0,0x1cf;0x1d2,0x1d1;0x1d4,0x1d3;0x1d6,0x1d5;0x1d8,0x1d7;0x1da,0x1d9;0x1dc,0x1db;0x1dd,0x18e;0x1df,0x1de;0x1e1,0x1e0;0x1e3,0x1e2;0x1e5,0x1e4;0x1e7,0x1e6;0x1e9,0x1e8;0x1eb,0x1ea;0x1ed,0x1ec;0x1ef,0x1ee;0x1f2,0x1f1;0x1f3,0x1f1;0x1f5,0x1f4;0x1f9,0x1f8;0x1fb,0x1fa;0x1fd,0x1fc;0x1ff,0x1fe;0x201,0x200;0x203,0x202;0x205,0x204;0x207,0x206;0x209,0x208;0x20b,0x20a;0x20d,0x20c;0x20f,0x20e;0x211,0x210;0x213,0x212;0x215,0x214;0x217,0x216;0x219,0x218;0x21b,0x21a;0x21d,0x21c;0x21f,0x21e;0x223,0x222;0x225,0x224;0x227,0x226;0x229,0x228;0x22b,0x22a;0x22d,0x22c;0x22f,0x22e;0x231,0x230;0x233,0x232;0x23c,0x23b;0x23f,0x2c7e;0x240,0x2c7f;0x242,0x241;0x247,0x246;0x249,0x248;0x24b,0x24a;0x24d,0x24c;0x24f,0x24e;0x250,0x2c6f;0x251,0x2c6d;0x252,0x2c70;0x253,0x181;0x254,0x186;0x256,0x189;0x257,0x18a;0x259,0x18f;0x25b,0x190;0x25c,0xa7ab;0x260,0x193;0x261,0xa7ac;0x263,0x194;0x265,0xa78d;0x266,0xa7aa;0x268,0x197;0x269,0x196;0x26a,0xa7ae;0x26b,0x2c62;0x26c,0xa7ad;0x26f,0x19c;0x271,0x2c6e;0x272,0x19d;0x275,0x19f;0x27d,0x2c64;0x280,0x1a6;0x282,0xa7c5;0x283,0x1a9;0x287,0xa7b1;0x288,0x1ae;0x289,0x244;0x28a,0x1b1;0x28b,0x1b2;0x28c,0x245;0x292,0x1b7;0x29d,0xa7b2;0x29e,0xa7b0;0x345,0x399;0x371,0x370;0x373,0x372;0x377,0x376;0x37b,0x3fd;0x37c,0x3fe;0x37d,0x3ff;0x3ac,0x386;0x3ad,0x388;0x3ae,0x389;0x3af,0x38a;0x3b1,0x391;0x3b2,0x392;0x3b3,0x393;0x3b4,0x394;0x3b5,0x395;0x3b6,0x396;0x3b7,0x397;0x3b8,0x398;0x3b9,0x399;0x3ba,0x39a;0x3bb,0x39b;0x3bc,0x39c;0x3bd,0x39d;0x3be,0x39e;0x3bf,0x39f;0x3c0,0x3a0;0x3c1,0x3a1;0x3c2,0x3a3;0x3c3,0x3a3;0x3c4,0x3a4;0x3c5,0x3a5;0x3c6,0x3a6;0x3c7,0x3a7;0x3c8,0x3a8;0x3c9,0x3a9;0x3ca,0x3aa;0x3cb,0x3ab;0x3cc,0x38c;0x3cd,0x38e;0x3ce,0x38f;0x3d0,0x392;0x3d1,0x398;0x3d5,0x3a6;0x3d6,0x3a0;0x3d7,0x3cf;0x3d9,0x3d8;0x3db,0x3da;0x3dd,0x3dc;0x3df,0x3de;0x3e1,0x3e0;0x3e3,0x3e2;0x3e5,0x3e4;0x3e7,0x3e6;0x3e9,0x3e8;0x3eb,0x3ea;0x3ed,0x3ec;0x3ef,0x3ee;0x3f0,0x39a;0x3f1,0x3a1;0x3f2,0x3f9;0x3f3,0x37f;0x3f5,0x395;0x3f8,0x3f7;0x3fb,0x3fa;0x430,0x410;0x431,0x411;0x432,0x412;0x433,0x413;0x434,0x414;0x435,0x415;0x436,0x416;0x437,0x417;0x438,0x418;0x439,0x419;0x43a,0x41a;0x43b,0x41b;0x43c,0x41c;0x43d,0x41d;0x43e,0x41e;0x43f,0x41f;0x440,0x420;0x441,0x421;0x442,0x422;0x443,0x423;0x444,0x424;0x445,0x425;0x446,0x426;0x447,0x427;0x448,0x428;0x449,0x429;0x44a,0x42a;0x44b,0x42b;0x44c,0x42c;0x44d,0x42d;0x44e,0x42e;0x44f,0x42f;0x450,0x400;0x451,0x401;0x452,0x402;0x453,0x403;0x454,0x404;0x455,0x405;0x456,0x406;0x457,0x407;0x458,0x408;0x459,0x409;0x45a,0x40a;0x45b,0x40b;0x45c,0x40c;0x45d,0x40d;0x45e,0x40e;0x45f,0x40f;0x461,0x460;0x463,0x462;0x465,0x464;0x467,0x466;0x469,0x468;0x46b,0x46a;0x46d,0x46c;0x46f,0x46e;0x471,0x470;0x473,0x472;0x475,0x474;0x477,0x476;0x479,0x478;0x47b,0x47a;0x47d,0x47c;0x47f,0x47e;0x481,0x480;0x48b,0x48a;0x48d,0x48c;0x48f,0x48e;0x491,0x490;0x493,0x492;0x495,0x494;0x497,0x496;0x499,0x498;0x49b,0x49a;0x49d,0x49c;0x49f,0x49e;0x4a1,0x4a0;0x4a3,0x4a2;0x4a5,0x4a4;0x4a7,0x4a6;0x4a9,0x4a8;0x4ab,0x4aa;0x4ad,0x4ac;0x4af,0x4ae;0x4b1,0x4b0;0x4b3,0x4b2;0x4b5,0x4b4;0x4b7,0x4b6;0x4b9,0x4b8;0x4bb,0x4ba;0x4bd,0x4bc;0x4bf,0x4be;0x4c2,0x4c1;0x4c4,0x4c3;0x4c6,0x4c5;0x4c8,0x4c7;0x4ca,0x4c9;0x4cc,0x4cb;0x4ce,0x4cd;0x4cf,0x4c0;0x4d1,0x4d0;0x4d3,0x4d2;0x4d5,0x4d4;0x4d7,0x4d6;0x4d9,0x4d8;0x4db,0x4da;0x4dd,0x4dc;0x4df,0x4de;0x4e1,0x4e0;0x4e3,0x4e2;0x4e5,0x4e4;0x4e7,0x4e6;0x4e9,0x4e8;0x4eb,0x4ea;0x4ed,0x4ec;0x4ef,0x4ee;0x4f1,0x4f0;0x4f3,0x4f2;0x4f5,0x4f4;0x4f7,0x4f6;0x4f9,0x4f8;0x4fb,0x4fa;0x4fd,0x4fc;0x4ff,0x4fe;0x501,0x500;0x503,0x502;0x505,0x504;0x507,0x506;0x509,0x508;0x50b,0x50a;0x50d,0x50c;0x50f,0x50e;0x511,0x510;0x513,0x512;0x515,0x514;0x517,0x516;0x519,0x518;0x51b,0x51a;0x51d,0x51c;0x51f,0x51e;0x521,0x520;0x523,0x522;0x525,0x524;0x527,0x526;0x529,0x528;0x52b,0x52a;0x52d,0x52c;0x52f,0x52e;0x561,0x531;0x562,0x532;0x563,0x533;0x564,0x534;0x565,0x535;0x566,0x536;0x567,0x537;0x568,0x538;0x569,0x539;0x56a,0x53a;0x56b,0x53b;0x56c,0x53c;0x56d,0x53d;0x56e,0x53e;0x56f,0x53f;0x570,0x540;0x571,0x541;0x572,0x542;0x573,0x543;0x574,0x544;0x575,0x545;0x576,0x546;0x577,0x547;0x578,0x548;0x579,0x549;0x57a,0x54a;0x57b,0x54b;0x57c,0x54c;0x57d,0x54d;0x57e,0x54e;0x57f,0x54f;0x580,0x550;0x581,0x551;0x582,0x552;0x583,0x553;0x584,0x554;0x585,0x555;0x586,0x556;0x10d0,0x1c90;0x10d1,0x1c91;0x10d2,0x1c92;0x10d3,0x1c93;0x10d4,0x1c94;0x10d5,0x1c95;0x10d6,0x1c96;0x10d7,0x1c97;0x10d8,0x1c98;0x10d9,0x1c99;0x10da,0x1c9a;0x10db,0x1c9b;0x10dc,0x1c9c;0x10dd,0x1c9d;0x10de,0x1c9e;0x10df,0x1c9f;0x10e0,0x1ca0;0x10e1,0x1ca1;0x10e2,0x1ca2;0x10e3,0x1ca3;0x10e4,0x1ca4;0x10e5,0x1ca5;0x10e6,0x1ca6;0x10e7,0x1ca7;0x10e8,0x1ca8;0x10e9,0x1ca9;0x10ea,0x1caa;0x10eb,0x1cab;0x10ec,0x1cac;0x10ed,0x1cad;0x10ee,0x1cae;0x10ef,0x1caf;0x10f0,0x1cb0;0x10f1,0x1cb1;0x10f2,0x1cb2;0x10f3,0x1cb3;0x10f4,0x1cb4;0x10f5,0x1cb5;0x10f6,0x1cb6;0x10f7,0x1cb7;0x10f8,0x1cb8;0x10f9,0x1cb9;0x10fa,0x1cba;0x10fd,0x1cbd;0x10fe,0x1cbe;0x10ff,0x1cbf;0x13f8,0x13f0;0x13f9,0x13f1;0x13fa,0x13f2;0x13fb,0x13f3;0x13fc,0x13f4;0x13fd,0x13f5;0x1c80,0x412;0x1c81,0x414;0x1c82,0x41e;0x1c83,0x421;0x1c84,0x422;0x1c85,0x422;0x1c86,0x42a;0x1c87,0x462;0x1c88,0xa64a;0x1d79,0xa77d;0x1d7d,0x2c63;0x1d8e,0xa7c6;0x1e01,0x1e00;0x1e03,0x1e02;0x1e05,0x1e04;0x1e07,0x1e06;0x1e09,0x1e08;0x1e0b,0x1e0a;0x1e0d,0x1e0c;0x1e0f,0x1e0e;0x1e11,0x1e10;0x1e13,0x1e12;0x1e15,0x1e14;0x1e17,0x1e16;0x1e19,0x1e18;0x1e1b,0x1e1a;0x1e1d,0x1e1c;0x1e1f,0x1e1e;0x1e21,0x1e20;0x1e23,0x1e22;0x1e25,0x1e24;0x1e27,0x1e26;0x1e29,0x1e28;0x1e2b,0x1e2a;0x1e2d,0x1e2c;0x1e2f,0x1e2e;0x1e31,0x1e30;0x1e33,0x1e32;0x1e35,0x1e34;0x1e37,0x1e36;0x1e39,0x1e38;0x1e3b,0x1e3a;0x1e3d,0x1e3c;0x1e3f,0x1e3e;0x1e41,0x1e40;0x1e43,0x1e42;0x1e45,0x1e44;0x1e47,0x1e46;0x1e49,0x1e48;0x1e4b,0x1e4a;0x1e4d,0x1e4c;0x1e4f,0x1e4e;0x1e51,0x1e50;0x1e53,0x1e52;0x1e55,0x1e54;0x1e57,0x1e56;0x1e59,0x1e58;0x1e5b,0x1e5a;0x1e5d,0x1e5c;0x1e5f,0x1e5e;0x1e61,0x1e60;0x1e63,0x1e62;0x1e65,0x1e64;0x1e67,0x1e66;0x1e69,0x1e68;0x1e6b,0x1e6a;0x1e6d,0x1e6c;0x1e6f,0x1e6e;0x1e71,0x1e70;0x1e73,0x1e72;0x1e75,0x1e74;0x1e77,0x1e76;0x1e79,0x1e78;0x1e7b,0x1e7a;0x1e7d,0x1e7c;0x1e7f,0x1e7e;0x1e81,0x1e80;0x1e83,0x1e82;0x1e85,0x1e84;0x1e87,0x1e86;0x1e89,0x1e88;0x1e8b,0x1e8a;0x1e8d,0x1e8c;0x1e8f,0x1e8e;0x1e91,0x1e90;0x1e93,0x1e92;0x1e95,0x1e94;0x1e9b,0x1e60;0x1ea1,0x1ea0;0x1ea3,0x1ea2;0x1ea5,0x1ea4;0x1ea7,0x1ea6;0x1ea9,0x1ea8;0x1eab,0x1eaa;0x1ead,0x1eac;0x1eaf,0x1eae;0x1eb1,0x1eb0;0x1eb3,0x1eb2;0x1eb5,0x1eb4;0x1eb7,0x1eb6;0x1eb9,0x1eb8;0x1ebb,0x1eba;0x1ebd,0x1ebc;0x1ebf,0x1ebe;0x1ec1,0x1ec0;0x1ec3,0x1ec2;0x1ec5,0x1ec4;0x1ec7,0x1ec6;0x1ec9,0x1ec8;0x1ecb,0x1eca;0x1ecd,0x1ecc;0x1ecf,0x1ece;0x1ed1,0x1ed0;0x1ed3,0x1ed2;0x1ed5,0x1ed4;0x1ed7,0x1ed6;0x1ed9,0x1ed8;0x1edb,0x1eda;0x1edd,0x1edc;0x1edf,0x1ede;0x1ee1,0x1ee0;0x1ee3,0x1ee2;0x1ee5,0x1ee4;0x1ee7,0x1ee6;0x1ee9,0x1ee8;0x1eeb,0x1eea;0x1eed,0x1eec;0x1eef,0x1eee;0x1ef1,0x1ef0;0x1ef3,0x1ef2;0x1ef5,0x1ef4;0x1ef7,0x1ef6;0x1ef9,0x1ef8;0x1efb,0x1efa;0x1efd,0x1efc;0x1eff,0x1efe;0x1f00,0x1f08;0x1f01,0x1f09;0x1f02,0x1f0a;0x1f03,0x1f0b;0x1f04,0x1f0c;0x1f05,0x1f0d;0x1f06,0x1f0e;0x1f07,0x1f0f;0x1f10,0x1f18;0x1f11,0x1f19;0x1f12,0x1f1a;0x1f13,0x1f1b;0x1f14,0x1f1c;0x1f15,0x1f1d;0x1f20,0x1f28;0x1f21,0x1f29;0x1f22,0x1f2a;0x1f23,0x1f2b;0x1f24,0x1f2c;0x1f25,0x1f2d;0x1f26,0x1f2e;0x1f27,0x1f2f;0x1f30,0x1f38;0x1f31,0x1f39;0x1f32,0x1f3a;0x1f33,0x1f3b;0x1f34,0x1f3c;0x1f35,0x1f3d;0x1f36,0x1f3e;0x1f37,0x1f3f;0x1f40,0x1f48;0x1f41,0x1f49;0x1f42,0x1f4a;0x1f43,0x1f4b;0x1f44,0x1f4c;0x1f45,0x1f4d;0x1f51,0x1f59;0x1f53,0x1f5b;0x1f55,0x1f5d;0x1f57,0x1f5f;0x1f60,0x1f68;0x1f61,0x1f69;0x1f62,0x1f6a;0x1f63,0x1f6b;0x1f64,0x1f6c;0x1f65,0x1f6d;0x1f66,0x1f6e;0x1f67,0x1f6f;0x1f70,0x1fba;0x1f71,0x1fbb;0x1f72,0x1fc8;0x1f73,0x1fc9;0x1f74,0x1fca;0x1f75,0x1fcb;0x1f76,0x1fda;0x1f77,0x1fdb;0x1f78,0x1ff8;0x1f79,0x1ff9;0x1f7a,0x1fea;0x1f7b,0x1feb;0x1f7c,0x1ffa;0x1f7d,0x1ffb;0x1f80,0x1f88;0x1f81,0x1f89;0x1f82,0x1f8a;0x1f83,0x1f8b;0x1f84,0x1f8c;0x1f85,0x1f8d;0x1f86,0x1f8e;0x1f87,0x1f8f;0x1f90,0x1f98;0x1f91,0x1f99;0x1f92,0x1f9a;0x1f93,0x1f9b;0x1f94,0x1f9c;0x1f95,0x1f9d;0x1f96,0x1f9e;0x1f97,0x1f9f;0x1fa0,0x1fa8;0x1fa1,0x1fa9;0x1fa2,0x1faa;0x1fa3,0x1fab;0x1fa4,0x1fac;0x1fa5,0x1fad;0x1fa6,0x1fae;0x1fa7,0x1faf;0x1fb0,0x1fb8;0x1fb1,0x1fb9;0x1fb3,0x1fbc;0x1fbe,0x399;0x1fc3,0x1fcc;0x1fd0,0x1fd8;0x1fd1,0x1fd9;0x1fe0,0x1fe8;0x1fe1,0x1fe9;0x1fe5,0x1fec;0x1ff3,0x1ffc;0x214e,0x2132;0x2170,0x2160;0x2171,0x2161;0x2172,0x2162;0x2173,0x2163;0x2174,0x2164;0x2175,0x2165;0x2176,0x2166;0x2177,0x2167;0x2178,0x2168;0x2179,0x2169;0x217a,0x216a;0x217b,0x216b;0x217c,0x216c;0x217d,0x216d;0x217e,0x216e;0x217f,0x216f;0x2184,0x2183;0x24d0,0x24b6;0x24d1,0x24b7;0x24d2,0x24b8;0x24d3,0x24b9;0x24d4,0x24ba;0x24d5,0x24bb;0x24d6,0x24bc;0x24d7,0x24bd;0x24d8,0x24be;0x24d9,0x24bf;0x24da,0x24c0;0x24db,0x24c1;0x24dc,0x24c2;0x24dd,0x24c3;0x24de,0x24c4;0x24df,0x24c5;0x24e0,0x24c6;0x24e1,0x24c7;0x24e2,0x24c8;0x24e3,0x24c9;0x24e4,0x24ca;0x24e5,0x24cb;0x24e6,0x24cc;0x24e7,0x24cd;0x24e8,0x24ce;0x24e9,0x24cf;0x2c30,0x2c00;0x2c31,0x2c01;0x2c32,0x2c02;0x2c33,0x2c03;0x2c34,0x2c04;0x2c35,0x2c05;0x2c36,0x2c06;0x2c37,0x2c07;0x2c38,0x2c08;0x2c39,0x2c09;0x2c3a,0x2c0a;0x2c3b,0x2c0b;0x2c3c,0x2c0c;0x2c3d,0x2c0d;0x2c3e,0x2c0e;0x2c3f,0x2c0f;0x2c40,0x2c10;0x2c41,0x2c11;0x2c42,0x2c12;0x2c43,0x2c13;0x2c44,0x2c14;0x2c45,0x2c15;0x2c46,0x2c16;0x2c47,0x2c17;0x2c48,0x2c18;0x2c49,0x2c19;0x2c4a,0x2c1a;0x2c4b,0x2c1b;0x2c4c,0x2c1c;0x2c4d,0x2c1d;0x2c4e,0x2c1e;0x2c4f,0x2c1f;0x2c50,0x2c20;0x2c51,0x2c21;0x2c52,0x2c22;0x2c53,0x2c23;0x2c54,0x2c24;0x2c55,0x2c25;0x2c56,0x2c26;0x2c57,0x2c27;0x2c58,0x2c28;0x2c59,0x2c29;0x2c5a,0x2c2a;0x2c5b,0x2c2b;0x2c5c,0x2c2c;0x2c5d,0x2c2d;0x2c5e,0x2c2e;0x2c5f,0x2c2f;0x2c61,0x2c60;0x2c65,0x23a;0x2c66,0x23e;0x2c68,0x2c67;0x2c6a,0x2c69;0x2c6c,0x2c6b;0x2c73,0x2c72;0x2c76,0x2c75;0x2c81,0x2c80;0x2c83,0x2c82;0x2c85,0x2c84;0x2c87,0x2c86;0x2c89,0x2c88;0x2c8b,0x2c8a;0x2c8d,0x2c8c;0x2c8f,0x2c8e;0x2c91,0x2c90;0x2c93,0x2c92;0x2c95,0x2c94;0x2c97,0x2c96;0x2c99,0x2c98;0x2c9b,0x2c9a;0x2c9d,0x2c9c;0x2c9f,0x2c9e;0x2ca1,0x2ca0;0x2ca3,0x2ca2;0x2ca5,0x2ca4;0x2ca7,0x2ca6;0x2ca9,0x2ca8;0x2cab,0x2caa;0x2cad,0x2cac;0x2caf,0x2cae;0x2cb1,0x2cb0;0x2cb3,0x2cb2;0x2cb5,0x2cb4;0x2cb7,0x2cb6;0x2cb9,0x2cb8;0x2cbb,0x2cba;0x2cbd,0x2cbc;0x2cbf,0x2cbe;0x2cc1,0x2cc0;0x2cc3,0x2cc2;0x2cc5,0x2cc4;0x2cc7,0x2cc6;0x2cc9,0x2cc8;0x2ccb,0x2cca;0x2ccd,0x2ccc;0x2ccf,0x2cce;0x2cd1,0x2cd0;0x2cd3,0x2cd2;0x2cd5,0x2cd4;0x2cd7,0x2cd6;0x2cd9,0x2cd8;0x2cdb,0x2cda;0x2cdd,0x2cdc;0x2cdf,0x2cde;0x2ce1,0x2ce0;0x2ce3,0x2ce2;0x2cec,0x2ceb;0x2cee,0x2ced;0x2cf3,0x2cf2;0x2d00,0x10a0;0x2d01,0x10a1;0x2d02,0x10a2;0x2d03,0x10a3;0x2d04,0x10a4;0x2d05,0x10a5;0x2d06,0x10a6;0x2d07,0x10a7;0x2d08,0x10a8;0x2d09,0x10a9;0x2d0a,0x10aa;0x2d0b,0x10ab;0x2d0c,0x10ac;0x2d0d,0x10ad;0x2d0e,0x10ae;0x2d0f,0x10af;0x2d10,0x10b0;0x2d11,0x10b1;0x2d12,0x10b2;0x2d13,0x10b3;0x2d14,0x10b4;0x2d15,0x10b5;0x2d16,0x10b6;0x2d17,0x10b7;0x2d18,0x10b8;0x2d19,0x10b9;0x2d1a,0x10ba;0x2d1b,0x10bb;0x2d1c,0x10bc;0x2d1d,0x10bd;0x2d1e,0x10be;0x2d1f,0x10bf;0x2d20,0x10c0;0x2d21,0x10c1;0x2d22,0x10c2;0x2d23,0x10c3;0x2d24,0x10c4;0x2d25,0x10c5;0x2d27,0x10c7;0x2d2d,0x10cd;0xa641,0xa640;0xa643,0xa642;0xa645,0xa644;0xa647,0xa646;0xa649,0xa648;0xa64b,0xa64a;0xa64d,0xa64c;0xa64f,0xa64e;0xa651,0xa650;0xa653,0xa652;0xa655,0xa654;0xa657,0xa656;0xa659,0xa658;0xa65b,0xa65a;0xa65d,0xa65c;0xa65f,0xa65e;0xa661,0xa660;0xa663,0xa662;0xa665,0xa664;0xa667,0xa666;0xa669,0xa668;0xa66b,0xa66a;0xa66d,0xa66c;0xa681,0xa680;0xa683,0xa682;0xa685,0xa684;0xa687,0xa686;0xa689,0xa688;0xa68b,0xa68a;0xa68d,0xa68c;0xa68f,0xa68e;0xa691,0xa690;0xa693,0xa692;0xa695,0xa694;0xa697,0xa696;0xa699,0xa698;0xa69b,0xa69a;0xa723,0xa722;0xa725,0xa724;0xa727,0xa726;0xa729,0xa728;0xa72b,0xa72a;0xa72d,0xa72c;0xa72f,0xa72e;0xa733,0xa732;0xa735,0xa734;0xa737,0xa736;0xa739,0xa738;0xa73b,0xa73a;0xa73d,0xa73c;0xa73f,0xa73e;0xa741,0xa740;0xa743,0xa742;0xa745,0xa744;0xa747,0xa746;0xa749,0xa748;0xa74b,0xa74a;0xa74d,0xa74c;0xa74f,0xa74e;0xa751,0xa750;0xa753,0xa752;0xa755,0xa754;0xa757,0xa756;0xa759,0xa758;0xa75b,0xa75a;0xa75d,0xa75c;0xa75f,0xa75e;0xa761,0xa760;0xa763,0xa762;0xa765,0xa764;0xa767,0xa766;0xa769,0xa768;0xa76b,0xa76a;0xa76d,0xa76c;0xa76f,0xa76e;0xa77a,0xa779;0xa77c,0xa77b;0xa77f,0xa77e;0xa781,0xa780;0xa783,0xa782;0xa785,0xa784;0xa787,0xa786;0xa78c,0xa78b;0xa791,0xa790;0xa793,0xa792;0xa794,0xa7c4;0xa797,0xa796;0xa799,0xa798;0xa79b,0xa79a;0xa79d,0xa79c;0xa79f,0xa79e;0xa7a1,0xa7a0;0xa7a3,0xa7a2;0xa7a5,0xa7a4;0xa7a7,0xa7a6;0xa7a9,0xa7a8;0xa7b5,0xa7b4;0xa7b7,0xa7b6;0xa7b9,0xa7b8;0xa7bb,0xa7ba;0xa7bd,0xa7bc;0xa7bf,0xa7be;0xa7c1,0xa7c0;0xa7c3,0xa7c2;0xa7c8,0xa7c7;0xa7ca,0xa7c9;0xa7d1,0xa7d0;0xa7d7,0xa7d6;0xa7d9,0xa7d8;0xa7f6,0xa7f5;0xab53,0xa7b3;0xab70,0x13a0;0xab71,0x13a1;0xab72,0x13a2;0xab73,0x13a3;0xab74,0x13a4;0xab75,0x13a5;0xab76,0x13a6;0xab77,0x13a7;0xab78,0x13a8;0xab79,0x13a9;0xab7a,0x13aa;0xab7b,0x13ab;0xab7c,0x13ac;0xab7d,0x13ad;0xab7e,0x13ae;0xab7f,0x13af;0xab80,0x13b0;0xab81,0x13b1;0xab82,0x13b2;0xab83,0x13b3;0xab84,0x13b4;0xab85,0x13b5;0xab86,0x13b6;0xab87,0x13b7;0xab88,0x13b8;0xab89,0x13b9;0xab8a,0x13ba;0xab8b,0x13bb;0xab8c,0x13bc;0xab8d,0x13bd;0xab8e,0x13be;0xab8f,0x13bf;0xab90,0x13c0;0xab91,0x13c1;0xab92,0x13c2;0xab93,0x13c3;0xab94,0x13c4;0xab95,0x13c5;0xab96,0x13c6;0xab97,0x13c7;0xab98,0x13c8;0xab99,0x13c9;0xab9a,0x13ca;0xab9b,0x13cb;0xab9c,0x13cc;0xab9d,0x13cd;0xab9e,0x13ce;0xab9f,0x13cf;0xaba0,0x13d0;0xaba1,0x13d1;0xaba2,0x13d2;0xaba3,0x13d3;0xaba4,0x13d4;0xaba5,0x13d5;0xaba6,0x13d6;0xaba7,0x13d7;0xaba8,0x13d8;0xaba9,0x13d9;0xabaa,0x13da;0xabab,0x13db;0xabac,0x13dc;0xabad,0x13dd;0xabae,0x13de;0xabaf,0x13df;0xabb0,0x13e0;0xabb1,0x13e1;0xabb2,0x13e2;0xabb3,0x13e3;0xabb4,0x13e4;0xabb5,0x13e5;0xabb6,0x13e6;0xabb7,0x13e7;0xabb8,0x13e8;0xabb9,0x13e9;0xabba,0x13ea;0xabbb,0x13eb;0xabbc,0x13ec;0xabbd,0x13ed;0xabbe,0x13ee;0xabbf,0x13ef;0xff41,0xff21;0xff42,0xff22;0xff43,0xff23;0xff44,0xff24;0xff45,0xff25;0xff46,0xff26;0xff47,0xff27;0xff48,0xff28;0xff49,0xff29;0xff4a,0xff2a;0xff4b,0xff2b;0xff4c,0xff2c;0xff4d,0xff2d;0xff4e,0xff2e;0xff4f,0xff2f;0xff50,0xff30;0xff51,0xff31;0xff52,0xff32;0xff53,0xff33;0xff54,0xff34;0xff55,0xff35;0xff56,0xff36;0xff57,0xff37;0xff58,0xff38;0xff59,0xff39;0xff5a,0xff3a;0x10428,0x10400;0x10429,0x10401;0x1042a,0x10402;0x1042b,0x10403;0x1042c,0x10404;0x1042d,0x10405;0x1042e,0x10406;0x1042f,0x10407;0x10430,0x10408;0x10431,0x10409;0x10432,0x1040a;0x10433,0x1040b;0x10434,0x1040c;0x10435,0x1040d;0x10436,0x1040e;0x10437,0x1040f;0x10438,0x10410;0x10439,0x10411;0x1043a,0x10412;0x1043b,0x10413;0x1043c,0x10414;0x1043d,0x10415;0x1043e,0x10416;0x1043f,0x10417;0x10440,0x10418;0x10441,0x10419;0x10442,0x1041a;0x10443,0x1041b;0x10444,0x1041c;0x10445,0x1041d;0x10446,0x1041e;0x10447,0x1041f;0x10448,0x10420;0x10449,0x10421;0x1044a,0x10422;0x1044b,0x10423;0x1044c,0x10424;0x1044d,0x10425;0x1044e,0x10426;0x1044f,0x10427;0x104d8,0x104b0;0x104d9,0x104b1;0x104da,0x104b2;0x104db,0x104b3;0x104dc,0x104b4;0x104dd,0x104b5;0x104de,0x104b6;0x104df,0x104b7;0x104e0,0x104b8;0x104e1,0x104b9;0x104e2,0x104ba;0x104e3,0x104bb;0x104e4,0x104bc;0x104e5,0x104bd;0x104e6,0x104be;0x104e7,0x104bf;0x104e8,0x104c0;0x104e9,0x104c1;0x104ea,0x104c2;0x104eb,0x104c3;0x104ec,0x104c4;0x104ed,0x104c5;0x104ee,0x104c6;0x104ef,0x104c7;0x104f0,0x104c8;0x104f1,0x104c9;0x104f2,0x104ca;0x104f3,0x104cb;0x104f4,0x104cc;0x104f5,0x104cd;0x104f6,0x104ce;0x104f7,0x104cf;0x104f8,0x104d0;0x104f9,0x104d1;0x104fa,0x104d2;0x104fb,0x104d3;0x10597,0x10570;0x10598,0x10571;0x10599,0x10572;0x1059a,0x10573;0x1059b,0x10574;0x1059c,0x10575;0x1059d,0x10576;0x1059e,0x10577;0x1059f,0x10578;0x105a0,0x10579;0x105a1,0x1057a;0x105a3,0x1057c;0x105a4,0x1057d;0x105a5,0x1057e;0x105a6,0x1057f;0x105a7,0x10580;0x105a8,0x10581;0x105a9,0x10582;0x105aa,0x10583;0x105ab,0x10584;0x105ac,0x10585;0x105ad,0x10586;0x105ae,0x10587;0x105af,0x10588;0x105b0,0x10589;0x105b1,0x1058a;0x105b3,0x1058c;0x105b4,0x1058d;0x105b5,0x1058e;0x105b6,0x1058f;0x105b7,0x10590;0x105b8,0x10591;0x105b9,0x10592;0x105bb,0x10594;0x105bc,0x10595;0x10cc0,0x10c80;0x10cc1,0x10c81;0x10cc2,0x10c82;0x10cc3,0x10c83;0x10cc4,0x10c84;0x10cc5,0x10c85;0x10cc6,0x10c86;0x10cc7,0x10c87;0x10cc8,0x10c88;0x10cc9,0x10c89;0x10cca,0x10c8a;0x10ccb,0x10c8b;0x10ccc,0x10c8c;0x10ccd,0x10c8d;0x10cce,0x10c8e;0x10ccf,0x10c8f;0x10cd0,0x10c90;0x10cd1,0x10c91;0x10cd2,0x10c92;0x10cd3,0x10c93;0x10cd4,0x10c94;0x10cd5,0x10c95;0x10cd6,0x10c96;0x10cd7,0x10c97;0x10cd8,0x10c98;0x10cd9,0x10c99;0x10cda,0x10c9a;0x10cdb,0x10c9b;0x10cdc,0x10c9c;0x10cdd,0x10c9d;0x10cde,0x10c9e;0x10cdf,0x10c9f;0x10ce0,0x10ca0;0x10ce1,0x10ca1;0x10ce2,0x10ca2;0x10ce3,0x10ca3;0x10ce4,0x10ca4;0x10ce5,0x10ca5;0x10ce6,0x10ca6;0x10ce7,0x10ca7;0x10ce8,0x10ca8;0x10ce9,0x10ca9;0x10cea,0x10caa;0x10ceb,0x10cab;0x10cec,0x10cac;0x10ced,0x10cad;0x10cee,0x10cae;0x10cef,0x10caf;0x10cf0,0x10cb0;0x10cf1,0x10cb1;0x10cf2,0x10cb2;0x10d70,0x10d50;0x10d71,0x10d51;0x10d72,0x10d52;0x10d73,0x10d53;0x10d74,0x10d54;0x10d75,0x10d55;0x10d76,0x10d56;0x10d77,0x10d57;0x10d78,0x10d58;0x10d79,0x10d59;0x10d7a,0x10d5a;0x10d7b,0x10d5b;0x10d7c,0x10d5c;0x10d7d,0x10d5d;0x10d7e,0x10d5e;0x10d7f,0x10d5f;0x10d80,0x10d60;0x10d81,0x10d61;0x10d82,0x10d62;0x10d83,0x10d63;0x10d84,0x10d64;0x10d85,0x10d65;0x118c0,0x118a0;0x118c1,0x118a1;0x118c2,0x118a2;0x118c3,0x118a3;0x118c4,0x118a4;0x118c5,0x118a5;0x118c6,0x118a6;0x118c7,0x118a7;0x118c8,0x118a8;0x118c9,0x118a9;0x118ca,0x118aa;0x118cb,0x118ab;0x118cc,0x118ac;0x118cd,0x118ad;0x118ce,0x118ae;0x118cf,0x118af;0x118d0,0x118b0;0x118d1,0x118b1;0x118d2,0x118b2;0x118d3,0x118b3;0x118d4,0x118b4;0x118d5,0x118b5;0x118d6,0x118b6;0x118d7,0x118b7;0x118d8,0x118b8;0x118d9,0x118b9;0x118da,0x118ba;0x118db,0x118bb;0x118dc,0x118bc;0x118dd,0x118bd;0x118de,0x118be;0x118df,0x118bf;0x16e60,0x16e40;0x16e61,0x16e41;0x16e62,0x16e42;0x16e63,0x16e43;0x16e64,0x16e44;0x16e65,0x16e45;0x16e66,0x16e46;0x16e67,0x16e47;0x16e68,0x16e48;0x16e69,0x16e49;0x16e6a,0x16e4a;0x16e6b,0x16e4b;0x16e6c,0x16e4c;0x16e6d,0x16e4d;0x16e6e,0x16e4e;0x16e6f,0x16e4f;0x16e70,0x16e50;0x16e71,0x16e51;0x16e72,0x16e52;0x16e73,0x16e53;0x16e74,0x16e54;0x16e75,0x16e55;0x16e76,0x16e56;0x16e77,0x16e57;0x16e78,0x16e58;0x16e79,0x16e59;0x16e7a,0x16e5a;0x16e7b,0x16e5b;0x16e7c,0x16e5c;0x16e7d,0x16e5d;0x16e7e,0x16e5e;0x16e7f,0x16e5f;0x1e922,0x1e900;0x1e923,0x1e901;0x1e924,0x1e902;0x1e925,0x1e903;0x1e926,0x1e904;0x1e927,0x1e905;0x1e928,0x1e906;0x1e929,0x1e907;0x1e92a,0x1e908;0x1e92b,0x1e909;0x1e92c,0x1e90a;0x1e92d,0x1e90b;0x1e92e,0x1e90c;0x1e92f,0x1e90d;0x1e930,0x1e90e;0x1e931,0x1e90f;0x1e932,0x1e910;0x1e933,0x1e911;0x1e934,0x1e912;0x1e935,0x1e913;0x1e936,0x1e914;0x1e937,0x1e915;0x1e938,0x1e916;0x1e939,0x1e917;0x1e93a,0x1e918;0x1e93b,0x1e919;0x1e93c,0x1e91a;0x1e93d,0x1e91b;0x1e93e,0x1e91c;0x1e93f,0x1e91d;0x1e940,0x1e91e;0x1e941,0x1e91f;0x1e942,0x1e920;0x1e943,0x1e921]
    let text code=if code<=0xffff then String [|char code|] else Char.ConvertFromUtf32 code
    let filters=cases |> List.map (fun (a,b) -> let a=text a in let b=text b in [a,b;b,a;"x"+a+"y",b;a,b+"y"] |> List.map (fun (name,pattern) -> enc (rcInternalCall<bool> "IRPrinting" "functionNameMatches" [|box (Some pattern);box name|])) |> list) |> list
    let halfPairs=[String [|char 0xd801|];String [|char 0xdc28|];String [|char 0xdc00|]] |> List.map (fun pattern -> enc (rcInternalCall<bool> "IRPrinting" "functionNameMatches" [|box (Some pattern);box "𐐨"|])) |> list
    let anfExprs=[(ANF.Atom ((ANF.StringLiteral source))); (ANF.TypedAtom ((ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (ANF.Prim ((ANF.Add), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.UnaryPrim ((ANF.Neg), (ANF.StringLiteral source))); (ANF.IfValue ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.Call ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.BorrowedCall ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.TailCall ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.IndirectCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.IndirectTailCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.ClosureAlloc ((AST.functionId System.UInt64.MaxValue), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.ClosureCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.ClosureTailCall ((ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.TupleAlloc ([(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.TupleGet ((ANF.StringLiteral source), (3))); (ANF.RecordAlloc (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.RecordGet (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.StringLiteral source), (3))); (ANF.RecordClone (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.RecordReuse (({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), ({ANF.RecordDescriptor.SourceTypeName = (source); ANF.RecordDescriptor.RuntimeTypeName = (source); ANF.RecordDescriptor.TypeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; ANF.RecordDescriptor.Fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; ANF.RecordDescriptor.ValueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : ANF.RecordDescriptor), (ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.StringConcat ((ANF.StringLiteral source), (ANF.StringLiteral source), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.CanonicalBufferEq ((MemoryModel.Utf8String), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.RefCountInc ((ANF.StringLiteral source), (3), (MemoryModel.GenericHeap), (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey = (Some ((source))); MemoryModel.RcMetadata.ReleasePlan = (Some ((MemoryModel.NoReleasePlan))); MemoryModel.RcMetadata.SourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : MemoryModel.RcMetadata))))); (ANF.RefCountDec ((ANF.StringLiteral source), (3), (MemoryModel.GenericHeap), (Some (({MemoryModel.RcMetadata.ReleasePlanCacheKey = (Some ((source))); MemoryModel.RcMetadata.ReleasePlan = (Some ((MemoryModel.NoReleasePlan))); MemoryModel.RcMetadata.SourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : MemoryModel.RcMetadata))))); (ANF.Print ((ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (ANF.StdoutWrite ((ANF.StringLiteral source), (true))); (ANF.StdinReadLine); (ANF.RuntimeError ((source))); (ANF.RuntimeErrorString ((ANF.StringLiteral source))); (ANF.FileReadBlob ((ANF.StringLiteral source))); (ANF.FileExists ((ANF.StringLiteral source))); (ANF.FileWriteBlob ((ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.FileAppendText ((ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.FileDelete ((ANF.StringLiteral source))); (ANF.FileCreateDirectory ((ANF.StringLiteral source))); (ANF.FileSetExecutable ((ANF.StringLiteral source))); (ANF.FileWriteFromPtr ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.FloatSqrt ((ANF.StringLiteral source))); (ANF.FloatAbs ((ANF.StringLiteral source))); (ANF.FloatNeg ((ANF.StringLiteral source))); (ANF.Int64ToFloat ((ANF.StringLiteral source))); (ANF.FloatToInt64 ((ANF.StringLiteral source))); (ANF.FloatToBits ((ANF.StringLiteral source))); (ANF.RawAlloc ((ANF.StringLiteral source))); (ANF.MappedAlloc ((ANF.StringLiteral source))); (ANF.RawFree ((ANF.StringLiteral source))); (ANF.MappedFree ((ANF.StringLiteral source))); (ANF.RawGet ((ANF.StringLiteral source), (ANF.StringLiteral source), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))))); (ANF.RawTake ((ANF.StringLiteral source), (ANF.StringLiteral source), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))))); (ANF.RawGetByte ((ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.RawWriteWord ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.RawWriteByte ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source))); (ANF.RawSlotInit ((ANF.StringLiteral source), (ANF.StringLiteral source), (ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (ANF.StringToRawPtr ((ANF.StringLiteral source))); (ANF.RawPtrToString ((ANF.StringLiteral source))); (ANF.BlobToRawPtr ((ANF.StringLiteral source))); (ANF.RawPtrToBlob ((ANF.StringLiteral source))); (ANF.RawPtrToInt128 ((ANF.StringLiteral source))); (ANF.RawPtrToUInt128 ((ANF.StringLiteral source))); (ANF.DictToRawPtr ((ANF.StringLiteral source))); (ANF.RawPtrToDict ((ANF.StringLiteral source), (ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (ANF.ListToRawPtr ((ANF.StringLiteral source))); (ANF.FixedBlockToRawPtr ((ANF.StringLiteral source))); (ANF.RawPtrToList ((ANF.StringLiteral source), (ANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))); (ANF.RefCountIncString ((ANF.StringLiteral source))); (ANF.RefCountDecString ((ANF.StringLiteral source))); (ANF.RefCountIncBlob ((ANF.StringLiteral source))); (ANF.RefCountDecBlob ((ANF.StringLiteral source))); (ANF.RefCountIncInt ((ANF.StringLiteral source))); (ANF.RefCountDecInt ((ANF.StringLiteral source))); (ANF.RandomInt64); (ANF.DateTimeNow); (ANF.Sleep ((ANF.StringLiteral source))); (ANF.CliNative ((ANF.Execute), [(ANF.StringLiteral source); (ANF.StringLiteral source)])); (ANF.FloatToString ((ANF.StringLiteral source)))]
    let definition body : ANF.Function={Id=AST.functionId UInt64.MaxValue;Name=source;TypedParams=[];ReturnType=AST.TRecord (source,[AST.TInt64]);ReturnOwnership=ANF.OwnedReturn;Body=body}
    let anf=anfExprs |> List.map (fun operation ->
        let body=ANF.Let (ANF.TempId 3,operation,ANF.Join ({ANF.TypedParam.Id=ANF.TempId 4;Type=AST.TFloat64},ANF.If (ANF.BoolLiteral true,ANF.Return (ANF.FloatLiteral -0.),ANF.Return (ANF.StringLiteral source)),ANF.Jump (ANF.TempId 4,ANF.Var (ANF.TempId 3))))
        let func=definition body
        let program=ANF.Program ([func],ANF.Return (ANF.FuncRef (AST.functionId UInt64.MaxValue)))
        tuple [enc (ANFPrinter.formatANF program);enc (ANFPrinter.formatANFFunction (FunctionIdMap.ofList [AST.functionId UInt64.MaxValue,"external"]) func);[None;Some "";Some "MISSING";Some source] |> List.map (fun filter -> [false;true] |> List.map (fun summary -> ANFPrinter.formatANFDump filter summary program) |> enc) |> list]) |> list
    let mir=[AST.TInt64;AST.TFloat64;AST.TRecord (source,[AST.TString])] |> List.map (fun typ -> [MIR.Int64Const Int64.MinValue;MIR.FloatSymbol -0.;MIR.StringSymbol source;MIR.FuncAddr (AST.functionId UInt64.MaxValue)] |> List.map (fun operand -> operations typ operand |> List.map (fun instr ->
        let entry=MIR.Label "entry"
        let block name term : MIR.BasicBlock={Label=MIR.Label name;Instrs=[instr];Terminator=term}
        let cfg : MIR.CFG={Entry=entry;Blocks=Map.ofList [entry,block "entry" (MIR.Branch (operand,MIR.Label "😀",MIR.Label ""));MIR.Label "😀",block "😀" (MIR.Jump entry);MIR.Label "",block "" (MIR.Ret operand)]}
        let func : MIR.Function={Id=fid 400;Name=source;TypedParams=[];ReturnType=typ;CFG=cfg;FloatRegs=Set.empty}
        let program=MIR.Program ([func],Map.empty,Map.empty)
        tuple [enc (MIRPrinter.formatMIR program);enc (MIRPrinter.formatMIRWithFunctionNames (FunctionIdMap.ofList [fid 200,"external";fid 400,"overridden"]) program);[None;Some "";Some "MISSING";Some source] |> List.map (fun filter -> [false;true] |> List.map (fun summary -> MIRPrinter.formatMIRDump filter summary program) |> enc) |> list]) |> list) |> list) |> list
    tuple [escaping;filters;halfPairs;anf;mir]

let mirSCCPObservation (source:string) =
    let enc value=closureAnalysisEncode value
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let attempt action=enc (try Ok (action ()) with error -> Error error.Message)
    let reg n=MIR.VReg n
    let v n=MIR.Register (reg n)
    let fid n=AST.functionId (uint64 n)
    let label text=MIR.Label text
    let block name instrs terminator : MIR.BasicBlock={Label=label name;Instrs=instrs;Terminator=terminator}
    let graph entry (blocks:MIR.BasicBlock list) : MIR.CFG={Entry=label entry;Blocks=blocks |> List.map (fun block -> block.Label,block) |> Map.ofList}
    let operations typ operand=[
            MIR.Mov (MIR.VReg 1, operand, Some typ);
            MIR.BinOp (MIR.VReg 1, MIR.Div, operand, operand, typ);
            MIR.UnaryOp (MIR.VReg 1, MIR.Not, operand);
            MIR.Call (MIR.VReg 1, fid 200, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.TailCall (fid 200, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.IndirectCall (MIR.VReg 1, operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.IndirectTailCall (operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.ClosureAlloc (MIR.VReg 1, fid 200, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.ClosureCall (MIR.VReg 1, operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.ClosureTailCall (operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ]);
            MIR.HeapAlloc (MIR.VReg 1, 3);
            MIR.HeapStore (MIR.VReg 1, 3, operand, Some typ);
            MIR.HeapLoad (MIR.VReg 1, MIR.VReg 2, 3, Some typ);
            MIR.StringConcat (MIR.VReg 1, operand, operand, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.CanonicalBufferEq (MIR.VReg 1, MemoryModel.Utf8String, operand, operand);
            MIR.RefCountInc (MIR.VReg 1, 3, MIR.GenericHeap, None);
            MIR.RefCountDec (MIR.VReg 1, 3, MIR.GenericHeap, None);
            MIR.Print (operand, typ);
            MIR.StdoutWrite (3, operand, true);
            MIR.StdinReadLine (MIR.VReg 1);
            MIR.RuntimeError (source);
            MIR.RuntimeErrorString (operand);
            MIR.FileReadBlob (MIR.VReg 1, operand);
            MIR.FileExists (MIR.VReg 1, operand);
            MIR.FileWriteBlob (MIR.VReg 1, operand, operand);
            MIR.FileAppendText (MIR.VReg 1, operand, operand);
            MIR.FileDelete (MIR.VReg 1, operand);
            MIR.FileCreateDirectory (MIR.VReg 1, operand);
            MIR.FileSetExecutable (MIR.VReg 1, operand);
            MIR.FileWriteFromPtr (MIR.VReg 1, operand, operand, operand);
            MIR.FloatSqrt (MIR.VReg 1, operand);
            MIR.FloatAbs (MIR.VReg 1, operand);
            MIR.FloatNeg (MIR.VReg 1, operand);
            MIR.Int64ToFloat (MIR.VReg 1, operand);
            MIR.FloatToInt64 (MIR.VReg 1, operand);
            MIR.FloatToBits (MIR.VReg 1, operand);
            MIR.RawAlloc (MIR.VReg 1, operand);
            MIR.MappedAlloc (MIR.VReg 1, operand);
            MIR.RawFree (operand);
            MIR.MappedFree (operand);
            MIR.RawGet (MIR.VReg 1, operand, operand, Some typ);
            MIR.RawGetByte (MIR.VReg 1, operand, operand);
            MIR.RawWriteWord (operand, operand, operand);
            MIR.RawWriteByte (operand, operand, operand);
            MIR.RawSlotInit (operand, operand, operand, typ);
            MIR.StringToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToString (MIR.VReg 1, operand);
            MIR.BlobToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToBlob (MIR.VReg 1, operand);
            MIR.DictToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToDict (MIR.VReg 1, operand, operand);
            MIR.ListToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToList (MIR.VReg 1, operand, operand);
            MIR.RefCountIncString (operand);
            MIR.RefCountDecString (operand);
            MIR.RefCountIncBlob (operand);
            MIR.RefCountDecBlob (operand);
            MIR.RefCountIncInt (operand);
            MIR.RefCountDecInt (operand);
            MIR.RandomInt64 (MIR.VReg 1);
            MIR.DateTimeNow (MIR.VReg 1);
            MIR.Sleep (3, MIR.VReg 2, operand);
            MIR.CliNative (MIR.VReg 1, MIR.HostOS, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.FloatToString (MIR.VReg 1, operand);
            MIR.Phi (MIR.VReg 1, [operand,MIR.Label source;MIR.Register (MIR.VReg 3),MIR.Label "other"], Some typ);
            MIR.CoverageHit (3)        ]
    let transforms=[MIRSparseConditionalConstants.applySparseConditionalConstantPropagation;MIRSparseConditionalConstants.applySparseConditionalConstantPropagationWithCallResults (fun fn -> if fn=fid 200 then Some (MIR.BoolConst true) else None);MIRSparseConditionalConstants.applySparseConditionalSimplification]
    let observe cfg=transforms |> List.map (fun transform -> attempt (fun () -> transform cfg)) |> list
    let types=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TFloat64;AST.TBool;AST.TString;AST.TChar;AST.TDateTime;AST.TUnit;AST.TInt128;AST.TSum ("Option",[AST.TInt64]);AST.TList AST.TInt64;AST.TDict (AST.TInt64,AST.TInt64);AST.TTuple [AST.TInt64]]
    let instructionGraphs typ operand instr =
        let instructions=[MIR.Mov (reg 2,operand,Some typ);MIR.Mov (reg 3,v 2,Some typ);instr]
        let straight=graph source [block source instructions (MIR.Ret (v 1))]
        let conditional=graph source [block source instructions (MIR.Branch (MIR.BoolConst true,label "child",label "dead"));block "child" [] (MIR.Ret (v 1));block "dead" [] (MIR.Ret (v 2))]
        [straight;conditional] |> List.map observe |> list
    let instructionCases=[AST.TInt64;AST.TInt8;AST.TUInt32;AST.TFloat64;AST.TString;AST.TBool] |> List.map (fun typ -> [MIR.Int64Const 0L;MIR.FloatSymbol (BitConverter.Int64BitsToDouble 0x7ff8000000000001L);MIR.StringSymbol source;v 4] |> List.map (fun operand -> operations typ (v 3) |> List.map (instructionGraphs typ operand) |> list) |> list) |> list
    let comparisons=[MIR.Eq;MIR.Neq;MIR.Lt;MIR.Gt;MIR.Lte;MIR.Gte]
    let paths=types |> List.map (fun typ -> comparisons |> List.map (fun op -> [Int64.MinValue;-1L;0L;255L;Int64.MaxValue] |> List.map (fun bound ->
        let entry=block source [MIR.BinOp (reg 1,op,v 4,MIR.Int64Const bound,typ);MIR.UnaryOp (reg 2,MIR.Not,v 1);MIR.Mov (reg 3,v 2,Some AST.TBool)] (MIR.Branch (v 3,label "yes",label "no"))
        let yes=block "yes" [MIR.BinOp (reg 5,op,v 4,MIR.Int64Const bound,typ);MIR.BinOp (reg 6,MIR.And,v 5,v 1,AST.TBool)] (MIR.Branch (v 6,label "a",label "b"))
        let no=block "no" [MIR.BinOp (reg 7,MIR.Or,v 1,MIR.BoolConst false,AST.TBool)] (MIR.Branch (v 7,label "a",label "b"))
        observe (graph source [entry;yes;no;block "a" [] (MIR.Ret (MIR.Int64Const 1L));block "b" [] (MIR.Ret (MIR.Int64Const 2L))])) |> list) |> list) |> list
    let heaps=[AST.TTuple [AST.TInt64];AST.TSum ("Option",[AST.TInt64]);AST.TList AST.TInt64;AST.TDict (AST.TInt64,AST.TInt64)] |> List.map (fun typ -> [1;2;16;17] |> List.map (fun count ->
        let arms=List.init count (fun i -> let name="arm"+string i in block name [MIR.HeapAlloc (reg (10+i),16);MIR.HeapStore (reg (10+i),8,MIR.Int64Const (if i=count-1 && count>1 then 1L else 0L),Some AST.TInt64)] (MIR.Jump (label "join")))
        let branches=List.init (max 0 (count-1)) (fun i -> block (if i=0 then source else "branch"+string i) [] (MIR.Branch (v 4,label ("arm"+string i),label (if i=count-2 then "arm"+string (count-1) else "branch"+string (i+1)))))
        let sources=arms |> List.mapi (fun i block -> v (10+i),block.Label)
        let entry=if count=1 then [block source [] (MIR.Jump (label "arm0"))] else branches
        observe (graph source (entry @ arms @ [block "join" [MIR.Phi (reg 1,sources,Some typ);MIR.HeapLoad (reg 2,reg 1,8,Some AST.TInt64);MIR.BinOp (reg 3,MIR.Eq,v 2,MIR.Int64Const 0L,AST.TInt64)] (MIR.Branch (v 3,label "yes",label "no"));block "yes" [] (MIR.Ret (v 2));block "no" [] (MIR.Ret (v 2))]))) |> list) |> list
    let floats=[0.;-0.;1.;-1.;Double.PositiveInfinity;Double.NegativeInfinity;BitConverter.Int64BitsToDouble 0x7ff8000000000001L;BitConverter.Int64BitsToDouble 0xfff8000000000002L]
    let arithmetic=[MIR.Add;MIR.Sub;MIR.Mul;MIR.Div;MIR.Mod;MIR.Eq;MIR.Neq;MIR.Lt;MIR.Gt;MIR.Lte;MIR.Gte] |> List.map (fun op -> floats |> List.map (fun a -> floats |> List.map (fun b -> observe (graph source [block source [MIR.Mov (reg 2,MIR.FloatSymbol a,Some AST.TFloat64);MIR.Mov (reg 3,MIR.FloatSymbol b,Some AST.TFloat64);MIR.BinOp (reg 1,op,v 2,v 3,AST.TFloat64);MIR.FloatAbs (reg 5,v 1);MIR.FloatToInt64 (reg 6,v 5);MIR.FloatToBits (reg 7,v 5)] (MIR.Branch (v 1,label "yes",label "no"));block "yes" [] (MIR.Ret (v 7));block "no" [] (MIR.Ret (v 6))])) |> list) |> list) |> list
    let cfgs=[graph source [block source [MIR.Mov (reg 1,MIR.Int64Const 0L,Some AST.TInt64);MIR.Mov (reg 2,v 1,Some AST.TInt64)] (MIR.Ret (v 2))];graph source [block source [MIR.Call (reg 1,fid 200,[],[],AST.TBool)] (MIR.Branch (v 1,label "yes",label "no"));block "yes" [] (MIR.Ret (MIR.Int64Const 1L));block "no" [] (MIR.Ret (MIR.Int64Const 2L))];graph source [block source [] (MIR.Jump (label "missing"))];graph source []]
    let functionDef id name cfg : MIR.Function={Id=fid id;Name=name;TypedParams=[];ReturnType=AST.TInt64;CFG=cfg;FloatRegs=Set.empty}
    let leaf=functionDef 200 "leaf" (graph "leaf" [block "leaf" [] (MIR.Ret (MIR.BoolConst true))])
    let programs=cfgs |> List.map (fun cfg -> [0..15] |> List.map (fun bits ->
        let options : MIROptimizationFacts.OptimizeOptions={EnableSCCP=bits &&& 1<>0;EnableCSE=bits &&& 2<>0;EnableDCE=bits &&& 4<>0;EnableLICM=bits &&& 8<>0}
        let func=functionDef 400 source cfg
        let program=MIR.Program ([leaf;func],Map.empty,Map.empty)
        tuple [attempt (fun () -> MIR_Optimize.optimizeCFGOnce options cfg);attempt (fun () -> MIR_Optimize.optimizeCFGWithOptions options cfg);attempt (fun () -> MIR_Optimize.optimizeFunctionWithOptions options func);enc (MIR_Optimize.constantReturnOperand func);attempt (fun () -> MIR_Optimize.optimizeProgramWithOptions options program);
               attempt (fun () -> let timings=ResizeArray<string*bool>() in let program=MIR_Optimize.optimizeProgramWithOptionsAndTrace (Some (fun name elapsed -> timings.Add(name,elapsed>=0.))) options program in program,List.ofSeq timings)]) |> list) |> list
    tuple [instructionCases;paths;heaps;arithmetic;cfgs |> List.map observe |> list;programs]

let mirLoopObservation (source:string) =
    let enc value=closureAnalysisEncode value
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let attempt action=enc (try Ok (action ()) with error -> Error error.Message)
    let reg n=MIR.VReg n
    let v n=MIR.Register (reg n)
    let label text=MIR.Label text
    let block name instrs terminator : MIR.BasicBlock={Label=label name;Instrs=instrs;Terminator=terminator}
    let graph entry (blocks:MIR.BasicBlock list) : MIR.CFG={Entry=label entry;Blocks=blocks |> List.map (fun block -> block.Label,block) |> Map.ofList}
    let types=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TFloat64;AST.TBool;AST.TChar;AST.TDateTime;AST.TString;AST.TUnit;AST.TInt128;AST.TUInt128;AST.TTuple [AST.TInt64]]
    let make typ scale offset variant=
        let scaleInstr=match scale with 0 -> MIR.BinOp (reg 6,MIR.Mul,v 1,v 4,typ) | 1 -> MIR.BinOp (reg 6,MIR.Mul,v 4,v 1,typ) | _ -> MIR.BinOp (reg 6,MIR.Shl,v 1,MIR.Int64Const 1L,typ)
        let affine=match offset with 0 -> MIR.BinOp (reg 7,MIR.Add,v 6,v 5,typ) | 1 -> MIR.BinOp (reg 7,MIR.Add,v 5,v 6,typ) | _ -> MIR.BinOp (reg 7,MIR.Sub,v 6,v 5,typ)
        let outside=if variant=2 then [MIR.Int64Const 0L,label "left";MIR.Int64Const 2L,label "right"] else [MIR.Int64Const 0L,label source]
        let phis=[MIR.Phi (reg 1,outside @ [v 10,label "latch"],Some typ);MIR.Phi (reg 3,outside @ [v 11,label "latch"],Some typ)]
        let invariantPhi=if variant=1 then [MIR.Phi (reg 30,[v 4,label source;v 30,label "latch"],Some typ)] else []
        let bound=if variant=4 then v 1 else if variant=5 then MIR.Int64Const 4L else v 2
        let header=block "header" (phis @ invariantPhi @ [MIR.BinOp (reg 12,MIR.Gte,v 1,bound,AST.TInt64)]) (MIR.Branch (v 12,label "exit",label "latch"))
        let extra=match variant with
                  | 1 -> [MIR.BinOp (reg 40,MIR.Add,v 4,MIR.Int64Const 1L,typ);MIR.BinOp (reg 41,MIR.Mul,v 40,MIR.Int64Const 2L,typ);MIR.FloatAbs (reg 42,v 30)]
                  | 2 | 3 -> [MIR.BinOp (reg 40,MIR.Add,v 4,MIR.Int64Const 1L,typ)]
                  | 6 -> [MIR.UnaryOp (reg 40,MIR.Neg,v 6)]
                  | 8 -> [MIR.Call (reg 40,AST.functionId 200UL,[v 4],[typ],typ)]
                  | 9 -> [MIR.FloatNeg (reg 40,v 4);MIR.FloatToBits (reg 41,v 40)]
                  | _ -> []
        let step=if variant=7 then 2L else 1L
        let latch=block "latch" ([scaleInstr;affine;MIR.BinOp (reg 11,MIR.Add,v 3,v 7,typ);MIR.BinOp (reg 10,MIR.Add,v 1,MIR.Int64Const step,typ)] @ extra) (MIR.Jump (label "header"))
        let exit=block "exit" [MIR.Mov (reg 13,v 3,Some typ)] (MIR.Ret (v 13))
        let entry=if variant=2 then [block source [] (MIR.Branch (v 2,label "left",label "right"));block "left" [] (MIR.Jump (label "header"));block "right" [] (MIR.Jump (label "header"))] else [block source [] (if variant=3 then MIR.Branch (v 2,label "header",label "exit") else MIR.Jump (label "header"))]
        let collision=if variant=5 then [block "latch_unroll_second" [] (MIR.Ret (v 2));block "exit_unroll_remainder" [MIR.Mov (reg 2147483646,v 4,Some typ)] (MIR.Ret (v 2));block "header_preheader" [] (MIR.Ret (v 2))] else []
        graph source (entry @ [header;latch;exit] @ collision)
    let union (typ:string) (case:string) (fields:JsonNode list) : JsonNode =
        let node=JsonObject()
        node["type"] <- JsonValue.Create typ
        node["case"] <- JsonValue.Create case
        node["fields"] <- JsonArray(Array.ofList fields)
        node
    let known cfg (functions:Set<AST.FunctionId>) =
        try
            let option=rcInternalCall<obj> "MIRLoopTopology" "tryBuildLoopTopology" [|box cfg|]
            let value=
                if isNull option then union "FSharpOption" "None" []
                else
                    let _,fields=FSharpValue.GetUnionFields(option,option.GetType())
                    let result=rcInternalCall<obj> "MIRLoopInvariantMotion" "applyLoopInvariantCodeMotionWithEffectFreeCalls" [|box functions;fields[0];box cfg|]
                    union "FSharpOption" "Some" [encode (result.GetType()) result]
            union "FSharpResult" "Ok" [value]
        with error -> union "FSharpResult" "Error" [encodeString error.Message]
    let observe typ scale offset variant =
        let cfg=make typ scale offset variant
        let publicResults=[MIRInduction.applyAffineInductionStrengthReduction;MIRUnrolling.applyCountedLoopUnrolling;MIRLoopInvariantMotion.applyLoopInvariantCodeMotion] |> List.map (fun transform -> attempt (fun () -> transform cfg)) |> list
        let knownResults=[Set.empty;Set.singleton (AST.functionId 200UL)] |> List.map (known cfg) |> list
        tuple [enc cfg;enc (rcInternalCall<int> "MIRInduction" "nextRegisterId" [|box cfg|]);publicResults;knownResults]
    types |> List.map (fun typ -> [0;1;2] |> List.map (fun scale -> [0;1;2] |> List.map (fun offset -> [0;1;2;3;4;5;6;7;8;9] |> List.map (observe typ scale offset) |> list) |> list) |> list) |> list

let mirCSEObservation (source:string) =
    let enc value=closureAnalysisEncode value
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let attempt action=enc (try Ok (action ()) with error -> Error error.Message)
    let fid index=AST.functionId (uint64 index)
    let reg n=MIR.VReg n
    let v n=MIR.Register (reg n)
    let label text=MIR.Label text
    let block name instrs terminator : MIR.BasicBlock={Label=label name;Instrs=instrs;Terminator=terminator}
    let graph entry (blocks:MIR.BasicBlock list) : MIR.CFG={Entry=label entry;Blocks=blocks |> List.map (fun block -> block.Label,block) |> Map.ofList}
    let operations typ operand=[
            MIR.Mov (MIR.VReg 1, operand, Some typ);
            MIR.BinOp (MIR.VReg 1, MIR.Div, operand, operand, typ);
            MIR.UnaryOp (MIR.VReg 1, MIR.Not, operand);
            MIR.Call (MIR.VReg 1, fid 200, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.TailCall (fid 200, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.IndirectCall (MIR.VReg 1, operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.IndirectTailCall (operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.ClosureAlloc (MIR.VReg 1, fid 200, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.ClosureCall (MIR.VReg 1, operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.ClosureTailCall (operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ]);
            MIR.HeapAlloc (MIR.VReg 1, 3);
            MIR.HeapStore (MIR.VReg 1, 3, operand, Some typ);
            MIR.HeapLoad (MIR.VReg 1, MIR.VReg 2, 3, Some typ);
            MIR.StringConcat (MIR.VReg 1, operand, operand, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.CanonicalBufferEq (MIR.VReg 1, MemoryModel.Utf8String, operand, operand);
            MIR.RefCountInc (MIR.VReg 1, 3, MIR.GenericHeap, None);
            MIR.RefCountDec (MIR.VReg 1, 3, MIR.GenericHeap, None);
            MIR.Print (operand, typ);
            MIR.StdoutWrite (3, operand, true);
            MIR.StdinReadLine (MIR.VReg 1);
            MIR.RuntimeError (source);
            MIR.RuntimeErrorString (operand);
            MIR.FileReadBlob (MIR.VReg 1, operand);
            MIR.FileExists (MIR.VReg 1, operand);
            MIR.FileWriteBlob (MIR.VReg 1, operand, operand);
            MIR.FileAppendText (MIR.VReg 1, operand, operand);
            MIR.FileDelete (MIR.VReg 1, operand);
            MIR.FileCreateDirectory (MIR.VReg 1, operand);
            MIR.FileSetExecutable (MIR.VReg 1, operand);
            MIR.FileWriteFromPtr (MIR.VReg 1, operand, operand, operand);
            MIR.FloatSqrt (MIR.VReg 1, operand);
            MIR.FloatAbs (MIR.VReg 1, operand);
            MIR.FloatNeg (MIR.VReg 1, operand);
            MIR.Int64ToFloat (MIR.VReg 1, operand);
            MIR.FloatToInt64 (MIR.VReg 1, operand);
            MIR.FloatToBits (MIR.VReg 1, operand);
            MIR.RawAlloc (MIR.VReg 1, operand);
            MIR.MappedAlloc (MIR.VReg 1, operand);
            MIR.RawFree (operand);
            MIR.MappedFree (operand);
            MIR.RawGet (MIR.VReg 1, operand, operand, Some typ);
            MIR.RawGetByte (MIR.VReg 1, operand, operand);
            MIR.RawWriteWord (operand, operand, operand);
            MIR.RawWriteByte (operand, operand, operand);
            MIR.RawSlotInit (operand, operand, operand, typ);
            MIR.StringToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToString (MIR.VReg 1, operand);
            MIR.BlobToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToBlob (MIR.VReg 1, operand);
            MIR.DictToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToDict (MIR.VReg 1, operand, operand);
            MIR.ListToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToList (MIR.VReg 1, operand, operand);
            MIR.RefCountIncString (operand);
            MIR.RefCountDecString (operand);
            MIR.RefCountIncBlob (operand);
            MIR.RefCountDecBlob (operand);
            MIR.RefCountIncInt (operand);
            MIR.RefCountDecInt (operand);
            MIR.RandomInt64 (MIR.VReg 1);
            MIR.DateTimeNow (MIR.VReg 1);
            MIR.Sleep (3, MIR.VReg 2, operand);
            MIR.CliNative (MIR.VReg 1, MIR.HostOS, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.FloatToString (MIR.VReg 1, operand);
            MIR.Phi (MIR.VReg 1, [operand,MIR.Label source;MIR.Register (MIR.VReg 3),MIR.Label "other"], Some typ);
            MIR.CoverageHit (3)        ]
    let types=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TFloat64;AST.TBool;AST.TChar;AST.TDateTime;AST.TString;AST.TUnit;AST.TInt128;AST.TUInt128;AST.TTuple [AST.TInt64]]
    let binary=[MIR.Add;MIR.Sub;MIR.Mul;MIR.Div;MIR.Mod;MIR.Shl;MIR.Shr;MIR.BitAnd;MIR.BitOr;MIR.BitXor;MIR.Eq;MIR.Neq;MIR.Lt;MIR.Gt;MIR.Lte;MIR.Gte;MIR.And;MIR.Or]
    let operands=[v 2;MIR.Int64Const -1L;MIR.BoolConst true;MIR.FloatSymbol -0.;MIR.FloatSymbol 0.;MIR.FloatSymbol (BitConverter.Int64BitsToDouble 0x7ff8000000000001L);MIR.StringSymbol "😀";MIR.StringSymbol "";MIR.FuncAddr (AST.functionId 0x8000000000000000UL);MIR.FuncAddr (AST.functionId 1UL)]
    let keys=binary |> List.map (fun op -> operands |> List.map (fun a -> operands |> List.map (fun b -> let a',b'=MIRCommonExpressions.normalizeOperands op a b in tuple [enc (MIRCommonExpressions.isCommutative op);enc a';enc b';enc (MIRCommonExpressions.makeBinExprKey op a b AST.TInt64)]) |> list) |> list) |> list
    let optimize graph=[Set.empty;Set.singleton (fid 200)] |> List.map (fun functions -> attempt (fun () -> let once,changed=MIRCommonExpressions.applyCSEWithEffectFreeCalls functions graph in let twice,changedAgain=MIRCommonExpressions.applyCSEWithEffectFreeCalls functions once in once,changed,twice,changedAgain)) |> list
    let barriers=types |> List.map (fun typ -> operations typ (v 2) |> List.map (fun instruction ->
        let expressions dest=[MIR.BinOp (reg dest,MIR.Add,v 2,v 3,typ);MIR.UnaryOp (reg (dest+1),MIR.Not,v 2);MIR.HeapLoad (reg (dest+2),reg 2,3,Some typ);MIR.Call (reg (dest+3),fid 200,[v 2],[typ],typ)]
        [graph source [block source (expressions 10 @ [instruction] @ expressions 20) (MIR.Ret (v 20))];graph source [block source (expressions 10 @ [instruction]) (MIR.Jump (label "child"));block "child" (expressions 20) (MIR.Ret (v 20))]] |> List.map optimize |> list) |> list) |> list
    let joins=types |> List.map (fun typ -> binary |> List.map (fun op ->
        let expression dest=MIR.BinOp (reg dest,op,v 2,v 3,typ)
        let join=block "join" [expression 20;MIR.UnaryOp (reg 21,MIR.Not,v 2)] (MIR.Ret (v 20))
        let entry=block source [] (MIR.Branch (v 2,label "left",label "right"))
        let left=block "left" [expression 10;MIR.UnaryOp (reg 11,MIR.Not,v 2)] (MIR.Jump (label "join"))
        let right=block "right" [] (MIR.Jump (label "join"))
        [graph source [entry;left;right;join];graph source [entry;left;{right with Terminator=MIR.Branch (v 2,label "join",label "exit")};join;block "exit" [] (MIR.Ret (v 2))];graph source [entry;left;right;{join with Instrs=[MIR.Mov (reg 2,v 3,Some typ);expression 20]}];graph source [entry;left;{right with Instrs=[expression 12]};join];graph source [block source [] (MIR.Ret (v 2));left;right;join]] |> List.map optimize |> list) |> list) |> list
    tuple [keys;barriers;joins]

let mirSSAObservation (source:string) =
    let enc value=closureAnalysisEncode value
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let attempt action=enc (try Ok (action ()) with error -> Error error.Message)
    let id value=MIR.VReg value
    let v value=MIR.Register (id value)
    let label text=MIR.Label text
    let block name instructions terminator : MIR.BasicBlock={Label=label name;Instrs=instructions;Terminator=terminator}
    let graph entry (blocks:MIR.BasicBlock list) : MIR.CFG={Entry=label entry;Blocks=blocks |> List.map (fun block -> block.Label,block) |> Map.ofList}
    let graphs typ=
        let move value=MIR.Mov (id 1,value,Some typ)
        let left=block "left" [move (v 2)] (MIR.Jump (label "join"))
        let right=block "right" [move (MIR.Int64Const 0L)] (MIR.Jump (label "join"))
        let join=block "join" [] (MIR.Ret (v 1))
        let entry=block source [] (MIR.Branch (v 2,label "left",label "right"))
        let header=block "header" [] (MIR.Branch (v 2,label "body",label "exit"))
        let body=block "body" [move (v 1)] (MIR.Jump (label "header"))
        let exit=block "exit" [] (MIR.Ret (v 1))
        [graph source [block source [MIR.Mov (id 1,v 1,Some typ)] (MIR.Ret (v 1))];
         graph source [entry;left;right;join];
         graph source [entry;left;{right with Instrs=[MIR.Mov (id 1,v 2,Some AST.TBool)]};join];
         graph source [entry;left;right;join;block "unreachable" [move (v 1)] (MIR.Ret (v 1))];
         graph source [block source [move (v 2)] (MIR.Jump (label "header"));header;body;exit];
         graph source [block source [move (v 2)] (MIR.Branch (v 2,label "join",label "join"));join];
         graph source [block source [MIR.RuntimeErrorString (v 1)] (MIR.Ret (v 2))];
         graph source [block source [MIR.Mov (id 2147474000,v 2,Some typ);MIR.Mov (id 1,MIR.Register (id 2147474000),Some typ)] (MIR.Ret (v 1))];
         graph source [block source [] (MIR.Jump (label "missing"))];graph source [];
         graph source [block source [MIR.Phi (id 1,[v 2,label source;v 1,label "unreachable"],Some typ)] (MIR.Ret (v 1));block "unreachable" [] (MIR.Jump (label source))];
   graph source [block source [MIR.Mov (id 3, v 2, Some typ)] (MIR.Jump (label "middle")); block "middle" [MIR.Phi (id 4, [v 3, label source], Some typ)] (MIR.Jump (label "exit")); block "exit" [MIR.Phi (id 5, [v 4, label "middle"], Some typ)] (MIR.Ret (v 5))];
   graph source [entry; {left with Instrs = []}; {right with Instrs = []}; block "join" [MIR.Phi (id 3, [v 2, label "left"; MIR.Int64Const 0L, label "right"], Some typ); MIR.Mov (id 4, v 3, Some typ)] (MIR.Ret (v 4))];
   graph source [block source [] (MIR.Jump (label "empty")); block "empty" [] (MIR.Jump (label "join")); block "join" [MIR.Phi (id 3, [v 2, label "empty"], Some typ)] (MIR.Ret (v 3))];
   graph source [block source [] (MIR.Jump (label "a")); block "a" [] (MIR.Jump (label "b")); block "b" [] (MIR.Jump (label "a"))];
   graph source [entry; {left with Instrs = [MIR.Mov (id 3, v 2, Some typ)]}; {right with Instrs = []}; block "join" [] (MIR.Ret (v 3))];
   graph source [block source [MIR.Mov (id 2, v 2, Some typ)] (MIR.Ret (v 2))];
   graph source [block source [MIR.Mov (id 3, v 4, Some typ); MIR.Mov (id 4, v 2, Some typ)] (MIR.Ret (v 3))];
   graph source [entry; {left with Instrs = []}; {right with Instrs = []}; block "join" [MIR.Phi (id 3, [v 2, label "left"; v 2, label "left"], Some typ)] (MIR.Ret (v 3))];
   graph source [entry; {left with Instrs = []}; {right with Instrs = []}; block "join" [MIR.Phi (id 3, [v 2, label "left"; v 2, label "right"], Some typ); MIR.RuntimeErrorString (v 3)] (MIR.Ret (v 3))]]
    let observe typ (cfg:MIR.CFG)=
        let parameters=[id 2]
        let floats=if typ=AST.TFloat64 then Set.ofList [1;2] else Set.empty
        let func : MIR.Function={Id=AST.functionId 200UL;Name=source;TypedParams=[{MIR.TypedMIRParam.Reg=id 2;Type=typ}];ReturnType=typ;CFG=cfg;FloatRegs=floats}
        let predecessors=SSA_Construction.buildPredecessors cfg
        let dominators=SSA_Construction.computeDominators cfg predecessors
        let frontier=SSA_Construction.computeDominanceFrontier cfg predecessors dominators
        tuple [enc predecessors;enc dominators;enc frontier;
               cfg.Blocks |> Map.toList |> List.map (fun (_,block) -> SSA_Construction.getBlockDefs block,SSA_Construction.getBlockUses block,SSA_Construction.getSuccessors block) |> enc;
               SSA_Construction.getAllDefs cfg |> enc;
               attempt (fun () -> SSA_Construction.computeLiveness cfg);
               attempt (fun () -> let input,_=SSA_Construction.computeLiveness cfg in SSA_Construction.insertPhiNodes cfg frontier predecessors input parameters [typ]);
               attempt (fun () -> SSA_Construction.convertFunctionToSSA func);
               attempt (fun () -> SSA_Construction.renameCFG cfg dominators floats parameters);
               SSA_Construction.buildDomTree dominators |> enc;
               attempt (fun () -> MIR_SSA_Verify.verifyFunction func);
               attempt (fun () -> MIR_SSA_Verify.verifyFunction (SSA_Construction.convertFunctionToSSA func));
               MIRLoopTopology.cfgHasReachableCycle cfg |> enc;
               attempt (fun () -> MIRLoopTopology.findNaturalLoops cfg);
               [MIRControlFlow.mergeLinearBlocks;MIRControlFlow.simplifyEmptyBlocks;MIRControlFlow.simplifyRetPhiJoins] |> List.map (fun transform -> attempt (fun () -> transform cfg)) |> list;
               attempt (fun () -> let func,timings=SSA_Construction.convertFunctionToSSAWithTiming func in func,timings |> List.map (fun timing -> timing.Phase,timing.ElapsedMs>=0.))]
    [AST.TInt64;AST.TFloat64;AST.TString] |> List.map (fun typ -> graphs typ |> List.map (observe typ) |> list) |> list

let mirFoundationsObservation (source:string) =
    let enc value=closureAnalysisEncode value
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let tuple values=namedArray "tuple" (Array.ofList values)
    let fid index=AST.functionId (uint64 index)
    let reg index=MIR.VReg index
    let v index=MIR.Register (reg index)
    let cfg label instructions terminator : MIR.CFG = {Entry=MIR.Label label;Blocks=Map.ofList [MIR.Label label,{MIR.BasicBlock.Label=MIR.Label label;Instrs=instructions;Terminator=terminator}]}
    let functionDef index name graph : MIR.Function = {Id=fid index;Name=name;TypedParams=[{MIR.TypedMIRParam.Reg=reg 2;Type=AST.TInt64}];ReturnType=AST.TInt64;CFG=graph;FloatRegs=Set.empty}
    let variants=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TFloat64;AST.TBool;AST.TString;AST.TUnit;AST.TInt128;AST.TUInt128]
    let operands=[v 2;MIR.Int64Const 0L;MIR.FloatSymbol (BitConverter.Int64BitsToDouble 0x7ff8000000000001L);MIR.StringSymbol source]
    let operations typ operand=[
            MIR.Mov (MIR.VReg 1, operand, Some typ);
            MIR.BinOp (MIR.VReg 1, MIR.Div, operand, operand, typ);
            MIR.UnaryOp (MIR.VReg 1, MIR.Not, operand);
            MIR.Call (MIR.VReg 1, fid 200, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.TailCall (fid 200, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.IndirectCall (MIR.VReg 1, operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.IndirectTailCall (operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.ClosureAlloc (MIR.VReg 1, fid 200, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.ClosureCall (MIR.VReg 1, operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
            MIR.ClosureTailCall (operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ]);
            MIR.HeapAlloc (MIR.VReg 1, 3);
            MIR.HeapStore (MIR.VReg 1, 3, operand, Some typ);
            MIR.HeapLoad (MIR.VReg 1, MIR.VReg 2, 3, Some typ);
            MIR.StringConcat (MIR.VReg 1, operand, operand, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.CanonicalBufferEq (MIR.VReg 1, MemoryModel.Utf8String, operand, operand);
            MIR.RefCountInc (MIR.VReg 1, 3, MIR.GenericHeap, None);
            MIR.RefCountDec (MIR.VReg 1, 3, MIR.GenericHeap, None);
            MIR.Print (operand, typ);
            MIR.StdoutWrite (3, operand, true);
            MIR.StdinReadLine (MIR.VReg 1);
            MIR.RuntimeError (source);
            MIR.RuntimeErrorString (operand);
            MIR.FileReadBlob (MIR.VReg 1, operand);
            MIR.FileExists (MIR.VReg 1, operand);
            MIR.FileWriteBlob (MIR.VReg 1, operand, operand);
            MIR.FileAppendText (MIR.VReg 1, operand, operand);
            MIR.FileDelete (MIR.VReg 1, operand);
            MIR.FileCreateDirectory (MIR.VReg 1, operand);
            MIR.FileSetExecutable (MIR.VReg 1, operand);
            MIR.FileWriteFromPtr (MIR.VReg 1, operand, operand, operand);
            MIR.FloatSqrt (MIR.VReg 1, operand);
            MIR.FloatAbs (MIR.VReg 1, operand);
            MIR.FloatNeg (MIR.VReg 1, operand);
            MIR.Int64ToFloat (MIR.VReg 1, operand);
            MIR.FloatToInt64 (MIR.VReg 1, operand);
            MIR.FloatToBits (MIR.VReg 1, operand);
            MIR.RawAlloc (MIR.VReg 1, operand);
            MIR.MappedAlloc (MIR.VReg 1, operand);
            MIR.RawFree (operand);
            MIR.MappedFree (operand);
            MIR.RawGet (MIR.VReg 1, operand, operand, Some typ);
            MIR.RawGetByte (MIR.VReg 1, operand, operand);
            MIR.RawWriteWord (operand, operand, operand);
            MIR.RawWriteByte (operand, operand, operand);
            MIR.RawSlotInit (operand, operand, operand, typ);
            MIR.StringToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToString (MIR.VReg 1, operand);
            MIR.BlobToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToBlob (MIR.VReg 1, operand);
            MIR.DictToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToDict (MIR.VReg 1, operand, operand);
            MIR.ListToRawPtr (MIR.VReg 1, operand);
            MIR.RawPtrToList (MIR.VReg 1, operand, operand);
            MIR.RefCountIncString (operand);
            MIR.RefCountDecString (operand);
            MIR.RefCountIncBlob (operand);
            MIR.RefCountDecBlob (operand);
            MIR.RefCountIncInt (operand);
            MIR.RefCountDecInt (operand);
            MIR.RandomInt64 (MIR.VReg 1);
            MIR.DateTimeNow (MIR.VReg 1);
            MIR.Sleep (3, MIR.VReg 2, operand);
            MIR.CliNative (MIR.VReg 1, MIR.HostOS, [operand; MIR.Register (MIR.VReg 3)]);
            MIR.FloatToString (MIR.VReg 1, operand);
            MIR.Phi (MIR.VReg 1, [operand,MIR.Label source;MIR.Register (MIR.VReg 3),MIR.Label "other"], Some typ);
            MIR.CoverageHit (3)        ]
    let copyCases=[Map.empty;Map.ofList [reg 2,v 5;reg 3,MIR.Int64Const 0x4000000000000000L];Map.ofList [reg 2,v 3;reg 3,v 2];Map.ofList [reg 2,MIR.BoolConst true]]
    let instructionFacts=[AST.TInt64;AST.TFloat64] |> List.map (fun typ -> operands |> List.map (fun operand -> operations typ operand |> List.map (fun instruction ->
        enc (instruction,MIROptimizationFacts.getInstrDest instruction,MIROptimizationFacts.foldInstrUses (fun values reg -> values @ [reg]) [] instruction,MIROptimizationFacts.getInstrUses instruction,MIROptimizationFacts.hasSideEffects instruction,copyCases |> List.map (fun copies -> try Ok (MIRCopyPropagation.propagateCopyInstr copies instruction) with error -> Error error.Message))) |> list) |> list) |> list
    let binary=[MIR.Add;MIR.Sub;MIR.Mul;MIR.Div;MIR.Mod;MIR.Shl;MIR.Shr;MIR.BitAnd;MIR.BitOr;MIR.BitXor;MIR.Eq;MIR.Neq;MIR.Lt;MIR.Gt;MIR.Lte;MIR.Gte;MIR.And;MIR.Or]
    let scalarOperands=[MIR.Int64Const Int64.MinValue;MIR.Int64Const -1L;MIR.Int64Const 0L;MIR.Int64Const 1L;MIR.Int64Const 2L;MIR.Int64Const 256L;MIR.Int64Const Int64.MaxValue;MIR.BoolConst false;MIR.BoolConst true;v 2;MIR.FloatSymbol Double.NaN;MIR.FloatSymbol -0.]
    let constants=variants |> List.map (fun typ -> binary |> List.map (fun operation -> scalarOperands |> List.map (fun left -> scalarOperands |> List.map (fun right -> MIRConstants.tryFoldBinOp operation left right typ)))) |> enc
    let copies=copyCases |> List.map (fun values -> values,operands |> List.map (MIRCopyPropagation.resolveCopy values),MIRCopyPropagation.resolveCopyMap values) |> enc
    let graphCases=[cfg source [MIR.Mov (reg 1,v 2,Some AST.TInt64);MIR.Mov (reg 3,v 1,Some AST.TInt64);MIR.BinOp (reg 4,MIR.Add,v 3,MIR.Int64Const 1L,AST.TInt64)] (MIR.Ret (v 4));cfg source [MIR.Phi (reg 1,[v 1,MIR.Label source],Some AST.TInt64);MIR.BinOp (reg 3,MIR.Add,v 1,v 2,AST.TInt64)] (MIR.Ret (v 2));cfg source [MIR.Mov (reg 1,MIR.Int64Const 0L,Some AST.TString);MIR.Mov (reg 3,MIR.Int64Const 0L,None);MIR.Print (v 1,AST.TString)] (MIR.Ret (v 3));cfg source [MIR.Mov (reg 1,v 2,Some AST.TInt64);MIR.Phi (reg 1,[v 3,MIR.Label source],Some AST.TInt64)] (MIR.Ret (v 1));cfg source [MIR.BinOp (reg 1,MIR.Div,v 2,MIR.Int64Const 0L,AST.TInt64)] (MIR.Ret (MIR.Int64Const 0L))]
    let graphs=graphCases |> List.map (fun graph -> let copies=MIRCopyPropagation.buildCopyMap graph in let optimized,changed=MIRDeadCode.eliminateDeadCode graph in copies,MIRCopyPropagation.resolveCopyMap copies,optimized,changed) |> enc
    let make index name instructions terminator=functionDef index name (cfg name instructions terminator)
    let leaf=make 200 "leaf" [] (MIR.Ret (v 2))
    let caller=make 400 "caller" [MIR.Call (reg 1,fid 200,[v 2],[AST.TInt64],AST.TInt64)] (MIR.Ret (v 1))
    let cycle=make 500 "cycle" [] (MIR.Jump (MIR.Label "cycle"))
    let recursive=make 600 "recursive" [MIR.Call (reg 1,fid 600,[v 2],[AST.TInt64],AST.TInt64)] (MIR.Ret (v 1))
    let unknown=make 700 "unknown" [MIR.Call (reg 1,fid 900,[v 2],[AST.TInt64],AST.TInt64)] (MIR.Ret (v 1))
    let reader=make 800 "reader" [MIR.RawGet (reg 1,v 2,MIR.Int64Const 0L,Some AST.TInt64)] (MIR.Ret (v 1))
    let programs=[[];[caller;leaf];[leaf;caller;leaf];[cycle;recursive;unknown;reader;caller;leaf];[caller;recursive;{recursive with Id=fid 200};leaf]]
    let analyses=programs |> List.map (fun functions -> CallGraphSchedule.calleeFirst functions,MIROptimizationFacts.analyzeEffectFreeFunctions functions,MIROptimizationFacts.analyzePurityWithKnown FunctionIdMap.empty functions |> FunctionIdMap.toList |> List.map (fun (id,summary) -> MIR.FuncAddr id,summary)) |> enc
    let moveCases=[[1,v 2];[1,v 2;2,v 1];[1,MIR.Int64Const 0L;2,v 1];[1,v 1];[1,v 2;2,v 3;3,v 1];[1,v 2;1,v 3;2,v 1];[1,v 2;3,v 2;2,v 1];[1,MIR.Int64Const 0L;2,MIR.StringSymbol source]]
    let moves=moveCases |> List.map (fun values -> ParallelMoves.resolve values (function MIR.Register (MIR.VReg reg) -> Some reg | _ -> None)) |> enc
    let strings=[source;"😀";"é";"é";String [|char 0|];String [|char 0xd800|];String [|char 0xdfff|];String [|char 0xd800;char 0xdfff|];source]
    let stringPool=LiteralPool.createStringPool strings
    let floats=[0.;-0.;BitConverter.Int64BitsToDouble 0x7ff8000000000001L;BitConverter.Int64BitsToDouble 0x7ff8000000000002L;Double.PositiveInfinity;Double.NegativeInfinity;BitConverter.Int64BitsToDouble 0x7ff8000000000001L]
    let floatPool=LiteralPool.createFloatPool floats
    let pools=enc (stringPool,floatPool)
    tuple [instructionFacts;constants;copies;graphs;analyses;moves;pools]

let inliningObservation (source:string) =
    let enc value = closureAnalysisEncode value
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let attempt action = enc (try Ok (action ()) with error -> Error error.Message)
    let id index = ANF.TempId index
    let v index = ANF.Var (id index)
    let int value = ANF.IntLiteral (ANF.Int64 value)
    let fid index = AST.functionId (uint64 index)
    let func index name parameters ret body : ANF.Function =
        {Id=fid index;Name=name;TypedParams=parameters |> List.map (fun (index,typ) -> {ANF.TypedParam.Id=id index;ANF.TypedParam.Type=typ});ReturnType=ret;ReturnOwnership=ANF.OwnedReturn;Body=body}
    let finish bindings result = List.foldBack (fun (index,operation) body -> ANF.Let (id index,operation,body)) bindings (ANF.Return result)
    let tuple=AST.TTuple [AST.TInt64;AST.TInt64]
    let option=AST.TSum ("Darklang.Stdlib.Option.Option",[AST.TInt64])
    let descriptor : ANF.RecordDescriptor={SourceTypeName="Darklang.Stdlib.Option.Option";RuntimeTypeName="Darklang.Stdlib.Option.Option";TypeArgs=[AST.TInt64];Fields=["tag",AST.TInt64;"payload",AST.TInt64];ValueType=option}
    let branch typ operation=func 200 ("helper"+source) [1,AST.TInt64] typ (ANF.Let (id 2,ANF.Prim (ANF.Gte,v 1,int 0L),ANF.If (v 2,finish [3,operation (int 0L)] (v 3),finish [4,operation (int 1L)] (v 4))))
    let cases=[
        func 200 ("helper"+source) [1,AST.TInt64] AST.TInt64 (finish [2,ANF.Prim (ANF.Add,v 1,int 1L)] (v 2)),[2,AST.TInt64],AST.TInt64;
        branch AST.TInt64 (fun atom -> ANF.Prim (ANF.Mul,v 1,atom)),[2,AST.TBool;3,AST.TInt64;4,AST.TInt64],AST.TInt64;
        func 200 ("helper"+source) [1,AST.TInt64] tuple (finish [2,ANF.Prim (ANF.Add,v 1,int 1L);3,ANF.TupleAlloc [v 1;v 2]] (v 3)),[2,AST.TInt64;3,tuple],tuple;
        branch option (fun tag -> ANF.RecordAlloc (descriptor,[tag;v 1])),[2,AST.TBool;3,option;4,option],option;
        func 200 ("helper"+source) [1,AST.TInt64;5,AST.TInt64] AST.TInt64 (ANF.Let (id 2,ANF.Prim (ANF.Gte,v 1,int 4L),ANF.If (v 2,ANF.Return (v 5),finish [3,ANF.Prim (ANF.Add,v 1,int 1L);4,ANF.Call (fid 200,[v 3;v 5])] (v 4)))),[2,AST.TBool;3,AST.TInt64;4,AST.TInt64],AST.TInt64;
        func 200 ("helper"+source) [] AST.TString (ANF.Return (ANF.StringLiteral source)),[],AST.TString]
    let configs=[InliningCommon.defaultConfig;{InliningCommon.defaultConfig with MaxFunctionSize=0};{InliningCommon.defaultConfig with MaxInlineDepth=0};{InliningCommon.defaultConfig with MaxExternalInlineSites=0};{InliningCommon.defaultConfig with MaxBoundedLoopIterations=0};{InliningCommon.defaultConfig with MaxProjectedTupleInlineSites=0};{InliningCommon.defaultConfig with MaxProjectedTupleInlineSize=0}]
    let observeCase (helper:ANF.Function, types, ret) argument isExternal excluded config =
        let args=match helper.TypedParams with [] -> [] | [_] -> [argument] | _ -> [argument;int 7L]
        let after,bodyTypes,callerReturn =
            match ret with
            | AST.TTuple _ -> finish [21,ANF.TupleGet (v 20,0);22,ANF.TypedAtom (v 21,AST.TInt64);23,ANF.TupleGet (v 20,1);24,ANF.Prim (ANF.Add,v 22,v 23)] (v 24),[21,AST.TInt64;22,AST.TInt64;23,AST.TInt64;24,AST.TInt64],AST.TInt64
            | AST.TSum _ -> ANF.Let (id 21,ANF.RecordGet (descriptor,v 20,1),ANF.Let (id 22,ANF.Prim (ANF.Gte,v 21,int 0L),ANF.If (v 22,ANF.Return (v 21),finish [23,ANF.Prim (ANF.Add,v 21,int 1L)] (v 23)))),[21,AST.TInt64;22,AST.TBool;23,AST.TInt64],AST.TInt64
            | AST.TInt64 -> finish [21,ANF.Prim (ANF.Add,v 20,int 2L)] (v 21),[21,AST.TInt64],AST.TInt64
            | _ -> ANF.Return (v 20),[],ret
        let caller=func 400 "caller" [10,AST.TInt64] callerReturn (ANF.Let (id 20,ANF.Call (fid 200,args),after))
        let convert (func:ANF.Function) types=let types=(func.TypedParams |> List.map (fun param -> param.Id,param.Type)) @ (types |> List.map (fun (index,typ) -> id index,typ)) in SSAANF.convertFunction 100 (ANF.TypeMap.ofSeq types) func
        match convert helper types,convert caller ((20,ret)::bodyTypes) with
        | Error error,_ | _,Error error -> enc (Error error : Result<SSAANF.Function list,string>)
        | Ok helperSSA,Ok callerSSA -> attempt (fun () ->
            let externals,externalSSA,locals,localSSA=if isExternal then [helper],[helperSSA],[caller],[callerSSA] else [],[],[helper;caller],[helperSSA;callerSSA]
            let excluded=if excluded then Set.singleton helper.Id else Set.empty
            SSAInlining.inlineProgramWithExternalCandidatesAndExclusions config (InliningCommon.buildExternalCandidateInfoMap config externals) externalSSA excluded locals localSSA)
    cases |> List.map (fun case -> [int -1L;int 0L;int 3L;v 10] |> List.map (fun argument -> [false;true] |> List.map (fun isExternal -> [false;true] |> List.map (fun excluded -> configs |> List.map (observeCase case argument isExternal excluded) |> list) |> list) |> list) |> list) |> list

let specializationObservation (source:string) =
    let enc value=closureAnalysisEncode value
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let attempt action=enc (try Ok (action ()) with error -> Error error.Message)
    let fid index=AST.functionId (uint64 index)
    let id index=ANF.TempId index
    let v index=ANF.Var (id index)
    let int value=ANF.IntLiteral (ANF.Int64 value)
    let fn index name parameters ret operations terminator extraTypes : SSAANF.Function =
        let parameters=parameters |> List.map (fun (index,typ) -> {ANF.TypedParam.Id=id index;ANF.TypedParam.Type=typ})
        let label=SSAANF.Label 0
        {Id=fid index;Name=name;TypedParams=parameters;ReturnType=ret;ReturnOwnership=ANF.OwnedReturn;Entry=label;Blocks=Map.ofList [label,{Label=label;Parameters=[];Operations=operations;Terminator=terminator}];FreshValueTypes=parameters |> List.fold (fun types parameter -> Map.add parameter.Id parameter.Type types) (extraTypes |> List.map (fun (index,typ) -> id index,typ) |> Map.ofList)}
    let literalCases=[AST.TInt64,[int 1L;int 2L;int 1L];AST.TUInt64,[ANF.IntLiteral (ANF.UInt64 0x8000000000000000UL);ANF.IntLiteral (ANF.UInt64 UInt64.MaxValue);ANF.IntLiteral (ANF.UInt64 0UL)];AST.TFloat64,[ANF.FloatLiteral 0.;ANF.FloatLiteral (-0.);ANF.FloatLiteral (BitConverter.Int64BitsToDouble 0x7ff8000000000001L)];AST.TString,[ANF.StringLiteral source;ANF.StringLiteral "😀";ANF.StringLiteral "é"];AST.TInt64,[int 1L;int 1L]]
    let directCases=literalCases |> List.map (fun (typ,args) ->
        let helper=fn 200 ("helper"+source) [1,typ] typ [] (SSAANF.Return (v 1)) []
        let caller=fn 400 "caller" [] typ (args |> List.mapi (fun index arg -> id (20+index),ANF.Call (fid 200,[arg]))) (SSAANF.Return (v (19+List.length args))) (args |> List.mapi (fun index _ -> 20+index,typ))
        [false;true] |> List.map (fun indirect ->
            let block=Map.find (SSAANF.Label 0) caller.Blocks
            let caller=if indirect then {caller with Blocks=Map.ofList [SSAANF.Label 0,{block with Operations=(id 90,ANF.Atom (ANF.FuncRef (fid 200)))::block.Operations}]} else caller
            attempt (fun () -> SSADirectCallSpecialization.specializeProgramWithFunctionNames FunctionIdMap.empty [helper;caller])) |> list) |> list
    let tupleType=AST.TTuple [AST.TInt64;AST.TBool]
    let helper=fn 210 "tupleHelper" [1,tupleType] AST.TInt64 [id 2,ANF.TupleGet (v 1,0)] (SSAANF.Return (v 2)) [2,AST.TInt64]
    let caller=fn 410 "tupleCaller" [] AST.TInt64 [id 10,ANF.TupleAlloc [int 1L;ANF.BoolLiteral false];id 11,ANF.TupleAlloc [int 2L;ANF.BoolLiteral true];id 20,ANF.Call (fid 210,[v 10]);id 21,ANF.Call (fid 210,[v 11])] (SSAANF.Return (v 21)) [10,tupleType;11,tupleType;20,AST.TInt64;21,AST.TInt64]
    let tupleClones=attempt (fun () -> SSADirectCallSpecialization.specializeProgramWithFunctionNames FunctionIdMap.empty [helper;caller])
    let closureType=AST.TTuple [AST.TInt64;AST.TInt64]
    let callbackType=AST.TFunction ([AST.TInt64],AST.TInt64)
    let target=fn 500 ("target"+source) [0,closureType;1,AST.TInt64] AST.TInt64 [id 2,ANF.TupleGet (v 0,1);id 3,ANF.Prim (ANF.Add,v 2,v 1)] (SSAANF.Return (v 3)) [2,AST.TInt64;3,AST.TInt64]
    let staticFunction=fn 501 ("static"+source) [1,AST.TInt64] AST.TInt64 [id 2,ANF.Prim (ANF.Add,v 1,int 1L)] (SSAANF.Return (v 2)) [2,AST.TInt64]
    let helper=fn 502 ("apply"+source) [10,callbackType;11,AST.TInt64] AST.TInt64 [id 20,ANF.ClosureCall (v 10,[v 11])] (SSAANF.Return (v 20)) [20,AST.TInt64]
    let factory=fn 504 "factory" [1,AST.TInt64] callbackType [id 2,ANF.ClosureAlloc (fid 500,[v 1])] (SSAANF.Return (v 2)) [2,callbackType]
    let caller kind=
        let operations=match kind with
                       | 0 -> [id 13,ANF.ClosureAlloc (fid 500,[v 12]);id 20,ANF.Call (fid 502,[v 13;int 3L])]
                       | 1 -> [id 20,ANF.Call (fid 502,[ANF.FuncRef (fid 501);int 3L])]
                       | _ -> [id 13,ANF.Call (fid 504,[v 12]);id 20,ANF.BorrowedCall (fid 502,[v 13;int 3L])]
        fn 503 "caller" [12,AST.TInt64] AST.TInt64 operations (SSAANF.Return (v 20)) [13,callbackType;20,AST.TInt64]
    let higherCases=[0;1;2] |> List.map (fun kind -> [false;true] |> List.map (fun collision ->
        let reserved=if collision then Map.ofList ["apply"+source+"__known_target"+source+"_0",fid 900] else Map.empty
        attempt (fun () -> SSAHigherOrderSpecialization.specializeProgramWithExternalFunctionsAndNames reserved 1000UL [] [target;staticFunction;helper;factory;caller kind])) |> list) |> list
    tuple [directCases;tupleClones;higherCases]

let rcInsertion (source:string) =
    let enc value = closureAnalysisEncode value
    let tuple values = namedArray "tuple" (Array.ofList values)
    let list values = JsonArray(Array.ofList values) :> JsonNode
    let attempt action = enc (try Ok (action ()) with error -> Error error.Message)
    let id index = ANF.TempId index
    let v index = ANF.Var (id index)
    let fid index = AST.functionId (uint64 index)
    let scalar = [AST.TUnit; AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TBool; AST.TChar; AST.TFloat64; AST.TInt; AST.TInt128; AST.TUInt128; AST.TString; AST.TBlob; AST.TDateTime; AST.TInternalRawPtr; AST.TNever; AST.TVar source]
    let managed = [AST.TList AST.TString; AST.TList AST.TInt64; AST.TList (AST.TFunction ([AST.TUnit],AST.TString)); AST.TTuple [AST.TString;AST.TInt64]; AST.TDict (AST.TString,AST.TList AST.TString); AST.TStream AST.TString; AST.TFunction ([AST.TUnit],AST.TString); AST.TRecord ("R",[]); AST.TSum ("S",[]); AST.TRecord ("S",[])]
    let record : TypeRegistries.RecordTypeInfo = {TypeParams=[]; Fields=[source,AST.TString;"next",AST.TList AST.TString]}
    let sum : MemoryModel.RcSumShapeInfo = {TypeParams=[]; Payloads=[0,None;1,Some AST.TString]; UnaryPayloadTags=Set.singleton 1}
    let descriptor : ANF.RecordDescriptor = {SourceTypeName="R"; RuntimeTypeName="R"; TypeArgs=[]; Fields=record.Fields; ValueType=AST.TRecord ("R",[])}
    let cases typ =
        let bind operation body = ANF.Let (id 20,operation,body)
        let make = ANF.Call (fid 200,[])
        let ret=ANF.Return (v 20)
        let unit=ANF.Return ANF.UnitLiteral
        [ANF.Return (v 10); bind (ANF.TypedAtom (v 10,typ)) ret; bind (ANF.Atom (v 10)) ret;
         bind make unit; bind make ret; bind make (ANF.Let (id 21,ANF.TypedAtom (v 20,typ),ANF.Return (v 21)));
         bind make (ANF.Let (id 21,ANF.TupleAlloc [v 20],ANF.Return (v 21)));
         bind make (ANF.Let (id 21,ANF.TupleAlloc [v 20;v 20],ANF.Return (v 21)));
         bind make (ANF.Let (id 21,ANF.RawSlotInit (v 12,ANF.IntLiteral (ANF.Int64 8L),v 20,typ),unit));
         bind make (ANF.Let (id 21,ANF.RawSlotInit (v 12,ANF.IntLiteral (ANF.Int64 8L),v 20,typ),ret));
         bind make (ANF.Let (id 21,ANF.TypedAtom (v 20,typ),ANF.Let (id 22,ANF.TupleAlloc [v 21],ANF.Return (v 22))));
         bind make (ANF.If (v 11,ret,unit));
         bind make (ANF.Join ({Id=id 30;Type=AST.TInt64},ANF.If (v 11,ret,ANF.Return (v 10)),ANF.Let (id 21,make,ANF.Jump (id 30,ANF.IntLiteral (ANF.Int64 7L)))));
         bind make (ANF.Let (id 21,ANF.Print (v 20,typ),ret));
         bind (ANF.BorrowedCall (fid 200,[])) unit; bind (ANF.BorrowedCall (fid 200,[])) ret;
         bind (ANF.IfValue (v 11,v 10,v 13)) ret;
         bind (ANF.RawGet (v 12,ANF.IntLiteral (ANF.Int64 8L),None)) (ANF.Let (id 21,ANF.TypedAtom (v 20,typ),ANF.Return (v 21)));
         bind (ANF.RawGet (v 12,ANF.IntLiteral (ANF.Int64 8L),None)) (ANF.Let (id 21,ANF.Atom (v 20),ANF.Let (id 22,ANF.RawSlotInit (v 12,ANF.IntLiteral (ANF.Int64 8L),v 21,typ),unit)));
         bind (ANF.TupleGet (v 14,0)) ret; bind (ANF.RecordGet (descriptor,v 15,0)) ret;
         bind (ANF.RecordAlloc (descriptor,[v 10;v 13])) unit;
         bind (ANF.RecordClone (descriptor,v 15,[v 10;v 13])) ret;
         bind (ANF.RecordReuse (descriptor,descriptor,v 15,[v 10;v 13])) ret;
         bind (ANF.ClosureAlloc (fid 203,[v 10])) (ANF.Let (id 21,ANF.ClosureCall (v 20,[ANF.UnitLiteral]),ANF.Return (v 21)));
         bind make (ANF.Let (id 21,ANF.TailCall (fid 100,[v 20;v 11]),ANF.Return (v 21)));
         bind make (ANF.Let (id 21,ANF.TailCall (fid 200,[]),ANF.Return (v 21)));
         bind make (ANF.Let (id 21,ANF.Call (fid 202,[v 13;v 20]),ANF.Return (v 21)))]
    let observeType typ =
        let functions = FunctionIdMap.ofList [fid 100,("loop",AST.TFunction ([typ;AST.TBool],typ)); fid 200,("make",AST.TFunction ([],typ)); fid 201,("observe",AST.TFunction ([AST.TInt64],AST.TUnit)); fid 202,("Darklang.Stdlib.List.__push_i64",AST.TFunction ([AST.TList typ;typ],AST.TList typ)); fid 203,("closure",AST.TFunction ([AST.TUnit],typ))]
        let initial=Map.ofList [id 10,typ;id 11,AST.TBool;id 12,AST.TInternalRawPtr;id 13,typ;id 14,AST.TTuple [typ];id 15,AST.TRecord ("R",[])]
        let ctx : RcTypeFacts.TypeContext = {TypeReg=Map.ofList ["R",record];VariantLookup=Map.empty;SumShapeReg=Map.ofList ["S",sum];FuncReg=functions;FuncParams=Map.empty;TempTypes=initial;ClosureFuncs=Map.empty;TypePlanning=RcTypeFacts.createRcTypePlanningContext ()}
        tuple [attempt (fun () -> rcInternalCall<MemoryModel.RcShape> "RcShapePlanning" "rcShapeForType" [|box ctx;box typ|]);
          cases typ |> List.map (fun body ->
            let analyzed=RcReturnAnalysis.analyzeReturns Map.empty Map.empty body
            let bindings=match analyzed with RcReturnAnalysis.RLet (id,operation,rest,_) -> attempt (fun () -> rcInternalCall<AST.SemanticType> "RcInsertExpression" "inferBindingType" [|box ctx;box id;box operation;box rest|]) | _ -> enc (Ok typ : Result<AST.SemanticType,string>)
            tuple [bindings; [100;2147483647] |> List.map (fun first -> attempt (fun () -> rcInternalCall<ANF.AExpr * ANF.VarGen * Map<ANF.TempId,AST.SemanticType>> "RcInsertExpression" "insertRCInternal" [|box ctx;box body;box (ANF.VarGen first);box initial|])) |> list;
              ["loop";"Darklang.Stdlib.List.__mapHelper_case"] |> List.map (fun name ->
                let definition : ANF.Function = {Id=fid 100;Name=name;TypedParams=initial |> Map.toList |> List.map (fun (id,typ) -> {ANF.Id=id;Type=typ});ReturnType=typ;ReturnOwnership=ANF.OwnedReturn;Body=body}
                tuple [attempt (fun () -> RefCountInsertion.insertRCInFunction ctx definition (ANF.VarGen 100));
                  enc (SSAANF.convertFunctionBeforeRC 30 ctx definition);
                  (match SSAANF.convertFunctionBeforeRC 30 ctx definition with
                   | Error error -> enc (Error error : Result<unit,string>)
                   | Ok value -> attempt (fun () ->
                       let live=RcSSAValueLiveness.analyze value
                       let returned=RcSSAReturnAnalysis.analyze value
                       let escaped=SSAEscapeAnalysis.optimizeFunction ctx.TypeReg ctx.SumShapeReg value
                       let cleaned=RcSSARefCountInsertion.insertBlockLocal ctx Set.empty value
                       let names=ctx.FuncReg |> FunctionIdMap.map (fun _ (name,_) -> name)
                       let context : ANFConstants.OptimizeContext = {TypeReg=TypeRegistries.recordFieldsRegistry ctx.TypeReg;RecordTypeParams=TypeRegistries.recordTypeParamsRegistry ctx.TypeReg;SumShapeReg=ctx.SumShapeReg;FunctionNames=names;FunctionIds=TypeRegistries.functionIdsFromNames names}
                       let disabled : ANFConstants.OptimizeOptions = {EnableConstFolding=false;EnableConstProp=false;EnableCopyProp=false;EnableDCE=false;EnableCSE=false;EnableStrengthReduction=false;EnableTailRecursionModuloOperation=false}
                       (live,returned,escaped,SSATailCallDetection.detect FunctionIdMap.empty value,cleaned,SSATailCallDetection.detect FunctionIdMap.empty cleaned,SSAOptimization.optimizeFunction context ANFConstants.defaultOptimizeOptions value,SSAOptimization.optimizeFunction context disabled value)));

                  enc (SSAANF.convertFunction 30 (ANF.TypeMap.ofSeq (Map.toSeq initial)) definition);
                  [OwnedIR.UnmanagedCallParameter;OwnedIR.BorrowedCallParameter;OwnedIR.ConsumedCallParameter;OwnedIR.UniqueCallParameter] |> List.map (fun ownership ->
                    let contract : OwnedIR.CallSignature = {Parameters=definition.TypedParams |> List.map (fun _ -> ownership);Result=OwnedIR.ProducedCallResult}
                    enc (rcInternalCall<Result<unit,string>> "RefCountInsertion" "verifyOwnershipContracts" [|box ctx;box (FunctionIdMap.ofList [fid 100,contract]);box (ANF.Program ([definition],ANF.Return ANF.UnitLiteral))|])) |> list]) |> list]) |> list]
    scalar @ managed |> List.map observeType |> list

let expressionLowering source =
    let tuple values=namedArray "tuple" (Array.ofList values)
    let list values=JsonArray(Array.ofList values) :> JsonNode
    let enc value=closureAnalysisEncode value
    let outcome encoder value =
        match value with
        | Error error -> enc (Error error : Result<unit,string>)
        | Ok value ->
            let node=JsonObject()
            node["type"]<-JsonValue.Create "FSharpResult"
            node["case"]<-JsonValue.Create "Ok"
            node["fields"]<-JsonArray([|encoder value|])
            node :> JsonNode
    let attempt action=enc (try Ok (action ()) with error -> Error error.Message)
    let helpers=["Darklang.Stdlib.Int.__value";"Darklang.Stdlib.Int.__equals";"Darklang.Stdlib.Int.bitwiseNot";"Darklang.Stdlib.Int128.__value";"Darklang.Stdlib.UInt128.__value";"Darklang.Stdlib.Int128.__equals";"Darklang.Stdlib.UInt128.__equals";"Darklang.Stdlib.Int128.bitwiseNot";"Darklang.Stdlib.UInt128.bitwiseNot";"Darklang.Stdlib.String.__normalizeAfterConcat";"Darklang.Stdlib.List.__headUnsafe_i64";"Darklang.Stdlib.List.__headUnsafeFloat";"Darklang.Stdlib.List.__tail_i64";"Darklang.Stdlib.List.__length_i64";"Darklang.Stdlib.List.__lengthFloat";"Darklang.Stdlib.List.__getAtInt64";"Darklang.Stdlib.List.__getAtFloat"]
    let examples=["1L + 2L";"if true then 1L else 2L";"let (x, y) = (1L, 2L) in x + y";"\"é\" ++ \"😀\"";"match 0.0 with | -0.0 -> 1L | 0.0 -> 2L | _ -> 3L";"match 1L with | 1L when false -> 2L | x -> x";"match [1L] with | [] -> 0L | [x] -> x | _ -> 2L";"match [1L, 2L] with | [1L, x] -> x | _ -> 0L";"match [1L, 2L] with | x :: tail -> x | _ -> 0L";"match [1L, 2L] with | x :: y :: tail -> x + y | _ -> 0L";"match [1L, 2L] with | 1L :: [x] -> x | _ -> 0L";"match [1.0, 2.0] with | [x, y] -> x + y | _ -> 0.0";"match [(1L, 2L)] with | [(1L, x)] -> x | _ -> 0L";"match [(1L, 2L)] with | (1L, x) :: tail -> x | _ -> 0L";"match [\"é\"] with | [\"é\"] -> 1L | _ -> 0L";"match ([\"abc\"], 1L) with | (\"abc\" :: _, x) -> x | _ -> 0L";"match [[1L], [2L]] with | [x] :: rest -> x | _ -> 0L";"type S = A of String | B\nmatch S.A \"é\" with | A \"é\" -> 1L | A x when false -> 2L | B -> 3L | _ -> 0L";"type S = A of Int64 | B\nmatch S.A 1L with | A x -> x | B -> 0L";"type S = A of (Int64 * String) | B of Int64\nmatch S.A (1L, \"a\") with | A (1L, x) when true -> x | _ -> \"b\"";"type S = A of Int64\nmatch S.A 1L with | A x -> x";"type S = A | B\nmatch S.A with | A -> 1L | B -> 2L";"type R = { a: Int64; b: String }\nR { b = \"é\"; a = 1L }";"type R = { a: Int64; b: String }\nlet r = R { a = 1L; b = \"x\" } in { r with a = 2L }";"let f (x: Int64): Int64 = x + 1L\nf 2L";"let f (x: List<Int64>): Int64 = match x with | h :: t when h == 1L -> h | _ -> 0L\nf [1L]";"let x = match [1L] with | [h] -> h | _ -> 0L in x + 1L";"match 1L with | 0L | 1L -> 2L | _ -> 3L";"match (1L, 2L) with | (x, _) when x == 1L -> x | _ -> 0L";"match [] with | [] -> 1L | _ -> 0L";"match [1L] with | [x] when x == 1L -> x | _ -> 0L";"match [1L] with | x :: tail when x == 1L -> x | _ -> 0L"]
    let program program =
        let symbols=CheckedAST.programSymbols program
        let tops=CheckedAST.programTopLevels program
        let types=WrittenChecking.typeCheckEnvironment program
        let registry : TypeRegistries.TypeRegistry = types.IndexedTypeReg |> Map.map (fun _ (info:CheckingTypes.RecordTypeInfo) -> {TypeParams=info.TypeParams;Fields=info.Fields})
        let variants=types.VariantLookup
        let sums=LoweringPrimitives.sumMetadataFromVariantLookup variants
        let typeNames=TypeRegistries.typeNamesFromSymbols symbols
        let funcs=tops |> List.choose (function CheckedAST.FunctionDef func -> Some func | _ -> None)
        let functions=funcs |> List.map (fun func -> func.Id,(func.Name,AST.TFunction (CheckedAST.functionParameterTypes func |> AST.NonEmptyList.toList |> List.map snd,CheckedAST.functionReturnType func))) |> FunctionIdMap.ofList
        let names=CheckedAST.functionNames symbols
        let ids=AST.allocateFunctionIds (names |> FunctionIdMap.toList |> Seq.map fst) (helpers |> Seq.filter (fun name -> not (Map.containsKey name (CheckedAST.functionIds symbols))))
        let names=ids |> Map.fold (fun names name id -> FunctionIdMap.add id name names) names
        let ids=TypeRegistries.functionIdsFromNames names
        let globals : TypeRegistries.VarEnv = CheckedAST.programValues program |> Map.toList |> List.mapi (fun index (name,(typ,_)) -> AST.topLevelValueId name,(ANF.TempId (-100-index),typ)) |> Map.ofList
        let bodies=tops |> List.choose (function
            | CheckedAST.FunctionDef func ->
                let env=CheckedAST.functionParameterTypes func |> AST.NonEmptyList.toList |> List.mapi (fun index (id,typ) -> id,(ANF.TempId (-1000-index),typ)) |> List.fold (fun env (id,value) -> Map.add id value env) globals
                Some (func.Body,env)
            | CheckedAST.ValueDef value -> Some (value.Body,globals)
            | CheckedAST.Expression value -> Some (value,globals)
            | _ -> None)
        bodies |> List.map (fun (expr,env) ->
            (if source="" then [0;Int32.MaxValue] else [0]) |> List.map (fun first ->
                let gen=ANF.VarGen first
                let modules=Stdlib.buildModuleRegistry ()
                tuple [attempt (fun () -> LoweringExpressions.toANFCore ids sums typeNames Set.empty expr gen env registry variants functions names modules)
                       attempt (fun () -> LoweringExpressions.toAtomCore ids sums typeNames Set.empty expr gen env registry variants functions names modules)
                       attempt (fun () -> LoweringExpressions.toANFBoundAtomCore ids sums typeNames Set.empty expr gen env registry variants functions names modules)]) |> list) |> list
    let sourceProgram source=WrittenParsing.parse LibParser.Validation.Script source |> Result.bind (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit]) |> Result.map (fun (_,value,_) -> value) |> outcome program
    tuple [sourceProgram source;(if source="" then examples |> List.map sourceProgram |> list else list [])]

let anfOutputPlanning source =
    let primitives = [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TChar;AST.TDateTime;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TVar source;AST.TInferenceVar (source,"fixed");AST.TFunction ([AST.TInt64],AST.TString);AST.TStream AST.TString]
    let record parameters fields : TypeRegistries.RecordTypeInfo = {TypeParams=parameters;Fields=fields}
    let records = Map.ofList [
        "Regular",record [] [source,AST.TString;"next",AST.TRecord ("Regular",[])];
        "Generic",record ["a"] [source,AST.TVar "a";"next",AST.TRecord ("Generic",[AST.TVar "a"])];
        "Growing",record ["a"] ["next",AST.TRecord ("Growing",[AST.TList (AST.TVar "a")])];
        "Closure",record [] ["value",AST.TFunction ([AST.TInt64],AST.TString)];
        "Mixed",record [] ["value",AST.TSum ("Sum",[])];
        "Alias",record [] ["value",AST.TRecord ("Sum",[])]]
    let sum parameters payloads : MemoryModel.RcSumShapeInfo = {TypeParams=parameters;Payloads=payloads;UnaryPayloadTags=Set.empty}
    let sums = Map.ofList [
        "Sum",sum [] [0,None;1,Some (AST.TTuple [AST.TString;AST.TSum ("Sum",[])])];
        "GenericSum",sum ["a"] [0,None;1,Some (AST.TTuple [AST.TVar "a";AST.TSum ("GenericSum",[AST.TVar "a"])])];
        "GrowingSum",sum ["a"] [0,None;1,Some (AST.TSum ("GrowingSum",[AST.TList (AST.TVar "a")]))];
        "ClosureSum",sum [] [1,Some (AST.TFunction ([AST.TInt64],AST.TString))];
        "EmptySum",sum [] []]
    let types = primitives @ List.map AST.TList primitives @ List.map (fun typ -> AST.TTuple [AST.TString;typ]) primitives @ [
        AST.TRecord ("Missing",[]);AST.TRecord ("Regular",[]);AST.TRecord ("Regular",[AST.TInt64]);
        AST.TRecord ("Generic",[AST.TString]);AST.TRecord ("Generic",[AST.TFunction ([AST.TInt64],AST.TString)]);
        AST.TRecord ("Growing",[AST.TInt64]);AST.TRecord ("Closure",[]);AST.TRecord ("Mixed",[]);AST.TRecord ("Alias",[]);
        AST.TSum ("Sum",[]);AST.TRecord ("Sum",[]);AST.TSum ("GenericSum",[AST.TInt64]);AST.TSum ("GenericSum",[AST.TStream AST.TString]);
        AST.TSum ("GrowingSum",[AST.TInt64]);AST.TSum ("ClosureSum",[]);AST.TSum ("EmptySum",[]);AST.TSum ("Sum",[AST.TString]);
        AST.TDict (AST.TString,AST.TList (AST.TRecord ("Regular",[])));AST.TDict (AST.TString,AST.TFunction ([AST.TInt64],AST.TString))]
    let destruction = [Map.empty;records] |> List.collect (fun typeReg -> [Map.empty;sums] |> List.collect (fun sumReg -> [false;true] |> List.map (fun allow -> types |> List.map (EscapeAnalysisFacts.hasNonObservableDestruction typeReg sumReg allow))))
    let descriptors = types |> List.collect (fun typ -> [AST.TRecord (source,[]);AST.TSum (source,[])] |> List.map (fun valueType ->
        let descriptor : ANF.RecordDescriptor = {SourceTypeName=source;RuntimeTypeName=source;TypeArgs=[];Fields=[source,typ;"next",AST.TString];ValueType=valueType}
        EscapeAnalysisFacts.descriptorHasNonObservableDestruction records sums descriptor))
    let supported = [AST.TInt64;AST.TInt;AST.TBool;AST.TString;AST.TChar;AST.TFloat64;AST.TList AST.TInt64]
    let printTypes = primitives @ List.map AST.TList supported @ List.map (fun typ -> AST.TSum ("Darklang.Stdlib.Option.Option",[AST.TList typ])) supported @ [AST.TSum ("Uuid",[]);AST.TList AST.TBlob;AST.TSum ("Darklang.Stdlib.Option.Option",[AST.TList AST.TBlob])]
    let helperNames = ["Darklang.Stdlib.List.__toDisplayString_i64";"Darklang.Stdlib.List.__toDisplayString_int";"Darklang.Stdlib.List.__toDisplayString_bool";"Darklang.Stdlib.List.__toDisplayString_str";"Darklang.Stdlib.List.__toDisplayString_char";"Darklang.Stdlib.List.__toDisplayString_f64";"Darklang.Stdlib.List.__toDisplayString_list_i64";"Darklang.Stdlib.Float.toString";"Darklang.Stdlib.DateTime.toString";"Darklang.Stdlib.Uuid.toString"]
    let ids = helperNames |> List.mapi (fun index name -> name,AST.functionId (uint64 (index+1))) |> Map.ofList
    let resolve name = match Map.tryFind name ids with Some id -> id | None -> Crash.crash ("Missing observation helper: " + name)
    let render = AST.functionId 11UL
    let functionNames = FunctionIdMap.ofList [render,"__dark_render_value_" + source;AST.functionId 12UL,"ordinary"]
    let bodies = [
        ANF.Return ANF.UnitLiteral;ANF.Return (ANF.Var (ANF.TempId 3));ANF.Return (ANF.StringLiteral source);ANF.Return (ANF.FloatLiteral (-0.));
        ANF.Return (ANF.IntLiteral (ANF.Int64 Int64.MinValue));
        ANF.Join ({Id=ANF.TempId 50;Type=AST.TInt64},ANF.Return (ANF.Var (ANF.TempId 50)),ANF.If (ANF.BoolLiteral true,ANF.Return (ANF.Var (ANF.TempId 3)),ANF.Jump (ANF.TempId 50,ANF.Var (ANF.TempId 4))));
        ANF.Let (ANF.TempId 10,ANF.RuntimeError source,ANF.Return ANF.UnitLiteral);
        ANF.Let (ANF.TempId 10,ANF.Call (render,[ANF.Var (ANF.TempId 3)]),ANF.Return (ANF.Var (ANF.TempId 10)));
        ANF.Let (ANF.TempId 10,ANF.Call (AST.functionId 12UL,[ANF.Var (ANF.TempId 3)]),ANF.Jump (ANF.TempId 50,ANF.Var (ANF.TempId 10)))]
    let fn name body id : ANF.Function = {Id=AST.functionId id;Name=name;TypedParams=[];ReturnType=AST.TString;ReturnOwnership=ANF.OwnedReturn;Body=body}
    let functions = [fn "entry" bodies[7] 11UL;fn "ordinary" bodies[8] 12UL;fn "entry" bodies[5] 13UL]
    let capture f = try Ok (f ()) with exn -> Error exn.Message
    namedArray "tuple" [|
        encode typeof<(bool * string option) list> (box (types |> List.map (fun typ -> EscapeAnalysisFacts.isScalarType typ,ListDisplay.getDisplayStringFunc typ)))
        encode typeof<bool list list> (box destruction)
        encode typeof<bool list> (box descriptors)
        encode typeof<Result<ANF.AExpr * ANF.VarGen,string> list list> (box (printTypes |> List.map (fun typ -> bodies |> List.map (fun body -> capture (fun () -> PrintInsertion.wrapReturnWithPrint resolve typ (ANF.VarGen 100) body)))))
        encode typeof<Result<ANF.Program,string> list> (box (printTypes |> List.map (fun typ -> capture (fun () -> PrintInsertion.insertPrint ids functions bodies[5] typ))))
        encode typeof<Result<ANF.Function list,string> list list> (box (["entry";"ordinary";"missing"] |> List.map (fun entry -> [AST.TString;AST.TList AST.TInt64] |> List.map (fun typ -> PrintInsertion.insertPrintInEntry ids entry typ functions))))
        encode typeof<Result<ANF.Function list,string> list list> (box (["entry";"ordinary";"missing"] |> List.map (fun entry -> [false;true] |> List.map (fun tupleWords -> PrintInsertion.insertRootWordProbeInEntry functionNames entry tupleWords functions))))|]

let processRequest (line: string) =
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
        | "lowering-aggregates" -> loweringAggregates source
        | "block-allocation" -> blockAllocationObservation source
        | "instruction-allocation" -> instructionAllocationObservation source
        | "phi-resolution" -> phiObservation source
        | "spill-operands" -> spillObservation source
        | "float-allocation" -> floatAllocationObservation source
        | "register-coloring" -> coloringObservation source
        | "allocation-foundations" -> allocationObservation source
        | "lir-tree" -> lirTreeObservation source
        | "lir-foundations" -> lirObservation source
        | "ir-printers" -> irPrinterObservation source
        | "mir-sccp" -> mirSCCPObservation source
        | "mir-loops" -> mirLoopObservation source
        | "mir-cse" -> mirCSEObservation source
        | "mir-ssa" -> mirSSAObservation source
        | "mir-foundations" -> mirFoundationsObservation source
        | "ssa-inlining" -> inliningObservation source
        | "ssa-specialization" -> specializationObservation source
        | "rc-insertion" -> rcInsertion source
        | "expression-lowering" -> expressionLowering source
        | "atom-lowering" -> atomLowering source
        | "lowering-types" -> loweringTypes source
        | "lowering-operators" -> loweringOperators source
        | "monomorphization" -> monomorphization source
        | "lift-functions" -> liftFunctions source
        | "lift-expressions" -> liftExpressions source
        | "closure-comparisons" -> closureComparisons source
        | "closure-analysis" -> closureAnalysis source
        | "checked-display" -> checkedDisplay source
        | "checked-structural-format" -> checkedFormat source
        | "inline-lambdas" -> inlineLambdas source
        | "type-substitution" -> typeSubstitution source
        | "lowering-primitives" -> loweringPrimitives source
        | "memory-planning" -> memoryPlanning source
        | "preparation-registries" -> preparationRegistries source
        | "anf-output-planning" -> anfOutputPlanning source
        | "anf-scalar-optimization" -> anfScalarOptimization source
        | "anf" -> anfObservation source
        | "checked-preparation" -> checkedPreparation source
        | "written-checking" -> writtenChecking source
        | "written-patterns" -> writtenPatterns source
        | "written-types" -> writtenTypes source
        | "program-checking" -> programChecking source
        | "function-checking" -> functionChecking source
        | "expression-checking" -> expressionChecking source
        | "match-checking" -> matchChecking source
        | "call-checking" -> callChecking source
        | "lambda-checking" -> lambdaChecking source
        | "stdlib-catalog" -> stdlibCatalog source
        | "binary-checking" -> binaryChecking source
        | "record-checking" -> recordChecking source
        | "declarations" -> declarations source
        | "materialize-helpers" -> materializeHelpers source
        | "helper-dependencies" -> helperDependencies source
        | "structural-helpers" -> structuralHelpers source
        | "comparison-planning" -> comparison source
        | "structural-format" -> structuralFormat source
        | "unification" -> unification source
        | "checking-types" -> checkingTypes source
        | "checked-ast" -> checkedAst source
        | "function-map" -> functionIdMap source
        | "free-variables" -> freeVariables source
        | "checking-diagnostics" -> checkingDiagnostics source
        | "resolution" -> resolution source
        | "names" -> names source
        | "written-source" -> writtenSource source
        | "ast" -> encode typeof<LibParser.Parser.ParseResult> (box (LibParser.Parser.parse source))
        | "parser-support" | "patterns" | "types" | "bindings" | "parameters" | "effects" -> parserSupport stage source
        | _ -> failwith $"Unsupported reference observation stage: {stage}"
    let response = JsonObject()
    response["schema"] <- JsonValue.Create 1
    response["stage"] <- JsonValue.Create stage
    response["value"] <- result
    Console.WriteLine(response.ToJsonString(jsonOutputOptions))

let mutable requestLine = reader.ReadLine()
while not (isNull requestLine) do
    processRequest requestLine
    GC.Collect()
    GC.WaitForPendingFinalizers()
    requestLine <- reader.ReadLine()
