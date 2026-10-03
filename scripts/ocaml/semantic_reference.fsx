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
    let cases = if source = "" then groups |> List.collect (fun params -> expectations |> List.collect (fun expected -> annotations |> List.collect (fun annotation -> bodies |> List.map (fun body -> params,expected,annotation,body))))
                else bodies |> List.mapi (fun index body -> groups[index % groups.Length],expectations[index % expectations.Length],annotations[index % annotations.Length],body)
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static
    let method = typeof<AST.SemanticType>.Assembly.GetType("CheckLambdas").GetMethod("check",flags)
    cases |> List.map (fun (params,expected,annotation,body) ->
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
        let result = method.Invoke(null,[|checker;box env;box (Map.empty<string,CheckingTypes.RecordTypeInfo>);box (Map.empty<string,string * string list * int * AST.SemanticType list>);box generic;box AST.defaultWarningSettings;box modules;box (Map.empty<string,string list * AST.SemanticType>);box expected;box params;box annotation;box body|])
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
        Console.WriteLine(response.ToJsonString())
        requests ()
requests ()
