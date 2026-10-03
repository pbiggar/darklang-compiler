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
