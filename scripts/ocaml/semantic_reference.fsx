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
