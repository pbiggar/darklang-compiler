open Dark_compiler
module R = InstrumentedTypeRegistries
module C = InstrumentedCheckedAST
module M = StringOrder.Map
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let map encode values = `Assoc ["map", list (fun (name, value) -> tuple [str name; encode value]) (M.bindings values)]
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let fields = list (fun (name, value) -> tuple [str name; typ value])
let info (value : R.recordTypeInfo) = SemanticJson.record "RecordTypeInfo" ["TypeParams", list str value.R.typeParams; "Fields", fields value.R.fields]
let id value = let value = AST.functionIdValue value in SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind", `String "uint64"; "value", `String (Z.to_string (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value))]]
let observe source =
 let records = M.of_list ["R", {R.typeParams = []; fields = [source, AST.TInt64; "second", AST.TRecord ("Number", [])]}; "Generic", {R.typeParams = ["a"]; fields = ["value", AST.TVar "a"]}] in
 let aliases = M.of_list ["Alias", ([], AST.TRecord ("R", [])); "Chain", ([], AST.TRecord ("Alias", [])); "SumAlias", ([], AST.TSum ("S", [])); "Number", ([], AST.TInt64); "Unresolved", ([], AST.TRecord ("Missing", [])); "Parameterized", (["a"], AST.TRecord ("Generic", [AST.TVar "a"]))] in
 let variants = M.of_list ["S.A", ("S", ["a"], 2, [AST.TVar "a"]); "S.B", ("S", ["later"], 1, []); "S.C", ("S", [], 2, [AST.TRecord ("S", []); AST.TStream (AST.TRecord ("S", []))]); "A", ("S", [], 9, []); "Other.A", ("Other", [], 0, [AST.TString]); "wrong", ("Lost", [], 3, [AST.TBool])] in
 let types = [AST.TInt64; AST.TString; AST.TFloat64; AST.TVar source; AST.TRecord ("S", []); AST.TRecord ("R", []); AST.TSum ("R", [AST.TRecord ("S", [])]); AST.TRecord ("Generic", [AST.TRecord ("S", [])]); AST.TList (AST.TRecord ("S", [])); AST.TStream (AST.TRecord ("S", [])); AST.TDict (AST.TRecord ("S", []), AST.TSum ("R", [])); AST.TFunction ([AST.TRecord ("S", [])], AST.TSum ("R", [])); AST.TTuple [AST.TString; AST.TInt64]] in
 let functionNames = FunctionIdMap.ofList [AST.functionId 0L, source; AST.functionId Int64.min_int, "middle"; AST.functionId (-1L), source; AST.functionId 7L, "other"] in
 let functionIds = R.functionIdsFromNames functionNames in
 let b0 = AST.bindingId 0 and b1 = AST.bindingId 1 in
 let variables = R.BindingMap.of_seq (List.to_seq [b0, (InstrumentedANF.TempId 3, AST.TInt64); b1, (InstrumentedANF.TempId 4, AST.TVar source); b0, (InstrumentedANF.TempId 5, AST.TString)]) in
 let constructor, symbols = C.internConstructor "S" "A" 2 (C.emptySymbols ()) in
 let field, symbols = C.internField "R" source 3 symbols in
 let metadata = [R.emptyTypeNames; R.typeNamesFromSymbols symbols] in
 let helperNames = ["Darklang.Stdlib.List.__headUnsafe_i64"; "Darklang.Stdlib.List.__headUnsafeFloat"; "Darklang.Stdlib.Json.__viewListHead"; "Darklang.Stdlib.Json.__viewFieldListHead"] in
 let helpers = List.mapi (fun index name -> name, AST.functionId (Int64.of_int index)) helperNames in
 let helperMaps = [M.of_list helpers; M.of_list (List.filter (fun (name, _) -> not (String.starts_with ~prefix:"Darklang.Stdlib.Json." name)) helpers)] in
 tuple [map fields (R.recordFieldsRegistry records); map (list str) (R.recordTypeParamsRegistry records); SemanticANF.memoryModel_rcSumShapeRegistry (R.rcSumShapeRegistryFromVariantLookup variants);
  map id functionIds; list (fun metadata -> tuple [C.observationSemanticMetadata metadata; option SemanticJson.int32 (R.tryFindConstructorTag constructor metadata); option SemanticJson.int32 (R.tryFindFieldIndex field metadata)]) metadata;
  list (fun helpers -> list (fun value -> SemanticANF.aNF_cExpr (R.listHeadUnsafeExpr helpers value (InstrumentedANF.StringLiteral source))) types) helperMaps;
  list (fun value -> tuple [typ (R.canonicalizeBareSumTypeRefsWithNames (StringOrder.Set.of_list ["S"]) value); typ (R.canonicalizeBareSumTypeRefs variants value); typ (R.canonicalizeNamedTypeRefs (StringOrder.Set.of_list ["R"]) (StringOrder.Set.of_list ["S"]) value)]) types;
  list (fun name -> str (R.resolveRecordTypeName aliases name)) ["Alias"; "Chain"; "SumAlias"; "Number"; source]; map info (R.expandTypeRegWithAliases records aliases);
  `Assoc ["map", list (fun (binding, value) -> tuple [C.observationBinding binding; typ value]) (R.BindingMap.bindings (R.typeEnvFromVarEnv variables))]]
