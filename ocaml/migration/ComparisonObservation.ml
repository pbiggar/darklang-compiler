open Dark_compiler
module C = ComparisonPlanning
module M = StringOrder.Map
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let expr = SemanticAST.expr
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticDiagnostics.typeError error]
let option encode = function Some value -> SemanticJson.union "FSharpOption" "Some" [encode value] | None -> SemanticJson.union "FSharpOption" "None" []
let plan = function C.EqualityComparison value -> SemanticJson.union "ComparisonPlan" "EqualityComparison" [typ value] | C.OrderingComparison value -> SemanticJson.union "ComparisonPlan" "OrderingComparison" [typ value]
let dispatch (C.EqHelperDispatchTypeApp (target, left, right)) = SemanticJson.union "InternalTypeApp" "EqHelperDispatchTypeApp" [typ target; expr left; expr right]
let observe source =
 let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
 AST.TVar source; AST.TInferenceVar (source, "fixed"); AST.TFunction ([AST.TInt64], AST.TString); AST.TFunction ([AST.TBlob], AST.TNever); AST.TTuple [AST.TInt64; AST.TString];
 AST.TRecord ("R", []); AST.TRecord ("R", [AST.TUnit]); AST.TRecord ("Recursive", []); AST.TRecord ("Opaque", []); AST.TRecord ("Missing", []); AST.TRecord ("S", []);
 AST.TSum ("S", []); AST.TSum ("BadSum", []); AST.TSum ("Generic", [AST.TString]); AST.TSum ("Generic", []); AST.TSum ("Uuid", []);
 AST.TList AST.TInt64; AST.TStream AST.TNever; AST.TDict (AST.TString, AST.TInt64); AST.TDict (AST.TInt64, AST.TString); AST.TDict (AST.TBlob, AST.TString)] in
 let lookup = M.of_list ["S.C", ("S", [], 0, [AST.TInt64]); "BadSum.C", ("BadSum", [], 0, [AST.TInternalRawPtr]); "Generic.C", ("Generic", ["a"], 0, [AST.TVar "a"])] in
 let raw = M.of_list ["R", ["field", AST.TInt64]; "Recursive", ["next", AST.TRecord ("Recursive", [])]; "Opaque", ["field", AST.TStream AST.TInt64]] in
 let registry = Types.indexTypeRegistry lookup (M.of_list ["R", []; "Recursive", []; "Opaque", []]) raw in
 let sums = Types.indexSumTypeRegistry lookup and aliases = M.singleton "Alias" ([], AST.TRecord ("R", [])) in
 let left = AST.Var source and right = AST.UnitLiteral in
 let perType = List.map (fun value ->
   let internal = C.makeInternalTypeApp (C.EqHelperDispatchTypeApp (value, left, right)) in
   tuple [typ (C.canonicalEqualityType lookup value); `Bool (C.needsEqHelperForResolvedType lookup value);
   str (C.eqHelperName value); str (C.compareHelperName value); expr internal; option dispatch (C.tryDecodeInternalTypeApp internal);
   expr (C.buildEqExprForType aliases lookup value left right); `Bool (C.canonicalSortableType aliases registry sums value); `Bool (C.dictKeyAdmissibleType aliases registry sums value);
   result (fun () -> `Null) (C.validateJsonTargetType aliases registry lookup sums value);
   list (fun name -> result (fun () -> `Null) (C.validateDictKeyCall aliases registry sums name [value])) [source; "Dict.set"; "Dict.__internal"; "Darklang.Stdlib.Dict.set"; "Darklang.Stdlib.Dict.__internal"];
   list (fun name -> result (fun () -> `Null) (C.validateCanonicalSortableCall aliases registry sums name [value])) [source; "__compare"; "Darklang.Stdlib.List.sort"; "Darklang.Stdlib.List.unique"];
   list (fun op -> expr (C.buildOrderingExprForType op value left right)) [AST.Lt; AST.Gt; AST.Lte; AST.Gte]]) samples in
 let varying = [AST.TVar source; AST.TInferenceVar (source, "fixed")] in
 let pairs = if source = "" then List.concat_map (fun left -> List.map (fun right -> left, right) samples) samples else
   List.concat_map (fun left -> List.concat_map (fun right -> [left, right; right, left]) samples) varying in
 let comparisons = List.map (fun (left, right) -> list (fun op -> result plan (C.classifyComparison aliases registry lookup sums op left right)) [AST.Eq; AST.Neq; AST.Lt; AST.Gt; AST.Lte; AST.Gte]) pairs in
 tuple [`List perType; `List comparisons; expr (C.chainAndExpr []); expr (C.chainAndExpr [left; right; left]);
   `Bool (C.sumTypeHasPayload lookup "S"); `Bool (C.sumTypeHasPayload lookup "Absent"); option dispatch (C.tryDecodeInternalTypeApp left);
   result (fun () -> `Null) (C.validateCanonicalSortableCall aliases registry sums "Darklang.Stdlib.List.sortBy" [AST.TInt64; AST.TBlob]);
   result (fun () -> `Null) (C.validateCanonicalSortableCall aliases registry sums "Darklang.Stdlib.List.uniqueBy" [AST.TInt64; AST.TBlob])]

let observeHelpers source =
 let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
 AST.TVar source; AST.TInferenceVar (source, "fixed"); AST.TFunction ([AST.TInt64], AST.TString); AST.TFunction ([AST.TBlob], AST.TNever); AST.TTuple [AST.TInt64; AST.TString];
 AST.TRecord ("R", []); AST.TRecord ("R", [AST.TUnit]); AST.TRecord ("Recursive", []); AST.TRecord ("Opaque", []); AST.TRecord ("Missing", []); AST.TRecord ("S", []);
 AST.TSum ("S", []); AST.TSum ("BadSum", []); AST.TSum ("Generic", [AST.TString]); AST.TSum ("Generic", []); AST.TSum ("Uuid", []);
 AST.TList AST.TInt64; AST.TStream AST.TNever; AST.TDict (AST.TString, AST.TInt64); AST.TDict (AST.TInt64, AST.TString); AST.TDict (AST.TBlob, AST.TString)] in
 let lookup = M.of_list ["S.C", ("S", [], 0, [AST.TInt64]); "BadSum.C", ("BadSum", [], 0, [AST.TInternalRawPtr]); "Generic.C", ("Generic", ["a"], 0, [AST.TVar "a"])] in
 let raw = M.of_list ["R", ["field", AST.TInt64]; "Recursive", ["next", AST.TRecord ("Recursive", [])]; "Opaque", ["field", AST.TStream AST.TInt64]] in

 let aliases = M.singleton "Alias" ([], AST.TRecord ("R", [])) in
 let left = AST.Var source and right = AST.UnitLiteral in
 let samples = samples @ [AST.TRecord ("GenericRecord", [AST.TInt64; AST.TString]); AST.TRecord ("GenericRecord", []); AST.TSum ("Multiple", [AST.TInt64])] in
 let raw = M.add "GenericRecord" ["z", AST.TVar "a"; "aa", AST.TVar "b"; source, AST.TList (AST.TVar "a")] raw in
 let registry = Types.indexTypeRegistry lookup (M.of_list ["R", []; "Recursive", []; "Opaque", []; "GenericRecord", ["a"; "b"]]) raw in
 let lookup = M.add ("Multiple." ^ source) ("Multiple", ["a"], 7, [AST.TVar "a"; AST.TList AST.TString]) lookup
   |> M.add "Multiple.z" ("Multiple", ["a"], 3, []) |> M.add "Multiple.aa" ("Multiple", ["a"], 1, [AST.TString]) in
 let sums = Types.indexSumTypeRegistry lookup in
 list (fun value -> list (fun mode -> tuple [expr (EqualityHelpers.buildEqHelperExpr aliases registry lookup sums mode value left right);
   expr (OrderingHelpers.buildCompareHelperExpr aliases registry lookup sums mode value left right)]) [EqualityHelpers.ExpandCurrent; EqualityHelpers.UseHelperCall]) samples

let observeDependencies source =
 let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
 AST.TVar source; AST.TInferenceVar (source, "fixed"); AST.TFunction ([AST.TInt64], AST.TString); AST.TFunction ([AST.TBlob], AST.TNever); AST.TTuple [AST.TInt64; AST.TString];
 AST.TRecord ("R", []); AST.TRecord ("R", [AST.TUnit]); AST.TRecord ("Recursive", []); AST.TRecord ("Opaque", []); AST.TRecord ("Missing", []); AST.TRecord ("S", []);
 AST.TSum ("S", []); AST.TSum ("BadSum", []); AST.TSum ("Generic", [AST.TString]); AST.TSum ("Generic", []); AST.TSum ("Uuid", []);
 AST.TList AST.TInt64; AST.TStream AST.TNever; AST.TDict (AST.TString, AST.TInt64); AST.TDict (AST.TInt64, AST.TString); AST.TDict (AST.TBlob, AST.TString)] in
 let lookup = M.of_list ["S.C", ("S", [], 0, [AST.TInt64]); "BadSum.C", ("BadSum", [], 0, [AST.TInternalRawPtr]); "Generic.C", ("Generic", ["a"], 0, [AST.TVar "a"])] in
 let raw = M.of_list ["R", ["field", AST.TInt64]; "Recursive", ["next", AST.TRecord ("Recursive", [])]; "Opaque", ["field", AST.TStream AST.TInt64]] in

 let aliases = M.singleton "Alias" ([], AST.TRecord ("R", [])) in
 let left = AST.Var source and right = AST.UnitLiteral in
 let samples = samples @ [AST.TRecord ("GenericRecord", [AST.TInt64; AST.TString]); AST.TRecord ("GenericRecord", []); AST.TSum ("Multiple", [AST.TInt64])] in
 let raw = M.add "GenericRecord" ["z", AST.TVar "a"; "aa", AST.TVar "b"; source, AST.TList (AST.TVar "a")] raw in
 let registry = Types.indexTypeRegistry lookup (M.of_list ["R", []; "Recursive", []; "Opaque", []; "GenericRecord", ["a"; "b"]]) raw in
 let lookup = M.add ("Multiple." ^ source) ("Multiple", ["a"], 7, [AST.TVar "a"; AST.TList AST.TString]) lookup
   |> M.add "Multiple.z" ("Multiple", ["a"], 3, []) |> M.add "Multiple.aa" ("Multiple", ["a"], 1, [AST.TString]) in
 let sums = Types.indexSumTypeRegistry lookup in
 let encodeGenerated values = `Assoc ["map", list (fun (name, definition) -> tuple [str name; SemanticAST.observationFunctionDefNode typ definition]) (M.bindings values)] in
 let eqState (state : HelperDependencies.eqHelperGenerationState) = SemanticJson.record "EqHelperGenerationState" ["InProgress", `Assoc ["set", list str (StringOrder.Set.elements state.HelperDependencies.inProgress)]; "Generated", encodeGenerated state.HelperDependencies.generated] in
 let compareState (state : HelperDependencies.compareHelperGenerationState) = SemanticJson.record "CompareHelperGenerationState" ["InProgress", `Assoc ["set", list str (StringOrder.Set.elements state.HelperDependencies.inProgress)]; "Generated", encodeGenerated state.HelperDependencies.generated] in
 let eqInitial : HelperDependencies.eqHelperGenerationState = {HelperDependencies.inProgress = StringOrder.Set.empty; generated = M.empty} in
 let compareInitial : HelperDependencies.compareHelperGenerationState = {HelperDependencies.inProgress = StringOrder.Set.empty; generated = M.empty} in
 let perType = list (fun value -> tuple [eqState (HelperDependencies.ensureEqHelperForType aliases registry lookup sums value eqInitial);
   compareState (HelperDependencies.ensureCompareHelperForType aliases registry lookup sums value compareInitial)]) samples in
 let call name args = AST.applyNamedWithTypes name args (NonEmptyList.fromList [left; right]) in
 let expressions = [C.makeInternalTypeApp (C.EqHelperDispatchTypeApp (AST.TVar source, left, right));
   call "__compare" [AST.TInt64]; call "__compare" [AST.TVar source]; call "Darklang.Stdlib.List.sort" [AST.TString];
   call "Darklang.Stdlib.List.unique" [AST.TString]; call "Darklang.Stdlib.List.uniqueBy" [AST.TInt64; AST.TVar source];
   call "Darklang.Stdlib.List.sortBy" [AST.TString; AST.TInt64]; call source [AST.TString; AST.TVar source]] in
 let expressions = expressions @ [AST.TupleLiteral expressions; AST.If (left, AST.TupleLiteral expressions, right);
   AST.Match (left, [{AST.patterns = NonEmptyList.singleton AST.PWildcard; guard = Some (List.hd expressions); body = AST.TupleLiteral expressions}]);
   AST.InterpolatedString (List.map (fun value -> AST.StringExpr value) expressions)] in
 let collect expression = tuple [list typ (HelperDependencies.TypeSet.elements (HelperDependencies.collectEqHelperTypesFromExpr aliases expression));
   list typ (HelperDependencies.TypeSet.elements (HelperDependencies.collectCompareHelperTypesFromExpr aliases expression))] in
 tuple [perType; eqState (List.fold_left (fun state value -> HelperDependencies.ensureEqHelperForType aliases registry lookup sums value state) eqInitial samples);
   compareState (List.fold_left (fun state value -> HelperDependencies.ensureCompareHelperForType aliases registry lookup sums value state) compareInitial samples);
   list collect expressions]

let observeMaterialization source =
 let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
 AST.TVar source; AST.TInferenceVar (source, "fixed"); AST.TFunction ([AST.TInt64], AST.TString); AST.TFunction ([AST.TBlob], AST.TNever); AST.TTuple [AST.TInt64; AST.TString];
 AST.TRecord ("R", []); AST.TRecord ("R", [AST.TUnit]); AST.TRecord ("Recursive", []); AST.TRecord ("Opaque", []); AST.TRecord ("Missing", []); AST.TRecord ("S", []);
 AST.TSum ("S", []); AST.TSum ("BadSum", []); AST.TSum ("Generic", [AST.TString]); AST.TSum ("Generic", []); AST.TSum ("Uuid", []);
 AST.TList AST.TInt64; AST.TStream AST.TNever; AST.TDict (AST.TString, AST.TInt64); AST.TDict (AST.TInt64, AST.TString); AST.TDict (AST.TBlob, AST.TString)] in
 let lookup = M.of_list ["S.C", ("S", [], 0, [AST.TInt64]); "BadSum.C", ("BadSum", [], 0, [AST.TInternalRawPtr]); "Generic.C", ("Generic", ["a"], 0, [AST.TVar "a"])] in
 let raw = M.of_list ["R", ["field", AST.TInt64]; "Recursive", ["next", AST.TRecord ("Recursive", [])]; "Opaque", ["field", AST.TStream AST.TInt64]] in

 let aliases = M.singleton "Alias" ([], AST.TRecord ("R", [])) in
 let left = AST.Var source and right = AST.UnitLiteral in
 let samples = samples @ [AST.TRecord ("GenericRecord", [AST.TInt64; AST.TString]); AST.TRecord ("GenericRecord", []); AST.TSum ("Multiple", [AST.TInt64])] in
 let raw = M.add "GenericRecord" ["z", AST.TVar "a"; "aa", AST.TVar "b"; source, AST.TList (AST.TVar "a")] raw in
 let registry = Types.indexTypeRegistry lookup (M.of_list ["R", []; "Recursive", []; "Opaque", []; "GenericRecord", ["a"; "b"]]) raw in
 let lookup = M.add ("Multiple." ^ source) ("Multiple", ["a"], 7, [AST.TVar "a"; AST.TList AST.TString]) lookup
   |> M.add "Multiple.z" ("Multiple", ["a"], 3, []) |> M.add "Multiple.aa" ("Multiple", ["a"], 1, [AST.TString]) in
 let sums = Types.indexSumTypeRegistry lookup in
 let dispatch value = C.makeInternalTypeApp (C.EqHelperDispatchTypeApp (value, left, right)) in
 let compare value = AST.applyNamedWithTypes "__compare" [value] (NonEmptyList.fromList [left; right]) in
 let bodies = List.map (fun value -> AST.TupleLiteral [dispatch value; compare value]) samples in
 let definition name typeParams body : AST.functionDef = {AST.name; typeParams; params = NonEmptyList.singleton ("arg", AST.TUnit); returnType = AST.TUnit; body; recursion = None} in
 let topLevels = [AST.FunctionDef (definition source [] (AST.TupleLiteral bodies));
   AST.FunctionDef (definition "template" ["a"] (AST.TupleLiteral bodies));
   AST.FunctionDef (definition (C.eqHelperName (AST.TList AST.TInt64)) [] AST.UnitLiteral);
   AST.ValueDef (AST.UncheckedValueDef ("unchecked", AST.TupleLiteral bodies));
   AST.ValueDef (AST.CheckedValueDef ("checked", AST.TUnit, AST.TupleLiteral bodies));
   AST.TypeDef (AST.RecordDef ("R", [], ["field", AST.TInt64])); AST.Expression ([source], AST.TupleLiteral bodies)] in
 let encode = list (SemanticAST.observationTopLevelNode typ) in
 tuple [encode (MaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums aliases registry lookup sums topLevels);
   encode (MaterializeHelpers.materializeEqHelpersInTopLevels aliases registry lookup topLevels);
   encode (MaterializeHelpers.materializeCompareHelpersInTopLevels aliases registry lookup topLevels)]
