open Dark_compiler
module W = InstrumentedWrittenTypeSupport
module P = InstrumentedWrittenPatternSupport
module C = InstrumentedCheckedAST
module WT = WrittenTypes
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let observe source =
 let r = WT.synthRange in
 let id name : WT.identifier = {WT.range = r; name} in
 let custom modules name args = WT.TCustom {WT.range = r; modules = List.map (fun name -> id name, r) modules; typ = id name; typeArgs = args} in
 let _references = [WT.TUnit r; WT.TBool r; WT.TInt r; WT.TInt8 r; WT.TUInt8 r; WT.TInt16 r; WT.TUInt16 r; WT.TInt32 r; WT.TUInt32 r; WT.TInt64 r; WT.TUInt64 r; WT.TInt128 r; WT.TUInt128 r; WT.TFloat r; WT.TChar r; WT.TString r; WT.TDateTime r; WT.TUuid r; WT.TBlob r;
  WT.TVariable (r, r, (r, source)); WT.TVariable (r, r, (r, "a")); WT.TList (r, r, r, WT.TVariable (r, r, (r, "a")), r);
  WT.TDict (r, r, r, WT.TString r, r, WT.TVariable (r, r, (r, source)), r);
  WT.TTuple (r, WT.TVariable (r, r, (r, "a")), r, WT.TString r, [r, WT.TVariable (r, r, (r, source))], r, r);
  WT.TFn (r, [WT.TVariable (r, r, (r, "a")), r; WT.TInt64 r, r], custom ["M"] source [WT.TVariable (r, r, (r, source))]);
  custom [] "RawPtr" []; custom [] "Stream" [WT.TInt64 r]; custom [] "Stream" []; custom ["M"] source [WT.TString r]; custom [] "R" []; custom [] "Alias" []; custom [] "S" []; custom [] "Generic" [WT.TString r]; custom [] "Generic" []; custom [] "Cycle" []] in
 let field name typ : WT.recordFieldSyntax = {WT.range = r; name = r, name; typ; description = ""; symbolColon = r} in
 let enum name : WT.enumCaseSyntax = {WT.range = r; name = r, name; fields = []; description = ""; keywordOf = None} in
 let entry kind params path definition : W.typeEntry = {W.kind; params; path; definition} in
 let inventory = StringOrder.Map.of_list ["R", entry W.RecordKind [] [] (WT.TDRecord [field source (WT.TInt64 r), None]);
  "Alias", entry W.AliasKind [] [] (WT.TDAlias (WT.TInt64 r)); "S", entry W.SumKind [] [] (WT.TDEnum [r, enum "C"]);
  "Other", entry W.SumKind [] [] (WT.TDEnum [r, enum "C"]); "Generic", entry W.RecordKind ["a"] [] (WT.TDRecord [field "value" (WT.TVariable (r, r, (r, "a"))), None]);
  "Cycle", entry W.AliasKind [] [] (WT.TDAlias (custom [] "Cycle" [])); "M.R", entry W.RecordKind [] ["M"] (WT.TDRecord [])] in
 let globals = {W.emptyGlobals with W.types = inventory; W.collidingCases = W.collidingCaseNames inventory; W.modulePath = ["M"]} in
 let symbols = C.emptySymbols () in
 let locals values = `Assoc ["map", list (fun (name, (typ, id)) -> tuple [SemanticJson.string name; tuple [SemanticAST.semanticType typ; C.observationBinding id]]) (StringOrder.Map.bindings values)] in
 let checked encode (value, bindings, symbols) = tuple [encode value; locals bindings; C.observationGlobalCatalog symbols] in
 let patterns = [WT.MPVariable (r, "_"); WT.MPVariable (r, source); WT.MPUnit r; WT.MPBool (r, true); WT.MPInt (r, (r, Z.one)); WT.MPInt64 (r, (r, 1L), r);
  WT.MPInt8 (r, (r, 1), r); WT.MPUInt8 (r, (r, 1), r); WT.MPInt16 (r, (r, 1), r); WT.MPUInt16 (r, (r, 1), r); WT.MPInt32 (r, (r, 1l), r); WT.MPUInt32 (r, (r, 1L), r); WT.MPUInt64 (r, (r, 1L), r); WT.MPInt128 (r, (r, Z.one), r); WT.MPUInt128 (r, (r, Z.one), r);
  WT.MPString (r, Some (r, source), r, r); WT.MPChar (r, Some (r, source), r, r); WT.MPString (r, None, r, r); WT.MPFloat (r, false, "1", "0"); WT.MPFloat (r, false, "1_2", "0");
  WT.MPTuple (r, WT.MPVariable (r, source), r, WT.MPVariable (r, "y"), [], r, r); WT.MPList (r, [WT.MPVariable (r, source), None], r, r); WT.MPListCons (r, WT.MPVariable (r, source), WT.MPVariable (r, "tail"), r);
  WT.MPEnum (r, (r, "C"), []); WT.MPEnum (r, (r, "Missing"), []); WT.MPOr (r, []); WT.MPOr (r, [WT.MPVariable (r, source); WT.MPVariable (r, source)]);
  WT.MPOr (r, [WT.MPVariable (r, source); WT.MPVariable (r, "other")]); WT.MPError r] in
 let types = [AST.TUnit; AST.TInt64; AST.TInt; AST.TBool; AST.TString; AST.TChar; AST.TFloat64; AST.TNever; AST.TVar source; AST.TInferenceVar (source, "fixed"); AST.TTuple [AST.TInt64; AST.TString]; AST.TList AST.TInt64; AST.TSum ("S", [])] in
 let cases = if source = "" then List.concat_map (fun pattern -> List.map (fun typ -> pattern, typ) types) patterns else List.mapi (fun index pattern -> pattern, List.nth types (index mod List.length types)) patterns in
 let results = list (fun (pattern, expected) -> result (checked C.observationPattern) (P.checkMatchPattern globals symbols None expected pattern)) cases in
 let letPatterns = [WT.LPUnit r; WT.LPWildcard r; WT.LPVariable (r, source); WT.LPTuple (r, WT.LPVariable (r, source), r, WT.LPVariable (r, "y"), [], r, r); WT.LPTuple (r, WT.LPVariable (r, source), r, WT.LPVariable (r, source), [], r, r)] in
 let letResults = list (fun pattern -> list (fun typ -> result (checked C.observationLetPattern) (P.checkLetPattern pattern typ symbols)) types) letPatterns in
 let boolCase pattern guard : C.matchCase = {C.patterns = NonEmptyList.singleton pattern; guard; body = C.UnitLiteral} in
 let witnesses = [AST.TBool, C.BoolLiteral true, [boolCase (C.PBool true) None]; AST.TBool, C.Local (AST.topLevelValueId "value"), [boolCase (C.PBool true) None];
  AST.TBool, C.Local (AST.topLevelValueId "value"), [boolCase (C.PBool true) None; boolCase (C.PBool false) None];
  AST.TBool, C.BoolLiteral true, [boolCase C.PWildcard (Some (C.BoolLiteral true))];
  AST.TList AST.TInt64, C.Local (AST.topLevelValueId "value"), [boolCase (C.PList []) None; boolCase (C.PListCons ([C.PWildcard], C.PWildcard)) None];
  AST.TTuple [AST.TBool; AST.TBool], C.Local (AST.topLevelValueId "value"), [boolCase (C.PTuple [C.PBool true; C.PWildcard]) None; boolCase (C.PTuple [C.PBool false; C.PBool true]) None; boolCase (C.PTuple [C.PBool false; C.PBool false]) None]] in
 tuple [results; letResults; list (fun (typ, value, cases) -> `Bool (P.matchIsExhaustive globals symbols typ value cases)) witnesses]
