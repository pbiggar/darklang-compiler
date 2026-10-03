open Dark_compiler
module W = WrittenTypeSupport
module WT = WrittenTypes
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let observe source =
 let r = WT.synthRange in
 let id name : WT.identifier = {WT.range = r; name} in
 let custom modules name args = WT.TCustom {WT.range = r; modules = List.map (fun name -> id name, r) modules; typ = id name; typeArgs = args} in
 let references = [WT.TUnit r; WT.TBool r; WT.TInt r; WT.TInt8 r; WT.TUInt8 r; WT.TInt16 r; WT.TUInt16 r; WT.TInt32 r; WT.TUInt32 r; WT.TInt64 r; WT.TUInt64 r; WT.TInt128 r; WT.TUInt128 r; WT.TFloat r; WT.TChar r; WT.TString r; WT.TDateTime r; WT.TUuid r; WT.TBlob r;
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
 let scopes = [StringOrder.Set.empty; StringOrder.Set.of_list [source; "a"]] in
 let customResolver modules name args = if name = "reject" then Error "custom rejected" else Ok (AST.TRecord (String.concat "." (modules @ [name]), args)) in
 let conversions = list (fun reference -> tuple [list (fun scope -> result SemanticAST.semanticType (W.typeReference customResolver scope reference)) scopes;
  list (fun scope -> list (fun internal -> result SemanticAST.semanticType (W.resolveWrittenType internal inventory ["M"] scope reference)) [false; true]) scopes;
  list SemanticJson.string (W.collectWrittenTypeParams ["existing"] reference)]) references in
 let semTypes = [AST.TUnit; AST.TInt64; AST.TString; AST.TChar; AST.TNever; AST.TVar source; AST.TList (AST.TVar "a"); AST.TList AST.TInt64; AST.TRecord ("R", []); AST.TRecord ("Generic", [AST.TString]); AST.TSum ("S", [])] in
 let requirements = list (fun left -> list (fun right -> result (fun () -> `Null) (W.requireType (Some left) right)) semTypes) semTypes in
 let floats = [false, "1", "0"; true, "0", "0"; false, "1_2", "0"; false, "1", "5e309"; false, " 1", "5 "; false, "NaN", ""; false, "1", "2e-5000"; false, source, "0"; false, "Infinity", ""; false, "1", "2e+3"] in
 tuple [conversions; requirements; list SemanticJson.string (StringOrder.Set.elements (W.collidingCaseNames inventory));
  list (fun (negative, whole, fraction) -> option (fun value -> `Assoc ["kind", `String "float64"; "value", `String (Printf.sprintf "%016Lx" (Int64.bits_of_float value))]) (WrittenPatternSupport.floatLiteral negative whole fraction)) floats]
