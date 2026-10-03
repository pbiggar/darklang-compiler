[@@@warning "-4"]
open Dark_compiler
module T = InstrumentedTypeSubstitution
module C = InstrumentedCheckedAST
module R = InstrumentedTypeRegistries
module W = InstrumentedWrittenChecking
module M = StringOrder.Map
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error message -> SemanticJson.union "FSharpResult" "Error" [str message]
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let map encode values = `Assoc ["map", list (fun (name, value) -> tuple [str name; encode value]) (M.bindings values)]
let fields = list (fun (name, value) -> tuple [str name; typ value])
let info (value : R.recordTypeInfo) = SemanticJson.record "RecordTypeInfo" ["TypeParams", list str value.R.typeParams; "Fields", fields value.R.fields]
let observe source =
 let types = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr; AST.TVar "a"; AST.TInferenceVar (source, "fixed"); AST.TRecord ("Alias", []); AST.TRecord ("G", [AST.TVar "a"]); AST.TSum ("Alias", []); AST.TSum ("G", [AST.TVar "a"]); AST.TTuple [AST.TVar "a"; AST.TString]; AST.TList (AST.TVar "a"); AST.TStream (AST.TVar "a"); AST.TDict (AST.TVar "a", AST.TInferenceVar (source,"fixed")); AST.TFunction ([AST.TVar "a"], AST.TInferenceVar (source,"fixed"))] in
 let substitutions = [M.empty; M.of_list ["a", AST.TInt64; "fixed", AST.TString]; M.of_list ["a", AST.TVar "fixed"; "fixed", AST.TInt64]] in
 let aliases = M.of_list ["Alias", ([], AST.TList AST.TString); "G", (["a"], AST.TTuple [AST.TVar "a"; AST.TRecord ("Alias", [])])] in
 let records = M.of_list ["R", {R.typeParams = ["a"; "phantom"]; fields = [source, AST.TVar "a"; source, AST.TBool; "alias", AST.TRecord ("Alias", [])]}; "Empty", {R.typeParams = []; fields = []}] in
 let observeProgram program =
  let bodies = C.programTopLevels program |> List.filter_map (function C.FunctionDef value -> Some value.C.body | C.ValueDef value -> Some value.C.body | C.Expression value -> Some value | C.TypeDef _ -> None) in
  let functions = C.programTopLevels program |> List.filter_map (function C.FunctionDef value -> Some value | _ -> None) in
  let specialized func args = try result C.observationFunctionDef (Ok (T.specializeFunction (AST.functionId 9L) func args)) with Failure message -> result C.observationFunctionDef (Error message) in
  tuple [C.observationProgram program;
   list (fun subst -> list C.observationExpr (List.map (T.applySubstToExpr subst) bodies)) substitutions;
   list (fun func -> tuple [C.observationFunctionDef (T.resolveAliasesInFunction aliases func); list (specialized func) [[]; [AST.TInt64]; [AST.TString; AST.TBool]]]) functions] in
 let sourceProgram source = result (fun (_, program, _) -> observeProgram program) (Result.bind (WrittenParsing.parse Validation.Script source) (fun unit -> W.checkSourceUnitsWithBase None false false [unit])) in
 let fixtures = if source = "" then ["let id (x: 'a) : 'a = x
id 1"; "type Alias = String
let f (x: Alias) : Alias = x
f \"a\""; "let recur (x: Int64) : Int64 = if x == 0 then x else recur (x - 1)
recur 1"; "let f (x: 'a) : 'a = (fun (y: 'a) -> y) x
f 1"] else [] in
 let bindings = List.concat_map (fun value -> [["a", AST.TVar "a"; "a", value]; ["a", value; "a", AST.TVar "a"]; ["a", AST.TInt64; "a", value]; ["a", AST.TStream (AST.TVar "a"); "a", value]; ["a", AST.TInferenceVar (source, "first"); "a", AST.TVar "b"; "z", value]]) types in
 tuple [list (fun subst -> list typ (List.map (T.applySubstToType subst) types)) substitutions;
 list (fun pattern -> list (fun actual -> result fields (T.matchTypePattern pattern actual)) types) types;
 list (fun values -> result (map typ) (T.consolidateTypeBindings values)) bindings;
 list typ (List.map (T.resolveAliasType aliases) types); map info (T.resolveAliasesInTypeRegistry aliases records);
 map (fun info -> tuple [fields (T.firstDeclaredRecordFields info.R.fields);
  list (fun args -> tuple [option (map typ) (T.buildDeclaredRecordFieldSubst info args); SemanticANF.aNF_recordDescriptor (T.recordDescriptor source args info);
   result SemanticANF.aNF_recordDescriptor (T.boxedSumDescriptor source info.R.typeParams args (List.map snd info.R.fields))]) [[]; [AST.TInt64]; [AST.TInt64; AST.TBool]]]) records;
 list (result observeProgram) (C.observationFixtures source);
 sourceProgram source; list sourceProgram fixtures]
