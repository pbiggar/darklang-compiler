open Dark_compiler
module W = InstrumentedWrittenChecking
module T = InstrumentedWrittenTypeSupport
module C = InstrumentedCheckedAST
let tuple values = `Assoc ["tuple", `List values]
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let list encode values = `List (List.map encode values)
let strings = list str
let map encode values = `Assoc ["map", list (fun (name, value) -> tuple [str name; encode value]) (StringOrder.Map.bindings values)]
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error value -> SemanticJson.union "FSharpResult" "Error" [str value]
let record = SemanticJson.record
let fn id = SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind", `String "uint64"; "value", `String (Z.to_string (if AST.functionIdValue id < 0L then Z.add (Z.of_int64 (AST.functionIdValue id)) (Z.shift_left Z.one 64) else Z.of_int64 (AST.functionIdValue id)))]]
let environment value =
 let globals, symbols = W.observationEnvironment value in
 let signature (value : T.functionSignature) = record "FunctionSignature" ["Id", fn value.T.id; "TypeParams", strings value.T.typeParams; "Parameters", list typ value.T.parameters; "Return", typ value.T.return] in
 let entry (value : T.typeEntry) = record "TypeEntry" ["Kind", SemanticJson.union "TypeKind" (match value.T.kind with T.RecordKind -> "RecordKind" | T.SumKind -> "SumKind" | T.AliasKind -> "AliasKind") []; "Params", strings value.T.params; "Path", strings value.T.path; "Definition", SemanticJson.typeDefinition value.T.definition] in
 let locals = map (fun (value, id) -> tuple [typ value; C.observationBinding id]) in
 let globals = record "Globals" ["Functions", map signature globals.T.functions; "Values", locals globals.T.values; "Types", map entry globals.T.types; "CollidingCases", `Assoc ["set", strings (StringOrder.Set.elements globals.T.collidingCases)]; "AllowInternal", `Bool globals.T.allowInternal; "TypeParams", `Assoc ["set", strings (StringOrder.Set.elements globals.T.typeParams)]; "ModulePath", strings globals.T.modulePath; "CurrentFunction", option (fun (id, name, params) -> tuple [fn id; str name; strings params]) globals.T.currentFunction] in
 SemanticJson.union "Environment" "Environment" [globals; C.observationGlobalCatalog symbols]
let program (typ_, value) = tuple [typ typ_; C.observationProgram value]
let checked (typ_, value, env) = tuple [typ typ_; C.observationProgram value; environment env]
let baseText = "type R = { field: Int64 }\nlet id (x: 'a) : 'a = x\nval value = 1\n"
let observe source =
 let parse = WrittenParsing.parse Validation.Script in
 let base = Result.bind (parse baseText) (fun unit -> W.checkSourceUnitsWithBase None false false [unit]) in
 let perSource source = result (fun validated ->
  let cases = [false, false; false, true; true, false; true, true] in
  tuple [result program (W.checkClosedProgram validated);
   list (fun require -> result program (W.checkSimpleProgram require validated)) [false; true];
   list (fun (internal, require) -> result checked (W.checkSourceUnitsWithBase None internal require [validated])) cases;
   result (fun (_, _, env) -> list (fun (internal, require) -> result checked (W.checkSourceUnitsWithBase (Some env) internal require [validated])) cases) base;
   result (fun (_, value, _) -> ProgramObservation.env (W.typeCheckEnvironment value)) (W.checkSourceUnitsWithBase None false false [validated])]) (parse source) in
 let fixtures = if source <> "" then [] else ["()"; "1"; "true"; "fun x -> x"; "(fun x -> x) 1"; "let x = 1 in x + 2"; "if true then 1 else 2"; "[1; 2]"; "Dict { \"x\" = 1; \"x\" = 2 }"; "(1, true)"; "match true with | true -> 1 | false -> 2"; "type R = { field: Int64 }\nR { field = 1 }"; "type S = C of Int64 | D\nS.C 1"; "type Box<'a> = { value: 'a }\nBox { value = 1 }"; "let id (x: 'a) : 'a = x\nid 1"; "let fact (n: Int64) : Int64 = if n == 0 then 1 else n * fact (n - 1)\nfact 5"; "val x = 1\nval y = x\ny"; "Builtin.boolNot true"; "Builtin.bitwiseNot 1uy"; "Builtin.testNan"; "1 ++ 'a'"; "let x = [1] in x"; "fun __x -> __x"; "id 1"; "R { field = value }"] in
 tuple [result checked base; perSource source; list perSource fixtures]
