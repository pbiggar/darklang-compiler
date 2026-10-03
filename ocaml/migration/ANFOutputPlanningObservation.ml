(* Complete output-planning and destruction-proof boundary observations. *)
[@@@warning "-4"]
open Dark_compiler
module A = InstrumentedANF
module R = InstrumentedTypeRegistries
module E = InstrumentedEscapeAnalysisFacts
module P = InstrumentedPrintInsertion
module M = StringOrder.Map
module J = SemanticANF
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let capture f = try Ok (f ()) with Failure message -> Error message
let expressionResult (expression, gen) = tuple [J.aNF_aExpr expression;J.aNF_varGen gen]
let observe source =
 let primitives = [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TChar;AST.TDateTime;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TVar source;AST.TInferenceVar (source,"fixed");AST.TFunction ([AST.TInt64],AST.TString);AST.TStream AST.TString] in
 let record params fields : R.recordTypeInfo = {R.typeParams=params;fields} in
 let records = M.of_list [
  "Regular",record [] [source,AST.TString;"next",AST.TRecord ("Regular",[])];
  "Generic",record ["a"] [source,AST.TVar "a";"next",AST.TRecord ("Generic",[AST.TVar "a"])];
  "Growing",record ["a"] ["next",AST.TRecord ("Growing",[AST.TList (AST.TVar "a")])];
  "Closure",record [] ["value",AST.TFunction ([AST.TInt64],AST.TString)];
  "Mixed",record [] ["value",AST.TSum ("Sum",[])];
  "Alias",record [] ["value",AST.TRecord ("Sum",[])]] in
 let sum params payloads : MemoryModel.rcSumShapeInfo = {MemoryModel.typeParams=params;payloads;unaryPayloadTags=MemoryModel.IntSet.empty} in
 let sums = M.of_list [
  "Sum",sum [] [0,None;1,Some (AST.TTuple [AST.TString;AST.TSum ("Sum",[])])];
  "GenericSum",sum ["a"] [0,None;1,Some (AST.TTuple [AST.TVar "a";AST.TSum ("GenericSum",[AST.TVar "a"])])];
  "GrowingSum",sum ["a"] [0,None;1,Some (AST.TSum ("GrowingSum",[AST.TList (AST.TVar "a")]))];
  "ClosureSum",sum [] [1,Some (AST.TFunction ([AST.TInt64],AST.TString))];
  "EmptySum",sum [] []] in
 let types = primitives @ List.map (fun typ -> AST.TList typ) primitives @ List.map (fun typ -> AST.TTuple [AST.TString;typ]) primitives @ [
  AST.TRecord ("Missing",[]);AST.TRecord ("Regular",[]);AST.TRecord ("Regular",[AST.TInt64]);
  AST.TRecord ("Generic",[AST.TString]);AST.TRecord ("Generic",[AST.TFunction ([AST.TInt64],AST.TString)]);
  AST.TRecord ("Growing",[AST.TInt64]);AST.TRecord ("Closure",[]);AST.TRecord ("Mixed",[]);AST.TRecord ("Alias",[]);
  AST.TSum ("Sum",[]);AST.TRecord ("Sum",[]);AST.TSum ("GenericSum",[AST.TInt64]);AST.TSum ("GenericSum",[AST.TStream AST.TString]);
  AST.TSum ("GrowingSum",[AST.TInt64]);AST.TSum ("ClosureSum",[]);AST.TSum ("EmptySum",[]);AST.TSum ("Sum",[AST.TString]);
  AST.TDict (AST.TString,AST.TList (AST.TRecord ("Regular",[])));AST.TDict (AST.TString,AST.TFunction ([AST.TInt64],AST.TString))] in
 let destruction = List.concat_map (fun typeReg -> List.concat_map (fun sumReg -> List.map (fun allow -> List.map (E.hasNonObservableDestruction typeReg sumReg allow) types) [false;true]) [M.empty;sums]) [M.empty;records] in
 let descriptors = List.concat_map (fun typ -> List.map (fun valueType ->
   let descriptor : A.recordDescriptor = {A.sourceTypeName=source;runtimeTypeName=source;typeArgs=[];fields=[source,typ;"next",AST.TString];valueType} in
   E.descriptorHasNonObservableDestruction records sums descriptor) [AST.TRecord (source,[]);AST.TSum (source,[])]) types in
 let supported = [AST.TInt64;AST.TInt;AST.TBool;AST.TString;AST.TChar;AST.TFloat64;AST.TList AST.TInt64] in
 let printTypes = primitives @ List.map (fun typ -> AST.TList typ) supported @ List.map (fun typ -> AST.TSum ("Darklang.Stdlib.Option.Option",[AST.TList typ])) supported @ [AST.TSum ("Uuid",[]);AST.TList AST.TBlob;AST.TSum ("Darklang.Stdlib.Option.Option",[AST.TList AST.TBlob])] in
 let helperNames = ["Darklang.Stdlib.List.__toDisplayString_i64";"Darklang.Stdlib.List.__toDisplayString_int";"Darklang.Stdlib.List.__toDisplayString_bool";"Darklang.Stdlib.List.__toDisplayString_str";"Darklang.Stdlib.List.__toDisplayString_char";"Darklang.Stdlib.List.__toDisplayString_f64";"Darklang.Stdlib.List.__toDisplayString_list_i64";"Darklang.Stdlib.Float.toString";"Darklang.Stdlib.DateTime.toString";"Darklang.Stdlib.Uuid.toString"] in
 let ids = M.of_list (List.mapi (fun index name -> name,AST.functionId (Int64.of_int (index+1))) helperNames) in
 let resolve name = match M.find_opt name ids with Some id -> id | None -> Crash.crash ("Missing observation helper: " ^ name) in
 let render = AST.functionId 11L in
 let functionNames = FunctionIdMap.ofList [render,"__dark_render_value_" ^ source;AST.functionId 12L,"ordinary"] in
 let bodies = [
  A.Return A.UnitLiteral;A.Return (A.Var (A.TempId 3));A.Return (A.StringLiteral source);A.Return (A.FloatLiteral (-0.));
  A.Return (A.IntLiteral (A.Int64 Int64.min_int));
  A.Join ({A.id=A.TempId 50;typ=AST.TInt64},A.Return (A.Var (A.TempId 50)),A.If (A.BoolLiteral true,A.Return (A.Var (A.TempId 3)),A.Jump (A.TempId 50,A.Var (A.TempId 4))));
  A.Let (A.TempId 10,A.RuntimeError source,A.Return A.UnitLiteral);
  A.Let (A.TempId 10,A.Call (render,[A.Var (A.TempId 3)]),A.Return (A.Var (A.TempId 10)));
  A.Let (A.TempId 10,A.Call (AST.functionId 12L,[A.Var (A.TempId 3)]),A.Jump (A.TempId 50,A.Var (A.TempId 10)))] in
 let fn name body id : A.functionDef = {A.id=AST.functionId id;name;typedParams=[];returnType=AST.TString;returnOwnership=A.OwnedReturn;body} in
 let functions = [fn "entry" (List.nth bodies 7) 11L;fn "ordinary" (List.nth bodies 8) 12L;fn "entry" (List.nth bodies 5) 13L] in
 tuple [list (fun typ -> tuple [`Bool (E.isScalarType typ);option SemanticJson.string (ListDisplay.getDisplayStringFunc typ)]) types;
  list (list (fun value -> `Bool value)) destruction;list (fun value -> `Bool value) descriptors;
  list (fun typ -> list (fun body -> result expressionResult (capture (fun () -> P.wrapReturnWithPrint resolve typ (A.VarGen 100) body))) bodies) printTypes;
  list (fun typ -> result J.aNF_program (capture (fun () -> P.insertPrint ids functions (List.nth bodies 5) typ))) printTypes;
  list (fun entry -> list (fun typ -> result (list J.aNF_functionDef) (P.insertPrintInEntry ids entry typ functions)) [AST.TString;AST.TList AST.TInt64]) ["entry";"ordinary";"missing"];
  list (fun entry -> list (fun tupleWords -> result (list J.aNF_functionDef) (P.insertRootWordProbeInEntry functionNames entry tupleWords functions)) [false;true]) ["entry";"ordinary";"missing"]]
