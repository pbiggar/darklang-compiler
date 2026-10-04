(* Observe complete generated renderers and eval-boundary expression rewrites. *)
open Dark_compiler
module C=InstrumentedCheckedAST
module T=InstrumentedTypes
module V=InstrumentedValueRendering
module M=StringOrder.Map
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let attempt action=try result C.observationProgram (Ok (action ())) with Failure message|Invalid_argument message->result C.observationProgram (Error message)
let observe source=
 let records=M.of_list ["R",([],["z",AST.TInt64;source,AST.TString;"a",AST.TBool]);"Box",(["a"],["value",AST.TVar "a"]);"Fallback",([],["value",AST.TVar "a"]);"StreamField",([],["value",AST.TStream (AST.TVar "a")]);"Node",([],["value",AST.TInt64;"children",AST.TList (AST.TRecord ("Node",[]))]);"Empty",([],[])] in
 let sums=M.of_list ["Choice",([],["Many",2,[AST.TString;AST.TInt64];"Zero",0,[];"One",1,[AST.TBool]]);"GSum",(["a"],["Pair",3,[AST.TVar "a";AST.TList (AST.TVar "a")]]);"Tree",([],["Empty",0,[];"Branch",1,[AST.TInt64;AST.TList (AST.TSum ("Tree",[]))]]);"Single",([],["S",0,[]])] in
 let recordMetadata=M.map (fun (typeParams,fields)->({T.fields;fieldTypes=M.of_list fields;typeParams}:T.recordTypeInfo)) records in
 let sumMetadata=M.map (fun (typeParams,variants)->({T.typeParams;variants=List.map (fun (name,tag,fields)->({T.name;tag;fields}:T.sumVariantInfo)) variants}:T.sumTypeInfo)) sums in
 let namedId,symbols=C.internFunction ("named_"^source) (C.emptySymbols ()) in
 let symbols=M.fold (fun owner (_,variants) symbols->List.fold_left (fun symbols (name,tag,_)->snd (C.internConstructor owner name tag symbols)) symbols variants) sums symbols in
 let valueId,symbols=C.allocateBinding "value" symbols in let parameter,symbols=C.allocateBinding "__partial_0" symbols in let other,symbols=C.allocateBinding "ordinary" symbols in let capture,symbols=C.allocateBinding "__partial_capture_0" symbols in
 let types=[AST.TUnit;AST.TBool;AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TFloat64;AST.TString;AST.TChar;AST.TDateTime;AST.TBlob;AST.TInternalRawPtr;AST.TNever;AST.TTuple [];AST.TTuple [AST.TString;AST.TList AST.TBool];AST.TList AST.TString;AST.TList (AST.TList AST.TInt64);AST.TStream (AST.TVar source);AST.TDict (AST.TString,AST.TInt64);AST.TDict (AST.TInt64,AST.TList AST.TString);AST.TDict (AST.TVar "a",AST.TVar "b");AST.TDict (AST.TInferenceVar ("s","a"),AST.TVar "b");AST.TDict (AST.TVar "a",AST.TInferenceVar ("s","b"));AST.TDict (AST.TInferenceVar ("s","a"),AST.TInferenceVar ("s","b"));AST.TRecord ("R",[]);AST.TRecord ("Box",[AST.TString]);AST.TRecord ("Fallback",[AST.TInt64]);AST.TRecord ("StreamField",[]);AST.TRecord ("Node",[]);AST.TRecord ("Empty",[]);AST.TSum ("Uuid",[]);AST.TSum ("Choice",[]);AST.TRecord ("Choice",[]);AST.TSum ("GSum",[AST.TString]);AST.TSum ("Tree",[]);AST.TFunction ([AST.TInt64],AST.TString);AST.TRecord ("Box",[]);AST.TSum ("GSum",[]);AST.TRecord ("missing",[]);AST.TSum ("missing",[]);AST.TVar source;AST.TInferenceVar (source,"id")] in
 let program expressions=C.programFromCheckedParts (symbols,List.map (fun expr->C.Expression expr) expressions) in
 let render typ=V.rewriteProgram recordMetadata sumMetadata typ (program [C.Local valueId;C.Local valueId]) in
 let rendered=list (fun typ->attempt (fun ()->render typ)) types in
 let primitive=attempt (fun ()->V.rewriteProgram M.empty M.empty AST.TString (program [C.StringLiteral source])) in
 let reused=list (fun typ->attempt (fun ()->let rendered=render typ in V.rewriteProgram recordMetadata sumMetadata typ rendered)) [AST.TString;AST.TRecord ("Node",[]);AST.TSum ("Tree",[])] in
 let lambda binding body=C.Lambda (NonEmptyList.singleton {C.pattern=C.LPVariable binding;typ=C.checkedType AST.TInt64},None,body) in
 let namedCall=C.Call (namedId,NonEmptyList.fromList [C.StringLiteral source;C.Local parameter]) in
 let partial=lambda parameter namedCall in
 let expressions=[C.FuncRef namedId;C.Local valueId;lambda other (C.Local other);partial;C.Let (C.LPVariable capture,C.StringLiteral source,partial);lambda parameter (C.TypeApp (namedId,[C.checkedType AST.TInt64],NonEmptyList.fromList [C.StringLiteral source;C.Local parameter]));lambda parameter (C.Call (namedId,NonEmptyList.singleton (C.Local parameter)));lambda parameter (C.Call (namedId,NonEmptyList.fromList [C.Local parameter;C.StringLiteral source]));lambda other (C.Call (namedId,NonEmptyList.fromList [C.StringLiteral source;C.Local other]))] in
 let boundaries=list (fun typ->attempt (fun ()->V.rewriteProgram recordMetadata sumMetadata typ (program expressions))) [AST.TFunction ([AST.TInt64],AST.TString);AST.TDateTime] in
 let dictionaryKeys=list (fun typ->attempt (fun ()->let key,symbols=C.allocateBinding "key" symbols in let id,symbols=C.internFunction ("Darklang.Stdlib.Dict.__renderGenericKey_"^source) symbols in let fn={C.id;name="Darklang.Stdlib.Dict.__renderGenericKey_"^source;typeParams=[];params=C.checkedParams (NonEmptyList.singleton (key,typ));returnType=C.checkedType AST.TString;body=C.StringLiteral "original";recursion=None} in let program=C.programFromCheckedParts (symbols,[C.FunctionDef fn;C.Expression (C.StringLiteral source)]) in V.rewriteDictionaryKeyRenderers recordMetadata sumMetadata (V.rewriteDictionaryKeyRenderers recordMetadata sumMetadata program))) types in
 tuple [rendered;primitive;reused;boundaries;dictionaryKeys]
