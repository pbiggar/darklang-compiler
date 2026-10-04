(* Observe full generated JSON codec programs and bounded session behavior. *)
open Dark_compiler
module C=InstrumentedCheckedAST
module T=InstrumentedTypes
module J=InstrumentedJsonPlanning
module M=StringOrder.Map
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let attempt f action=try result f (Ok (action ())) with Failure message|Invalid_argument message->result f (Error message)
let observe source=
 let emptyEnv=match WrittenParsing.parse Validation.Script "()" |> fun parsed->Result.bind parsed (fun parsed->InstrumentedWrittenChecking.checkSourceUnitsWithBase None true false [parsed]) with Ok (_,program,_)->InstrumentedWrittenChecking.typeCheckEnvironment program|Error message->failwith message in
 let records=M.of_list ["R",([],["z",AST.TInt64;"a",AST.TString;"field",AST.TBool]);"Box",(["a"],["value",AST.TVar "a"]);"Node",([],["value",AST.TInt64;"children",AST.TList (AST.TRecord ("Node",[]))]);"Darklang.LanguageTools.RuntimeTypes.NameResolution",(["a"],["originalName",AST.TList AST.TString;"resolved",AST.TSum ("Darklang.Stdlib.Result.Result",[AST.TVar "a";AST.TUnit])])] in
 let sums=M.of_list ["Choice",([],["Many",2,[AST.TString;AST.TInt64];"Zero",0,[];"One",1,[AST.TBool]]);"GSum",(["a"],["Pair",3,[AST.TVar "a";AST.TList (AST.TVar "a")]]);"Tree",([],["Empty",0,[];"Branch",1,[AST.TInt64;AST.TList (AST.TSum ("Tree",[]))]])] in
 let standard=["Darklang.Stdlib.Result.Result",["Ok";"Error"];"Darklang.Stdlib.Option.Option",["None";"Some"];"Darklang.LanguageTools.RuntimeTypes.Hash",["Hash"];"Darklang.LanguageTools.RuntimeTypes.FQTypeName.FQTypeName",["Package"];"Darklang.LanguageTools.RuntimeTypes.TypeReference",["TUnit";"TBool";"TInt8";"TUInt8";"TInt16";"TUInt16";"TInt32";"TUInt32";"TInt64";"TUInt64";"TInt128";"TUInt128";"TInt";"TFloat";"TChar";"TString";"TBlob";"TUuid";"TDateTime";"TList";"TDict";"TTuple";"TFn";"TCustomType";"TVariable"];"Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part",["Root";"Index";"Field"];"Darklang.Stdlib.Json.ParseError.ParseError",["CantMatchWithType";"EnumMissingField";"EnumExtraField";"RecordMissingField";"RecordDuplicateField";"EnumInvalidCasename";"EnumTooManyCases";"NotJson"];"Darklang.Stdlib.Json.InternalEnumObject",["EnumNoFields";"EnumOneField";"EnumManyFields"]] in
 let serializeId,symbols=C.internFunction "Darklang.Stdlib.Json.serialize" (C.emptySymbols ()) in
 let parseId,symbols=C.internFunction "Darklang.Stdlib.Json.parse" symbols in
 let userId,symbols=C.internFunction "user" symbols in
 let symbols=List.fold_left (fun symbols (owner,cases)->List.mapi (fun tag name->tag,name) cases |> List.fold_left (fun symbols (tag,name)->snd (C.internConstructor owner name tag symbols)) symbols) symbols standard in
 let symbols=M.fold (fun owner (_,variants) symbols->List.fold_left (fun symbols (name,tag,_)->snd (C.internConstructor owner name tag symbols)) symbols variants) sums symbols in
 let symbols=M.fold (fun owner (_,fields) symbols->List.mapi (fun index (name,_)->index,name) fields |> List.fold_left (fun symbols (index,name)->snd (C.internField owner name index symbols)) symbols) records symbols in
 let indexedTypeReg=M.map (fun (typeParams,fields)->({T.fields;fieldTypes=M.of_list fields;typeParams}:T.recordTypeInfo)) records in
 let indexedSumTypeReg=M.map (fun (typeParams,variants)->({T.typeParams;variants=List.map (fun (name,tag,fields)->({T.name;tag;fields}:T.sumVariantInfo)) variants}:T.sumTypeInfo)) sums in
 let env={emptyEnv with T.indexedTypeReg;indexedSumTypeReg;aliasReg=M.of_list ["Alias",([],AST.TRecord ("R",[]));"Chain",([],AST.TRecord ("Alias",[]));"GenericAlias",(["x"],AST.TSum ("GSum",[AST.TVar "x"]));"Uuid",([],AST.TString);"DateTime",([],AST.TString)]} in
 let types=[AST.TUnit;AST.TBool;AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TFloat64;AST.TString;AST.TChar;AST.TDateTime;AST.TSum ("Uuid",[]);AST.TRecord ("Uuid",[]);AST.TRecord ("DateTime",[]);AST.TList AST.TString;AST.TList (AST.TList AST.TInt64);AST.TTuple [AST.TInt64;AST.TString;AST.TBool];AST.TTuple [AST.TTuple [AST.TInt64;AST.TString];AST.TBool];AST.TTuple [AST.TInt64;AST.TTuple [AST.TString;AST.TBool]];AST.TDict (AST.TString,AST.TList AST.TInt64);AST.TRecord ("R",[]);AST.TRecord ("Box",[AST.TString]);AST.TRecord ("Node",[]);AST.TSum ("Choice",[]);AST.TSum ("GSum",[AST.TString]);AST.TSum ("Tree",[]);AST.TRecord ("Alias",[]);AST.TRecord ("Chain",[]);AST.TSum ("GenericAlias",[AST.TInt64]);AST.TRecord ("Box",[]);AST.TSum ("GSum",[]);AST.TRecord (source,[]);AST.TBlob;AST.TInternalRawPtr;AST.TNever;AST.TStream AST.TInt64;AST.TVar source;AST.TFunction ([AST.TInt64],AST.TString);AST.TDict (AST.TInt64,AST.TString);AST.TTuple [];AST.TTuple [AST.TString]] in
 let program parse typ=
  let inputId,symbols=C.allocateBinding "input" symbols in
  let inputType=if parse then AST.TString else typ in
  let returnType=if parse then AST.TSum ("Darklang.Stdlib.Result.Result",[typ;AST.TSum ("Darklang.Stdlib.Json.ParseError.ParseError",[])]) else AST.TString in
  let call=C.TypeApp ((if parse then parseId else serializeId),[C.checkedType typ],NonEmptyList.singleton (C.Local inputId)) in
  let fn={C.id=userId;name="user";typeParams=[];params=C.checkedParams (NonEmptyList.singleton (inputId,inputType));returnType=C.checkedType returnType;body=call;recursion=None} in
  C.programFromCheckedParts (symbols,[C.FunctionDef fn]) in
 let nested=
  let inputId,nestedSymbols=C.allocateBinding "input" symbols in
  let bindingId,nestedSymbols=C.allocateBinding "local" nestedSymbols in
  let literal=C.Local inputId in
  let serialize typ=C.TypeApp (serializeId,[C.checkedType typ],NonEmptyList.singleton literal) in
  let parse typ=C.TypeApp (parseId,[C.checkedType typ],NonEmptyList.singleton (C.StringLiteral source)) in
  let value=serialize AST.TString in
  let other=serialize (AST.TRecord ("R",[])) in
  let parsed=parse (AST.TList AST.TString) in
  let tuple values=C.TupleLiteral (C.tupleElementsOfList values) in
  let owner=Option.get (C.tryFindTypeId "R" nestedSymbols) in
  let field name=Option.get (C.tryFindFieldId "R" name nestedSymbols) in
  let complete=match C.completeRecordFields owner 3 [field "z",value;field "a",other;field "field",parsed] with Ok value->value|Error message->failwith message in
  let constructorId=Option.get (C.tryFindConstructorId "Choice" "Many" nestedSymbols) in
  let case={C.patterns=NonEmptyList.singleton C.PWildcard;guard=Some value;body=other} in
  let wrappers=[tuple [value;other;parsed;value];C.InterpolatedString [C.StringText source;C.StringExpr value;C.StringExpr other];C.BinOp (AST.Add,value,other);C.UnaryOp (AST.Not,value);C.Let (C.LPVariable bindingId,value,other);C.If (value,other,parsed);C.Sequence (value,other);C.Call (userId,NonEmptyList.fromList [value;other]);C.TypeApp (userId,[C.checkedType AST.TString],NonEmptyList.fromList [value;other]);C.TupleAccess (tuple [value;other],0);C.DictLiteral (C.checkedType AST.TString,C.checkedType AST.TString,[value,other]);C.RecordLiteral ({C.typeId=owner;typeArgs=[]},complete);C.RecordUpdate (literal,[field "a",value;field "z",other]);C.RecordAccess (value,field "a");C.Constructor ({C.typeId=AST.constructorIdOwner constructorId;constructorId;typeArgs=[]},[value;other]);C.Match (value,NonEmptyList.singleton case);C.ListLiteral [value;other];C.Lambda (NonEmptyList.singleton {C.pattern=C.LPVariable bindingId;typ=C.checkedType AST.TString},None,value);C.Apply (value,NonEmptyList.singleton other);C.IndirectApply (value,NonEmptyList.singleton other);C.Closure (userId,[value;other]);C.BoundaryRender (userId,value)] in
  list (fun body->attempt C.observationProgram (fun ()->let fn={C.id=userId;name="user";typeParams=[];params=C.checkedParams (NonEmptyList.singleton (inputId,AST.TString));returnType=C.checkedType AST.TString;body;recursion=None} in J.rewriteProgram env (C.programFromCheckedParts (nestedSymbols,[C.FunctionDef fn;C.Expression parsed])))) wrappers in
 let observations=list (fun parse->list (fun typ->attempt C.observationProgram (fun ()->J.rewriteProgram env (program parse typ))) types) [false;true] in
 let session=new J.planningSession in
 let snapshot ()=tuple [SemanticJson.int32 session#count;SemanticJson.int32 session#hitCount;SemanticJson.int32 session#missCount] in
 let cached pass parse typ=let observed=attempt C.observationProgram (fun ()->J.rewriteProgramWithSession (Some session) pass (program parse typ)) in let counts=snapshot () in tuple [observed;counts] in
 let cachedAll=list (fun parse->list (fun typ->cached env parse typ) types) [false;true] in
 let repeated=list (fun parse->list (fun typ->cached env parse typ) [AST.TString;AST.TRecord ("R",[]);AST.TSum ("Tree",[]);AST.TList AST.TInt64]) [false;true] in
 let changed={env with T.indexedTypeReg=M.add "R" {T.typeParams=[];fields=["z",AST.TList AST.TBool;"a",AST.TString;"field",AST.TBool];fieldTypes=M.of_list ["z",AST.TList AST.TBool;"a",AST.TString;"field",AST.TBool]} env.T.indexedTypeReg} in
 let changedCases=list (fun parse->cached changed parse (AST.TRecord ("R",[]))) [false;true] in
 session#dispose;
 let disposed=list (fun parse->cached env parse AST.TString) [false;true] in
 let unchanged=attempt C.observationProgram (fun ()->J.rewriteProgram env (C.programFromCheckedParts (symbols,[C.Expression (C.StringLiteral source)]))) in
 tuple [observations;cachedAll;repeated;changedCases;disposed;unchanged;nested]
