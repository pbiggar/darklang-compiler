(* JsonPlanning.ml - monomorphic, type-directed JSON conversion plans.

   Json.serialize and Json.parse are public generic intrinsics. This pass runs
   after type checking, when every explicit type argument is concrete, and
   replaces those calls with ordinary Dark functions. Backends therefore see
   only statically shaped values and use the normal retain/release machinery. *)
[@@@warning "-4"]
open CheckedAST
module M=StringOrder.Map
module S=StringOrder.Set
module N=NonEmptyList
let ( let* ) = Result.bind
let mapFold action initial values =
 let reversed,state=List.fold_left (fun (reversed,state) value->let mapped,next=action state value in mapped::reversed,next) ([],initial) values in
 List.rev reversed,state
let matchExpr (value,cases)=Match (value,N.fromList cases)
(* Bounded, caller-owned cache of generated typed JSON codec declarations.
   Entries are keyed by direction plus the complete reachable shape of the
   resolved root type, so unrelated declarations do not prevent reuse while
   same-named local declarations with different shapes remain isolated. *)
class planningSession = object
 val artifacts : (string,CheckedAST.functionDef list) Hashtbl.t = Hashtbl.create 127
 val mutable disposed=false
 val mutable hits=0
 val mutable misses=0
 method tryFind key =
  if disposed then None else match Hashtbl.find_opt artifacts key with
   | Some functions->hits<-hits+1;Some functions
   | None->misses<-misses+1;None
 method store key functions = if not disposed then Hashtbl.replace artifacts key functions
 method count = if disposed then 0 else Hashtbl.length artifacts
 method hitCount = hits
 method missCount = misses
 method dispose = Hashtbl.clear artifacts;disposed<-true
end
[@@@warning "-30"]
type sumVariant=Types.sumVariantInfo
type sumInfo=Types.sumTypeInfo
type env={records:Types.indexedTypeRegistry;sums:sumInfo M.t;aliases:Types.aliasRegistry;symbols:CheckedAST.symbols}
type state={functions:CheckedAST.functionDef M.t;symbols:CheckedAST.symbols}
let freshBinding name (state:state)=let id,symbols=allocateBinding name state.symbols in id,{state with symbols}
let reserveFunction name (state:state)=let id,symbols=internFunction name state.symbols in id,{state with symbols}
let freshBindings names (state:state)=let bindings,next=mapFold (fun current name->let id,next=freshBinding name current in (name,id),next) state names in M.of_list bindings,next
let local name bindings=match M.find_opt name bindings with Some id->Local id|None->Crash.crash ("Generated JSON binding was not allocated: "^name)
let patternLocal name bindings=match M.find_opt name bindings with Some id->PVariable id|None->Crash.crash ("Generated JSON pattern binding was not allocated: "^name)
let args values=N.fromList values
let resolveFunction (env:env) name=match tryFindFunctionId name env.symbols with Some id->id|None->Crash.crash ("Generated JSON function was not interned: "^name)
let call (env:env) name values=Call (resolveFunction env name,args values)
let listPush (env:env) elementType list value=TypeApp (resolveFunction env "Darklang.Stdlib.List.push",[checkedType elementType],args [list;value])
let stableHash value=String.fold_left (fun hash byte->Int64.mul (Int64.logxor hash (Int64.of_int (Char.code byte))) 1099511628211L) 0xcbf29ce484222325L value
(* Generated plan names must distinguish structurally different types whose
   public spelling is intentionally flattened (notably nested tuples). Encode
   the union directly to avoid generic formatting overhead when primitive codecs are requested
   by many separate compilations. *)
let textLength value=String.length value
let rec structuralTypeKey typ=
 let encodeText tag value=tag^string_of_int (textLength value)^":"^value in
 let encodeTypes tag types=let encoded=List.map structuralTypeKey types |> List.map (fun value->string_of_int (textLength value)^":"^value) |> String.concat "" in tag^string_of_int (List.length types)^":"^encoded in
 match typ with
 | AST.TInt8->"i8"|AST.TInt16->"i16"|AST.TInt32->"i32"|AST.TInt64->"i64"|AST.TInt128->"i128"|AST.TInt->"int"
 | AST.TUInt8->"u8"|AST.TUInt16->"u16"|AST.TUInt32->"u32"|AST.TUInt64->"u64"|AST.TUInt128->"u128"
 | AST.TBool->"bool"|AST.TFloat64->"float64"|AST.TString->"string"|AST.TBlob->"blob"|AST.TChar->"char"|AST.TDateTime->"datetime"
 | AST.TUnit->"unit"|AST.TNever->"runtime-error"|AST.TInternalRawPtr->"raw-ptr"
 | AST.TVar name|AST.TInferenceVar (name,_)->encodeText "var" name
 | AST.TList typ->encodeTypes "list" [typ]|AST.TStream typ->encodeTypes "stream" [typ]
 | AST.TDict (key,value)->encodeTypes "dict" [key;value]|AST.TTuple types->encodeTypes "tuple" types
 | AST.TRecord (name,types)->let name=encodeText "record" name in let types=encodeTypes "args" types in name^types
 | AST.TSum (name,types)->let name=encodeText "sum" name in let types=encodeTypes "args" types in name^types
 | AST.TFunction (parameters,result)->let parameters=encodeTypes "function" parameters in let result=encodeTypes "returns" [result] in parameters^result
let namedPlan prefix typ=Printf.sprintf "%s%016Lx" prefix (stableHash (structuralTypeKey typ))
let serializeName typ=namedPlan "__dark_json_serialize_" typ
let listName typ=namedPlan "__dark_json_serialize_list_" typ
let dictName typ=namedPlan "__dark_json_serialize_dict_" typ
let decoderName typ=namedPlan "__dark_json_decode_" typ
let decodeListName typ=namedPlan "__dark_json_decode_list_" typ
let decodeDictName typ=namedPlan "__dark_json_decode_dict_" typ
let makeCase pattern body={patterns=N.singleton pattern;guard=None;body}
let typeId (env:env) name=match tryFindTypeId name env.symbols with Some id->id|None->Crash.crash ("Generated JSON type was not interned: "^name)
let constructor (env:env) owner caseName payload=match tryFindConstructorId owner caseName env.symbols with
 | Some id->Constructor ({typeId=typeId env owner;constructorId=id;typeArgs=[]},Option.to_list payload)
 | None->Crash.crash ("JSON constructor was not interned: "^owner^"."^caseName)
let constructorPattern (env:env) owner caseName fields=match tryFindConstructorId owner caseName env.symbols with Some id->PConstructor (id,fields)|None->Crash.crash ("JSON constructor pattern was not interned: "^owner^"."^caseName)
let fieldId (env:env) owner fieldName=match tryFindFieldId owner fieldName env.symbols with Some id->id|None->Crash.crash ("Generated JSON record field was not interned: "^owner^"."^fieldName)
let recordLiteral (env:env) typeName typeArgs fields=
 let owner=typeId env typeName in
 let fieldCount=match M.find_opt typeName env.records with Some info->List.length info.Types.fields|None->Crash.crash ("Generated JSON record type '"^typeName^"' is absent") in
 match completeRecordFields owner fieldCount fields with
 | Ok complete->RecordLiteral ({typeId=owner;typeArgs=checkedTypeArgs typeArgs},complete)
 | Error detail->Crash.crash ("Generated JSON record '"^typeName^"': "^detail)
let tuplePayload values=Some (TupleLiteral (tupleElementsOfList values))
let ok (env:env) value=constructor env "Darklang.Stdlib.Result.Result" "Ok" (Some value)
let error (env:env) value=constructor env "Darklang.Stdlib.Result.Result" "Error" (Some value)
let _none (env:env)=constructor env "Darklang.Stdlib.Option.Option" "None" None
let _some (env:env) value=constructor env "Darklang.Stdlib.Option.Option" "Some" (Some value)
let jsonErrorType=AST.TSum ("Darklang.Stdlib.Json.ParseError.ParseError",[])
let valueViewType=AST.TInt64
let pathPartType=AST.TSum ("Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part",[])
let pathType=AST.TList pathPartType
let resultType okType=AST.TSum ("Darklang.Stdlib.Result.Result",[okType;jsonErrorType])
let writerType=AST.TString
let writerEmpty (env:env)=call env "Darklang.Stdlib.Json.__writerEmpty" [UnitLiteral]
let writerFinish (env:env) writer=call env "Darklang.Stdlib.Json.__writerFinish" [writer]
let writerRaw (env:env) writer value=call env "Darklang.Stdlib.Json.__writerWriteRaw" [writer;value]
let writerString (env:env) writer value=call env "Darklang.Stdlib.Json.__writerWriteString" [writer;value]
let writerBeginArray (env:env) writer=call env "Darklang.Stdlib.Json.__writerBeginArray" [writer]
let writerEndArray (env:env) writer=call env "Darklang.Stdlib.Json.__writerEndArray" [writer]
let writerBeginObject (env:env) writer=call env "Darklang.Stdlib.Json.__writerBeginObject" [writer]
let writerEndObject (env:env) writer=call env "Darklang.Stdlib.Json.__writerEndObject" [writer]
let writerSeparator (env:env) writer=call env "Darklang.Stdlib.Json.__writerSeparator" [writer]
let writerFieldName (env:env) writer name=call env "Darklang.Stdlib.Json.__writerFieldName" [writer;name]
let rec typeReference (env:env) typ=
 let owner="Darklang.LanguageTools.RuntimeTypes.TypeReference" in
 let nullary caseName=constructor env owner caseName None in
 let unary caseName value=constructor env owner caseName (Some value) in
 let custom name typeArgs=
  let originalName=ListLiteral (List.map (fun s->StringLiteral s) (String.split_on_char '.' name)) in
  let hash=constructor env "Darklang.LanguageTools.RuntimeTypes.Hash" "Hash" (Some (StringLiteral "")) in
  let fqNameType=AST.TSum ("Darklang.LanguageTools.RuntimeTypes.FQTypeName.FQTypeName",[]) in
  let fqName=constructor env "Darklang.LanguageTools.RuntimeTypes.FQTypeName.FQTypeName" "Package" (Some hash) in
  let resolved=ok env fqName in
  let resolution=recordLiteral env "Darklang.LanguageTools.RuntimeTypes.NameResolution" [fqNameType] [fieldId env "Darklang.LanguageTools.RuntimeTypes.NameResolution" "originalName",originalName;fieldId env "Darklang.LanguageTools.RuntimeTypes.NameResolution" "resolved",resolved] in
  constructor env owner "TCustomType" (tuplePayload [resolution;ListLiteral (List.map (typeReference env) typeArgs)]) in
 match typ with
 | AST.TUnit->nullary "TUnit"|AST.TBool->nullary "TBool"|AST.TInt8->nullary "TInt8"|AST.TUInt8->nullary "TUInt8"
 | AST.TInt16->nullary "TInt16"|AST.TUInt16->nullary "TUInt16"|AST.TInt32->nullary "TInt32"|AST.TUInt32->nullary "TUInt32"
 | AST.TInt64->nullary "TInt64"|AST.TUInt64->nullary "TUInt64"|AST.TInt128->nullary "TInt128"|AST.TUInt128->nullary "TUInt128"
 | AST.TInt->nullary "TInt"|AST.TFloat64->nullary "TFloat"|AST.TChar->nullary "TChar"|AST.TString->nullary "TString"|AST.TBlob->nullary "TBlob"
 | AST.TSum ("Uuid",[])->nullary "TUuid"|AST.TDateTime->nullary "TDateTime"
 | AST.TList typ->unary "TList" (typeReference env typ)
 | AST.TDict (AST.TString,typ)->unary "TDict" (typeReference env typ)
 | AST.TTuple (first::second::rest)->constructor env owner "TTuple" (tuplePayload [typeReference env first;typeReference env second;ListLiteral (List.map (typeReference env) rest)])
 | AST.TFunction (parameters,result)->constructor env owner "TFn" (tuplePayload [ListLiteral (List.map (typeReference env) parameters);typeReference env result])
 | AST.TStream typ->custom "Darklang.Stdlib.Stream.Stream" [typ]
 | AST.TRecord (name,args)|AST.TSum (name,args)->custom name args
 | AST.TVar name|AST.TInferenceVar (name,_)->unary "TVariable" (StringLiteral name)
 | AST.TTuple []|AST.TTuple [_]|AST.TInternalRawPtr|AST.TNever|AST.TDict _->unary "TVariable" (StringLiteral (CheckingDiagnostics.typeToString typ))
let cantMatch (env:env) typ raw path=error env (constructor env "Darklang.Stdlib.Json.ParseError.ParseError" "CantMatchWithType" (tuplePayload [typeReference env typ;raw;call env "Darklang.Stdlib.Json.ParseError.__copyPath" [path]]))
let rawSource (env:env) source raw=call env "Darklang.Stdlib.Json.__copyRaw" [source;raw]
let resultCases (env:env) okId okBody errorId=[makeCase (constructorPattern env "Darklang.Stdlib.Result.Result" "Ok" [PVariable okId]) okBody;makeCase (constructorPattern env "Darklang.Stdlib.Result.Result" "Error" [PVariable errorId]) (error env (Local errorId))]
let applySubstitution subst typ=
 let rec apply typ=match typ with
 | AST.TVar name|AST.TInferenceVar (_,name)->Option.value ~default:typ (M.find_opt name subst)
 | AST.TList typ->AST.TList (apply typ)|AST.TDict (key,value)->AST.TDict (apply key,apply value)
 | AST.TTuple types->AST.TTuple (List.map apply types)|AST.TFunction (parameters,result)->AST.TFunction (List.map apply parameters,apply result)
 | AST.TRecord (name,args)->AST.TRecord (name,List.map apply args)|AST.TSum (name,args)->AST.TSum (name,List.map apply args)
 | other->other in apply typ
let substitution typeParams typeArgs=if List.length typeParams=List.length typeArgs then Ok (M.of_list (List.combine typeParams typeArgs)) else Error (Printf.sprintf "JSON type argument mismatch: expected %d, got %d" (List.length typeParams) (List.length typeArgs))
let resolveJsonType (env:env) typ=
 (* Preserve semantic aliases recursively before ordinary alias expansion. *)
 let rec resolve typ=
  let resolveNamed makeType name typeArgs=
   let resolvedArgs=List.map resolve typeArgs in
   match name,resolvedArgs,M.find_opt name env.aliases with
   | ("Uuid"|"DateTime"),[],_->makeType name []
   | _,_,Some (params,target) when List.length params=List.length resolvedArgs->resolve (applySubstitution (M.of_list (List.combine params resolvedArgs)) target)
   | _,_,None when M.mem name env.records->AST.TRecord (name,resolvedArgs)
   | _,_,None when M.mem name env.sums->AST.TSum (name,resolvedArgs)
   | _->makeType name resolvedArgs in
  match typ with
  | AST.TRecord (name,args)->resolveNamed (fun name args->AST.TRecord (name,args)) name args
  | AST.TSum (name,args)->resolveNamed (fun name args->AST.TSum (name,args)) name args
  | AST.TTuple types->AST.TTuple (List.map resolve types)|AST.TList typ->AST.TList (resolve typ)
  | AST.TDict (key,value)->AST.TDict (resolve key,resolve value)|AST.TFunction (parameters,result)->AST.TFunction (List.map resolve parameters,resolve result)
  | other->other in resolve typ
let canonicalCodecTypeKey (env:env) rootType=
 (* Include only declarations reachable from the requested type. This is
    deliberately narrower than fingerprinting the complete type-checking
    environment: most E2E files add unrelated declarations, and those must
    not defeat reuse of primitive and standard-library codecs. *)
 let rec encode visiting typ=
  let typ=resolveJsonType env typ in
  match typ with
  | AST.TList typ->"list("^encode visiting typ^")"
  | AST.TDict (key,value)->"dict("^encode visiting key^","^encode visiting value^")"
  | AST.TTuple types->"tuple("^String.concat "," (List.map (encode visiting) types)^")"
  | AST.TFunction (parameters,result)->let parameters=String.concat "," (List.map (encode visiting) parameters) in "fn("^parameters^")->"^encode visiting result
  | AST.TRecord (name,typeArgs)->
   let identity="record:"^structuralTypeKey typ in
   if S.mem identity visiting then "ref("^identity^")" else
   let visiting=S.add identity visiting in
   let args=String.concat "," (List.map (encode visiting) typeArgs) in
   (match M.find_opt name env.records with
   | None->identity^"<"^args^">"
   | Some info->match substitution info.Types.typeParams typeArgs with
    | Error _->identity^"<"^args^">:invalid-arity"
    | Ok subst->let fields=List.map (fun (name,typ)->let concrete=applySubstitution subst typ in name^":"^encode visiting concrete) info.Types.fields |> String.concat "," in identity^"<"^args^">{"^fields^"}")
  | AST.TSum (name,typeArgs)->
   let identity="sum:"^structuralTypeKey typ in
   if S.mem identity visiting then "ref("^identity^")" else
   let visiting=S.add identity visiting in
   let args=String.concat "," (List.map (encode visiting) typeArgs) in
   (match M.find_opt name env.sums with
   | None->identity^"<"^args^">"
   | Some info->match substitution info.Types.typeParams typeArgs with
    | Error _->identity^"<"^args^">:invalid-arity"
    | Ok subst->let variants=List.stable_sort (fun (a:sumVariant) b->compare a.Types.tag b.Types.tag) info.Types.variants |> List.map (fun (variant:sumVariant)->let fields=List.map (fun typ->encode visiting (applySubstitution subst typ)) variant.Types.fields |> String.concat "," in Printf.sprintf "%d:%s:[%s]" variant.Types.tag variant.Types.name fields) |> String.concat "," in identity^"<"^args^">["^variants^"]")
  | other->structuralTypeKey other in encode S.empty rootType
let rec ensureSerializer (env:env) typ (state:state) =
 let typ=resolveJsonType env typ in
 let name=serializeName typ in
 match M.find_opt name state.functions with
 | Some _->Ok (name,state)
 | None->
  let functionId,state=reserveFunction name state in
  let bindings,state=freshBindings ["__writer";"__value"] state in
  let env={env with symbols=state.symbols} in
  let writerId=match M.find_opt "__writer" bindings with Some id->id|None->Crash.crash "JSON serializer writer binding was not allocated" in
  let valueId=match M.find_opt "__value" bindings with Some id->id|None->Crash.crash "JSON serializer value binding was not allocated" in
  let placeholder={id=functionId;name;typeParams=[];params=checkedParams (args [writerId,writerType;valueId,typ]);returnType=checkedType writerType;body=Local writerId;recursion=None} in
  let reserved={state with functions=M.add name placeholder state.functions} in
  let* body,nextState=serializeBody env typ (Local valueId) (Local writerId) reserved in
  let completed={placeholder with body} in
  Ok (name,{nextState with functions=M.add name completed nextState.functions})
and serializeCall (env:env) typ writer value (state:state)=
 let* name,nextState=ensureSerializer env typ state in
 let currentEnv={env with symbols=nextState.symbols} in
 Ok (call currentEnv name [writer;value],nextState)
and serializeItems (env:env) items writer (state:state)=
 let rec loop remaining currentWriter currentState=match remaining with
 | []->Ok (currentWriter,currentState)
 | (typ,value)::rest->let* nextWriter,nextState=serializeCall env typ currentWriter value currentState in loop rest nextWriter nextState in
 loop items writer state
and ensureListSerializer (env:env) elemType (state:state)=
 let elemType=resolveJsonType env elemType in
 let typ=AST.TList elemType in
 let name=listName typ in
 match M.find_opt name state.functions with
 | Some _->Ok (name,state)
 | None->
  let functionId,state=reserveFunction name state in
  let bindings,state=freshBindings ["__items";"__writer";"__first";"__head";"__tail"] state in
  let env={env with symbols=state.symbols} in
  let binding name=match M.find_opt name bindings with Some id->id|None->Crash.crash ("JSON list serializer binding was not allocated: "^name) in
  let placeholder={id=functionId;name;typeParams=[];params=checkedParams (args [binding "__items",typ;binding "__writer",writerType;binding "__first",AST.TBool]);returnType=checkedType writerType;body=local "__writer" bindings;recursion=None} in
  let reserved={state with functions=M.add name placeholder state.functions} in
  let separated=If (local "__first" bindings,local "__writer" bindings,writerSeparator env (local "__writer" bindings)) in
  let* encoded,nextState=serializeCall env elemType separated (local "__head" bindings) reserved in
  let body=matchExpr (local "__items" bindings,[makeCase (PList []) (local "__writer" bindings);makeCase (PListCons ([patternLocal "__head" bindings],patternLocal "__tail" bindings)) (call env name [local "__tail" bindings;encoded;BoolLiteral false])]) in
  let completed={placeholder with body} in Ok (name,{nextState with functions=M.add name completed nextState.functions})
and ensureDictSerializer (env:env) valueType (state:state)=
 let valueType=resolveJsonType env valueType in
 let dictType=AST.TDict (AST.TString,valueType) in
 let entryType=AST.TTuple [AST.TString;valueType] in
 let listType=AST.TList entryType in
 let name=dictName dictType in
 match M.find_opt name state.functions with
 | Some _->Ok (name,state)
 | None->
  let functionId,state=reserveFunction name state in
  let bindings,state=freshBindings ["__entries";"__writer";"__first";"__entry";"__tail"] state in
  let env={env with symbols=state.symbols} in
  let binding name=match M.find_opt name bindings with Some id->id|None->Crash.crash ("JSON dictionary serializer binding was not allocated: "^name) in
  let placeholder={id=functionId;name;typeParams=[];params=checkedParams (args [binding "__entries",listType;binding "__writer",writerType;binding "__first",AST.TBool]);returnType=checkedType writerType;body=local "__writer" bindings;recursion=None} in
  let reserved={state with functions=M.add name placeholder state.functions} in
  let separated=If (local "__first" bindings,local "__writer" bindings,writerSeparator env (local "__writer" bindings)) in
  let withName=writerFieldName env separated (TupleAccess (local "__entry" bindings,0)) in
  let* encoded,nextState=serializeCall env valueType withName (TupleAccess (local "__entry" bindings,1)) reserved in
  let body=matchExpr (local "__entries" bindings,[makeCase (PList []) (local "__writer" bindings);makeCase (PListCons ([patternLocal "__entry" bindings],patternLocal "__tail" bindings)) (call env name [local "__tail" bindings;encoded;BoolLiteral false])]) in
  let completed={placeholder with body} in Ok (name,{nextState with functions=M.add name completed nextState.functions})
and serializeBody (env:env) typ value writer (state:state)=
 let raw functionName=Ok (writerRaw env writer (call env functionName [value]),state) in
 match typ with
 | AST.TUnit->Ok (writerRaw env writer (StringLiteral "null"),state)
 | AST.TBool->Ok (writerRaw env writer (If (value,StringLiteral "true",StringLiteral "false")),state)
 | AST.TInt8->raw "Darklang.Stdlib.Int8.toString"|AST.TInt16->raw "Darklang.Stdlib.Int16.toString"|AST.TInt32->raw "Darklang.Stdlib.Int32.toString"|AST.TInt64->raw "Darklang.Stdlib.Int64.toString"|AST.TInt->raw "Darklang.Stdlib.Int.toString"
 | AST.TUInt8->raw "Darklang.Stdlib.UInt8.toString"|AST.TUInt16->raw "Darklang.Stdlib.UInt16.toString"|AST.TUInt32->raw "Darklang.Stdlib.UInt32.toString"|AST.TUInt64->raw "Darklang.Stdlib.UInt64.toString"|AST.TInt128->raw "Darklang.Stdlib.Int128.toString"|AST.TUInt128->raw "Darklang.Stdlib.UInt128.toString"
 | AST.TFloat64->raw "Darklang.Stdlib.Json.__serializeFloat"
 | AST.TString|AST.TChar->Ok (writerString env writer value,state)
 | AST.TSum ("Uuid",[])->Ok (writerString env writer (call env "Darklang.Stdlib.Uuid.toString" [value]),state)
 | AST.TDateTime->Ok (writerString env writer (call env "Darklang.Stdlib.DateTime.toString" [value]),state)
 | AST.TTuple elementTypes->
  let* encoded,nextState=List.mapi (fun index typ->index,(typ,TupleAccess (value,index))) elementTypes
   |> List.fold_left (fun result (index,item)->let* currentWriter,currentState=result in let separated=if index=0 then currentWriter else writerSeparator env currentWriter in serializeItems env [item] separated currentState) (Ok (writerBeginArray env writer,state)) in
  Ok (writerEndArray env encoded,nextState)
 | AST.TList elemType->
  let* name,nextState=ensureListSerializer env elemType state in
  let currentEnv={env with symbols=nextState.symbols} in
  let encoded=call currentEnv name [value;writerBeginArray currentEnv writer;BoolLiteral true] in
  Ok (writerEndArray currentEnv encoded,nextState)
 | AST.TDict (AST.TString,valueType)->
  let* name,nextState=ensureDictSerializer env valueType state in
  let entriesId,nextState=freshBinding "__entries" nextState in
  let currentEnv={env with symbols=nextState.symbols} in
  let entries=TypeApp (resolveFunction currentEnv "Darklang.Stdlib.Dict.toList",checkedTypeArgs [AST.TString;valueType],N.singleton value) in
  let encoded=call currentEnv name [Local entriesId;writerBeginObject currentEnv writer;BoolLiteral true] in
  Ok (Let (LPVariable entriesId,entries,writerEndObject currentEnv encoded),nextState)
 | AST.TRecord (typeName,typeArgs)->(match M.find_opt typeName env.records with
  | None->Error ("Unsupported type in JSON: "^CheckingDiagnostics.typeToString typ)
  | Some recordInfo->
   let* subst=substitution recordInfo.Types.typeParams typeArgs in
   let rec loop remaining index currentWriter (currentState:state)=match remaining with
   | []->Ok (currentWriter,currentState)
   | (fieldIndex,fieldName,fieldType)::rest->
    let concrete=resolveJsonType env (applySubstitution subst fieldType) in
    let separated=if index=0 then currentWriter else writerSeparator env currentWriter in
    let named=writerFieldName env separated (StringLiteral fieldName) in
    let fieldId,symbols=internField typeName fieldName fieldIndex currentState.symbols in
    let* encoded,nextState=serializeCall env concrete named (RecordAccess (value,fieldId)) {currentState with symbols} in
    loop rest (index+1) encoded nextState in
   let fields=List.mapi (fun index (name,typ)->index,name,typ) recordInfo.Types.fields |> List.stable_sort (fun (_,a,_) (_,b,_)->StringOrder.compare a b) in
   let* encoded,nextState=loop fields 0 (writerBeginObject env writer) state in
   Ok (writerEndObject env encoded,nextState))
 | AST.TSum (typeName,typeArgs)->(match M.find_opt typeName env.sums with
  | None->Error ("Unsupported type in JSON: "^CheckingDiagnostics.typeToString typ)
  | Some sumInfo->
   let* subst=substitution sumInfo.Types.typeParams typeArgs in
   let rec loop remaining current acc=match remaining with
   | []->Ok (List.rev acc,current)
   | (variant:sumVariant)::rest->(match variant.Types.fields with
    | []->
     let body=writerBeginObject env writer |> fun writer->writerFieldName env writer (StringLiteral variant.Types.name) |> writerBeginArray env |> writerEndArray env |> writerEndObject env in
     loop rest current (makeCase (constructorPattern env typeName variant.Types.name []) body::acc)
    | fieldTypes->
     let concreteFields=List.map (fun typ->resolveJsonType env (applySubstitution subst typ)) fieldTypes in
     let fieldNames=List.mapi (fun index _->Printf.sprintf "__field_%d_%d" variant.Types.tag index) fieldTypes in
     let fieldIds,current=mapFold (fun state name->freshBinding name state) current fieldNames in
     let fields=List.combine concreteFields (List.map (fun id->Local id) fieldIds) in
     let initialWriter=writerBeginObject env writer |> fun writer->writerFieldName env writer (StringLiteral variant.Types.name) |> writerBeginArray env in
     let* encoded,next=List.mapi (fun index item->index,item) fields |> List.fold_left (fun result (index,item)->let* currentWriter,currentState=result in let separated=if index=0 then currentWriter else writerSeparator env currentWriter in serializeItems env [item] separated currentState) (Ok (initialWriter,current)) in
     let body=writerEndObject env (writerEndArray env encoded) in
     loop rest next (makeCase (constructorPattern env typeName variant.Types.name (List.map (fun id->PVariable id) fieldIds)) body::acc)) in
   let* cases,nextState=loop (List.stable_sort (fun (a:sumVariant) b->compare a.Types.tag b.Types.tag) sumInfo.Types.variants) state [] in Ok (matchExpr (value,cases),nextState))
 | AST.TFunction _|AST.TBlob|AST.TInternalRawPtr|AST.TNever|AST.TStream _|AST.TVar _|AST.TInferenceVar _|AST.TDict _->Error ("Unsupported type in JSON: "^CheckingDiagnostics.typeToString typ^". Some types are not supported in Json serialization")
let optionDecoder (env:env) typ functionName source view path (state:state)=
 let valueId,state=freshBinding "__value" state in
 let failure=cantMatch env typ (rawSource env source view) path in
 matchExpr (call env functionName [source;view],[makeCase (constructorPattern env "Darklang.Stdlib.Option.Option" "Some" [PVariable valueId]) (ok env (Local valueId));makeCase (constructorPattern env "Darklang.Stdlib.Option.Option" "None" []) failure]),state
let rec ensureDecoder (env:env) typ (state:state)=
 let typ=resolveJsonType env typ in
 let name=decoderName typ in
 match M.find_opt name state.functions with
 | Some _->Ok (name,state)
 | None->
  let functionId,state=reserveFunction name state in
  let bindings,state=freshBindings ["__source";"__view";"__path"] state in
  let env={env with symbols=state.symbols} in
  let binding name=match M.find_opt name bindings with Some id->id|None->Crash.crash ("JSON decoder binding was not allocated: "^name) in
  let placeholder={id=functionId;name;typeParams=[];params=checkedParams (args [binding "__source",AST.TString;binding "__view",valueViewType;binding "__path",pathType]);returnType=checkedType (resultType typ);body=RuntimeError "unfinished JSON decoder";recursion=None} in
  let reserved={state with functions=M.add name placeholder state.functions} in
  let* body,nextState=decodeBody env typ (local "__source" bindings) (local "__view" bindings) (local "__path" bindings) reserved in
  let completed={placeholder with body} in Ok (name,{nextState with functions=M.add name completed nextState.functions})
and decodeCall (env:env) typ source view path (state:state)=
 let* name,nextState=ensureDecoder env typ state in
 let currentEnv={env with symbols=nextState.symbols} in Ok (call currentEnv name [source;view;path],nextState)
and sequenceDecoded (env:env) source items build (state:state)=
 let rec loop remaining current bindings=match remaining with
 | []->Ok (build (List.rev bindings),current)
 | (typ,view,path,bindingName)::rest->
  let* decoded,next=decodeCall env typ source view path current in
  let errorId,next=freshBinding "__decode_error" next in
  let* tail,finalState=loop rest next ((bindingName,typ)::bindings) in
  Ok (matchExpr (decoded,resultCases env bindingName tail errorId),finalState) in loop items state []
and ensureListDecoder (env:env) elemType (state:state)=
 let elemType=resolveJsonType env elemType in
 let listType=AST.TList elemType in
 let name=decodeListName listType in
 match M.find_opt name state.functions with
 | Some _->Ok (name,state)
 | None->
  let functionId,state=reserveFunction name state in
  let bindings,state=freshBindings ["__source";"__array_view";"__next_index";"__path";"__index";"__head";"__after_item";"__decoded_head";"__decoded_tail";"__tail_error";"__head_error"] state in
  let env={env with symbols=state.symbols} in
  let binding name=match M.find_opt name bindings with Some id->id|None->Crash.crash ("JSON list decoder binding was not allocated: "^name) in
  let placeholder={id=functionId;name;typeParams=[];params=checkedParams (args [binding "__source",AST.TString;binding "__array_view",valueViewType;binding "__next_index",AST.TInt64;binding "__path",pathType;binding "__index",AST.TInt64]);returnType=checkedType (resultType listType);body=RuntimeError "unfinished JSON list decoder";recursion=None} in
  let reserved={state with functions=M.add name placeholder state.functions} in
  let itemPath=listPush env pathPartType (local "__path" bindings) (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Index" (Some (call env "Darklang.Stdlib.Int.fromInt64" [local "__index" bindings]))) in
  let* decodedHead,nextState=decodeCall env elemType (local "__source" bindings) (local "__head" bindings) itemPath reserved in
  let decodedTail=call env name [local "__source" bindings;local "__array_view" bindings;local "__after_item" bindings;local "__path" bindings;BinOp (AST.Add,local "__index" bindings,Int64Literal 1L)] in
  let invalid=cantMatch env listType (rawSource env (local "__source" bindings) (local "__array_view" bindings)) (local "__path" bindings) in
  let decodedTailResult=matchExpr (decodedTail,resultCases env (binding "__decoded_tail") (ok env (listPush env elemType (local "__decoded_tail" bindings) (local "__decoded_head" bindings))) (binding "__tail_error")) in
  let decodedHeadResult=matchExpr (decodedHead,resultCases env (binding "__decoded_head") decodedTailResult (binding "__head_error")) in
  let body=Let (LPVariable (binding "__head"),call env "Darklang.Stdlib.Json.__arrayNext" [local "__source" bindings;local "__array_view" bindings;local "__next_index" bindings],
   If (BinOp (AST.Eq,local "__head" bindings,Int64Literal (-1L)),ok env (ListLiteral []),
    If (BinOp (AST.Eq,local "__head" bindings,Int64Literal (-2L)),invalid,
     Let (LPVariable (binding "__after_item"),call env "Darklang.Stdlib.Json.__arrayAfter" [local "__source" bindings;local "__array_view" bindings;local "__head" bindings],
      If (BinOp (AST.Lt,local "__after_item" bindings,Int64Literal 0L),invalid,decodedHeadResult))))) in
  let completed={placeholder with body} in Ok (name,{nextState with functions=M.add name completed nextState.functions})
and ensureDictDecoder (env:env) valueType (state:state)=
 let valueType=resolveJsonType env valueType in
 let dictType=AST.TDict (AST.TString,valueType) in
 let name=decodeDictName dictType in
 match M.find_opt name state.functions with
 | Some _->Ok (name,state)
 | None->
  let functionId,state=reserveFunction name state in
  let viewFieldsType=AST.TList (AST.TTuple [AST.TString;valueViewType]) in
  let bindings,state=freshBindings ["__source";"__fields";"__path";"__dict";"__entry";"__tail";"__decoded_value";"__decoded_dict";"__dict_tail_error";"__dict_error"] state in
  let env={env with symbols=state.symbols} in
  let binding name=match M.find_opt name bindings with Some id->id|None->Crash.crash ("JSON dictionary decoder binding was not allocated: "^name) in
  let placeholder={id=functionId;name;typeParams=[];params=checkedParams (args [binding "__source",AST.TString;binding "__fields",viewFieldsType;binding "__path",pathType;binding "__dict",dictType]);returnType=checkedType (resultType dictType);body=RuntimeError "unfinished JSON dictionary decoder";recursion=None} in
  let reserved={state with functions=M.add name placeholder state.functions} in
  let key=call env "Darklang.Stdlib.Json.__viewFieldName" [local "__entry" bindings] in
  let fieldView=call env "Darklang.Stdlib.Json.__viewFieldValue" [local "__entry" bindings] in
  let fieldPath=listPush env pathPartType (local "__path" bindings) (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Field" (Some key)) in
  let* decoded,nextState=decodeCall env valueType (local "__source" bindings) fieldView fieldPath reserved in
  let withValue=TypeApp (resolveFunction env "Darklang.Stdlib.Dict.setOverridingDuplicates",checkedTypeArgs [AST.TString;valueType],args [local "__dict" bindings;key;local "__decoded_value" bindings]) in
  let body=matchExpr (local "__fields" bindings,[makeCase (PList []) (ok env (local "__dict" bindings));makeCase (PListCons ([patternLocal "__entry" bindings],patternLocal "__tail" bindings))
   (matchExpr (decoded,resultCases env (binding "__decoded_value")
    (matchExpr (call env name [local "__source" bindings;local "__tail" bindings;local "__path" bindings;withValue],resultCases env (binding "__decoded_dict") (ok env (local "__decoded_dict" bindings)) (binding "__dict_tail_error"))) (binding "__dict_error")))]) in
  let completed={placeholder with body} in Ok (name,{nextState with functions=M.add name completed nextState.functions})
and decodeEnumCase (env:env) typ typeName subst source path caseRaw (variant:sumVariant) (state:state)=
 let casePath=listPush env pathPartType path (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Field" (Some (StringLiteral variant.Types.name))) in
 let fieldTypes=List.map (fun typ->resolveJsonType env (applySubstitution subst typ)) variant.Types.fields in
 let rawNames=List.mapi (fun index _->Printf.sprintf "__enum_raw_%d_%d" variant.Types.tag index) fieldTypes in
 let valueNames=List.mapi (fun index _->Printf.sprintf "__enum_value_%d_%d" variant.Types.tag index) fieldTypes in
 let extraName=Printf.sprintf "__enum_extra_%d" variant.Types.tag in
 let bindings,state=freshBindings (rawNames@valueNames@[extraName;"__enum_args"]) state in
 let decodedItems count=List.take count fieldTypes |> List.mapi (fun index fieldType->
  let argumentPath=listPush env pathPartType casePath (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Index" (Some (BigIntLiteral (Z.of_int index)))) in
  let valueId=match M.find_opt (List.nth valueNames index) bindings with Some id->id|None->Crash.crash "JSON enum value binding was not allocated" in
  fieldType,local (List.nth rawNames index) bindings,argumentPath,valueId) in
 let constructed=
  let values=List.map (fun name->local name bindings) valueNames in
  let typeArgs=match typ with AST.TSum (_,args)->checkedTypeArgs args|_->Crash.crash "JSON enum decoder received a non-sum type" in
  match tryFindConstructorId typeName variant.Types.name env.symbols with
  | Some id->ok env (Constructor ({typeId=typeId env typeName;constructorId=id;typeArgs},values))
  | None->Crash.crash ("JSON enum constructor was not interned: "^typeName^"."^variant.Types.name) in
 let* exactBody,exactState=sequenceDecoded env source (decodedItems (List.length fieldTypes)) (fun _->constructed) state in
 let rec missingCases count current acc=
  if count>=List.length fieldTypes then Ok (List.rev acc,current) else
  let missing=error env (constructor env "Darklang.Stdlib.Json.ParseError.ParseError" "EnumMissingField" (tuplePayload [typeReference env (List.nth fieldTypes count);BigIntLiteral (Z.of_int count);casePath])) in
  let* body,next=sequenceDecoded env source (decodedItems count) (fun _->missing) current in
  missingCases (count+1) next (makeCase (PList (List.map (fun name->patternLocal name bindings) (List.take count rawNames))) body::acc) in
 let* missing,missingState=missingCases 0 exactState [] in
 let extraPath=listPush env pathPartType casePath (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Index" (Some (BigIntLiteral (Z.of_int (List.length fieldTypes))))) in
 let extra=error env (constructor env "Darklang.Stdlib.Json.ParseError.ParseError" "EnumExtraField" (tuplePayload [rawSource env source (local extraName bindings);extraPath])) in
 let* extraBody,finalState=sequenceDecoded env source (decodedItems (List.length fieldTypes)) (fun _->extra) missingState in
 let exact=makeCase (PList (List.map (fun name->patternLocal name bindings) rawNames)) exactBody in
 let extraPattern=PListCons (List.map (fun name->patternLocal name bindings) rawNames@[patternLocal extraName bindings],PWildcard) in
 let arrayBody=matchExpr (local "__enum_args" bindings,missing@[exact;makeCase extraPattern extraBody]) in
 let body=matchExpr (call env "Darklang.Stdlib.Json.__arrayItems" [source;caseRaw],[makeCase (constructorPattern env "Darklang.Stdlib.Option.Option" "Some" [patternLocal "__enum_args" bindings]) arrayBody;makeCase PWildcard (cantMatch env typ (rawSource env source caseRaw) casePath)]) in
 Ok (body,finalState)
and decodeBody (env:env) typ source view path (state:state)=
 let failure=cantMatch env typ (rawSource env source view) path in
 let optional name=Ok (optionDecoder env typ name source view path state) in
 match typ with
 | AST.TUnit->Ok (If (call env "Darklang.Stdlib.Json.__isNull" [source;view],ok env UnitLiteral,failure),state)
 | AST.TBool->optional "Darklang.Stdlib.Json.__boolValue"|AST.TString->optional "Darklang.Stdlib.Json.__stringValue"|AST.TChar->optional "Darklang.Stdlib.Json.__viewChar"
 | AST.TInt8->optional "Darklang.Stdlib.Json.__viewInt8"|AST.TInt16->optional "Darklang.Stdlib.Json.__viewInt16"|AST.TInt32->optional "Darklang.Stdlib.Json.__viewInt32"|AST.TInt64->optional "Darklang.Stdlib.Json.__viewInt64"|AST.TInt128->optional "Darklang.Stdlib.Json.__viewInt128"|AST.TInt->optional "Darklang.Stdlib.Json.__viewInt"
 | AST.TUInt8->optional "Darklang.Stdlib.Json.__viewUInt8"|AST.TUInt16->optional "Darklang.Stdlib.Json.__viewUInt16"|AST.TUInt32->optional "Darklang.Stdlib.Json.__viewUInt32"|AST.TUInt64->optional "Darklang.Stdlib.Json.__viewUInt64"|AST.TUInt128->optional "Darklang.Stdlib.Json.__viewUInt128"
 | AST.TFloat64->optional "Darklang.Stdlib.Json.__viewFloat"|AST.TSum ("Uuid",[])->optional "Darklang.Stdlib.Json.__viewUuid"|AST.TDateTime->optional "Darklang.Stdlib.Json.__viewDateTime"
 | AST.TList elemType->
  let* listDecoder,nextState=ensureListDecoder env elemType state in
  let arrayStartId,nextState=freshBinding "__array_start" nextState in
  let currentEnv={env with symbols=nextState.symbols} in
  Ok (Let (LPVariable arrayStartId,call currentEnv "Darklang.Stdlib.Json.__arrayStart" [source;view],If (BinOp (AST.Lt,Local arrayStartId,Int64Literal 0L),failure,call currentEnv listDecoder [source;view;Local arrayStartId;path;Int64Literal 0L])),nextState)
 | AST.TTuple elementTypes->
  let names=List.mapi (fun index _->Printf.sprintf "__tuple_raw_%d" index) elementTypes in
  let valueNames=List.mapi (fun index _->Printf.sprintf "__tuple_value_%d" index) elementTypes in
  let bindings,state=freshBindings (names@valueNames) state in
  let patterns=List.map (fun name->patternLocal name bindings) names in
  let items=List.mapi (fun index elemType->
   let itemPath=listPush env pathPartType path (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Index" (Some (BigIntLiteral (Z.of_int index)))) in
   let valueId=match M.find_opt (List.nth valueNames index) bindings with Some id->id|None->Crash.crash "JSON tuple value binding was not allocated" in
   elemType,local (List.nth names index) bindings,itemPath,valueId) elementTypes in
  let* decoded,nextState=sequenceDecoded env source items (fun decoded->ok env (TupleLiteral (tupleElementsOfList (List.map (fun (id,_)->Local id) decoded)))) state in
  Ok (matchExpr (call env "Darklang.Stdlib.Json.__arrayItems" [source;view],[makeCase (constructorPattern env "Darklang.Stdlib.Option.Option" "Some" [PList patterns]) decoded;makeCase PWildcard failure]),nextState)
 | AST.TRecord (typeName,typeArgs)->(match M.find_opt typeName env.records with
  | None->Error ("Unsupported type in JSON: "^CheckingDiagnostics.typeToString typ)
  | Some recordInfo->
   let* subst=substitution recordInfo.Types.typeParams typeArgs in
   let objectMapId,state=freshBinding "__object_field_map" state in
   (* Conversion checks required fields in declaration order; wire
      serialization is independently ordinal-by-name. *)
   let fields=recordInfo.Types.fields in
   let rec build remaining current decodedFields=match remaining with
   | []->Ok (ok env (recordLiteral env typeName typeArgs (List.rev decodedFields)),current)
   | (fieldName,fieldType)::rest->
    let concrete=resolveJsonType env (applySubstitution subst fieldType) in
    let fieldRawId,current=freshBinding "__field_raw" current in
    let fieldValueId,current=freshBinding ("__field_"^fieldName) current in
    let fieldErrorId,current=freshBinding "__field_error" current in
    let matches=TypeApp (resolveFunction env "Darklang.Stdlib.Dict.get",checkedTypeArgs [AST.TString;valueViewType],args [Local objectMapId;StringLiteral fieldName]) in
    let fieldPath=listPush env pathPartType path (constructor env "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Field" (Some (StringLiteral fieldName))) in
    let* decoded,next=decodeCall env concrete source (Local fieldRawId) fieldPath current in
    let id=fieldId env typeName fieldName in
    let* tail,finalState=build rest next ((id,Local fieldValueId)::decodedFields) in
    let missing=error env (constructor env "Darklang.Stdlib.Json.ParseError.ParseError" "RecordMissingField" (tuplePayload [StringLiteral fieldName;path])) in
    let duplicate=error env (constructor env "Darklang.Stdlib.Json.ParseError.ParseError" "RecordDuplicateField" (tuplePayload [StringLiteral fieldName;path])) in
    let one=matchExpr (decoded,resultCases env fieldValueId tail fieldErrorId) in
    Ok (matchExpr (matches,[makeCase (constructorPattern env "Darklang.Stdlib.Option.Option" "None" []) missing;makeCase (constructorPattern env "Darklang.Stdlib.Option.Option" "Some" [PVariable fieldRawId]) (If (call env "Darklang.Stdlib.Json.__viewIsDuplicate" [Local fieldRawId],duplicate,one))]),finalState) in
   let* decoded,nextState=build fields state [] in
   Ok (matchExpr (call env "Darklang.Stdlib.Json.__objectFieldMap" [source;view],[makeCase (constructorPattern env "Darklang.Stdlib.Option.Option" "Some" [PVariable objectMapId]) decoded;makeCase PWildcard failure]),nextState))
 | AST.TDict (AST.TString,valueType)->
  let* dictDecoder,nextState=ensureDictDecoder env valueType state in
  let objectFieldsId,nextState=freshBinding "__object_fields" nextState in
  let currentEnv={env with symbols=nextState.symbols} in
  let empty=DictLiteral (checkedType AST.TString,checkedType valueType,[]) in
  Ok (matchExpr (call currentEnv "Darklang.Stdlib.Json.__objectFields" [source;view],[makeCase (constructorPattern currentEnv "Darklang.Stdlib.Option.Option" "Some" [PVariable objectFieldsId]) (call currentEnv dictDecoder [source;Local objectFieldsId;path;empty]);makeCase PWildcard failure]),nextState)
 | AST.TSum (typeName,typeArgs)->(match M.find_opt typeName env.sums with
  | None->Error ("Unsupported type in JSON: "^CheckingDiagnostics.typeToString typ)
  | Some sumInfo->
   let* subst=substitution sumInfo.Types.typeParams typeArgs in
   let caseNameId,state=freshBinding "__case_name" state in
   let caseRawId,state=freshBinding "__case_raw" state in
   let caseNamesId,state=freshBinding "__case_names" state in
   let rec buildCases remaining current acc=match remaining with
   | []->Ok (List.rev acc,current)
   | (variant:sumVariant)::rest->let* body,next=decodeEnumCase env typ typeName subst source path (Local caseRawId) variant current in buildCases rest next (makeCase (PString variant.Types.name) body::acc) in
   let* caseMatches,nextState=buildCases (List.stable_sort (fun (a:sumVariant) b->compare a.Types.tag b.Types.tag) sumInfo.Types.variants) state [] in
   let invalidCase=error env (constructor env "Darklang.Stdlib.Json.ParseError.ParseError" "EnumInvalidCasename" (tuplePayload [typeReference env typ;Local caseNameId;path])) in
   let oneField=matchExpr (Local caseNameId,caseMatches@[makeCase PWildcard invalidCase]) in
   let tooMany=error env (constructor env "Darklang.Stdlib.Json.ParseError.ParseError" "EnumTooManyCases" (tuplePayload [typeReference env typ;Local caseNamesId;path])) in
   let checkedOneField=oneField in
   let objectBody=matchExpr (call env "Darklang.Stdlib.Json.__enumCandidate" [source;view],[makeCase (constructorPattern env "Darklang.Stdlib.Json.InternalEnumObject" "EnumNoFields" []) failure;makeCase (constructorPattern env "Darklang.Stdlib.Json.InternalEnumObject" "EnumOneField" [PVariable caseNameId;PVariable caseRawId]) checkedOneField;makeCase (constructorPattern env "Darklang.Stdlib.Json.InternalEnumObject" "EnumManyFields" [PVariable caseNamesId]) tooMany;makeCase PWildcard failure]) in
   Ok (objectBody,nextState))
 | AST.TFunction _|AST.TBlob|AST.TInternalRawPtr|AST.TNever|AST.TStream _|AST.TVar _|AST.TInferenceVar _|AST.TDict _->Error ("Unsupported type in JSON: "^CheckingDiagnostics.typeToString typ^". Some types are not supported in Json serialization")
let rec mapExpr rewrite symbols expr=
 let mapList values state=mapFold (fun current value->mapExpr rewrite current value) state values in
 let mapNonEmpty values state=let mapped,next=mapList (N.toList values) state in N.fromList mapped,next in
 let mapPair first second state=let first,afterFirst=mapExpr rewrite state first in let second,next=mapExpr rewrite afterFirst second in first,second,next in
 let mapped,symbols=match expr with
 | UnitLiteral|Int64Literal _|Int128Literal _|BigIntLiteral _|Int8Literal _|Int16Literal _|Int32Literal _|UInt8Literal _|UInt16Literal _|UInt32Literal _|UInt64Literal _|UInt128Literal _|BoolLiteral _|StringLiteral _|BlobLiteral _|CharLiteral _|FloatLiteral _|Local _|FuncRef _|RuntimeError _->expr,symbols
 | InterpolatedString parts->let parts,next=mapFold (fun current part->match part with StringText _->part,current|StringExpr value->let value,next=mapExpr rewrite current value in StringExpr value,next) symbols parts in InterpolatedString parts,next
 | BinOp (op,left,right)->let left,right,next=mapPair left right symbols in BinOp (op,left,right),next
 | UnaryOp (op,inner)->let inner,next=mapExpr rewrite symbols inner in UnaryOp (op,inner),next
 | Let (pattern,value,body)->let value,body,next=mapPair value body symbols in Let (pattern,value,body),next
 | RecursiveLet (recursion,value,body)->let value,body,next=mapPair value body symbols in RecursiveLet (recursion,value,body),next
 | If (condition,thenBranch,elseBranch)->let condition,afterCondition=mapExpr rewrite symbols condition in let thenBranch,elseBranch,next=mapPair thenBranch elseBranch afterCondition in If (condition,thenBranch,elseBranch),next
 | Sequence (first,nextExpr)->let first,nextExpr,next=mapPair first nextExpr symbols in Sequence (first,nextExpr),next
 | Call (name,values)->let values,next=mapNonEmpty values symbols in Call (name,values),next
 | TypeApp (name,types,values)->let values,next=mapNonEmpty values symbols in TypeApp (name,types,values),next
 | TupleLiteral values->let values,next=mapList (tupleElementsToList values) symbols in TupleLiteral (tupleElementsOfList values),next
 | TupleAccess (value,index)->let value,next=mapExpr rewrite symbols value in TupleAccess (value,index),next
 | DictLiteral (keyType,valueType,entries)->let entries,next=mapFold (fun current (key,value)->let key,value,next=mapPair key value current in (key,value),next) symbols entries in DictLiteral (keyType,valueType,entries),next
 | RecordLiteral (name,fields)->let fields,next=mapFoldRecordFields (mapExpr rewrite) symbols fields in RecordLiteral (name,fields),next
 | RecordUpdate (record,fields)->let record,afterRecord=mapExpr rewrite symbols record in let fields,next=mapFold (fun current (field,value)->let value,next=mapExpr rewrite current value in (field,value),next) afterRecord fields in RecordUpdate (record,fields),next
 | RecordAccess (record,field)->let record,next=mapExpr rewrite symbols record in RecordAccess (record,field),next
 | Constructor (reference,fields)->let fields,next=mapList fields symbols in Constructor (reference,fields),next
 | Match (value,cases)->let value,afterValue=mapExpr rewrite symbols value in
  let cases,next=mapFold (fun current case->let guard,afterGuard=match case.guard with None->None,current|Some guard->let guard,next=mapExpr rewrite current guard in Some guard,next in let body,next=mapExpr rewrite afterGuard case.body in {case with guard;body},next) afterValue (N.toList cases) in matchExpr (value,cases),next
 | ListLiteral values->let values,next=mapList values symbols in ListLiteral values,next
 | Lambda (parameters,annotation,body)->let body,next=mapExpr rewrite symbols body in Lambda (parameters,annotation,body),next
 | Apply (fn,values)|IndirectApply (fn,values)->let fn,afterFn=mapExpr rewrite symbols fn in let values,next=mapNonEmpty values afterFn in (match expr with Apply _->Apply (fn,values),next|_->IndirectApply (fn,values),next)
 | Closure (name,captures)->let captures,next=mapList captures symbols in Closure (name,captures),next
 | BoundaryRender (renderer,value)->let value,next=mapExpr rewrite symbols value in BoundaryRender (renderer,value),next in
 rewrite symbols mapped
let rewriteProgramWithSession (session:planningSession option) (env:Types.typeCheckEnv) program=
 let symbols,topLevels=viewProgram program in
 let serializeId=tryFindFunctionId "Darklang.Stdlib.Json.serialize" symbols in
 let parseId=tryFindFunctionId "Darklang.Stdlib.Json.parse" symbols in
 let collect expr acc=
  let initialResult=acc in
  (* mapExpr provides a compact complete traversal; the fold result is
     threaded functionally through this local recursive collector. *)
  let rec walk current collected=
   let collected=match current with
   | TypeApp (id,[typ],_) when Some id=serializeId->semanticType typ::fst collected,snd collected
   | TypeApp (id,[typ],_) when Some id=parseId->fst collected,semanticType typ::snd collected
   | _->collected in
   let capture child=walk child in
   match current with
   | BinOp (_,a,b)|Sequence (a,b)->capture b (capture a collected)
   | UnaryOp (_,a)|TupleAccess (a,_)|RecordAccess (a,_)|BoundaryRender (_,a)->capture a collected
   | Let (_,a,b)|RecursiveLet (_,a,b)->capture b (capture a collected)
   | If (a,b,c)->capture c (capture b (capture a collected))
   | Call (_,values)|TypeApp (_,_,values)->List.fold_left (fun state expr->capture expr state) collected (N.toList values)
   | TupleLiteral values->List.fold_left (fun state expr->capture expr state) collected (tupleElementsToList values)
   | ListLiteral values|Closure (_,values)->List.fold_left (fun state expr->capture expr state) collected values
   | DictLiteral (_,_,entries)->List.fold_left (fun state (key,value)->capture value (capture key state)) collected entries
   | RecordLiteral (_,fields)->List.fold_left (fun state (_,expr)->capture expr state) collected (recordFieldsInSourceOrder fields)
   | RecordUpdate (record,fields)->List.fold_left (fun state (_,expr)->capture expr state) (capture record collected) fields
   | Constructor (_,fields)->List.fold_left (fun state field->capture field state) collected fields
   | Match (value,cases)->List.fold_left (fun state (case:CheckedAST.matchCase)->capture case.body (match case.guard with Some guard->capture guard state|None->state)) (capture value collected) (N.toList cases)
   | Lambda (_,_,body)->capture body collected
   | Apply (fn,values)|IndirectApply (fn,values)->List.fold_left (fun state expr->capture expr state) (capture fn collected) (N.toList values)
   | InterpolatedString parts->List.fold_left (fun state part->match part with StringText _->state|StringExpr expr->capture expr state) collected parts
   | UnitLiteral|Int64Literal _|Int128Literal _|BigIntLiteral _|Int8Literal _|Int16Literal _|Int32Literal _|UInt8Literal _|UInt16Literal _|UInt32Literal _|UInt64Literal _|UInt128Literal _|BoolLiteral _|StringLiteral _|BlobLiteral _|CharLiteral _|FloatLiteral _|Local _|FuncRef _|RuntimeError _->collected in walk expr initialResult in
 let serializers,parsers=List.fold_left (fun acc topLevel->match topLevel with FunctionDef fn->collect fn.body acc|ValueDef value->collect (valueDefBody value) acc|Expression expr->collect expr acc|TypeDef _->acc) ([],[]) topLevels in
 let distinct values=List.fold_left (fun accumulated value->if List.mem value accumulated then accumulated else value::accumulated) [] values |> List.rev in
 let serializerTypes,parserTypes=distinct serializers,distinct parsers in
 let hasJsonCalls=not (serializerTypes=[] && parserTypes=[]) in
 let symbols=if hasJsonCalls then List.fold_left (fun current name->snd (internFunction name current)) symbols
  [
   "Darklang.Stdlib.DateTime.toString";
   "Darklang.Stdlib.Int.fromInt64";
   "Darklang.Stdlib.Int.toString";
   "Darklang.Stdlib.Int128.toString";
   "Darklang.Stdlib.Int16.toString";
   "Darklang.Stdlib.Int32.toString";
   "Darklang.Stdlib.Int64.toString";
   "Darklang.Stdlib.Int8.toString";
   "Darklang.Stdlib.Json.ParseError.__copyPath";
   "Darklang.Stdlib.Json.__arrayAfter";
   "Darklang.Stdlib.Json.__arrayItems";
   "Darklang.Stdlib.Json.__arrayNext";
   "Darklang.Stdlib.Json.__arrayStart";
   "Darklang.Stdlib.Json.__boolValue";
   "Darklang.Stdlib.Json.__copyRaw";
   "Darklang.Stdlib.Json.__enumCandidate";
   "Darklang.Stdlib.Json.__isNull";
   "Darklang.Stdlib.Json.__objectFieldMap";
   "Darklang.Stdlib.Json.__objectFields";
   "Darklang.Stdlib.Json.__parseRoot";
   "Darklang.Stdlib.Json.__serializeFloat";
   "Darklang.Stdlib.Json.__stringValue";
   "Darklang.Stdlib.Json.__viewChar";
   "Darklang.Stdlib.Json.__viewDateTime";
   "Darklang.Stdlib.Json.__viewFieldName";
   "Darklang.Stdlib.Json.__viewFieldValue";
   "Darklang.Stdlib.Json.__viewFloat";
   "Darklang.Stdlib.Json.__viewInt";
   "Darklang.Stdlib.Json.__viewInt128";
   "Darklang.Stdlib.Json.__viewInt16";
   "Darklang.Stdlib.Json.__viewInt32";
   "Darklang.Stdlib.Json.__viewInt64";
   "Darklang.Stdlib.Json.__viewInt8";
   "Darklang.Stdlib.Json.__viewIsDuplicate";
   "Darklang.Stdlib.Json.__viewUInt128";
   "Darklang.Stdlib.Json.__viewUInt16";
   "Darklang.Stdlib.Json.__viewUInt32";
   "Darklang.Stdlib.Json.__viewUInt64";
   "Darklang.Stdlib.Json.__viewUInt8";
   "Darklang.Stdlib.Json.__viewUuid";
   "Darklang.Stdlib.Json.__writerBeginArray";
   "Darklang.Stdlib.Json.__writerBeginObject";
   "Darklang.Stdlib.Json.__writerEmpty";
   "Darklang.Stdlib.Json.__writerEndArray";
   "Darklang.Stdlib.Json.__writerEndObject";
   "Darklang.Stdlib.Json.__writerFieldName";
   "Darklang.Stdlib.Json.__writerFinish";
   "Darklang.Stdlib.Json.__writerSeparator";
   "Darklang.Stdlib.Json.__writerWriteRaw";
   "Darklang.Stdlib.Json.__writerWriteString";
   "Darklang.Stdlib.List.push";
   "Darklang.Stdlib.Dict.toList";
   "Darklang.Stdlib.Dict.setOverridingDuplicates";
   "Darklang.Stdlib.Dict.get";
   "Darklang.Stdlib.UInt128.toString";
   "Darklang.Stdlib.UInt16.toString";
   "Darklang.Stdlib.UInt32.toString";
   "Darklang.Stdlib.UInt64.toString";
   "Darklang.Stdlib.UInt8.toString";
   "Darklang.Stdlib.Uuid.toString";
  ] else symbols in
 let planningSymbols=M.fold (fun typeName (recordInfo:Types.recordTypeInfo) current->List.mapi (fun index field->index,field) recordInfo.Types.fields |> List.fold_left (fun current (index,(fieldName,_))->snd (internField typeName fieldName index current)) current) env.Types.indexedTypeReg symbols in
 let planningEnv={records=env.Types.indexedTypeReg;sums=(if hasJsonCalls then env.Types.indexedSumTypeReg else M.empty);aliases=env.Types.aliasReg;symbols=planningSymbols} in
 let _mergeArtifact (state:state) functions=
  List.fold_left (fun result (fn:CheckedAST.functionDef)->let* current=result in match M.find_opt fn.name current.functions with
   | Some existing when existing<>fn->Error ("JSON codec name collision for canonical plan '"^fn.name^"'")
   | Some _->Ok current
   | None->Ok {current with functions=M.add fn.name fn current.functions}) (Ok state) functions in
 let planCached direction ensure typ state=
  let concrete=resolveJsonType planningEnv typ in
  let key=direction^"|"^canonicalCodecTypeKey planningEnv concrete in
  match Option.bind session (fun current->current#tryFind key) with
  | Some _->Result.map snd (ensure planningEnv concrete state)
  | None->
   let existingNames=M.to_seq state.functions |> Seq.map fst |> S.of_seq in
   let* _,artifactState=ensure planningEnv concrete state in
   let functions=M.bindings artifactState.functions |> List.filter_map (fun (name,fn)->if S.mem name existingNames then None else Some fn) in
   Option.iter (fun current->current#store key functions) session;
   Ok artifactState in
 let planned=match session with
 | None->
  (* A one-shot caller can share dependencies directly in one state
     without paying for cache keys or artifact merging. *)
  let serializersPlanned=List.fold_left (fun result typ->let* state=result in Result.map snd (ensureSerializer planningEnv typ state)) (Ok {functions=M.empty;symbols=planningSymbols}) serializerTypes in
  List.fold_left (fun result typ->let* state=result in Result.map snd (ensureDecoder planningEnv typ state)) serializersPlanned parserTypes
 | Some _->
  let serializersPlanned=List.fold_left (fun result typ->let* state=result in planCached "serialize" ensureSerializer typ state) (Ok {functions=M.empty;symbols=planningSymbols}) serializerTypes in
  List.fold_left (fun result typ->let* state=result in planCached "parse" ensureDecoder typ state) serializersPlanned parserTypes in
 if not hasJsonCalls then programFromCheckedParts (symbols,topLevels) else
 match planned with
 | Error error->
  let rewrite currentSymbols expr=match expr with TypeApp (id,_,_) when Some id=serializeId || Some id=parseId->RuntimeError error,currentSymbols|_->expr,currentSymbols in
  let rewritten,symbols=mapFold (fun currentSymbols topLevel->match topLevel with
   | FunctionDef fn->let body,next=mapExpr rewrite currentSymbols fn.body in FunctionDef {fn with body},next
   | Expression expr->let expr,next=mapExpr rewrite currentSymbols expr in Expression expr,next
   | other->other,currentSymbols) symbols topLevels in
  programFromCheckedParts (symbols,rewritten)
 | Ok state->
  let finalPlanningEnv={planningEnv with symbols=state.symbols} in
  let rewrite currentSymbols expr=match expr with
  | TypeApp (id,[typ],values) when Some id=serializeId->
   let written=call finalPlanningEnv (serializeName (resolveJsonType finalPlanningEnv (semanticType typ))) (writerEmpty finalPlanningEnv::N.toList values) in
   writerFinish finalPlanningEnv written,currentSymbols
  | TypeApp (id,[typ],values) when Some id=parseId->
   let concrete=resolveJsonType finalPlanningEnv (semanticType typ) in
   let source=N.head values in
   let sourceId,symbols1=allocateBinding "__json_source" currentSymbols in
   let parseResultId,symbols2=allocateBinding "__json_parse_result" symbols1 in
   let viewId,symbols3=allocateBinding "__json_view" symbols2 in
   let parsed=call finalPlanningEnv "Darklang.Stdlib.Json.__parseRoot" [Local sourceId] in
   let rootPath=ListLiteral [constructor finalPlanningEnv "Darklang.Stdlib.Json.ParseError.JsonPath.Part.Part" "Root" None] in
   Let (LPVariable sourceId,source,Let (LPVariable parseResultId,parsed,matchExpr (Local parseResultId,[makeCase (constructorPattern planningEnv "Darklang.Stdlib.Result.Result" "Ok" [PVariable viewId]) (call finalPlanningEnv (decoderName concrete) [Local sourceId;Local viewId;rootPath]);makeCase (constructorPattern planningEnv "Darklang.Stdlib.Result.Result" "Error" [PWildcard]) (error finalPlanningEnv (constructor finalPlanningEnv "Darklang.Stdlib.Json.ParseError.ParseError" "NotJson" None))]))),symbols3
  | _->expr,currentSymbols in
  let rewritten,finalSymbols=mapFold (fun currentSymbols topLevel->match topLevel with
   | FunctionDef fn->let body,next=mapExpr rewrite currentSymbols fn.body in FunctionDef {fn with body},next
   | Expression expr->let expr,next=mapExpr rewrite currentSymbols expr in Expression expr,next
   | other->other,currentSymbols) state.symbols topLevels in
  let generated=List.map (fun (_,fn)->FunctionDef fn) (M.bindings state.functions) in
  programFromCheckedParts (finalSymbols,generated@rewritten)
let rewriteProgram (env:Types.typeCheckEnv) program=rewriteProgramWithSession None env program
