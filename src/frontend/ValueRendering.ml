(* ValueRendering.fs - Interpreter-compatible result rendering.
   Builds monomorphic Dark functions which render values at the eval boundary.
   Keeping recursion in ordinary Dark code gives tuples, lists, records, and sums
   one renderer on every native target instead of backend-specific shape switches. *)
[@@@warning "-4"]
open AST
module M=StringOrder.Map
module C=CheckedAST
module N=AST.NonEmptyList
(* Declaration metadata stays lazy at primitive rendering boundaries. *)
type renderEnv={records:Types.recordTypeInfo M.t Lazy.t;sums:Types.sumTypeInfo M.t Lazy.t}
type renderState={functions:C.functionDef M.t;symbols:C.symbols}
let freshBinding name state=let id,symbols=C.allocateBinding name state.symbols in id,{state with symbols}
let constructorPattern typeName (variant:Types.sumVariantInfo) symbols fields=match C.tryFindConstructorId typeName variant.Types.name symbols with Some id->C.PConstructor (id,fields)|None->Crash.crash ("Value renderer constructor was not interned: "^typeName^"."^variant.Types.name)
let args values=N.fromList values
let resolveFunction symbols name=match C.tryFindFunctionId name symbols with Some id->id|None->Crash.crash ("Value renderer function was not interned: "^name)
let call symbols name values=C.Call (resolveFunction symbols name,args values)
let concat symbols=function []->C.StringLiteral ""|first::rest->
 (* Every renderer fragment has an ASCII delimiter at each join: quotes,
    punctuation, separators, or the edge of a canonical numeric value.
    Those boundaries cannot compose under NFC, so retain the native raw
    concat used before public StringConcat acquired normalization. *)
 List.fold_left (fun acc part->call symbols "__string_concat_raw" [acc;part]) first rest
let stableHash value=String.fold_left (fun hash byte->Int64.mul (Int64.logxor hash (Int64.of_int (Char.code byte))) 1099511628211L) 0xcbf29ce484222325L value
let hashed prefix typ=prefix^Printf.sprintf "%016Lx" (stableHash (CheckingDiagnostics.typeToHelperIdentityString typ))
let rendererName=hashed "__dark_render_value_"
let listItemsRendererName=hashed "__dark_render_list_items_"
let dictItemsRendererName=hashed "__dark_render_dict_items_"
let runtimeFunctionNames=["__string_concat_raw";"Darklang.Stdlib.Bool.toString";"Darklang.Stdlib.DateTime.toString";"Darklang.Stdlib.Dict.__renderKey";"Darklang.Stdlib.Dict.toList";"Darklang.Stdlib.Float.toString";"Darklang.Stdlib.Int.toString";"Darklang.Stdlib.Int128.toString";"Darklang.Stdlib.Int16.toString";"Darklang.Stdlib.Int32.toString";"Darklang.Stdlib.Int64.toString";"Darklang.Stdlib.Int8.toString";"Darklang.Stdlib.String.length";"Darklang.Stdlib.String.replaceAll";"Darklang.Stdlib.UInt128.toString";"Darklang.Stdlib.UInt16.toString";"Darklang.Stdlib.UInt32.toString";"Darklang.Stdlib.UInt64.toString";"Darklang.Stdlib.UInt8.toString";"Darklang.Stdlib.Uuid.toString"]
let applySubstitution subst typ=
 let rec apply=function
 |(TVar name|TInferenceVar (_,name)) as typ->Option.value ~default:typ (M.find_opt name subst)
 |TList elem->TList (apply elem)|TStream elem->TStream (apply elem)|TDict (key,value)->TDict (apply key,apply value)
 |TFunction (params,result)->TFunction (List.map apply params,apply result)|TTuple elems->TTuple (List.map apply elems)
 |TRecord (name,args)->TRecord (name,List.map apply args)|TSum (name,args)->TSum (name,List.map apply args)
 |(TInt8|TInt16|TInt32|TInt64|TInt128|TInt|TUInt8|TUInt16|TUInt32|TUInt64|TUInt128|TBool|TFloat64|TString|TBlob|TChar|TDateTime|TUnit|TNever|TInternalRawPtr) as typ->typ in apply typ
let typeSubstitution params arguments=if List.length params=List.length arguments then M.of_list (List.combine params arguments) else Crash.crash ("Value renderer type argument mismatch: params="^string_of_int (List.length params)^", args="^string_of_int (List.length arguments))
let escapedString symbols quote value=
 let replace oldValue newValue input=call symbols "Darklang.Stdlib.String.replaceAll" [input;C.StringLiteral oldValue;C.StringLiteral newValue] in
 let escaped=value |> replace "\\" "\\\\" |> replace "\n" "\\n" |> replace "\r" "\\r" |> replace "\t" "\\t" |> replace quote ("\\"^quote) in
 concat symbols [C.StringLiteral quote;escaped;C.StringLiteral quote]
let makeCase pattern body={C.patterns=N.singleton pattern;guard=None;body}
let rec canonicalRenderType env typ=let canonical=canonicalRenderType env in match typ with
 |TRecord (name,args) when M.mem name (Lazy.force env.sums)->TSum (name,List.map canonical args)
 |TRecord (name,args)->TRecord (name,List.map canonical args)|TSum (name,args)->TSum (name,List.map canonical args)
 |TTuple elems->TTuple (List.map canonical elems)|TList elem->TList (canonical elem)|TDict (key,value)->TDict (canonical key,canonical value)
 |TFunction (params,result)->TFunction (List.map canonical params,canonical result)|_->typ
let functionDef id name parameter typ={C.id;name;typeParams=[];params=N.singleton (parameter,C.checkedType typ);returnType=C.checkedType TString;body=C.StringLiteral "";recursion=None}
let rec ensureRenderer env typ (state:renderState)=
 let typ=canonicalRenderType env typ in let name=rendererName typ in
 match M.find_opt name state.functions with Some _->name,state|None->
 let functionId,symbols=C.internFunction name state.symbols in let state={state with symbols} in let valueId,state=freshBinding "__value" state in
 (* Reserve the name before descending so recursive sum types terminate. *)
 let reservedDefinition=functionDef functionId name valueId typ in let reserved={state with functions=M.add name reservedDefinition state.functions} in
 let body,(withDependencies:renderState)=renderBody env typ (C.Local valueId) reserved in
 let completed={reservedDefinition with C.body} in name,{withDependencies with functions=M.add name completed withDependencies.functions}
and renderCall env typ value state=let name,nextState=ensureRenderer env typ state in call nextState.symbols name [value],nextState
and renderDelimited env items state=
 let rec loop remaining current acc=match remaining with []->List.rev acc,current|(typ,expr)::rest->let rendered,next=renderCall env typ expr current in loop rest next (rendered::acc) in loop items state []
and ensureListItemsRenderer env elemType (state:renderState)=
 let listType=TList elemType in let name=listItemsRendererName listType in
 match M.find_opt name state.functions with Some _->name,state|None->
 let functionId,symbols=C.internFunction name state.symbols in let state={state with symbols} in
 let itemsId,state=freshBinding "__items" state in let headId,state=freshBinding "__head" state in let tailId,state=freshBinding "__tail" state in
 let reservedDefinition=functionDef functionId name itemsId listType in let reserved={state with functions=M.add name reservedDefinition state.functions} in
 let renderedHead,withElemRenderer=renderCall env elemType (C.Local headId) reserved in
 let tailBody=C.Match (C.Local tailId,args [makeCase (C.PList []) (C.StringLiteral "");makeCase C.PWildcard (concat reserved.symbols [C.StringLiteral ", ";call reserved.symbols name [C.Local tailId]])]) in
 let body=C.Match (C.Local itemsId,args [makeCase (C.PList []) (C.StringLiteral "");makeCase (C.PListCons ([C.PVariable headId],C.PVariable tailId)) (concat withElemRenderer.symbols [renderedHead;tailBody])]) in
 name,{withElemRenderer with functions=M.add name {reservedDefinition with C.body} withElemRenderer.functions}
and ensureDictItemsRenderer env keyType valueType (state:renderState)=
 let listType=TList (TTuple [keyType;valueType]) in let name=dictItemsRendererName (TDict (keyType,valueType)) in
 match M.find_opt name state.functions with Some _->name,state|None->
 let functionId,symbols=C.internFunction name state.symbols in let state={state with symbols} in
 let entriesId,state=freshBinding "__entries" state in let entryId,state=freshBinding "__entry" state in let tailId,state=freshBinding "__tail" state in
 let reservedDefinition=functionDef functionId name entriesId listType in let reserved={state with functions=M.add name reservedDefinition state.functions} in
 let entryKey=C.TupleAccess (C.Local entryId,0) in let entryValue=C.TupleAccess (C.Local entryId,1) in
 let renderedKey,withKeyRenderer=match keyType with TString->call reserved.symbols "Darklang.Stdlib.Dict.__renderKey" [entryKey],reserved|_->renderCall env keyType entryKey reserved in
 let separator=if keyType=TString then " = " else ": " in let renderedValue,withValueRenderer=renderCall env valueType entryValue withKeyRenderer in
 let renderedEntry=concat withValueRenderer.symbols [renderedKey;C.StringLiteral separator;renderedValue] in
 let tailBody=C.Match (C.Local tailId,args [makeCase (C.PList []) (C.StringLiteral "");makeCase C.PWildcard (concat withValueRenderer.symbols [C.StringLiteral "; ";call withValueRenderer.symbols name [C.Local tailId]])]) in
 let body=C.Match (C.Local entriesId,args [makeCase (C.PList []) (C.StringLiteral "");makeCase (C.PListCons ([C.PVariable entryId],C.PVariable tailId)) (concat withValueRenderer.symbols [renderedEntry;tailBody])]) in
 name,{withValueRenderer with functions=M.add name {reservedDefinition with C.body} withValueRenderer.functions}
and renderBody env typ value state=match typ with
 |TUnit->C.StringLiteral "()",state
 |TBool->call state.symbols "Darklang.Stdlib.Bool.toString" [value],state
 |TInt8->call state.symbols "Darklang.Stdlib.Int8.toString" [value],state|TInt16->call state.symbols "Darklang.Stdlib.Int16.toString" [value],state|TInt32->call state.symbols "Darklang.Stdlib.Int32.toString" [value],state|TInt64->call state.symbols "Darklang.Stdlib.Int64.toString" [value],state|TInt->call state.symbols "Darklang.Stdlib.Int.toString" [value],state
 |TUInt8->call state.symbols "Darklang.Stdlib.UInt8.toString" [value],state|TUInt16->call state.symbols "Darklang.Stdlib.UInt16.toString" [value],state|TUInt32->call state.symbols "Darklang.Stdlib.UInt32.toString" [value],state|TUInt64->call state.symbols "Darklang.Stdlib.UInt64.toString" [value],state
 (* Fixed-block 128-bit values cross the textual boundary through their
    limb-based decimal formatters. *)
 |TInt128->call state.symbols "Darklang.Stdlib.Int128.toString" [value],state|TUInt128->call state.symbols "Darklang.Stdlib.UInt128.toString" [value],state
 |TFloat64->call state.symbols "Darklang.Stdlib.Float.toString" [value],state|TString->escapedString state.symbols "\"" value,state|TChar->escapedString state.symbols "'" value,state|TDateTime->call state.symbols "Darklang.Stdlib.DateTime.toString" [value],state
 |TTuple elems->let items=List.mapi (fun index typ->typ,C.TupleAccess (value,index)) elems in let rendered,next=renderDelimited env items state in let separated=List.concat (List.mapi (fun index expr->if index=0 then [expr] else [C.StringLiteral ", ";expr]) rendered) in concat next.symbols (C.StringLiteral "("::separated @ [C.StringLiteral ")"]),next
 |TList elem->let itemsName,next=ensureListItemsRenderer env elem state in let typeName=CheckingDiagnostics.typeToString typ in C.Match (value,args [makeCase (C.PList []) (C.StringLiteral (typeName^" []"));makeCase C.PWildcard (concat next.symbols [C.StringLiteral "[";call next.symbols itemsName [value];C.StringLiteral "]"])]),next
 |TStream _->C.StringLiteral "<stream>",state
 (* An unconstrained Dict value can only be the polymorphic empty literal;
    no value renderer is needed because there are no entries to inspect. *)
 |TDict ((TVar _|TInferenceVar _),(TVar _|TInferenceVar _))->C.StringLiteral "Dict { }",state
 |TDict (key,valueType)->let itemsName,next=ensureDictItemsRenderer env key valueType state in let entriesId,next=freshBinding "__dict_entries" next in let entries=C.TypeApp (resolveFunction next.symbols "Darklang.Stdlib.Dict.toList",C.checkedTypeArgs [key;valueType],N.singleton value) in
  C.Let (C.LPVariable entriesId,entries,C.Match (C.Local entriesId,args [makeCase (C.PList []) (C.StringLiteral "Dict { }");makeCase C.PWildcard (concat next.symbols [C.StringLiteral "Dict { ";call next.symbols itemsName [C.Local entriesId];C.StringLiteral " }"])])),next
 |TRecord (typeName,typeArgs)->(match M.find_opt typeName (Lazy.force env.records) with None->Crash.crash ("Missing record metadata for value renderer: "^typeName)|Some recordInfo->
  let rec collect=function TVar name|TInferenceVar (_,name)->[name]|TList elem->collect elem|TDict (key,value)->collect key @ collect value|TFunction (params,result)->List.concat_map collect params @ collect result|TTuple elems->List.concat_map collect elems|TRecord (_,args)|TSum (_,args)->List.concat_map collect args|_->[] in
  let fallbackTypeParams=List.concat_map (fun (_,typ)->collect typ) recordInfo.Types.fields |> List.fold_left (fun names name->if List.mem name names then names else names @ [name]) [] in
  let typeParams=if recordInfo.Types.typeParams=[] then fallbackTypeParams else recordInfo.Types.typeParams in let subst=typeSubstitution typeParams typeArgs in
  let sortedFields=List.mapi (fun index (name,typ)->index,name,typ) recordInfo.Types.fields |> List.stable_sort (fun (_,a,_) (_,b,_)->StringOrder.compare a b) in
  let rec renderFields remaining current acc=match remaining with []->List.rev acc,current|(index,name,typ)::rest->let concrete=applySubstitution subst typ in let fieldId,symbols=C.internField typeName name index current.symbols in let rendered,next=renderCall env concrete (C.RecordAccess (value,fieldId)) {current with symbols} in renderFields rest next ((name,rendered)::acc) in
  let renderedFields,next=renderFields sortedFields state [] in
  let parts separator=List.concat (List.mapi (fun index (name,rendered)->[C.StringLiteral ((if index=0 then "" else separator)^name^": ");rendered]) renderedFields) in
  let typeText=CheckingDiagnostics.typeToString typ in let short=concat next.symbols (C.StringLiteral (typeText^" { ")::parts ", " @ [C.StringLiteral " }"]) in let long=concat next.symbols (C.StringLiteral (typeText^" {\n  ")::parts ",\n  " @ [C.StringLiteral "\n}"]) in
  let shortId,next=freshBinding "__record_short" next in C.Let (C.LPVariable shortId,short,C.If (C.BinOp (Lte,call next.symbols "Darklang.Stdlib.String.length" [C.Local shortId],C.BigIntLiteral (Z.of_int 80)),C.Local shortId,long)),next)
 |TSum ("Uuid",[])->call state.symbols "Darklang.Stdlib.Uuid.toString" [value],state
 |TSum (typeName,typeArgs)->(match M.find_opt typeName (Lazy.force env.sums) with None->Crash.crash ("Missing sum metadata for value renderer: "^typeName)|Some sumInfo->
  let subst=typeSubstitution sumInfo.Types.typeParams typeArgs in let typeText=CheckingDiagnostics.typeToString typ in
  let rec buildCases remaining current acc=match remaining with []->List.rev acc,current|(variant:Types.sumVariantInfo)::rest->match variant.Types.fields with
  |[]->let case=makeCase (constructorPattern typeName variant current.symbols []) (C.StringLiteral (typeText^"."^variant.Types.name)) in buildCases rest current (case::acc)
  |fieldTypes->let concreteTypes=List.map (applySubstitution subst) fieldTypes in let fieldNames=List.mapi (fun index _->"__field_"^string_of_int variant.Types.tag^"_"^string_of_int index) fieldTypes in
   let reversed,current=List.fold_left (fun (ids,state) name->let id,next=freshBinding name state in id::ids,next) ([],current) fieldNames in let fieldIds=List.rev reversed in
   let items=List.map2 (fun typ id->typ,C.Local id) concreteTypes fieldIds in let rendered,next=renderDelimited env items current in let separated=List.concat (List.mapi (fun index expr->if index=0 then [expr] else [C.StringLiteral ", ";expr]) rendered) in
   let body=concat next.symbols (C.StringLiteral (typeText^"."^variant.Types.name^"(")::separated @ [C.StringLiteral ")"]) in let case=makeCase (constructorPattern typeName variant next.symbols (List.map (fun id->C.PVariable id) fieldIds)) body in buildCases rest next (case::acc) in
  let cases,next=buildCases (List.stable_sort (fun (a:Types.sumVariantInfo) (b:Types.sumVariantInfo)->Int.compare a.Types.tag b.Types.tag) sumInfo.Types.variants) state [] in C.Match (value,args cases),next)
 |TFunction _->C.StringLiteral "(lambda)",state
 |TBlob->
  (* The interpreter deliberately does not expose ephemeral Blob payloads
     or process-local identities through value rendering. *)
  C.StringLiteral "<Blob: ephemeral>",state
 |TInternalRawPtr->call state.symbols "Darklang.Stdlib.Int64.toString" [value],state|TNever->C.StringLiteral "()",state
 |TVar name->Crash.crash ("Unresolved type variable in value renderer: "^name)|TInferenceVar (displayName,_)->Crash.crash ("Unresolved inference variable in value renderer: "^displayName)
let starts name prefix=Text.startsWith name prefix
let includeRuntimeFunctions symbols=List.fold_left (fun symbols name->snd (C.internFunction name symbols)) symbols runtimeFunctionNames
let existingRenderers topLevels=List.filter_map (function C.FunctionDef definition when starts definition.C.name "__dark_render_"->Some (definition.C.name,definition)|_->None) topLevels |> M.of_list
let rewriteProgram recordMetadata sumMetadata programType program=
 let symbols,topLevels=C.viewProgram program in let symbols=includeRuntimeFunctions symbols in
 (* Type checking already built and overlaid these immutable indexes. Keep
    them lazy so primitive renderers do not inspect declaration metadata. *)
 let env={records=lazy recordMetadata;sums=lazy sumMetadata} in let existing=existingRenderers topLevels in
 let renderName,state=match programType with TDateTime->None,{functions=existing;symbols}|_->let name,state=ensureRenderer env programType {functions=existing;symbols} in Some name,state in
 let rec tryNamedPartialName=function
 |C.Let (C.LPVariable capture,_,body) when Option.fold ~none:false ~some:(fun name->starts name "__partial_capture_") (C.bindingName capture symbols)->tryNamedPartialName body
 |C.Lambda (parameters,_returnAnnotation,body)->
  let parameterIds=N.toList parameters |> List.filter_map (fun (parameter:C.lambdaParameter)->match parameter.C.pattern with C.LPVariable id->Some id|_->None) in
  let generatedPartial=List.length parameterIds=N.length parameters && List.for_all (fun id->Option.fold ~none:false ~some:(fun name->starts name "__partial_") (C.bindingName id symbols)) parameterIds in
  let callNameAndArgs=match body with C.Call (name,callArgs)|C.TypeApp (name,_,callArgs)->Some (name,N.toList callArgs)|_->None in
  (match generatedPartial,callNameAndArgs with true,Some (name,callArgs) when List.length callArgs>List.length parameterIds->let trailing=List.filteri (fun index _->index>=List.length callArgs-List.length parameterIds) callArgs in if List.for_all2 (fun arg id->arg=C.Local id) trailing parameterIds then Some name else None|_->None)
 |_->None in
 let rewriteExpression state expr=
  let rendered,next=match programType,expr,tryNamedPartialName expr with
  |TDateTime,_,_->C.BoundaryRender (resolveFunction state.symbols "Darklang.Stdlib.DateTime.toString",expr),state
  |TFunction _,_,Some functionId->let id,next=freshBinding "__rendered_named_partial" state in let name=match C.functionName functionId state.symbols with Some name->name|None->Crash.crash "Named partial function identity is absent from symbols" in C.Let (C.LPVariable id,expr,C.StringLiteral name),next
  |TFunction _,C.FuncRef functionId,_->let id,next=freshBinding "__rendered_named_function" state in let name=match C.functionName functionId state.symbols with Some name->name|None->Crash.crash "Function identity is absent from symbols" in C.Let (C.LPVariable id,expr,C.StringLiteral name),next
  |TFunction _,C.Lambda _,_->let id,next=freshBinding "__rendered_lambda" state in C.Let (C.LPVariable id,expr,C.StringLiteral "(lambda)"),next
  |_->let renderer=match renderName with Some name->resolveFunction state.symbols name|None->Crash.crash "Missing boundary value renderer" in C.BoundaryRender (renderer,expr),state in
  C.Expression rendered,next in
 let reversed,final=List.fold_left (fun (tops,state) top->let top,next=match top with C.Expression expr->rewriteExpression state expr|_->top,state in top::tops,next) ([],state) topLevels in
 let generated=M.bindings (M.filter (fun name _->not (M.mem name existing)) state.functions) |> List.map (fun (_,func)->C.FunctionDef func) in
 C.programFromCheckedParts (final.symbols,generated @ List.rev reversed)
(* Replace the concrete specialization of Dict's generic key renderer with
   the same monomorphic renderer used for eval results. *)
let rewriteDictionaryKeyRenderers recordMetadata sumMetadata program=
 let symbols,topLevels=C.viewProgram program in let symbols=includeRuntimeFunctions symbols in let env={records=lazy recordMetadata;sums=lazy sumMetadata} in let existing=existingRenderers topLevels in let initial={functions=existing;symbols} in
 let reversed,final=List.fold_left (fun (tops,state) top->let top,next=match top with C.FunctionDef definition when starts definition.C.name "Darklang.Stdlib.Dict.__renderGenericKey_"->(match N.toList definition.C.params with [(parameter,keyType)]->let renderer,next=ensureRenderer env (C.semanticType keyType) state in C.FunctionDef {definition with C.body=call next.symbols renderer [C.Local parameter]},next|_->Crash.crash "Dictionary key renderer has an invalid parameter list")|_->top,state in top::tops,next) ([],initial) topLevels in
 let generated=M.bindings (M.filter (fun name _->not (M.mem name existing)) final.functions) |> List.map (fun (_,func)->C.FunctionDef func) in C.programFromCheckedParts (final.symbols,generated @ List.rev reversed)
