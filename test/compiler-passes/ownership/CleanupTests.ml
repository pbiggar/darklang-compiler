(*
   CleanupTests.fs - Verify alias transfers and cleanup ordering through ANF scopes.
*)
[@@@warning "-4-42"]
open Dark_compiler
open ANF
open MemoryModel
module M=StringOrder.Map
module T=RcTypeFacts.TempMap
module TS=RcReturnAnalysis.TempSet
let (let*)=Result.bind
let require condition error=if condition then Ok () else Error error
let functionRegistry entries=List.map (fun (name,typ)->TestIds.functionIdForName name,(name,typ)) entries |> FunctionIdMap.ofList
let context funcReg:RcTypeFacts.typeContext={RcTypeFacts.typeReg=M.empty;variantLookup=M.empty;sumShapeReg=M.empty;funcReg;funcParams=M.empty;tempTypes=T.empty;closureFuncs=T.empty;typePlanning=RcTypeFacts.createRcTypePlanningContext ()}
let functionWith name typedParams returnType body:ANF.functionDef={ANF.id=TestIds.functionIdForName name;name;typedParams;returnType;returnOwnership=OwnedReturn;body}
let transform ctx func=let transformed,_,_=RefCountInsertion.insertRCInFunction ctx func initialVarGen in transformed.ANF.body
let param id typ:ANF.typedParam={ANF.id;typ}
let int n=IntLiteral (Int64 n)
let call name args=Call (TestIds.functionIdForName name,args)
let rec hasDecAfterNonSelfTailCall funcName=function
 |Jump _|Return _->false
 |Let (_,TailCall (target,_),Let (_,(RefCountDec _|RefCountDecString _),_)) when target<>funcName->true
 |Let (_,_,body)->hasDecAfterNonSelfTailCall funcName body
 |Join (_,a,b)|If (_,a,b)->hasDecAfterNonSelfTailCall funcName a || hasDecAfterNonSelfTailCall funcName b
let rec hasOperation predicate=function
 |Jump _|Return _->false|Let (_,value,body)->predicate value || hasOperation predicate body|Join (_,a,b)|If (_,a,b)->hasOperation predicate a || hasOperation predicate b
let hasRefCountIncForTemp target=hasOperation (function RefCountInc (Var id,_,_,_) when id=target->true|_->false)
let rec countRefCountIncsForTemps targets=function
 |Jump _|Return _->0|Let (_,RefCountInc (Var id,_,_,_),body)->(if TS.mem id targets then 1 else 0)+countRefCountIncsForTemps targets body
 |Let (_,_,body)->countRefCountIncsForTemps targets body|Join (_,a,b)|If (_,a,b)->countRefCountIncsForTemps targets a+countRefCountIncsForTemps targets b
let hasRefCountDecForTemp target=hasOperation (function RefCountDec (Var id,_,_,_) when id=target->true|_->false)
let hasRawSlotInitForValue target=hasOperation (function RawSlotInit (_,_,Var id,_) when id=target->true|_->false)
let hasRawWriteWordForValue target=hasOperation (function RawWriteWord (_,_,Var id) when id=target->true|_->false)
let hasStringRetainForTemp target=hasOperation (function RefCountIncString (Var id) when id=target->true|_->false)
let hasStringReleaseForTemp target=hasOperation (function RefCountDecString (Var id) when id=target->true|_->false)
let pathHasRetainsBeforeDec targets decTarget expr=
 let required=TS.of_list targets in
 let rec loop joins seen=function
 |Return _->false
 |Jump (target,_)->(match T.find_opt target joins with Some next->loop joins seen next|None->Crash.crash "RC path check: jump target outside lexical scope")
 |Join (parameter,continuation,entry)->loop (T.add parameter.ANF.id continuation joins) seen entry
 |Let (_,RefCountInc (Var id,_,_,_),body)->loop joins (TS.add id seen) body
 |Let (_,RefCountDec (Var id,_,_,_),_) when id=decTarget->TS.subset required seen
 |Let (_,_,body)->loop joins seen body|If (_,a,b)->loop joins seen a || loop joins seen b in loop T.empty TS.empty expr
let rawSlotTransferTestFunction valueType usesValueAfterSlot=
 let listType=AST.TList valueType in let ctx=context (functionRegistry ["makeValue",AST.TFunction ([],valueType);"observeValue",AST.TFunction ([valueType],AST.TUnit);"makeList",AST.TFunction ([],listType)]) in
 let value=TempId 0 and ptr=TempId 1 and slot=TempId 2 and count=TempId 3 and tagged=TempId 4 and list=TempId 5 and observed=TempId 6 and alternateTagged=TempId 7 and alternateList=TempId 8 in
 let listReturn tagged list=Let (tagged,Prim (BitOr,Var ptr,int 2L),Let (list,TypedAtom (Var tagged,listType),Return (Var list))) in
 let tail=if usesValueAfterSlot then listReturn tagged list else If (BoolLiteral true,listReturn tagged list,listReturn alternateTagged alternateList) in
 let after=if usesValueAfterSlot then Let (observed,call "observeValue" [Var value],tail) else tail in
 let body=Let (value,call "makeValue" [],Let (ptr,RawAlloc (int 16L),Let (slot,RawSlotInit (Var ptr,int 0L,Var value,valueType),Let (count,RawWriteWord (Var ptr,int 8L,int 1L),after)))) in ctx,functionWith "makeList" [] listType body,value
let testFreshOwnedValueTransfersIntoRawSlot ()=
 let ctx,func,value=rawSlotTransferTestFunction AST.TString false in let body=transform ctx func in
 let* ()=require (not (hasRawSlotInitForValue value body)) "Fresh raw-slot payload should not retain a copied ownership edge" in
 let* ()=require (hasRawWriteWordForValue value body) "Fresh raw-slot payload should move its owned edge with an unmanaged store" in
 require (not (hasStringReleaseForTemp value body)) "Raw-slot ownership transfer should remove the producer's pending release"
let testRawSlotRetainsValueUsedAfterInitialization ()=
 let ctx,func,value=rawSlotTransferTestFunction AST.TString true in let body=transform ctx func in
 let* ()=require (hasRawSlotInitForValue value body) "Raw slot must retain a payload that remains in use after initialization" in
 require (hasStringReleaseForTemp value body) "Raw-slot payload used afterward must keep the producer's pending release"
let testRawSlotRetainsFreshStreamValue ()=
 let ctx,func,value=rawSlotTransferTestFunction (AST.TStream AST.TInt64) false in let body=transform ctx func in
 let* ()=require (hasRawSlotInitForValue value body) "Fresh Stream raw-slot payload must keep the retain that establishes its first owned edge" in
 require (hasRefCountDecForTemp value body) "Fresh Stream raw-slot payload must retain its local pending release"
let rec tryRefCountDecSourceTypeForTemp target=function
 |Jump _|Return _->None
 |Let (_,RefCountDec (Var id,_,_,metadata),_) when id=target->Option.bind metadata (fun value->value.MemoryModel.sourceType)
 |Let (_,_,body)->tryRefCountDecSourceTypeForTemp target body
 |Join (_,a,b)|If (_,a,b)->match tryRefCountDecSourceTypeForTemp target a with Some typ->Some typ|None->tryRefCountDecSourceTypeForTemp target b
let testBranchLocalTempReuseUsesCurrentTypeContext ()=
 let listType=AST.TList (AST.TTuple [AST.TInt64;AST.TFloat64]) in let expected=AST.TTuple [AST.TInt64;listType] in
 let condition=TempId 0 and payload=TempId 1 and wrapper=TempId 2 in
 let branch value=Let (payload,value,Let (wrapper,TupleAlloc [int 0L;Var payload],Return (int 0L))) in
 let func=functionWith "branchLocalTypeContext" [param condition AST.TBool] AST.TInt64 (If (Var condition,branch (Atom (int 1L)),branch (TypedAtom (int 0L,listType)))) in
 match transform (context FunctionIdMap.empty) func with
 |If (_,_,other)->(match tryRefCountDecSourceTypeForTemp wrapper other with Some actual when actual=expected->Ok ()|Some actual->Error ("Expected branch-local wrapper type "^StructuralFormat.semanticType expected^", got "^StructuralFormat.semanticType actual)|None->Error "Expected branch-local wrapper to receive an automatic RefCountDec")
 |_->Error "Expected branch-local type fixture to retain its conditional body"
let childType=AST.TTuple [AST.TInt64]
let aggregateContext name params result=context (functionRegistry ["makeChild",AST.TFunction ([],childType);name,AST.TFunction (params,result)])
let aliasTransfer typed=
 let outerType=AST.TTuple [childType] in let child=TempId 0 and alias=TempId 1 and outer=TempId 2 in
 let body=Let (child,call "makeChild" [],Let (alias,(if typed then TypedAtom (Var child,childType) else Atom (Var child)),Let (outer,TupleAlloc [Var alias],Return (Var outer)))) in
 let transformed=transform (aggregateContext "wrapChild" [] outerType) (functionWith "wrapChild" [] outerType body) in transformed,child,alias
let testReturnedAggregateTransfersOwnedValueThroughAlias ()=
 let body,child,alias=aliasTransfer false in
 let* ()=require (not (hasRefCountIncForTemp alias body)) "Returned aggregate should adopt owned value through a pure alias without retaining it" in
 require (not (hasRefCountDecForTemp child body)) "Returned aggregate alias transfer should remove the original owner's pending release"
let testReturnedAggregateTransfersOwnedValueThroughTypedAlias ()=
 let body,child,alias=aliasTransfer true in
 let* ()=require (not (hasRefCountIncForTemp alias body)) "Returned aggregate should adopt owned value through a typed alias without retaining it" in
 require (not (hasRefCountDecForTemp child body)) "Returned aggregate typed-alias transfer should remove the original owner's pending release"
let testReturnedAggregateRetainsOwnershipProducingStreamAlias ()=
 let streamType=AST.TStream AST.TInt64 in let outerType=AST.TTuple [streamType] in
 let ptr=TempId 0 and stream=TempId 1 and outer=TempId 2 in
 let body=Let (ptr,RawAlloc (int 32L),Let (stream,TypedAtom (Var ptr,streamType),Let (outer,TupleAlloc [Var stream],Return (Var outer)))) in
 let ctx=context (functionRegistry ["wrapStream",AST.TFunction ([],outerType)]) in
 require (hasRefCountIncForTemp stream (transform ctx (functionWith "wrapStream" [] outerType body))) "RawPtr-to-Stream creates ownership at the aggregate boundary and must retain the Stream alias"
let testReturnedAggregateTransfersOwnedValueAfterBorrowedUse ()=
 let outerType=AST.TTuple [childType;AST.TInt64] in let child=TempId 0 and alias=TempId 1 and inspected=TempId 2 and outer=TempId 3 in
 let ctx=context (functionRegistry ["makeChild",AST.TFunction ([],childType);"inspectChild",AST.TFunction ([childType],AST.TInt64);"wrapChild",AST.TFunction ([],outerType)]) in
 let body=Let (child,call "makeChild" [],Let (alias,Atom (Var child),Let (inspected,call "inspectChild" [Var alias],Let (outer,TupleAlloc [Var alias;Var inspected],Return (Var outer))))) in
 let body=transform ctx (functionWith "wrapChild" [] outerType body) in
 let* ()=require (not (hasRefCountIncForTemp alias body)) "Returned aggregate should adopt an owned value after its earlier borrowed uses" in
 require (not (hasRefCountDecForTemp child body)) "Borrowed uses before returned aggregate transfer should not preserve the owner's release"
let testExplicitReleaseBlocksLaterAggregateTransfer ()=
 let outerType=AST.TTuple [childType] in let child=TempId 0 and release=TempId 1 and outer=TempId 2 in
 let body=Let (child,call "makeChild" [],Let (release,RefCountDec (Var child,8,GenericHeap,None),Let (outer,TupleAlloc [Var child],Return (Var outer)))) in
 require (hasRefCountIncForTemp child (transform (aggregateContext "wrapChild" [] outerType) (functionWith "wrapChild" [] outerType body))) "An explicit release must block ownership transfer at a later aggregate use"
let branchTransfer every=
 let outerType=AST.TTuple [childType] in let condition=TempId 0 and child=TempId 1 in
 let thenOuter,elseOuter=if every then TempId 2,TempId 3 else TempId 3,TempId 4 in
 let branch target value=Let (target,TupleAlloc [Var value],Return (Var target)) in
 let other=if every then branch elseOuter child else Let (TempId 2,call "makeChild" [],branch elseOuter (TempId 2)) in
 let body=Let (child,call "makeChild" [],If (Var condition,branch thenOuter child,other)) in
 transform (aggregateContext "wrapChild" [AST.TBool] outerType) (functionWith "wrapChild" [param condition AST.TBool] outerType body),child
let testReturnedAggregateTransfersOwnedValueAcrossBranches ()=
 let body,child=branchTransfer true in
 let* ()=require (not (hasRefCountIncForTemp child body)) "Every returning branch should adopt the owned value without retaining it" in
 require (not (hasRefCountDecForTemp child body)) "Branch-complete aggregate transfer should remove the original owner's release"
let testReturnedAggregateRequiresEveryBranchToTransferOwnedValue ()=
 let body,child=branchTransfer false in
 require (hasRefCountIncForTemp child body) "A value absent from one returning branch must keep its retain in the branch that packages it"
let testReturnedAggregateTransfersNestedOwnedAliases ()=
 let innerType=AST.TTuple [childType] in let outerType=AST.TTuple [innerType] in
 let child=TempId 0 and childAlias=TempId 1 and inner=TempId 2 and innerAlias=TempId 3 and outer=TempId 4 in
 let body=Let (child,call "makeChild" [],Let (childAlias,Atom (Var child),Let (inner,TupleAlloc [Var childAlias],Let (innerAlias,Atom (Var inner),Let (outer,TupleAlloc [Var innerAlias],Return (Var outer)))))) in
 let body=transform (aggregateContext "wrapChild" [] outerType) (functionWith "wrapChild" [] outerType body) in
 let* ()=require (not (hasRefCountIncForTemp childAlias body || hasRefCountIncForTemp innerAlias body)) "Nested returned aggregates should adopt owned values through pure aliases without retaining them" in
 require (not (hasRefCountDecForTemp child body || hasRefCountDecForTemp inner body)) "Nested returned aggregate alias transfer should remove each original owner's pending release"
let testReturnedAggregateDoesNotTransferDuplicatedAliases ()=
 let outerType=AST.TTuple [childType;childType] in let child=TempId 0 and first=TempId 1 and second=TempId 2 and outer=TempId 3 in
 let body=Let (child,call "makeChild" [],Let (first,Atom (Var child),Let (second,TypedAtom (Var child,childType),Let (outer,TupleAlloc [Var first;Var second],Return (Var outer))))) in
 let body=transform (aggregateContext "duplicateChild" [] outerType) (functionWith "duplicateChild" [] outerType body) in
 let retainCount=countRefCountIncsForTemps (TS.of_list [first;second]) body in
 require (retainCount=1 && not (hasRefCountDecForTemp child body)) (Printf.sprintf "Duplicated aliases should transfer one owned edge and retain one shared edge; got %d retains" retainCount)
let testStaticStringBindingSkipsNoOpRcTraffic ()=
 let string=TempId 0 and result=TempId 1 in let body=Let (string,Atom (StringLiteral "static"),Let (result,Atom (int 1L),Return (Var result))) in
 require (not (hasStringReleaseForTemp string (transform (context FunctionIdMap.empty) (functionWith "staticString" [] AST.TInt64 body)))) "Static string binding should not emit a runtime no-op release"
let testKnownEmptyListBindingSkipsNoOpRcTraffic ()=
 let list=TempId 0 and result=TempId 1 in let body=Let (list,TypedAtom (int 0L,AST.TList AST.TInt64),Let (result,Atom (int 1L),Return (Var result))) in
 require (not (hasRefCountDecForTemp list (transform (context FunctionIdMap.empty) (functionWith "emptyList" [] AST.TInt64 body)))) "Known empty-list binding should not emit a runtime no-op release"
let testAggregateSkipsRetainsForKnownNonRcSentinels ()=
 let listType=AST.TList AST.TInt64 in let resultType=AST.TTuple [AST.TString;listType] in
 let string=TempId 0 and list=TempId 1 and result=TempId 2 in
 let body=Let (string,Atom (StringLiteral "static"),Let (list,TypedAtom (int 0L,listType),Let (result,TupleAlloc [Var string;Var list],Return (Var result)))) in
 let body=transform (context FunctionIdMap.empty) (functionWith "sentinelTuple" [] resultType body) in
 let* ()=require (not (hasStringRetainForTemp string body)) "Aggregate should not retain a known static string field" in
 require (not (hasRefCountIncForTemp list body)) "Aggregate should not retain a known empty-list field"
let testAggregateSkipsRetainForConditionalStaticString ()=
 let resultType=AST.TTuple [AST.TString] in let condition=TempId 0 and string=TempId 1 and result=TempId 2 in
 let body=Let (string,IfValue (Var condition,StringLiteral "first",StringLiteral "second"),Let (result,TupleAlloc [Var string],Return (Var result))) in
 let body=transform (context FunctionIdMap.empty) (functionWith "conditionalStaticString" [param condition AST.TBool] resultType body) in
 require (not (hasStringRetainForTemp string body)) "Aggregate should not retain a conditional whose alternatives are both static strings"
let testNonSelfTailCallDoesNotLeaveDecAfterTailCall ()=
 let p=TempId 0 and tuple=TempId 1 and result=TempId 2 in
 let body=Let (tuple,TupleAlloc [Var p;int 1L],Let (result,TailCall (TestIds.functionIdForName "callee",[Var p]),Return (Var result))) in
 let ctx=context (functionRegistry ["callee",AST.TFunction ([AST.TInt64],AST.TInt64);"caller",AST.TFunction ([AST.TInt64],AST.TInt64)]) in
 let func=functionWith "caller" [param p AST.TInt64] AST.TInt64 body in
 require (not (hasDecAfterNonSelfTailCall func.ANF.id (transform ctx func))) "Found RefCountDec after non-self TailCall; dec should execute before tailcall"
let testAliasReturnMaterializesOwnershipEvenIfFunctionMarkedBorrowed ()=
 let nodeType=AST.TList AST.TInt64 in let node=TempId 0 and index=TempId 1 and child=TempId 2 in
 let body=Let (child,RawGet (Var node,int 0L,Some nodeType),Return (Var child)) in
 let func={ (functionWith "Darklang.Stdlib.List.__node2GetChild_i64" [param node nodeType;param index AST.TInt64] nodeType body) with returnOwnership=BorrowedReturn } in
 require (hasRefCountIncForTemp child (transform (context FunctionIdMap.empty) func)) "Alias return should materialize ownership with RefCountInc even when function is marked BorrowedReturn"
let elaborateSingleFieldRecordReuseWithTypes additionalTypes typeName fieldName fieldType=
 let descriptor:ANF.recordDescriptor={ANF.sourceTypeName=typeName;runtimeTypeName=typeName;typeArgs=[];fields=[fieldName,fieldType;"count",AST.TInt64];valueType=AST.TRecord (typeName,[])} in
 let recordType=AST.TRecord (descriptor.ANF.runtimeTypeName,[]) in
 let makeOldName="makeOld"^typeName and fixtureName="reuse"^typeName in
 let replacement=TempId 0 and old=TempId 1 and source=TempId 2 and result=TempId 3 in
 let info:TypeRegistries.recordTypeInfo={TypeRegistries.typeParams=[];fields=descriptor.ANF.fields} in
 let ctx={ (context (functionRegistry [makeOldName,AST.TFunction ([],fieldType);fixtureName,AST.TFunction ([fieldType],recordType)])) with RcTypeFacts.typeReg=M.of_list ((typeName,info)::additionalTypes) } in
 let body=Let (old,call makeOldName [],Let (source,RecordAlloc (descriptor,[Var old;int 1L]),Let (result,RecordReuse (descriptor,descriptor,Var source,[Var replacement;int 2L]),Return (Var result)))) in
 transform ctx (functionWith fixtureName [param replacement fieldType] recordType body),replacement,source
let elaborateSingleFieldRecordReuse typeName fieldName fieldType=elaborateSingleFieldRecordReuseWithTypes [] typeName fieldName fieldType
let hasOrderedReset retain release index target replacement source expression=
 let rec loop retained released=function
 |Let (_,value,rest) when retain replacement value->loop true released rest
 |Let (field,RecordGet (_,Var id,i),rest) when retained && id=source && i=index->(match rest with Let (_,value,after) when release field value->loop retained true after|_->false)
 |Let (_,RecordReuse (_,descriptor,Var id,_),_) when id=source && target descriptor->retained && released
 |Let (_,_,rest)->loop retained released rest
 |Join (_,a,b)|If (_,a,b)->loop retained released a || loop retained released b
 |Return _|Jump _->false in loop false false expression
let retainString id=function RefCountIncString (Var other) when id=other->true|_->false
let releaseString id=function RefCountDecString (Var other) when id=other->true|_->false
let retainGeneric id=function RefCountInc (Var other,_,_,_) when id=other->true|_->false
let releaseGeneric typ recursive id=function
 |RefCountDec (Var other,_,_,Some metadata) when id=other && metadata.MemoryModel.sourceType=Some typ->
   (match metadata.MemoryModel.releasePlan,recursive with Some _,None->true|Some plan,Some required->MemoryPlanning.SemanticTypeSet.mem required (MemoryPlanning.recursiveReleaseTypes plan)|_->false)
 |_->false
let bodyDiagnostic prefix body=prefix^ANFTestFormatting.expr body
let testRecordReuseRetainsReplacementBeforeReleasingOldChild ()=
 let body,replacement,source=elaborateSingleFieldRecordReuse "ManagedReuseRecord" "label" AST.TString in
 if hasOrderedReset retainString releaseString 0 (fun _->true) replacement source body then Ok () else Error (bodyDiagnostic "Expected retain, displaced-child release, then record reuse; got " body)
let testCompositeRecordReuseCarriesRecursiveReleasePlan ()=
 let listType=AST.TList AST.TInt64 in let body,replacement,source=elaborateSingleFieldRecordReuse "CompositeReuseRecord" "items" listType in
 if hasOrderedReset retainGeneric (releaseGeneric listType None) 0 (fun _->true) replacement source body then Ok () else Error (bodyDiagnostic "Expected composite reuse to carry ordered recursive cleanup; got " body)
let testNestedRecordReuseCarriesRecursiveReleasePlan ()=
 let name="NestedReuseInner" in let typ=AST.TRecord (name,[]) in let info:TypeRegistries.recordTypeInfo={TypeRegistries.typeParams=[];fields=["items",AST.TList AST.TInt64]} in
 let body,replacement,source=elaborateSingleFieldRecordReuseWithTypes [name,info] "NestedReuseOuter" "inner" typ in
 if hasOrderedReset retainGeneric (releaseGeneric typ None) 0 (fun _->true) replacement source body then Ok () else Error (bodyDiagnostic "Expected nested record reuse to carry ordered recursive cleanup; got " body)
let testRecursiveRecordReuseCarriesTypedBackEdgeReleasePlan ()=
 let name="RecursiveReuseNode" in let typ=AST.TRecord (name,[]) in let children=AST.TList typ in let info:TypeRegistries.recordTypeInfo={TypeRegistries.typeParams=[];fields=["value",AST.TInt64;"children",children]} in
 let body,replacement,source=elaborateSingleFieldRecordReuseWithTypes [name,info] "RecursiveReuseContainer" "children" children in
 if hasOrderedReset retainGeneric (releaseGeneric children (Some typ)) 0 (fun _->true) replacement source body then Ok () else Error (bodyDiagnostic "Expected recursive record reuse to carry a typed back-edge cleanup; got " body)
let sumReuse recursive=
 let name=if recursive then "RecursiveReuseChain" else "ReuseChoice" in let sumType=AST.TSum (name,[]) in
 let oldPayloadType=if recursive then AST.TList sumType else AST.TList AST.TInt64 in let newPayloadType=if recursive then oldPayloadType else AST.TString in
 let descriptor:ANF.recordDescriptor={ANF.sourceTypeName=name;runtimeTypeName=name;typeArgs=[];fields=["$tag",AST.TInt64;"$payload",oldPayloadType];valueType=sumType} in
 let target={descriptor with ANF.fields=["$tag",AST.TInt64;"$payload",newPayloadType]} in
 let makeOldName=if recursive then "makeOldRecursiveReuseChain" else "makeOldReuseChoice" in let fixtureName=if recursive then "reuseRecursiveReuseChain" else "reuseChoice" in
 let replacement=TempId 0 and old=TempId 1 and source=TempId 2 and result=TempId 3 in
 let shape:MemoryModel.rcSumShapeInfo={MemoryModel.typeParams=[];payloads=(if recursive then [0,Some oldPayloadType] else [0,Some oldPayloadType;1,Some newPayloadType]);unaryPayloadTags=(if recursive then MemoryModel.IntSet.empty else MemoryModel.IntSet.of_list [0;1])} in
 let ctx={ (context (functionRegistry [makeOldName,AST.TFunction ([],oldPayloadType);fixtureName,AST.TFunction ([newPayloadType],sumType)])) with RcTypeFacts.sumShapeReg=M.singleton name shape } in
 let body=Let (old,call makeOldName [],Let (source,RecordAlloc (descriptor,[int 0L;Var old]),Let (result,RecordReuse (descriptor,target,Var source,[int (if recursive then 0L else 1L);Var replacement]),Return (Var result)))) in
 transform ctx (functionWith fixtureName [param replacement newPayloadType] sumType body),replacement,source,target,oldPayloadType,sumType
let testBoxedSumReuseReleasesSourceVariantBeforeOverwrite ()=
 let body,replacement,source,target,oldPayloadType,_=sumReuse false in
 if hasOrderedReset retainString (releaseGeneric oldPayloadType None) 1 ((=) target) replacement source body then Ok () else Error (bodyDiagnostic "Expected boxed-sum payload release before variant overwrite; got " body)
let testRecursiveBoxedSumReuseCarriesTypedBackEdgeReleasePlan ()=
 let body,replacement,source,_,payloadType,sumType=sumReuse true in
 if hasOrderedReset retainGeneric (releaseGeneric payloadType (Some sumType)) 1 (fun _->true) replacement source body then Ok () else Error (bodyDiagnostic "Expected recursive boxed-sum reuse to carry typed back-edge cleanup; got " body)
