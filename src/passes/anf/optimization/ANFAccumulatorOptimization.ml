(* ANFAccumulatorOptimization.ml - Lower eligible recursion through scalar accumulators or constructor destinations. *)
[@@@warning "-4-30"]
open ANF
module TS = ANFEffects.TempSet
module FS = SpecializationIdentity.FunctionSet
type siblingAddition = {firstCallId : tempId; firstArgs : atom list; secondCallId : tempId; secondArgs : atom list; resultId : tempId}
(*
   A direct recursive call whose native-integer result is immediately
   multiplied by a same-width parameter or literal. The factor restriction makes moving the wrapped
   multiply ahead of the call observably safe without effect analysis.
*)
type wrappedMultiplication = {callId : tempId; callArgs : atom list; factor : atom; resultId : tempId}
type wrappedSubtraction = {callId : tempId; callArgs : atom list; subtrahend : atom; resultId : tempId}
let nativeIntegerTypeName = function AST.TInt8 -> Some "Int8" | AST.TInt16 -> Some "Int16" | AST.TInt32 -> Some "Int32" | AST.TInt64 -> Some "Int64" | AST.TUInt8 -> Some "UInt8" | AST.TUInt16 -> Some "UInt16" | AST.TUInt32 -> Some "UInt32" | AST.TUInt64 -> Some "UInt64" | _ -> None
let integerLiteral typ value = match typ with
 | AST.TInt8 -> IntLiteral (Int8 (let bits=value land 255 in if bits >= 128 then bits-256 else bits))
 | AST.TInt16 -> IntLiteral (Int16 (let bits=value land 65535 in if bits >= 32768 then bits-65536 else bits))
 | AST.TInt32 -> IntLiteral (Int32 (Int32.of_int value)) | AST.TInt64 -> IntLiteral (Int64 (Int64.of_int value))
 | AST.TUInt8 -> IntLiteral (UInt8 (value land 255)) | AST.TUInt16 -> IntLiteral (UInt16 (value land 65535)) | AST.TUInt32 -> IntLiteral (UInt32 (Int64.logand (Int64.of_int value) 0xffffffffL)) | AST.TUInt64 -> IntLiteral (UInt64 (Int64.of_int value))
 | _ -> Crash.crash ("Tail-recursion accumulator requested a non-native integer literal for " ^ StructuralFormat.semanticType typ)
let isIntegerBinary primitive = function Prim (op,left,right) when op=primitive -> Some (left,right) | _ -> None
(*
   A recursive list result immediately prepended with one already-evaluated
   value. The public push wrapper is retained so the rewrite uses the same
   typed skew-list constructor as the source expression.
*)
type wrappedListPrepend = {callId : tempId; callArgs : atom list; pushName : AST.functionId; value : atom; resultId : tempId}
type constructorLayerKind = TupleLayer of atom list | RecordLayer of recordDescriptor * atom list
type constructorLayer = {resultId : tempId; kind : constructorLayerKind; holeIndex : int; resultType : AST.semanticType; childType : AST.semanticType}
type constructorContext = {callId : tempId [@warning "-69"]; callArgs : atom list; prefix : (tempId * cExpr) list; layersInsideOut : constructorLayer list}
let tryLinearBindings expr =
 let rec collect reversed = function Let (tid,cexpr,body) -> collect ((tid,cexpr)::reversed) body | Return atom -> Some (List.rev reversed,atom) | Jump _ | Join _ | If _ -> None in collect [] expr
(*
   Recognize a complete linear sibling-recursion arm. Requiring exactly two
   self calls and a final addition keeps effect order and the rewrite boundary
   explicit; the function-level gate rejects any recursion outside this shape.
*)
let trySiblingAddition funcName expr : siblingAddition option =
 match tryLinearBindings expr with
 | Some (bindings,Var returnedId) -> (match List.rev bindings with
  | (resultId,operation)::_ when resultId=returnedId ->
   let calls = List.filter_map (fun (tid,cexpr) -> match cexpr with Call (target,args) when target=funcName -> Some (tid,args) | _ -> None) bindings in
   (match isIntegerBinary Add operation,calls with
   | Some (Var leftId,Var rightId),[(firstCallId,firstArgs);(secondCallId,secondArgs)] when (leftId=firstCallId && rightId=secondCallId) || (leftId=secondCallId && rightId=firstCallId) -> Some {firstCallId;firstArgs;secondCallId;secondArgs;resultId}
   | _ -> None)
  | _ -> None)
 | _ -> None
let isIntegerParameterOrLiteral params = function IntLiteral _ -> true | Var tid -> TS.mem tid params | _ -> false
(*
   Recognize one direct self call wrapped by a final Int64 multiplication.
   Every preceding binding must be pure and first-order, which rejects managed
   allocations, effects, indirect calls, and unmodelled control-flow values.
*)
let tryWrappedMultiplication funcName integerParams expr : wrappedMultiplication option =
 match tryLinearBindings expr with
 | Some (bindings,Var returnedId) ->
  let isAllowedBinding = function _,Atom _ | _,TypedAtom _ | _,Prim _ | _,UnaryPrim _ -> true | _,Call (target,_) when target=funcName -> true | _ -> false in
  (match List.rev bindings with
  | (resultId,operation)::_ when resultId=returnedId ->
   let calls = List.filter_map (fun (tid,cexpr) -> match cexpr with Call (target,args) when target=funcName -> Some (tid,args) | _ -> None) bindings in
   let noOtherCalls = List.for_all isAllowedBinding bindings in
   (match isIntegerBinary Mul operation,calls with
   | Some (left,right),[(callId,callArgs)] when noOtherCalls ->
    (match left,right with
    | Var id,factor when id=callId && isIntegerParameterOrLiteral integerParams factor -> Some {callId;callArgs;factor;resultId}
    | factor,Var id when id=callId && isIntegerParameterOrLiteral integerParams factor -> Some {callId;callArgs;factor;resultId}
    | _ -> None)
   | _ -> None)
  | _ -> None)
 | _ -> None
let tryWrappedSubtraction funcName integerParams expr : wrappedSubtraction option =
 match tryLinearBindings expr with
 | Some (bindings,Var returnedId) -> (match List.rev bindings with
  | (resultId,operation)::_ when resultId=returnedId ->
   let calls = List.filter_map (fun (tid,cexpr) -> match cexpr with Call (target,args) when target=funcName -> Some (tid,args) | _ -> None) bindings in
   let allowed = List.for_all (fun (_,cexpr) -> match cexpr with Atom _ | TypedAtom _ | Prim _ | UnaryPrim _ -> true | Call (target,_) when target=funcName -> true | _ -> false) bindings in
   (match isIntegerBinary Sub operation,calls with Some (Var id,subtrahend),[(callId,callArgs)] when id=callId && allowed && isIntegerParameterOrLiteral integerParams subtrahend -> Some {callId;callArgs;subtrahend;resultId} | _ -> None)
  | _ -> None)
 | _ -> None
let tryWrappedListPrepend isListPush funcName expr : wrappedListPrepend option =
 match tryLinearBindings expr with
 | Some (bindings,Var returnedId) -> (match List.rev bindings with
  | (resultId,Call (pushName,[Var listId;value]))::_ when resultId=returnedId && isListPush pushName ->
   let calls = List.filter_map (fun (tid,cexpr) -> match cexpr with Call (target,args) when target=funcName -> Some (tid,args) | _ -> None) bindings in
   let allowed = List.for_all (fun (tid,cexpr) -> match cexpr with
   | Atom _ | TypedAtom _ | Prim _ | UnaryPrim _ -> true
   | Call (target,_) when target=funcName -> true
   | Call (target,[Var sourceId;_]) -> tid=resultId && target=pushName && sourceId=listId
   | _ -> false) bindings in
   (match calls with [(callId,callArgs)] when callId=listId && allowed -> Some {callId;callArgs;pushName;value;resultId} | _ -> None)
  | _ -> None)
 | _ -> None
let tryConstructorContext funcName returnType expr : constructorContext option =
 let allocationFields = function TupleAlloc fields -> Some (TupleLayer fields,fields) | RecordAlloc (descriptor,fields) -> Some (RecordLayer (descriptor,fields),fields) | _ -> None in
 let childType kind holeIndex expectedResultType isDirectCall = match kind with
 | RecordLayer (descriptor,_) -> Option.map snd (if holeIndex < 0 then None else List.nth_opt descriptor.fields holeIndex)
 | TupleLayer fields -> (match expectedResultType,isDirectCall,holeIndex,List.length fields with AST.TSum _,true,1,2 -> Some returnType | _ -> None) in
 match tryLinearBindings expr with
 | Some (bindings,Var returnedId) ->
  let indexed = List.mapi (fun index binding -> index,binding) bindings in
  let calls = List.filter_map (fun (index,(tid,cexpr)) -> match cexpr with Call (target,args) when target=funcName -> Some (index,tid,args) | _ -> None) indexed in
  (match calls with
  | [(callIndex,callId,callArgs)] ->
   let rec drop n xs = if n=0 then xs else match xs with _::rest -> drop (n-1) rest | [] -> invalid_arg "List.skip" in
   let suffix = drop (callIndex+1) bindings in
   let rec collect previousId remaining layers = match remaining with
   | [] -> (match layers with (outerId,_,_)::_ when outerId=returnedId -> Some (List.rev layers) | _ -> None)
   | (resultId,cexpr)::rest -> Option.bind (allocationFields cexpr) (fun (kind,fields) ->
    let holes = List.filter_map (fun (index,atom) -> match atom with Var id when id=previousId -> Some index | _ -> None) (List.mapi (fun index atom -> index,atom) fields) in
    match holes with [holeIndex] -> collect resultId rest ((resultId,kind,holeIndex)::layers) | _ -> None) in
   Option.bind (collect callId suffix []) (fun rawLayers ->
    let rec assign expectedResult remaining assigned = match remaining with
    | [] -> Some assigned
    | (resultId,kind,holeIndex)::rest -> Option.bind (childType kind holeIndex expectedResult (rest=[])) (fun expectedChild ->
     let layer : constructorLayer = {resultId;kind;holeIndex;resultType=expectedResult;childType=expectedChild} in assign expectedChild rest (layer::assigned)) in
    Option.map (fun layersInsideOut ->
     let rec take n xs = if n=0 then [] else match xs with first::rest -> first::take (n-1) rest | [] -> invalid_arg "List.take" in
     {callId;callArgs;prefix=take callIndex bindings;layersInsideOut}) (assign returnType (List.rev rawLayers) []))
  | _ -> None)
 | _ -> None
let addCount a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let selfCallCount funcName expr =
 let rec count = function Jump _ | Return _ -> 0 | Let (_,cexpr,body) -> let current=match cexpr with Call (target,_) when target=funcName -> 1 | _ -> 0 in addCount current (count body) | Join (_,continuation,entry) -> addCount (count continuation) (count entry) | If (_,yes,no) -> addCount (count yes) (count no) in count expr
let siblingAdditionCount funcName expr =
 let rec count expr = match trySiblingAddition funcName expr with Some _ -> 1 | None -> match expr with Jump _ | Return _ -> 0 | Let (_,_,body) -> count body | Join (_,continuation,entry) -> addCount (count continuation) (count entry) | If (_,yes,no) -> addCount (count yes) (count no) in count expr
let wrappedMultiplicationCount funcName params expr =
 let rec count expr = match tryWrappedMultiplication funcName params expr with Some _ -> 1 | None -> match expr with Jump _ | Return _ -> 0 | Let (_,_,body) -> count body | Join (_,continuation,entry) -> addCount (count continuation) (count entry) | If (_,yes,no) -> addCount (count yes) (count no) in count expr
let wrappedSubtractionCount funcName params expr =
 let rec count expr = match tryWrappedSubtraction funcName params expr with Some _ -> 1 | None -> match expr with Jump _ | Return _ -> 0 | Let (_,_,body) -> count body | Join (_,continuation,entry) -> addCount (count continuation) (count entry) | If (_,yes,no) -> addCount (count yes) (count no) in count expr
let wrappedListPrependCount isListPush funcName expr =
 let rec count expr = match tryWrappedListPrepend isListPush funcName expr with Some _ -> 1 | None -> match expr with Jump _ | Return _ -> 0 | Let (_,_,body) -> count body | Join (_,continuation,entry) -> addCount (count continuation) (count entry) | If (_,yes,no) -> addCount (count yes) (count no) in count expr
let listPrependCallCount isListPush expr =
 let rec count = function Jump _ | Return _ -> 0 | Let (_,cexpr,body) -> let current=match cexpr with Call (target,_) when isListPush target -> 1 | _ -> 0 in addCount current (count body) | Join (_,continuation,entry) -> addCount (count continuation) (count entry) | If (_,yes,no) -> addCount (count yes) (count no) in count expr
let constructorContextCount funcName returnType expr =
 let rec count expr = match tryConstructorContext funcName returnType expr with Some _ -> 1 | None -> match expr with Jump _ | Return _ -> 0 | Let (_,_,body) -> count body | Join (_,continuation,entry) -> addCount (count continuation) (count entry) | If (_,yes,no) -> addCount (count yes) (count no) in count expr
let rebuildBindings bindings body = List.fold_right (fun (tid,cexpr) body -> Let (tid,cexpr,body)) bindings body
let transformSiblingAddition helperName accumulatorId zero varGen (sibling : siblingAddition) bindings =
 let nextAccumulatorId,next = freshVar varGen in
 let rec rewrite = function
 | [] -> Return (Var sibling.secondCallId)
 | (tid,_)::rest when tid=sibling.resultId -> rewrite rest
 | (tid,Call _)::rest when tid=sibling.firstCallId -> Let (tid,Call (helperName,sibling.firstArgs @ [zero]),rewrite rest)
 | (tid,Call _)::rest when tid=sibling.secondCallId -> Let (nextAccumulatorId,Prim (Add,Var accumulatorId,Var sibling.firstCallId),Let (tid,Call (helperName,sibling.secondArgs @ [Var nextAccumulatorId]),rewrite rest))
 | binding::rest -> rebuildBindings [binding] (rewrite rest) in rewrite bindings,next
let rec transformAccumulatorBody funcName helperName accumulatorId zero varGen expr =
 match trySiblingAddition funcName expr,tryLinearBindings expr with
 | Some sibling,Some (bindings,_) -> transformSiblingAddition helperName accumulatorId zero varGen sibling bindings
 | _ -> match expr with
  | Jump _ -> expr,varGen
  | Join (parameter,continuation,entry) -> let body,next=transformAccumulatorBody funcName helperName accumulatorId zero varGen continuation in let entry',final=transformAccumulatorBody funcName helperName accumulatorId zero next entry in Join (parameter,body,entry'),final
  | Return atom -> let result,next=freshVar varGen in Let (result,Prim (Add,Var accumulatorId,atom),Return (Var result)),next
  | Let (tid,cexpr,body) -> let body',next=transformAccumulatorBody funcName helperName accumulatorId zero varGen body in Let (tid,cexpr,body'),next
  | If (condition,yes,no) -> let yes,next=transformAccumulatorBody funcName helperName accumulatorId zero varGen yes in let no,final=transformAccumulatorBody funcName helperName accumulatorId zero next no in If (condition,yes,no),final
let transformWrappedMultiplication helperName accumulatorId varGen (wrapped : wrappedMultiplication) bindings =
 let nextAccumulatorId,next=freshVar varGen in
 let rec rewrite = function [] -> Return (Var wrapped.callId) | (tid,_)::rest when tid=wrapped.resultId -> rewrite rest | (tid,Call _)::rest when tid=wrapped.callId -> Let (nextAccumulatorId,Prim (Mul,Var accumulatorId,wrapped.factor),Let (tid,Call (helperName,wrapped.callArgs @ [Var nextAccumulatorId]),rewrite rest)) | binding::rest -> rebuildBindings [binding] (rewrite rest) in rewrite bindings,next
let rec transformMultiplicationAccumulatorBody funcName params helperName accumulatorId varGen expr =
 match tryWrappedMultiplication funcName params expr,tryLinearBindings expr with
 | Some wrapped,Some (bindings,_) -> transformWrappedMultiplication helperName accumulatorId varGen wrapped bindings
 | _ -> match expr with
  | Jump _ -> expr,varGen
  | Join (parameter,continuation,entry) -> let body,next=transformMultiplicationAccumulatorBody funcName params helperName accumulatorId varGen continuation in let entry',final=transformMultiplicationAccumulatorBody funcName params helperName accumulatorId next entry in Join (parameter,body,entry'),final
  | Return atom -> let result,next=freshVar varGen in Let (result,Prim (Mul,Var accumulatorId,atom),Return (Var result)),next
  | Let (tid,cexpr,body) -> let body',next=transformMultiplicationAccumulatorBody funcName params helperName accumulatorId varGen body in Let (tid,cexpr,body'),next
  | If (condition,yes,no) -> let yes,next=transformMultiplicationAccumulatorBody funcName params helperName accumulatorId varGen yes in let no,final=transformMultiplicationAccumulatorBody funcName params helperName accumulatorId next no in If (condition,yes,no),final
let transformWrappedSubtraction helperName accumulatorId varGen (wrapped : wrappedSubtraction) bindings =
 let nextAccumulatorId,next=freshVar varGen in
 let rec rewrite = function [] -> Return (Var wrapped.callId) | (tid,_)::rest when tid=wrapped.resultId -> rewrite rest | (tid,Call _)::rest when tid=wrapped.callId -> Let (nextAccumulatorId,Prim (Sub,Var accumulatorId,wrapped.subtrahend),Let (tid,Call (helperName,wrapped.callArgs @ [Var nextAccumulatorId]),rewrite rest)) | binding::rest -> rebuildBindings [binding] (rewrite rest) in rewrite bindings,next
let rec transformSubtractionAccumulatorBody funcName params helperName accumulatorId varGen expr =
 match tryWrappedSubtraction funcName params expr,tryLinearBindings expr with
 | Some wrapped,Some (bindings,_) -> transformWrappedSubtraction helperName accumulatorId varGen wrapped bindings
 | _ -> match expr with
  | Jump _ -> expr,varGen
  | Join (parameter,continuation,entry) -> let body,next=transformSubtractionAccumulatorBody funcName params helperName accumulatorId varGen continuation in let entry',final=transformSubtractionAccumulatorBody funcName params helperName accumulatorId next entry in Join (parameter,body,entry'),final
  | Return atom -> let result,next=freshVar varGen in Let (result,Prim (Add,Var accumulatorId,atom),Return (Var result)),next
  | Let (tid,cexpr,body) -> let body',next=transformSubtractionAccumulatorBody funcName params helperName accumulatorId varGen body in Let (tid,cexpr,body'),next
  | If (condition,yes,no) -> let yes,next=transformSubtractionAccumulatorBody funcName params helperName accumulatorId varGen yes in let no,final=transformSubtractionAccumulatorBody funcName params helperName accumulatorId next no in If (condition,yes,no),final
let transformWrappedListPrepend helperName accumulatorId suffixCellId varGen (wrapped : wrappedListPrepend) bindings =
 let nextAccumulatorId,next=freshVar varGen in
 let rec rewrite = function [] -> Return (Var wrapped.callId) | (tid,_)::rest when tid=wrapped.resultId -> rewrite rest | (tid,Call _)::rest when tid=wrapped.callId -> Let (nextAccumulatorId,Call (wrapped.pushName,[Var accumulatorId;wrapped.value]),Let (tid,Call (helperName,wrapped.callArgs @ [Var nextAccumulatorId;Var suffixCellId]),rewrite rest)) | binding::rest -> rebuildBindings [binding] (rewrite rest) in rewrite bindings,next
let rec transformListAccumulatorBody isListPush funcName helperName listType accumulatorId suffixCellId varGen expr =
 match tryWrappedListPrepend isListPush funcName expr,tryLinearBindings expr with
 | Some wrapped,Some (bindings,_) -> transformWrappedListPrepend helperName accumulatorId suffixCellId varGen wrapped bindings
 | _ -> match expr with
  | Jump _ -> expr,varGen
  | Join (parameter,continuation,entry) -> let body,next=transformListAccumulatorBody isListPush funcName helperName listType accumulatorId suffixCellId varGen continuation in let entry',final=transformListAccumulatorBody isListPush funcName helperName listType accumulatorId suffixCellId next entry in Join (parameter,body,entry'),final
  | Return atom -> let store,next=freshVar varGen in Let (store,RawSlotInit (Var suffixCellId,IntLiteral (Int64 0L),atom,listType),Return (Var accumulatorId)),next
  | Let (tid,cexpr,body) -> let body',next=transformListAccumulatorBody isListPush funcName helperName listType accumulatorId suffixCellId varGen body in Let (tid,cexpr,body'),next
  | If (condition,yes,no) -> let yes,next=transformListAccumulatorBody isListPush funcName helperName listType accumulatorId suffixCellId varGen yes in let no,final=transformListAccumulatorBody isListPush funcName helperName listType accumulatorId suffixCellId next no in If (condition,yes,no),final
let layerWithPlaceholder (layer : constructorLayer) placeholder =
 let replace fields=List.mapi (fun index atom -> if index=layer.holeIndex then placeholder else atom) fields in
 match layer.kind with TupleLayer fields -> TupleAlloc (replace fields) | RecordLayer (descriptor,fields) -> RecordAlloc (descriptor,replace fields)
let buildConstructorLayers incomingDestination existingRoot layersInsideOut varGen =
 let outerToInner=List.rev layersInsideOut in
 let rec build remaining destination rootId reversedBindings vg = match remaining with
 | [] -> (match destination,rootId with Some (rawId,offset),Some root -> Some (root,rawId,offset,List.rev reversedBindings,vg) | _ -> None)
 | (layer : constructorLayer)::rest ->
  let placeholderId,afterPlaceholder=freshVar vg in let rawId,afterRaw=freshVar afterPlaceholder in
  let offset=IntLiteral (Int64 (Int64.of_int32 (Int32.mul (Int32.of_int layer.holeIndex) 8l))) in
  let allocation=layerWithPlaceholder layer (Var placeholderId) in
  let baseBindings=[placeholderId,TypedAtom (IntLiteral (Int64 0L),layer.childType);layer.resultId,allocation;rawId,FixedBlockToRawPtr (Var layer.resultId)] in
  let storeBindings,afterStore=match destination with None -> [],afterRaw | Some (ptr,offset) -> let store,next=freshVar afterRaw in [store,RawSlotInit (ptr,offset,Var layer.resultId,layer.resultType)],next in
  let nextRoot=match destination with None -> Some layer.resultId | Some _ -> rootId in
  build rest (Some (Var rawId,offset)) nextRoot (List.rev (baseBindings @ storeBindings) @ reversedBindings) afterStore in
 match outerToInner with [] -> None | _ -> build outerToInner incomingDestination existingRoot [] varGen
let transformConstructorContext helperName destinationId destinationOffsetId rootId (context : constructorContext) varGen =
 let incomingDestination=match destinationId,destinationOffsetId with Some destination,Some offset -> Some (Var destination,Var offset) | None,None -> None | _ -> Crash.crash "Constructor TRMC destination parameters are incomplete" in
 match buildConstructorLayers incomingDestination rootId context.layersInsideOut varGen with
 | None -> Crash.crash "Constructor TRMC context has no constructor layers"
 | Some (constructedRootId,leafRawId,leafOffset,constructorBindings,afterConstructors) ->
  let leafOffsetId,afterOffset=freshVar afterConstructors in let callResultId,afterCall=freshVar afterOffset in
  let returnedRoot=Option.value ~default:constructedRootId rootId in
  let body=Let (leafOffsetId,Atom leafOffset,Let (callResultId,Call (helperName,context.callArgs @ [leafRawId;Var leafOffsetId;Var returnedRoot]),Return (Var callResultId))) in
  rebuildBindings context.prefix (rebuildBindings constructorBindings body),afterCall
let rec transformConstructorBody funcName returnType helperName destinationId destinationOffsetId rootId varGen expr =
 match tryConstructorContext funcName returnType expr with
 | Some context -> transformConstructorContext helperName (Some destinationId) (Some destinationOffsetId) (Some rootId) context varGen
 | None -> match expr with
  | Jump _ -> expr,varGen
  | Return atom -> let store,next=freshVar varGen in Let (store,RawSlotInit (Var destinationId,Var destinationOffsetId,atom,returnType),Return (Var rootId)),next
  | Let (tid,cexpr,body) -> let body',next=transformConstructorBody funcName returnType helperName destinationId destinationOffsetId rootId varGen body in Let (tid,cexpr,body'),next
  | Join (parameter,continuation,entry) -> let body,next=transformConstructorBody funcName returnType helperName destinationId destinationOffsetId rootId varGen continuation in let entry',final=transformConstructorBody funcName returnType helperName destinationId destinationOffsetId rootId next entry in Join (parameter,body,entry'),final
  | If (condition,yes,no) -> let yes,next=transformConstructorBody funcName returnType helperName destinationId destinationOffsetId rootId varGen yes in let no,final=transformConstructorBody funcName returnType helperName destinationId destinationOffsetId rootId next no in If (condition,yes,no),final
let freshHelperName occupiedNames generatedNames funcName =
 let rec choose suffix = let candidate=funcName ^ "$trmo" ^ (if suffix=0 then "" else string_of_int suffix) in
 if StringOrder.Map.mem candidate occupiedNames || StringOrder.Set.mem candidate generatedNames then choose (addCount suffix 1) else candidate in choose 0
let planTailRecursionModuloHelpers nextFunctionOrdinal functionNames functionIds eligibleFunctions =
 let originals=List.map (fun id -> match FunctionIdMap.tryFind id functionNames with Some name -> id,name | None -> Crash.crash "Accumulator helper source has no function name") (FS.elements eligibleFunctions) |> List.stable_sort (fun (_,a) (_,b) -> StringOrder.compare a b) in
 let reversed,_=List.fold_left (fun (acc,generated) (id,name) -> let helperName=freshHelperName functionIds generated name in (id,helperName)::acc,StringOrder.Set.add helperName generated) ([],StringOrder.Set.empty) originals in
 let helperNames=List.rev reversed in
 let allocated=AST.allocateFunctionIdsFromOrdinal nextFunctionOrdinal (List.to_seq (List.map snd helperNames)) in
 FunctionIdMap.ofList (List.map (fun (id,name) -> let helperId=match StringOrder.Map.find_opt name allocated with Some id -> id | None -> Crash.crash "Accumulator helper identity was not allocated" in id,(name,helperId)) helperNames)
let rec transformConstructorWrapperBody funcName returnType helperName varGen expr =
 match tryConstructorContext funcName returnType expr with
 | Some context -> transformConstructorContext helperName None None None context varGen
 | None -> match expr with
  | Jump _ | Return _ -> expr,varGen
  | Let (tid,cexpr,body) -> let body',next=transformConstructorWrapperBody funcName returnType helperName varGen body in Let (tid,cexpr,body'),next
  | Join (parameter,continuation,entry) -> let body,next=transformConstructorWrapperBody funcName returnType helperName varGen continuation in let entry',final=transformConstructorWrapperBody funcName returnType helperName next entry in Join (parameter,body,entry'),final
  | If (condition,yes,no) -> let yes,next=transformConstructorWrapperBody funcName returnType helperName varGen yes in let no,final=transformConstructorWrapperBody funcName returnType helperName next no in If (condition,yes,no),final
(*
   Lower linear recursion beneath tuple-backed sum constructors and record
   constructors with destination passing. Constructor fields other than the
   recursive path must already be atoms, so no effects move across the call.
*)
let transformTailRecursionModuloFixedConstructors helpers initialVarGen (Program (functions,main)) =
 let reversed,final=List.fold_left (fun (rewritten,varGen) (func : functionDef) ->
  let managedReturn=match func.returnType with AST.TRecord _ | AST.TSum _ -> true | _ -> false in
  let candidate=FunctionIdMap.containsKey func.id helpers && managedReturn in
  let contexts=if candidate then constructorContextCount func.id func.returnType func.body else 0 in
  let calls=if candidate && contexts>0 then selfCallCount func.id func.body else 0 in
  if not candidate || contexts=0 || contexts<>calls then func::rewritten,varGen else
  let helperName,helperId=FunctionIdMap.find func.id helpers in
  let destinationId,afterDestination=freshVar varGen in let destinationOffsetId,afterOffset=freshVar afterDestination in let rootId,afterRoot=freshVar afterOffset in
  let helperBody,afterHelper=transformConstructorBody func.id func.returnType helperId destinationId destinationOffsetId rootId afterRoot func.body in
  let wrapperBody,afterWrapper=transformConstructorWrapperBody func.id func.returnType helperId afterHelper func.body in
  let helper={func with id=helperId;name=helperName;typedParams=func.typedParams @ [{id=destinationId;typ=AST.TInternalRawPtr};{id=destinationOffsetId;typ=AST.TInt64};{id=rootId;typ=func.returnType}];body=helperBody} in
  let wrapper={func with body=wrapperBody} in helper::wrapper::rewritten,afterWrapper) ([],initialVarGen) functions in Program (List.rev reversed,main),final
let transformTailRecursionModuloAddition helpers initialVarGen (Program (functions,main)) =
 let reversed,final=List.fold_left (fun (rewritten,varGen) (func : functionDef) ->
  let candidate=FunctionIdMap.containsKey func.id helpers && Option.is_some (nativeIntegerTypeName func.returnType) in
  let pairs=if candidate then siblingAdditionCount func.id func.body else 0 in let calls=if candidate && pairs>0 then selfCallCount func.id func.body else 0 in
  let eligible=candidate && pairs>0 && calls=Int32.to_int (Int32.mul (Int32.of_int pairs) 2l) in
  if not eligible then func::rewritten,varGen else
  let helperName,helperId=FunctionIdMap.find func.id helpers in let accumulatorId,afterAccumulator=freshVar varGen in
  let helperBody,afterHelper=transformAccumulatorBody func.id helperId accumulatorId (integerLiteral func.returnType 0) afterAccumulator func.body in
  let wrapperResultId,afterWrapper=freshVar afterHelper in
  let helper={func with id=helperId;name=helperName;typedParams=func.typedParams @ [{id=accumulatorId;typ=func.returnType}];body=helperBody} in
  let wrapper={func with body=Let (wrapperResultId,Call (helperId,List.map (fun (param : typedParam) -> Var param.id) func.typedParams @ [integerLiteral func.returnType 0]),Return (Var wrapperResultId))} in
  helper::wrapper::rewritten,afterWrapper) ([],initialVarGen) functions in Program (List.rev reversed,main),final
(*
   Turn direct recursive native-integer multiplication with a pure
   parameter/literal factor into an accumulator helper. Modular machine-word
   multiplication is associative at every supported width.
*)
let transformTailRecursionModuloMultiplication helpers initialVarGen (Program (functions,main)) =
 let reversed,final=List.fold_left (fun (rewritten,varGen) (func : functionDef) ->
  let candidate=FunctionIdMap.containsKey func.id helpers && Option.is_some (nativeIntegerTypeName func.returnType) in
  let params=if candidate then TS.of_list (List.filter_map (fun (param : typedParam) -> if param.typ=func.returnType then Some param.id else None) func.typedParams) else TS.empty in
  let wrapped=if candidate then wrappedMultiplicationCount func.id params func.body else 0 in let calls=if candidate && wrapped>0 then selfCallCount func.id func.body else 0 in
  let eligible=candidate && wrapped>0 && calls=wrapped in if not eligible then func::rewritten,varGen else
  let helperName,helperId=FunctionIdMap.find func.id helpers in let accumulatorId,afterAccumulator=freshVar varGen in
  let helperBody,afterHelper=transformMultiplicationAccumulatorBody func.id params helperId accumulatorId afterAccumulator func.body in let result,afterWrapper=freshVar afterHelper in
  let helper={func with id=helperId;name=helperName;typedParams=func.typedParams @ [{id=accumulatorId;typ=func.returnType}];body=helperBody} in
  let wrapper={func with body=Let (result,Call (helperId,List.map (fun (param : typedParam) -> Var param.id) func.typedParams @ [integerLiteral func.returnType 1]),Return (Var result))} in helper::wrapper::rewritten,afterWrapper) ([],initialVarGen) functions in Program (List.rev reversed,main),final
let transformTailRecursionModuloSubtraction helpers initialVarGen (Program (functions,main)) =
 let reversed,final=List.fold_left (fun (rewritten,varGen) (func : functionDef) ->
  let candidate=FunctionIdMap.containsKey func.id helpers && Option.is_some (nativeIntegerTypeName func.returnType) in
  let params=if candidate then TS.of_list (List.filter_map (fun (param : typedParam) -> if param.typ=func.returnType then Some param.id else None) func.typedParams) else TS.empty in
  let wrapped=if candidate then wrappedSubtractionCount func.id params func.body else 0 in let calls=if candidate && wrapped>0 then selfCallCount func.id func.body else 0 in
  let eligible=candidate && wrapped>0 && calls=wrapped in if not eligible then func::rewritten,varGen else
  let helperName,helperId=FunctionIdMap.find func.id helpers in let accumulatorId,afterAccumulator=freshVar varGen in
  let helperBody,afterHelper=transformSubtractionAccumulatorBody func.id params helperId accumulatorId afterAccumulator func.body in let result,afterWrapper=freshVar afterHelper in
  let helper={func with id=helperId;name=helperName;typedParams=func.typedParams @ [{id=accumulatorId;typ=func.returnType}];body=helperBody} in
  let wrapper={func with body=Let (result,Call (helperId,List.map (fun (param : typedParam) -> Var param.id) func.typedParams @ [integerLiteral func.returnType 0]),Return (Var result))} in helper::wrapper::rewritten,afterWrapper) ([],initialVarGen) functions in Program (List.rev reversed,main),final
(*
   Turn recursive `List.push (self ...) value` construction into a reverse
   accumulator loop and finish with the existing linear `__reverseInto`
   kernel. Every recursive call must have the same constructor boundary.
*)
let transformTailRecursionModuloListConstructors helpers externalFunctions initialVarGen (Program (functions,main)) =
 let externalNamesById=FunctionIdMap.ofList (List.map (fun (name,(func : functionDef)) -> func.id,name) (StringOrder.Map.bindings externalFunctions)) in
 let isListPush id=Option.fold ~none:false ~some:(String.starts_with ~prefix:"Darklang.Stdlib.List.push_") (FunctionIdMap.tryFind id externalNamesById) in
 let replaceAll text pattern replacement =
  let output=Buffer.create (String.length text) in
  let rec loop index=if index=String.length text then Buffer.contents output else
   if index+String.length pattern<=String.length text && String.sub text index (String.length pattern)=pattern then (Buffer.add_string output replacement;loop (index+String.length pattern)) else (Buffer.add_char output text.[index];loop (index+1)) in loop 0 in
 let reversed,final=List.fold_left (fun (rewritten,varGen) (func : functionDef) ->
  let candidate=FunctionIdMap.containsKey func.id helpers && (match func.returnType with AST.TList _ -> true | _ -> false) in
  let wrapped=if candidate then wrappedListPrependCount isListPush func.id func.body else 0 in
  let prependCalls=if candidate && wrapped>0 then listPrependCallCount isListPush func.body else 0 in
  let recursiveCalls=if candidate && wrapped>0 then selfCallCount func.id func.body else 0 in
  let pushName=let rec find expr=match tryWrappedListPrepend isListPush func.id expr with Some wrapped -> Some wrapped.pushName | None -> match expr with
   | Jump _ | Return _ -> None | Let (_,_,body) -> find body
   | Join (_,continuation,entry) -> (match find continuation with Some _ as value -> value | None -> find entry)
   | If (_,yes,no) -> (match find yes with Some _ as value -> value | None -> find no) in if candidate && wrapped>0 then find func.body else None in
  let finishTarget=Option.map (fun id -> let name=match FunctionIdMap.tryFind id externalNamesById with Some name -> name | None -> Crash.crash "List push target has no external function name" in replaceAll name "Darklang.Stdlib.List.push_" "Darklang.Stdlib.List.__reverseInto_") pushName in
  let eligible=match finishTarget with Some target -> candidate && wrapped>0 && recursiveCalls=wrapped && prependCalls=wrapped && StringOrder.Map.mem target externalFunctions | _ -> false in
  if not eligible then func::rewritten,varGen else
  let targetName=match finishTarget with Some target -> target | None -> Crash.crash "Eligible list TRMC function lost its finish target" in
  let target=match StringOrder.Map.find_opt targetName externalFunctions with Some (func : functionDef) -> func.id | None -> Crash.crash "Eligible list TRMC finish target is absent" in
  let helperName,helperId=FunctionIdMap.find func.id helpers in
  let accumulatorId,afterAccumulator=freshVar varGen in let suffixCellId,afterSuffixCell=freshVar afterAccumulator in
  let helperBody,afterHelper=transformListAccumulatorBody isListPush func.id helperId func.returnType accumulatorId suffixCellId afterSuffixCell func.body in
  let emptyId,afterEmpty=freshVar afterHelper in let initialSuffixCellId,afterInitialSuffix=freshVar afterEmpty in let reversedId,afterReversed=freshVar afterInitialSuffix in let suffixId,afterSuffix=freshVar afterReversed in let result,afterWrapper=freshVar afterSuffix in
  let helper={func with id=helperId;name=helperName;typedParams=func.typedParams @ [{id=accumulatorId;typ=func.returnType};{id=suffixCellId;typ=AST.TTuple [func.returnType]}];body=helperBody} in
  let wrapper={func with body=Let (emptyId,TypedAtom (IntLiteral (Int64 0L),func.returnType),Let (initialSuffixCellId,TupleAlloc [Var emptyId],Let (reversedId,Call (helperId,List.map (fun (param : typedParam) -> Var param.id) func.typedParams @ [Var emptyId;Var initialSuffixCellId]),Let (suffixId,TupleGet (Var initialSuffixCellId,0),Let (result,Call (target,[Var reversedId;Var suffixId]),Return (Var result))))))} in
  helper::wrapper::rewritten,afterWrapper) ([],initialVarGen) functions in Program (List.rev reversed,main),final
