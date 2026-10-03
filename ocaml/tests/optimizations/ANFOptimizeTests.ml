(*
   These tests cover optimizer decisions that must agree with the RcShape
   metadata used by ownership insertion and backend helper selection.
*)
(* ANFOptimizeTests.fs - Unit tests for ANF optimizer ownership-sensitive DCE. *)
[@@@warning "-4-42"]
open Dark_compiler
open MemoryModel
open ANF
open ANFConstants
module FS = SpecializationIdentity.FunctionSet
module TM = TempMap
let dceOnlyOptions = {defaultOptimizeOptions with enableConstFolding=false;enableConstProp=false;enableCopyProp=false;enableDCE=true;enableStrengthReduction=false}
let optimizeMain context expr = let Program (_,main)=ANF_Optimize.optimizeProgramWithOptions context dceOnlyOptions (Program ([],expr)) in main
let emptyContext : optimizeContext = {typeReg=StringOrder.Map.empty;recordTypeParams=StringOrder.Map.empty;sumShapeReg=StringOrder.Map.empty;functionNames=FunctionIdMap.empty;functionIds=StringOrder.Map.empty}
let optimizationFunctionNames = FunctionIdMap.ofList (List.map (fun name -> let fullName="Darklang.Stdlib." ^ name in TestIds.functionIdForName fullName,fullName) ["Int.fromInt64";"String.getByteAt";"String.__getByteAtInt64";"String.__byteLength";"String.__byteAtUnchecked"])
let markerContext : optimizeContext = {typeReg=StringOrder.Map.empty;recordTypeParams=StringOrder.Map.empty;functionNames=optimizationFunctionNames;functionIds=StringOrder.Map.of_list (List.map (fun (id,name) -> name,id) (FunctionIdMap.toList optimizationFunctionNames));
 sumShapeReg=StringOrder.Map.of_list ["Marker",{typeParams=["a"];payloads=[0,None;1,None];unaryPayloadTags=IntSet.empty};"Box",{typeParams=["a"];payloads=[0,Some (AST.TVar "a")];unaryPayloadTags=IntSet.singleton 0}]}
let optimizeExpression typeEnv expr = fst (ANFExpressionOptimization.optimizeAExpr markerContext defaultOptimizeOptions TM.empty typeEnv expr)
let rec expressionCalls = function
 | Return _ | Jump _ -> FS.empty
 | Let (_,cexpr,body) -> let current=match cexpr with Call (name,_) -> FS.singleton name | _ -> FS.empty in FS.union current (expressionCalls body)
 | If (_,yes,no) | Join (_,yes,no) -> FS.union (expressionCalls yes) (expressionCalls no)
let stdlibFunction name=TestIds.functionIdForName ("Darklang.Stdlib." ^ name)
let testStringByteIndexConversionFusesWithLookup () =
 let fromInt64=stdlibFunction "Int.fromInt64" and getByteAt=stdlibFunction "String.getByteAt" and getByteAtInt64=stdlibFunction "String.__getByteAtInt64" in
 let expr=Let (TempId 2,Call (fromInt64,[Var (TempId 0)]),Let (TempId 3,Call (getByteAt,[Var (TempId 1);Var (TempId 2)]),Return (Var (TempId 3)))) in
 let optimized=optimizeExpression (TM.of_list [TempId 0,AST.TInt64;TempId 1,AST.TString]) expr in let calls=expressionCalls optimized in
 if FS.mem getByteAtInt64 calls && not (FS.mem fromInt64 calls) && not (FS.mem getByteAt calls) then Ok () else Error ("Expected index conversion and byte lookup to fuse, got " ^ ANFTestFormatting.expr optimized)
let byteOptionMatch usePayload =
 let someBranch=if usePayload then Let (TempId 5,TupleGet (Var (TempId 2),1),Return (Var (TempId 5))) else Return (IntLiteral (Int64 1L)) in
 Let (TempId 2,Call (stdlibFunction "String.__getByteAtInt64",[Var (TempId 0);Var (TempId 1)]),Let (TempId 3,TupleGet (Var (TempId 2),0),Let (TempId 4,Prim (Eq,Var (TempId 3),IntLiteral (Int64 0L)),If (Var (TempId 4),someBranch,Return (IntLiteral (Int64 0L))))))
let testDeadStringBytePayloadLowersToBoundsCheck () =
 let original=stdlibFunction "String.__getByteAtInt64" and byteLength=stdlibFunction "String.__byteLength" in
 let optimized=optimizeExpression (TM.of_list [TempId 0,AST.TString;TempId 1,AST.TInt64]) (byteOptionMatch false) in let calls=expressionCalls optimized in
 if FS.mem byteLength calls && not (FS.mem original calls) then Ok () else Error ("Expected dead Option payload construction to lower to a bounds check, got " ^ ANFTestFormatting.expr optimized)
let testLiveStringBytePayloadLowersToUncheckedLoad () =
 let original=stdlibFunction "String.__getByteAtInt64" and byteLength=stdlibFunction "String.__byteLength" and uncheckedLoad=stdlibFunction "String.__byteAtUnchecked" in
 let optimized=optimizeExpression (TM.of_list [TempId 0,AST.TString;TempId 1,AST.TInt64]) (byteOptionMatch true) in let calls=expressionCalls optimized in
 if FS.mem byteLength calls && FS.mem uncheckedLoad calls && not (FS.mem original calls) then Ok () else Error ("Expected live Option payload construction to lower to a guarded unchecked load, got " ^ ANFTestFormatting.expr optimized)
let testLiteralFloatAbsoluteValueFolds () =
 let optimized=optimizeExpression TM.empty (Let (TempId 0,FloatAbs (FloatLiteral (-3.5)),Return (Var (TempId 0)))) in
 match optimized with Return (FloatLiteral value) when value=3.5 -> Ok () | _ -> Error ("Expected literal FloatAbs to fold to 3.5, got " ^ ANFTestFormatting.expr optimized)
let testLiteralFloatToInt64Folds () =
 let optimized=optimizeExpression TM.empty (Let (TempId 0,FloatToInt64 (FloatLiteral (-3.75)),Return (Var (TempId 0)))) in
 match optimized with Return (IntLiteral (Int64 (-3L))) -> Ok () | _ -> Error ("Expected literal FloatToInt64 to truncate to -3, got " ^ ANFTestFormatting.expr optimized)
let testLiteralBitNotFolds () =
 let optimized=optimizeExpression TM.empty (Let (TempId 0,UnaryPrim (BitNot,IntLiteral (Int64 1L)),Return (Var (TempId 0)))) in
 match optimized with Return (IntLiteral (Int64 (-2L))) -> Ok () | _ -> Error ("Expected literal BitNot to fold to -2, got " ^ ANFTestFormatting.expr optimized)
let testDceDropsUnusedPureGenericSumTypedAtom () =
 let markerType=AST.TSum ("Marker",[AST.TString]) in
 match optimizeMain markerContext (Let (TempId 0,TypedAtom (IntLiteral (Int64 0L),markerType),Return UnitLiteral)) with Return UnitLiteral -> Ok () | other -> Error ("Expected unused pure generic sum TypedAtom to be removed, got " ^ ANFTestFormatting.expr other)
let testDcePreservesUnusedHeapGenericSumTypedAtom () =
 let boxType=AST.TSum ("Box",[AST.TString]) in
 match optimizeMain markerContext (Let (TempId 0,TypedAtom (IntLiteral (Int64 0L),boxType),Return UnitLiteral)) with Let (TempId 0,TypedAtom (_,typ),Return UnitLiteral) when typ=boxType -> Ok () | other -> Error ("Expected heap generic sum TypedAtom to be preserved, got " ^ ANFTestFormatting.expr other)
let testCseReusesFloatAbsoluteValue () =
 let parameter : typedParam = {id=TempId 0;typ=AST.TFloat64} in
 let func : functionDef = {id=TestIds.functionIdForName "floatAbsCse";name="floatAbsCse";typedParams=[parameter];returnType=AST.TTuple [AST.TFloat64;AST.TFloat64];returnOwnership=OwnedReturn;body=Let (TempId 1,FloatAbs (Var parameter.id),Let (TempId 2,FloatAbs (Var parameter.id),Let (TempId 3,TupleAlloc [Var (TempId 1);Var (TempId 2)],Return (Var (TempId 3)))))} in
 let Program (functions,_)=ANF_Optimize.optimizeProgram emptyContext (Program ([func],Return UnitLiteral)) in
 match functions with [{body=Let (TempId 1,FloatAbs (Var (TempId 0)),Let (TempId 3,TupleAlloc [Var left;Var right],Return (Var (TempId 3))));_}] when left=TempId 1 && right=TempId 1 -> Ok () | _ -> Error "Expected duplicate FloatAbs expressions to reuse the first result"
let testCanonicalBufferSelfEqualityFoldsForBothRepresentations () =
 let check kind =
  let parameterType=match kind with Utf8String -> AST.TString | NullableUtf8String -> AST.TSum ("NullableText",[]) | GraphemeCluster -> AST.TChar | NullableGraphemeCluster -> AST.TSum ("NullableChar",[]) in
  let parameter : typedParam = {id=TempId 0;typ=parameterType} in
  let func : functionDef = {id=TestIds.functionIdForName "bufferEquality";name="bufferEquality";typedParams=[parameter];returnType=AST.TBool;returnOwnership=OwnedReturn;body=Let (TempId 1,CanonicalBufferEq (kind,Var parameter.id,Var parameter.id),Return (Var (TempId 1)))} in
  let Program (functions,_)=ANF_Optimize.optimizeProgram emptyContext (Program ([func],Return UnitLiteral)) in
  match functions with [{body=Return (BoolLiteral true);_}] -> Ok () | _ ->
   let name=match kind with Utf8String -> "Utf8String" | NullableUtf8String -> "NullableUtf8String" | GraphemeCluster -> "GraphemeCluster" | NullableGraphemeCluster -> "NullableGraphemeCluster" in Error ("Expected " ^ name ^ " self-equality to fold to true") in
 List.fold_left (fun result kind -> Result.bind result (fun () -> check kind)) (Ok ()) [Utf8String;NullableUtf8String;GraphemeCluster;NullableGraphemeCluster]
let tests=["DCE drops unused pure generic sum TypedAtom",testDceDropsUnusedPureGenericSumTypedAtom;"DCE preserves unused heap generic sum TypedAtom",testDcePreservesUnusedHeapGenericSumTypedAtom;"String byte lookup fuses Int64 index conversion",testStringByteIndexConversionFusesWithLookup;"String byte lookup with dead payload lowers to bounds checks",testDeadStringBytePayloadLowersToBoundsCheck;"String byte lookup with live payload lowers to unchecked load",testLiveStringBytePayloadLowersToUncheckedLoad;"Literal FloatAbs folds",testLiteralFloatAbsoluteValueFolds;"Literal FloatToInt64 folds",testLiteralFloatToInt64Folds;"Literal BitNot folds",testLiteralBitNotFolds;"ANF CSE reuses FloatAbs",testCseReusesFloatAbsoluteValue;"Canonical buffer self-equality folds for String and Char storage",testCanonicalBufferSelfEqualityFoldsForBothRepresentations]
