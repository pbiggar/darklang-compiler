(*
   ASTToANFTests.fs - Unit tests for AST to ANF conversion behavior
   Covers targeted AST-to-ANF regression cases that are easier to express
   directly at the pass boundary than through end-to-end language tests.
*)
[@@@warning "-4-42"]
open Dark_compiler
module C=CheckedAST
module M=StringOrder.Map
module B=TypeRegistries.BindingMap
type testResult=(unit,string) result
let (let*)=Result.bind
let emptyTypeReg=M.empty
let emptyVariantLookup=M.empty
let emptyFuncReg=FunctionIdMap.empty
let emptyModuleRegistry=M.empty
let checkedProgram source=let* parsed=WrittenParsing.parse Validation.Script source in Result.map snd (WrittenChecking.checkSourceUnits false false [parsed])
let testMissingVariantPayloadTypeErrors ()=
 let xId=AST.bindingId 1 and payloadId=AST.bindingId 2 in
 let constructorId,symbols=C.internConstructor "MissingType" "Missing" 0 (C.emptySymbols ()) in
 let env=B.of_list [xId,(ANF.TempId 0,AST.TSum ("MissingType",[]))] in
 let pattern=C.PConstructor (constructorId,[C.PVariable payloadId]) in
 match NonEmptyList.tryFromList [pattern] with None->Error "NonEmptyList.tryFromList returned None for a non-empty list"|Some patterns->
 let matchCase:C.matchCase={C.patterns;guard=None;body=C.Local payloadId} in
 let expr=C.Match (C.Local xId,NonEmptyList.singleton matchCase) in
 match AST_to_ANF.toANFWithMetadata (TypeRegistries.typeNamesFromSymbols symbols) expr ANF.initialVarGen env emptyTypeReg emptyVariantLookup emptyFuncReg emptyModuleRegistry with
 |Ok _->Error "Expected error when constructor payload type is missing from variant lookup"|Error msg->if Text.contains msg "Constructor tag" then Ok () else Error ("Unexpected error message: "^msg)
let testNeedsLambdaLoweringIgnoresShadowedFunc ()=let* program=checkedProgram "let f = 1L in f" in if Monomorphization.programNeedsLambdaLowering (StringOrder.Set.of_list ["f"]) program then Error "Expected shadowed function name to not trigger lambda lowering" else Ok ()
let testNeedsLambdaLoweringDetectsFuncValue ()=let* program=checkedProgram "let f (x: Int64) : Int64 = x\nf" in if Monomorphization.programNeedsLambdaLowering (StringOrder.Set.of_list ["f"]) program then Ok () else Error "Expected function value usage to trigger lambda lowering"
let testNeedsLambdaLoweringDetectsLambda ()=let* program=checkedProgram "let apply (f: Int64 -> Int64) : Int64 = f 1L\napply (fun x -> x)" in if Monomorphization.programNeedsLambdaLowering StringOrder.Set.empty program then Ok () else Error "Expected lambda to trigger lambda lowering"
let testMangledTypePreservesFreshenedTypeVariables ()=match LoweringPrimitives.tryParseMangledType M.empty "k$0" with
 |Ok (AST.TVar "k$0")->Ok ()|Ok other->Error ("Expected freshened type variable to remain TVar, got "^StructuralFormat.semanticType other)|Error err->Error ("Expected freshened type variable to parse, got error: "^err)
let testMangledFunctionTypePreservesSyntheticTypeVariables ()=
 let typ=AST.TFunction ([AST.TVar "__synthetic_lambda_0_1_y"],AST.TInt64) in
 let mangled=SpecializationIdentity.typeToMangledName typ in
 match LoweringPrimitives.tryParseMangledType M.empty mangled with
 |Ok (AST.TFunction ([AST.TVar "$u$usynthetic$ulambda$u0$u1$uy"],AST.TInt64))->Ok ()
 |Ok other->Error ("Expected synthetic type variable to parse inside function type, got "^StructuralFormat.semanticType other^" from "^mangled)
 |Error err->Error ("Expected synthetic type variable function type to parse from "^mangled^", got error: "^err)
let rec findCallArgs funcName=function
 |ANF.Let (_,ANF.Call (name,args),_) when name=TestIds.functionIdForName funcName->Some args
 |ANF.Let (_,_,rest)->findCallArgs funcName rest
 |ANF.Join (_,thenBranch,elseBranch)|ANF.If (_,thenBranch,elseBranch)->(match findCallArgs funcName thenBranch with Some _ as args->args|None->findCallArgs funcName elseBranch)
 |ANF.Jump _|ANF.Return _->None
let rec containsCExpr predicate=function
 |ANF.Let (_,cexpr,rest)->predicate cexpr || containsCExpr predicate rest
 |ANF.Join (_,thenBranch,elseBranch)|ANF.If (_,thenBranch,elseBranch)->containsCExpr predicate thenBranch || containsCExpr predicate elseBranch
 |ANF.Jump _|ANF.Return _->false
let lowerTwoElementListPattern elementType=
 let valueId=AST.bindingId 1 and headId=AST.bindingId 2 in let listType=AST.TList elementType in
 let env=B.of_list [valueId,(ANF.TempId 0,AST.TTuple [listType;AST.TInt64])] in
 let matchCase:C.matchCase={C.patterns=NonEmptyList.singleton (C.PTuple [C.PList [C.PVariable headId;C.PWildcard];C.PWildcard]);guard=None;body=C.Local headId} in
 let expr=C.Match (C.Local valueId,NonEmptyList.singleton matchCase) in
 let functions=["Darklang.Stdlib.List.__length_i64",AST.TFunction ([listType],AST.TInt64);"Darklang.Stdlib.List.__tail_i64",AST.TFunction ([listType],listType);"Darklang.Stdlib.List.__headUnsafe_i64",AST.TFunction ([listType],elementType);"Darklang.Stdlib.List.__headUnsafeFloat",AST.TFunction ([listType],elementType)] |> List.map (fun (name,typ)->TestIds.functionIdForName name,(name,typ)) |> FunctionIdMap.ofList in
 Result.map fst (AST_to_ANF.toANF expr ANF.initialVarGen env emptyTypeReg emptyVariantLookup functions emptyModuleRegistry)
let testErasedListHeadPatternLowersToBorrowedCall ()=match lowerTwoElementListPattern (AST.TDict (AST.TString,AST.TString)) with
 |Error err->Error ("Unexpected conversion error: "^err)|Ok anfExpr->
 let hasBorrowed=containsCExpr (function ANF.BorrowedCall (name,_) when name=TestIds.functionIdForName "Darklang.Stdlib.List.__headUnsafe_i64"->true|_->false) anfExpr in
 let hasOwned=containsCExpr (function ANF.Call (name,_) when name=TestIds.functionIdForName "Darklang.Stdlib.List.__headUnsafe_i64"->true|_->false) anfExpr in
 if not hasBorrowed then Error "Managed erased list-head pattern did not lower to BorrowedCall" else if hasOwned then Error "Managed erased list-head pattern also emitted an owned Call" else Ok ()
let testTypedListHeadPatternRemainsOwnedCall ()=match lowerTwoElementListPattern AST.TFloat64 with
 |Error err->Error ("Unexpected conversion error: "^err)|Ok anfExpr->
 let hasOwned=containsCExpr (function ANF.Call (name,_) when name=TestIds.functionIdForName "Darklang.Stdlib.List.__headUnsafeFloat"->true|_->false) anfExpr in
 let hasBorrowed=containsCExpr (function ANF.BorrowedCall (name,_) when name=TestIds.functionIdForName "Darklang.Stdlib.List.__headUnsafeFloat"->true|_->false) anfExpr in
 if not hasOwned then Error "Typed float list-head pattern did not lower to an owned Call" else if hasBorrowed then Error "Typed float list-head pattern incorrectly lowered to BorrowedCall" else Ok ()
let testSyntheticNullaryCallLowersToZeroArgs ()=
 let funcName="Darklang.Stdlib.List.__TAG_SINGLE" in
 let expr=C.Call (TestIds.functionIdForName funcName,NonEmptyList.singleton C.UnitLiteral) in
 let funcReg=FunctionIdMap.ofList [TestIds.functionIdForName funcName,(funcName,AST.TFunction ([],AST.TInt64))] in
 match AST_to_ANF.toANF expr ANF.initialVarGen B.empty emptyTypeReg emptyVariantLookup funcReg emptyModuleRegistry with
 |Error err->Error ("Unexpected conversion error: "^err)|Ok (anfExpr,_)->match findCallArgs funcName anfExpr with None->Error "Expected to find lowered direct call in ANF output"|Some []->Ok ()|Some args->Error (Printf.sprintf "Expected synthetic nullary call to lower to zero args, got %d" (List.length args))
let testSyntheticUnitParamLowersFunctionToZeroParams ()=
 let unitId,symbols=C.allocateBinding "$unit0" (C.emptySymbols ()) in
 let funcDef:C.functionDef={C.id=TestIds.functionIdForName "syntheticNullary";name="syntheticNullary";typeParams=[];params=NonEmptyList.singleton (unitId,C.checkedType AST.TUnit);returnType=C.checkedType AST.TInt64;body=C.Int64Literal 1L;recursion=None} in
 let funcReg=FunctionIdMap.ofList [TestIds.functionIdForName "syntheticNullary",("syntheticNullary",AST.TFunction ([],AST.TInt64))] in
 match AST_to_ANF.convertFunction symbols funcDef ANF.initialVarGen emptyTypeReg emptyVariantLookup funcReg emptyModuleRegistry with
 |Error err->Error ("Unexpected conversion error: "^err)|Ok (anfFunc,_)->match anfFunc.ANF.typedParams with []->Ok ()|typedParams->Error (Printf.sprintf "Expected 0 lowered params, got %d" (List.length typedParams))
let displayList display values=let rec first count=function []->[]|_ when count=0->["... "]|value::rest->display value::first (count-1) rest in "["^String.concat "; " (first 3 values)^"]"
let testTypedParamAllocationPreservesOrder ()=
 let loweredParams=[AST.bindingId 1,AST.TInt64;AST.bindingId 2,AST.TBool;AST.bindingId 3,AST.TString] in
 let typedParams,nextVarGen=AST_to_ANF.allocateTypedParams loweredParams ANF.initialVarGen in
 match typedParams,nextVarGen with
 |[{ANF.id=ANF.TempId 0;typ=AST.TInt64};{ANF.id=ANF.TempId 1;typ=AST.TBool};{ANF.id=ANF.TempId 2;typ=AST.TString}],ANF.VarGen 3->Ok ()
 |_->Error ("Expected ordered typed params t0, t1, t2 and next VarGen 3, got "^displayList (fun value->StructuralFormat.format (ANFTestFormatting.aNF_typedParam value)) typedParams^" and "^StructuralFormat.format (ANFTestFormatting.aNF_varGen nextVarGen))
let testOverlayFunctionIdsContainOnlyLocalDefinitions ()=
 let baseId,symbols=C.internFunction "Test.baseFunction" (C.emptySymbols ()) in
 let baseParam,baseSymbols=C.allocateBinding "baseParam" symbols in
 let baseFunction:C.functionDef={C.id=baseId;name="Test.baseFunction";typeParams=[];params=NonEmptyList.singleton (baseParam,C.checkedType AST.TInt64);returnType=C.checkedType AST.TInt64;body=C.Local baseParam;recursion=None} in
 let localId,symbols=C.internFunction "Test.localFunction" baseSymbols in
 let localParam,overlaySymbols=C.allocateBinding "localParam" symbols in
 let localFunction:C.functionDef={C.id=localId;name="Test.localFunction";typeParams=[];params=NonEmptyList.singleton (localParam,C.checkedType AST.TInt64);returnType=C.checkedType AST.TInt64;body=C.Local localParam;recursion=None} in
 let base=AST_to_ANF.buildRegistries baseSymbols emptyModuleRegistry [] M.empty [baseFunction] in
 let overlay=AST_to_ANF.buildOverlayRegistries overlaySymbols emptyModuleRegistry [] M.empty [localFunction] in
 let merged=AST_to_ANF.mergeRegistries base overlay in
 if not (M.equal (=) overlay.AST_to_ANF.functionIds (M.of_list ["Test.localFunction",localId])) then
 Error ("Expected the overlay function index to contain only its local definition, got "^StructuralFormat.format (StructuralValue.Union ("map",[StructuralValue.Sequence (M.bindings overlay.AST_to_ANF.functionIds |> List.map (fun (name,id)->StructuralValue.Tuple [StructuralValue.Text name;AST.DiagnosticFormatting.func id]))])))
 else if M.find_opt "Test.baseFunction" merged.AST_to_ANF.functionIds<>Some baseId then Error "Merged function index lost the base definition"
 else if M.find_opt "Test.localFunction" merged.AST_to_ANF.functionIds<>Some localId then Error "Merged function index lost the local definition" else Ok ()
let tests=["Missing constructor payload type errors",testMissingVariantPayloadTypeErrors;"Lambda lowering ignores shadowed functions",testNeedsLambdaLoweringIgnoresShadowedFunc;"Lambda lowering detects function value",testNeedsLambdaLoweringDetectsFuncValue;"Lambda lowering detects lambda",testNeedsLambdaLoweringDetectsLambda;"Mangled type preserves freshened type variables",testMangledTypePreservesFreshenedTypeVariables;"Mangled function type preserves synthetic type variables",testMangledFunctionTypePreservesSyntheticTypeVariables;"Synthetic nullary call lowers to zero args",testSyntheticNullaryCallLowersToZeroArgs;"Synthetic unit param lowers function to zero params",testSyntheticUnitParamLowersFunctionToZeroParams;"Erased list-head pattern lowers to borrowed call",testErasedListHeadPatternLowersToBorrowedCall;"Typed list-head pattern remains owned call",testTypedListHeadPatternRemainsOwnedCall;"Typed parameter allocation preserves order",testTypedParamAllocationPreservesOrder;"Overlay function IDs contain only local definitions",testOverlayFunctionIdsContainOnlyLocalDefinitions]
