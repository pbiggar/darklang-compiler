(*
   TypeCheckingTests.ml - Unit tests for type checking pass
   Tests the type checker for Phase 0 (integers only)
   Will be extended in future phases for booleans, variables, functions, etc.
   NOTE: All tests now return Result<> instead of using failwith
*)
(* TypeCheckingTests.ml - Original type-checking and declaration boundary assertions. *)
[@@@warning "-4-42"]
open Dark_compiler
open! AST
module C=CheckedAST
module D=CheckingDiagnostics
(*
   Test result type
*)
type testResult=(unit,string) result
let (let*)=Result.bind
(*
   Helper to check that type checking succeeds with expected type
*)
let expectType (expr:AST.expr) expectedType=
 match TypeChecking.checkProgram (Program [Expression ([],expr)]) with
 |Ok (actualType,_)->if actualType=expectedType then Ok () else Error ("Expected "^D.typeToString expectedType^", got "^D.typeToString actualType)
 |Error error->Error ("Type checking failed: "^D.typeErrorToString error)
(*
   Test that integer literals have type TInt64
*)
let testInt64Literal ()=expectType (Int64Literal 42L) TInt64
(*
   Test that Int128 literals have type TInt128
*)
let testInt128Literal ()=expectType (Int128Literal (Z.of_string "42")) TInt128
(*
   Test that UInt128 literals have type TUInt128
*)
let testUInt128Literal ()=expectType (UInt128Literal (Z.of_string "42")) TUInt128
let rec countMatches expr=
 let childMatches=match expr with
 |C.BoundaryRender (_,value)->[value]
 |C.UnitLiteral|C.Int64Literal _|C.Int128Literal _|C.BigIntLiteral _|C.Int8Literal _|C.Int16Literal _|C.Int32Literal _|C.UInt8Literal _|C.UInt16Literal _|C.UInt32Literal _|C.UInt64Literal _|C.UInt128Literal _|C.BoolLiteral _|C.StringLiteral _|C.BlobLiteral _|C.CharLiteral _|C.FloatLiteral _|C.Local _|C.FuncRef _|C.RuntimeError _->[]
 |C.InterpolatedString parts->List.filter_map (function C.StringText _->None|C.StringExpr e->Some e) parts
 |C.BinOp (_,left,right)->[left;right]
 |C.UnaryOp (_,inner)->[inner]
 |C.Let (_,value,body)|C.RecursiveLet (_,value,body)->[value;body]
 |C.If (condition,yes,no)->[condition;yes;no]
 |C.Sequence (first,next)->[first;next]
 |C.Call (_,args)|C.TypeApp (_,_,args)->NonEmptyList.toList args
 |C.TupleLiteral args->C.tupleElementsToList args
 |C.ListLiteral args->args
 |C.TupleAccess (value,_)->[value]
 |C.DictLiteral (_,_,entries)->List.concat_map (fun (key,value)->[key;value]) entries
 |C.RecordLiteral (_,fields)->C.recordFieldsInSourceOrder fields |> List.map snd
 |C.RecordUpdate (value,updates)->value::List.map snd updates
 |C.RecordAccess (value,_)->[value]
 |C.Constructor (_,fields)->fields
 |C.Match (scrutinee,cases)->scrutinee::List.map (fun (case:C.matchCase)->case.C.body) (NonEmptyList.toList cases)
 |C.Lambda (_,_,body)->[body]
 |C.Apply (func,args)|C.IndirectApply (func,args)->func::NonEmptyList.toList args
 |C.Closure (_,captures)->captures in
 let childCount=List.fold_left (fun total child->total+countMatches child) 0 childMatches in
 match expr with C.Match _->childCount+1|_->childCount
(*
   Sum equality should lower to one pair-match instead of nested match trees.
*)
let testSumEqualityUsesSinglePairMatch ()=
 let sumDef=TypeDef (SumTypeDef ("ChoiceTc",["a";"b"],[{AST.name="ChoiceLeftTc";fields=[TVar "a"]};{AST.name="ChoiceRightTc";fields=[TVar "b"]}])) in
 let eqExpr=Let (LPVariable "a",Constructor (UnresolvedConstructor (Some "ChoiceTc"),"ChoiceLeftTc",[Int64Literal 1L]),Let (LPVariable "b",Constructor (UnresolvedConstructor (Some "ChoiceTc"),"ChoiceLeftTc",[Int64Literal 1L]),BinOp (Eq,Var "a",Var "b"))) in
 match TypeChecking.checkProgram (Program [sumDef;Expression ([],eqExpr)]) with
 |Error error->Error ("Type checking failed: "^D.typeErrorToString error)
 |Ok (actualType,checkedProgram)->
 let topLevels=C.programTopLevels checkedProgram in
 if actualType<>TBool then Error ("Expected Bool result type, got "^D.typeToString actualType) else
 let helperDefs=List.filter_map (function C.FunctionDef func when String.starts_with ~prefix:"__dark_eq_" func.C.name->Some func|_->None) topLevels in
 let expressionMatchCountResult=match List.filter_map (function C.Expression expr->Some (countMatches expr)|_->None) topLevels with first::_->Ok first|[]->Error "Expected checked program to include a top-level expression" in
 match helperDefs with []->Error "Expected generated structural equality helper function for sum equality"|helperDef::_->
 let helperMatchCount=countMatches helperDef.C.body in
 let helperCaseCount=match helperDef.C.body with C.Let (_,_,C.Match (_,cases))->NonEmptyList.length cases|_->0 in
 let* expressionMatchCount=expressionMatchCountResult in
 if helperMatchCount<>1 then Error (Printf.sprintf "Expected helper body to contain one Match, got %d" helperMatchCount)
 else if helperCaseCount<>3 then Error (Printf.sprintf "Expected two unique variant cases and one default, got %d cases" helperCaseCount)
 else if expressionMatchCount<>0 then Error (Printf.sprintf "Expected top-level expression to call helper without Match nodes, got %d" expressionMatchCount) else Ok ()
(*
   Record access should report invalid generic record arity instead of
   silently treating the field's type variables as concrete field types.
*)
let testRecordAccessRejectsInvalidRecordArity ()=
 let recordDef=TypeDef (RecordDef ("ArityBoxTc",["a"],["value",TVar "a"])) in
 let funcDef=FunctionDef {AST.name="main";typeParams=[];params=NonEmptyList.singleton ("box",TRecord ("ArityBoxTc",[TInt64;TBool]));returnType=TInt64;body=RecordAccess (Var "box",unresolvedRecordFieldReference "value");recursion=None} in
 match TypeChecking.checkProgram (Program [recordDef;funcDef]) with
 |Ok _->Error "Expected invalid record type argument arity to fail type checking"
 |Error (D.GenericError message) when Text.contains message "Record type argument arity mismatch"->Ok ()
 |Error error->Error ("Expected record arity mismatch, got: "^D.typeErrorToString error)
(*
   Test that addition of integers has type TInt64
*)
let testAddition ()=expectType (BinOp (Add,Int64Literal 2L,Int64Literal 3L)) TInt64
(*
   Test that subtraction of integers has type TInt64
*)
let testSubtraction ()=expectType (BinOp (Sub,Int64Literal 10L,Int64Literal 5L)) TInt64
(*
   Test that multiplication of integers has type TInt64
*)
let testMultiplication ()=expectType (BinOp (Mul,Int64Literal 7L,Int64Literal 6L)) TInt64
(*
   Test that division of integers has type TInt64
*)
let testDivision ()=expectType (BinOp (Div,Int64Literal 20L,Int64Literal 4L)) TInt64
(*
   Test that negation of integers has type TInt64
*)
let testNegation ()=expectType (UnaryOp (Neg,Int64Literal 42L)) TInt64
(*
   Test nested operations
   2 + 3 * 4
*)
let testNestedOperations ()=expectType (BinOp (Add,Int64Literal 2L,BinOp (Mul,Int64Literal 3L,Int64Literal 4L))) TInt64
(*
   Test complex nested expression
   (10 + 20) * (30 - 15) / 2
*)
let testComplexExpression ()=expectType (BinOp (Div,BinOp (Mul,BinOp (Add,Int64Literal 10L,Int64Literal 20L),BinOp (Sub,Int64Literal 30L,Int64Literal 15L)),Int64Literal 2L)) TInt64
let expectDeclarationError program expectedMessage=match TypeChecking.checkPublicProgram program with
 |Error (D.GenericError actual) when actual=expectedMessage->Ok ()
 |Error error->Error ("Expected '"^expectedMessage^"', got: "^D.typeErrorToString error)
 |Ok _->Error ("Expected declaration error: "^expectedMessage)
let testDuplicateNominalTypeDeclarationUsesLastOverlay ()=
 let program=Program [TypeDef (SumTypeDef ("DuplicateNominalTc",[],[{AST.name="A";fields=[]}]));TypeDef (SumTypeDef ("DuplicateNominalTc",[],[{AST.name="B";fields=[]}]));Expression ([],Constructor (UnresolvedConstructor (Some "DuplicateNominalTc"),"B",[]))] in
 match TypeChecking.checkPublicProgram program with
 |Ok (TSum ("DuplicateNominalTc",[]),_)->Ok ()
 |Ok (typ,_)->Error ("Expected overlaid nominal type, got: "^D.typeToString typ)
 |Error error->Error ("Expected last nominal declaration to win, got: "^D.typeErrorToString error)
let testDuplicateConstructorDeclarationRejected ()=expectDeclarationError (Program [TypeDef (SumTypeDef ("DuplicateCaseTc",[],[{AST.name="SameCaseTc";fields=[]};{AST.name="SameCaseTc";fields=[TInt64]}]));Expression ([],UnitLiteral)]) "Duplicate constructor declaration: DuplicateCaseTc.SameCaseTc"
let testDuplicateAndUndeclaredTypeParametersRejected ()=
 let duplicateProgram=Program [TypeDef (SumTypeDef ("DuplicateParamTc",["a";"a"],[{AST.name="ParamCaseTc";fields=[TVar "a"]}]));Expression ([],UnitLiteral)] in
 let undeclaredProgram=Program [TypeDef (SumTypeDef ("UndeclaredParamTc",["a"],[{AST.name="ParamCaseTc";fields=[TVar "b"]}]));Expression ([],UnitLiteral)] in
 let* ()=expectDeclarationError duplicateProgram "Duplicate type parameter: a in DuplicateParamTc" in expectDeclarationError undeclaredProgram "Undeclared type parameter: 'b in UndeclaredParamTc"
let testEmptyNominalDeclarationsRejected ()=
 let emptySum=Program [TypeDef (SumTypeDef ("EmptySumTc",[],[]));Expression ([],UnitLiteral)] in
 let emptyRecord=Program [TypeDef (RecordDef ("EmptyRecordTc",[],[]));Expression ([],UnitLiteral)] in
 let* ()=expectDeclarationError emptySum "Enum declaration must contain at least one case: EmptySumTc" in expectDeclarationError emptyRecord "Record declaration must contain at least one field: EmptyRecordTc"
let testInvalidDeclarationTypeReferencesRejected ()=
 let unknownType=Program [TypeDef (SumTypeDef ("UnknownPayloadTc",[],[{AST.name="UnknownPayloadCaseTc";fields=[TRecord ("MissingTypeTc",[])]}]));Expression ([],UnitLiteral)] in
 let wrongArity=Program [TypeDef (SumTypeDef ("GenericTargetTc",["a"],[{AST.name="GenericTargetCaseTc";fields=[]}]));TypeDef (SumTypeDef ("WrongArityTc",[],[{AST.name="WrongArityCaseTc";fields=[TSum ("GenericTargetTc",[])]}]));Expression ([],UnitLiteral)] in
 let* ()=expectDeclarationError unknownType "Unknown type reference: MissingTypeTc in UnknownPayloadTc" in expectDeclarationError wrongArity "Type argument arity mismatch: GenericTargetTc expects 1, got 0 in WrongArityTc"
let testConstructorIdentityCollisionRejected ()=
 expectDeclarationError (Program [TypeDef (SumTypeDef ("CollisionType151Tc",[],[{AST.name="CollisionCaseTc";fields=[]}]));TypeDef (SumTypeDef ("CollisionType155Tc",[],[{AST.name="CollisionCaseTc";fields=[]}]));Expression ([],UnitLiteral)]) "Constructor identity collision 1236: CollisionType151Tc.CollisionCaseTc, CollisionType155Tc.CollisionCaseTc"
let testRecursiveGroupsReceiveStableTypedIdentities ()=
 let source="let groupEven (n: Int64) : Int64 = if n == 0L then 1L else groupOdd (n - 1L)\n"^"let groupOdd (n: Int64) : Int64 = if n == 0L then 0L else groupEven (n - 1L)\n"^"let completed (n: Int64) : Int64 = n + 1L\n\ncompleted 1L" in
 let* program=Result.map_error (fun error->"Recursive group parse failed: "^error) (WrittenParsing.parse Validation.Script source) in
 let* _,checkedProgram=Result.map_error (fun error->"Recursive group type check failed: "^error) (WrittenChecking.checkSourceUnits false true [program]) in
 let topLevels=C.programTopLevels checkedProgram in
 let recursionByName=List.filter_map (function C.FunctionDef func->Option.map (fun typed->func.C.name,typed.C.resolved) func.C.recursion|_->None) topLevels |> StringOrder.Map.of_list in
 let actual=StringOrder.Map.find_opt "groupEven" recursionByName,StringOrder.Map.find_opt "groupOdd" recursionByName,StringOrder.Map.find_opt "completed" recursionByName in
 match actual with Some evenMember,Some oddMember,Some completedMember when evenMember.AST.group=oddMember.AST.group && evenMember.AST.group<>completedMember.AST.group && evenMember.AST.availability=MutualRecursiveMember && oddMember.AST.availability=MutualRecursiveMember && completedMember.AST.availability=CompletedGroupMember && [evenMember.AST.groupIndex;oddMember.AST.groupIndex]=[0;1]->Ok ()
 |left,middle,right->let optional=function None->StructuralValue.Union ("None",[])|Some value->StructuralValue.Union ("Some",[ResolutionTestFormatting.resolvedRecursiveMember value]) in Error ("Unexpected recursive group identities: "^StructuralFormat.format (StructuralValue.Tuple [optional left;optional middle;optional right]))
let testWrittenRecordRejectsRepeatedField ()=
 let source="type DuplicateDeclarationField = { value: Int64; value: String }\n1L" in
 match WrittenParsing.parse Validation.Script source with Error error->Error ("Expected valid syntax, got: "^error)|Ok program->match WrittenChecking.checkSourceUnits false true [program] with
 |Error error when Text.contains error "Duplicate field 'value' in record type DuplicateDeclarationField"->Ok ()
 |Error error->Error ("Expected duplicate record field error, got: "^error)
 |Ok _->Error "Expected duplicate record field to fail type checking"
(*
   Regression for the largest compatible E2E batch: the type checker must not
   multiply top-level traversal depth by the depth of its final let chain.
*)
let testManyTopLevelFunctionsAndLetsAreStackSafe ()=
 let programSize=373 in
 let functions=List.init programSize (fun index->FunctionDef {AST.name=Printf.sprintf "stackSafeTypeCheck%d" index;typeParams=[];params=NonEmptyList.singleton ("value",TInt64);returnType=TInt64;body=Var "value";recursion=None}) in
 let expression=List.fold_right (fun index body->Let (LPVariable (Printf.sprintf "stackSafeResult%d" index),applyNamed (Printf.sprintf "stackSafeTypeCheck%d" index) (NonEmptyList.singleton (Int64Literal (Int64.of_int index))),body)) (List.init programSize Fun.id) (Int64Literal 0L) in
 match TypeChecking.checkProgram (Program (functions@[Expression ([],expression)])) with
 |Ok (TInt64,_)->Ok ()
 |Ok (typ,_)->Error ("Expected Int64 result, got "^D.typeToString typ)
 |Error error->Error ("Expected large program to type check, got: "^D.typeErrorToString error)
(*
   Filtering must preserve same-spelled survivors and their import precedence.
*)
let testFilteredResolutionCandidates ()=
 let candidate owner=match NameResolution.candidate "SharedCase" (NameResolution.ConstructorSymbol (owner,"SharedCase")) (NameResolution.SourceDeclaration owner) with Some value->value|None->Crash.crash "Test candidate has an invalid fixed spelling" in
 let first=candidate "First" in let second=candidate "Second" in
 let environment=NameResolution.addCandidates [first;second] NameResolution.empty in
 let filtered=NameResolution.filterCandidates (fun (value:NameResolution.candidate)->value.NameResolution.identity=first.NameResolution.identity) environment in
 let imported=NameResolution.merge filtered NameResolution.empty in let removed=NameResolution.filterCandidates (fun _->false) environment in
 let before=NameResolution.resolve NameResolution.Constructor "SharedCase" environment in
 let after=NameResolution.resolve NameResolution.Constructor "SharedCase" filtered in
 let importResult=NameResolution.resolve NameResolution.Constructor "SharedCase" imported in
 let removal=NameResolution.resolve NameResolution.Constructor "SharedCase" removed in
 match before,after,importResult,removal with
 |Error (NameResolution.AmbiguousReference _),Ok local,Ok imported,Error (NameResolution.UnresolvedName _) when local.NameResolution.identity=first.NameResolution.identity && local.NameResolution.provenance=first.NameResolution.provenance && imported.NameResolution.identity=first.NameResolution.identity && imported.NameResolution.provenance=NameResolution.PackageDeclaration "First"->Ok ()
 |_->let result=function Ok value->StructuralValue.Union ("Ok",[ResolutionTestFormatting.successfulResolution value])|Error error->StructuralValue.Union ("Error",[ResolutionTestFormatting.resolutionError error]) in Error ("Filtered/imported candidate resolution changed: "^StructuralFormat.format (StructuralValue.Tuple [result before;result after;result importResult;result removal]))
let tests=["Filtered name-resolution candidates preserve survivors and imports",testFilteredResolutionCandidates;"Integer literal",testInt64Literal;"Int128 literal",testInt128Literal;"UInt128 literal",testUInt128Literal;"Sum equality uses single pair match",testSumEqualityUsesSinglePairMatch;"Record access rejects invalid record arity",testRecordAccessRejectsInvalidRecordArity;"Addition",testAddition;"Subtraction",testSubtraction;"Multiplication",testMultiplication;"Division",testDivision;"Negation",testNegation;"Nested operations",testNestedOperations;"Complex expression",testComplexExpression;"Duplicate nominal type declaration uses last overlay",testDuplicateNominalTypeDeclarationUsesLastOverlay;"Duplicate constructor declaration rejected",testDuplicateConstructorDeclarationRejected;"Duplicate and undeclared type parameters rejected",testDuplicateAndUndeclaredTypeParametersRejected;"Empty nominal declarations rejected",testEmptyNominalDeclarationsRejected;"Invalid declaration type references rejected",testInvalidDeclarationTypeReferencesRejected;"Constructor identity collision rejected",testConstructorIdentityCollisionRejected;"Recursive groups receive stable typed identities",testRecursiveGroupsReceiveStableTypedIdentities;"Written record rejects repeated field",testWrittenRecordRejectsRepeatedField;"Many top-level functions and lets are stack-safe",testManyTopLevelFunctionsAndLetsAreStackSafe]
(*
   Run all type checking unit tests
   Returns Ok () if all pass, Error with first failure message if any fail
*)
let runAll ()=let rec runTests=function []->Ok ()|(name,test)::rest->match test () with Ok ()->runTests rest|Error message->Error (name^" test failed: "^message) in runTests tests
