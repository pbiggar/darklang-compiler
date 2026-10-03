(* HIRConstructionTests.fs - Checked-function normalization and structured-edge laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module H = HIR
module C = CheckedAST
module N = ConstructHIRFunctions
module B = C.BindingIdMap
module BS = ClosureAnalysis.BindingSet
let ( let* ) = Result.bind
let binding name = Array.fold_left (fun hash ch -> Int32.add (Int32.mul hash 31l) (Int32.of_int ch)) 17l (HostText.utf16Units name) |> Int32.to_int |> AST.bindingId
let local name = C.Local (binding name)
let variable name = C.LPVariable (binding name)
let parameter name typ = binding name, typ
let rec dependencies = function
 | C.Local id -> BS.singleton id
 | C.Let (pattern, value, body) -> let bound = BS.of_list (C.letPatternBindings pattern) in BS.union (dependencies value) (BS.diff (dependencies body) bound)
 | C.Sequence (first, next) -> BS.union (dependencies first) (dependencies next)
 | C.If (condition, yes, no) -> List.fold_left BS.union BS.empty [dependencies condition; dependencies yes; dependencies no]
 | C.BinOp (_, left, right) -> BS.union (dependencies left) (dependencies right)
 | C.UnaryOp (_, operand) -> dependencies operand
 | C.Call (_, arguments) -> List.map dependencies (NonEmptyList.toList arguments) |> List.fold_left BS.union BS.empty
 | _ -> BS.empty
let rec infer types = function
 | C.UnitLiteral -> Ok AST.TUnit | C.Int8Literal _ -> Ok AST.TInt8 | C.Int16Literal _ -> Ok AST.TInt16 | C.Int32Literal _ -> Ok AST.TInt32 | C.Int64Literal _ -> Ok AST.TInt64
 | C.UInt8Literal _ -> Ok AST.TUInt8 | C.UInt16Literal _ -> Ok AST.TUInt16 | C.UInt32Literal _ -> Ok AST.TUInt32 | C.UInt64Literal _ -> Ok AST.TUInt64
 | C.BoolLiteral _ -> Ok AST.TBool | C.FloatLiteral _ -> Ok AST.TFloat64 | C.StringLiteral _ -> Ok AST.TString | C.BigIntLiteral _ -> Ok AST.TInt
 | C.Local id -> (match B.find_opt id types with Some typ -> Ok typ | None -> Error ("unknown value " ^ HostStructuralFormat.format (AST.DiagnosticFormatting.binding id)))
 | C.If (_, yes, _) -> infer types yes
 | C.Let (C.LPVariable id, value, body) -> let* valueType = infer types value in infer (B.add id valueType types) body
 | C.Let (_, _, body) -> infer types body
 | C.UnaryOp (_, operand) -> infer types operand
 | C.BinOp (op, left, _) -> (match op with AST.Eq | AST.Neq | AST.Lt | AST.Gt | AST.Lte | AST.Gte | AST.And | AST.Or -> Ok AST.TBool | AST.StringConcat -> Ok AST.TString | _ -> infer types left)
 | expression -> Error ("unsupported test expression " ^ CheckedStructuralFormat.toString expression)
let functionDefinition body : C.functionDef = {C.id = TestIds.functionIdForName "choose"; name = "choose"; typeParams = []; params = C.checkedParams {NonEmptyList.head = parameter "flag" AST.TBool; tail = [parameter "first" AST.TInt64; parameter "second" AST.TInt64]}; returnType = C.checkedType AST.TInt64; body; recursion = None}
let noCalls : N.callContracts = {N.externalSignature = (fun _ -> None); contract = (fun _ -> None)}
let showError = function N.CannotInferExpression (name, message) -> HostStructuralFormat.format (StructuralValue.Union ("CannotInferExpression", [StructuralValue.Text name; StructuralValue.Text message])) | N.InconsistentCallSignature (name, target) -> HostStructuralFormat.format (StructuralValue.Union ("InconsistentCallSignature", [StructuralValue.Text name; AST.DiagnosticFormatting.func target]))
let construct definition = N.constructFunction infer dependencies noCalls definition |> Result.map_error (fun error -> "Unexpected HIR construction failure: " ^ showError error)
let testConstructsOrderedStructuredFunction () =
 let definition = functionDefinition (C.Let (variable "selected", C.If (local "flag", local "first", local "second"), C.Sequence (C.UnitLiteral, local "selected"))) in
 let* constructed = construct definition in let block = N.body constructed.H.body in
 match block.H.parameters, block.H.operations with
 | [flag; first; second], [H.Branch (result, condition, ifTrue, ifFalse); H.Leaf (N.Literal (unitResult, N.UnitLiteral))] ->
  let trueBlock = N.body ifTrue in let falseBlock = N.body ifFalse in
  let actualParameters = List.map (fun (p : H.parameter) -> p.H.binding, p.H.value.H.typ) [flag; first; second] in
  let orderedParameters = actualParameters = [parameter "flag" AST.TBool; parameter "first" AST.TInt64; parameter "second" AST.TInt64] in
  let structuredEdges = B.equal (=) condition.H.inputs (B.singleton (binding "flag") flag.H.value) && trueBlock.H.result = first.H.value && falseBlock.H.result = second.H.value && block.H.result = result && unitResult.H.typ = AST.TUnit in
  let verified = VerifyHIR.verifyFunctions (N.verificationDialect noCalls) [constructed] = Ok () in
  if orderedParameters && structuredEdges && verified then Ok () else Error "Constructed function did not preserve its checked boundary and branch edges"
 | _ -> Error "Expected one branch followed by the sequenced Unit evaluation"
let unsupported () = functionDefinition (C.Let (variable "unsupported", C.TupleLiteral (C.tupleElementsOfList [C.Int64Literal 1L; C.Int64Literal 2L]), local "first"))
let testReportsBindingInferenceFailure () = match N.constructFunction infer dependencies noCalls (unsupported ()) with Error (N.CannotInferExpression ("choose", _)) -> Ok () | _ -> Error "Expected a scoped inference failure"
let testFallsBackToOpaqueCheckedFunctionForScheduling () = let source = unsupported () in
 match N.constructFunctionsWithOpaqueFallback FunctionIdMap.empty infer dependencies noCalls [source] with
 | Ok [constructed] -> let block = N.body constructed.H.body in (match block.H.parameters, block.H.operations with [_; first; _], [H.ScalarBinding (result, operand)] when result = block.H.result && operand.H.expression = source.C.body && B.equal (=) operand.H.inputs (B.singleton first.H.binding first.H.value) -> Ok () | _ -> Error "Expected one boundary-typed opaque operation for the unsupported function")
 | _ -> Error "Expected conservative opaque construction"
let callFunction name firstParameter remainingParameters body : C.functionDef = {C.id = TestIds.functionIdForName name; name; typeParams = []; params = C.checkedParams {NonEmptyList.head = firstParameter; tail = remainingParameters}; returnType = C.checkedType AST.TInt64; body; recursion = None}
let callContract aliasResult (call : H.functionCall) : H.primitiveContract = {H.inputs = call.H.arguments; operands = []; outputs = [{H.value = call.H.result; alias = if aliasResult then H.MayReuseInput call.H.result else H.NoManagedAlias}]; effects = H.EffectSet.singleton H.MayInvokeUserCode}
let contractedCalls aliasResult : N.callContracts = {N.externalSignature = (fun _ -> None); contract = (fun target -> if target = TestIds.functionIdForName "callee" then Some (callContract aliasResult) else None)}
let testNormalizesContractedCallsInArgumentOrder () =
 let callee = callFunction "callee" (parameter "first" AST.TInt64) [parameter "second" AST.TInt64] (local "first") in
 let caller = callFunction "caller" (parameter "unit" AST.TUnit) [] (C.Call (TestIds.functionIdForName "callee", {NonEmptyList.head = C.Int64Literal 1L; tail = [C.Int64Literal 2L]})) in let calls = contractedCalls false in
 match N.constructFunctions infer dependencies calls [callee; caller] with
 | Error error -> Error ("Unexpected direct-call construction failure: " ^ showError error)
 | Ok ([_; constructedCaller] as constructed) -> let block = N.body constructedCaller.H.body in
  let orderedArguments = match block.H.operations with [H.Leaf (N.Literal (_, N.Int64Literal 1L)); H.Leaf (N.Literal (_, N.Int64Literal 2L)); H.Call _] -> true | _ -> false in
  let verified = VerifyHIR.verifyFunctions (N.verificationDialect calls) constructed = Ok () in if orderedArguments && verified then Ok () else Error "Contracted call did not preserve argument order and typed verification"
 | Ok _ -> Error "Expected two constructed functions"
let oneArgumentCalls () = let callee = callFunction "callee" (parameter "value" AST.TInt64) [] (local "value") in let caller = callFunction "caller" (parameter "value" AST.TInt64) [] (C.Call (TestIds.functionIdForName "callee", NonEmptyList.singleton (local "value"))) in callee, caller
let testKeepsUncontractedCallsOpaque () = let callee, caller = oneArgumentCalls () in match N.constructFunctions infer dependencies noCalls [callee; caller] with
 | Ok [_; constructedCaller] -> let block = N.body constructedCaller.H.body in (match block.H.parameters, block.H.operations with [parameter], [H.ScalarBinding (_, operand)] when operand.H.expression = caller.C.body && B.equal (=) operand.H.inputs (B.singleton (binding "value") parameter.H.value) -> Ok () | _ -> Error "Uncontracted direct call did not remain an opaque checked operand")
 | Ok _ -> Error "Expected two constructed functions" | Error error -> Error ("Unexpected opaque-call construction failure: " ^ showError error)
let testRejectsInvalidCallAliasContract () = let callee, caller = oneArgumentCalls () in let calls = contractedCalls true in
 match N.constructFunctions infer dependencies calls [callee; caller] with Error error -> Error ("Unexpected direct-call construction failure: " ^ showError error) | Ok constructed -> (match VerifyHIR.verifyFunctions (N.verificationDialect calls) constructed with Error (VerifyHIR.InvalidAliasSource _) -> Ok () | _ -> Error "Expected invalid call alias provenance")
let scalarFunction name parameter returnType body : C.functionDef = {C.id = TestIds.functionIdForName name; name; typeParams = []; params = C.checkedParams (NonEmptyList.singleton parameter); returnType = C.checkedType returnType; body; recursion = None}
let testNormalizesScalarPrimitivesWithContracts () =
 let definition = scalarFunction "calculate" (parameter "unit" AST.TUnit) AST.TInt64 (C.BinOp (AST.Div, C.BinOp (AST.Add, C.Int64Literal 4L, C.UnaryOp (AST.Neg, C.Int64Literal 2L)), C.Int64Literal 3L)) in
 let* constructed = construct definition in let block = N.body constructed.H.body in match block.H.operations with
 | [H.Leaf (N.Literal (four, N.Int64Literal 4L)); H.Leaf (N.Literal (two, N.Int64Literal 2L)); H.Leaf (N.Unary (negative, AST.Neg, unaryInput)); H.Leaf (N.Binary (sum, AST.Add, addLeft, addRight)); H.Leaf (N.Literal (three, N.Int64Literal 3L)); H.Leaf (N.Binary (quotient, AST.Div, dividend, divisor))] ->
  let additionContract = N.primitiveContract (N.Binary (sum, AST.Add, addLeft, addRight)) in let divisionContract = N.primitiveContract (N.Binary (quotient, AST.Div, dividend, divisor)) in
  let outputIsUnmanaged (contract : H.primitiveContract) expected = contract.H.outputs = [{H.value = expected; alias = H.NoManagedAlias}] in
  let orderedValues = unaryInput = two && addLeft = four && addRight = negative && dividend = sum && divisor = three && block.H.result = quotient in
  let exactContracts = additionContract.H.inputs = [four; negative] && H.EffectSet.is_empty additionContract.H.effects && outputIsUnmanaged additionContract sum && divisionContract.H.inputs = [sum; three] && H.EffectSet.equal divisionContract.H.effects (H.EffectSet.singleton H.MayFail) && outputIsUnmanaged divisionContract quotient in
  let verified = VerifyHIR.verifyFunctions (N.verificationDialect noCalls) [constructed] = Ok () in if orderedValues && exactContracts && verified then Ok () else Error "Scalar primitive values or contracts were inconsistent"
 | _ -> Error "Expected ordered literal, unary, arithmetic, and division leaves"
let testRestoresLexicalScopeBetweenPrimitiveOperands () =
 let definition = scalarFunction "shadow" (parameter "value" AST.TInt64) AST.TInt64 (C.BinOp (AST.Add, C.Let (variable "value", C.Int64Literal 1L, local "value"), local "value")) in
 let* constructed = construct definition in let block = N.body constructed.H.body in match block.H.parameters, block.H.operations with [parameter], [H.Leaf (N.Literal (inner, N.Int64Literal 1L)); H.Leaf (N.Binary (_, AST.Add, left, right))] when left = inner && right = parameter.H.value -> Ok () | _ -> Error "Primitive operands did not restore their outer lexical scope"
let testNormalizesUnsignedBitwiseNot () = let definition = scalarFunction "invert" (parameter "value" AST.TUInt64) AST.TUInt64 (C.UnaryOp (AST.BitNot, local "value")) in
 let* constructed = construct definition in let block = N.body constructed.H.body in match block.H.parameters, block.H.operations with [parameter], [H.Leaf (N.Unary (result, AST.BitNot, operand))] when operand = parameter.H.value && result = block.H.result -> Ok () | _ -> Error "Unsigned bitwise operation was not normalized"
let testKeepsUnsupportedPrimitivesOpaque () =
 let managedBody = C.BinOp (AST.StringConcat, local "value", C.StringLiteral "suffix") in let managedDefinition = scalarFunction "append" (parameter "value" AST.TString) AST.TString managedBody in
 let arbitraryPrecisionBody = C.BinOp (AST.Add, C.BigIntLiteral Z.one, C.BigIntLiteral (Z.of_int 2)) in let arbitraryPrecisionDefinition = scalarFunction "addInts" (parameter "unit" AST.TUnit) AST.TInt arbitraryPrecisionBody in
 let opaqueBlock definition expectedExpression expectedInputNames = let* constructed = construct definition in let block = N.body constructed.H.body in
  let expectedBindings = BS.of_list (List.map binding expectedInputNames) in let expectedInputs = List.filter_map (fun (parameter : H.parameter) -> if BS.mem parameter.H.binding expectedBindings then Some (parameter.H.binding, parameter.H.value) else None) block.H.parameters |> B.of_list in
  match block.H.operations with [H.ScalarBinding (_, operand)] when operand.H.expression = expectedExpression && B.equal (=) operand.H.inputs expectedInputs -> Ok () | _ -> Error "Unsupported primitive did not remain one opaque evaluation" in
 let managed = opaqueBlock managedDefinition managedBody ["value"] in let arbitraryPrecision = opaqueBlock arbitraryPrecisionDefinition arbitraryPrecisionBody [] in
 match managed, arbitraryPrecision with Ok (), Ok () -> Ok () | Error error, _ | _, Error error -> Error error
let tests = [
 "Checked functions construct ordered structured HIR", testConstructsOrderedStructuredFunction;
 "Checked function construction reports scoped inference failures", testReportsBindingInferenceFailure;
 "Ownership scheduling can conservatively retain inference-resistant checked functions", testFallsBackToOpaqueCheckedFunctionForScheduling;
 "Contracted direct calls preserve argument evaluation order", testNormalizesContractedCallsInArgumentOrder;
 "Uncontracted direct calls remain opaque", testKeepsUncontractedCallsOpaque;
 "Direct-call alias contracts remain verifier-authoritative", testRejectsInvalidCallAliasContract;
 "Native scalar primitives expose ordered effects and aliases", testNormalizesScalarPrimitivesWithContracts;
 "Primitive operands restore their outer lexical scope", testRestoresLexicalScopeBetweenPrimitiveOperands;
 "Unsigned bitwise primitives are normalized", testNormalizesUnsignedBitwiseNot;
 "Managed and call-lowered primitives remain opaque", testKeepsUnsupportedPrimitivesOpaque
]
