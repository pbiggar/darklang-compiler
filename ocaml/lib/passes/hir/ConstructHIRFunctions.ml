(* ConstructFunctions.fs - Normalize checked functions into structured semantic HIR. *)
[@@@warning "-4-42"]
module H = HIR
module C = CheckedAST
module B = C.BindingIdMap
module BS = ClosureAnalysis.BindingSet
module F = FunctionIdMap
let ( let* ) = Result.bind
(*
   Source scalar primitives retain the operation identity needed by later
   lowering while exposing their semantic contract independently.
*)
type scalarLiteral = UnitLiteral | Int8Literal of int | Int16Literal of int | Int32Literal of int32 | Int64Literal of int64 | UInt8Literal of int | UInt16Literal of int | UInt32Literal of int64 | UInt64Literal of int64 | BoolLiteral of bool | FloatLiteral of float
type primitive = Literal of H.value * scalarLiteral | Unary of H.value * AST.unaryOp * H.value | Binary of H.value * AST.binOp * H.value * H.value | FreshManaged of H.value * H.operand | ListTransform of H.value * H.value * H.operand
type block = Block of (primitive, block) H.operation H.block
type constructionError = CannotInferExpression of string * string | InconsistentCallSignature of string * AST.functionId
type callContracts = {externalSignature : AST.functionId -> H.functionSignature option; contract : AST.functionId -> (H.functionCall -> H.primitiveContract) option}
type state = {values : H.value B.t; operations : (primitive, block) H.operation list; nextId : int}
let body (Block block) = block
let primitiveContract primitive : H.primitiveContract =
 let inputs, output, effects = match primitive with
 | Literal (result, _) -> [], result, H.EffectSet.empty
 | Unary (result, _, operand) -> [operand], result, H.EffectSet.empty
 | Binary (result, (AST.Div | AST.Mod), left, right) when left.H.typ <> AST.TFloat64 -> [left; right], result, H.EffectSet.singleton H.MayFail
 | Binary (result, _, left, right) -> [left; right], result, H.EffectSet.empty
 | FreshManaged (result, source) -> List.map snd (B.bindings source.H.inputs), result, H.EffectSet.of_list [H.MayEvaluateOpaqueSource; H.MayAllocate]
 | ListTransform (result, input, source) -> input :: (B.bindings source.H.inputs |> List.filter_map (fun (_, (value : H.value)) -> if value.H.id <> input.H.id then Some value else None)), result, H.EffectSet.of_list [H.MayEvaluateOpaqueSource; H.MayAllocate; H.MayInvokeUserCode; H.ReadsOwnedStorage; H.WritesOwnedStorage] in
 let alias = match primitive with FreshManaged _ -> H.FreshManaged | ListTransform (_, input, _) -> H.MayReuseInput input | Literal _ | Unary _ | Binary _ -> H.NoManagedAlias in
 let operands = match primitive with FreshManaged (_, source) | ListTransform (_, _, source) -> [source] | Literal _ | Unary _ | Binary _ -> [] in
 {H.inputs; outputs = [{H.value = output; alias}]; operands; effects}
let verificationDialect calls : (primitive, block) VerifyHIR.dialect = {VerifyHIR.body; leaf = primitiveContract; callSignature = calls.externalSignature; callContract = (fun call -> Option.map (fun contract -> contract call) (calls.contract call.H.target))}
let signatureOfCheckedFunction (definition : C.functionDef) : H.functionSignature = {H.parameters = List.map snd (NonEmptyList.toList (C.functionParameterTypes definition)); result = C.functionReturnType definition}
let constructWithSignatures functionHasName infer dependencies calls (callSignature : AST.functionId -> H.functionSignature option) (definition : C.functionDef) =
 let parameterValues = List.mapi (fun nextId (binding, typ) -> {H.binding; value = {H.id = H.ValueId nextId; typ}}) (NonEmptyList.toList (C.functionParameterTypes definition)) in
 let nextId = List.length parameterValues in let values = B.of_list (List.map (fun (parameter : H.parameter) -> parameter.H.binding, parameter.H.value) parameterValues) in
 let types state = B.map (fun (value : H.value) -> value.H.typ) state.values in
 let inferExpression state expression = infer (types state) expression |> Result.map_error (fun message -> CannotInferExpression (definition.C.name, message)) in
 let fresh typ state = let value = {H.id = H.ValueId state.nextId; typ} in value, {state with nextId = state.nextId + 1} in
 let operand state expression typ : H.operand = let inputs = BS.elements (dependencies expression) |> List.filter_map (fun name -> Option.map (fun value -> name, value) (B.find_opt name state.values)) |> B.of_list in {H.expression; typ; inputs} in
 let opaque state expression typ = let result, next = fresh typ state in let operation = H.ScalarBinding (result, operand state expression typ) in Ok (result, {next with operations = operation :: state.operations}) in
 let emitPrimitive state typ build = let result, next = fresh typ state in let operation = H.Leaf (build result) in result, {next with operations = operation :: state.operations} in
 let literal expected expression = match expression, expected with
 | C.UnitLiteral, AST.TUnit -> Some UnitLiteral
 | C.Int8Literal value, AST.TInt8 -> Some (Int8Literal value)
 | C.Int16Literal value, AST.TInt16 -> Some (Int16Literal value)
 | C.Int32Literal value, AST.TInt32 -> Some (Int32Literal value)
 | C.Int64Literal value, AST.TInt64 -> Some (Int64Literal value)
 | C.UInt8Literal value, AST.TUInt8 -> Some (UInt8Literal value)
 | C.UInt16Literal value, AST.TUInt16 -> Some (UInt16Literal value)
 | C.UInt32Literal value, AST.TUInt32 -> Some (UInt32Literal value)
 | C.UInt64Literal value, AST.TUInt64 -> Some (UInt64Literal value)
 | C.BoolLiteral value, AST.TBool -> Some (BoolLiteral value)
 | C.FloatLiteral value, AST.TFloat64 -> Some (FloatLiteral value)
 | _ -> None in
 let nativeNumericType = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TFloat64 -> true | _ -> false in
 let nativeIntegerType typ = nativeNumericType typ && typ <> AST.TFloat64 in
 let nativeImmediateType typ = nativeNumericType typ || typ = AST.TBool || typ = AST.TUnit in
 let supportsUnary op operandType expected = operandType = expected && match op with AST.Neg -> nativeNumericType operandType | AST.Not -> operandType = AST.TBool | AST.BitNot -> (match operandType with AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> true | _ -> false) in
 let supportsBinary op leftType rightType expected = leftType = rightType && match op with
 | AST.Add | AST.Sub | AST.Mul | AST.Div -> expected = leftType && nativeNumericType leftType
 | AST.Mod | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor -> expected = leftType && nativeIntegerType leftType
 | AST.Eq | AST.Neq -> expected = AST.TBool && nativeImmediateType leftType
 | AST.Lt | AST.Gt | AST.Lte | AST.Gte -> expected = AST.TBool && nativeNumericType leftType
 | AST.And | AST.Or -> expected = AST.TBool && leftType = AST.TBool
 | AST.Pow | AST.StringConcat -> false in
 let finishNested initialState finalState result = Block {H.parameters = []; operations = List.rev finalState.operations; result}, {initialState with nextId = finalState.nextId} in
 let rec normalize state expected expression = match literal expected expression with Some value -> Ok (emitPrimitive state expected (fun result -> Literal (result, value))) | None -> normalizeNonLiteral state expected expression
 and normalizeNonLiteral state expected expression = match expression with
 | C.ListLiteral _ when expected = AST.TList AST.TInt64 -> let result, next = fresh expected state in let operation = H.Leaf (FreshManaged (result, operand state expression expected)) in Ok (result, {next with operations = operation :: state.operations})
 | C.Call (target, arguments) when expected = AST.TList AST.TInt64 && (functionHasName target "Darklang.Stdlib.List.map_i64_i64" || functionHasName target "Darklang.Stdlib.List.reverse_i64") ->
  (match NonEmptyList.toList arguments with inputExpression :: _ -> let* inputType = inferExpression state inputExpression in if inputType <> AST.TList AST.TInt64 then Error (InconsistentCallSignature (definition.C.name, target)) else
   let* input, afterInput = normalize state inputType inputExpression in let result, next = fresh expected afterInput in let operation = H.Leaf (ListTransform (result, input, operand afterInput expression expected)) in Ok (result, {next with operations = operation :: afterInput.operations})
   | [] -> Error (InconsistentCallSignature (definition.C.name, target)))
 | C.Local id -> (match B.find_opt id state.values with Some value when value.H.typ = expected -> Ok (value, state) | _ -> opaque state expression expected)
 | C.Let (C.LPVariable id, value, continuation) -> let* valueType = inferExpression state value in let* boundValue, afterValue = normalize state valueType value in normalize {afterValue with values = B.add id boundValue afterValue.values} expected continuation
 | C.Let ((C.LPUnit | C.LPWildcard), value, continuation) -> let* valueType = inferExpression state value in let* _, afterValue = normalize state valueType value in normalize afterValue expected continuation
 | C.Sequence (first, continuation) -> let* _, afterFirst = normalize state AST.TUnit first in normalize afterFirst expected continuation
 | C.If (condition, ifTrue, ifFalse) -> let condition = operand state condition AST.TBool in let branchState = {state with operations = []} in
  let* trueResult, afterTrue = normalize branchState expected ifTrue in let trueBlock, nextAfterTrue = finishNested state afterTrue trueResult in
  let falseState = {branchState with nextId = nextAfterTrue.nextId} in let* falseResult, afterFalse = normalize falseState expected ifFalse in let falseBlock, nextAfterFalse = finishNested state afterFalse falseResult in
  let result, next = fresh expected nextAfterFalse in let branch = H.Branch (result, condition, trueBlock, falseBlock) in Ok (result, {next with operations = branch :: state.operations})
 | C.Call (target, arguments) -> (match callSignature target, calls.contract target with
  | Some signature, Some _ -> let arguments = NonEmptyList.toList arguments in if List.length arguments <> List.length signature.H.parameters || signature.H.result <> expected then Error (InconsistentCallSignature (definition.C.name, target)) else
   let* arguments, afterArguments = normalizeArguments state target signature.H.parameters arguments in let result, next = fresh signature.H.result afterArguments in let operation = H.Call {H.target; arguments; result} in Ok (result, {next with operations = operation :: afterArguments.operations})
  | _ -> opaque state expression expected)
 | C.UnaryOp (op, operandExpression) -> let* operandType = inferExpression state operandExpression in if supportsUnary op operandType expected then let* operandValue, afterOperand = normalize state operandType operandExpression in let afterOperand = {afterOperand with values = state.values} in Ok (emitPrimitive afterOperand expected (fun result -> Unary (result, op, operandValue))) else opaque state expression expected
 | C.BinOp (op, leftExpression, rightExpression) -> let* leftType = inferExpression state leftExpression in let* rightType = inferExpression state rightExpression in
  if supportsBinary op leftType rightType expected then let* leftValue, afterLeft = normalize state leftType leftExpression in let afterLeft = {afterLeft with values = state.values} in
   let* rightValue, afterRight = normalize afterLeft rightType rightExpression in let afterRight = {afterRight with values = state.values} in Ok (emitPrimitive afterRight expected (fun result -> Binary (result, op, leftValue, rightValue))) else opaque state expression expected
 | _ -> opaque state expression expected
 and normalizeArguments state target parameterTypes arguments = match parameterTypes, arguments with
 | [], [] -> Ok ([], state)
 | parameterType :: parameterTypes, argument :: arguments -> let* argumentType = inferExpression state argument in if argumentType <> parameterType then Error (InconsistentCallSignature (definition.C.name, target)) else
  let* value, afterArgument = normalize state parameterType argument in let next = {afterArgument with values = state.values} in let* values, finalState = normalizeArguments next target parameterTypes arguments in Ok (value :: values, finalState)
 | _ -> Error (InconsistentCallSignature (definition.C.name, target)) in
 let initial = {values; operations = []; nextId} in let* result, finalState = normalize initial (C.functionReturnType definition) definition.C.body in
 Ok {H.id = definition.C.id; name = definition.C.name; body = Block {H.parameters = parameterValues; operations = List.rev finalState.operations; result}}
let constructFunction infer dependencies calls (definition : C.functionDef) = let internalSignature target = if target = definition.C.id then Some (signatureOfCheckedFunction definition) else calls.externalSignature target in constructWithSignatures (fun _ _ -> false) infer dependencies calls internalSignature definition
let constructFunctions infer dependencies calls definitions =
 let internalSignatures = F.ofList (List.map (fun (definition : C.functionDef) -> definition.C.id, signatureOfCheckedFunction definition) definitions) in
 let callSignature target = match F.tryFind target internalSignatures with Some signature -> Some signature | None -> calls.externalSignature target in
 List.fold_left (fun result definition -> let* functions = result in let* fn = constructWithSignatures (fun _ _ -> false) infer dependencies calls callSignature definition in Ok (fn :: functions)) (Ok []) definitions |> Result.map List.rev
(*
   Ownership scheduling can conservatively retain a checked function whose
   internal expression types are unavailable to the lowering inference helper.
   The opaque operation preserves its typed boundary and all visible parameter
   dependencies without weakening the strict construction API above.
*)
let constructFunctionsWithOpaqueFallback functionNames infer dependencies calls definitions =
 let functionHasName id name = F.tryFind id functionNames = Some name in
 let internalSignatures = F.ofList (List.map (fun (definition : C.functionDef) -> definition.C.id, signatureOfCheckedFunction definition) definitions) in
 let callSignature target = match F.tryFind target internalSignatures with Some signature -> Some signature | None -> calls.externalSignature target in
 let opaque (definition : C.functionDef) =
  let parameters = List.mapi (fun index (binding, typ) -> {H.binding; value = {H.id = H.ValueId index; typ}}) (NonEmptyList.toList (C.functionParameterTypes definition)) in
  let visible = B.of_list (List.map (fun (parameter : H.parameter) -> parameter.H.binding, parameter.H.value) parameters) in
  let inputs = BS.elements (dependencies definition.C.body) |> List.filter_map (fun binding -> Option.map (fun value -> binding, value) (B.find_opt binding visible)) |> B.of_list in
  let result = {H.id = H.ValueId (List.length parameters); typ = C.functionReturnType definition} in
  {H.id = definition.C.id; name = definition.C.name; body = Block {H.parameters; operations = [H.ScalarBinding (result, {H.expression = definition.C.body; typ = C.functionReturnType definition; inputs})]; result}} in
 List.fold_left (fun result definition -> let* functions = result in match constructWithSignatures functionHasName infer dependencies calls callSignature definition with Ok fn -> Ok (fn :: functions) | Error (CannotInferExpression _ | InconsistentCallSignature _) -> Ok (opaque definition :: functions)) (Ok []) definitions |> Result.map List.rev
