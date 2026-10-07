(* Contextual lambda inference and currying from WrittenLambdaSupport.ml. *)
open WrittenTypeSupport
module WT = WrittenTypes
module C = CheckedAST
module M = StringOrder.Map
type expressionChecker = globals -> locals -> C.symbols -> AST.semanticType option -> WT.expr -> (checkedExpression, string) result
let[@warning "-4"] check checkExpression globals locals symbols expected (range : WT.range) patterns body keywordFun symbolArrow =
 let check symbols expected expr = checkExpression globals locals symbols expected expr in
 if List.exists (function WT.LPVariable (_, "") -> true | _ -> false) patterns then
  let patterns = List.filter (function WT.LPVariable (_, "") -> false | _ -> true) patterns in
  match patterns with [] -> check symbols expected body | _ -> check symbols expected (WT.ELambda (range, patterns, body, keywordFun, symbolArrow))
 else match expected with
 | Some (AST.TFunction (argumentTypes, _)) when argumentTypes <> [] && List.length argumentTypes < List.length patterns ->
   let outer = List.filteri (fun index _ -> index < List.length argumentTypes) patterns and inner = List.filteri (fun index _ -> index >= List.length argumentTypes) patterns in
   check symbols expected (WT.ELambda (range, outer, WT.ELambda (range, inner, body, keywordFun, symbolArrow), keywordFun, symbolArrow))
 | _ ->
   let knownParameterTypes = match expected with Some (AST.TFunction (argumentTypes, _)) when List.length argumentTypes = List.length patterns ->
    M.of_list (List.filter_map (fun (pattern, typ) -> match pattern with WT.LPVariable (_, name) when not (Unification.containsTVar typ) -> Some (name, typ) | _ -> None) (List.combine patterns argumentTypes)) | _ -> M.empty in
   let numericOperandType = function
   | WT.EInt _ -> Some AST.TInt | WT.EInt8 _ -> Some AST.TInt8 | WT.EInt16 _ -> Some AST.TInt16 | WT.EInt32 _ -> Some AST.TInt32 | WT.EInt64 _ -> Some AST.TInt64 | WT.EInt128 _ -> Some AST.TInt128
   | WT.EUInt8 _ -> Some AST.TUInt8 | WT.EUInt16 _ -> Some AST.TUInt16 | WT.EUInt32 _ -> Some AST.TUInt32 | WT.EUInt64 _ -> Some AST.TUInt64 | WT.EUInt128 _ -> Some AST.TUInt128 | WT.EFloat _ -> Some AST.TFloat64
   | WT.EVariable (_, name) -> (match M.find_opt name knownParameterTypes with Some typ -> Some typ | None -> Option.map fst (M.find_opt name locals))
   | _ -> None in
   let isNumericOperator = function
   | WT.InfixFnCall (WT.ArithmeticPlus | WT.ArithmeticMinus | WT.ArithmeticMultiply | WT.ArithmeticDivide | WT.ArithmeticModulo | WT.ArithmeticPower | WT.ComparisonGreaterThan | WT.ComparisonGreaterThanOrEqual | WT.ComparisonLessThan | WT.ComparisonLessThanOrEqual) -> true
   | _ -> false in
   let orElseWith next value = match value with Some _ -> value | None -> next () in
   let rec inferParameterType name expression = match expression with
   | WT.EInfix (_, (_, op), WT.EVariable (_, leftName), right) when leftName = name && isNumericOperator op -> numericOperandType right
   | WT.EInfix (_, (_, op), left, WT.EVariable (_, rightName)) when rightName = name && isNumericOperator op -> numericOperandType left
   | WT.EInfix (_, _, left, right) -> inferParameterType name left |> orElseWith (fun () -> inferParameterType name right)
   | WT.EEnum (_, _, _, fields, _) -> List.find_map (inferParameterType name) fields
   | WT.EApply (_, WT.EFnName (_, functionName), _, args) -> (match resolveFunction globals (qualifiedFnName functionName) with
     | Some signature when List.length args = List.length signature.parameters -> List.find_map (fun (argument, typ) -> match argument with WT.EVariable (_, argName) when argName = name && not (Unification.containsTVar typ) -> Some typ | _ -> inferParameterType name argument) (List.combine args signature.parameters)
     | _ -> List.find_map (inferParameterType name) args)
   | WT.ELet (_, _, value, next, _, _) | WT.EStatement (_, value, next) -> inferParameterType name value |> orElseWith (fun () -> inferParameterType name next)
   | WT.EIf (_, condition, thenBranch, elseBranch, _, _, _) -> inferParameterType name condition |> orElseWith (fun () -> inferParameterType name thenBranch) |> orElseWith (fun () -> Option.bind elseBranch (inferParameterType name))
   | _ -> None in
   let lambdaExpected = match expected with
   | Some (AST.TFunction (argumentTypes, returnType)) when List.length argumentTypes = List.length patterns ->
     Some (AST.TFunction (List.map2 (fun pattern typ -> match pattern with WT.LPVariable (_, name) when Unification.containsTVar typ -> Option.value (inferParameterType name body) ~default:typ | _ -> typ) patterns argumentTypes, returnType))
   | Some (AST.TFunction _ as typ) -> Some typ | Some (AST.TVar _ | AST.TInferenceVar _) -> None | Some other -> Some other | None -> None in
   let lambdaExpected = match lambdaExpected with Some _ -> lambdaExpected | None ->
    let moduleKey = String.concat "_" globals.modulePath in
    let position = string_of_int range.Tokenizer.start.Tokenizer.row ^ "_" ^ string_of_int range.Tokenizer.start.Tokenizer.column in
    let args = List.mapi (fun index pattern -> match (match pattern with WT.LPVariable (_, name) -> inferParameterType name body | _ -> None) with Some typ -> typ | None -> let name = "t$lambda_" ^ moduleKey ^ "_" ^ position ^ "_" ^ string_of_int index in AST.TInferenceVar (name, name)) patterns in
    let returnName = "t$lambda_return_" ^ moduleKey ^ "_" ^ position in Some (AST.TFunction (args, AST.TInferenceVar (returnName, returnName))) in
   match lambdaExpected with
   | Some (AST.TFunction (argumentTypes, returnType)) when List.length patterns = List.length argumentTypes ->
     let result = List.fold_left (fun result (pattern, typ) -> Result.bind result (fun (reversed, capturedLocals, symbols) -> Result.bind (WrittenPatternSupport.checkLetPattern pattern typ symbols) (fun (pattern, bindings, symbols) -> Result.map (fun merged -> let param : C.lambdaParameter = {C.pattern; typ = C.checkedType typ} in param :: reversed, merged, symbols) (WrittenPatternSupport.mergePatternBindings capturedLocals bindings)))) (Ok ([], M.empty, symbols)) (List.combine patterns argumentTypes) in
     Result.bind result (fun (reversed, parameters, symbols) -> match NonEmptyList.tryFromList (List.rev reversed) with
     | None -> Error "Lambda requires at least one parameter"
     | Some parametersChecked ->
       let bodyLocals = M.fold M.add parameters locals in
       let bodyExpected = match body with WT.EIf (_, _, _, Some _, _, _, _) when Unification.containsTVar returnType -> None | _ -> Some returnType in
       Result.map (fun (bodyType, body, symbols) -> let inferredReturn = if Unification.containsTVar returnType then bodyType else returnType in AST.TFunction (argumentTypes, inferredReturn), C.Lambda (parametersChecked, Some (C.checkedType inferredReturn), body), symbols) (checkExpression globals bodyLocals symbols bodyExpected body))
   | Some (AST.TFunction _) -> Error "Lambda parameter count mismatch"
   | _ -> Error "Lambda requires an expected function type"
