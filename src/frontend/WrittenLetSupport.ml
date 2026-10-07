(* Continuation inference and local recursive bindings from WrittenLetSupport.ml. *)
[@@@warning "-4"]
module WT = WrittenTypes
module C = CheckedAST
open! C
open! Tokenizer
open! AST
module M = StringOrder.Map
open! WrittenTypeSupport
let bind = Result.bind
let map = Result.map
let orElse first fallback = match first with Some _ -> first | None -> fallback ()
let check checkExpression globals locals symbols expected (range : WT.range) pattern value body =
 let check = checkExpression globals locals in
 let callsBinding name = function WT.EVariable (_, called) -> called = name | WT.EFnName (_, called) -> qualifiedFnName called = [name] | _ -> false in
 let rec inferredUsageType name = function
 | WT.EApply (_, target, [], arguments) when callsBinding name target ->
  (match ResultList.traverse (fun argument -> map (fun (typ, _, _) -> typ) (check symbols None argument)) arguments with
   | Error _ -> None
   | Ok types -> let name = "t$let_lambda_return_" ^ string_of_int range.start.row ^ "_" ^ string_of_int range.start.column in Some (AST.TFunction (types, AST.TInferenceVar (name, name))))
 | WT.EApply (_, WT.EFnName (_, functionName), _, arguments) ->
  (match resolveFunction globals (qualifiedFnName functionName) with
   | Some signature when List.length arguments = List.length signature.parameters ->
    let pairs = List.combine arguments signature.parameters in
    let substitutions = M.of_list (List.concat_map (fun (argument, parameter) -> if callsBinding name argument then [] else match check symbols None argument with Ok (actual, _, _) -> Result.value (Unification.matchTypes parameter actual) ~default:[] | Error _ -> []) pairs) in
    List.find_map (fun (argument, parameter) -> if callsBinding name argument then Some (Types.applyTypeArguments substitutions parameter) else inferredUsageType name argument) pairs
   | _ -> List.find_map (inferredUsageType name) arguments)
 | WT.EInfix (_, _, left, right) -> orElse (inferredUsageType name left) (fun () -> inferredUsageType name right)
 | WT.EPipe (_, source, segments) ->
  let piped = match segments with
  | (_, WT.EPipeFnCall (_, functionName, [], arguments)) :: _ ->
   (match resolveFunction globals (qualifiedFnName functionName), check symbols None source with
    | Some signature, Ok (sourceType, _, _) when List.length signature.parameters = 1 + List.length arguments ->
     (match signature.parameters with first :: rest ->
       (match Unification.matchTypes first sourceType with Error _ -> None | Ok bindings ->
        let substitutions = M.of_list bindings in List.find_map (fun (argument, parameter) -> if callsBinding name argument then Some (Types.applyTypeArguments substitutions parameter) else inferredUsageType name argument) (List.combine arguments rest))
      | [] -> None)
    | _ -> None)
  | _ -> None in orElse (inferredUsageType name source) (fun () -> piped)
 | WT.ELet (_, _, bound, next, _, _) | WT.EStatement (_, bound, next) -> orElse (inferredUsageType name bound) (fun () -> inferredUsageType name next)
 | WT.EIf (_, condition, yes, no, _, _, _) -> orElse (orElse (inferredUsageType name condition) (fun () -> inferredUsageType name yes)) (fun () -> Option.bind no (inferredUsageType name))
 | _ -> None in
 let valueExpected = match pattern, value with WT.LPVariable (_, name), (WT.ELambda _ | WT.EEnum _) -> inferredUsageType name body | _ -> None in
 let rec referencesSelf name = function
 | WT.EApply (_, target, _, arguments) -> callsBinding name target || referencesSelf name target || List.exists (referencesSelf name) arguments
 | WT.EInfix (_, _, left, right) | WT.EStatement (_, left, right) -> referencesSelf name left || referencesSelf name right
 | WT.EIf (_, condition, yes, no, _, _, _) -> referencesSelf name condition || referencesSelf name yes || Option.fold ~none:false ~some:(referencesSelf name) no
 | WT.ELet (_, pattern, bound, next, _, _) -> referencesSelf name bound || (match pattern with WT.LPVariable (_, boundName) when name = boundName -> false | _ -> referencesSelf name next)
 | WT.ELambda (_, _, body, _, _) -> referencesSelf name body
 | _ -> false in
 match pattern, value with
 | WT.LPVariable (_, name), WT.ELambda (_, _, lambdaBody, keywordFun, _) when not (M.mem name locals) && keywordFun.start = keywordFun.end_ && referencesSelf name lambdaBody && Option.is_some (resolveFunction globals [name]) -> Error ("Nested function name '" ^ name ^ "' is ambiguous")
 | WT.LPVariable (_, name), WT.ELambda (_, parameters, lambdaBody, keywordFun, _) when not (M.mem name locals) && referencesSelf name lambdaBody ->
  let provisional = match valueExpected with Some typ -> typ | None ->
   let prefix = string_of_int range.start.row ^ "_" ^ string_of_int range.start.column in
   let types = List.mapi (fun index _ -> let name = "t$recursive_" ^ prefix ^ "_" ^ string_of_int index in AST.TInferenceVar (name, name)) parameters in
   let result = "t$recursive_return_" ^ prefix in AST.TFunction (types, AST.TInferenceVar (result, result)) in
  let binding, withBinding = C.allocateBinding name symbols in
  bind (checkExpression globals (M.add name (provisional, binding) locals) withBinding (Some provisional) value) (fun (valueType, checkedValue, afterValue) ->
   map (fun (bodyType, checkedBody, finalSymbols) ->
    let ordinal = C.nextBindingOrdinal symbols in
    let member = AST.recursiveMemberId ordinal in
    let kind = if keywordFun.start = keywordFun.end_ then AST.NamedLocalFunctionMember else AST.DirectLambdaValueMember in
    let parsed : AST.parsedRecursiveMember = {binding; boundary = AST.scopeBoundaryId ordinal; member; sourceName = name; kind} in
    let resolved : AST.resolvedRecursiveMember = {parsed; group = AST.singletonRecursiveGroupId member; groupIndex = 0; availability = AST.SelfRecursiveMember} in
    let typed : C.recursiveMember = {resolved; monomorphicType = C.checkedType valueType} in
    bodyType, C.RecursiveLet (typed, checkedValue, checkedBody), finalSymbols)
    (checkExpression globals (M.add name (valueType, binding) locals) afterValue expected body))
 | _ -> bind (check symbols valueExpected value) (fun (valueType, checkedValue, afterValue) ->
  bind (WrittenPatternSupport.checkLetPattern pattern valueType afterValue) (fun (pattern, bindings, afterPattern) ->
   let bodyLocals = M.union (fun _ _ newer -> Some newer) locals bindings in
   map (fun (typ, body, symbols) -> typ, C.Let (pattern, checkedValue, body), symbols) (checkExpression globals bodyLocals afterPattern expected body)))
