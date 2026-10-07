(* Builtin and indirect applications in the direct source checker. *)
[@@@warning "-4"]
module WT = WrittenTypes
module C = CheckedAST
open! C
let bind = Result.bind
let map = Result.map
let format = StructuralFormat.semanticType
let integers = [AST.TInt; AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128]
let builtin checkExpression literal globals locals symbols expected name typeArgs args =
 let check = checkExpression globals locals in
 match name, typeArgs, args with
 | "negate", [], [argument] -> bind (check symbols expected argument) (fun (typ, argument, symbols) ->
  if List.mem typ [AST.TInt; AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TFloat64] then literal expected symbols typ (C.UnaryOp (AST.Neg, argument)) else Error ("Unary negation is unavailable for " ^ format typ))
 | "negate", _, _ -> Error "Builtin.negate expects one argument"
 | ("boolNot" | "bitwiseNot"), [], [argument] -> bind (check symbols None argument) (fun (typ, argument, symbols) ->
  if typ = AST.TNever then literal expected symbols AST.TNever argument
  else if name = "boolNot" && typ = AST.TBool then literal expected symbols AST.TBool (C.UnaryOp (AST.Not, argument))
  else if name = "bitwiseNot" && List.mem typ integers then literal expected symbols typ (C.UnaryOp (AST.BitNot, argument))
  else Error ("Unary operator is unavailable for " ^ format typ))
 | ("boolNot" | "bitwiseNot"), _, _ -> Error "Unary operator expects one argument"
 | "unwrap", [], [argument] ->
  let argumentExpected = match expected, argument with
   | Some typ, WT.EEnum (_, _, (_, ("None" | "Some")), _, _) -> Ok (Some (AST.TSum ("Darklang.Stdlib.Option.Option", [typ])))
   | Some typ, WT.EEnum (_, _, (_, ("Error" | "Ok")), _, _) -> Ok (Some (AST.TSum ("Darklang.Stdlib.Result.Result", [typ; AST.TInferenceVar ("unwrap_error", "unwrap_error")])))
   (* The success payload does not exist for these constructors.
      TNever keeps the type precise while the runtime reports the failed unwrap. *)
   | None, WT.EEnum (_, _, (_, "None"), [], _) -> Ok (Some (AST.TSum ("Darklang.Stdlib.Option.Option", [AST.TNever])))
   | None, WT.EEnum (_, _, (_, "Error"), [errorValue], _) -> map (fun (typ, _, _) -> Some (AST.TSum ("Darklang.Stdlib.Result.Result", [AST.TNever; typ]))) (check symbols None errorValue)
   | _ -> Ok None in
  bind argumentExpected (fun expectedArgument -> bind (check symbols expectedArgument argument) (fun (typ, argument, symbols) ->
   let output = match typ with AST.TSum ("Darklang.Stdlib.Option.Option", [value]) | AST.TSum ("Darklang.Stdlib.Result.Result", [value; _]) -> Ok value | _ -> Error ("Can only unwrap Options and Results, yet got " ^ format typ) in
   bind output (fun typ -> let id, symbols = C.internFunction "Builtin.unwrap" symbols in literal expected symbols typ (C.Call (id, NonEmptyList.singleton argument)))))
 | "unwrap", [], _ -> Error ("Builtin.unwrap expects 1 argument, got " ^ string_of_int (List.length args))
 | "unwrap", _, _ -> Error "Builtin.unwrap does not accept type arguments"
 | _ -> Crash.crash "Unknown direct-checking builtin application"
let take count list = List.filteri (fun index _ -> index < count) list
let drop count list = List.filteri (fun index _ -> index >= count) list
let indirect checkExpression literal globals locals symbols expected range target typeArgs args =
 let check = checkExpression globals locals in
 let checkArgs symbols args types = List.fold_left (fun result (arg, typ) -> bind result (fun (reversed, symbols) -> map (fun (actual, value, symbols) -> (actual, value) :: reversed, symbols) (check symbols typ arg))) (Ok ([], symbols)) (List.combine args types) |> map (fun (reversed, symbols) -> List.rev reversed, symbols) in
 match target, typeArgs with
 | WT.ELambda (_, patterns, _, _, _), [] when patterns <> [] && List.length args > List.length patterns ->
  check symbols expected (WT.EApply (range, WT.EApply (range, target, [], take (List.length patterns) args), [], drop (List.length patterns) args))
 | WT.ELambda (_, patterns, _, _, _), [] when List.length patterns = List.length args ->
  bind (checkArgs symbols args (List.map (fun _ -> None) args)) (fun (arguments, symbols) ->
   let returnType = Option.value expected ~default:(AST.TInferenceVar ("t$applied_lambda_return", "t$applied_lambda_return")) in
   bind (check symbols (Some (AST.TFunction (List.map fst arguments, returnType))) target) (fun (targetType, target, symbols) ->
    match targetType, NonEmptyList.tryFromList (List.map snd arguments) with AST.TFunction (_, result), Some args -> literal expected symbols result (C.Apply (target, args)) | _ -> Error "An indirect call requires at least one argument"))
 | _ when typeArgs <> [] -> Error "Type arguments require a named function"
 | _ -> bind (check symbols None target) (fun (targetType, target, symbols) -> match targetType with
  | AST.TFunction (parameters, returnType) when args <> [] && List.length args < List.length parameters ->
   bind (checkArgs symbols args (List.map Option.some (take (List.length args) parameters))) (fun (arguments, symbols) ->
    let targetId, symbols = C.allocateBinding "__partial_target" symbols in
    let captures, symbols = List.fold_left (fun (reversed, symbols) (index, (_, argument)) -> let id, symbols = C.allocateBinding ("__partial_capture_" ^ string_of_int index) symbols in (id, argument) :: reversed, symbols) ([], symbols) (List.mapi (fun index value -> index, value) arguments) in
    let captures = List.rev captures and remaining = drop (List.length args) parameters in
    let parameters, symbols = List.fold_left (fun (reversed, symbols) (index, typ) -> let id, symbols = C.allocateBinding ("__partial_arg_" ^ string_of_int index) symbols in let param : C.lambdaParameter = {pattern = C.LPVariable id; typ = C.checkedType typ} in (id, param) :: reversed, symbols) ([], symbols) (List.mapi (fun index typ -> index, typ) remaining) in
    let parameters = List.rev parameters in
    let arguments = NonEmptyList.fromList (List.map (fun (id, _) -> C.Local id) captures @ List.map (fun (id, _) -> C.Local id) parameters) in
    let lambda = C.Lambda (NonEmptyList.fromList (List.map snd parameters), Some (C.checkedType returnType), C.Apply (C.Local targetId, arguments)) in
    let partial = List.fold_right (fun (id, argument) body -> C.Let (C.LPVariable id, argument, body)) captures lambda in
    literal expected symbols (AST.TFunction (remaining, returnType)) (C.Let (C.LPVariable targetId, target, partial)))
  | AST.TFunction (parameters, returnType) when List.length parameters = List.length args ->
   bind (checkArgs symbols args (List.map Option.some parameters)) (fun (arguments, symbols) -> match NonEmptyList.tryFromList (List.map snd arguments) with Some arguments -> literal expected symbols returnType (C.Apply (target, arguments)) | None -> Error "An indirect call requires at least one argument")
  | AST.TFunction _ -> Error "Indirect call argument count mismatch"
  | _ -> Error "Expression is not callable")
