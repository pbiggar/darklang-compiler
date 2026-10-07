(*
   CheckLambdas.ml - Check Lambda expressions while preserving source diagnostics and order.
*)
(* CheckLambdas.ml - Check Lambda expressions while preserving source diagnostics and order. *)
open! AST
open CheckingDiagnostics
module M = StringOrder.Map
module S = StringOrder.Set

(*
   Interpreter lambdas may expose a prefix of their binders as
   the callable expected by a higher-order argument. Preserve
   the remaining binders as a returned lambda so applying the
   outer closure once has the same curried behavior.
   Bottom has no runtime payload representation. Unit is
   the canonical monomorphic witness when the result is
   otherwise unconstrained.
*)
let check checkExpr env registry lookup generic warnings modules aliases
    expected parameters annotation body =
  let ( let* ) = Result.bind in
  let params = NonEmptyList.toList parameters in
  let names =
    S.of_list
      (List.concat_map
         (fun (param : AST.lambdaParameter) ->
           AST.letPatternBindings param.pattern)
         params)
  in
  let filter predicate = function
    | Some value when predicate value -> Some value
    | Some _ | None -> None
  in
  let concrete value =
    let typ =
      match value with
      | UnitLiteral -> Some TUnit
      | Int64Literal _ -> Some TInt64
      | Int128Literal _ -> Some TInt128
      | BigIntLiteral _ -> Some TInt
      | Int8Literal _ -> Some TInt8
      | Int16Literal _ -> Some TInt16
      | Int32Literal _ -> Some TInt32
      | UInt8Literal _ -> Some TUInt8
      | UInt16Literal _ -> Some TUInt16
      | UInt32Literal _ -> Some TUInt32
      | UInt64Literal _ -> Some TUInt64
      | UInt128Literal _ -> Some TUInt128
      | FloatLiteral _ -> Some TFloat64
      | BoolLiteral _ -> Some TBool
      | StringLiteral _ -> Some TString
      | CharLiteral _ -> Some TChar
      | Var name -> M.find_opt name env
      | Apply (Var name, _, _) -> (
          match M.find_opt name env with
          | Some (TFunction (_, ret)) -> Some ret
          | _ ->
              Option.map
                (fun ((func : AST.moduleFunc), _) -> func.returnType)
                (DarkStdlib.tryGetFunction modules name))
      | _ -> None
    in
    filter (fun typ -> not (Unification.containsTVar typ)) typ
  in
  let add name typ constraints =
    if S.mem name names && not (Unification.containsTVar typ) then
      match M.find_opt name constraints with
      | Some existing -> (
          match Unification.reconcileTypes (Some aliases) existing typ with
          | Some typ -> M.add name typ constraints
          | None -> constraints)
      | None -> M.add name typ constraints
    else constraints
  in
  let first left right = match left with Some _ -> left | None -> right () in
  let rec collect expected value constraints =
    let children values constraints =
      List.fold_left
        (fun acc value -> collect None value acc)
        constraints values
    in
    let arguments func args constraints =
      let values = NonEmptyList.toList args in
      let constraints = collect None func constraints in
      match func with
      | Var name -> (
          match M.find_opt name env with
          | Some (TFunction (types, _))
            when List.length values <= List.length types ->
              List.fold_left
                (fun acc (typ, value) -> collect (Some typ) value acc)
                constraints
                (List.combine (List.take (List.length values) types) values)
          | _ -> children values constraints)
      | _ -> children values constraints
    in
    match value with
    | Var name -> (
        match expected with
        | Some typ -> add name typ constraints
        | None -> constraints)
    | BinOp (op, left, right) ->
        let operand =
          match op with
          | Add | Sub | Mul | Div | Mod | Pow | Shl | Shr | BitAnd | BitOr
          | BitXor ->
              first
                (filter
                   (fun typ -> not (Unification.containsTVar typ))
                   expected)
                (fun () -> first (concrete left) (fun () -> concrete right))
          | Lt | Gt | Lte | Gte | Eq | Neq ->
              first (concrete left) (fun () -> concrete right)
          | StringConcat -> Some TString
          | And | Or -> Some TBool
        in
        collect operand right (collect operand left constraints)
    | UnaryOp (Not, value) -> collect (Some TBool) value constraints
    | UnaryOp (_, value) -> collect expected value constraints
    | Apply (Var name, _, args) -> (
        let values = NonEmptyList.toList args in
        match M.find_opt name env with
        | Some (TFunction (types, _))
          when List.length types = List.length values ->
            List.fold_left
              (fun acc (typ, value) -> collect (Some typ) value acc)
              constraints
              (List.combine types values)
        | _ -> children values constraints)
    | Apply (func, _, args) | IndirectApply (func, args) ->
        arguments func args constraints
    | Let (pattern, value, continuation) ->
        let constraints = collect None value constraints in
        if
          List.exists
            (fun name -> S.mem name names)
            (AST.letPatternBindings pattern)
        then constraints
        else collect expected continuation constraints
    | RecursiveLet (_, value, continuation) ->
        collect expected continuation (collect None value constraints)
    | Lambda (params, _, body) ->
        let shadows =
          List.exists
            (fun (param : AST.lambdaParameter) ->
              List.exists
                (fun name -> S.mem name names)
                (AST.letPatternBindings param.pattern))
            (NonEmptyList.toList params)
        in
        if shadows then constraints
        else
          let ret =
            match expected with
            | Some (TFunction (_, ret)) -> Some ret
            | _ -> None
          in
          collect ret body constraints
    | BoundaryRender (_, value)
    | TupleAccess (value, _)
    | RecordAccess (value, _) ->
        collect None value constraints
    | Sequence (first, next) ->
        collect expected next (collect None first constraints)
    | If (condition, yes, no) ->
        collect expected no
          (collect expected yes (collect (Some TBool) condition constraints))
    | TupleLiteral values | ListLiteral values -> children values constraints
    | DictLiteral (_, _, entries) ->
        children
          (List.concat_map (fun (key, value) -> [ key; value ]) entries)
          constraints
    | RecordLiteral (_, fields) -> children (List.map snd fields) constraints
    | RecordUpdate (record, fields) ->
        children (record :: List.map snd fields) constraints
    | Constructor (_, _, fields) -> children fields constraints
    | Match (scrutinee, cases) ->
        List.fold_left
          (fun acc (case : AST.matchCase) ->
            let acc =
              match case.guard with
              | Some guard -> collect (Some TBool) guard acc
              | None -> acc
            in
            collect expected case.body acc)
          (collect None scrutinee constraints)
          cases
    | Closure (_, captures) -> children captures constraints
    | InterpolatedString parts ->
        children
          (List.filter_map
             (function StringExpr value -> Some value | StringText _ -> None)
             parts)
          constraints
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
    | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
    | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _
    | RuntimeError _ ->
        constraints
  in
  let constraints = collect annotation body M.empty in
  let rec refine pattern typ =
    match (pattern, typ) with
    | LPVariable name, _ ->
        Option.value (M.find_opt name constraints) ~default:typ
    | LPTuple (first, second, rest), TTuple types ->
        let patterns = first :: second :: rest in
        if List.length patterns = List.length types then
          TTuple (List.map2 refine patterns types)
        else typ
    | _ -> typ
  in
  let typeCheck resolved bodyExpected =
    let bindings =
      List.map
        (fun ((param : AST.lambdaParameter), typ) ->
          bindLetPatternTypes param.pattern typ)
        resolved
    in
    if List.exists Option.is_none bindings then
      Error
        (GenericError
           "Lambda parameter pattern is incompatible with its inferred type")
    else
      let bindings = List.concat (List.filter_map Fun.id bindings) in
      let env =
        List.fold_left (fun env (name, typ) -> M.add name typ env) env bindings
      in
      let effective =
        match annotation with Some _ -> annotation | None -> bodyExpected
      in
      let* bodyType, body =
        checkExpr body env registry lookup generic warnings modules aliases
          effective
      in
      let types = List.map snd resolved in
      let params =
        NonEmptyList.fromList
          (List.map
             (fun ((param : AST.lambdaParameter), typ) ->
               { param with inferredType = Some typ })
             resolved)
      in
      Ok (TFunction (types, bodyType), Lambda (params, annotation, body))
  in
  let initial =
    List.mapi
      (fun index (param : AST.lambdaParameter) ->
        let initial =
          match param.inferredType with
          | Some typ -> typ
          | None -> (
              match param.sourceAnnotation with
              | Some typ -> typ
              | None ->
                  inferredLetPatternType
                    ("lambda_" ^ string_of_int index)
                    param.pattern)
        in
        (param, refine param.pattern initial))
      params
  in
  match expected with
  | Some (TFunction (types, ret)) -> (
      if List.length types < List.length params then
        let outer = List.take (List.length types) params
        and remaining = List.drop (List.length types) params in
        match
          (NonEmptyList.tryFromList outer, NonEmptyList.tryFromList remaining)
        with
        | Some outer, Some remaining ->
            checkExpr
              (Lambda (outer, None, Lambda (remaining, annotation, body)))
              env registry lookup generic warnings modules aliases expected
        | _ ->
            Error
              (GenericError "Lambda currying requires non-empty binder groups")
      else if List.length types <> List.length params then
        Error
          (GenericError
             ("Expected "
             ^ string_of_int (List.length params)
             ^ " arguments, got "
             ^ string_of_int (List.length types)))
      else
        let rec reconcile remaining acc =
          match remaining with
          | [] -> Ok (List.rev acc)
          | ((param, declared), expected) :: rest -> (
              match
                Unification.reconcileTypes (Some aliases) declared expected
              with
              | Some typ -> reconcile rest ((param, typ) :: acc)
              | None ->
                  Error
                    (TypeMismatch (expected, declared, "lambda parameter type"))
              )
        in
        let* resolved = reconcile (List.combine initial types) [] in
        let bodyExpected =
          if Unification.containsTVar ret then None else Some ret
        in
        let* funcType, expr = typeCheck resolved bodyExpected in
        match funcType with
        | TFunction (params, bodyType) -> (
            match Unification.reconcileTypes (Some aliases) ret bodyType with
            | None -> Error (TypeMismatch (ret, bodyType, "lambda return type"))
            | Some typ ->
                let typ =
                  if bodyType = TNever && Unification.containsTVar ret then
                    TUnit
                  else typ
                in
                Ok (TFunction (params, typ), expr))
        | _ ->
            Error
              (GenericError
                 "Internal error: lambda did not type-check to a function"))
  | Some other -> (
      let* funcType, expr = typeCheck initial None in
      match Unification.reconcileTypes (Some aliases) other funcType with
      | Some typ -> Ok (typ, expr)
      | None -> Error (TypeMismatch (other, funcType, "lambda")))
  | None -> typeCheck initial None
[@@warning "-4"]
