(* ContextInference.ml - Continuation evidence from ContextInference.ml, retaining lexical scope and evaluation order. *)
open! AST
module M = StringOrder.Map
module T = Types
module U = Unification
type checker = AST.expr -> AST.semanticType option -> (AST.semanticType * AST.expr, CheckingDiagnostics.typeError) result
let orElse first alternative = match first with Some _ -> first | None -> alternative ()
let filter predicate = function Some value when predicate value -> Some value | Some _ | None -> None
let rec tryFindCallArguments target candidate =
 let children values = List.find_map (tryFindCallArguments target) values in
 let shadows pattern = List.mem target (AST.letPatternBindings pattern) in
 let patternShadows pattern = match AST.validateBinders (MatchBinderPattern pattern) with Ok names -> List.mem target names | Error _ -> true in
 match candidate with
 | Apply (Var name, _, args) when name = target -> Some (NonEmptyList.toList args)
 | Let (pattern, value, body) -> (match tryFindCallArguments target value with Some _ as args -> args | None when shadows pattern -> None | None -> tryFindCallArguments target body)
 | RecursiveLet (recursion, value, body) -> (match tryFindCallArguments target value with Some _ as args -> args | None when AST.recursiveBindingName recursion = target -> None | None -> tryFindCallArguments target body)
 | Lambda (params, _, body) -> if List.exists (fun (param : AST.lambdaParameter) -> shadows param.pattern) (NonEmptyList.toList params) then None else tryFindCallArguments target body
 | Match (scrutinee, cases) -> orElse (tryFindCallArguments target scrutinee) (fun () ->
   List.find_map (fun (case : AST.matchCase) -> let shadows = List.exists patternShadows (NonEmptyList.toList case.patterns) in
    let guard = if shadows then None else Option.bind case.guard (tryFindCallArguments target) in
    orElse guard (fun () -> if shadows then None else tryFindCallArguments target case.body)) cases)
 | BoundaryRender (_, value) | UnaryOp (_, value) | TupleAccess (value, _) | RecordAccess (value, _) -> tryFindCallArguments target value
 | BinOp (_, left, right) | Sequence (left, right) -> children [left; right]
 | If (condition, yes, no) -> children [condition; yes; no]
 | TupleLiteral values | ListLiteral values -> children values
 | DictLiteral (_, _, entries) -> children (List.concat_map (fun (key, value) -> [key; value]) entries)
 | RecordLiteral (_, fields) -> children (List.map snd fields) | RecordUpdate (record, fields) -> children (record :: List.map snd fields)
 | Constructor (_, _, fields) -> children fields
 | Apply (func, _, args) | IndirectApply (func, args) -> children (func :: NonEmptyList.toList args)
 | Closure (_, captures) -> children captures
 | InterpolatedString parts -> children (List.filter_map (function StringExpr value -> Some value | StringText _ -> None) parts)
 | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _ | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | RuntimeError _ -> None
let inferFunctionExpectationFromArguments checker count arguments =
 if List.length arguments <> count then None else
 ResultList.traverse (fun argument -> Result.map fst (checker argument None)) arguments |> Result.to_option |> Option.map (fun types -> TFunction (types, TVar "binding_return"))
let rec expectedTypeForNestedVariable aliases registry lookup target expected candidate =
 let recurse expected value = expectedTypeForNestedVariable aliases registry lookup target expected value in
 match candidate, T.resolveType aliases expected with
 | Var name, _ when name = target -> Some expected
 | TupleLiteral values, TTuple types when List.length values = List.length types -> List.find_map (fun (value, typ) -> recurse typ value) (List.combine values types)
 | ListLiteral values, TList typ -> List.find_map (recurse typ) values
 | RecordLiteral (_, fields), TRecord (name, args) ->
   (match M.find_opt name registry with None -> None | Some (info : T.recordTypeInfo) ->
    match T.buildRecordFieldSubstitutionFromParams info.T.typeParams args with Error _ -> None | Ok subst ->
     List.find_map (fun ((field : AST.recordFieldReference), value) -> Option.bind (M.find_opt field.sourceFieldName info.T.fieldTypes) (fun typ -> recurse (T.applyTypeArguments subst typ) value)) fields)
 | Constructor (reference, variant, fields), TSum (name, args) ->
   (match T.tryFindVariant reference variant lookup with Some (owner, params, _, types) when owner = name && List.length params = List.length args && List.length fields = List.length types ->
     let subst = M.of_list (List.combine params args) in List.find_map (fun (value, typ) -> recurse (T.applySubst subst typ) value) (List.combine fields types) | _ -> None)
 | If (_, yes, no), _ -> List.find_map (recurse expected) [yes; no]
 | Match (_, cases), _ -> List.find_map (fun (case : AST.matchCase) -> recurse expected case.body) cases
 | _ -> None
[@@warning "-4"]
let rec tryFindFunctionValueExpectation checker env registry lookup modules aliases target candidate =
 let recurse target value = tryFindFunctionValueExpectation checker env registry lookup modules aliases target value in
 let nested expected value = expectedTypeForNestedVariable aliases registry lookup target expected value in
 let children values = List.find_map (recurse target) values in
 let fromCall name arguments =
  let functionType = orElse (M.find_opt name env) (fun () -> Option.map (fun (func, _) -> DarkStdlib.getFunctionType func) (DarkStdlib.tryGetFunction modules name)) in
  match functionType with
  | Some (TFunction (params, _)) when List.length params = List.length arguments ->
    let pairs = List.combine params arguments in
    let bindings = ResultList.traverse (fun (typ, argument) ->
      if Option.is_some (nested typ argument) then Ok [] else
      let ( let* ) = Result.bind in let* actual, _ = checker argument None in
      match U.matchTypes typ actual with Ok bindings -> Ok bindings | Error _ -> Ok []) pairs
      |> Result.map List.concat |> (fun result -> Result.bind result (fun bindings -> U.consolidateBindings bindings |> Result.map_error (fun message -> CheckingDiagnostics.GenericError message))) |> Result.to_option |> Option.value ~default:M.empty in
    let found = List.find_map (fun (typ, argument) -> nested (T.applySubst bindings typ) argument) pairs in
    orElse found (fun () -> List.find_map (fun (_, argument) -> recurse target argument) pairs)
  | _ -> children arguments in
 let fromOther other value = Option.bind (Result.to_option (checker other None)) (fun (typ, _) -> nested typ value) in
 match candidate with
 | Apply (Var name, _, args) -> fromCall name (NonEmptyList.toList args)
 | Let (LPVariable name, value, body) when name <> target ->
   let found = Option.bind (recurse name body) (fun expected -> nested expected value) |> filter (fun typ -> not (U.containsTVar typ)) in
   orElse found (fun () -> orElse (recurse target value) (fun () -> recurse target body))
 | Let (pattern, value, body) -> orElse (recurse target value) (fun () -> if List.mem target (AST.letPatternBindings pattern) then None else recurse target body)
 | RecursiveLet (recursion, value, body) -> orElse (recurse target value) (fun () -> if AST.recursiveBindingName recursion = target then None else recurse target body)
 | Lambda (params, _, body) -> if List.exists (fun (param : AST.lambdaParameter) -> List.mem target (AST.letPatternBindings param.pattern)) (NonEmptyList.toList params) then None else recurse target body
 | BoundaryRender (_, value) | UnaryOp (_, value) | TupleAccess (value, _) | RecordAccess (value, _) -> recurse target value
 | BinOp ((Eq | Neq), left, right) -> orElse (fromOther right left) (fun () -> orElse (fromOther left right) (fun () -> children [left; right]))
 | BinOp (_, left, right) | Sequence (left, right) -> children [left; right]
 | If (condition, yes, no) -> orElse (fromOther no yes) (fun () -> orElse (fromOther yes no) (fun () -> children [condition; yes; no]))
 | TupleLiteral values | ListLiteral values -> children values
 | DictLiteral (_, _, entries) -> children (List.concat_map (fun (key, value) -> [key; value]) entries)
 | RecordLiteral (reference, fields) ->
   let found = match T.tryResolveRecordLiteralInfo aliases registry reference with None -> None | Some (_, args, info) ->
    (match T.buildRecordFieldSubstitutionFromParams info.T.typeParams args with Error _ -> None | Ok subst ->
      List.find_map (fun ((field : AST.recordFieldReference), value) -> Option.bind (M.find_opt field.sourceFieldName info.T.fieldTypes) (fun typ -> nested (T.applyTypeArguments subst typ) value)) fields) in
   orElse found (fun () -> children (List.map snd fields))
 | RecordUpdate (record, fields) -> children (record :: List.map snd fields)
 | Constructor (reference, variant, fields) ->
   let found = match T.tryFindVariant reference variant lookup with Some (_, params, _, types) when List.length fields = List.length types ->
    let args = match reference with ResolvedConstructor (_, _, args) when List.length args = List.length params -> args | _ -> List.map (fun name -> TVar name) params in
    let subst = M.of_list (List.combine params args) in List.find_map (fun (value, typ) -> nested (T.applySubst subst typ) value) (List.combine fields types) | _ -> None in
   orElse found (fun () -> children fields)
 | Match (scrutinee, cases) -> children (scrutinee :: List.concat_map (fun (case : AST.matchCase) -> Option.to_list case.guard @ [case.body]) cases)
 | Apply (func, _, args) | IndirectApply (func, args) -> children (func :: NonEmptyList.toList args)
 | Closure (_, captures) -> children captures
 | InterpolatedString parts -> children (List.filter_map (function StringExpr value -> Some value | StringText _ -> None) parts)
 | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _ | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | RuntimeError _ -> None
[@@warning "-4"]
