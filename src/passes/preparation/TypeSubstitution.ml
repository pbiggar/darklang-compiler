(*
   TypeSubstitution.ml - Substitute concrete source types and instantiate generic function bodies.
*)
[@@@warning "-4"]

module M = StringOrder.Map
module S = StringOrder.Set
module C = CheckedAST
module R = TypeRegistries

type substitution = AST.semanticType M.t

(*
   Apply a substitution to a type, replacing type variables with concrete types
   Unbound type variable remains as-is
   Concrete types are unchanged
*)
let rec applySubstToType subst typ =
  let recurse = applySubstToType subst in
  match typ with
  | AST.TVar name | AST.TInferenceVar (_, name) ->
      Option.value (M.find_opt name subst) ~default:typ
  | AST.TFunction (parameters, result) ->
      AST.TFunction (List.map recurse parameters, recurse result)
  | AST.TTuple values -> AST.TTuple (List.map recurse values)
  | AST.TList value -> AST.TList (recurse value)
  | AST.TStream value -> AST.TStream (recurse value)
  | AST.TDict (key, value) -> AST.TDict (recurse key, recurse value)
  | AST.TRecord (name, values) -> AST.TRecord (name, List.map recurse values)
  | AST.TSum (name, values) -> AST.TSum (name, List.map recurse values)
  | _ -> typ

(*
   Native record layouts have one keyed slot per field. When a declaration
   repeats a name, the interpreter's head-first lookup makes the first
   declaration authoritative and the later declarations do not add slots.
*)
let firstDeclaredRecordFields fields =
  let _, result =
    List.fold_left
      (fun (seen, retained) ((name, _) as field) ->
        if S.mem name seen then (seen, retained)
        else (S.add name seen, field :: retained))
      (S.empty, []) fields
  in
  List.rev result

let buildDeclaredRecordFieldSubst (info : R.recordTypeInfo) args =
  if List.length info.R.typeParams = List.length args then
    Some (M.of_list (List.combine info.R.typeParams args))
  else None

let recordDescriptor name args (info : R.recordTypeInfo) =
  let fields =
    match buildDeclaredRecordFieldSubst info args with
    | Some subst ->
        List.map
          (fun (name, typ) -> (name, applySubstToType subst typ))
          info.R.fields
    | None -> info.R.fields
  in
  {
    ANF.sourceTypeName = name;
    runtimeTypeName = name;
    typeArgs = args;
    fields;
    valueType = AST.TRecord (name, args);
  }

let boxedSumDescriptor name parameters args fields =
  if List.length parameters <> List.length args then
    Error ("Boxed sum '" ^ name ^ "' has inconsistent type arguments")
  else
    let subst = M.of_list (List.combine parameters args) in
    let payload =
      match List.map (applySubstToType subst) fields with
      | [] -> AST.TInt64
      | [ value ] -> value
      | values -> AST.TTuple values
    in
    Ok
      {
        ANF.sourceTypeName = name;
        runtimeTypeName = name;
        typeArgs = args;
        fields = [ ("$tag", AST.TInt64); ("$payload", payload) ];
        valueType = AST.TSum (name, args);
      }

(*
   Match a type pattern (may contain type variables) against a concrete type.
   ANF-side inference may observe unresolved constructor type args.
   Treat unconstrained actual type variables as compatible placeholders.
*)
let rec matchTypePattern pattern actual =
  let same left right = AST.compareSemanticType left right = 0 in
  let combine results =
    List.fold_left
      (fun accumulated result ->
        match (accumulated, result) with
        | Ok values, Ok next -> Ok (values @ next)
        | Error error, _ | _, Error error -> Error error)
      (Ok []) results
  in
  let pairs patterns actuals =
    List.map
      (fun (pattern, actual) -> matchTypePattern pattern actual)
      (List.combine patterns actuals)
  in
  match pattern with
  | AST.TVar name | AST.TInferenceVar (_, name) ->
      if same actual pattern then Ok [] else Ok [ (name, actual) ]
  | _ -> (
      match actual with
      | AST.TVar _ | AST.TInferenceVar _ -> Ok []
      | _ -> (
          match pattern with
          | AST.TFunction (parameters, result) -> (
              match actual with
              | AST.TFunction (actualParameters, actualResult)
                when List.length parameters = List.length actualParameters ->
                  let parameters = pairs parameters actualParameters in
                  let result = matchTypePattern result actualResult in
                  combine (parameters @ [ result ])
              | _ -> Error "Function type mismatch")
          | AST.TTuple elements -> (
              match actual with
              | AST.TTuple actualElements
                when List.length elements = List.length actualElements ->
                  combine (pairs elements actualElements)
              | _ -> Error "Tuple type mismatch")
          | AST.TRecord (name, args) -> (
              match actual with
              | AST.TRecord (actualName, actualArgs)
                when name = actualName
                     && List.length args = List.length actualArgs ->
                  combine (pairs args actualArgs)
              | _ -> Error "Record type mismatch")
          | AST.TSum (name, args) -> (
              match actual with
              | AST.TSum (actualName, actualArgs)
                when name = actualName
                     && List.length args = List.length actualArgs ->
                  combine (pairs args actualArgs)
              | _ ->
                  Error
                    ("Sum type mismatch: expected "
                    ^ LoweringPrimitives.typeToString pattern
                    ^ ", got "
                    ^ LoweringPrimitives.typeToString actual))
          | AST.TList value -> (
              match actual with
              | AST.TList actualValue -> matchTypePattern value actualValue
              | _ -> Error "List type mismatch")
          | AST.TDict (key, value) -> (
              match actual with
              | AST.TDict (actualKey, actualValue) ->
                  let keys = matchTypePattern key actualKey in
                  let values = matchTypePattern value actualValue in
                  combine [ keys; values ]
              | _ -> Error "Dict type mismatch")
          | _ -> if same pattern actual then Ok [] else Error "Type mismatch"))

(*
   Consolidate type variable bindings, preferring concrete types when both appear.
*)
let consolidateTypeBindings bindings =
  List.fold_left
    (fun accumulated (name, typ) ->
      Result.bind accumulated (fun mapping ->
          match M.find_opt name mapping with
          | None -> Ok (M.add name typ mapping)
          | Some existing ->
              if AST.compareSemanticType existing typ = 0 then Ok mapping
              else if
                SpecializationIdentity.containsTypeVar existing
                && not (SpecializationIdentity.containsTypeVar typ)
              then Ok (M.add name typ mapping)
              else if
                SpecializationIdentity.containsTypeVar typ
                && not (SpecializationIdentity.containsTypeVar existing)
              then Ok mapping
              else if
                SpecializationIdentity.containsTypeVar existing
                && SpecializationIdentity.containsTypeVar typ
              then Ok mapping
              else
                Error
                  ("Type variable " ^ name ^ " has conflicting inferences: "
                  ^ LoweringPrimitives.typeToString existing
                  ^ " vs "
                  ^ LoweringPrimitives.typeToString typ)))
    (Ok M.empty) bindings

(*
   Apply a substitution to an expression, replacing type variables in type annotations
   No types to substitute in literals, variables, function references, and closures
   Substitute in type arguments and value arguments
   Substitute types in parameter annotations and body
*)
let rec applySubstToExpr subst expr =
  let recurse = applySubstToExpr subst in
  let checked typ =
    C.checkedType (applySubstToType subst (C.semanticType typ))
  in
  let types values =
    C.checkedTypeArgs
      (List.map (applySubstToType subst) (C.semanticTypeArgs values))
  in
  match expr with
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
  | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
  | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
  | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
  | C.CharLiteral _ | C.FloatLiteral _ | C.Local _ | C.FuncRef _ | C.Closure _
  | C.RuntimeError _ ->
      expr
  | C.BoundaryRender (renderer, value) ->
      C.BoundaryRender (renderer, recurse value)
  | C.BinOp (operation, left, right) ->
      C.BinOp (operation, recurse left, recurse right)
  | C.UnaryOp (operation, value) -> C.UnaryOp (operation, recurse value)
  | C.Let (pattern, value, body) -> C.Let (pattern, recurse value, recurse body)
  | C.RecursiveLet (recursion, value, body) ->
      C.RecursiveLet (recursion, recurse value, recurse body)
  | C.If (condition, yes, no) ->
      C.If (recurse condition, recurse yes, recurse no)
  | C.Sequence (first, next) -> C.Sequence (recurse first, recurse next)
  | C.Call (target, args) -> C.Call (target, NonEmptyList.map recurse args)
  | C.TypeApp (target, parameters, args) ->
      C.TypeApp (target, types parameters, NonEmptyList.map recurse args)
  | C.TupleLiteral elements ->
      C.TupleLiteral (C.mapTupleElements recurse elements)
  | C.TupleAccess (value, index) -> C.TupleAccess (recurse value, index)
  | C.DictLiteral (key, value, entries) ->
      C.DictLiteral
        ( checked key,
          checked value,
          List.map (fun (key, value) -> (recurse key, recurse value)) entries )
  | C.RecordLiteral (reference, fields) ->
      C.RecordLiteral
        ( { reference with C.typeArgs = types reference.C.typeArgs },
          C.mapRecordFields recurse fields )
  | C.RecordUpdate (record, fields) ->
      C.RecordUpdate
        ( recurse record,
          List.map (fun (name, value) -> (name, recurse value)) fields )
  | C.RecordAccess (record, field) -> C.RecordAccess (recurse record, field)
  | C.Constructor (reference, fields) ->
      C.Constructor
        ( { reference with C.typeArgs = types reference.C.typeArgs },
          List.map recurse fields )
  | C.Match (value, cases) ->
      C.Match
        ( recurse value,
          NonEmptyList.map
            (fun (case : C.matchCase) ->
              {
                case with
                C.guard = Option.map recurse case.C.guard;
                body = recurse case.C.body;
              })
            cases )
  | C.ListLiteral values -> C.ListLiteral (List.map recurse values)
  | C.Lambda (parameters, annotation, body) ->
      C.Lambda
        ( NonEmptyList.map
            (fun (parameter : C.lambdaParameter) ->
              { parameter with C.typ = checked parameter.C.typ })
            parameters,
          Option.map checked annotation,
          recurse body )
  | C.Apply (target, args) ->
      C.Apply (recurse target, NonEmptyList.map recurse args)
  | C.IndirectApply (target, args) ->
      C.IndirectApply (recurse target, NonEmptyList.map recurse args)
  | C.InterpolatedString parts ->
      C.InterpolatedString
        (List.map
           (function
             | C.StringText _ as part -> part
             | C.StringExpr value -> C.StringExpr (recurse value))
           parts)

(*
   Resolve type aliases to their target types
*)
let rec resolveAliasType aliases typ =
  let recurse = resolveAliasType aliases in
  match typ with
  | AST.TRecord (name, []) | AST.TSum (name, []) -> (
      match M.find_opt name aliases with
      | Some ([], target) -> recurse target
      | _ -> typ)
  | AST.TSum (name, args) -> (
      match M.find_opt name aliases with
      | Some (parameters, target) ->
          if List.length parameters <> List.length args then typ
          else
            recurse
              (applySubstToType
                 (M.of_list (List.combine parameters args))
                 target)
      | None -> AST.TSum (name, List.map recurse args))
  | AST.TRecord (name, args) -> AST.TRecord (name, List.map recurse args)
  | AST.TTuple values -> AST.TTuple (List.map recurse values)
  | AST.TList value -> AST.TList (recurse value)
  | AST.TDict (key, value) -> AST.TDict (recurse key, recurse value)
  | AST.TFunction (args, result) ->
      AST.TFunction (List.map recurse args, recurse result)
  | _ -> typ

let resolveAliasesInTypeRegistry aliases registry =
  M.map
    (fun (info : R.recordTypeInfo) ->
      {
        info with
        R.fields =
          List.map
            (fun (name, typ) -> (name, resolveAliasType aliases typ))
            info.R.fields;
      })
    registry

(*
   Resolve type aliases within function signatures
*)
let resolveAliasesInFunction aliases (func : C.functionDef) =
  {
    func with
    C.params =
      NonEmptyList.map
        (fun (binding, typ) ->
          ( binding,
            C.checkedType (resolveAliasType aliases (C.semanticType typ)) ))
        func.C.params;
    returnType =
      C.checkedType (resolveAliasType aliases (C.functionReturnType func));
  }

(*
   Specialize a generic function definition with specific type arguments
   Build substitution from type parameters to type args
   Generate specialized name
   Apply substitution to parameters, return type, and body
   Specialized function has no type parameters
   Collect all TypeApp call sites from an expression
*)
let specializeFunction id (func : C.functionDef) args =
  let subst =
    if List.length func.C.typeParams <> List.length args then
      Crash.crash
        (Printf.sprintf
           "Specialization arity mismatch for %s: expected %d, got %d"
           func.C.name
           (List.length func.C.typeParams)
           (List.length args))
    else M.of_list (List.combine func.C.typeParams args)
  in
  let name = SpecializationIdentity.specName func.C.name args in
  let params =
    NonEmptyList.map
      (fun (binding, typ) ->
        (binding, C.checkedType (applySubstToType subst (C.semanticType typ))))
      func.C.params
  in
  let returnType =
    C.checkedType (applySubstToType subst (C.functionReturnType func))
  in
  let body = applySubstToExpr subst func.C.body in
  {
    C.id;
    name;
    typeParams = [];
    params;
    returnType;
    body;
    recursion = func.C.recursion;
  }
