(* ClosureComparisons.ml - Plan equality for lifted closures and their captures. *)
[@@@warning "-4"]

module C = CheckedAST
module A = ClosureAnalysis
module S = SpecializationIdentity
module B = C.BindingIdMap
module BS = A.BindingSet
module CM = A.ComparisonMap

type lambdaComparisonPlan = {
  identity : AST.functionId option;
  captureNames : AST.bindingId list;
  captureTypes : AST.semanticType list;
  captureExprs : C.expr list;
  body : C.expr;
  compareCaptures : bool;
}

let comparisonNameForIdentity identity captures state =
  match identity with
  | None ->
      let name, state = A.freshLiftedName state "__closure_comparison_" in
      (name, true, state)
  | Some identity -> (
      let key = (identity, captures) in
      match CM.find_opt key state.A.comparisonFuncs with
      | Some name -> (name, false, state)
      | None ->
          let name, state = A.freshLiftedName state "__closure_comparison_" in
          ( name,
            true,
            {
              state with
              A.comparisonFuncs = CM.add key name state.A.comparisonFuncs;
            } ))

(* Recognize the lambdas synthesized for named partial application. Their
   already-applied arguments are semantic identity, unlike ordinary lexical
   captures, and must therefore become explicit closure payload slots. *)
let planLambdaComparison parameters body state =
  let parameterBindings =
    NonEmptyList.toList parameters |> List.concat_map S.lambdaParameterBindings
  in
  let parameterNames = List.map fst parameterBindings in
  let simpleNames =
    List.fold_left
      (fun names (parameter : C.lambdaParameter) ->
        match (names, parameter.C.pattern) with
        | Some names, C.LPVariable name -> Some (name :: names)
        | _ -> None)
      (Some [])
      (NonEmptyList.toList parameters)
    |> Option.map List.rev
  in
  let tryNamedPartial target args rebuild =
    let arguments = NonEmptyList.toList args in
    let count =
      List.length arguments - List.length (NonEmptyList.toList parameters)
    in
    let synthesized =
      Option.fold ~none:false
        ~some:
          (List.for_all (fun id ->
               Option.fold ~none:false
                 ~some:(String.starts_with ~prefix:"__partial_")
                 (C.bindingName id state.A.symbols)))
        simpleNames
    in
    if count <= 0 || not synthesized then None
    else
      let provided = List.take count arguments in
      let remaining = List.drop count arguments in
      let suffix =
        Option.fold ~none:false
          ~some:(fun names ->
            List.for_all2 (fun id arg -> arg = C.Local id) names remaining)
          simpleNames
      in
      match (suffix, FunctionIdMap.tryFind target state.A.funcParams) with
      | true, Some targetParams when List.length targetParams >= count ->
          let captures, state =
            List.fold_left
              (fun (names, state) index ->
                let id, symbols =
                  C.allocateBinding
                    ("__comparison_applied_" ^ string_of_int index)
                    state.A.symbols
                in
                (id :: names, { state with A.symbols }))
              ([], state) (List.init count Fun.id)
          in
          let captures = List.rev captures in
          let replacement =
            List.map (fun id -> C.Local id) captures @ remaining
            |> S.exprArgsFromList
          in
          Some
            ( {
                identity = Some target;
                captureNames = captures;
                captureTypes = List.take count targetParams;
                captureExprs = provided;
                body = rebuild replacement;
                compareCaptures = true;
              },
              state )
      | _ -> None
  in
  let partial =
    match body with
    | C.Call (target, args) ->
        tryNamedPartial target args (fun args -> C.Call (target, args))
    | C.TypeApp (target, types, args) ->
        tryNamedPartial target args (fun args ->
            C.TypeApp (target, types, args))
    | _ -> None
  in
  match partial with
  | Some plan -> Ok plan
  | None ->
      let parameterSet = BS.of_list parameterNames in
      let parameterSet =
        match state.A.recursiveSelf with
        | Some (_, closure, _, _) -> BS.add closure parameterSet
        | None -> parameterSet
      in
      let captures =
        A.freeVars body parameterSet
        |> BS.filter (fun name -> B.mem name state.A.typeEnv)
        |> BS.elements
      in
      let rec collect remaining acc =
        match remaining with
        | [] -> Ok (List.rev acc)
        | name :: rest -> (
            match B.find_opt name state.A.typeEnv with
            | Some typ -> collect rest (typ :: acc)
            | None -> Error "Missing type for captured variable identity")
      in
      Result.map
        (fun types ->
          ( {
              identity = None;
              captureNames = captures;
              captureTypes = types;
              captureExprs = List.map (fun id -> C.Local id) captures;
              body;
              compareCaptures = false;
            },
            state ))
        (collect captures [])

let comparisonForCapturedValue symbols _variants typ left right =
  let structural =
    match typ with
    | AST.TList _ | AST.TDict _ | AST.TTuple _ | AST.TRecord _ | AST.TSum _ ->
        true
    | _ -> false
  in
  let resolved name =
    match C.tryFindFunctionId name symbols with
    | Some id -> id
    | None ->
        Crash.crash
          ("Closure comparison function '" ^ name ^ "' is absent from symbols")
  in
  (* Named partials can capture callbacks whose equality was never requested
    before lambda lifting. Their operands here are pure closure-slot reads.
    Use the same comparator-identity guard as EqualityHelpers, so a late
    callback capture does not require a missing global helper definition. *)
  if match typ with AST.TFunction _ -> true | _ -> false then
    let comparator value = C.TupleAccess (value, 1) in
    C.If
      ( C.BinOp (AST.Eq, comparator left, comparator right),
        C.IndirectApply (comparator left, S.exprArgsFromList [ left; right ]),
        C.BoolLiteral false )
  else if structural then
    C.Call
      ( resolved (ComparisonPlanning.eqHelperName typ),
        S.exprArgsFromList [ left; right ] )
  else if typ = AST.TString then C.BinOp (AST.Eq, left, right)
  else if typ = AST.TInt then
    C.Call
      ( resolved "Darklang.Stdlib.Int.__equals",
        S.exprArgsFromList [ left; right ] )
  else C.BinOp (AST.Eq, left, right)

let makeClosureComparator name captures compareCaptures variants symbols =
  let comparatorStorageType = AST.TInternalRawPtr in
  let runtimeClosureType =
    AST.TTuple (AST.TInt64 :: comparatorStorageType :: captures)
  in
  let left, symbols = C.allocateBinding "__comparison_left_closure" symbols in
  let right, symbols = C.allocateBinding "__comparison_right_closure" symbols in
  let comparisons =
    if compareCaptures then
      List.mapi
        (fun index typ ->
          comparisonForCapturedValue symbols variants typ
            (C.TupleAccess (C.Local left, index + 2))
            (C.TupleAccess (C.Local right, index + 2)))
        captures
    else []
  in
  let body =
    match comparisons with
    | [] -> C.BoolLiteral true
    | first :: rest ->
        List.fold_left (fun acc item -> C.BinOp (AST.And, acc, item)) first rest
  in
  let id, symbols = C.internFunction name symbols in
  ( {
      C.id;
      name;
      typeParams = [];
      params =
        C.checkedParams
          (S.paramsFromList "makeClosureComparator"
             [ (left, runtimeClosureType); (right, runtimeClosureType) ]);
      returnType = C.checkedType AST.TBool;
      body;
      recursion = None;
    },
    symbols )

(* Replace references already resolved to a singleton recursive binder with
   the closure value passed to its lifted code. This creates no closure-to-self
   capture edge: the operational closure parameter is reused directly. *)
let rec rewriteRecursiveSelfReferences self closure expr =
  let recurse = rewriteRecursiveSelfReferences self closure in
  match expr with
  | C.Local id when AST.compareBindingId id self = 0 -> C.Local closure
  | C.Apply (C.Local id, args) when AST.compareBindingId id self = 0 ->
      C.Apply (C.Local closure, NonEmptyList.map recurse args)
  | C.Let (pattern, value, body) -> C.Let (pattern, recurse value, recurse body)
  | C.RecursiveLet (recursion, value, body)
    when AST.compareBindingId (C.recursiveBindingId recursion) self = 0 -> (
      match C.recursiveBindingAvailability recursion with
      | AST.OrdinaryBinding -> C.RecursiveLet (recursion, recurse value, body)
      | _ -> expr)
  | C.RecursiveLet (recursion, value, body) ->
      C.RecursiveLet (recursion, recurse value, recurse body)
  | C.Lambda (parameters, annotation, body) ->
      C.Lambda (parameters, annotation, recurse body)
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
  | C.BoundaryRender (renderer, value) ->
      C.BoundaryRender (renderer, recurse value)
  | C.BinOp (operation, left, right) ->
      C.BinOp (operation, recurse left, recurse right)
  | C.UnaryOp (operation, value) -> C.UnaryOp (operation, recurse value)
  | C.If (condition, yes, no) ->
      C.If (recurse condition, recurse yes, recurse no)
  | C.Sequence (first, next) -> C.Sequence (recurse first, recurse next)
  | C.Call (target, args) -> C.Call (target, NonEmptyList.map recurse args)
  | C.TypeApp (target, parameters, args) ->
      C.TypeApp (target, parameters, NonEmptyList.map recurse args)
  | C.TupleLiteral elements ->
      C.TupleLiteral (C.mapTupleElements recurse elements)
  | C.TupleAccess (value, index) -> C.TupleAccess (recurse value, index)
  | C.DictLiteral (key, value, entries) ->
      C.DictLiteral
        ( key,
          value,
          List.map (fun (key, value) -> (recurse key, recurse value)) entries )
  | C.RecordLiteral (reference, fields) ->
      C.RecordLiteral (reference, C.mapRecordFields recurse fields)
  | C.RecordUpdate (record, fields) ->
      C.RecordUpdate
        ( recurse record,
          List.map (fun (name, value) -> (name, recurse value)) fields )
  | C.RecordAccess (record, field) -> C.RecordAccess (recurse record, field)
  | C.Constructor (reference, fields) ->
      C.Constructor (reference, List.map recurse fields)
  | C.ListLiteral values -> C.ListLiteral (List.map recurse values)
  | C.Apply (target, args) ->
      C.Apply (recurse target, NonEmptyList.map recurse args)
  | C.IndirectApply (target, args) ->
      C.IndirectApply (recurse target, NonEmptyList.map recurse args)
  | C.Closure (target, captures) -> C.Closure (target, List.map recurse captures)
  | C.InterpolatedString parts ->
      C.InterpolatedString
        (List.map
           (function
             | C.StringText _ as part -> part
             | C.StringExpr value -> C.StringExpr (recurse value))
           parts)
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
  | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
  | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
  | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
  | C.CharLiteral _ | C.FloatLiteral _ | C.RuntimeError _ | C.Local _
  | C.FuncRef _ | C.GenericFuncRef _ ->
      expr

(* Once the lifted member has a code identity, recursive closure calls become
   direct calls with the existing group environment as their first argument. *)
let rec rewriteLiftedSelfCalls lifted closure captures expr =
  let recurse = rewriteLiftedSelfCalls lifted closure captures in
  match expr with
  | C.Apply (C.Local id, args) when AST.compareBindingId id closure = 0 ->
      C.Call
        ( lifted,
          NonEmptyList.cons (C.Local closure) (NonEmptyList.map recurse args) )
  | C.Local id when AST.compareBindingId id closure = 0 ->
      (* The operational parameter has a tuple layout type. A source-level
         function value needs a closure with its semantic function type so
         containers and ownership use the function's release descriptor. *)
      C.Closure (lifted, captures)
  | C.Let (pattern, value, body) -> C.Let (pattern, recurse value, recurse body)
  | C.RecursiveLet (recursion, value, body) ->
      C.RecursiveLet (recursion, recurse value, recurse body)
  | C.Lambda (parameters, annotation, body) ->
      C.Lambda (parameters, annotation, recurse body)
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
  | C.BoundaryRender (renderer, value) ->
      C.BoundaryRender (renderer, recurse value)
  | C.BinOp (operation, left, right) ->
      C.BinOp (operation, recurse left, recurse right)
  | C.UnaryOp (operation, value) -> C.UnaryOp (operation, recurse value)
  | C.If (condition, yes, no) ->
      C.If (recurse condition, recurse yes, recurse no)
  | C.Sequence (first, next) -> C.Sequence (recurse first, recurse next)
  | C.Call (target, args) -> C.Call (target, NonEmptyList.map recurse args)
  | C.TypeApp (target, parameters, args) ->
      C.TypeApp (target, parameters, NonEmptyList.map recurse args)
  | C.TupleLiteral elements ->
      C.TupleLiteral (C.mapTupleElements recurse elements)
  | C.TupleAccess (value, index) -> C.TupleAccess (recurse value, index)
  | C.DictLiteral (key, value, entries) ->
      C.DictLiteral
        ( key,
          value,
          List.map (fun (key, value) -> (recurse key, recurse value)) entries )
  | C.RecordLiteral (reference, fields) ->
      C.RecordLiteral (reference, C.mapRecordFields recurse fields)
  | C.RecordUpdate (record, fields) ->
      C.RecordUpdate
        ( recurse record,
          List.map (fun (name, value) -> (name, recurse value)) fields )
  | C.RecordAccess (record, field) -> C.RecordAccess (recurse record, field)
  | C.Constructor (reference, fields) ->
      C.Constructor (reference, List.map recurse fields)
  | C.ListLiteral values -> C.ListLiteral (List.map recurse values)
  | C.Apply (target, args) ->
      C.Apply (recurse target, NonEmptyList.map recurse args)
  | C.IndirectApply (target, args) ->
      C.IndirectApply (recurse target, NonEmptyList.map recurse args)
  | C.Closure (target, captures) -> C.Closure (target, List.map recurse captures)
  | C.InterpolatedString parts ->
      C.InterpolatedString
        (List.map
           (function
             | C.StringText _ as part -> part
             | C.StringExpr value -> C.StringExpr (recurse value))
           parts)
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
  | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
  | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
  | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
  | C.CharLiteral _ | C.FloatLiteral _ | C.RuntimeError _ | C.Local _
  | C.FuncRef _ | C.GenericFuncRef _ ->
      expr
(* Lift lambdas in an expression, returning (transformed expr, new state) *)
