(* Monomorphization.ml - Solve reachable generic instances and replace type applications. *)
[@@@warning "-4"]

module C = CheckedAST
module A = ClosureAnalysis
module S = SpecializationIdentity
module P = LoweringPrimitives
module T = TypeSubstitution
module SS = S.SpecSet
module SM = S.SpecMap
module FS = S.FunctionSet
module M = StringOrder.Map
module Names = StringOrder.Set

let ( let* ) = Result.bind

let resolvedFunctionId symbols name =
  match C.tryFindFunctionId name symbols with
  | Some id -> id
  | None ->
      Crash.crash ("Resolved function '" ^ name ^ "' is absent from symbols")

let hasPredefinedKeyIntrinsic = function
  | AST.TInt64 | AST.TBool | AST.TFloat64 | AST.TString | AST.TBlob -> true
  | _ -> false

let ordinal id =
  let bits = AST.functionIdValue id in
  Z.to_string
    (if bits < 0L then Z.add (Z.of_int64 bits) (Z.shift_left Z.one 64)
     else Z.of_int64 bits)

(*
   Optimization: avoid building a Dict from an empty list when types are concrete.
*)
let collectTypeApps symbols expr =
  let resolve id =
    match C.functionName id symbols with
    | Some name -> name
    | None ->
        Crash.crash
          ("Type application function identity " ^ ordinal id
         ^ " is absent from symbols")
  in
  let rec visit specs expr =
    let many specs values = List.fold_left visit specs values in
    match expr with
    | C.GenericFuncRef (id, types, _) ->
        SS.add (resolve id, C.semanticTypeArgs types) specs
    | C.BoundaryRender (_, value)
    | C.UnaryOp (_, value)
    | C.TupleAccess (value, _)
    | C.RecordAccess (value, _)
    | C.Lambda (_, _, value) ->
        visit specs value
    | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
    | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
    | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
    | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
    | C.CharLiteral _ | C.FloatLiteral _ | C.Local _ | C.FuncRef _ | C.Closure _
    | C.RuntimeError _ ->
        specs
    | C.BinOp (_, left, right)
    | C.Let (_, left, right)
    | C.RecursiveLet (_, left, right)
    | C.Sequence (left, right) ->
        visit (visit specs left) right
    | C.If (condition, yes, no) -> visit (visit (visit specs condition) yes) no
    | C.Call (_, args) -> many specs (NonEmptyList.toList args)
    | C.TypeApp (id, types, args) ->
        let types = C.semanticTypeArgs types in
        let name = resolve id in
        let args = NonEmptyList.toList args in
        let specs = many specs args in
        let variables = List.exists S.containsTypeVar types in
        if name = P.eqHelperDispatchMarker || name = "__compare" then
          if variables then specs else SS.add (name, types) specs
        else if S.isGenericKeyIntrinsicName name then
          match types with
          | [ typ ] when (not variables) && hasPredefinedKeyIntrinsic typ ->
              SS.add (name, types) specs
          | _ -> specs
        else if
          (name = "Darklang.Stdlib.Dict.fromList" || name = "Dict.fromList")
          && args = [ C.ListLiteral [] ]
          && not variables
        then SS.add ("Darklang.Stdlib.Dict.empty", types) specs
        else SS.add (name, types) specs
    | C.TupleLiteral values -> many specs (C.tupleElementsToList values)
    | C.ListLiteral values | C.Constructor (_, values) -> many specs values
    | C.DictLiteral (keyType, valueType, entries) ->
        let specs =
          List.fold_left
            (fun specs (key, value) -> visit (visit specs key) value)
            specs entries
        in
        if entries = [] then specs
        else
          SS.add
            ( "Darklang.Stdlib.Dict.__setOverwriting",
              C.semanticTypeArgs [ keyType; valueType ] )
            specs
    | C.RecordLiteral (_, fields) ->
        List.fold_left
          (fun specs (_, value) -> visit specs value)
          specs
          (C.recordFieldsInSourceOrder fields)
    | C.RecordUpdate (record, fields) ->
        List.fold_left
          (fun specs (_, value) -> visit specs value)
          (visit specs record) fields
    | C.Match (value, cases) ->
        List.fold_left
          (fun specs (case : C.matchCase) ->
            let specs =
              Option.fold ~none:specs ~some:(visit specs) case.C.guard
            in
            visit specs case.C.body)
          (visit specs value)
          (NonEmptyList.toList cases)
    | C.Apply (target, args) | C.IndirectApply (target, args) ->
        many (visit specs target) (NonEmptyList.toList args)
    | C.InterpolatedString parts ->
        List.fold_left
          (fun specs -> function
            | C.StringText _ -> specs | C.StringExpr value -> visit specs value)
          specs parts
  in
  visit SS.empty expr

(*
   Collect TypeApps from a function definition
*)
let collectTypeAppsFromFunc symbols (func : C.functionDef) =
  collectTypeApps symbols func.C.body

(*
   Collect canonical call targets without interpreting their names. The AOT
   catalog bridge uses this typed traversal to materialize only lookup
   primitives that are reachable from the current compilation.
*)
let rec collectCalledFunctions expr =
  let many values =
    List.map collectCalledFunctions values |> List.fold_left FS.union FS.empty
  in
  match expr with
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
  | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
  | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
  | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
  | C.CharLiteral _ | C.FloatLiteral _ | C.Local _ | C.RuntimeError _ ->
      FS.empty
  | C.FuncRef id | C.GenericFuncRef (id, _, _) -> FS.singleton id
  | C.BoundaryRender (renderer, value) ->
      FS.add renderer (collectCalledFunctions value)
  | C.UnaryOp (_, value)
  | C.TupleAccess (value, _)
  | C.RecordAccess (value, _)
  | C.Lambda (_, _, value) ->
      collectCalledFunctions value
  | C.BinOp (_, left, right)
  | C.Sequence (left, right)
  | C.Let (_, left, right)
  | C.RecursiveLet (_, left, right) ->
      many [ left; right ]
  | C.If (condition, yes, no) -> many [ condition; yes; no ]
  | C.Call (id, args) | C.TypeApp (id, _, args) ->
      FS.add id (many (NonEmptyList.toList args))
  | C.TupleLiteral values -> many (C.tupleElementsToList values)
  | C.ListLiteral values | C.Constructor (_, values) -> many values
  | C.Closure (id, captures) -> FS.add id (many captures)
  | C.DictLiteral (_, _, entries) ->
      List.concat_map (fun (key, value) -> [ key; value ]) entries |> many
  | C.RecordLiteral (_, fields) ->
      C.recordFieldsInSourceOrder fields |> List.map snd |> many
  | C.RecordUpdate (record, fields) -> many (record :: List.map snd fields)
  | C.Match (value, cases) ->
      let calls =
        NonEmptyList.toList cases
        |> List.concat_map (fun (case : C.matchCase) ->
            case.C.body :: Option.to_list case.C.guard)
        |> many
      in
      FS.union (collectCalledFunctions value) calls
  | C.Apply (target, args) | C.IndirectApply (target, args) ->
      many (target :: NonEmptyList.toList args)
  | C.InterpolatedString parts ->
      List.filter_map
        (function C.StringText _ -> None | C.StringExpr value -> Some value)
        parts
      |> many

(*
   Specialize only the requested generic specs, returning new functions and a registry
*)
let specializeFromSpecs symbols definitions initial =
  let rec iterate pending processed functions registry externalSpecs symbols =
    let fresh = SS.diff pending processed in
    if SS.is_empty fresh then
      let functions =
        List.map
          (fun (artifact : S.genericFunctionArtifact) ->
            {
              artifact with
              S.symbols = C.catalogForCheckedUnit artifact.S.symbols;
            })
          functions
      in
      {
        S.specializedFuncs = functions;
        specRegistry = registry;
        externalSpecs;
        symbols;
      }
    else
      let newFunctions, pending, registry, externalSpecs, symbols =
        SS.elements fresh
        |> List.fold_left
             (fun (funcs, pending, registry, externalSpecs, symbols)
                  (name, types) ->
               match M.find_opt name definitions with
               | None ->
                   ( funcs,
                     pending,
                     registry,
                     SS.add (name, types) externalSpecs,
                     symbols )
               | Some (artifact : S.genericFunctionArtifact) ->
                   let specializedName =
                     S.specName artifact.S.func.C.name types
                   in
                   let id, symbols = C.internFunction specializedName symbols in
                   let func = T.specializeFunction id artifact.S.func types in
                   let specialized =
                     {
                       S.symbols;
                       func;
                       directDependencies = S.directDependencies func.C.body;
                     }
                   in
                   let registry = SM.add (name, types) func.C.name registry in
                   let bodySpecs =
                     collectTypeAppsFromFunc artifact.S.symbols func
                   in
                   ( specialized :: funcs,
                     SS.union pending bodySpecs,
                     registry,
                     externalSpecs,
                     symbols ))
             ([], SS.empty, registry, externalSpecs, symbols)
      in
      iterate pending (SS.union processed fresh) (newFunctions @ functions)
        registry externalSpecs symbols
  in
  iterate initial SS.empty [] SM.empty SS.empty symbols

let isIntrinsicTypeAppName = function
  | "__raw_get" | "__raw_take" | "__raw_slot_init" | "__stream_to_rawptr"
  | "__rawptr_to_stream" | "__empty_dict" | "__dict_is_null" | "__dict_get_tag"
  | "__dict_to_rawptr" | "__rawptr_to_dict" | "__list_empty" | "__list_is_null"
  | "__list_get_tag" | "__list_to_rawptr" | "__rawptr_to_list"
  | "Builtin.pmEvaluateValue" ->
      true
  | _ -> false

let missingSpecMessage name types =
  "Missing specialization for " ^ name ^ "<"
  ^ String.concat ", " (List.map S.typeToMangledName types)
  ^ ">"

(* The two reference traversals share constructor recursion, but retain their
   separate type-application rules and error/evaluation order. *)
let replaceTypeAppsCore symbols registry expr =
  let resolved = resolvedFunctionId symbols in
  let rec replace expr =
    let many values = ResultList.mapResults replace values in
    let one value build =
      let* value = replace value in
      Ok (build value)
    in
    let two left right build =
      let* left = replace left in
      let* right = replace right in
      Ok (build left right)
    in
    let materialize typ args =
      replace (P.materializeComparisonPlan resolved typ args)
    in
    let call name args = C.Call (resolved name, S.exprArgsFromList args) in
    let wrap = S.wrapWithIgnoredArgEvaluations in
    let key name types args =
      match (name, types) with
      | "__hash", [ typ ] when hasPredefinedKeyIntrinsic typ ->
          Ok (call (S.specName name types) args)
      | "__hash", [ _ ] -> Ok (wrap args (C.Int64Literal 0L))
      | "__key_eq", [ typ ] when hasPredefinedKeyIntrinsic typ ->
          Ok (call (S.specName name types) args)
      | "__key_eq", [ typ ] -> materialize typ args
      | _ -> (
          match registry with
          | None ->
              Crash.crash ("Invalid generic key intrinsic application: " ^ name)
          | Some _ ->
              Error ("Invalid generic key intrinsic application: " ^ name))
    in
    match expr with
    | C.GenericFuncRef (id, args, _) -> (
        let name =
          match C.functionName id symbols with
          | Some name -> name
          | None -> Crash.crash "Generic function value is absent from symbols"
        in
        let types = C.semanticTypeArgs args in
        let specialized = S.specName name types in
        if List.exists S.containsTypeVar types then
          Error ("Cannot infer specialization for function value '" ^ name ^ "'")
        else
          match registry with
          | None -> Ok (C.FuncRef (resolved specialized))
          | Some registry -> (
              match SM.find_opt (name, types) registry with
              | Some name -> Ok (C.FuncRef (resolved name))
              | None -> Error (missingSpecMessage name types)))
    | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
    | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
    | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
    | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
    | C.CharLiteral _ | C.FloatLiteral _ | C.Local _ | C.FuncRef _ | C.Closure _
    | C.RuntimeError _ ->
        Ok expr
    | C.BoundaryRender (renderer, value) ->
        one value (fun value -> C.BoundaryRender (renderer, value))
    | C.BinOp (op, left, right) ->
        two left right (fun left right -> C.BinOp (op, left, right))
    | C.UnaryOp (op, value) -> one value (fun value -> C.UnaryOp (op, value))
    | C.Let (pattern, value, body) ->
        two value body (fun value body -> C.Let (pattern, value, body))
    | C.RecursiveLet (recursion, value, body) ->
        two value body (fun value body ->
            C.RecursiveLet (recursion, value, body))
    | C.If (condition, yes, no) ->
        let* condition = replace condition in
        let* yes = replace yes in
        let* no = replace no in
        Ok (C.If (condition, yes, no))
    | C.Sequence (first, next) ->
        two first next (fun first next -> C.Sequence (first, next))
    | C.Call (id, args) ->
        let* args = many (NonEmptyList.toList args) in
        Ok (C.Call (id, S.exprArgsFromList args))
    | C.TypeApp (id, checkedTypes, arguments) -> (
        let types = C.semanticTypeArgs checkedTypes in
        let name =
          match C.functionName id symbols with
          | Some name -> name
          | None ->
              Crash.crash
                "Type application function identity is absent from symbols"
        in
        let variables = List.exists S.containsTypeVar types in
        let args = NonEmptyList.toList arguments in
        let emptyDict =
          (name = "Darklang.Stdlib.Dict.fromList" || name = "Dict.fromList")
          && args = [ C.ListLiteral [] ]
          && not variables
        in
        let genericKey = S.isGenericKeyIntrinsicName name in
        let unresolvedKey args =
          wrap args
            (S.unresolvedKeyIntrinsicTypeArgErrorExpr
               (resolved "Builtin.testRuntimeError")
               name)
        in
        let polymorphicComparison args =
          if name = P.eqHelperDispatchMarker then
            match args with
            | [ left; right ] -> C.BinOp (AST.Eq, left, right)
            | _ -> Crash.crash "Comparison plan expected exactly two operands"
          else
            wrap args
              (C.RuntimeError
                 "Canonical comparison remained polymorphic after \
                  monomorphization")
        in
        match registry with
        | None ->
            if name = P.eqHelperDispatchMarker then
              let* args = many args in
              match types with
              | [ typ ] when not variables -> materialize typ args
              | _ -> Ok (polymorphicComparison args)
            else if name = "__compare" then
              let* args = many args in
              match (types, args) with
              | [ typ ], [ left; right ] when not variables ->
                  Ok
                    (call
                       (ComparisonPlanning.compareHelperName typ)
                       [ left; right ])
              | _ -> Ok (polymorphicComparison args)
            else if emptyDict then
              Ok (call (S.specName "Darklang.Stdlib.Dict.empty" types) [])
            else if genericKey && variables then
              let* args = many args in
              Ok (unresolvedKey args)
            else if genericKey then
              let* args = many args in
              key name types args
            else
              let specialized = S.specName name types in
              (* Resolve the target before traversing arguments, as in Call construction. *)
              let target = resolved specialized in
              let* args = many args in
              Ok (C.Call (target, S.exprArgsFromList args))
        | Some registry ->
            let resolvedName =
              if name = P.eqHelperDispatchMarker then
                match types with
                | [ _ ] when not variables -> Ok P.eqHelperDispatchMarker
                | _ ->
                    Error
                      "Comparison helper remained polymorphic after \
                       monomorphization"
              else if name = "__compare" then
                match types with
                | [ typ ] when not variables ->
                    Ok (ComparisonPlanning.compareHelperName typ)
                | _ ->
                    Error
                      "Canonical comparison remained polymorphic after \
                       monomorphization"
              else if genericKey || isIntrinsicTypeAppName name then
                Ok (S.specName name types)
              else
                let name =
                  if emptyDict then "Darklang.Stdlib.Dict.empty" else name
                in
                match SM.find_opt (name, types) registry with
                | Some specialized -> Ok specialized
                | None -> Error (missingSpecMessage name types)
            in
            if genericKey && variables then
              let* args = many args in
              Ok (unresolvedKey args)
            else if genericKey then
              let* args = many args in
              key name types args
            else if
              (name = P.eqHelperDispatchMarker || name = "__compare")
              && variables
            then
              let* args = many args in
              Ok (polymorphicComparison args)
            else if name = P.eqHelperDispatchMarker && not variables then
              let* args = many args in
              let target =
                match types with
                | head :: _ -> head
                | [] ->
                    Crash.crash "The input list was empty. (Parameter 'list')"
              in
              materialize target args
            else
              let* specialized = resolvedName in
              if emptyDict then Ok (call specialized [])
              else
                let* args = many args in
                Ok (call specialized args))
    | C.TupleLiteral values ->
        let* values = many (C.tupleElementsToList values) in
        Ok (C.TupleLiteral (C.tupleElementsOfList values))
    | C.TupleAccess (value, index) ->
        one value (fun value -> C.TupleAccess (value, index))
    | C.DictLiteral (key, value, entries) -> (
        match entries with
        | [] -> Ok expr
        | _ ->
            let lowered =
              List.fold_left
                (fun dict (keyExpr, valueExpr) ->
                  C.TypeApp
                    ( resolved "Darklang.Stdlib.Dict.__setOverwriting",
                      [ key; value ],
                      NonEmptyList.fromList [ dict; keyExpr; valueExpr ] ))
                (C.DictLiteral (key, value, []))
                entries
            in
            replace lowered)
    | C.RecordLiteral (owner, fields) ->
        let* fields = C.traverseRecordFields replace fields in
        Ok (C.RecordLiteral (owner, fields))
    | C.RecordUpdate (record, updates) ->
        let* record = replace record in
        let* updates =
          ResultList.mapResults
            (fun (field, value) ->
              let* value = replace value in
              Ok (field, value))
            updates
        in
        Ok (C.RecordUpdate (record, updates))
    | C.RecordAccess (record, field) ->
        one record (fun record -> C.RecordAccess (record, field))
    | C.Constructor (reference, fields) ->
        let* fields = many fields in
        Ok (C.Constructor (reference, fields))
    | C.Match (value, cases) ->
        let* value = replace value in
        let* cases =
          ResultList.mapResults
            (fun (case : C.matchCase) ->
              let* guard =
                match case.C.guard with
                | None -> Ok None
                | Some guard -> Result.map Option.some (replace guard)
              in
              let* body = replace case.C.body in
              Ok { case with C.guard; body })
            (NonEmptyList.toList cases)
        in
        Ok (C.Match (value, NonEmptyList.fromList cases))
    | C.ListLiteral values ->
        let* values = many values in
        Ok (C.ListLiteral values)
    | C.Lambda (parameters, annotation, body) ->
        one body (fun body -> C.Lambda (parameters, annotation, body))
    | C.Apply (target, args) ->
        let* target = replace target in
        let* args = many (NonEmptyList.toList args) in
        Ok (C.Apply (target, S.exprArgsFromList args))
    | C.IndirectApply (target, args) ->
        let* target = replace target in
        let* args = many (NonEmptyList.toList args) in
        Ok (C.IndirectApply (target, S.exprArgsFromList args))
    | C.InterpolatedString parts ->
        let* parts =
          ResultList.mapResults
            (function
              | C.StringText _ as part -> Ok part
              | C.StringExpr value ->
                  Result.map (fun value -> C.StringExpr value) (replace value))
            parts
        in
        Ok (C.InterpolatedString parts)
  in
  replace expr

(*
   Replace TypeApp with Call using specialized name in an expression
   Replace with a regular Call to the specialized name
   Optimization: avoid building a Dict from an empty list when types are concrete.
*)
let replaceTypeApps symbols expr =
  match replaceTypeAppsCore symbols None expr with
  | Ok expr -> expr
  | Error message -> Crash.crash message

(*
   Replace TypeApp with Call using a precomputed specialization registry
   The original generic template remains in the combined
   preamble alongside its callable concrete specializations.
   Concrete copies have already received substituted plans;
   lower only this unreachable template to a compilable form.
*)
let replaceTypeAppsWithRegistry symbols registry expr =
  replaceTypeAppsCore symbols (Some registry) expr

(*
   Replace TypeApp with Call in a function definition
*)
let replaceTypeAppsInFunc symbols (func : C.functionDef) =
  { func with C.body = replaceTypeApps symbols func.C.body }

(*
   Replace TypeApp with Call in a function definition using a registry
*)
let replaceTypeAppsInFuncWithRegistry symbols registry (func : C.functionDef) =
  Result.map
    (fun body -> { func with C.body })
    (replaceTypeAppsWithRegistry symbols registry func.C.body)

let mapFold transform state values =
  let reversed, state =
    List.fold_left
      (fun (acc, state) value ->
        let value, state = transform state value in
        (value :: acc, state))
      ([], state) values
  in
  (List.rev reversed, state)

let materializeFunctionComparisons program =
  let rec rewrite symbols expr =
    let one value build =
      let value, symbols = rewrite symbols value in
      (build value, symbols)
    in
    let two left right build =
      let left, symbols = rewrite symbols left in
      let right, symbols = rewrite symbols right in
      (build left right, symbols)
    in
    let args symbols values =
      let values, symbols =
        mapFold rewrite symbols (NonEmptyList.toList values)
      in
      (S.exprArgsFromList values, symbols)
    in
    match expr with
    | C.BoundaryRender (renderer, value) ->
        one value (fun value -> C.BoundaryRender (renderer, value))
    | C.BinOp (op, left, right) ->
        two left right (fun left right -> C.BinOp (op, left, right))
    | C.UnaryOp (op, value) -> one value (fun value -> C.UnaryOp (op, value))
    | C.Let (pattern, value, body) ->
        two value body (fun value body -> C.Let (pattern, value, body))
    | C.RecursiveLet (recursion, value, body) ->
        two value body (fun value body ->
            C.RecursiveLet (recursion, value, body))
    | C.If (condition, yes, no) ->
        let condition, symbols = rewrite symbols condition in
        let yes, symbols = rewrite symbols yes in
        let no, symbols = rewrite symbols no in
        (C.If (condition, yes, no), symbols)
    | C.Sequence (first, next) ->
        two first next (fun first next -> C.Sequence (first, next))
    | C.Call (target, values) ->
        let values, symbols = args symbols values in
        (C.Call (target, values), symbols)
    | C.TypeApp (target, types, values) ->
        let values, symbols = args symbols values in
        let functionComparison =
          match (C.functionName target symbols, C.semanticTypeArgs types) with
          | Some name, [ AST.TFunction _ ]
            when name = P.eqHelperDispatchMarker || name = "__key_eq" ->
              true
          | _ -> false
        in
        if not functionComparison then
          (C.TypeApp (target, types, values), symbols)
        else
          let left, symbols = C.allocateBinding "__comparison_left" symbols in
          let right, symbols = C.allocateBinding "__comparison_right" symbols in
          ( P.materializeFunctionComparisonPlan left right
              (NonEmptyList.toList values),
            symbols )
    | C.TupleLiteral values ->
        let values, symbols =
          mapFold rewrite symbols (C.tupleElementsToList values)
        in
        (C.TupleLiteral (C.tupleElementsOfList values), symbols)
    | C.TupleAccess (value, index) ->
        one value (fun value -> C.TupleAccess (value, index))
    | C.DictLiteral (key, value, entries) ->
        let entries, symbols =
          mapFold
            (fun symbols (key, value) ->
              let key, symbols = rewrite symbols key in
              let value, symbols = rewrite symbols value in
              ((key, value), symbols))
            symbols entries
        in
        (C.DictLiteral (key, value, entries), symbols)
    | C.RecordLiteral (reference, fields) ->
        let fields, symbols = C.mapFoldRecordFields rewrite symbols fields in
        (C.RecordLiteral (reference, fields), symbols)
    | C.RecordUpdate (record, fields) ->
        let record, symbols = rewrite symbols record in
        let fields, symbols =
          mapFold
            (fun symbols (name, value) ->
              let value, symbols = rewrite symbols value in
              ((name, value), symbols))
            symbols fields
        in
        (C.RecordUpdate (record, fields), symbols)
    | C.RecordAccess (record, field) ->
        one record (fun record -> C.RecordAccess (record, field))
    | C.Constructor (reference, fields) ->
        let fields, symbols = mapFold rewrite symbols fields in
        (C.Constructor (reference, fields), symbols)
    | C.Match (value, cases) ->
        let value, symbols = rewrite symbols value in
        let cases, symbols =
          mapFold
            (fun symbols (case : C.matchCase) ->
              let guard, symbols =
                match case.C.guard with
                | None -> (None, symbols)
                | Some guard ->
                    let guard, symbols = rewrite symbols guard in
                    (Some guard, symbols)
              in
              let body, symbols = rewrite symbols case.C.body in
              ({ case with C.guard; body }, symbols))
            symbols
            (NonEmptyList.toList cases)
        in
        (C.Match (value, NonEmptyList.fromList cases), symbols)
    | C.ListLiteral values ->
        let values, symbols = mapFold rewrite symbols values in
        (C.ListLiteral values, symbols)
    | C.Lambda (parameters, annotation, body) ->
        one body (fun body -> C.Lambda (parameters, annotation, body))
    | C.Apply (target, values) ->
        let target, symbols = rewrite symbols target in
        let values, symbols = args symbols values in
        (C.Apply (target, values), symbols)
    | C.IndirectApply (target, values) ->
        let target, symbols = rewrite symbols target in
        let values, symbols = args symbols values in
        (C.IndirectApply (target, values), symbols)
    | C.Closure (target, captures) ->
        let captures, symbols = mapFold rewrite symbols captures in
        (C.Closure (target, captures), symbols)
    | C.InterpolatedString parts ->
        let parts, symbols =
          mapFold
            (fun symbols -> function
              | C.StringText _ as part -> (part, symbols)
              | C.StringExpr value ->
                  let value, symbols = rewrite symbols value in
                  (C.StringExpr value, symbols))
            symbols parts
        in
        (C.InterpolatedString parts, symbols)
    | _ -> (expr, symbols)
  in
  let tops, symbols =
    mapFold
      (fun symbols -> function
        | C.FunctionDef func ->
            let body, symbols = rewrite symbols func.C.body in
            (C.FunctionDef { func with C.body }, symbols)
        | C.ValueDef value ->
            let body, symbols = rewrite symbols value.C.body in
            (C.ValueDef { value with C.body }, symbols)
        | C.Expression expr ->
            let expr, symbols = rewrite symbols expr in
            (C.Expression expr, symbols)
        | C.TypeDef _ as value -> (value, symbols))
      (C.programSymbols program)
      (C.programTopLevels program)
  in
  C.programFromCheckedParts (symbols, tops)

(*
   Replace TypeApp with Call across a program using a registry (drops generic defs)
*)
let replaceTypeAppsInProgramWithRegistry registry program =
  let program = materializeFunctionComparisons program in
  let initialSymbols = C.programSymbols program in
  let tops = C.programTopLevels program in
  let specs =
    List.map
      (function
        | C.FunctionDef func -> collectTypeAppsFromFunc initialSymbols func
        | C.Expression expr -> collectTypeApps initialSymbols expr
        | C.ValueDef value -> collectTypeApps initialSymbols value.C.body
        | C.TypeDef _ -> SS.empty)
      tops
    |> List.fold_left SS.union SS.empty
  in
  let names = SM.bindings registry |> List.map snd |> Names.of_list in
  let names =
    SS.fold
      (fun ((name, types) as spec) names ->
        match SM.find_opt spec registry with
        | Some name -> Names.add name names
        | None ->
            if name = P.eqHelperDispatchMarker then
              match types with
              | [ typ ] when not (S.containsTypeVar typ) ->
                  Names.add (ComparisonPlanning.eqHelperName typ) names
              | _ -> names
            else if name = "__compare" then
              match types with
              | [ typ ] when not (S.containsTypeVar typ) ->
                  Names.add (ComparisonPlanning.compareHelperName typ) names
              | _ -> names
            else if
              isIntrinsicTypeAppName name || S.isGenericKeyIntrinsicName name
            then Names.add (S.specName name types) names
            else names)
      specs names
  in
  let symbols =
    Names.fold
      (fun name symbols -> snd (C.internFunction name symbols))
      names initialSymbols
  in
  let rec loop remaining acc =
    match remaining with
    | [] -> Ok (C.programFromCheckedParts (symbols, List.rev acc))
    | C.FunctionDef func :: rest when func.C.typeParams <> [] -> loop rest acc
    | C.FunctionDef func :: rest ->
        let* func = replaceTypeAppsInFuncWithRegistry symbols registry func in
        loop rest (C.FunctionDef func :: acc)
    | C.Expression expr :: rest ->
        let* expr = replaceTypeAppsWithRegistry symbols registry expr in
        loop rest (C.Expression expr :: acc)
    | C.ValueDef value :: rest ->
        let* body = replaceTypeAppsWithRegistry symbols registry value.C.body in
        loop rest (C.ValueDef { value with C.body } :: acc)
    | (C.TypeDef _ as value) :: rest -> loop rest (value :: acc)
  in
  loop tops []

let collectInitialMonomorphizationSpecs program =
  let symbols = C.programSymbols program in
  C.programTopLevels program
  |> List.map (function
    | C.FunctionDef func when func.C.typeParams = [] ->
        collectTypeAppsFromFunc symbols func
    | C.ValueDef value -> collectTypeApps symbols value.C.body
    | C.Expression expr -> collectTypeApps symbols expr
    | _ -> SS.empty)
  |> List.fold_left SS.union SS.empty

let registryWithExternalSpecs (specialization : S.specializationResult) =
  SS.fold
    (fun (name, types) registry ->
      SM.add (name, types) (S.specName name types) registry)
    specialization.S.externalSpecs specialization.S.specRegistry

let monomorphizeWithGenericFuncDefs definitions program =
  let program = StaticCallReduction.reduce definitions program in
  let specs = collectInitialMonomorphizationSpecs program in
  let specialization =
    specializeFromSpecs (C.programSymbols program) definitions specs
  in
  let symbols, functions =
    S.importSpecializedFunctions specialization.S.symbols
      specialization.S.specializedFuncs
  in
  let tops =
    List.map (fun func -> C.FunctionDef func) functions
    @ C.programTopLevels program
  in
  let program = C.programFromCheckedParts (symbols, tops) in
  match
    replaceTypeAppsInProgramWithRegistry
      (registryWithExternalSpecs specialization)
      program
  with
  | Ok program -> program
  | Error message -> Crash.crash ("monomorphizeWithGenericFuncDefs: " ^ message)

(*
   Check if a program needs lambda lowering (lambda inlining + lifting)
   based on lambdas, closures, or function values.
   Lambda Inlining
   For first-class function support, we inline lambdas at their call sites.
   This transforms:
   let f = fun x -> x + 1 in f(5)
   Into:
   let f = fun x -> x + 1 in (fun x -> x + 1)(5)
   Which is then handled by immediate application desugaring.
   Environment mapping variable names to their lambda definitions
*)
let programNeedsLambdaLowering _knownNames program =
  let rec needs bound expr =
    let child = needs bound in
    let many values = List.exists child values in
    match expr with
    | C.Lambda _ | C.Apply _ | C.IndirectApply _ | C.FuncRef _
    | C.GenericFuncRef _ | C.Closure _ ->
        true
    | C.Local _ -> false
    | C.BoundaryRender (_, value)
    | C.UnaryOp (_, value)
    | C.TupleAccess (value, _)
    | C.RecordAccess (value, _) ->
        child value
    | C.Let (pattern, value, body) ->
        child value
        || needs
             (A.BindingSet.union bound
                (A.BindingSet.of_list (C.letPatternBindings pattern)))
             body
    | C.RecursiveLet (recursion, value, body) ->
        let bound = A.BindingSet.add (C.recursiveBindingId recursion) bound in
        needs bound value || needs bound body
    | C.If (condition, yes, no) -> child condition || child yes || child no
    | C.Sequence (first, next) | C.BinOp (_, first, next) ->
        child first || child next
    | C.Call (_, args) | C.TypeApp (_, _, args) ->
        many (NonEmptyList.toList args)
    | C.TupleLiteral values -> many (C.tupleElementsToList values)
    | C.ListLiteral values | C.Constructor (_, values) -> many values
    | C.RecordLiteral (_, fields) ->
        List.exists
          (fun (_, value) -> child value)
          (C.recordFieldsInSourceOrder fields)
    | C.RecordUpdate (record, fields) ->
        child record || List.exists (fun (_, value) -> child value) fields
    | C.Match (value, cases) ->
        child value
        || List.exists
             (fun (case : C.matchCase) ->
               Option.fold ~none:false ~some:child case.C.guard
               || child case.C.body)
             (NonEmptyList.toList cases)
    | C.InterpolatedString parts ->
        List.exists
          (function
            | C.StringText _ -> false | C.StringExpr value -> child value)
          parts
    | _ -> false
  in
  C.programTopLevels program
  |> List.exists (function
    | C.FunctionDef func ->
        let bound =
          C.functionParameterTypes func
          |> NonEmptyList.toList |> List.map fst |> A.BindingSet.of_list
        in
        needs bound func.C.body
    | C.Expression expr -> needs A.BindingSet.empty expr
    | C.ValueDef value -> needs A.BindingSet.empty value.C.body
    | C.TypeDef _ -> false)
