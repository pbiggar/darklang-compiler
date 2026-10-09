(*
   CheckedMaterializeHelpers.ml - Materialize comparison helpers in checked syntax.
   Concrete generic specializations are created after source checking. This
   pass keeps that late helper discovery entirely on CheckedAST while reusing
   the canonical helper-definition generators owned by semantic checking.
*)
(* Materialize comparison helpers after specialization entirely in checked syntax. *)
[@@@warning "-4"]

module C = CheckedAST
module S = HelperDependencies.TypeSet
module M = StringOrder.Map
module Names = StringOrder.Set

let empty = (S.empty, S.empty)

let union (eq, compare) (childEq, childCompare) =
  (S.union eq childEq, S.union compare childCompare)

let rec collectHelperTypes symbols aliases expr =
  let collect = collectHelperTypes symbols aliases in
  let combine values =
    List.fold_left
      (fun result value -> union result (collect value))
      empty values
  in
  match expr with
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
  | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
  | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
  | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
  | C.CharLiteral _ | C.FloatLiteral _ | C.Local _ | C.FuncRef _
  | C.GenericFuncRef _ | C.RuntimeError _ ->
      empty
  | C.BoundaryRender (_, value)
  | C.UnaryOp (_, value)
  | C.TupleAccess (value, _)
  | C.RecordAccess (value, _) ->
      collect value
  | C.BinOp (_, left, right)
  | C.Let (_, left, right)
  | C.RecursiveLet (_, left, right)
  | C.Sequence (left, right) ->
      combine [ left; right ]
  | C.If (condition, yes, no) -> combine [ condition; yes; no ]
  | C.Call (_, args) -> combine (NonEmptyList.toList args)
  | C.TypeApp (id, typeArgs, args) ->
      let types = C.semanticTypeArgs typeArgs
      and arguments = NonEmptyList.toList args in
      let concrete =
        List.map (Types.resolveType aliases) types
        |> List.filter (fun typ -> not (Unification.containsTVar typ))
        |> S.of_list
      in
      let equality =
        match (C.functionName id symbols, types, arguments) with
        | Some "__dark_internal_eq_helper_dispatch", [ target ], [ _; _ ] ->
            S.singleton (Types.resolveType aliases target)
        (* These traversals never compare their element type. Eagerly building
      equality for ErrorSegment here expands its recursive Dval fields. *)
        | ( Some
              ( "Darklang.Stdlib.List.reverse" | "Darklang.Stdlib.List.fold"
              | "Darklang.Stdlib.List.push" | "Darklang.Stdlib.List.map"
              | "Darklang.Stdlib.List.append" | "Darklang.Stdlib.List.head"
              | "Darklang.Stdlib.List.getAt" | "Darklang.Stdlib.List.indexedMap"
              | "Darklang.Stdlib.Dict.__entries" ),
            _,
            _ ) ->
            S.empty
        | _ -> concrete
      in
      let ordering =
        match (C.functionName id symbols, types) with
        | ( Some
              ( "__compare" | "Darklang.Stdlib.List.sort"
              | "Darklang.Stdlib.List.unique" ),
            [ target ] )
        | Some "Darklang.Stdlib.List.uniqueBy", [ target; _ ] ->
            let target = Types.resolveType aliases target in
            if Unification.containsTVar target then S.empty
            else S.singleton target
        | Some "Darklang.Stdlib.List.sortBy", [ value; key ] ->
            let pair =
              AST.TTuple
                [
                  Types.resolveType aliases key; Types.resolveType aliases value;
                ]
            in
            if Unification.containsTVar pair then S.empty else S.singleton pair
        | _ -> S.empty
      in
      union (equality, ordering) (combine arguments)
  | C.TupleLiteral values -> combine (C.tupleElementsToList values)
  | C.ListLiteral values | C.Constructor (_, values) -> combine values
  | C.DictLiteral (_, _, entries) ->
      combine (List.concat_map (fun (key, value) -> [ key; value ]) entries)
  | C.RecordLiteral (_, entries) ->
      combine (List.map snd (C.recordFieldsInSourceOrder entries))
  | C.RecordUpdate (record, entries) -> combine (record :: List.map snd entries)
  | C.Match (scrutinee, cases) ->
      combine
        (scrutinee
        :: List.concat_map
             (fun (case : C.matchCase) ->
               case.C.body :: Option.to_list case.C.guard)
             (NonEmptyList.toList cases))
  | C.Lambda (_, _, body) -> collect body
  | C.Apply (func, args) | C.IndirectApply (func, args) ->
      combine (func :: NonEmptyList.toList args)
  | C.Closure (_, captures) -> combine captures
  | C.InterpolatedString parts ->
      combine
        (List.filter_map
           (function C.StringExpr expr -> Some expr | C.StringText _ -> None)
           parts)

let rec rewriteHelperCalls symbols aliases variants expr =
  let recurse = rewriteHelperCalls symbols aliases variants in
  let args = NonEmptyList.map recurse in
  let resolved name =
    match C.tryFindFunctionId name symbols with
    | Some id -> id
    | None ->
        Crash.crash
          ("Materialized helper function '" ^ name ^ "' is absent from symbols")
  in
  match expr with
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
  | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
  | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
  | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
  | C.CharLiteral _ | C.FloatLiteral _ | C.Local _ | C.FuncRef _
  | C.GenericFuncRef _ | C.RuntimeError _ ->
      expr
  | C.BoundaryRender (renderer, value) ->
      C.BoundaryRender (renderer, recurse value)
  | C.BinOp (op, left, right) -> C.BinOp (op, recurse left, recurse right)
  | C.UnaryOp (op, value) -> C.UnaryOp (op, recurse value)
  | C.Let (pattern, value, body) -> C.Let (pattern, recurse value, recurse body)
  | C.RecursiveLet (recursion, value, body) ->
      C.RecursiveLet (recursion, recurse value, recurse body)
  | C.If (condition, yes, no) ->
      C.If (recurse condition, recurse yes, recurse no)
  | C.Sequence (first, next) -> C.Sequence (recurse first, recurse next)
  | C.Call (id, values) -> C.Call (id, args values)
  | C.TypeApp (id, [ target ], { NonEmptyList.head = left; tail = [ right ] })
    when C.functionName id symbols = Some "__dark_internal_eq_helper_dispatch"
    ->
      let helperType = Types.resolveType aliases (C.semanticType target) in
      let values = NonEmptyList.fromList [ recurse left; recurse right ] in
      if ComparisonPlanning.needsEqHelperForResolvedType variants helperType
      then C.Call (resolved (ComparisonPlanning.eqHelperName helperType), values)
      else C.TypeApp (id, [ target ], values)
  | C.TypeApp (id, [ target ], values)
    when C.functionName id symbols = Some "__compare"
         && not (Unification.containsTVar (C.semanticType target)) ->
      C.Call
        ( resolved
            (ComparisonPlanning.compareHelperName
               (Types.resolveType aliases (C.semanticType target))),
          args values )
  | C.TypeApp (id, types, values) -> C.TypeApp (id, types, args values)
  | C.TupleLiteral values -> C.TupleLiteral (C.mapTupleElements recurse values)
  | C.TupleAccess (value, index) -> C.TupleAccess (recurse value, index)
  | C.DictLiteral (key, value, entries) ->
      C.DictLiteral
        ( key,
          value,
          List.map (fun (key, value) -> (recurse key, recurse value)) entries )
  | C.RecordLiteral (reference, fields) ->
      C.RecordLiteral (reference, C.mapRecordFields recurse fields)
  | C.RecordUpdate (record, updates) ->
      C.RecordUpdate
        ( recurse record,
          List.map (fun (id, value) -> (id, recurse value)) updates )
  | C.RecordAccess (record, id) -> C.RecordAccess (recurse record, id)
  | C.Constructor (reference, fields) ->
      C.Constructor (reference, List.map recurse fields)
  | C.Match (scrutinee, cases) ->
      C.Match
        ( recurse scrutinee,
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
      C.Lambda (parameters, annotation, recurse body)
  | C.Apply (func, values) -> C.Apply (recurse func, args values)
  | C.IndirectApply (func, values) -> C.IndirectApply (recurse func, args values)
  | C.Closure (id, captures) -> C.Closure (id, List.map recurse captures)
  | C.InterpolatedString parts ->
      C.InterpolatedString
        (List.map
           (function
             | C.StringText text -> C.StringText text
             | C.StringExpr expr -> C.StringExpr (recurse expr))
           parts)

let checkedGeneratedFunction variants typeReg symbols func =
  let fieldCounts name =
    Option.map
      (fun (info : Types.recordTypeInfo) -> List.length info.Types.fields)
      (M.find_opt name typeReg)
  in
  match C.ofTypedFunction variants fieldCounts symbols func with
  | Ok result -> result
  | Error error -> Crash.crash error

let materializeEqHelpersInTopLevelsWithIndexedSums symbols aliases typeReg
    variants indexedSums topLevels =
  let concrete =
    List.filter_map
      (function
        | C.FunctionDef func when func.C.typeParams = [] -> Some func.C.body
        | C.Expression expr -> Some expr
        | C.ValueDef value -> Some value.C.body
        | C.FunctionDef _ | C.TypeDef _ -> None)
      topLevels
  in
  let equality, ordering =
    List.fold_left
      (fun combined expr ->
        union combined (collectHelperTypes symbols aliases expr))
      empty concrete
  in
  let equalityState =
    S.fold
      (HelperDependencies.ensureEqHelperForType aliases typeReg variants
         indexedSums)
      equality
      ({ HelperDependencies.inProgress = Names.empty; generated = M.empty }
        : HelperDependencies.eqHelperGenerationState)
  in
  let orderingState =
    S.fold
      (HelperDependencies.ensureCompareHelperForType aliases typeReg variants
         indexedSums)
      ordering
      ({ HelperDependencies.inProgress = Names.empty; generated = M.empty }
        : HelperDependencies.compareHelperGenerationState)
  in
  let existing =
    List.filter_map
      (function C.FunctionDef func -> Some func.C.name | _ -> None)
      topLevels
    |> Names.of_list
  in
  let helpers =
    M.union
      (fun _ _ newer -> Some newer)
      equalityState.HelperDependencies.generated
      orderingState.HelperDependencies.generated
    |> M.bindings |> List.map snd
    |> List.filter (fun (func : AST.functionDef) ->
        not (Names.mem func.AST.name existing))
  in
  let helpers, symbols =
    List.fold_left
      (fun (reversed, symbols) helper ->
        let func, symbols =
          checkedGeneratedFunction variants typeReg symbols helper
        in
        (C.FunctionDef func :: reversed, symbols))
      ([], symbols) helpers
  in
  let rewritten =
    List.map
      (function
        | C.FunctionDef func when func.C.typeParams = [] ->
            C.FunctionDef
              {
                func with
                C.body = rewriteHelperCalls symbols aliases variants func.C.body;
              }
        | C.Expression expr ->
            C.Expression (rewriteHelperCalls symbols aliases variants expr)
        | C.ValueDef value ->
            C.ValueDef
              {
                value with
                C.body =
                  rewriteHelperCalls symbols aliases variants value.C.body;
              }
        | other -> other)
      topLevels
  in
  (symbols, List.rev helpers @ rewritten)

let materializeEqHelpersInTopLevels symbols aliases typeReg variants topLevels =
  materializeEqHelpersInTopLevelsWithIndexedSums symbols aliases typeReg
    variants
    (Types.indexSumTypeRegistry variants)
    topLevels
