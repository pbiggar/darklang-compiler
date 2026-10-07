(*
   MaterializeHelpers.ml - Insert reachable equality and ordering helper definitions.
*)
(* MaterializeHelpers.ml - Insert reachable equality and ordering helper definitions. *)
open! AST
module M = StringOrder.Map
module S = StringOrder.Set
module T = HelperDependencies.TypeSet

let rec materializeHelperCallsInExpr includeEquality aliases lookup expr =
  let visit = materializeHelperCallsInExpr includeEquality aliases lookup in
  match expr with
  | BoundaryRender (name, value) -> BoundaryRender (name, visit value)
  | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
  | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
  | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
  | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _
  | RuntimeError _ ->
      expr
  | BinOp (op, left, right) -> BinOp (op, visit left, visit right)
  | UnaryOp (op, inner) -> UnaryOp (op, visit inner)
  | Let (pattern, value, body) -> Let (pattern, visit value, visit body)
  | RecursiveLet (recursion, value, body) ->
      RecursiveLet (recursion, visit value, visit body)
  | If (condition, yes, no) -> If (visit condition, visit yes, visit no)
  | Sequence (first, next) -> Sequence (visit first, visit next)
  | Apply (Var name, args, values) as applied -> (
      match ComparisonPlanning.tryDecodeInternalTypeApp applied with
      | Some (ComparisonPlanning.EqHelperDispatchTypeApp (target, left, right))
        when includeEquality ->
          let typ = Types.resolveType aliases target in
          if ComparisonPlanning.needsEqHelperForResolvedType lookup typ then
            AST.applyNamed
              (ComparisonPlanning.eqHelperName typ)
              (NonEmptyList.fromList [ visit left; visit right ])
          else
            AST.applyNamedWithTypes name args
              (NonEmptyList.fromList [ visit left; visit right ])
      | Some (ComparisonPlanning.EqHelperDispatchTypeApp _) | None -> (
          (match (name, args) with
          | "__compare", [ target ] when not (Unification.containsTVar target)
            ->
              AST.applyNamed
                (ComparisonPlanning.compareHelperName
                   (Types.resolveType aliases target))
                (NonEmptyList.map visit values)
          | _ ->
              AST.applyNamedWithTypes name args (NonEmptyList.map visit values))
          [@warning "-4"]))
  | TupleLiteral values -> TupleLiteral (List.map visit values)
  | TupleAccess (tuple, index) -> TupleAccess (visit tuple, index)
  | DictLiteral (key, value, entries) ->
      DictLiteral
        ( key,
          value,
          List.map (fun (key, value) -> (visit key, visit value)) entries )
  | RecordLiteral (reference, fields) ->
      RecordLiteral
        (reference, List.map (fun (field, value) -> (field, visit value)) fields)
  | RecordUpdate (record, fields) ->
      RecordUpdate
        ( visit record,
          List.map (fun (field, value) -> (field, visit value)) fields )
  | RecordAccess (record, field) -> RecordAccess (visit record, field)
  | Constructor (reference, name, fields) ->
      Constructor (reference, name, List.map visit fields)
  | Match (scrutinee, cases) ->
      Match
        ( visit scrutinee,
          List.map
            (fun (case : AST.matchCase) ->
              {
                case with
                guard = Option.map visit case.guard;
                body = visit case.body;
              })
            cases )
  | ListLiteral values -> ListLiteral (List.map visit values)
  | Lambda (params, annotation, body) -> Lambda (params, annotation, visit body)
  | Apply (func, args, values) ->
      Apply (visit func, args, NonEmptyList.map visit values)
  | IndirectApply (func, values) ->
      IndirectApply (visit func, NonEmptyList.map visit values)
  | Closure (name, values) -> Closure (name, List.map visit values)
  | InterpolatedString parts ->
      InterpolatedString
        (List.map
           (function
             | StringText text -> StringText text
             | StringExpr value -> StringExpr (visit value))
           parts)

(*
   Generic templates are retained for later specialization. Their
   comparison plans are materialized in each concrete copy.
*)
let materializeHelpersInTopLevels includeEquality aliases registry lookup sums
    topLevels =
  let collect collectExpr = function
    | FunctionDef definition when definition.typeParams = [] ->
        collectExpr aliases definition.body
    | FunctionDef _ | TypeDef _ -> T.empty
    | Expression (_, expr) -> collectExpr aliases expr
    | ValueDef definition -> collectExpr aliases (AST.valueDefBody definition)
  in
  let visit = materializeHelperCallsInExpr includeEquality aliases lookup in
  let rewrite = function
    | FunctionDef definition when definition.typeParams = [] ->
        FunctionDef { definition with body = visit definition.body }
    | (FunctionDef _ | TypeDef _) as item -> item
    | Expression (path, expr) -> Expression (path, visit expr)
    | ValueDef definition -> (
        let body = visit (AST.valueDefBody definition) in
        match definition with
        | UncheckedValueDef (name, _) ->
            ValueDef (UncheckedValueDef (name, body))
        | CheckedValueDef (name, typ, _) ->
            ValueDef (CheckedValueDef (name, typ, body)))
  in
  let collectAll collectExpr =
    List.fold_left
      (fun acc item -> T.union acc (collect collectExpr item))
      T.empty topLevels
  in
  let eqTypes =
    if includeEquality then
      collectAll HelperDependencies.collectEqHelperTypesFromExpr
    else T.empty
  in
  let compareTypes =
    collectAll HelperDependencies.collectCompareHelperTypesFromExpr
  in
  let rewritten = List.map rewrite topLevels in
  if T.is_empty eqTypes && T.is_empty compareTypes then rewritten
  else
    let eqInitial : HelperDependencies.eqHelperGenerationState =
      { HelperDependencies.inProgress = S.empty; generated = M.empty }
    in
    let eqState =
      T.fold
        (fun typ state ->
          HelperDependencies.ensureEqHelperForType aliases registry lookup sums
            typ state)
        eqTypes eqInitial
    in
    let compareInitial : HelperDependencies.compareHelperGenerationState =
      { HelperDependencies.inProgress = S.empty; generated = M.empty }
    in
    let compareState =
      T.fold
        (fun typ state ->
          HelperDependencies.ensureCompareHelperForType aliases registry lookup
            sums typ state)
        compareTypes compareInitial
    in
    let names =
      List.filter_map
        (function
          | FunctionDef definition -> Some definition.name
          | TypeDef _ | ValueDef _ | Expression _ -> None)
        topLevels
      |> S.of_list
    in
    let helpers =
      M.fold M.add compareState.HelperDependencies.generated
        eqState.HelperDependencies.generated
      |> M.bindings |> List.map snd
      |> List.filter (fun (definition : AST.functionDef) ->
          not (S.mem definition.name names))
      |> List.map (fun definition -> FunctionDef definition)
    in
    helpers @ rewritten

(*
   Materialize equality and canonical comparison dispatches in a complete
   concrete program.
*)
let materializeEqHelpersInTopLevelsWithIndexedSums aliases registry lookup sums
    topLevels =
  materializeHelpersInTopLevels true aliases registry lookup sums topLevels

(*
   Compatibility entry point for callers that do not retain a type-checking
   environment. Hot compilation paths pass its existing indexed sum registry.
*)
let materializeEqHelpersInTopLevels aliases registry lookup topLevels =
  materializeEqHelpersInTopLevelsWithIndexedSums aliases registry lookup
    (Types.indexSumTypeRegistry lookup)
    topLevels

(*
   Materialize only canonical comparison dispatches in newly-specialized
   stdlib functions. Equality dispatches retain the established stdlib
   specialization path.
*)
let materializeCompareHelpersInTopLevels aliases registry lookup topLevels =
  materializeHelpersInTopLevels false aliases registry lookup
    (Types.indexSumTypeRegistry lookup)
    topLevels
