(* FuseOwnershipListCalls.ml - Inline selected List<Int64> boundaries before region extraction. *)
[@@@warning "-4-42"]

module C = CheckedAST
module O = OwnedIR
module M = MaterializeOwnershipVariants
module F = FunctionIdMap
module FS = SpecializationIdentity.FunctionSet
module Sites = LowerOwnershipVariants.SiteSet

module SiteOrder = struct
  type t = O.callSiteIdentity

  let compare (first : t) (second : t) =
    let caller =
      Int64.unsigned_compare
        (AST.functionIdValue first.O.caller)
        (AST.functionIdValue second.O.caller)
    in
    if caller <> 0 then caller
    else
      let (HIR.ValueId first) = first.O.result in
      let (HIR.ValueId second) = second.O.result in
      Int.compare first second
end

module SiteMap = Map.Make (SiteOrder)

type fusionResult = { functions : C.functionDef list; fusedSites : Sites.t }

let mapFold operation state values =
  let reversed, final =
    List.fold_left
      (fun (reversed, state) value ->
        let mapped, next = operation state value in
        (mapped :: reversed, next))
      ([], state) values
  in
  (List.rev reversed, final)

let rec mapExpr rewrite state expr =
  let mapList values state = mapFold (mapExpr rewrite) state values in
  let mapNonEmpty values state =
    let values, next = mapList (NonEmptyList.toList values) state in
    (NonEmptyList.fromList values, next)
  in
  let mapPair first second state =
    let first, afterFirst = mapExpr rewrite state first in
    let second, next = mapExpr rewrite afterFirst second in
    (first, second, next)
  in
  let mapped, next =
    match expr with
    | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
    | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
    | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
    | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.CharLiteral _
    | C.FloatLiteral _ | C.BlobLiteral _ | C.Local _ | C.FuncRef _ | C.GenericFuncRef _
    | C.RuntimeError _ ->
        (expr, state)
    | C.InterpolatedString parts ->
        let parts, next =
          mapFold
            (fun current part ->
              match part with
              | C.StringText _ -> (part, current)
              | C.StringExpr value ->
                  let value, next = mapExpr rewrite current value in
                  (C.StringExpr value, next))
            state parts
        in
        (C.InterpolatedString parts, next)
    | C.BinOp (op, left, right) ->
        let left, right, next = mapPair left right state in
        (C.BinOp (op, left, right), next)
    | C.UnaryOp (op, value) ->
        let value, next = mapExpr rewrite state value in
        (C.UnaryOp (op, value), next)
    | C.Let (pattern, value, body) ->
        let value, body, next = mapPair value body state in
        (C.Let (pattern, value, body), next)
    | C.RecursiveLet (recursion, value, body) ->
        let value, body, next = mapPair value body state in
        (C.RecursiveLet (recursion, value, body), next)
    | C.If (condition, yes, no) ->
        let condition, afterCondition = mapExpr rewrite state condition in
        let yes, no, next = mapPair yes no afterCondition in
        (C.If (condition, yes, no), next)
    | C.Sequence (first, second) ->
        let first, second, next = mapPair first second state in
        (C.Sequence (first, second), next)
    | C.Call (target, arguments) ->
        let arguments, next = mapNonEmpty arguments state in
        (C.Call (target, arguments), next)
    | C.TypeApp (target, types, arguments) ->
        let arguments, next = mapNonEmpty arguments state in
        (C.TypeApp (target, types, arguments), next)
    | C.TupleLiteral values ->
        let values, next = mapList (C.tupleElementsToList values) state in
        (C.TupleLiteral (C.tupleElementsOfList values), next)
    | C.TupleAccess (value, index) ->
        let value, next = mapExpr rewrite state value in
        (C.TupleAccess (value, index), next)
    | C.DictLiteral (keyType, valueType, entries) ->
        let entries, next =
          mapFold
            (fun current (key, value) ->
              let key, value, next = mapPair key value current in
              ((key, value), next))
            state entries
        in
        (C.DictLiteral (keyType, valueType, entries), next)
    | C.RecordLiteral (reference, fields) ->
        let fields, next =
          C.mapFoldRecordFields (mapExpr rewrite) state fields
        in
        (C.RecordLiteral (reference, fields), next)
    | C.RecordUpdate (record, fields) ->
        let record, afterRecord = mapExpr rewrite state record in
        let fields, next =
          mapFold
            (fun current (field, value) ->
              let value, next = mapExpr rewrite current value in
              ((field, value), next))
            afterRecord fields
        in
        (C.RecordUpdate (record, fields), next)
    | C.RecordAccess (record, field) ->
        let record, next = mapExpr rewrite state record in
        (C.RecordAccess (record, field), next)
    | C.Constructor (reference, fields) ->
        let fields, next = mapList fields state in
        (C.Constructor (reference, fields), next)
    | C.Match (value, cases) ->
        let value, afterValue = mapExpr rewrite state value in
        let cases, next =
          mapFold
            (fun current (case : C.matchCase) ->
              let guard, afterGuard =
                match case.C.guard with
                | None -> (None, current)
                | Some guard ->
                    let guard, next = mapExpr rewrite current guard in
                    (Some guard, next)
              in
              let body, next = mapExpr rewrite afterGuard case.C.body in
              ({ case with C.guard; body }, next))
            afterValue
            (NonEmptyList.toList cases)
        in
        (C.Match (value, NonEmptyList.fromList cases), next)
    | C.ListLiteral values ->
        let values, next = mapList values state in
        (C.ListLiteral values, next)
    | C.Lambda (parameters, annotation, body) ->
        let body, next = mapExpr rewrite state body in
        (C.Lambda (parameters, annotation, body), next)
    | C.Apply (functionValue, arguments)
    | C.IndirectApply (functionValue, arguments) -> (
        let functionValue, afterFunction =
          mapExpr rewrite state functionValue
        in
        let arguments, next = mapNonEmpty arguments afterFunction in
        match expr with
        | C.Apply _ -> (C.Apply (functionValue, arguments), next)
        | _ -> (C.IndirectApply (functionValue, arguments), next))
    | C.Closure (target, captures) ->
        let captures, next = mapList captures state in
        (C.Closure (target, captures), next)
    | C.BoundaryRender (renderer, value) ->
        let value, next = mapExpr rewrite state value in
        (C.BoundaryRender (renderer, value), next)
  in
  rewrite next mapped

let substitute parameters body =
  fst
    (mapExpr
       (fun () expression ->
         match expression with
         | C.Local binding ->
             ( Option.value
                 (C.BindingIdMap.find_opt binding parameters)
                 ~default:expression,
               () )
         | _ -> (expression, ()))
       () body)

type state = {
  selected : O.callSiteIdentity list F.t;
  fused : Sites.t;
  fusedValues : C.expr list;
}

let rec bindValue pattern value body =
  match value with
  | C.Let (innerPattern, innerValue, continuation) ->
      C.Let (innerPattern, innerValue, bindValue pattern continuation body)
  | C.Sequence (first, continuation) ->
      C.Sequence (first, bindValue pattern continuation body)
  | result -> C.Let (pattern, result, body)

let rec removeFirst value = function
  | [] -> []
  | head :: tail when head = value -> tail
  | head :: tail -> head :: removeFirst value tail

let isSafeArgument = function
  | C.Local _ | C.UnitLiteral | C.Int64Literal _ | C.Int8Literal _
  | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _ | C.UInt16Literal _
  | C.UInt32Literal _ | C.UInt64Literal _ | C.BoolLiteral _ | C.FloatLiteral _
  | C.FuncRef _ | C.GenericFuncRef _ ->
      true
  | _ -> false

let isSupportedTransformTarget functionNames target =
  match F.tryFind target functionNames with
  | Some "Darklang.Stdlib.List.map_i64_i64"
  | Some "Darklang.Stdlib.List.reverse_i64" ->
      true
  | _ -> false

let isTransformBody functionNames body =
  let _, targets =
    mapExpr
      (fun targets expression ->
        match expression with
        | C.Call (target, _) -> (expression, FS.add target targets)
        | _ -> (expression, targets))
      FS.empty body
  in
  (not (FS.is_empty targets))
  && FS.for_all (isSupportedTransformTarget functionNames) targets

let eligible functionNames (callee : C.functionDef)
    (ownership : O.callSignature) =
  let rec hasConsumedListParameter parameters modes =
    match (parameters, modes) with
    | ( (_, AST.TList AST.TInt64) :: _,
        (O.ConsumedCallParameter | O.UniqueCallParameter) :: _ ) ->
        true
    | _ :: parameters, _ :: modes -> hasConsumedListParameter parameters modes
    | _, _ -> false
  in
  C.functionReturnType callee = AST.TList AST.TInt64
  && isTransformBody functionNames callee.C.body
  && ownership.O.result = O.UniqueProducedCallResult
  && hasConsumedListParameter
       (NonEmptyList.toList (C.functionParameterTypes callee))
       ownership.O.parameters

let ownershipBoundary boundaryId argument =
  C.Call (boundaryId, NonEmptyList.fromList [ argument ])

let substitutions boundaryId parameters modes arguments =
  let rec build result parameters modes arguments =
    match (parameters, modes, arguments) with
    | (binding, typ) :: parameters, mode :: modes, argument :: arguments ->
        let replacement =
          if typ = AST.TList AST.TInt64 && mode = O.ConsumedCallParameter then
            ownershipBoundary boundaryId argument
          else argument
        in
        build
          (C.BindingIdMap.add binding replacement result)
          parameters modes arguments
    | [], [], [] -> Some result
    | _ -> None
  in
  build C.BindingIdMap.empty parameters modes arguments

(*
   Fuse only compiler-selected unique list calls, and only when substitution
   cannot duplicate evaluation. Unsupported calls keep their scheduled ANF
   specialization and the ordinary persistent-list representation.
*)
let fuse functionNames plan functions =
  let boundaryId =
    Seq.find_map
      (fun (id, name) ->
        if name = "Darklang.Stdlib.List.__arrayOwnershipBoundary_i64" then
          Some id
        else None)
      (F.toSeq functionNames)
  in
  let checkedById =
    F.ofList
      (List.map
         (fun (definition : C.functionDef) -> (definition.C.id, definition))
         functions)
  in
  let rewrites =
    SiteMap.of_list
      (List.map
         (fun (rewrite : M.callRewrite) ->
           (rewrite.M.site, rewrite.M.ownership))
         (M.rewrites plan))
  in
  let grouped =
    List.fold_left
      (fun callers (rewrite : M.callRewrite) ->
        let targets =
          Option.value
            (F.tryFind rewrite.M.site.O.caller callers)
            ~default:F.empty
        in
        let sites =
          Option.value
            (F.tryFind rewrite.M.original.HIR.target targets)
            ~default:[]
        in
        F.add rewrite.M.site.O.caller
          (F.add rewrite.M.original.HIR.target (rewrite.M.site :: sites) targets)
          callers)
      F.empty (M.rewrites plan)
  in
  let selectedByCaller =
    F.map
      (fun _ targets ->
        F.map (fun _ sites -> List.sort SiteOrder.compare sites) targets)
      grouped
  in
  let fuseFunction (definition : C.functionDef) =
    let initial =
      {
        selected =
          Option.value
            (F.tryFind definition.C.id selectedByCaller)
            ~default:F.empty;
        fused = Sites.empty;
        fusedValues = [];
      }
    in
    let rewrite state expression =
      match expression with
      | C.Let (pattern, value, body) when List.mem value state.fusedValues ->
          ( bindValue pattern value body,
            { state with fusedValues = removeFirst value state.fusedValues } )
      | C.Call (target, arguments) -> (
          match F.tryFind target state.selected with
          | Some (site :: rest) -> (
              let selected =
                if rest = [] then F.remove target state.selected
                else F.add target rest state.selected
              in
              let next = { state with selected } in
              match
                (SiteMap.find_opt site rewrites, F.tryFind target checkedById)
              with
              | Some ownership, Some callee
                when eligible functionNames callee ownership
                     && List.for_all isSafeArgument
                          (NonEmptyList.toList arguments) -> (
                  let boundaryId =
                    match boundaryId with
                    | Some id -> id
                    | None ->
                        Crash.crash
                          "Ownership boundary function is absent from semantic \
                           function metadata"
                  in
                  match
                    substitutions boundaryId
                      (NonEmptyList.toList (C.functionParameterTypes callee))
                      ownership.O.parameters
                      (NonEmptyList.toList arguments)
                  with
                  | Some replacements ->
                      let fused = substitute replacements callee.C.body in
                      ( fused,
                        {
                          next with
                          fused = Sites.add site next.fused;
                          fusedValues = fused :: next.fusedValues;
                        } )
                  | None -> (expression, next))
              | _ -> (expression, next))
          | Some [] | None -> (expression, state))
      | _ -> (expression, state)
    in
    let body, final = mapExpr rewrite initial definition.C.body in
    ({ definition with C.body }, final.fused)
  in
  let rewritten, fused =
    List.map fuseFunction functions
    |> List.fold_left
         (fun (definitions, allFused) (definition, fused) ->
           (definition :: definitions, Sites.union allFused fused))
         ([], Sites.empty)
  in
  { functions = List.rev rewritten; fusedSites = fused }
