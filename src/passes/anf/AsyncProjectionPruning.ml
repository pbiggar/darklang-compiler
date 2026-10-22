(* AsyncProjectionPruning.ml - Remove unused typed projections before they introduce async demands. *)
[@@@warning "-4"]

module A = ANF
module T = ANFEffects.TempSet

let prune (fn : A.functionDef) =
  let derived projections = function
    | A.SumPayload _ | A.TupleGet _ | A.RecordGet _ -> true
    | A.Atom (A.Var id) | A.TypedAtom (A.Var id, _) -> T.mem id projections
    | _ -> false
  in
  let rec expression projections = function
    | A.Return atom -> (A.Return atom, ANFEffects.addAtomUse atom T.empty)
    | A.Jump (target, atom) ->
        (A.Jump (target, atom), ANFEffects.addAtomUse atom (T.singleton target))
    | A.If (condition, yes, no) ->
        let yes, yesUses = expression projections yes in
        let no, noUses = expression projections no in
        ( A.If (condition, yes, no),
          ANFEffects.addAtomUse condition (T.union yesUses noUses) )
    | A.Join (parameter, continuation, entry) ->
        let projections = T.remove parameter.A.id projections in
        let continuation, continuationUses =
          expression projections continuation
        in
        let entry, entryUses = expression projections entry in
        ( A.Join (parameter, continuation, entry),
          T.remove parameter.A.id (T.union continuationUses entryUses) )
    | A.Let (id, operation, rest) ->
        let removable = derived projections operation in
        let projections =
          if removable then T.add id projections else T.remove id projections
        in
        let rest, uses = expression projections rest in
        if removable && not (T.mem id uses) then (rest, uses)
        else
          ( A.Let (id, operation, rest),
            ANFEffects.addCExprUses operation (T.remove id uses) )
  in
  (* Typed raw reads can establish owning edges even when their result is unused.
     Only aliases proven to originate in a typed projection are removable here;
     producer calls, raw reads, arithmetic and unrelated casts remain evaluated. *)
  let body, _ = expression T.empty fn.A.body in
  { fn with A.body }
