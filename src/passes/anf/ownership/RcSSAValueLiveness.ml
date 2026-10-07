(* RcSSAValueLiveness.ml - Compute operation and edge liveness for SSA ownership cleanup. *)
module A = ANF
module S = SSAANF
module Set = RcReturnAnalysis.TempSet
module M = RcTypeFacts.TempMap

type facts = {
  atEntry : Set.t S.LabelMap.t;
  atTerminator : Set.t S.LabelMap.t;
  afterDefinition : Set.t M.t;
}

let atomUses = function
  | A.Var id -> Set.singleton id
  | A.UnitLiteral | A.IntLiteral _ | A.BoolLiteral _ | A.StringLiteral _
  | A.FloatLiteral _ | A.FuncRef _ ->
      Set.empty

let liveAcrossEdge (target : S.block) args live =
  if List.length target.S.parameters <> List.length args then
    Crash.crash
      "SSA liveness: edge argument count does not match block parameters";
  let parameters =
    Set.of_list
      (List.map (fun (param : A.typedParam) -> param.A.id) target.S.parameters)
  in
  let values =
    List.fold_left2
      (fun values (param : A.typedParam) arg ->
        if Set.mem param.A.id live then Set.union values (atomUses arg)
        else values)
      Set.empty target.S.parameters args
  in
  Set.union (Set.diff live parameters) values

let liveAtTerminator blocks known (block : S.block) =
  let incoming label args =
    match S.LabelMap.find_opt label blocks with
    | None -> Crash.crash "SSA liveness: missing successor block"
    | Some target ->
        liveAcrossEdge target args
          (Option.value ~default:Set.empty (S.LabelMap.find_opt label known))
  in
  match block.S.terminator with
  | S.Return atom -> atomUses atom
  | S.Jump (target, args) -> incoming target args
  | S.Branch (condition, yes, no) ->
      Set.union (atomUses condition)
        (Set.union (incoming yes []) (incoming no []))

let beforeDefinition live (id, operation) =
  Set.union (Set.remove id live) (ANFEffects.cexprTempUses operation)

let analyze (func : S.functionDef) =
  let rec settle known =
    let next =
      S.LabelMap.map
        (fun block ->
          List.fold_right
            (fun definition live -> beforeDefinition live definition)
            block.S.operations
            (liveAtTerminator func.S.blocks known block))
        func.S.blocks
    in
    if S.LabelMap.equal Set.equal next known then known else settle next
  in
  let atEntry = settle S.LabelMap.empty in
  let atTerminator =
    S.LabelMap.map (liveAtTerminator func.S.blocks atEntry) func.S.blocks
  in
  let afterDefinition =
    S.LabelMap.fold
      (fun label block acc ->
        let start =
          Option.value ~default:Set.empty
            (S.LabelMap.find_opt label atTerminator)
        in
        let _, updates =
          List.fold_right
            (fun ((id, _) as definition) (live, updates) ->
              (beforeDefinition live definition, M.add id live updates))
            block.S.operations (start, M.empty)
        in
        M.fold (fun id live state -> M.add id live state) updates acc)
      func.S.blocks M.empty
  in
  { atEntry; atTerminator; afterDefinition }
