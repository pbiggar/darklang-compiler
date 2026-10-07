(* RcSSAReturnAnalysis.ml - Track values that flow to a return across SSA block edges. *)
module A = ANF
module S = SSAANF
module Set = RcReturnAnalysis.TempSet
module M = RcTypeFacts.TempMap
type facts = {atEntry : Set.t S.LabelMap.t; atTerminator : Set.t S.LabelMap.t; afterDefinition : Set.t M.t}
let returnedAtom = function A.Var id -> Set.singleton id | A.UnitLiteral | A.IntLiteral _ | A.BoolLiteral _ | A.StringLiteral _ | A.FloatLiteral _ | A.FuncRef _ -> Set.empty
let returnedAcrossEdge (target : S.block) args returned =
 if List.length target.S.parameters <> List.length args then Crash.crash "SSA return analysis: edge argument count does not match block parameters";
 let parameters = Set.of_list (List.map (fun (param : A.typedParam) -> param.A.id) target.S.parameters) in
 let values = List.fold_left2 (fun values (param : A.typedParam) arg -> if Set.mem param.A.id returned then Set.union values (returnedAtom arg) else values) Set.empty target.S.parameters args in
 Set.union (Set.diff returned parameters) values
let returnedAtTerminator blocks known (block : S.block) =
 let incoming label args = match S.LabelMap.find_opt label blocks with None -> Crash.crash "SSA return analysis: missing successor block" | Some target -> returnedAcrossEdge target args (Option.value ~default:Set.empty (S.LabelMap.find_opt label known)) in
 match block.S.terminator with S.Return atom -> returnedAtom atom | S.Jump (target, args) -> incoming target args | S.Branch (_, yes, no) -> Set.union (incoming yes []) (incoming no [])
let beforeDefinition returned (id, operation) =
 let source = if Set.mem id returned then Option.fold ~none:Set.empty ~some:Set.singleton (RcReturnAnalysis.tryOwnershipPreservingAliasSource operation) else Set.empty in
 Set.union (Set.remove id returned) source
let factsForBlock blocks known (block : S.block) = let terminator = returnedAtTerminator blocks known block in List.fold_right (fun definition returned -> beforeDefinition returned definition) block.S.operations terminator, terminator
(*
   A backwards fixed point is required for joins with backedges. Facts at an
   edge refer to the source value, while facts inside a successor refer to its
   block parameter. Only ownership-preserving aliases inherit return status.
*)
let analyze (func : S.functionDef) =
 let rec settle known = let next = S.LabelMap.map (fun block -> fst (factsForBlock func.S.blocks known block)) func.S.blocks in if S.LabelMap.equal Set.equal next known then known else settle next in
 let atEntry = settle S.LabelMap.empty in
 let atTerminator = S.LabelMap.map (returnedAtTerminator func.S.blocks atEntry) func.S.blocks in
 let afterDefinition = S.LabelMap.fold (fun label block acc ->
  let start = Option.value ~default:Set.empty (S.LabelMap.find_opt label atTerminator) in
  let _, updates = List.fold_right (fun ((id, _) as definition) (returned, updates) -> beforeDefinition returned definition, M.add id returned updates) block.S.operations (start, M.empty) in M.fold (fun id returned state -> M.add id returned state) updates acc) func.S.blocks M.empty in
 {atEntry; atTerminator; afterDefinition}
