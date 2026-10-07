(* SSATailCallDetection.ml - Preserve ownership-safe tail calls on SSA ANF blocks. *)
[@@@warning "-4"]
module A = ANF
module S = SSAANF
module T = TailCallDetection
module M = RcTypeFacts.TempMap
module Set = RcReturnAnalysis.TempSet
type facts = {aliases : A.tempId M.t; borrows : Set.t M.t; retained : Set.t; released : Set.t}
let emptyFacts = {aliases = M.empty; borrows = M.empty; retained = Set.empty; released = Set.empty}
let advance facts (id, operation) =
 let root = T.canonicalTempId facts.aliases in
 let retained, released = match operation with
  | A.RefCountInc (A.Var value, _, _, _) | A.RefCountIncString (A.Var value) | A.RefCountIncBlob (A.Var value) | A.RefCountIncInt (A.Var value) -> Set.add (root value) facts.retained, facts.released
  | A.RefCountDec (A.Var value, _, _, _) | A.RefCountDecString (A.Var value) | A.RefCountDecBlob (A.Var value) | A.RefCountDecInt (A.Var value) -> Set.remove (root value) facts.retained, Set.add (root value) facts.released
  | _ -> facts.retained, facts.released in
 {aliases = T.extendAliasRoots facts.aliases id operation; borrows = T.extendBorrowRoots facts.aliases facts.borrows id operation; retained; released}
(*
   An alias or ownership event is certain only if every incoming path has
   it. A borrow may depend on any predecessor, so retain every source.
*)
let mergeFacts left right = {
 aliases = M.filter (fun id root -> M.find_opt id right.aliases = Some root) left.aliases;
 borrows = M.fold (fun id sources acc -> M.add id (Set.union (Option.value ~default:Set.empty (M.find_opt id acc)) sources) acc) right.borrows left.borrows;
 retained = Set.inter left.retained right.retained; released = Set.inter left.released right.released}
let addEdgeParameters (target : S.block) args facts =
 if List.length target.S.parameters <> List.length args then Crash.crash "SSA tail-call detection: edge argument count does not match block parameters";
 List.fold_left2 (fun current (param : A.typedParam) arg -> match arg with
  | A.Var source -> let root = T.canonicalTempId current.aliases source in
    let inherited = Option.value ~default:Set.empty (match M.find_opt source current.borrows with Some _ as sources -> sources | None -> M.find_opt root current.borrows) in
    {current with aliases = M.add param.A.id root current.aliases; borrows = if Set.is_empty inherited then current.borrows else M.add param.A.id inherited current.borrows}
  | _ -> current) facts target.S.parameters args
(*
   Propagate ownership-sensitive facts through SSA edges to a fixed point.
   Loop headers can receive another predecessor after their first visit.
*)
let equalFacts left right = M.equal (=) left.aliases right.aliases && M.equal Set.equal left.borrows right.borrows && Set.equal left.retained right.retained && Set.equal left.released right.released
let incomingFacts (func : S.functionDef) =
 let rec settle known = function
  | [] -> known
  | label :: rest -> match S.LabelMap.find_opt label known, S.LabelMap.find_opt label func.S.blocks with
    | Some facts, Some block ->
      let after = List.fold_left advance facts block.S.operations in
      let edges = match block.S.terminator with S.Return _ -> [] | S.Jump (target, args) -> [target, args] | S.Branch (_, yes, no) -> [yes, []; no, []] in
      let known, pending = List.fold_left (fun (current, queue) (label, args) -> match S.LabelMap.find_opt label func.S.blocks with
       | None -> current, queue
       | Some target -> let candidate = addEdgeParameters target args after in
         let next = match S.LabelMap.find_opt label current with None -> candidate | Some previous -> mergeFacts previous candidate in
         if (match S.LabelMap.find_opt label current with Some previous -> equalFacts previous next | None -> false) then current, queue else S.LabelMap.add label next current, label :: queue) (known, rest) edges in settle known pending
    | _ -> settle known rest in
 settle (S.LabelMap.singleton func.S.entry emptyFacts) [func.S.entry]
let detect recursiveMembers (func : S.functionDef) =
 if not (T.isEligibleFunctionName func.S.name) then func else
 let facts = incomingFacts func in
 let params = Set.of_list (List.map (fun (param : A.typedParam) -> param.A.id) func.S.typedParams) in
 let entry = match S.LabelMap.find_opt func.S.entry func.S.blocks with Some block -> block | None -> Crash.crash "SSA tail-call detection: missing entry block" in
 let wrap operations body = List.fold_right (fun (id, operation) body -> A.Let (id, operation, body)) operations body in
 let owned = T.leadingRetainedParams params (wrap entry.S.operations (A.Return A.UnitLiteral)) in
 let isCurrentMember target = match FunctionIdMap.tryFind func.S.id recursiveMembers, FunctionIdMap.tryFind target recursiveMembers with
  | Some current, Some other -> current.AST.typed.AST.resolved.AST.parsed.AST.binding = other.AST.typed.AST.resolved.AST.parsed.AST.binding
  | None, None -> target = func.S.id
  | _ -> false in
 let transform label (block : S.block) = match block.S.terminator, S.LabelMap.find_opt label facts with
  | S.Return value, Some facts ->
    let converted = T.detectTailCalls func.S.id isCurrentMember func.S.typedParams owned facts.released true facts.aliases facts.borrows facts.retained (wrap block.S.operations (A.Return value)) in
    let rec unpack operations = function A.Let (id, operation, rest) -> unpack ((id, operation) :: operations) rest | A.Return result -> {block with S.operations = List.rev operations; terminator = S.Return result} | _ -> Crash.crash "SSA tail-call detection changed a block exit" in unpack [] converted
  | _ -> block in
 {func with S.blocks = S.LabelMap.mapi transform func.S.blocks}
