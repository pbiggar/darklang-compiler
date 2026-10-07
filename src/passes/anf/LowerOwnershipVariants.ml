(* LowerOwnershipVariants.ml - Preserve scheduled ownership clones and call routing in ANF. *)
[@@@warning "-4-42"]
module A = ANF
module O = OwnedIR
module H = HIR
module M = MaterializeOwnershipVariants
module F = FunctionIdMap
module FS = SpecializationIdentity.FunctionSet
module TM = InliningCommon.TempMap
let ( let* ) = Result.bind
type lowered = {functions : A.functionDef list; contracts : O.callSignature F.t; varGen : A.varGen}
type loweringError = MissingSourceFunction of AST.functionId | MissingSourceCalls of AST.functionId * O.callSiteIdentity list | InvalidOwnershipBoundary of AST.functionId
module SiteOrder = struct type t = O.callSiteIdentity let compare (first : t) (second : t) = let caller = Int64.unsigned_compare (AST.functionIdValue first.O.caller) (AST.functionIdValue second.O.caller) in if caller <> 0 then caller else let H.ValueId first = first.O.result in let H.ValueId second = second.O.result in Int.compare first second end
module SiteMap = Map.Make (SiteOrder)
module SiteSet = Set.Make (SiteOrder)
let directTarget = function A.Call (target, _) | A.BorrowedCall (target, _) | A.TailCall (target, _) -> Some target | _ -> None
let replaceTarget replacement = function A.Call (_, arguments) -> A.Call (replacement, arguments) | A.BorrowedCall (_, arguments) -> A.BorrowedCall (replacement, arguments) | A.TailCall (_, arguments) -> A.TailCall (replacement, arguments) | expression -> expression
let rec ownedCalls (block : ('leaf, 'id) O.block) = List.concat_map (function O.Evaluate (H.Call call) -> [call] | O.Evaluate (H.Branch (_, _, yes, no)) -> let yes = ownedCalls yes in let no = ownedCalls no in yes @ no | O.Evaluate (H.Leaf _ | H.ScalarBinding _) | O.Dup _ | O.Drop _ -> []) block.O.body.H.operations
let rec rewriteAll targets expression =
 let rewriteCExpr cexpr = match directTarget cexpr with Some target -> (match F.tryFind target targets with Some replacement -> replaceTarget replacement cexpr | None -> cexpr) | None -> cexpr in
 match expression with
 | A.Return _ | A.Jump _ -> expression
 | A.Let (id, cexpr, body) -> let cexpr = rewriteCExpr cexpr in let body = rewriteAll targets body in A.Let (id, cexpr, body)
 | A.If (condition, yes, no) -> let yes = rewriteAll targets yes in let no = rewriteAll targets no in A.If (condition, yes, no)
 | A.Join (parameter, continuation, entry) -> let continuation = rewriteAll targets continuation in let entry = rewriteAll targets entry in A.Join (parameter, continuation, entry)
(*
   HIR structured branches enumerate the entry arms before their
   continuation. ANF joins store the continuation first.
*)
let rewriteSelected caller sites replacements expression =
 let rec rewrite pending = function
 | (A.Return _ | A.Jump _) as expression -> expression, pending
 | A.Let (id, cexpr, body) -> let cexpr, afterCall = match directTarget cexpr, pending with Some target, (site, expected) :: rest when target = expected -> (match SiteMap.find_opt site replacements with Some replacement -> replaceTarget replacement cexpr | None -> cexpr), rest | _ -> cexpr, pending in let body, remaining = rewrite afterCall body in A.Let (id, cexpr, body), remaining
 | A.If (condition, yes, no) -> let yes, afterYes = rewrite pending yes in let no, afterNo = rewrite afterYes no in A.If (condition, yes, no), afterNo
 | A.Join (parameter, continuation, entry) -> let entry, afterEntry = rewrite pending entry in let continuation, remaining = rewrite afterEntry continuation in A.Join (parameter, continuation, entry), remaining in
 let rewritten, remaining = rewrite sites expression in match remaining with [] -> Ok rewritten | rest -> Error (MissingSourceCalls (caller, List.map fst rest))
module Make (Identity : O.Identity) = struct
 module Verify = VerifyOwnership.Make (Identity)
 let callSignature (definition : ('leaf, Identity.t) O.functionDef) = Verify.callSignatureOfFunction definition.O.ownership |> Result.map_error (fun _ -> InvalidOwnershipBoundary definition.O.definition.H.id)
(*
   ANF keeps the ordinary runtime representation in this slice. The lowering
   nevertheless makes every materialized symbol and ownership boundary
   explicit so RC insertion, tail-call rewriting, and later storage lowering
   share one authoritative contract registry.
*)
 let lower originalOwned plan originalANF varGen elidedSites =
  let anfById = F.ofList (List.map (fun (definition : A.functionDef) -> definition.A.id, definition) originalANF) in
  let replacements = List.filter (fun (rewrite : M.callRewrite) -> not (SiteSet.mem rewrite.M.site elidedSites)) (M.rewrites plan) |> List.map (fun (rewrite : M.callRewrite) -> rewrite.M.site, rewrite.M.specialized.H.target) |> SiteMap.of_list in
  let usedTargets = FS.of_list (List.map snd (SiteMap.bindings replacements)) in
  let retainedGroups = List.filter (fun group -> List.exists (fun (memberDefinition : ('leaf, Identity.t) M.specializedFunction) -> FS.mem memberDefinition.M.functionDef.O.definition.H.id usedTargets) (NonEmptyList.toList group.M.members)) (M.groups plan) in
  let callsByCaller = F.ofList (List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.H.id, List.map (fun (call : H.functionCall) -> {O.caller = definition.O.definition.H.id; result = call.H.result.H.id}, call.H.target) (ownedCalls definition.O.definition.H.body)) originalOwned) in
  let* originals = List.fold_left (fun result (definition : A.functionDef) -> let* rewritten = result in
   let sites = Option.value (F.tryFind definition.A.id callsByCaller) ~default:[] |> List.filter (fun (site, _) -> not (SiteSet.mem site elidedSites)) in
   if not (List.exists (fun (site, _) -> SiteMap.mem site replacements) sites) then Ok (definition :: rewritten) else let* body = rewriteSelected definition.A.id sites replacements definition.A.body in Ok ({definition with A.body} :: rewritten)) (Ok []) originalANF |> Result.map List.rev in
  let* clones, currentVarGen = List.fold_left (fun result group -> let* clones, currentVarGen = result in
   let members = NonEmptyList.toList group.M.members in
   let targets = F.ofList (List.map (fun (memberDefinition : ('leaf, Identity.t) M.specializedFunction) -> memberDefinition.M.original, memberDefinition.M.functionDef.O.definition.H.id) members) in
   List.fold_left (fun result (memberDefinition : ('leaf, Identity.t) M.specializedFunction) -> let* clones, currentVarGen = result in
    match F.tryFind memberDefinition.M.original anfById with None -> Error (MissingSourceFunction memberDefinition.M.original) | Some source ->
    let parameters, mapping, afterParameters = List.fold_left (fun (parameters, mapping, current) (parameter : A.typedParam) -> let fresh, next = A.freshVar current in {parameter with A.id = fresh} :: parameters, TM.add parameter.A.id fresh mapping, next) ([], TM.empty, currentVarGen) source.A.typedParams in
    let parameters = List.rev parameters in let renamedBody, nextVarGen = InliningCommon.renameExpr mapping afterParameters source.A.body in
    let clone = {source with A.id = memberDefinition.M.functionDef.O.definition.H.id; name = memberDefinition.M.functionDef.O.definition.H.name; typedParams = parameters; body = rewriteAll targets renamedBody} in Ok (clone :: clones, nextVarGen)) (Ok (clones, currentVarGen)) members) (Ok ([], varGen)) retainedGroups in
  let clones = List.rev clones in
  let definitions = List.concat_map (fun group -> List.map (fun (memberDefinition : ('leaf, Identity.t) M.specializedFunction) -> memberDefinition.M.functionDef) (NonEmptyList.toList group.M.members)) retainedGroups in
  let* contracts = List.fold_left (fun result (definition : ('leaf, Identity.t) O.functionDef) -> let* contracts = result in let* signature = callSignature definition in Ok (F.add definition.O.definition.H.id signature contracts)) (Ok F.empty) definitions in
  Ok {functions = originals @ clones; contracts; varGen = currentVarGen}
end
