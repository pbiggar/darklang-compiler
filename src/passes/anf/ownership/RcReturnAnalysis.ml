(* RcReturnAnalysis.ml - Analyze aliases and ownership escaping through returns and aggregates. *)
[@@@warning "-4"]
module A = ANF
module TempMap = InliningCommon.TempMap
module TempSet = ANFEffects.TempSet
type returnAnnotatedExpr = RReturn of A.atom * TempSet.t | RLet of A.tempId * A.cExpr * returnAnnotatedExpr * TempSet.t | RIf of A.atom * returnAnnotatedExpr * returnAnnotatedExpr * TempSet.t | RJoin of A.typedParam * returnAnnotatedExpr * returnAnnotatedExpr * TempSet.t | RJump of A.tempId * A.atom * TempSet.t
(*
   Get the set of returned TempIds for a return-annotated expression
*)
let returnedSet = function RReturn (_, returned) | RLet (_, _, _, returned) | RIf (_, _, _, returned) | RJoin (_, _, _, returned) | RJump (_, _, returned) -> returned
(*
   Collect the alias chain for a TempId (includes the TempId itself)
*)
let rec collectAliasChain aliases id = match TempMap.find_opt id aliases with Some next -> TempSet.add id (collectAliasChain aliases next) | None -> TempSet.singleton id
(*
   Most TypedAtoms only preserve type information for an existing value. A
   RawPtr-to-Stream TypedAtom is different: Stream construction seeds its RC
   word at zero and relies on the first owning use to materialize an edge.
   Following that cast back to the RawPtr would invent ownership that does not
   yet exist and incorrectly remove the required retain.
*)
let tryOwnershipPreservingAliasSource = function A.Atom (A.Var source) -> Some source | A.TypedAtom (A.Var _, AST.TStream _) -> None | A.TypedAtom (A.Var source, _) | A.RecordReuse (_, _, A.Var source, _) -> Some source | _ -> None
(*
   Analyze return values and track alias chains in a single pass
*)
let rec analyzeReturns joins aliases = function
 | A.Jump (target, value) ->
   let returned = match TempMap.find_opt target joins with None -> let A.TempId id = target in Crash.crash ("Return analysis: join target TempId " ^ string_of_int id ^ " is not in scope")
   | Some returned when TempSet.mem target returned -> let argument = match value with A.Var id -> collectAliasChain aliases id | _ -> TempSet.empty in TempSet.union (TempSet.remove target returned) argument
   | Some returned -> returned in RJump (target, value, returned)
 | A.Join (param, continuation, entry) ->
   let body = analyzeReturns joins (TempMap.remove param.A.id aliases) continuation in
   let entry = analyzeReturns (TempMap.add param.A.id (returnedSet body) joins) aliases entry in RJoin (param, body, entry, returnedSet entry)
 | A.Return value -> RReturn (value, match value with A.Var id -> collectAliasChain aliases id | _ -> TempSet.empty)
 | A.Let (id, expr, body) ->
   let aliases = match tryOwnershipPreservingAliasSource expr with Some source -> TempMap.add id source aliases | None -> aliases in
   let body = analyzeReturns joins aliases body in RLet (id, expr, body, returnedSet body)
 | A.If (condition, yes, no) -> let yes = analyzeReturns joins aliases yes in let no = analyzeReturns joins aliases no in RIf (condition, yes, no, TempSet.union (returnedSet yes) (returnedSet no))
let atomOccurrenceCount id values = List.fold_left (fun count -> function A.Var id' when id = id' -> Int32.to_int (Int32.succ (Int32.of_int count)) | _ -> count) 0 values
(*
   Count ownership-bearing aggregate fields drawn from an alias family.
   RecordClone's source record is borrowed by the clone operation, so it cannot
   be the ownership-transfer use.
*)
let aggregateAliasFieldCount aliases expr =
 let fields values = TempSet.fold (fun id count -> Int32.to_int (Int32.add (Int32.of_int count) (Int32.of_int (atomOccurrenceCount id values)))) aliases 0 in
 match expr with A.TupleAlloc values | A.RecordAlloc (_, values) | A.ClosureAlloc (_, values) -> fields values
 | A.RecordClone (_, source, values) -> if TempSet.exists (fun id -> ANFEffects.atomUsesTemp id source) aliases then 0 else fields values | _ -> 0
let rawSlotAliasValueCount aliases = function A.RawSlotInit (_, _, A.Var value, _) when TempSet.mem value aliases -> 1 | _ -> 0
let rec returnAnnotatedExprUsesAnyAlias aliases body =
 let atom value = TempSet.exists (fun id -> ANFEffects.atomUsesTemp id value) aliases in
 match body with
 | RReturn (value, _) | RJump (_, value, _) -> atom value
 | RJoin (param, continuation, entry, _) -> returnAnnotatedExprUsesAnyAlias (TempSet.remove param.A.id aliases) continuation || returnAnnotatedExprUsesAnyAlias aliases entry
 | RLet (_, expr, body, _) -> TempSet.exists (fun id -> ANFEffects.cexprUsesTemp id expr) aliases || returnAnnotatedExprUsesAnyAlias aliases body
 | RIf (condition, yes, no, _) -> atom condition || returnAnnotatedExprUsesAnyAlias aliases yes || returnAnnotatedExprUsesAnyAlias aliases no
let cexprReleasesAnyAlias aliases = function A.RefCountDec (value, _, _, _) | A.RefCountDecString value | A.RefCountDecBlob value | A.RefCountDecInt value -> TempSet.exists (fun id -> ANFEffects.atomUsesTemp id value) aliases | _ -> false
let terminalPrintConsumesReturnedAlias aliases expr body = match expr, body with A.Print (value, _), RReturn (A.Var id, _) when TempSet.mem id aliases -> TempSet.exists (fun id -> ANFEffects.atomUsesTemp id value) aliases | _ -> false
(*
   Once ownership is transferred into an aggregate, do not permit observable
   work before that aggregate is returned. This preserves internal refcount
   probes while allowing nested tuple/record/closure construction suffixes.
*)
let rec aggregateFlowsDirectlyToReturn id body =
 let rec loop aliases = function
 | RReturn (A.Var id, _) -> TempSet.mem id aliases
 | RLet (id, expr, body, _) ->
   (match tryOwnershipPreservingAliasSource expr with Some source when TempSet.mem source aliases -> loop (TempSet.add id aliases) body
   | _ when terminalPrintConsumesReturnedAlias aliases expr body -> loop aliases body
   | _ -> aggregateAliasFieldCount aliases expr > 0 && aggregateFlowsDirectlyToReturn id body)
 | RIf (_, yes, no, _) -> loop aliases yes && loop aliases no
 | RJoin _ | RJump _ | RReturn _ -> false in loop (TempSet.singleton id) body
(*
   Find the aggregate use that begins a closed construction suffix ending in
   Return. Earlier borrowed uses preserve the candidate's owned edge; an
   explicit release makes the proof ineligible.
*)
let transfersIntoReturnedAggregate id body =
 let rec loop aliases = function
 | RLet (id, expr, body, _) ->
   (match tryOwnershipPreservingAliasSource expr with Some source when TempSet.mem source aliases -> loop (TempSet.add id aliases) body
   | _ -> if aggregateAliasFieldCount aliases expr > 0 then aggregateFlowsDirectlyToReturn id body
     else if rawSlotAliasValueCount aliases expr > 0 || cexprReleasesAnyAlias aliases expr then false else loop aliases body)
 | RIf (_, yes, no, _) -> loop aliases yes && loop aliases no
 | RJoin _ | RJump _ | RReturn _ -> false in loop (TempSet.singleton id) body
(*
   Move a locally-owned edge into its first ownership-bearing raw slot use
   when no alias remains live afterward. RawSlotInit normally retains a copied
   edge; the move instead lets the slot adopt the binding's pending ownership.
*)
let transfersIntoRawSlot id body =
 let rec loop aliases = function
 | RLet (next, expr, body, _) ->
   (match tryOwnershipPreservingAliasSource expr with Some source when TempSet.mem source aliases -> loop (TempSet.add next aliases) body
   | _ -> let uses = rawSlotAliasValueCount aliases expr in
     if uses > 0 then uses = 1 && not (returnAnnotatedExprUsesAnyAlias aliases body)
     else if aggregateAliasFieldCount aliases expr > 0 || cexprReleasesAnyAlias aliases expr then false else loop aliases body)
 | RIf (_, yes, no, _) -> loop aliases yes && loop aliases no
 | RJoin _ | RJump _ | RReturn _ -> false in loop (TempSet.singleton id) body
(*
   Check if a CExpr is a borrowing/aliasing operation
   Borrowed/aliased values should NOT get their own RefCountDec - the original value owns the memory
   Selects one of two existing values; no ownership transfer
   Extracts pointer from tuple/list - borrowed from parent
   Record projections borrow from the owning record
   Reuses the source allocation and transfers its ownership
   RawGet reads existing memory; it does not transfer ownership
   RawTake transfers the slot's existing ownership to the result
   RawPtr view is borrowed from the dynamic buffer
   RawPtr view is borrowed from the tagged container
   RawPtr view is borrowed from the fixed block
   Callee returns an alias kept alive by one of its arguments
   Alias/copy of existing variable - don't double-dec
   TypedAtom wrapping a variable - also borrowed
*)
let isBorrowingExpr = function
 | A.IfValue _ | A.TupleGet _ | A.RecordGet _ | A.RecordReuse _ | A.RawGet _ | A.StringToRawPtr _ | A.BlobToRawPtr _ | A.DictToRawPtr _ | A.ListToRawPtr _ | A.FixedBlockToRawPtr _ | A.BorrowedCall _ | A.Atom (A.Var _) | A.TypedAtom (A.Var _, _) -> true
 | A.RawTake _ -> false | _ -> false
