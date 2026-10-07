(* SSARefCountInsertion.fs - Insert ownership operations directly in SSA ANF blocks. *)
[@@@warning "-4"]
module A = ANF
module S = SSAANF
module F = RcTypeFacts
module R = RcReturnAnalysis
module Shape = RcShapePlanning
module C = RcCleanup
module P = MemoryPlanning
module L = RcSSAValueLiveness
module Ret = RcSSAReturnAnalysis
module M = F.TempMap
module Set = R.TempSet
type definitions = {operations : A.cExpr M.t; owned : Set.t; ownedParams : Set.t; types : AST.semanticType M.t; funcReg : TypeRegistries.functionRegistry}
let exists predicate = function Some value -> predicate value | None -> false
let named reg target predicate = exists (fun (name, _) -> predicate name) (FunctionIdMap.tryFind target reg)
let starts prefix name = String.starts_with ~prefix name
let contains text part = let rec at index = index + String.length part <= String.length text && (String.sub text index (String.length part) = part || at (index + 1)) in at 0
let isEmptyListCall reg = function A.Call (target, []) -> named reg target (starts "Darklang.Stdlib.List.__empty") | _ -> false
let definitions ctx frontier (func : S.functionDef) =
 let types = func.S.freshValueTypes in
 let operations = S.LabelMap.fold (fun _ block acc -> List.fold_left (fun acc (id, operation) -> M.add id operation acc) acc block.S.operations) func.S.blocks M.empty in
 let ownedOperations = M.fold (fun id operation owned -> match M.find_opt id types with
  | None -> owned
  | Some typ ->
    let shape = Shape.rcShapeForType ctx typ in
    let adopts = match operation with A.TypedAtom (A.Var source, resultType) -> M.find_opt source types = Some AST.TInternalRawPtr && (match resultType with AST.TStream _ -> false | _ -> true) | _ -> false in
    let null = match operation, typ with A.Atom (A.IntLiteral (A.Int64 0L)), AST.TList _ | A.Atom (A.IntLiteral (A.Int64 0L)), AST.TDict _ -> true | _ -> false in
    let materializes = match operation with A.BorrowedCall _ | A.IfValue _ -> true | _ -> false in
    if Shape.bindingNeedsShapeAutomaticDec ctx operation typ shape && (not (R.isBorrowingExpr operation) || adopts || materializes) && not (Shape.cexprProducesNonRcSentinel operation) && not null && not (isEmptyListCall ctx.F.funcReg operation) then Set.add id owned else owned) operations Set.empty in
 let ownedBlocks = S.LabelMap.fold (fun _ block owned -> List.fold_left (fun owned (param : A.typedParam) -> if P.rcShapeNeedsOwnedScopeRelease (Shape.rcShapeForType ctx param.A.typ) then Set.add param.A.id owned else owned) owned block.S.parameters) func.S.blocks ownedOperations in
 let returned = Ret.analyze func in
 let returningSelf = M.exists (fun id operation -> match operation with A.Call (target, _) when target = func.S.id -> exists (Set.mem id) (M.find_opt id returned.Ret.afterDefinition) | _ -> false) operations in
 let atEntry = if returningSelf then Option.value ~default:Set.empty (S.LabelMap.find_opt func.S.entry returned.Ret.atEntry) else Set.empty in
 let ownedParams = Set.of_list (List.filter_map (fun (param : A.typedParam) -> if (Set.mem param.A.id frontier || (Set.mem param.A.id atEntry && (match param.A.typ with AST.TRecord _ -> true | _ -> contains func.S.name "$trmo"))) && P.rcShapeNeedsOwnedScopeRelease (Shape.rcShapeForType ctx param.A.typ) then Some param.A.id else None) func.S.typedParams) in
 {operations; owned = Set.union ownedBlocks ownedParams; ownedParams; types; funcReg = ctx.F.funcReg}
let sourceOfAlias = R.tryOwnershipPreservingAliasSource
(*
   Pattern lowering reuses scalar getAt wrappers for erased payload types.
   Their result can still point into the source list after the call returns.
*)
let borrowedSource definitions = function
 | A.Call (target, A.Var source :: _) when named definitions.funcReg target (starts "Darklang.Stdlib.List.__getAt") -> Some source
 | A.Prim ((A.BitAnd | A.BitOr), A.Var source, _) | A.TupleGet (A.Var source, _) | A.RecordGet (_, A.Var source, _) | A.RawGet (A.Var source, _, _) | A.RawTake (A.Var source, _, _) | A.StringToRawPtr (A.Var source) | A.BlobToRawPtr (A.Var source) | A.DictToRawPtr (A.Var source) | A.ListToRawPtr (A.Var source) | A.FixedBlockToRawPtr (A.Var source) -> Some source
 | _ -> None
let ownerOf definitions id =
 let rec follow visited id = if Set.mem id visited then None else if Set.mem id definitions.owned then Some id else Option.bind (Option.bind (M.find_opt id definitions.operations) sourceOfAlias) (follow (Set.add id visited)) in follow Set.empty id
let isNonRcSentinel definitions id =
 let rec follow visited id = if Set.mem id visited then false else match M.find_opt id definitions.operations with
  | Some (A.Atom (A.StringLiteral _)) | Some (A.TypedAtom (A.StringLiteral _, _)) -> true
  | Some (A.Atom (A.IntLiteral (A.Int64 0L))) -> (match M.find_opt id definitions.types with Some (AST.TList _ | AST.TDict _) -> true | _ -> false)
  | Some operation when isEmptyListCall definitions.funcReg operation -> true
  | Some operation -> exists (follow (Set.add id visited)) (sourceOfAlias operation)
  | None -> false in follow Set.empty id
let liveOwners definitions live =
 let rec roots visited id = if Set.mem id visited then Set.empty else
  let visited = Set.add id visited in
  let own = Option.fold ~none:Set.empty ~some:Set.singleton (ownerOf definitions id) in
  let source = Option.bind (M.find_opt id definitions.operations) (fun operation -> match borrowedSource definitions operation with Some _ as source -> source | None -> sourceOfAlias operation) in
  Set.union own (Option.fold ~none:Set.empty ~some:(roots visited) source) in
 Set.fold (fun id acc -> Set.union acc (roots Set.empty id)) live Set.empty
let valueType definitions kind id = match M.find_opt id definitions.types with Some typ -> typ | None -> let A.TempId index = id in Crash.crash ("SSA RC: " ^ kind ^ " value TempId " ^ string_of_int index ^ " has no type")
let release ctx definitions id next =
 let typ = valueType definitions "owned" id in let shape = Shape.rcShapeForType ctx typ in
 let _, _, _, kind, metadata, nullable = C.createReturnDec ctx id typ shape None in
 let operation = C.releaseExprForShape id typ shape kind metadata nullable in
 let dummy, next = A.freshVar next in (dummy, operation), next, (dummy, AST.TUnit)
let retain ctx definitions id next =
 let typ = valueType definitions "retained" id in let shape = Shape.rcShapeForType ctx typ in
 let operation = C.retainExprForShape ctx id typ shape in
 let dummy, next = A.freshVar next in (dummy, operation), next, (dummy, AST.TUnit)
let maxValueId (func : S.functionDef) =
 let ids = List.map (fun (param : A.typedParam) -> param.A.id) func.S.typedParams in
 let ids = S.LabelMap.fold (fun _ block ids -> ids @ List.map (fun (param : A.typedParam) -> param.A.id) block.S.parameters @ List.map fst block.S.operations) func.S.blocks ids in List.fold_left (fun largest (A.TempId id) -> max largest id) (-1) ids
let releaseSource = function A.RefCountDec (A.Var id, _, _, _) | A.RefCountDecString (A.Var id) | A.RefCountDecBlob (A.Var id) | A.RefCountDecInt (A.Var id) -> Some id | _ -> None
let capturedValues definitions = function
 | A.TupleAlloc values | A.RecordAlloc (_, values) | A.ClosureAlloc (_, values) | A.RecordClone (_, _, values) | A.RecordReuse (_, _, _, values) -> values
 | A.Call (target, [_; value]) | A.TailCall (target, [_; value]) when named definitions.funcReg target (fun name -> starts "Darklang.Stdlib.List.__push_i64" name || starts "Darklang.Stdlib.List.__pushBack_i64" name) -> [value]
 | _ -> []
let getLive id table = Option.value ~default:Set.empty (M.find_opt id table)
let getBlockLive label table = Option.value ~default:Set.empty (S.LabelMap.find_opt label table)
let descending owners = List.sort (fun (A.TempId left) (A.TempId right) -> Int.compare right left) (Set.elements owners)
(*
   Insert retains and releases using SSA value liveness and edge ownership.
   A live cleanup after a recursive call prevents tail-call
   conversion. In that case the callee retains its arguments,
   so the caller must release its own frontier values.
   Source lifetime continues through the block's remaining
   effects. Finalizers and reference-count inspection can
   observe whether cleanup precedes those effects.
   The successor owns a retained borrowed projection.
   Its aggregate source no longer needs to survive the edge.
*)
let insertBlockLocal ctx frontier (func : S.functionDef) =
 let definitions = definitions (F.withTempTypes ctx func.S.freshValueTypes) frontier func in
 let liveness = L.analyze func in let returned = Ret.analyze func in
 let direct = M.fold (fun id _ ids -> if Set.mem id (getLive id returned.Ret.afterDefinition) then Set.add id ids else ids) definitions.operations Set.empty in
 let rec includeCaptured ids = let next = M.fold (fun id operation known -> if Set.mem id ids then List.fold_left (fun ids -> function A.Var source -> Set.add source ids | _ -> ids) known (capturedValues definitions operation) else known) definitions.operations ids in if Set.equal next ids then ids else includeCaptured next in
 let returningConstruction = includeCaptured direct in
 let selfRecursive = S.LabelMap.exists (fun _ block -> List.exists (fun (_, operation) -> match operation with A.Call (target, _) -> target = func.S.id | _ -> false) block.S.operations) func.S.blocks in
 let initial = A.VarGen (max 4000 (Int32.to_int (Int32.add (Int32.of_int (maxValueId func)) 1l))) in
 let appendRelease (operations, next, types) owner = let binding, next, (id, typ) = release ctx definitions owner next in operations @ [binding], next, M.add id typ types in
 let blocks, next, types = S.LabelMap.fold (fun label block (blocks, next, types) ->
  let operations, next, types, deferred = List.fold_left (fun (operations, next, types, deferred) ((id, originalOperation) as original) ->
   let needed = liveOwners definitions (getLive id liveness.L.afterDefinition) in
   let captures = capturedValues definitions originalOperation in
   let returning = Set.mem id returningConstruction in
   let captureRetains, transferred = List.fold_left (fun (retains, transferred) -> function
    | A.Var source when not (isNonRcSentinel definitions source) -> (match M.find_opt source definitions.types with
      | Some typ when P.rcShapeNeedsBorrowedRetain (Shape.rcShapeForType ctx typ) -> (match ownerOf definitions source with Some owner when returning && not (Set.mem owner needed) && not (Set.mem owner transferred) -> retains, Set.add owner transferred | _ -> source :: retains, transferred)
      | _ -> retains, transferred)
    | _ -> retains, transferred) ([], Set.empty) captures in
   let captureBindings, next, types = List.fold_left (fun (bindings, next, types) source -> let binding, next, (id, typ) = retain ctx definitions source next in binding :: bindings, next, M.add id typ types) ([], next, types) (List.rev captureRetains) in
   let captureBindings = List.rev captureBindings in
   let reuseBindings, next, types = match originalOperation with
    | A.RecordReuse (descriptor, _, A.Var source, _) ->
      List.fold_left (fun (bindings, next, types) (index, typ) ->
       let field, next = A.freshVar next in let releaseId, next = A.freshVar next in let shape = Shape.rcShapeForType ctx typ in
       let _, _, _, kind, metadata, nullable = C.createReturnDec ctx field typ shape None in
       let release = C.releaseExprForShape field typ shape kind metadata nullable in
       bindings @ [field, A.RecordGet (descriptor, A.Var source, index); releaseId, release], next, M.add releaseId AST.TUnit (M.add field typ types)) ([], next, types)
       (List.filter (fun (_, typ) -> P.rcShapeNeedsOwnedScopeRelease (Shape.rcShapeForType ctx typ)) (List.mapi (fun index (_, typ) -> index, typ) descriptor.A.fields))
    | A.RecordReuse _ -> Crash.crash "SSA RC: record reuse source must be a variable"
    | _ -> [], next, types in
   let operation = match originalOperation with A.RawSlotInit (ptr, offset, A.Var source, typ) when (match typ with AST.TStream _ -> false | _ -> true) && exists (fun owner -> not (Set.mem owner needed)) (ownerOf definitions source) -> A.RawWriteWord (ptr, offset, A.Var source) | _ -> originalOperation in
   let rawTransfer = match original, operation with (_, A.RawSlotInit (_, _, A.Var source, _)), A.RawWriteWord _ -> Option.fold ~none:Set.empty ~some:Set.singleton (ownerOf definitions source) | _ -> Set.empty in
   let frontierCandidates = match operation with A.Call (target, args) when target = func.S.id -> Set.of_list (List.filter_map (fun ((param : A.typedParam), argument) -> if Set.mem param.A.id frontier then match argument with A.Var source -> ownerOf definitions source | _ -> None else None) (List.combine func.S.typedParams args)) | _ -> Set.empty in
   let used = liveOwners definitions (ANFEffects.cexprTempUses operation) in
   let otherDead = Set.diff (Set.diff (Set.diff (Set.diff used needed) frontierCandidates) transferred) rawTransfer in
   let frontierTransfer = if Set.is_empty otherDead then frontierCandidates else Set.empty in
   let transferred = Set.union transferred (Set.union rawTransfer frontierTransfer) in
   let dead = Set.diff (Set.diff used needed) transferred in
   let dead = if Set.mem id definitions.owned && not (Set.mem id needed) && not (Set.mem id transferred) then Set.add id dead else dead in
   let deferredHere = if selfRecursive then Set.empty else dead in
   let dead = Set.diff dead deferredHere in
   let resultRetain, next, types = match operation with A.BorrowedCall _ | A.IfValue _ when Set.mem id definitions.owned -> let binding, next, (id, typ) = retain ctx definitions id next in [binding], next, M.add id typ types | _ -> [], next, types in
   let releases, next, types = Set.fold (fun owner (bindings, next, types) -> let binding, next, (id, typ) = release ctx definitions owner next in binding :: bindings, next, M.add id typ types) dead ([], next, types) in
   operations @ captureBindings @ reuseBindings @ ((id, operation) :: resultRetain) @ List.rev releases, next, types, Set.union deferred deferredHere) ([], next, types, Set.empty) block.S.operations in
  let operations, next, types = List.fold_left appendRelease (operations, next, types) (descending deferred) in
  S.LabelMap.add label {block with S.operations} blocks, next, types) func.S.blocks (S.LabelMap.empty, initial, func.S.freshValueTypes) in
 let blocks, next, types = match S.LabelMap.find_opt func.S.entry func.S.blocks, S.LabelMap.find_opt func.S.entry blocks with
  | Some original, Some entry when not selfRecursive -> (match entry.S.terminator with
    | S.Branch _ ->
      let owners = Set.inter (Set.of_list (List.map fst original.S.operations)) definitions.owned in
      let move = Set.inter (Set.of_list (List.filter_map (fun (_, operation) -> releaseSource operation) entry.S.operations)) owners in
      let entry = {entry with S.operations = List.filter (fun (_, operation) -> not (exists (fun id -> Set.mem id move) (releaseSource operation))) entry.S.operations} in
      let blocks = S.LabelMap.add func.S.entry entry blocks in
      S.LabelMap.fold (fun label block (blocks, next, types) -> match block.S.terminator with S.Return _ -> let operations, next, types = List.fold_left appendRelease (block.S.operations, next, types) (descending move) in S.LabelMap.add label {block with S.operations} blocks, next, types | _ -> blocks, next, types) blocks (blocks, next, types)
    | _ -> blocks, next, types)
  | _ -> blocks, next, types in
 let appendBindings operations owners next types = Set.fold (fun owner state -> appendRelease state owner) owners (operations, next, types) in
 let blocks, next, types = S.LabelMap.fold (fun label _ (blocks, next, types) ->
  let block = match S.LabelMap.find_opt label blocks with Some block -> block | None -> Crash.crash "SSA RC: block disappeared during cleanup" in
  match block.S.terminator with
  | S.Return (A.Var id) ->
    let owner = ownerOf definitions id in
    let needsRetain = match M.find_opt id definitions.types with Some typ -> P.rcShapeNeedsBorrowedRetain (Shape.rcShapeForType ctx typ) && Option.is_none owner | None -> false in
    let operations, next, types = if needsRetain then let binding, next, (id, typ) = retain ctx definitions id next in block.S.operations @ [binding], next, M.add id typ types else block.S.operations, next, types in
    let live = liveOwners definitions (getBlockLive label liveness.L.atTerminator) in
    let owners = match owner with Some owner -> Set.remove owner live | None -> live in
    let operations, next, types = appendBindings operations owners next types in S.LabelMap.add label {block with S.operations} blocks, next, types
  | S.Return _ -> S.LabelMap.add label block blocks, next, types
  | S.Jump (successor, args) ->
    let target = match S.LabelMap.find_opt successor blocks with Some target -> target | None -> Crash.crash "SSA RC: missing jump successor" in
    if List.length target.S.parameters <> List.length args then Crash.crash "SSA RC: jump argument count does not match parameters";
    let pairs = List.combine target.S.parameters args in
    let bindings, next, types, transferred = List.fold_left (fun (bindings, next, types, transferred) ((param : A.typedParam), arg) -> if not (Set.mem param.A.id definitions.owned) then bindings, next, types, transferred else match arg with
     | A.Var source when not (isNonRcSentinel definitions source) -> (match ownerOf definitions source with Some owner when not (Set.mem owner transferred) -> bindings, next, types, Set.add owner transferred | _ -> let binding, next, (id, typ) = retain ctx definitions source next in bindings @ [binding], next, M.add id typ types, transferred)
     | _ -> bindings, next, types, transferred) ([], next, types, Set.empty) pairs in
    let before = liveOwners definitions (getBlockLive label liveness.L.atTerminator) in
    let immediate = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128 | AST.TInt | AST.TBool | AST.TFloat64 | AST.TChar | AST.TDateTime | AST.TUnit -> true | _ -> false in
    let borrowed = List.fold_left (fun owners ((param : A.typedParam), arg) -> match arg with
     | A.Var source when immediate param.A.typ -> Set.union owners (liveOwners definitions (Set.singleton source))
     | A.Var source when Set.mem param.A.id definitions.owned && Option.is_none (ownerOf definitions source) && not (isNonRcSentinel definitions source) -> Set.union owners (liveOwners definitions (Set.singleton source))
     | _ -> owners) Set.empty pairs in
    let required = liveOwners definitions (getBlockLive successor liveness.L.atEntry) in
    let owners = Set.diff (Set.diff (Set.union before borrowed) required) transferred in
    let operations, next, types = appendBindings (block.S.operations @ bindings) owners next types in S.LabelMap.add label {block with S.operations} blocks, next, types
  | S.Branch (_, yes, no) ->
    let before = liveOwners definitions (getBlockLive label liveness.L.atTerminator) in
    let owners successor = Set.diff before (liveOwners definitions (getBlockLive successor liveness.L.atEntry)) in
    let blocks = S.LabelMap.add label block blocks in
    let add (blocks, next, types) successor owners = match S.LabelMap.find_opt successor blocks with None -> Crash.crash "SSA RC: missing branch successor" | Some block -> let bindings, next, types = appendBindings [] owners next types in S.LabelMap.add successor {block with S.operations = bindings @ block.S.operations} blocks, next, types in
    let state = add (blocks, next, types) yes (owners yes) in add state no (owners no)) blocks (blocks, next, types) in
 let entry = match S.LabelMap.find_opt func.S.entry blocks with Some block -> block | None -> Crash.crash "SSA RC: missing entry block" in
 let retains, _, types = Set.fold (fun parameter (bindings, next, types) -> let binding, next, (id, typ) = retain ctx definitions parameter next in binding :: bindings, next, M.add id typ types) definitions.ownedParams ([], next, types) in
 let blocks = S.LabelMap.add func.S.entry {entry with S.operations = List.rev retains @ entry.S.operations} blocks in
 {func with S.blocks; freshValueTypes = types}
