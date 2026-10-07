(* Cleanup.fs - Plan retain/release placement and preserve cleanup across tail calls. *)
[@@@warning "-4"]
module A = ANF
module F = RcTypeFacts
module R = RcReturnAnalysis
module S = RcShapePlanning
module P = MemoryPlanning
module K = Continuations
module M = F.TempMap
module Set = R.TempSet
type returnDec = A.tempId * AST.semanticType * MemoryModel.rcShape * MemoryModel.rcKind option * MemoryModel.rcMetadata option * bool
type internalOwnedParamKind = ReturnedAccumulator | NonEscapingLoopState
type ownedParamDec = {paramIndex : int; releaseOnTerminalReturn : bool; dec : returnDec}
type recordReuseCleanup = {descriptor : A.recordDescriptor; source : A.tempId; fields : (int * AST.semanticType * MemoryModel.rcShape) list}
(*
   Stored state for rebuilding a Let while unwinding an expression spine
   Managed child edges displaced by RecordReuse. The source descriptor may
   differ from the target for boxed-sum variant changes. Replacements are
   retained before these fields are loaded and released; stores happen
   afterward.
   The pass owns exactly one pending release for this value, and its next
   use transfers that ownership into a closed returned aggregate suffix.
*)
type letFrame = {tempId : A.tempId; cExpr : A.cExpr; allocationIncTargets : (A.tempId * AST.semanticType * MemoryModel.rcShape) list; recordReuseCleanup : recordReuseCleanup option; transferableOwnership : returnDec option; returnInc : (AST.semanticType * MemoryModel.rcShape) option; branchDec : returnDec option}
let tryItem index values = if index < 0 then None else List.nth_opt values index
let createReturnDec ctx id typ shape kind =
 let metadata = match P.rcShapeReleaseOperation shape with Some (MemoryModel.FixedSizeRoot _) -> Some (S.rcMetadataForTypeAndShape ctx typ shape) | Some MemoryModel.DynamicStringBuffer | Some MemoryModel.DynamicBlobBuffer | Some MemoryModel.DynamicIntBuffer | None -> None in
 id, typ, shape, kind, metadata, P.isNullablePointerSumType ctx.F.sumShapeReg typ
(*
   The Int buffer operation shares String's header but skips zero.
*)
let retainExprForShape ctx id typ shape = match P.rcShapeRetainOperation shape with
 | Some MemoryModel.DynamicStringBuffer -> if P.isNullablePointerSumType ctx.F.sumShapeReg typ then A.RefCountIncInt (A.Var id) else A.RefCountIncString (A.Var id)
 | Some MemoryModel.DynamicIntBuffer -> A.RefCountIncInt (A.Var id)
 | Some MemoryModel.DynamicBlobBuffer -> A.RefCountIncBlob (A.Var id)
 | Some (MemoryModel.FixedSizeRoot (size, kind)) -> A.RefCountInc (A.Var id, size, kind, Some (S.rcMetadataForTypeAndShape ctx typ shape))
 | None -> Crash.crash ("retainExprForShape: type '" ^ StructuralFormat.semanticType typ ^ "' does not have an RC retain operation")
(*
   Nullable sums use the zero-safe dynamic-buffer operation.
*)
let releaseExprForShape id typ shape kind metadata nullable = match P.rcShapeReleaseOperation shape with
 | Some MemoryModel.DynamicStringBuffer -> if nullable then A.RefCountDecInt (A.Var id) else A.RefCountDecString (A.Var id)
 | Some MemoryModel.DynamicIntBuffer -> A.RefCountDecInt (A.Var id)
 | Some MemoryModel.DynamicBlobBuffer -> A.RefCountDecBlob (A.Var id)
 | Some (MemoryModel.FixedSizeRoot (size, defaultKind)) ->
   let metadata = match metadata with Some metadata -> metadata | None -> Crash.crash ("releaseExprForShape: fixed-size type '" ^ StructuralFormat.semanticType typ ^ "' is missing RC metadata") in
   A.RefCountDec (A.Var id, size, Option.value ~default:defaultKind kind, Some metadata)
 | None -> Crash.crash ("releaseExprForShape: type '" ^ StructuralFormat.semanticType typ ^ "' does not have an RC release operation")
let isMapHelper name = name = "Darklang.Stdlib.List.__mapHelper" || String.starts_with ~prefix:"Darklang.Stdlib.List.__mapHelper_" name
let functionParamReturnTransfersOwnedAccumulator ctx func index typ =
 let name = Option.fold ~none:"" ~some:fst (FunctionIdMap.tryFind func ctx.F.funcReg) in
 let returnsClosureList = match F.tryGetFuncReturnTypeFromReg ctx func with Some (AST.TList (AST.TFunction _)) -> true | _ -> false in
 match isMapHelper name, index, typ with true, 0, AST.TList _ when returnsClosureList -> true | true, 2, AST.TList _ -> true | _ -> false
(*
   Recognize managed parameters that can become owned loop state. Returned
   accumulators transfer their final edge to the caller; non-escaping state is
   released at terminal returns. Both forms release obsolete values before
   adopting freshly owned replacements at self-recursive backedges.
*)
let internalOwnedTailParamKind (func : A.functionDef) index (param : A.typedParam) =
 let rec canonicalAlias aliases id = match M.find_opt id aliases with Some source when source <> id -> canonicalAlias aliases source | _ -> id in
 let rec returned aliases = function
 | A.Join _ | A.Jump _ -> false, false
 | A.Return (A.Var id) -> canonicalAlias aliases id = param.A.id, false
 | A.Return _ -> false, false
 | A.Let (call, A.Call (target, args), A.Return (A.Var result)) when target = func.A.id && call = result ->
   (match tryItem index args with Some (A.Var replacement) when canonicalAlias aliases replacement <> param.A.id -> true, true | _ -> false, false)
 | A.Let (id, A.Atom (A.Var source), body) | A.Let (id, A.TypedAtom (A.Var source, _), body) -> returned (M.add id (canonicalAlias aliases source) aliases) body
 | A.Let (_, A.Call (target, _), _) when target = func.A.id -> false, false
 | A.Let (_, _, body) -> returned aliases body
 | A.If (_, yes, no) -> let yv, yr = returned aliases yes in let nv, nr = returned aliases no in yv && nv, yr || nr in
 let rec nonEscaping aliases = function
 | A.Join _ | A.Jump _ -> false, false, false
 | A.Return (A.Var id) -> canonicalAlias aliases id <> param.A.id, false, false
 | A.Return _ -> true, false, false
 | A.Let (call, A.Call (target, args), A.Return (A.Var result)) when target = func.A.id && call = result ->
   (match tryItem index args with Some (A.Var replacement) -> true, true, canonicalAlias aliases replacement <> param.A.id | Some _ -> true, true, true | None -> false, false, false)
 | A.Let (id, A.Atom (A.Var source), body) | A.Let (id, A.TypedAtom (A.Var source, _), body) -> nonEscaping (M.add id (canonicalAlias aliases source) aliases) body
 | A.Let (_, A.Call (target, _), _) when target = func.A.id -> false, false, false
 | A.Let (_, _, body) -> nonEscaping aliases body
 | A.If (_, yes, no) -> let yv, yr, yp = nonEscaping aliases yes in let nv, nr, np = nonEscaping aliases no in yv && nv, yr || nr, yp || np in
 let supported = match param.A.typ with AST.TRecord _ -> true | _ ->
   let needle = "$trmo" in let len = String.length needle in let rec find index = index + len <= String.length func.A.name && (String.sub func.A.name index len = needle || find (index + 1)) in find 0 in
 if supported && func.A.returnType = param.A.typ then let valid, recurse = returned M.empty func.A.body in if valid && recurse then Some ReturnedAccumulator else None
 else if func.A.returnType <> param.A.typ then let valid, recurse, replace = nonEscaping M.empty func.A.body in if valid && recurse && replace then Some NonEscapingLoopState else None
 else None
(*
   An owned loop parameter may adopt a freshly produced value or another
   owned loop parameter. It must not adopt a projection borrowed from the old
   state: releasing that parent at the backedge can invalidate the projection
   before the next iteration starts.
*)
let internalOwnedTailParamHasSafeReplacements (func : A.functionDef) index owned =
 let isBorrowedResult = function A.IfValue _ | A.TupleGet _ | A.RecordGet _ | A.RecordReuse _ | A.RawGet _ | A.StringToRawPtr _ | A.BlobToRawPtr _ | A.DictToRawPtr _ | A.ListToRawPtr _ | A.FixedBlockToRawPtr _ | A.BorrowedCall _ | A.Atom (A.Var _) | A.TypedAtom (A.Var _, _) -> true | _ -> false in
 let rec validate safe = function
 | A.Join _ | A.Jump _ -> false | A.Return _ -> true
 | A.Let (call, A.Call (target, args), A.Return (A.Var result)) when target = func.A.id && call = result ->
   (match tryItem index args with Some (A.Var replacement) -> Set.mem replacement safe | Some _ -> true | None -> false)
 | A.Let (_, A.Call (target, _), _) when target = func.A.id -> false
 | A.Let (id, expr, body) -> let safe = match expr with
   | A.Atom (A.Var source) | A.TypedAtom (A.Var source, _) when Set.mem source safe -> Set.add id safe
   | _ when isBorrowedResult expr -> safe | _ -> Set.add id safe in validate safe body
 | A.If (_, yes, no) -> validate safe yes && validate safe no in validate owned func.A.body
(*
   Insert RefCountInc for returned parameters at a Return node
*)
let insertParamIncsAtReturn ctx incs returned expr gen types =
 List.fold_right (fun (id, typ, shape) (expr, gen, types) -> let dummy, gen = A.freshVar gen in
  A.Let (dummy, retainExprForShape ctx id typ shape, expr), gen, M.add dummy AST.TUnit types)
  (List.filter (fun (id, _, _) -> Set.mem id returned) incs) (expr, gen, types)
(*
   Insert RefCountDec operations before a Return using the current dec stack
*)
let insertReturnDecs decs expr gen types =
 List.fold_left (fun (expr, gen, types) (id, typ, shape, kind, metadata, nullable) -> let dummy, gen = A.freshVar gen in
  A.Let (dummy, releaseExprForShape id typ shape kind metadata nullable, expr), gen, M.add dummy AST.TUnit types) (expr, gen, types) (List.rev decs)
(*
   Apply a single Let frame around an expression (uses current varGen/types)
*)
let applyLetFrame ctx frame (expr, gen, types) =
 let incs, gen = List.fold_left (fun (acc, gen) (id, typ, shape) -> let dummy, gen = A.freshVar gen in (dummy, retainExprForShape ctx id typ shape) :: acc, gen) ([], gen) frame.allocationIncTargets in
 let incs = List.rev incs in let types = List.fold_left (fun types (id, _) -> M.add id AST.TUnit types) types incs in
 let cleanup, gen, types = match frame.recordReuseCleanup with None -> [], gen, types | Some cleanup ->
  let bindings, gen, types = List.fold_left (fun (bindings, gen, types) (index, typ, shape) ->
   let field, gen = A.freshVar gen in let release, gen = A.freshVar gen in
   let _, _, _, kind, metadata, nullable = createReturnDec ctx field typ shape None in
   let expression = releaseExprForShape field typ shape kind metadata nullable in
   (release, expression) :: (field, A.RecordGet (cleanup.descriptor, A.Var cleanup.source, index)) :: bindings,
   gen, M.add release AST.TUnit (M.add field typ types)) ([], gen, types) cleanup.fields in List.rev bindings, gen, types in
 let returned, gen, types = match frame.returnInc with None -> [], gen, types | Some (typ, shape) ->
  let id, gen = A.freshVar gen in [id, retainExprForShape ctx frame.tempId typ shape], gen, M.add id AST.TUnit types in
 K.wrapBindings incs (K.wrapBindings cleanup (A.Let (frame.tempId, frame.cExpr, K.wrapBindings returned expr))), gen, types
(*
   Apply a stack of Let frames (innermost-first)
*)
let applyLetFrames ctx frames state = List.fold_left (fun state frame -> applyLetFrame ctx frame state) state frames
let tailCallArgTempIds expr =
 let fromAtom = function A.Var id -> Set.singleton id | _ -> Set.empty in
 match expr with A.TailCall (_, args) -> List.fold_left (fun ids value -> Set.union ids (fromAtom value)) Set.empty args
 | A.IndirectTailCall (value, args) | A.ClosureTailCall (value, args) -> List.fold_left (fun ids value -> Set.union ids (fromAtom value)) (fromAtom value) args | _ -> Set.empty
let isSelfTailCallTarget ctx current target = target = current ||
 match FunctionIdMap.tryFind current ctx.F.funcReg, FunctionIdMap.tryFind target ctx.F.funcReg with Some (current, _), Some (target, _) -> String.starts_with ~prefix:(current ^ "_") target | _ -> false
let isTempUsedAsSelfTailCallArg ctx current target expr =
 let rec loop aliases = function
 | R.RJump _ | R.RReturn _ -> false
 | R.RJoin (_, continuation, entry, _) -> loop aliases continuation || loop aliases entry
 | R.RLet (_, A.Call (target, args), _, _) when isSelfTailCallTarget ctx current target && List.exists (function A.Var id -> Set.mem id aliases | _ -> false) args -> true
 | R.RLet (_, A.TailCall (target, args), _, _) when isSelfTailCallTarget ctx current target && List.exists (function A.Var id -> Set.mem id aliases | _ -> false) args -> true
 | R.RLet (id, A.Atom (A.Var source), body, _) | R.RLet (id, A.TypedAtom (A.Var source, _), body, _) when Set.mem source aliases -> loop (Set.add id aliases) body
 | R.RLet (_, _, body, _) -> loop aliases body
 | R.RIf (_, yes, no, _) -> loop aliases yes || loop aliases no in loop (Set.singleton target) expr
let rec collectMovableTailDecPrefix arguments expr = match expr with
 | A.Let (id, (A.RefCountDec (A.Var value, _, _, _) as operation), rest) when not (Set.mem value arguments) ->
   let bindings, rest = collectMovableTailDecPrefix arguments rest in (id, operation) :: bindings, rest
 | A.Let (id, (A.RefCountDecString value as operation), rest)
 | A.Let (id, (A.RefCountDecBlob value as operation), rest)
 | A.Let (id, (A.RefCountDecInt value as operation), rest) ->
   if (match value with A.Var value -> Set.mem value arguments | _ -> false) then [], expr else
   let bindings, rest = collectMovableTailDecPrefix arguments rest in (id, operation) :: bindings, rest
 | _ -> [], expr
let rec moveDecsBeforeNonSelfTailCalls current = function
 | (A.Jump _ | A.Return _) as expr -> expr
 | A.Join (param, continuation, entry) -> A.Join (param, moveDecsBeforeNonSelfTailCalls current continuation, moveDecsBeforeNonSelfTailCalls current entry)
 | A.If (condition, yes, no) -> A.If (condition, moveDecsBeforeNonSelfTailCalls current yes, moveDecsBeforeNonSelfTailCalls current no)
 | A.Let (id, operation, body) ->
   let body = moveDecsBeforeNonSelfTailCalls current body in
   match operation with A.TailCall (target, _) when target <> current ->
    let bindings, body = collectMovableTailDecPrefix (tailCallArgTempIds operation) body in K.wrapBindings bindings (A.Let (id, operation, body))
   | _ -> A.Let (id, operation, body)
let insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx current owned expr gen types =
 let decsForSelfTailCall args = List.filter_map (fun owned -> let id, _, _, _, _, _ = owned.dec in match tryItem owned.paramIndex args with Some (A.Var argument) when argument = id -> None | _ -> Some owned.dec) owned in
 let terminal = List.filter_map (fun owned -> if owned.releaseOnTerminalReturn then Some owned.dec else None) owned in
 let wrap decs state = List.fold_left (fun (expr, gen, types) (id, typ, shape, kind, metadata, nullable) ->
  let dummy, gen = A.freshVar gen in A.Let (dummy, releaseExprForShape id typ shape kind metadata nullable, expr), gen, M.add dummy AST.TUnit types) state decs in
 let rec rewrite terminalState expr gen types = match expr with
 | A.Jump _ -> expr, gen, types
 | A.Join (param, continuation, entry) -> let continuation, gen, types = rewrite terminalState continuation gen types in let entry, gen, types = rewrite terminalState entry gen types in A.Join (param, continuation, entry), gen, types
 | A.Return _ when terminalState -> wrap terminal (expr, gen, types)
 | A.Return _ -> expr, gen, types
 | A.If (condition, yes, no) -> let yes, gen, types = rewrite terminalState yes gen types in let no, gen, types = rewrite terminalState no gen types in A.If (condition, yes, no), gen, types
 | A.Let (id, (A.Call (target, args) as operation), body) when isSelfTailCallTarget ctx current target -> let body, gen, types = rewrite false body gen types in wrap (decsForSelfTailCall args) (A.Let (id, operation, body), gen, types)
 | A.Let (id, (A.TailCall (target, args) as operation), body) when isSelfTailCallTarget ctx current target -> let body, gen, types = rewrite false body gen types in wrap (decsForSelfTailCall args) (A.Let (id, operation, body), gen, types)
 | A.Let (id, operation, body) -> let body, gen, types = rewrite terminalState body gen types in A.Let (id, operation, body), gen, types in rewrite true expr gen types
let isClosureMapHelperTarget ctx target = Option.fold ~none:false ~some:(fun (name, _) -> isMapHelper name) (FunctionIdMap.tryFind target ctx.F.funcReg)
(*
   Find the two rare post-RC cleanups with one allocation-free body scan.
*)
let rec requiredFunctionCleanups ctx current = function
 | A.Jump _ | A.Return _ -> false, false
 | A.Join (_, yes, no) | A.If (_, yes, no) -> let ym, yt = requiredFunctionCleanups ctx current yes in let nm, nt = requiredFunctionCleanups ctx current no in ym || nm, yt || nt
 | A.Let (_, expr, body) -> let bm, bt = requiredFunctionCleanups ctx current body in
   let map = match expr with A.Call (target, _) | A.TailCall (target, _) -> isClosureMapHelperTarget ctx target | _ -> false in
   let tail = match expr with A.TailCall (target, _) -> target <> current | _ -> false in map || bm, tail || bt
(*
   Insert reference counting operations using return analysis and a dec stack
   Returns (transformed expr, varGen, types defined in this subtree)
*)
let rec insertClosureMapSourceRetainsBeforeHelperCalls ctx current expr gen types =
 let currentIsMapHelper = isClosureMapHelperTarget ctx current in
 let targetReturnsClosureList target = match F.tryGetFuncReturnTypeFromReg ctx target with Some (AST.TList (AST.TFunction _)) -> true | _ -> false in
 let wrap target args expr gen types = match currentIsMapHelper, targetReturnsClosureList target, args with false, true, A.Var source :: _ ->
  (match F.tryGetType (F.withTempTypes ctx types) source with Some typ -> let shape = S.rcShapeForType ctx typ in
   if P.rcShapeNeedsBorrowedRetain shape then let dummy, gen = A.freshVar gen in A.Let (dummy, retainExprForShape ctx source typ shape, expr), gen, M.add dummy AST.TUnit types else expr, gen, types
   | None -> expr, gen, types) | _ -> expr, gen, types in
 match expr with
 | A.Jump _ | A.Return _ -> expr, gen, types
 | A.Join (param, continuation, entry) -> let continuation, gen, types = insertClosureMapSourceRetainsBeforeHelperCalls ctx current continuation gen types in let entry, gen, types = insertClosureMapSourceRetainsBeforeHelperCalls ctx current entry gen types in A.Join (param, continuation, entry), gen, types
 | A.If (condition, yes, no) -> let yes, gen, types = insertClosureMapSourceRetainsBeforeHelperCalls ctx current yes gen types in let no, gen, types = insertClosureMapSourceRetainsBeforeHelperCalls ctx current no gen types in A.If (condition, yes, no), gen, types
 | A.Let (id, (A.Call (target, args) as operation), body) when isClosureMapHelperTarget ctx target -> let body, gen, types = insertClosureMapSourceRetainsBeforeHelperCalls ctx current body gen types in wrap target args (A.Let (id, operation, body)) gen types
 | A.Let (id, (A.TailCall (target, args) as operation), body) when isClosureMapHelperTarget ctx target -> let body, gen, types = insertClosureMapSourceRetainsBeforeHelperCalls ctx current body gen types in wrap target args (A.Let (id, operation, body)) gen types
 | A.Let (id, operation, body) -> let body, gen, types = insertClosureMapSourceRetainsBeforeHelperCalls ctx current body gen types in A.Let (id, operation, body), gen, types
