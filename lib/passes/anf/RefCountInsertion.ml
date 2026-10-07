(* RefCountInsertion.fs - Orchestrate function RC elaboration and verify complete type and join interfaces. *)
[@@@warning "-4"]
module A = ANF
module F = RcTypeFacts
module R = RcReturnAnalysis
module S = RcShapePlanning
module C = RcCleanup
module I = RcInsertExpression
module P = MemoryPlanning
module M = F.TempMap
module Set = R.TempSet
let ( let* ) = Result.bind
let paramId (param : A.typedParam) = param.A.id
let verifyOwnershipContracts ctx (contracts : OwnedIR.callSignature FunctionIdMap.t) (A.Program (functions, main)) =
 let managed typ = P.rcShapeNeedsOwnedScopeRelease (S.rcShapeForType ctx typ) in
 let parameterMatches typ = function OwnedIR.UnmanagedCallParameter -> not (managed typ) | OwnedIR.BorrowedCallParameter | OwnedIR.ConsumedCallParameter | OwnedIR.UniqueCallParameter -> managed typ in
 let resultMatches typ = function OwnedIR.UnmanagedCallResult -> not (managed typ) | OwnedIR.BorrowedCallResult _ | OwnedIR.ProducedCallResult | OwnedIR.UniqueProducedCallResult -> managed typ in
 let verifyFunction (func : A.functionDef) (contract : OwnedIR.callSignature) =
  if List.length func.A.typedParams <> List.length contract.OwnedIR.parameters then Error ("Ownership contract parameter count changed for " ^ func.A.name)
  else if List.exists2 (fun param ownership -> not (parameterMatches param.A.typ ownership)) func.A.typedParams contract.OwnedIR.parameters then Error ("Ownership contract parameter representation changed for " ^ func.A.name)
  else if not (resultMatches func.A.returnType contract.OwnedIR.result) then Error ("Ownership contract result representation changed for " ^ func.A.name)
  else match contract.OwnedIR.result with OwnedIR.BorrowedCallResult index when index < 0 || index >= List.length contract.OwnedIR.parameters -> Error ("Ownership contract borrowed result index changed for " ^ func.A.name) | _ -> Ok () in
 let rec verifyCalls owner = function
  | A.Return _ | A.Jump _ -> Ok ()
  | A.Let (_, operation, body) ->
    let call = match operation with
     | A.Call (target, args) | A.BorrowedCall (target, args) | A.TailCall (target, args) -> (match FunctionIdMap.tryFind target contracts with None -> Ok () | Some contract when List.length args = List.length contract.OwnedIR.parameters -> Ok () | Some _ -> Error ("Ownership call arity changed in " ^ owner))
     | _ -> Ok () in
    let* () = call in verifyCalls owner body
  | A.If (_, yes, no) -> let* () = verifyCalls owner yes in verifyCalls owner no
  | A.Join (_, continuation, entry) -> let* () = verifyCalls owner entry in verifyCalls owner continuation in
 let* () = List.fold_left (fun result (func : A.functionDef) -> let* () = result in let* () = match FunctionIdMap.tryFind func.A.id contracts with Some contract -> verifyFunction func contract | None -> Ok () in verifyCalls func.A.name func.A.body) (Ok ()) functions in
 verifyCalls "<main>" main
(*
   Reuse the pre-SSA proof for dictionary frontier loop state.
*)
let ownedDictionaryFrontierParams (func : A.functionDef) =
 let candidates = List.filter_map (fun (index, parameter) -> match parameter.A.typ with AST.TDict _ | AST.TTuple _ -> (match C.internalOwnedTailParamKind func index parameter with Some C.NonEscapingLoopState -> Some (index, parameter) | _ -> None) | _ -> None) (List.mapi (fun index param -> index, param) func.A.typedParams) in
 if not (List.exists (fun (_, param) -> match param.A.typ with AST.TDict _ -> true | _ -> false) candidates) then Set.empty else
 let rec validate ids = let next = Set.of_list (List.filter_map (fun (index, param) -> if C.internalOwnedTailParamHasSafeReplacements func index ids then Some (paramId param) else None) candidates) in if Set.equal next ids then ids else validate next in
 validate (Set.of_list (List.map (fun (_, param) -> (paramId param)) candidates))
type functionPhaseTimings = {returnAnalysisMs : float; parameterAnalysisMs : float; bodyInsertionMs : float; accumulatorCleanupMs : float; cleanupPlanningMs : float; cleanupRewriteMs : float}
let emptyFunctionPhaseTimings = {returnAnalysisMs = 0.; parameterAnalysisMs = 0.; bodyInsertionMs = 0.; accumulatorCleanupMs = 0.; cleanupPlanningMs = 0.; cleanupRewriteMs = 0.}
let addFunctionPhaseTimings left right = {returnAnalysisMs = left.returnAnalysisMs +. right.returnAnalysisMs; parameterAnalysisMs = left.parameterAnalysisMs +. right.parameterAnalysisMs; bodyInsertionMs = left.bodyInsertionMs +. right.bodyInsertionMs; accumulatorCleanupMs = left.accumulatorCleanupMs +. right.accumulatorCleanupMs; cleanupPlanningMs = left.cleanupPlanningMs +. right.cleanupPlanningMs; cleanupRewriteMs = left.cleanupRewriteMs +. right.cleanupRewriteMs}
let measureFunctionPhase enabled work = if enabled then let started = HostClock.milliseconds () in let result = work () in result, HostClock.milliseconds () -. started else work (), 0.
(*
   Insert RC operations into a function
   Returns (transformed function, varGen, accumulated TempTypes)
   General loop-state ownership currently targets persistent dictionary
   frontiers and their compact tuple bookkeeping. Excluding list and record
   candidates avoids adding RC traffic to ordinary traversals where eager
   replacement cleanup is not profitable.
   Process function body with return analysis
*)
let insertRCInFunctionInternal trace ctx (func : A.functionDef) gen types =
 let typesWithParams = List.fold_left (fun types param -> M.add (paramId param) param.A.typ types) types func.A.typedParams in
 let ctxWithParams = F.withTempTypes ctx typesWithParams in
 let bodyInfo, returnAnalysisMs = measureFunctionPhase trace (fun () -> R.analyzeReturns M.empty M.empty func.A.body) in
 let infos, parameterAnalysisMs = measureFunctionPhase trace (fun () -> List.mapi (fun index param ->
  let shape = S.rcShapeForType ctxWithParams param.A.typ in
  let transfers = C.functionParamReturnTransfersOwnedAccumulator ctxWithParams func.A.id index param.A.typ in
  let kind = if P.rcShapeNeedsBorrowedRetain shape then C.internalOwnedTailParamKind func index param else None in
  index, param, shape, transfers, kind) func.A.typedParams) in
 let provisional = Set.of_list (List.filter_map (fun (_, param, _, _, kind) -> Option.map (fun _ -> (paramId param)) kind) infos) in
 let hasDictionary, onlyFrontier = List.fold_left (fun (dict, supported) (_, param, _, _, kind) -> match param.A.typ, kind with AST.TDict _, Some C.NonEscapingLoopState -> true, supported | AST.TTuple _, Some C.NonEscapingLoopState -> dict, supported | _, Some C.NonEscapingLoopState -> dict, false | _ -> dict, supported) (false, true) infos in
 let rec validate ids =
  let next = Set.of_list (List.filter_map (fun (index, param, _, _, kind) -> match kind with Some C.ReturnedAccumulator -> Some (paramId param) | Some C.NonEscapingLoopState when hasDictionary && onlyFrontier && C.internalOwnedTailParamHasSafeReplacements func index ids -> Some (paramId param) | _ -> None) infos) in
  if Set.equal next ids then next else validate next in
 let validated = validate provisional in
 let infos = List.map (fun (index, param, shape, transfers, kind) -> index, param, shape, transfers, if Set.mem (paramId param) validated then kind else None) infos in
 let internalParams = List.filter_map (fun (_, param, shape, _, kind) -> Option.map (fun kind -> param, shape, kind) kind) infos in
 let internalIds = Set.of_list (List.map (fun (param, _, _) -> (paramId param)) internalParams) in
 let paramIncs = List.filter_map (fun (_, param, shape, transfers, _) -> if P.rcShapeNeedsBorrowedRetain shape && not transfers && not (Set.mem (paramId param) internalIds) then Some ((paramId param), param.A.typ, shape) else None) infos in
 let ownedDecs = List.filter_map (fun (index, param, shape, transfers, kind) -> match transfers, kind with
  | true, _ -> Some {C.paramIndex = index; releaseOnTerminalReturn = false; dec = C.createReturnDec ctxWithParams (paramId param) param.A.typ shape None}
  | false, Some kind -> Some {C.paramIndex = index; releaseOnTerminalReturn = kind = C.NonEscapingLoopState; dec = C.createReturnDec ctxWithParams (paramId param) param.A.typ shape None}
  | false, None -> None) infos in
 let (body, gen, types), bodyInsertionMs = measureFunctionPhase trace (fun () -> I.insertRCWithAnalysis M.empty [] ctxWithParams (Some func.A.id) bodyInfo gen [] [] paramIncs typesWithParams) in
 let (body, gen, types), accumulatorCleanupMs = measureFunctionPhase trace (fun () ->
  let body, gen, types = if ownedDecs = [] then body, gen, types else C.insertOwnedAccumulatorDecsBeforeSelfTailCalls ctxWithParams func.A.id ownedDecs body gen types in
  List.fold_right (fun (param, shape, _) (body, gen, types) -> let dummy, gen = A.freshVar gen in let retain = C.retainExprForShape ctxWithParams (paramId param) param.A.typ shape in A.Let (dummy, retain, body), gen, M.add dummy AST.TUnit types) internalParams (body, gen, types)) in
 let (needsMap, needsTail), cleanupPlanningMs = measureFunctionPhase trace (fun () -> C.requiredFunctionCleanups ctx func.A.id body) in
 let (body, gen, types), cleanupRewriteMs = measureFunctionPhase trace (fun () ->
  let body, gen, types = if needsMap then C.insertClosureMapSourceRetainsBeforeHelperCalls ctxWithParams func.A.id body gen types else body, gen, types in
  (if needsTail then C.moveDecsBeforeNonSelfTailCalls func.A.id body else body), gen, types) in
 {func with A.body}, gen, types, {returnAnalysisMs; parameterAnalysisMs; bodyInsertionMs; accumulatorCleanupMs; cleanupPlanningMs; cleanupRewriteMs}
(*
   Insert RC operations into a function
   Returns (transformed function, varGen, accumulated TempTypes)
*)
let insertRCInFunction ctx func gen = let func, gen, types, _ = insertRCInFunctionInternal false ctx func gen M.empty in func, gen, types
(*
   TypeMap Completeness Verification
*)
let isTempMissing typeMap id = Option.is_none (A.TypeMap.tryFind id typeMap)
let rec collectMissingTempIdsInExpr typeMap expr acc = match expr with
 | A.Jump _ | A.Return _ -> acc
 | A.Let (id, _, body) -> collectMissingTempIdsInExpr typeMap body (if isTempMissing typeMap id then id :: acc else acc)
 | A.Join (param, continuation, entry) -> let acc = if isTempMissing typeMap (paramId param) then (paramId param) :: acc else acc in collectMissingTempIdsInExpr typeMap entry (collectMissingTempIdsInExpr typeMap continuation acc)
 | A.If (_, yes, no) -> collectMissingTempIdsInExpr typeMap no (collectMissingTempIdsInExpr typeMap yes acc)
let collectMissingTempIdsInFunction typeMap (func : A.functionDef) acc =
 let acc = List.fold_left (fun acc param -> if isTempMissing typeMap (paramId param) then (paramId param) :: acc else acc) acc func.A.typedParams in collectMissingTempIdsInExpr typeMap func.A.body acc
(*
   Find the greatest TempId defined by an ANF expression. ANF variables are
   introduced only by function parameters and Let bindings, so definitions
   are sufficient to place a fresh-variable generator beyond every use.
*)
let rec maxDefinedTempIdInExpr = function
 | A.Jump _ | A.Return _ -> -1
 | A.Let (A.TempId id, _, body) -> max id (maxDefinedTempIdInExpr body)
 | A.Join (param, continuation, entry) -> let A.TempId id = (paramId param) in max id (max (maxDefinedTempIdInExpr continuation) (maxDefinedTempIdInExpr entry))
 | A.If (_, yes, no) -> max (maxDefinedTempIdInExpr yes) (maxDefinedTempIdInExpr no)
let maxDefinedTempIdInFunction (func : A.functionDef) = max (List.fold_left (fun current param -> let A.TempId id = (paramId param) in max current id) (-1) func.A.typedParams) (maxDefinedTempIdInExpr func.A.body)
let freshVarGenAfterProgram (A.Program (functions, main)) =
 let maxId = max (List.fold_left (fun current func -> max current (maxDefinedTempIdInFunction func)) (-1) functions) (maxDefinedTempIdInExpr main) in
 A.VarGen (Int32.to_int (Int32.add (Int32.of_int maxId) 1l))
(*
   Verify that all defined TempIds have types in the TypeMap
   Returns a list of TempIds that are missing from the TypeMap
*)
let verifyTypeMapCompleteness (A.Program (functions, main)) typeMap = List.rev (collectMissingTempIdsInExpr typeMap main (List.fold_left (fun acc func -> collectMissingTempIdsInFunction typeMap func acc) [] functions))
let tempText (A.TempId id) = "TempId " ^ string_of_int id
(*
   Check immediate join interfaces and lexical captures after type recovery.
   Legacy tree-only functions are outside this verifier's migration boundary.
*)
let verifyJoinInterfaces ctx (A.Program (functions, main)) =
 let rec containsJoin = function A.Join _ | A.Jump _ -> true | A.Let (_, _, body) -> containsJoin body | A.If (_, yes, no) -> containsJoin yes || containsJoin no | A.Return _ -> false in
 let atomUses = function A.Var id -> Set.singleton id | _ -> Set.empty in
 let checkUses visible uses =
  let missing = Set.diff uses visible in
  if Set.is_empty missing then Ok () else Error ("ANF join interface: operands outside lexical scope: " ^ HostStructuralFormat.format (HostStructuralFormat.Union ("set", [HostStructuralFormat.Sequence (List.map (fun (A.TempId id) -> HostStructuralFormat.Union ("TempId", [HostStructuralFormat.Scalar (string_of_int id)])) (Set.elements missing))]))) in
 let typeText = HostStructuralFormat.semanticType in
 let optionType = function None -> "None" | Some typ -> HostStructuralFormat.format (HostStructuralFormat.Union ("Some", [HostStructuralFormat.semanticValue typ])) in
 let rec check visible joins canReturn = function
  | A.Return atom -> if canReturn then checkUses visible (atomUses atom) else Error "ANF join interface: entry returns a value instead of transferring control"
  | A.Jump (target, atom) -> let* () = checkUses visible (atomUses atom) in
    (match M.find_opt target joins, F.inferAtomType ctx atom with None, _ -> Error ("ANF join interface: target " ^ tempText target ^ " is outside lexical scope") | Some expected, Some actual when expected = actual -> Ok () | Some expected, actual -> Error ("ANF join interface: target " ^ tempText target ^ " expects " ^ typeText expected ^ ", got " ^ optionType actual))
  | A.Let (id, operation, body) -> let* () = checkUses visible (ANFEffects.cexprTempUses operation) in (match operation with A.RuntimeError _ | A.RuntimeErrorString _ -> Ok () | _ -> check (Set.add id visible) joins canReturn body)
  | A.If (condition, yes, no) -> let* () = checkUses visible (atomUses condition) in let* () = check visible joins canReturn yes in check visible joins canReturn no
  | A.Join (param, continuation, entry) ->
    if not (Continuations.isSupportedJoinArgumentType param.A.typ) then Error ("ANF join interface: managed or unsupported block argument " ^ typeText param.A.typ)
    else if Set.mem (paramId param) visible || M.mem (paramId param) joins then Error ("ANF join interface: target " ^ tempText (paramId param) ^ " shadows an enclosing identity")
    else let* () = check (Set.add (paramId param) visible) joins canReturn continuation in check visible (M.add (paramId param) param.A.typ joins) false entry in
 let verify visible expr = if containsJoin expr then check visible M.empty true expr else Ok () in
 let* () = List.fold_left (fun result (func : A.functionDef) -> let* () = result in verify (Set.of_list (List.map (fun param -> (paramId param)) func.A.typedParams)) func.A.body |> Result.map_error (fun error -> func.A.name ^ ": " ^ error)) (Ok ()) functions in
 verify Set.empty main
(*
   Inlining and generated JSON helpers can produce thousands of existing
   temporaries. A fixed starting value eventually collides with them, and
   sibling-branch type state can then suppress a required retain.
   Process all functions, accumulating types
   Process main expression
   Verify TypeMap completeness - all defined TempIds should have types
*)
let insertRCInProgramInternal recorder (result : AST_to_ANF.conversionResult) =
 let start () = Option.map (fun _ -> HostClock.milliseconds ()) recorder in
 let record name timer = match recorder, timer with Some record, Some started -> record name (HostClock.milliseconds () -. started) | _ -> () in
 let timer = start () in let ctx = F.createContext result in record "Reference Count Context" timer;
 let A.Program (functions, main) = result.AST_to_ANF.program in
 let ownershipVerification = verifyOwnershipContracts ctx result.AST_to_ANF.ownershipContracts result.AST_to_ANF.program in
 let gen = freshVarGenAfterProgram result.AST_to_ANF.program in
 let rec process funcs gen acc types timings = match funcs with
  | [] -> List.rev acc, gen, types, timings
  | func :: rest -> let func, gen, localTypes, localTimings = insertRCInFunctionInternal (Option.is_some recorder) ctx func gen M.empty in
    let types = M.fold (fun id typ acc -> M.add id typ acc) localTypes types in process rest gen (func :: acc) types (addFunctionPhaseTimings timings localTimings) in
 let timer = start () in
 let functions, gen, types, timings = process functions gen [] M.empty emptyFunctionPhaseTimings in
 Option.iter (fun record -> record "Reference Count Return Analysis" timings.returnAnalysisMs; record "Reference Count Parameter Analysis" timings.parameterAnalysisMs; record "Reference Count Body Insertion" timings.bodyInsertionMs; record "Reference Count Accumulator Cleanup" timings.accumulatorCleanupMs; record "Reference Count Cleanup Planning" timings.cleanupPlanningMs; record "Reference Count Cleanup Rewrite" timings.cleanupRewriteMs) recorder;
 record "Reference Count Functions" timer;
 let timer = start () in let main, _, types = I.insertRCInternal ctx main gen types in record "Reference Count Main" timer;
 let timer = start () in let program = A.Program (functions, main) in let frozen = A.TypeMap.ofSeq (M.to_seq types) in
 let missing = verifyTypeMapCompleteness program frozen in record "Reference Count Verification" timer;
 if missing <> [] then Crash.crash ("RefCountInsertion: TypeMap incomplete - missing types for: " ^ String.concat ", " (List.map (fun (A.TempId id) -> "t" ^ string_of_int id) missing));
 let* () = ownershipVerification in let* () = verifyJoinInterfaces (F.withTempTypes ctx types) program in Ok (program, frozen)
(*
   Insert RC operations into a program
   Returns (ANF.Program, TypeMap) where TypeMap contains all TempId -> Type mappings
*)
let insertRCInProgram result = insertRCInProgramInternal None result
(*
   Insert RC operations while reporting nested phase timings.
*)
let insertRCInProgramWithTrace recorder result = insertRCInProgramInternal recorder result
