(* SSADirectCallSpecialization.fs - Specialize direct calls on typed SSA blocks. *)
[@@@warning "-4"]
module A = ANF
module S = SSAANF
module F = DirectCallFacts
module M = F.TempMap
module Set = ANFEffects.TempSet
module Labels = Stdlib.Set.Make (struct type t = S.label let compare (S.Label left) (S.Label right) = Int.compare left right end)
module Patterns = Map.Make (struct type t = F.literalPattern let compare = F.compareLiteralPattern end)
type specialization = {functions : S.functionDef list; cloneOrigins : AST.functionId FunctionIdMap.t}
let addInt left right = Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))
let rec take count values = if count <= 0 then [] else match values with [] -> [] | head :: rest -> head :: take (count - 1) rest
let tryItem index values = if index < 0 then None else List.nth_opt values index
let operations (func : S.functionDef) = S.LabelMap.bindings func.S.blocks |> List.concat_map (fun (_, block) -> block.S.operations)
let mapOperations transform (func : S.functionDef) = {func with S.blocks = S.LabelMap.map (fun block -> {block with S.operations = List.map transform block.S.operations}) func.S.blocks}
let exposeKnownIndirectTargets func = mapOperations (fun (id, operation) -> id, F.exposeKnownIndirectCExpr operation) func
let analyzeTerminator terminator analysis = match terminator with S.Return atom -> F.analyzeAtom atom analysis | S.Jump (_, args) -> List.fold_left (fun state atom -> F.analyzeAtom atom state) analysis args | S.Branch (condition, _, _) -> F.analyzeAtom condition analysis
let analyzeProgram functions = List.fold_left (fun analysis (func : S.functionDef) -> S.LabelMap.fold (fun _ block state -> analyzeTerminator block.S.terminator (List.fold_left (fun state (_, operation) -> F.analyzeCExpr operation state) state block.S.operations)) func.S.blocks analysis) F.emptyAnalysis functions
let uniformLiteralAt index calls = match List.map (fun args -> Option.bind (tryItem index args) F.scalarLiteralAtom) calls with Some first :: rest when List.for_all (fun value -> value = Some first) rest -> Some (F.atomForScalarLiteral first) | _ -> None
let buildRewriteMap analysis functions = FunctionIdMap.ofList (List.filter_map (fun (func : S.functionDef) -> match FunctionIdMap.tryFind func.S.id analysis.F.directCalls with
 | None -> None
 | Some _ when F.FunctionSet.mem func.S.id analysis.F.indirectTargets -> None
 | Some calls -> let rewrites = List.mapi (fun index (param : A.typedParam) -> match uniformLiteralAt index calls with Some literal when F.isScalarLiteralType param.A.typ -> (match F.scalarLiteralAtom literal with Some value when F.scalarLiteralMatchesType param.A.typ value -> F.ReplaceParameterWith literal | _ -> F.KeepParameter) | _ -> F.KeepParameter) func.S.typedParams in if List.for_all ((=) F.KeepParameter) rewrites then None else Some (func.S.id, rewrites)) functions)
let rewriteTerminator substitutions = function S.Return atom -> S.Return (F.rewriteAtom substitutions atom) | S.Jump (target, args) -> S.Jump (target, List.map (F.rewriteAtom substitutions) args) | S.Branch (condition, yes, no) -> S.Branch (F.rewriteAtom substitutions condition, yes, no)
let rewriteBody rewrites substitutions (func : S.functionDef) = {func with S.blocks = S.LabelMap.map (fun block -> {block with S.operations = List.map (fun (id, operation) -> id, F.rewriteCExpr rewrites substitutions operation) block.S.operations; terminator = rewriteTerminator substitutions block.S.terminator}) func.S.blocks}
let rewriteFunction rewrites (func : S.functionDef) = match FunctionIdMap.tryFind func.S.id rewrites with
 | None -> rewriteBody rewrites M.empty func
 | Some modes ->
   let rec pair reversed params modes = match params, modes with [], [] -> List.rev reversed | param :: ps, mode :: ms -> pair ((param, mode) :: reversed) ps ms | _ -> Crash.crash "SSA direct-call parameter rewrite count mismatch" in
   let pairs = pair [] func.S.typedParams modes in
   let params = List.filter_map (fun (param, mode) -> match mode with F.KeepParameter -> Some param | F.ReplaceParameterWith _ -> None) pairs in
   let substitutions = M.of_list (List.filter_map (fun ((param : A.typedParam), mode) -> match mode with F.ReplaceParameterWith literal -> Some (param.A.id, literal) | F.KeepParameter -> None) pairs) in { (rewriteBody rewrites substitutions func) with S.typedParams = params}
(*
   Definitions dominate their uses, so a CFG walk from entry sees every known
   construction before a valid use, including joins whose labels sort earlier.
*)
let knownValues names (func : S.functionDef) =
 let rec walk visited known label = if Labels.mem label visited then visited, known else match S.LabelMap.find_opt label func.S.blocks with
  | None -> Crash.crash "SSA direct-call facts: missing successor block"
  | Some block -> let visited = Labels.add label visited in
    let known = List.fold_left (fun facts (id, operation) -> match F.knownValueForCExpr names facts operation with Some value -> M.add id value facts | None -> facts) known block.S.operations in
    let next = match block.S.terminator with S.Return _ -> [] | S.Jump (target, _) -> [target] | S.Branch (_, yes, no) -> [yes; no] in List.fold_left (fun (seen, facts) target -> walk seen facts target) (visited, known) next in snd (walk Labels.empty M.empty func.S.entry)
let knownCallsInProgram names functions = List.fold_left (fun calls func -> let known = knownValues names func in List.fold_left (fun calls (_, operation) -> match operation with A.Call (name, args) | A.BorrowedCall (name, args) | A.TailCall (name, args) -> F.addKnownCall name args known calls | _ -> calls) calls (operations func)) FunctionIdMap.empty functions
let directCallsTo target func = List.filter_map (fun (_, operation) -> match operation with A.Call (name, args) | A.BorrowedCall (name, args) | A.TailCall (name, args) when name = target -> Some args | _ -> None) (operations func)
let cloneableParameterIndices (func : S.functionDef) =
 let selfCalls = directCallsTo func.S.id func in F.IntSet.of_list (List.filter_map (fun (index, (param : A.typedParam)) -> if F.isSpecializableValueType param.A.typ && List.for_all (fun args -> tryItem index args = Some (A.Var param.A.id)) selfCalls then Some index else None) (List.mapi (fun index param -> index, param) func.S.typedParams))
let cloneGroups analysis calls functions =
 List.filter_map (fun (func : S.functionDef) -> match FunctionIdMap.tryFind func.S.id calls with
  | None -> None
  | Some _ when F.FunctionSet.mem func.S.id analysis.F.indirectTargets -> None
  | Some calls ->
    let eligible = cloneableParameterIndices func in let recursive = directCallsTo func.S.id func <> [] in
    let benefit = function F.LiteralValue _ -> 1 | F.Int128Value _ | F.UInt128Value _ -> 2 | F.TupleValue fields | F.RecordValue (_, fields) -> addInt 1 (List.length fields) in
    let patterns = List.map (fun values -> let pattern = List.filter (fun (index, value) -> Option.fold ~none:false ~some:(fun (param : A.typedParam) -> F.knownValueMatchesType param.A.typ value) (tryItem index func.S.typedParams)) (F.literalPatternAt eligible values) in if recursive then List.filter (fun (_, value) -> match value with F.LiteralValue _ -> true | _ -> false) pattern else pattern) calls |> List.filter (fun pattern -> pattern <> []) in
    let counts = List.fold_left (fun counts pattern -> Patterns.add pattern (addInt 1 (Option.value ~default:0 (Patterns.find_opt pattern counts))) counts) Patterns.empty patterns in
    let score (pattern, occurrences) = Int32.to_int (Int32.neg (Int32.mul (Int32.of_int (List.fold_left (fun saved (_, value) -> addInt saved (benefit value)) 0 pattern)) (Int32.of_int occurrences))) in
    let patterns = Patterns.bindings counts |> List.sort (fun left right -> let order = Int.compare (score left) (score right) in if order = 0 then F.compareLiteralPattern (fst left) (fst right) else order) |> List.map fst |> take F.maxLiteralClonesPerFunction in
    if List.length patterns < 2 then None else Some (func.S.id, func.S.name, patterns)) functions |> List.sort (fun (left, _, _) (right, _, _) -> Int64.unsigned_compare (AST.functionIdValue left) (AST.functionIdValue right))
let routeFunction names clones func = let known = knownValues names func in mapOperations (fun (id, operation) -> id, F.routeCExpr clones known operation) func
let atomUse used = function A.Var id -> Set.add id used | _ -> used
let terminatorUses used = function S.Return atom -> atomUse used atom | S.Jump (_, args) -> List.fold_left atomUse used args | S.Branch (condition, _, _) -> atomUse used condition
let removeUnusedRematerializedValues names (func : S.functionDef) =
 let definitions = M.of_list (operations func) in
 let removable = M.filter (fun _ operation -> F.isRematerializedValue names operation || (match operation with A.ClosureAlloc _ | A.Atom _ | A.TypedAtom _ | A.IfValue _ -> true | _ -> false)) definitions in
 let addUse id counts = M.add id (addInt 1 (Option.value ~default:0 (M.find_opt id counts))) counts in
 let counts = S.LabelMap.fold (fun _ block counts -> let counts = List.fold_left (fun counts (_, operation) -> Set.fold addUse (ANFEffects.cexprTempUses operation) counts) counts block.S.operations in Set.fold addUse (terminatorUses Set.empty block.S.terminator) counts) func.S.blocks M.empty in
 let initial = Set.of_list (M.bindings removable |> List.filter_map (fun (id, _) -> if Option.value ~default:0 (M.find_opt id counts) = 0 then Some id else None)) in
 let rec eliminate counts removed pending = match Set.min_elt_opt pending with
  | None -> removed
  | Some id -> let pending = Set.remove id pending in
    if Set.mem id removed || Option.value ~default:0 (M.find_opt id counts) <> 0 then eliminate counts removed pending else
    let operation = match M.find_opt id removable with Some value -> value | None -> Crash.crash "SSA direct-call cleanup lost a removable definition" in
    let counts, pending = Set.fold (fun used (counts, pending) -> let remaining = addInt (Option.value ~default:0 (M.find_opt used counts)) (-1) in M.add used remaining counts, if remaining = 0 && M.mem used removable then Set.add used pending else pending) (ANFEffects.cexprTempUses operation) (counts, pending) in eliminate counts (Set.add id removed) pending in
 let removed = eliminate counts Set.empty initial in {func with S.blocks = S.LabelMap.map (fun block -> {block with S.operations = List.filter (fun (id, _) -> not (Set.mem id removed)) block.S.operations}) func.S.blocks}
let cloneFunction names clones originals (clone : F.literalClone) =
 let original = match FunctionIdMap.tryFind clone.F.originalId originals with Some func -> func | None -> Crash.crash ("Missing SSA direct-call clone source '" ^ StructuralFormat.format (AST.DiagnosticFormatting.func clone.F.originalId) ^ "'") in
 let values = ANFConstants.IntMap.of_list clone.F.pattern in
 let parameters = List.filter_map (fun (index, param) -> if ANFConstants.IntMap.mem index values then None else Some param) (List.mapi (fun index param -> index, param) original.S.typedParams) in
 let substitutions = M.of_list (List.filter_map (fun (index, (param : A.typedParam)) -> match ANFConstants.IntMap.find_opt index values with Some (F.LiteralValue literal) -> Some (param.A.id, F.atomForScalarLiteral literal) | _ -> None) (List.mapi (fun index param -> index, param) original.S.typedParams)) in
 let materializations = List.filter_map (fun (index, (param : A.typedParam)) -> match ANFConstants.IntMap.find_opt index values with Some (F.LiteralValue _) | None -> None | Some value -> Some (param.A.id, F.cexprForKnownValue value)) (List.mapi (fun index param -> index, param) original.S.typedParams) in
 let rewritten = rewriteBody FunctionIdMap.empty substitutions original in
 let blocks = S.LabelMap.update rewritten.S.entry (function Some block -> Some {block with S.operations = materializations @ block.S.operations} | None -> Crash.crash "SSA direct-call clone has no entry block") rewritten.S.blocks in
 routeFunction names clones {rewritten with S.id = clone.F.cloneId; name = clone.F.cloneName; typedParams = parameters; blocks}
let specializeProgramWithFunctionNames names functions =
 let names = List.fold_left (fun names (func : S.functionDef) -> FunctionIdMap.add func.S.id func.S.name names) names functions in
 let exposed = List.map exposeKnownIndirectTargets functions in
 let analysis = analyzeProgram exposed in let rewrites = buildRewriteMap analysis exposed in
 let rewritten = List.map (rewriteFunction rewrites) exposed in
 let analysis = analyzeProgram rewritten in let calls = knownCallsInProgram names rewritten in
 let localIds = F.FunctionSet.of_list (List.map (fun (func : S.functionDef) -> func.S.id) rewritten) in
 let candidates = F.boundedCloneGroups (cloneGroups analysis calls rewritten) in
 let clones = if candidates = [] then [] else F.buildLiteralClones (List.to_seq (F.FunctionSet.elements localIds @ List.of_seq (FunctionIdMap.keys names))) (StringOrder.Set.of_seq (FunctionIdMap.values names)) candidates in
 let clonesByName = List.fold_left (fun groups (clone : F.literalClone) -> FunctionIdMap.add clone.F.originalId (Option.value ~default:[] (FunctionIdMap.tryFind clone.F.originalId groups) @ [clone]) groups) FunctionIdMap.empty clones in
 let originals = FunctionIdMap.ofList (List.map (fun (func : S.functionDef) -> func.S.id, func) rewritten) in
 let cloned = List.map (cloneFunction names clonesByName originals) clones in
 let routed = List.map (routeFunction names clonesByName) rewritten in
 {functions = List.map (removeUnusedRematerializedValues names) (cloned @ routed); cloneOrigins = FunctionIdMap.ofList (List.map (fun (clone : F.literalClone) -> clone.F.cloneId, clone.F.originalId) clones)}
let reachableFrom roots functions =
 let byId = FunctionIdMap.ofList (List.map (fun (func : S.functionDef) -> func.S.id, func) functions) in
 let rec visit seen = function [] -> seen | id :: rest when F.FunctionSet.mem id seen -> visit seen rest | id :: rest -> let successors = match FunctionIdMap.tryFind id byId with None -> [] | Some func -> let analysis = analyzeProgram [func] in List.of_seq (FunctionIdMap.keys analysis.F.directCalls) @ F.FunctionSet.elements analysis.F.indirectTargets in visit (F.FunctionSet.add id seen) (successors @ rest) in
 let reachable = visit F.FunctionSet.empty (F.FunctionSet.elements roots) in List.filter (fun (func : S.functionDef) -> F.FunctionSet.mem func.S.id reachable) functions
