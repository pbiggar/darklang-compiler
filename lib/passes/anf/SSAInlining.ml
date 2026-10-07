(*
   Each copied value receives a fresh identity. Single-block callees stay in
   the caller's block so ownership cleanup can follow the remaining effects.
   Multi-block callees use a typed continuation. Small scalar Option projections
   may copy that continuation at each return to expose branch-local values.
*)
(* SSAInlining.fs - Inline eligible typed SSA functions at direct call sites. *)
[@@@warning "-4"]
module A = ANF
module S = SSAANF
module I = InliningCommon
module M = I.TempMap
module Set = ANFEffects.TempSet
module Labels = Stdlib.Set.Make (struct type t = S.label let compare (S.Label left) (S.Label right) = Int.compare left right end)
module IndexMap = ANFConstants.IntMap
type candidate = {info : I.functionInfo; body : S.functionDef}
type state = {func : S.functionDef; nextValue : int; nextLabel : int; processed : Set.t; depths : int M.t}
let addInt left right = Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))
let mulInt left right = Int32.to_int (Int32.mul (Int32.of_int left) (Int32.of_int right))
let exists predicate = function Some value -> predicate value | None -> false
let nextValue state typ = let id = A.TempId state.nextValue in id, {state with nextValue = addInt state.nextValue 1; func = {state.func with S.freshValueTypes = M.add id typ state.func.S.freshValueTypes}}
let nextLabel state = S.Label state.nextLabel, {state with nextLabel = addInt state.nextLabel 1}
let valueType (func : S.functionDef) id = match M.find_opt id func.S.freshValueTypes with Some typ -> typ | None -> let A.TempId index = id in Crash.crash ("SSA inlining lost the type of TempId " ^ string_of_int index ^ " in '" ^ func.S.name ^ "'")
let requiredTemp id mapping = match M.find_opt id mapping with Some value -> value | None -> Crash.crash "SSA inlining lost a verified mapping"
let requiredLabel id mapping = match S.LabelMap.find_opt id mapping with Some value -> value | None -> Crash.crash "SSA inlining lost a verified mapping"
let requiredIndex id mapping = match IndexMap.find_opt id mapping with Some value -> value | None -> Crash.crash "SSA inlining lost a verified mapping"
let renameAtom = I.renameAtom
let renameTerminator labels mapping continuation = function
 | S.Return atom -> S.Jump (continuation, [renameAtom mapping atom])
 | S.Jump (target, args) -> let target = match S.LabelMap.find_opt target labels with Some label -> label | None -> Crash.crash "SSA inlining lost a callee jump target" in S.Jump (target, List.map (renameAtom mapping) args)
 | S.Branch (condition, yes, no) -> let label old = match S.LabelMap.find_opt old labels with Some value -> value | None -> Crash.crash "SSA inlining lost a callee branch target" in S.Branch (renameAtom mapping condition, label yes, label no)
let initialState (func : S.functionDef) = {func; nextValue = addInt (M.fold (fun (A.TempId id) _ largest -> max largest id) func.S.freshValueTypes 4000) 1; nextLabel = addInt (S.LabelMap.fold (fun (S.Label id) _ largest -> max largest id) func.S.blocks 0) 1; processed = Set.empty; depths = M.empty}
let splitAtOperation index operations =
 let before, at, after = List.fold_left (fun (before, at, after) (current, operation) -> if current < index then operation :: before, at, after else if current = index then before, Some operation, after else before, at, operation :: after) ([], None, []) (List.mapi (fun index operation -> index, operation) operations) in List.rev before, at, List.rev after
type boundedLoop = {parameters : A.tempId list; inductionIndex : int; bound : int64; exit : A.atom; iteration : (A.tempId * A.cExpr) list; recursiveArguments : A.atom list}
let boundedPrimitive = function A.Atom _ | A.TypedAtom (_, AST.TInt64) | A.Prim _ | A.UnaryPrim _ -> true | _ -> false
let tryBoundedLoop candidate =
 let source = candidate.info.I.func in
 let rec iteration reversed = function A.Let (id, A.Call (name, args), A.Return (A.Var returned)) when name = source.A.id && id = returned -> Some (List.rev reversed, args) | A.Let (id, operation, rest) when boundedPrimitive operation -> iteration ((id, operation) :: reversed) rest | _ -> None in
 match List.for_all (fun (param : A.typedParam) -> param.A.typ = AST.TInt64) source.A.typedParams, source.A.returnType, source.A.body with
 | true, AST.TInt64, A.Let (guard, A.Prim (A.Gte, A.Var induction, A.IntLiteral (A.Int64 bound)), A.If (A.Var condition, A.Return exit, body)) when guard = condition ->
   let parameters = List.map (fun (param : A.typedParam) -> param.A.id) source.A.typedParams in
   let index = List.find_index ((=) induction) parameters in
   (match index, iteration [] body with Some index, Some (bindings, args) when List.length args = List.length parameters && (match exit with A.Var id -> List.mem id parameters | A.IntLiteral _ -> true | _ -> false) ->
    let advances = match List.nth args index with A.Var next -> List.exists (fun (id, operation) -> id = next && operation = A.Prim (A.Add, A.Var induction, A.IntLiteral (A.Int64 1L))) bindings | _ -> false in if advances then Some {parameters; inductionIndex = index; bound; exit; iteration = bindings; recursiveArguments = args} else None | _ -> None)
 | _ -> None
let boundedTripCount maximum start bound = let rec count current completed = if current >= bound then Some completed else if completed = maximum || current = Int64.max_int then None else count (Int64.add current 1L) (addInt completed 1) in count start 0
let substituteAtom mapping = function A.Var id as atom -> Option.value ~default:atom (M.find_opt id mapping) | atom -> atom
let substituteBounded mapping = function A.Atom atom -> A.Atom (substituteAtom mapping atom) | A.TypedAtom (atom, typ) -> A.TypedAtom (substituteAtom mapping atom, typ) | A.Prim (op, left, right) -> A.Prim (op, substituteAtom mapping left, substituteAtom mapping right) | A.UnaryPrim (op, atom) -> A.UnaryPrim (op, substituteAtom mapping atom) | _ -> Crash.crash "SSA inlining encountered an unsupported bounded-loop operation"
let renameCallerTerminator labels mapping = function
 | S.Return atom -> S.Return (renameAtom mapping atom)
 | S.Jump (label, args) -> S.Jump (Option.value ~default:label (S.LabelMap.find_opt label labels), List.map (renameAtom mapping) args)
 | S.Branch (condition, yes, no) -> S.Branch (renameAtom mapping condition, Option.value ~default:yes (S.LabelMap.find_opt yes labels), Option.value ~default:no (S.LabelMap.find_opt no labels))
let terminatorUses id = function S.Return atom -> ANFEffects.atomsUseTemp id [atom] | S.Jump (_, args) -> ANFEffects.atomsUseTemp id args | S.Branch (condition, _, _) -> ANFEffects.atomsUseTemp id [condition]
let successors = function S.Return _ -> [] | S.Jump (label, _) -> [label] | S.Branch (_, yes, no) -> [yes; no]
let reachable (func : S.functionDef) excluded start =
 let rec visit seen = function [] -> seen | label :: rest when Labels.mem label seen || Some label = excluded -> visit seen rest | label :: rest -> match S.LabelMap.find_opt label func.S.blocks with Some block -> visit (Labels.add label seen) (successors block.S.terminator @ rest) | None -> Crash.crash "SSA inlining found a missing caller block" in visit Labels.empty [start]
let continuationRegion (func : S.functionDef) (block : S.block) result after returns =
 let region = Labels.remove block.S.label (Labels.diff (reachable func None block.S.label) (reachable func (Some block.S.label) func.S.entry)) in
 let blocks = List.map (fun label -> requiredLabel label func.S.blocks) (Labels.elements region) in
 let definitions = Set.of_list (result :: List.map fst after @ List.concat_map (fun body -> List.map (fun (param : A.typedParam) -> param.A.id) body.S.parameters @ List.map fst body.S.operations) blocks) in
 let outside = S.LabelMap.exists (fun label body -> label <> block.S.label && not (Labels.mem label region) && (List.exists (fun (_, operation) -> Set.exists (fun id -> ANFEffects.cexprUsesTemp id operation) definitions) body.S.operations || Set.exists (fun id -> terminatorUses id body.S.terminator) definitions)) func.S.blocks in
 let size = addInt (addInt 1 (List.length after)) (List.fold_left (fun total body -> addInt total (addInt 1 (List.length body.S.operations))) 0 blocks) in if returns > 1 && mulInt (addInt returns (-1)) size <= 1024 && not outside then Some blocks else None
let canShareLargeContinuation = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TBool | AST.TDateTime | AST.TUnit | AST.TInternalRawPtr -> true | _ -> false
type projectionPlan = {tupleId : A.tempId; fields : A.atom list; parameters : A.typedParam list; remaining : (A.tempId * A.cExpr) list}
let scalarType = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TBool | AST.TFloat64 | AST.TDateTime | AST.TUnit | AST.TNever | AST.TInternalRawPtr -> true | _ -> false
let tryLast values = match List.rev values with head :: _ -> Some head | [] -> None
let eligibleProjectedResult candidate (config : I.inliningConfig) =
 let returned = S.LabelMap.bindings candidate.body.S.blocks |> List.filter_map (fun (_, block) -> match tryLast block.S.operations, block.S.terminator with Some (tuple, A.TupleAlloc fields), S.Return (A.Var id) when tuple = id -> Some fields | _, S.Return _ -> Some [] | _ -> None) in
 let bindings = S.LabelMap.fold (fun _ block known -> List.fold_left (fun values (id, operation) -> M.add id operation values) known block.S.operations) candidate.body.S.blocks M.empty in
 let info = candidate.info in match candidate.body.S.returnType, returned with
 | AST.TTuple elements, [fields] when fields <> [] ->
   let ids = List.filter_map (function A.Var id -> Some id | _ -> None) fields in
   let freshRecords = List.length ids = List.length fields && Set.cardinal (Set.of_list ids) = List.length ids && List.for_all (fun id -> match M.find_opt id bindings with Some (A.RecordAlloc (descriptor, _)) | Some (A.RecordClone (descriptor, _, _)) -> List.for_all (fun (_, typ) -> scalarType typ) descriptor.A.fields | _ -> false) ids in
   info.I.size <= config.I.maxProjectedTupleInlineSize && not info.I.isRecursive && not info.I.hasClosures && not info.I.hasTailCalls && (List.for_all scalarType elements || freshRecords)
 | _ -> false
let tryProjectedPlan (config : I.inliningConfig) (func : S.functionDef) (block : S.block) index result siteCount candidate =
 if candidate.info.I.isExternal || siteCount > config.I.maxProjectedTupleInlineSites || not (eligibleProjectedResult candidate config) then None else
 let _, _, after = splitAtOperation index block.S.operations in
 let returns = S.LabelMap.bindings candidate.body.S.blocks |> List.filter_map (fun (_, body) -> match tryLast body.S.operations, body.S.terminator with Some (tuple, A.TupleAlloc fields), S.Return (A.Var returned) when tuple = returned -> Some (tuple, fields) | _, S.Return _ -> Some (A.TempId (-1), []) | _ -> None) in
 let outside = S.LabelMap.exists (fun label body -> label <> block.S.label && (List.exists (fun (_, operation) -> ANFEffects.cexprUsesTemp result operation) body.S.operations || terminatorUses result body.S.terminator)) func.S.blocks in
 match returns, candidate.body.S.returnType with
 | [(tupleId, fields)], AST.TTuple types when tupleId <> A.TempId (-1) && List.length fields = List.length types && not outside ->
   let rec projections found aliases kept = function
    | (id, A.TupleGet (A.Var source, index)) :: tail when source = result -> projections ((index, id) :: found) (Set.add id aliases) kept tail
    | (id, (A.TypedAtom (A.Var source, _) as operation)) :: tail when Set.mem source aliases -> projections found (Set.add id aliases) ((id, operation) :: kept) tail
    | rest -> List.rev found, List.rev kept @ rest in
   let found, remaining = projections [] Set.empty [] after in
   let indexes = List.sort Int.compare (List.map fst found) in
   let expected = List.init (List.length types) Fun.id in
   let used = List.exists (fun (_, operation) -> ANFEffects.cexprUsesTemp result operation) remaining || terminatorUses result block.S.terminator in
   if indexes <> expected || used then None else let ids = IndexMap.of_list found in let parameters = List.mapi (fun index typ -> {A.id = requiredIndex index ids; typ}) types in Some {tupleId; fields; parameters; remaining}
 | _ -> None
let tryExpandBoundedCall (config : I.inliningConfig) state (block : S.block) index result args candidate = match tryBoundedLoop candidate with
 | Some loop when List.length args = List.length loop.parameters -> (match List.nth args loop.inductionIndex with
   | A.IntLiteral (A.Int64 start) -> (match boundedTripCount config.I.maxBoundedLoopIterations start loop.bound with
     | Some rounds when mulInt rounds (List.length loop.iteration) <= config.I.maxBoundedLoopExpansion && List.for_all (fun (id, _) -> M.mem id candidate.body.S.freshValueTypes) loop.iteration ->
       let rec expand remaining args reversed state =
        let values = M.of_list (List.combine loop.parameters args) in
        if remaining = 0 then List.rev reversed, substituteAtom values loop.exit, state else
        let cloned, mapping, state = List.fold_left (fun (cloned, mapping, state) (old, operation) -> let fresh, state = nextValue state (valueType candidate.body old) in let renamed = substituteBounded mapping operation in (fresh, renamed) :: cloned, M.add old (A.Var fresh) mapping, state) ([], values, state) loop.iteration in
        let args = List.map (substituteAtom mapping) loop.recursiveArguments in expand (remaining - 1) args (cloned @ reversed) state in
       let expanded, returned, state = expand rounds args [] state in
       let before, _, after = splitAtOperation index block.S.operations in let replacement = {block with S.operations = before @ expanded @ [result, A.Atom returned] @ after} in
       Some {state with func = {state.func with S.blocks = S.LabelMap.add block.S.label replacement state.func.S.blocks}}
     | _ -> None)
   | _ -> None)
 | _ -> None
let bindParameters parameters args state = List.fold_left2 (fun (mapping, bindings, state) (param : A.typedParam) argument -> match argument with A.Var id -> M.add param.A.id id mapping, bindings, state | atom -> let id, state = nextValue state param.A.typ in M.add param.A.id id mapping, bindings @ [id, A.Atom atom], state) (M.empty, [], state) parameters args
(*
   SSA labels are assigned before nested branches are traversed;
   allocate continuation copies in return-substitution order.
*)
let cloneAt state (block : S.block) index result args candidate depth projection =
 let before, _, after = splitAtOperation index block.S.operations in
 let after = Option.fold ~none:after ~some:(fun plan -> plan.remaining) projection in
 let parameterMapping, bindings, state = bindParameters candidate.body.S.typedParams args state in
 let labels, state = S.LabelMap.fold (fun old _ (labels, state) -> let fresh, state = nextLabel state in S.LabelMap.add old fresh labels, state) candidate.body.S.blocks (S.LabelMap.empty, state) in
 let definitions = S.LabelMap.bindings candidate.body.S.blocks |> List.concat_map (fun (_, body) -> List.map (fun (param : A.typedParam) -> param.A.id) body.S.parameters @ List.map fst body.S.operations) in
 let returnedValues = Set.of_list (S.LabelMap.bindings candidate.body.S.blocks |> List.filter_map (fun (_, body) -> match body.S.terminator with S.Return (A.Var id) -> Some id | _ -> None)) in
 let mapping, state = List.fold_left (fun (mapping, state) old -> let typ = if Set.mem old returnedValues then candidate.body.S.returnType else valueType candidate.body old in let id, state = nextValue state typ in M.add old id mapping, state) (parameterMapping, state) definitions in
 let continuation, state = nextLabel state in
 let remapParameter (param : A.typedParam) = match M.find_opt param.A.id mapping with Some id -> {param with A.id} | None -> Crash.crash "SSA inlining lost a block parameter" in
 let clonedBlocks = S.LabelMap.bindings candidate.body.S.blocks |> List.map (fun (old, body) ->
  let label = match S.LabelMap.find_opt old labels with Some label -> label | None -> Crash.crash "SSA inlining lost a callee block" in
  let operations = List.filter_map (fun (old, operation) -> if exists (fun plan -> old = plan.tupleId) projection then None else let id = match M.find_opt old mapping with Some id -> id | None -> Crash.crash "SSA inlining lost a callee value" in Some (id, I.renameCExpr mapping operation)) body.S.operations in
  let terminator = match projection, body.S.terminator with Some plan, S.Return (A.Var returned) when returned = plan.tupleId -> S.Jump (continuation, List.map (renameAtom mapping) plan.fields) | _ -> renameTerminator labels mapping continuation body.S.terminator in
  label, {S.label; parameters = List.map remapParameter body.S.parameters; operations; terminator}) in
 let entry = match S.LabelMap.find_opt candidate.body.S.entry labels with Some label -> label | None -> Crash.crash "SSA inlining lost a callee entry" in
 let source = {block with S.operations = before @ bindings; terminator = S.Jump (entry, [])} in
 let continuationBlock : S.block = {S.label = continuation; parameters = Option.fold ~none:[{A.id = result; typ = candidate.body.S.returnType}] ~some:(fun (plan : projectionPlan) -> plan.parameters) projection; operations = after; terminator = block.S.terminator} in
 let blocks = List.fold_left (fun blocks (label, cloned) -> S.LabelMap.add label cloned blocks) (S.LabelMap.add continuation continuationBlock (S.LabelMap.add block.S.label source state.func.S.blocks)) clonedBlocks in
 let depths = List.fold_left (fun depths (_, cloned) -> List.fold_left (fun depths (id, _) -> M.add id (addInt depth 1) depths) depths cloned.S.operations) state.depths clonedBlocks in
 let resultState = {state with func = {state.func with S.blocks; freshValueTypes = M.add result candidate.body.S.returnType state.func.S.freshValueTypes}; depths; processed = Set.add result state.processed} in
 let returns = List.filter_map (fun (label, cloned) -> match cloned.S.terminator with S.Jump (target, [_]) when target = continuation -> Some label | _ -> None) clonedBlocks in
 match projection, continuationRegion state.func block result after (List.length returns) with
 | None, Some region ->
   let copied = List.fold_left (fun state returnLabel ->
    let returnBlock = requiredLabel returnLabel state.func.S.blocks in
    let returned = match returnBlock.S.terminator with S.Jump (target, [atom]) when target = continuation -> atom | _ -> Crash.crash "SSA inlining lost a copied return" in
    let alias, state = nextValue state candidate.body.S.returnType in
    let labels, state = List.fold_left (fun (labels, state) original -> let fresh, state = nextLabel state in S.LabelMap.add original.S.label fresh labels, state) (S.LabelMap.empty, state) region in
    let originals = List.map fst after @ List.concat_map (fun body -> List.map (fun (param : A.typedParam) -> param.A.id) body.S.parameters @ List.map fst body.S.operations) region in
    let mapping, state = List.fold_left (fun (mapping, state) old -> let fresh, state = nextValue state (valueType state.func old) in M.add old fresh mapping, {state with depths = M.add fresh (Option.value ~default:0 (M.find_opt old resultState.depths)) state.depths}) (M.singleton result alias, state) originals in
    let renameOperations operations = List.map (fun (old, operation) -> requiredTemp old mapping, I.renameCExpr mapping operation) operations in
    let regionCopies = List.map (fun original -> let label = requiredLabel original.S.label labels in label, {S.label; parameters = List.map (fun (param : A.typedParam) -> {param with A.id = requiredTemp param.A.id mapping}) original.S.parameters; operations = renameOperations original.S.operations; terminator = renameCallerTerminator labels mapping original.S.terminator}) region in
    let returningBlock = {returnBlock with S.operations = returnBlock.S.operations @ [alias, A.Atom returned] @ renameOperations after; terminator = renameCallerTerminator labels mapping block.S.terminator} in
    let blocks = List.fold_left (fun blocks (label, copied) -> S.LabelMap.add label copied blocks) (S.LabelMap.add returnLabel returningBlock state.func.S.blocks) regionCopies in
    let processed = M.fold (fun old fresh processed -> if Set.mem old resultState.processed then Set.add fresh processed else processed) mapping state.processed in {state with processed; func = {state.func with S.blocks}}) resultState (List.rev returns) in
   let blocks = List.fold_left (fun blocks body -> S.LabelMap.remove body.S.label blocks) (S.LabelMap.remove continuation copied.func.S.blocks) region in {copied with func = {copied.func with S.blocks}}
 | _ -> resultState
let cloneLinearAt state (block : S.block) index result args candidate depth projection = match S.LabelMap.bindings candidate.body.S.blocks with
 | [(label, body)] when label = candidate.body.S.entry && body.S.parameters = [] -> (match body.S.terminator with
   | S.Return returned ->
     let before, _, after = splitAtOperation index block.S.operations in
     let after = Option.fold ~none:after ~some:(fun plan -> plan.remaining) projection in
     let mapping, bindings, state = bindParameters candidate.body.S.typedParams args state in
     let mapping, state = List.fold_left (fun (mapping, state) (old, _) -> let id, state = nextValue state (valueType candidate.body old) in M.add old id mapping, state) (mapping, state) body.S.operations in
     let operations = List.filter_map (fun (old, operation) -> if exists (fun plan -> old = plan.tupleId) projection then None else Some (requiredTemp old mapping, I.renameCExpr mapping operation)) body.S.operations in
     let resultOperations = match projection with Some plan -> List.map2 (fun (param : A.typedParam) field -> param.A.id, A.Atom (renameAtom mapping field)) plan.parameters plan.fields | None -> [result, A.Atom (renameAtom mapping returned)] in
     let replacement = {block with S.operations = before @ bindings @ operations @ resultOperations @ after} in
     let depths = List.fold_left (fun depths (id, _) -> M.add id (addInt depth 1) depths) state.depths operations in
     Some {state with func = {state.func with S.blocks = S.LabelMap.add block.S.label replacement state.func.S.blocks}; depths; processed = Set.add result state.processed}
   | _ -> None)
 | _ -> None
let callSitesInBlock processed (block : S.block) = List.mapi (fun index operation -> index, operation) block.S.operations |> List.filter_map (function index, (id, A.Call (name, args)) when not (Set.mem id processed) -> Some (index, id, name, args) | _ -> None) |> List.rev
(*
   Cache each block's reverse-ordered pending sites. Declined calls consume the
   worklist directly; only replaced/new blocks need operand discovery again.
*)
let refreshCallSites state previous = S.LabelMap.fold (fun label block sites -> match S.LabelMap.find_opt label previous with Some (original, pending) when original == block -> S.LabelMap.add label (block, pending) sites | _ -> match callSitesInBlock state.processed block with [] -> sites | pending -> S.LabelMap.add label (block, pending) sites) state.func.S.blocks S.LabelMap.empty
let countExternalCalls externals (func : S.functionDef) = S.LabelMap.fold (fun _ block count -> List.fold_left (fun count (_, operation) -> match operation with A.Call (name, _) | A.BorrowedCall (name, _) when FunctionIdMap.containsKey name externals -> addInt count 1 | _ -> count) count block.S.operations) func.S.blocks 0
let mandatoryExternal candidate = match candidate.info.I.func.A.typedParams, candidate.info.I.func.A.body with [], A.Return (A.IntLiteral _) | [], A.Return (A.BoolLiteral _) | [], A.Return (A.FloatLiteral _) | [], A.Return (A.StringLiteral _) | [], A.Return A.UnitLiteral -> true | _ -> false
let inlineFunction (config : I.inliningConfig) candidates externals (func : S.functionDef) =
 let useExternal = countExternalCalls externals func <= config.I.maxExternalInlineSites in
 let counts = S.LabelMap.fold (fun _ block counts -> List.fold_left (fun counts (_, operation) -> match operation with A.Call (name, _) -> FunctionIdMap.add name (addInt 1 (Option.value ~default:0 (FunctionIdMap.tryFind name counts))) counts | _ -> counts) counts block.S.operations) func.S.blocks FunctionIdMap.empty in
 let rec visit state sites previousBlocks =
  let sites = if previousBlocks == state.func.S.blocks then sites else refreshCallSites state sites in
  match S.LabelMap.max_binding_opt sites with
  | None -> state.func
  | Some (label, (block, pending)) ->
    let index, id, name, args, rest = match pending with (index, id, name, args) :: rest -> index, id, name, args, rest | [] -> Crash.crash "SSA inlining retained an empty site worklist" in
    let sites = if rest = [] then S.LabelMap.remove label sites else S.LabelMap.add label (block, rest) sites in
    let visitNext next = visit next sites state.func.S.blocks in
    let state = {state with processed = Set.add id state.processed} in
    let depth = Option.value ~default:0 (M.find_opt id state.depths) in
    match FunctionIdMap.tryFind name candidates, S.LabelMap.find_opt label state.func.S.blocks with
    | Some candidate, Some block when candidate.info.I.isRecursive -> (match tryExpandBoundedCall config state block index id args candidate with Some expanded -> visitNext expanded | None -> visitNext state)
    | Some candidate, Some block when (not candidate.info.I.isExternal || useExternal || mandatoryExternal candidate) && List.length candidate.body.S.typedParams = List.length args ->
      let count = Option.value ~default:0 (FunctionIdMap.tryFind name counts) in
      (match tryProjectedPlan config state.func block index id count candidate with
       | Some projection -> (match cloneLinearAt state block index id args candidate depth (Some projection) with Some inlined -> visitNext inlined | None -> visitNext (cloneAt state block index id args candidate depth (Some projection)))
       | None when I.shouldInline candidate.info config depth -> (match cloneLinearAt state block index id args candidate depth None with Some inlined -> visitNext inlined | None ->
         let returns = S.LabelMap.fold (fun _ body count -> match body.S.terminator with S.Return _ -> addInt count 1 | _ -> count) candidate.body.S.blocks 0 in
         let _, _, after = splitAtOperation index block.S.operations in
         if returns <= 1 || canShareLargeContinuation candidate.body.S.returnType || Option.is_some (continuationRegion state.func block id after returns) then visitNext (cloneAt state block index id args candidate depth None) else visitNext state)
       | None -> visitNext state)
    | _ -> visitNext state in
 let state = initialState func in visit state (refreshCallSites state S.LabelMap.empty) state.func.S.blocks
let inlineProgramWithExternalCandidatesAndExclusions config externals externalSSA excluded localSource functions =
 let withSSAFacts (body : S.functionDef) (info : I.functionInfo) =
  let size, closures, tails = S.LabelMap.fold (fun _ block facts -> List.fold_left (fun (size, closures, tails) (_, operation) -> let closure, tail = match operation with A.ClosureAlloc _ | A.ClosureCall _ -> true, false | A.ClosureTailCall _ -> true, true | A.TailCall _ | A.IndirectTailCall _ -> false, true | _ -> false, false in addInt size 1, closures || closure, tails || tail) facts block.S.operations) body.S.blocks (0, false, false) in
  {info with I.size; hasClosures = closures; hasTailCalls = tails} in
 let localInfo = FunctionIdMap.filter (fun name _ -> not (SpecializationIdentity.FunctionSet.mem name excluded)) (I.buildFunctionInfoMap localSource) in
 let all = FunctionIdMap.ofList (List.map (fun (func : S.functionDef) -> func.S.id, func) (externalSSA @ functions)) in
 let candidates = FunctionIdMap.fold (fun current name body -> match FunctionIdMap.tryFind name externals with Some info -> FunctionIdMap.add name {info = withSSAFacts body info; body} current | None -> current) FunctionIdMap.empty all in
 let candidates = FunctionIdMap.fold (fun current name info -> match FunctionIdMap.tryFind name all with Some body -> FunctionIdMap.add name {info = withSSAFacts body info; body} current | None -> current) candidates localInfo in
 List.map (inlineFunction config candidates externals) functions
