(*
   Lexical joins become block parameters, and their jumps carry the parameter
   value on the edge. ANF may reuse one TempId in sibling branches; construction
   freshens later definitions so each SSA value has one defining site.
   LIR uses these virtual IDs for physical spill and ABI scratch registers.
*)
(* SSAANF.ml - Typed control-flow form for optimized ANF operations. *)
[@@@warning "-4"]
module A = ANF
module F = RcTypeFacts
module R = RcReturnAnalysis
module M = F.TempMap
module Set = R.TempSet
module I = InliningCommon
type label = Label of int
module LabelMap = Map.Make (struct type t = label let compare (Label left) (Label right) = Int.compare left right end)
type terminator = Return of A.atom | Jump of label * A.atom list | Branch of A.atom * label * label
type block = {label : label; parameters : A.typedParam list; operations : (A.tempId * A.cExpr) list; terminator : terminator}
type functionDef = {id : AST.functionId; name : string; typedParams : A.typedParam list; returnType : AST.semanticType; returnOwnership : A.returnOwnership; entry : label; blocks : block LabelMap.t; freshValueTypes : AST.semanticType M.t}
type renaming = {seen : Set.t; next : A.varGen; freshTypes : AST.semanticType M.t}
let ( let* ) = Result.bind
let intIncrement value = Int32.to_int (Int32.add (Int32.of_int value) 1l)
let tempText (A.TempId id) = "TempId " ^ string_of_int id
let reserved (A.TempId id) = id = 1000 || id = 1001 || id = 1002 || id = 2000 || (id >= 3000 && id < 4000)
let define typeMap id known state =
 if not (Set.mem id state.seen) && not (reserved id) then Ok (id, {state with seen = Set.add id state.seen}) else
 let fresh, next = A.freshVar state.next in
 let typ = match known with Some _ -> known | None -> A.TypeMap.tryFind id typeMap in
 match typ with None -> Error ("SSA ANF: missing type for repeated value " ^ tempText id) | Some typ -> Ok (fresh, {seen = Set.add fresh state.seen; next; freshTypes = M.add fresh typ state.freshTypes})
let renamedId mapping id = Option.value ~default:id (M.find_opt id mapping)
let rec freshenDefinitions typeMap mapping state = function
 | A.Let (id, operation, rest) ->
   let operation = I.renameCExpr mapping operation in
   let* defined, state = define typeMap id None state in
   let* rest, state = freshenDefinitions typeMap (M.add id defined mapping) state rest in Ok (A.Let (defined, operation, rest), state)
 | A.Return value -> Ok (A.Return (I.renameAtom mapping value), state)
 | A.Jump (target, value) -> Ok (A.Jump (renamedId mapping target, I.renameAtom mapping value), state)
 | A.If (condition, yes, no) ->
   let* yes, state = freshenDefinitions typeMap mapping state yes in
   let* no, state = freshenDefinitions typeMap mapping state no in Ok (A.If (I.renameAtom mapping condition, yes, no), state)
 | A.Join (param, continuation, entry) ->
   let* defined, state = define typeMap param.A.id (Some param.A.typ) state in
   let scoped = M.add param.A.id defined mapping in
   let* continuation, state = freshenDefinitions typeMap scoped state continuation in
   let* entry, state = freshenDefinitions typeMap scoped state entry in Ok (A.Join ({param with A.id = defined}, continuation, entry), state)
(*
   Freshen source definitions while recovering their types in lexical order.
   The type belongs to a definition site, so sibling definitions that reuse
   one ANF TempId may have different types after SSA freshening.
*)
let rec freshenTypedDefinitions mapping state ctx types = function
 | R.RLet (id, operation, rest, _) ->
   let typ = RcInsertExpression.inferBindingType (F.withTempTypes ctx types) id operation rest in
   let operation' = I.renameCExpr mapping operation in
   let* defined, state = define A.TypeMap.empty id (Some typ) state in
   let types = match operation with A.TypedAtom (A.Var source, aliasType) -> M.add source aliasType (M.add id typ types) | _ -> M.add id typ types in
   let ctx = match operation with A.ClosureAlloc (func, _) -> F.addClosureFunc (F.withTempTypes ctx types) id func | _ -> F.withTempTypes ctx types in
   let state = {state with freshTypes = M.add defined typ state.freshTypes} in
   let state = match operation with A.TypedAtom (A.Var source, aliasType) -> let source = renamedId mapping source in (match M.find_opt source state.freshTypes with Some (AST.TVar _ | AST.TInferenceVar _) -> {state with freshTypes = M.add source aliasType state.freshTypes} | _ -> state) | _ -> state in
   let* rest, state, types = freshenTypedDefinitions (M.add id defined mapping) state ctx types rest in Ok (A.Let (defined, operation', rest), state, types)
 | R.RReturn (value, _) -> Ok (A.Return (I.renameAtom mapping value), state, types)
 | R.RJump (target, value, _) -> Ok (A.Jump (renamedId mapping target, I.renameAtom mapping value), state, types)
 | R.RIf (condition, yes, no, _) ->
   let* yes, state, types = freshenTypedDefinitions mapping state ctx types yes in
   let* no, state, types = freshenTypedDefinitions mapping state ctx types no in Ok (A.If (I.renameAtom mapping condition, yes, no), state, types)
 | R.RJoin (param, continuation, entry, _) ->
   let* defined, state = define A.TypeMap.empty param.A.id (Some param.A.typ) state in
   let scoped = M.add param.A.id defined mapping in
   let types = M.add param.A.id param.A.typ types in
   let state = {state with freshTypes = M.add defined param.A.typ state.freshTypes} in
   let* continuation, state, types = freshenTypedDefinitions scoped state ctx types continuation in
   let* entry, state, types = freshenTypedDefinitions scoped state ctx types entry in Ok (A.Join ({param with A.id = defined}, continuation, entry), state, types)
type builder = {nextLabel : int; builtBlocks : block LabelMap.t}
let freshLabel builder = Label builder.nextLabel, {builder with nextLabel = intIncrement builder.nextLabel}
let finish label parameters operationsRev terminator builder =
 if LabelMap.mem label builder.builtBlocks then let Label id = label in Error ("SSA ANF: block Label " ^ string_of_int id ^ " is defined twice") else
 let block = {label; parameters; operations = List.rev operationsRev; terminator} in Ok {builder with builtBlocks = LabelMap.add label block builder.builtBlocks}
let rec convertExpr joins label params operations expr builder = match expr with
 | A.Let (id, operation, rest) -> convertExpr joins label params ((id, operation) :: operations) rest builder
 | A.Return value -> finish label params operations (Return value) builder
 | A.Jump (target, value) -> (match M.find_opt target joins with Some target -> finish label params operations (Jump (target, [value])) builder | None -> Error ("SSA ANF: join target " ^ tempText target ^ " is outside lexical scope"))
 | A.If (condition, yes, no) ->
   let yesLabel, builder = freshLabel builder in let noLabel, builder = freshLabel builder in
   let* builder = finish label params operations (Branch (condition, yesLabel, noLabel)) builder in
   let* builder = convertExpr joins yesLabel [] [] yes builder in convertExpr joins noLabel [] [] no builder
 | A.Join (param, continuation, entry) ->
   let continuationLabel, builder = freshLabel builder in
   let* builder = convertExpr (M.add param.A.id continuationLabel joins) label params operations entry builder in
   convertExpr joins continuationLabel [param] [] continuation builder
let initialRenaming maxSource (func : A.functionDef) = {seen = Set.of_list (List.map (fun (param : A.typedParam) -> param.A.id) func.A.typedParams); next = A.VarGen (max 4000 (intIncrement maxSource)); freshTypes = M.empty}
let parameters beforeRC params state =
 List.fold_left (fun (mapping, paramsRev, state) (param : A.typedParam) ->
  if reserved param.A.id then let fresh, next = A.freshVar state.next in
   M.add param.A.id fresh mapping, {param with A.id = fresh} :: paramsRev, {next; seen = Set.add fresh state.seen; freshTypes = if beforeRC then M.add fresh param.A.typ state.freshTypes else state.freshTypes}
  else mapping, param :: paramsRev, (if beforeRC then {state with freshTypes = M.add param.A.id param.A.typ state.freshTypes} else state)) (M.empty, [], state) params
let finishFunction (func : A.functionDef) params state body =
 let entry = Label 0 in let* builder = convertExpr M.empty entry [] [] body {nextLabel = 1; builtBlocks = LabelMap.empty} in
 Ok {id = func.A.id; name = func.A.name; typedParams = List.rev params; returnType = func.A.returnType; returnOwnership = func.A.returnOwnership; entry; blocks = builder.builtBlocks; freshValueTypes = state.freshTypes}
(*
   Fresh source IDs must also avoid LIR's fixed virtual-register range.
*)
let convertFunction maxSource typeMap (func : A.functionDef) =
 let mapping, params, state = parameters false func.A.typedParams (initialRenaming maxSource func) in
 let* body, state = freshenDefinitions typeMap mapping state func.A.body in finishFunction func params state body
(*
   Construct SSA from optimized ANF before reference-count elaboration.
   Type recovery is tied to each definition site, rather than the final
   program-wide TempId map produced by RC insertion.
*)
let convertFunctionBeforeRC maxSource ctx (func : A.functionDef) =
 let mapping, params, state = parameters true func.A.typedParams (initialRenaming maxSource func) in
 let types = List.fold_left (fun types (param : A.typedParam) -> M.add param.A.id param.A.typ types) M.empty func.A.typedParams in
 let body = R.analyzeReturns M.empty M.empty func.A.body in
 let* body, state, _ = freshenTypedDefinitions mapping state ctx types body in finishFunction func params state body
