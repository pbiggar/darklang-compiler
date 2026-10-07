(*
   The SSA inliner uses these source-level facts before cloning typed blocks.
*)
(* InliningCommon.ml - Call eligibility, external candidate analysis, and ANF renaming. *)
[@@@warning "-4-42"]
open ANF
module F = FunctionIdMap
module FS = SpecializationIdentity.FunctionSet
module TempMap = Map.Make (struct type t = tempId let compare (TempId first) (TempId second) = Int.compare first second end)
(*
   Inlining configuration
   Maximum function body size (in TempIds) to inline
   Maximum depth of nested inlining
   Maximum external stdlib wrapper calls to inline in one caller body
   Maximum statically proven recursive-loop iterations to expand
   Maximum primitive bindings introduced by one bounded-loop expansion
   Maximum body size for a tuple-return call whose every element is projected immediately
   Maximum projected-tuple calls expanded in one caller
*)
type inliningConfig = {maxFunctionSize : int; maxInlineDepth : int; maxExternalInlineSites : int; maxBoundedLoopIterations : int; maxBoundedLoopExpansion : int; maxProjectedTupleInlineSize : int; maxProjectedTupleInlineSites : int}
(*
   Default inlining configuration
*)
let defaultConfig = {maxFunctionSize = 20; maxInlineDepth = 3; maxExternalInlineSites = 8; maxBoundedLoopIterations = 8; maxBoundedLoopExpansion = 48; maxProjectedTupleInlineSize = 64; maxProjectedTupleInlineSites = 12}
(*
   Information about a function for inlining decisions
   Count of TempIds (Let bindings) in body
   Calls itself directly
   Contains ClosureAlloc or ClosureCall
   Contains TailCall or ClosureTailCall
   Body is available only as an inline candidate
*)
type functionInfo = {func : functionDef; calls : FS.t; size : int; isRecursive : bool; hasClosures : bool; hasTailCalls : bool; isExternal : bool}
(*
   Phase 1: Analysis - Build function info map
   Properties used by call-graph construction and inlining eligibility.
   Collecting them together keeps function analysis to one ANF traversal.
*)
type functionAnalysis = {calls : FS.t; size : int; maxTempId : int; hasClosures : bool; hasTailCalls : bool}
let emptyAnalysis : functionAnalysis = {calls = FS.empty; size = 0; maxTempId = 0; hasClosures = false; hasTailCalls = false}
let rec analyzeExpr expr : functionAnalysis = match expr with
 | Let (TempId tempId, cexpr, body) -> let bodyAnalysis = analyzeExpr body in let letAnalysis = {bodyAnalysis with size = bodyAnalysis.size + 1; maxTempId = max tempId bodyAnalysis.maxTempId} in (match cexpr with
  | Call (name, _) | BorrowedCall (name, _) -> {letAnalysis with calls = FS.add name letAnalysis.calls}
  | TailCall (name, _) -> {letAnalysis with calls = FS.add name letAnalysis.calls; hasTailCalls = true}
  | ClosureTailCall _ -> {letAnalysis with hasClosures = true; hasTailCalls = true}
  | IndirectTailCall _ -> {letAnalysis with hasTailCalls = true}
  | ClosureAlloc _ | ClosureCall _ -> {letAnalysis with hasClosures = true}
  | _ -> letAnalysis)
 | Jump (TempId target, atom) -> let value = analyzeExpr (Return atom) in {value with maxTempId = max target value.maxTempId; size = 1}
 | Join (parameter, continuation, entry) -> let body = analyzeExpr continuation in let entry = analyzeExpr entry in let TempId parameterId = parameter.id in
  {calls = FS.union body.calls entry.calls; size = 1 + body.size + entry.size; maxTempId = max parameterId (max body.maxTempId entry.maxTempId); hasClosures = body.hasClosures || entry.hasClosures; hasTailCalls = body.hasTailCalls || entry.hasTailCalls}
 | Return (Var (TempId tempId)) -> {emptyAnalysis with maxTempId = tempId}
 | Return _ -> emptyAnalysis
 | If (condition, yes, no) -> let yes = analyzeExpr yes in let no = analyzeExpr no in let conditionMaxTempId = match condition with Var (TempId tempId) -> tempId | _ -> 0 in
  {calls = FS.union yes.calls no.calls; size = yes.size + no.size; maxTempId = max conditionMaxTempId (max yes.maxTempId no.maxTempId); hasClosures = yes.hasClosures || no.hasClosures; hasTailCalls = yes.hasTailCalls || no.hasTailCalls}
(*
   Mutual Recursion Detection via SCC (Strongly Connected Components)
   Uses Kosaraju's algorithm to find SCCs in the call graph
   Build reverse call graph: Map<callee, Set<callers>>
*)
let buildReverseCallGraph graph = F.fold (fun acc caller callees -> FS.fold (fun callee acc -> let existing = Option.value (F.tryFind callee acc) ~default:FS.empty in F.add callee (FS.add caller existing) acc) callees acc) F.empty graph
(*
   DFS to compute finish order (for Kosaraju's algorithm)
*)
let rec dfsFinishOrder graph node visited order = if FS.mem node visited then visited, order else
 let visited = FS.add node visited in let neighbors = Option.value (F.tryFind node graph) ~default:FS.empty in
 let visited, order = FS.fold (fun neighbor (visited, order) -> dfsFinishOrder graph neighbor visited order) neighbors (visited, order) in visited, node :: order
(*
   DFS to collect SCC members
*)
let rec dfsCollectSCC graph node visited scc = if FS.mem node visited then visited, scc else
 let visited = FS.add node visited in let scc = FS.add node scc in let neighbors = Option.value (F.tryFind node graph) ~default:FS.empty in
 FS.fold (fun neighbor (visited, scc) -> dfsCollectSCC graph neighbor visited scc) neighbors (visited, scc)
(*
   Find all SCCs using Kosaraju's algorithm
   Returns list of SCCs, where each SCC is a Set of function names
   Step 1: DFS on original graph to get finish order
   Step 2: DFS on reverse graph in reverse finish order to find SCCs
*)
let findSCCs names graph = let reverseGraph = buildReverseCallGraph graph in
 let _, finishOrder = FS.fold (fun name (visited, order) -> dfsFinishOrder graph name visited order) names (FS.empty, []) in
 let _, sccs = List.fold_left (fun (visited, components) name -> if FS.mem name visited then visited, components else let visited, scc = dfsCollectSCC reverseGraph name visited FS.empty in visited, scc :: components) (FS.empty, []) finishOrder in sccs
(*
   Find all functions involved in mutual recursion (in SCCs of size > 1)
   or direct self-recursion (calls itself)
   Functions in SCCs of size > 1 (mutual recursion)
   Functions that call themselves (direct recursion)
*)
let findRecursiveFunctions funcs graph =
 let names = FS.of_list (List.map (fun (f : functionDef) -> f.id) funcs) in let sccs = findSCCs names graph in
 let mutuallyRecursive = List.fold_left FS.union FS.empty (List.filter (fun scc -> FS.cardinal scc > 1) sccs) in
 let directlyRecursive = List.filter (fun (f : functionDef) -> FS.mem f.id (Option.value (F.tryFind f.id graph) ~default:FS.empty)) funcs |> List.map (fun (f : functionDef) -> f.id) |> FS.of_list in FS.union mutuallyRecursive directlyRecursive
(*
   Build function info for a single function
*)
let buildFunctionInfo recursiveFuncs (func : functionDef) (analysis : functionAnalysis) : functionInfo = {func; calls = analysis.calls; size = analysis.size; isRecursive = FS.mem func.id recursiveFuncs; hasClosures = analysis.hasClosures; hasTailCalls = analysis.hasTailCalls; isExternal = false}
(*
   Analyze all functions once, building the inlining map while also finding the
   highest TempId needed to initialize the inliner's fresh-variable generator.
*)
let buildFunctionInfoMapAndMaxTempId funcs =
 let analyzedFuncs = List.map (fun (func : functionDef) -> func, analyzeExpr func.body) funcs in
 let graph = F.ofList (List.map (fun ((func : functionDef), (analysis : functionAnalysis)) -> func.id, analysis.calls) analyzedFuncs) in
 let recursiveFuncs = findRecursiveFunctions funcs graph in
 let infoMap = F.ofList (List.map (fun ((func : functionDef), analysis) -> func.id, buildFunctionInfo recursiveFuncs func analysis) analyzedFuncs) in
 let maxTempId = List.fold_left (fun maximum ((func : functionDef), (analysis : functionAnalysis)) -> let functionMax = List.fold_left (fun current (param : typedParam) -> let TempId id = param.id in max current id) analysis.maxTempId func.typedParams in max maximum functionMax) 0 analyzedFuncs in infoMap, maxTempId
(*
   Build function info map for all functions
*)
let buildFunctionInfoMap funcs = fst (buildFunctionInfoMapAndMaxTempId funcs)
(*
   Phase 2: TempId Renaming - Avoid variable conflicts when inlining
   Rename an atom (substitute TempIds)
   External reference, keep as-is
*)
let renameAtom mapping = function Var tid as atom -> (match TempMap.find_opt tid mapping with Some newTid -> Var newTid | None -> atom) | atom -> atom
(*
   Rename all TempIds in a CExpr
*)
let renameCExpr mapping cexpr =
    let r = renameAtom mapping in
    match cexpr with
    | Atom a -> Atom (r a)
    | TypedAtom (a, t) -> TypedAtom (r a, t)
    | Prim (op, left, right) -> Prim (op, r left, r right)
    | UnaryPrim (op, src) -> UnaryPrim (op, r src)
    | IfValue (cond, thenVal, elseVal) -> IfValue (r cond, r thenVal, r elseVal)
    | Call (name, args) -> Call (name, List.map r args)
    | BorrowedCall (name, args) -> BorrowedCall (name, List.map r args)
    | TailCall (name, args) -> TailCall (name, List.map r args)
    | IndirectCall (func, args) -> IndirectCall (r func, List.map r args)
    | IndirectTailCall (func, args) -> IndirectTailCall (r func, List.map r args)
    | ClosureAlloc (name, captures) -> ClosureAlloc (name, List.map r captures)
    | ClosureCall (closure, args) -> ClosureCall (r closure, List.map r args)
    | ClosureTailCall (closure, args) -> ClosureTailCall (r closure, List.map r args)
    | TupleAlloc elems -> TupleAlloc (List.map r elems)
    | TupleGet (tuple, idx) -> TupleGet (r tuple, idx)
    | RecordAlloc (descriptor, fields) -> RecordAlloc (descriptor, List.map r fields)
    | RecordGet (descriptor, record, idx) -> RecordGet (descriptor, r record, idx)
    | RecordClone (descriptor, record, fields) ->
        RecordClone (descriptor, r record, List.map r fields)
    | RecordReuse (sourceDescriptor, targetDescriptor, record, fields) ->
        RecordReuse (sourceDescriptor, targetDescriptor, r record, List.map r fields)
    | StringConcat (first, second, remaining) ->
        StringConcat (r first, r second, List.map r remaining)
    | CanonicalBufferEq (kind, left, right) -> CanonicalBufferEq (kind, r left, r right)
    | RefCountInc (a, size, kind, sourceType) -> RefCountInc (r a, size, kind, sourceType)
    | RefCountDec (a, size, kind, sourceType) -> RefCountDec (r a, size, kind, sourceType)
    | Print (a, t) -> Print (r a, t)
    | StdoutWrite (a, appendNewline) -> StdoutWrite (r a, appendNewline)
    | StdinReadLine -> StdinReadLine
    | FileReadBlob path -> FileReadBlob (r path)
    | FileExists path -> FileExists (r path)
    | FileWriteBlob (path, content) -> FileWriteBlob (r path, r content)
    | FileAppendText (path, content) -> FileAppendText (r path, r content)
    | FileDelete path -> FileDelete (r path)
    | FileCreateDirectory path -> FileCreateDirectory (r path)
    | FileSetExecutable path -> FileSetExecutable (r path)
    | FileWriteFromPtr (path, ptr, len) -> FileWriteFromPtr (r path, r ptr, r len)
    | FloatSqrt a -> FloatSqrt (r a)
    | FloatAbs a -> FloatAbs (r a)
    | FloatNeg a -> FloatNeg (r a)
    | Int64ToFloat a -> Int64ToFloat (r a)
    | FloatToInt64 a -> FloatToInt64 (r a)
    | FloatToBits a -> FloatToBits (r a)
    | RawAlloc numBytes -> RawAlloc (r numBytes)
    | MappedAlloc numBytes -> MappedAlloc (r numBytes)
    | RawFree ptr -> RawFree (r ptr)
    | MappedFree ptr -> MappedFree (r ptr)
    | RawGet (ptr, offset, valueType) -> RawGet (r ptr, r offset, valueType)
    | RawTake (ptr, offset, valueType) -> RawTake (r ptr, r offset, valueType)
    | RawGetByte (ptr, offset) -> RawGetByte (r ptr, r offset)
    | RawWriteWord (ptr, offset, value) -> RawWriteWord (r ptr, r offset, r value)
    | RawWriteByte (ptr, offset, value) -> RawWriteByte (r ptr, r offset, r value)
    | RawSlotInit (ptr, offset, value, valueType) -> RawSlotInit (r ptr, r offset, r value, valueType)
    | StringToRawPtr value -> StringToRawPtr (r value)
    | RawPtrToString ptr -> RawPtrToString (r ptr)
    | BlobToRawPtr value -> BlobToRawPtr (r value)
    | RawPtrToBlob ptr -> RawPtrToBlob (r ptr)
    | RawPtrToInt128 ptr -> RawPtrToInt128 (r ptr)
    | RawPtrToUInt128 ptr -> RawPtrToUInt128 (r ptr)
    | DictToRawPtr dict -> DictToRawPtr (r dict)
    | RawPtrToDict (ptr, tag, dictType) -> RawPtrToDict (r ptr, r tag, dictType)
    | ListToRawPtr list -> ListToRawPtr (r list)
    | FixedBlockToRawPtr value -> FixedBlockToRawPtr (r value)
    | RawPtrToList (ptr, tag, listType) -> RawPtrToList (r ptr, r tag, listType)
    | RefCountIncString a -> RefCountIncString (r a)
    | RefCountDecString a -> RefCountDecString (r a)
    | RefCountIncBlob a -> RefCountIncBlob (r a)
    | RefCountDecBlob a -> RefCountDecBlob (r a)
    | RefCountIncInt a -> RefCountIncInt (r a)
    | RefCountDecInt a -> RefCountDecInt (r a)
    | RandomInt64 -> RandomInt64
    | DateTimeNow -> DateTimeNow
    | Sleep delayMs -> Sleep (r delayMs)
    | CliNative (operation, args) -> CliNative (operation, List.map r args)
    | FloatToString a -> FloatToString (r a)
    | RuntimeError message -> RuntimeError message
    | RuntimeErrorString atom -> RuntimeErrorString (r atom)

(*
   Rename all TempIds in an expression, allocating fresh TempIds
   Allocate fresh TempId for this binding
   Rename the CExpr (uses old mapping for references)
   Rename the body (uses new mapping including this binding)
*)
let rec renameExpr mapping varGen = function
 | Let (tid, cexpr, body) -> let newTid, varGen = freshVar varGen in let mappingNew = TempMap.add tid newTid mapping in let cexpr = renameCExpr mapping cexpr in let body, varGen = renameExpr mappingNew varGen body in Let (newTid, cexpr, body), varGen
 | Jump (target, atom) -> let target = Option.value (TempMap.find_opt target mapping) ~default:target in Jump (target, renameAtom mapping atom), varGen
 | Join (parameter, continuation, entry) -> let newId, next = freshVar varGen in let mapping = TempMap.add parameter.id newId mapping in let body, afterBody = renameExpr mapping next continuation in let entry, final = renameExpr mapping afterBody entry in Join ({parameter with id = newId}, body, entry), final
 | Return atom -> Return (renameAtom mapping atom), varGen
 | If (condition, yes, no) -> let yes, afterYes = renameExpr mapping varGen yes in let no, final = renameExpr mapping afterYes no in If (renameAtom mapping condition, yes, no), final
(*
   Eligibility shared by SSA inlining and external candidate selection
   Check if a function should be inlined
*)
let shouldInline (info : functionInfo) config depth = info.size <= config.maxFunctionSize && not (String.starts_with ~prefix:"Darklang.Stdlib.Json.__" info.func.name) && not info.isRecursive && not info.hasClosures && not info.hasTailCalls && depth < config.maxInlineDepth
let isSimpleExternalCExpr = function Atom _ | TypedAtom _ | Prim _ | UnaryPrim _ | IfValue _ | TupleGet _ | StringConcat _ | CanonicalBufferEq _ | FloatSqrt _ | FloatAbs _ | FloatNeg _ | Int64ToFloat _ | FloatToInt64 _ | FloatToBits _ | FloatToString _ -> true | _ -> false
let isScalarRawReadExternalCExpr cexpr = isSimpleExternalCExpr cexpr || match cexpr with RawGet _ | RawGetByte _ | StringToRawPtr _ -> true | _ -> false
let rec isExternalExprWith isAllowed = function Let (_, cexpr, body) -> isAllowed cexpr && isExternalExprWith isAllowed body | Return _ -> true | Jump _ | Join _ | If _ -> false
let isSimpleExternalExpr expr = isExternalExprWith isSimpleExternalCExpr expr
let isScalarRawReadReturnType = function
 | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TInt | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128 | AST.TBool | AST.TFloat64 | AST.TChar | AST.TDateTime | AST.TUnit -> true
 | AST.TString | AST.TBlob | AST.TNever | AST.TInternalRawPtr | AST.TFunction _ | AST.TTuple _ | AST.TRecord _ | AST.TSum _ | AST.TList _ | AST.TDict _ | AST.TStream _ | AST.TVar _ | AST.TInferenceVar _ -> false
let rec _countCallsToNames names = function
 | Let (_, (Call (name, _) | BorrowedCall (name, _)), body) -> (if FS.mem name names then 1 else 0) + _countCallsToNames names body
 | Let (_, _, body) -> _countCallsToNames names body
 | Jump _ | Return _ -> 0
 | Join (_, continuation, entry) -> _countCallsToNames names continuation + _countCallsToNames names entry
 | If (_, yes, no) -> _countCallsToNames names yes + _countCallsToNames names no
let shouldUseExternalCandidate (info : functionInfo) config = shouldInline info config 0 && FS.is_empty info.calls && (isSimpleExternalExpr info.func.body || isScalarRawReadReturnType info.func.returnType && isExternalExprWith isScalarRawReadExternalCExpr info.func.body)
(*
   Analyze and qualify external functions once so user-program inlining can
   reuse the metadata without traversing stdlib bodies on every compilation.
*)
let buildExternalCandidateInfoMap config functions = F.fold (fun candidates name info -> if shouldUseExternalCandidate info config then F.add name {info with isExternal = true} candidates else candidates) F.empty (buildFunctionInfoMap functions)
