(*
   Text fixtures for the production SSA inliner. Each function body is parsed
   once and lowered to the ANF analysis input and the SSA transformation input.
*)
(* SSAInliningFormat.fs - Text fixtures for the production SSA inliner. *)
[@@@warning "-4-42"]
open Dark_compiler
module A = ANF
module S = SSAANF
module M = InliningCommon.TempMap
module Names = StringOrder.Map
module Functions = SpecializationIdentity.FunctionSet
type expr = Return of A.atom | Bind of A.tempId * AST.semanticType * A.cExpr * expr | Branch of A.atom * expr * expr
type fixture = {source : A.functionDef; ssa : S.functionDef; isExternal : bool}
type assertion = {text : string; afterEscape : bool; measure : S.functionDef -> int; minimum : bool; expected : int}
type case = {name : string; functions : fixture list; assertions : assertion list; optimizeSSA : bool; skipInlining : bool}
type rawCase = {rawName : string; rawFunctions : string list list; externals : string list list; expected : string list; optimize : bool; skip : bool}
let problem message = Crash.crash ("SSA inlining fixture: " ^ message)
(* Translate the small regular-expression syntax used by the frozen fixtures,
   retaining only the original capturing groups in the result. *)
let matched pattern text =
 let output = Buffer.create (String.length pattern) in
 let rec convert index inClass group captures =
  if index = String.length pattern then List.rev captures else
  match pattern.[index] with
  | '\\' when index + 1 < String.length pattern ->
    let value = match pattern.[index + 1] with 's' -> "[ \t\r\n\012]" | 'd' -> "[0-9]" | 'S' -> "[^ \t\r\n\012]" | '(' -> "(" | ')' -> ")" | character -> "\\" ^ String.make 1 character in
    Buffer.add_string output value; convert (index + 2) inClass group captures
  | '[' -> Buffer.add_char output '['; convert (index + 1) true group captures
  | ']' -> Buffer.add_char output ']'; convert (index + 1) false group captures
  | '(' when not inClass ->
    let hidden = index + 2 < String.length pattern && String.sub pattern index 3 = "(?:" in
    Buffer.add_string output "\\("; convert (index + if hidden then 3 else 1) false (group + 1) (if hidden then captures else (group + 1) :: captures)
  | ')' when not inClass -> Buffer.add_string output "\\)"; convert (index + 1) false group captures
  | '|' when not inClass -> Buffer.add_string output "\\|"; convert (index + 1) false group captures
  | character -> Buffer.add_char output character; convert (index + 1) inClass group captures in
 let captures = convert 0 false 0 [] in
 let regex = Str.regexp (Buffer.contents output) in
 if Str.string_match regex text 0 then Some (Array.of_list (text :: List.map (fun index -> try Str.matched_group index text with Not_found -> "") captures)) else None
let group values index = values.(index)
let parse32 text = match HostText.tryParseInt32 text with Some value -> Int32.to_int value | None -> raise (Failure "Value was either too large or too small for an Int32.")
let add left right = Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))
let mul left right = Int32.to_int (Int32.mul (Int32.of_int left) (Int32.of_int right))
let temp text = match matched {|^t(\d+)$|} (HostText.trim text) with Some values -> A.TempId (parse32 (group values 1)) | None -> problem ("expected a temp ID, got '" ^ text ^ "'")
let atom text =
 let text = HostText.trim text in
 if text = "true" then A.BoolLiteral true else if text = "false" then A.BoolLiteral false else if Option.is_some (matched {|^t\d+$|} text) then A.Var (temp text) else
 let number = try if Option.is_some (matched {|^[+-]?\d+$|} text) then Some (Int64.of_string text) else None with Failure _ -> None in
 match number with Some value -> A.IntLiteral (A.Int64 value) | None when Option.is_some (matched {|^-?\d+\.\d+$|} text) -> A.FloatLiteral (float_of_string text) | None -> problem ("expected a literal or temp ID, got '" ^ text ^ "'")
let rec typ text = match HostText.trim text with
 | "Int64" -> AST.TInt64 | "Float" -> AST.TFloat64 | "Bool" -> AST.TBool | "Unit" -> AST.TUnit
 | "FnInt64" -> AST.TFunction ([AST.TInt64], AST.TInt64) | "Body" -> AST.TRecord ("Body", [])
 | "Option<Int64>" -> AST.TSum ("Darklang.Stdlib.Option.Option", [AST.TInt64]) | "Option<Float>" -> AST.TSum ("Darklang.Stdlib.Option.Option", [AST.TFloat64])
 | "Tuple<Int64,Int64>" -> AST.TTuple [AST.TInt64; AST.TInt64] | "Tuple<Body,Body>" -> AST.TTuple [typ "Body"; typ "Body"] | "Tuple<Body,Body,Bool>" -> AST.TTuple [typ "Body"; typ "Body"; AST.TBool]
 | other -> problem ("unsupported type '" ^ other ^ "'")
let optionDescriptor : A.recordDescriptor = {A.sourceTypeName = "Darklang.Stdlib.Option.Option"; runtimeTypeName = "Darklang.Stdlib.Option.Option"; typeArgs = [AST.TInt64]; fields = ["tag", AST.TInt64; "payload", AST.TInt64]; valueType = typ "Option<Int64>"}
let floatOptionDescriptor = {optionDescriptor with A.typeArgs = [AST.TFloat64]; fields = ["tag", AST.TInt64; "payload", AST.TFloat64]; valueType = typ "Option<Float>"}
let bodyDescriptor : A.recordDescriptor = {A.sourceTypeName = "Body"; runtimeTypeName = "Body"; typeArgs = []; fields = ["x", AST.TInt64; "y", AST.TInt64]; valueType = typ "Body"}
let operands text = if HostText.trim text = "" then [] else List.map atom (String.split_on_char ',' text)
let operation valueType text =
 match matched {|^closure\s+([A-Za-z_][A-Za-z_0-9.]*)$|} text, matched {|^call\s+([A-Za-z_][A-Za-z_0-9.]*)\((.*)\)$|} text with
 | Some values, _ -> A.ClosureAlloc (TestIds.functionIdForName (group values 1), [])
 | None, Some values -> A.Call (TestIds.functionIdForName (group values 1), operands (group values 2))
 | None, None ->
  let values = match matched {|^([a-z_][a-z_0-9]*)\((.*)\)$|} text with Some values -> values | None -> problem ("invalid operation '" ^ text ^ "'") in
  let name = group values 1 and args = operands (group values 2) in
  let binary op = match args with [left; right] -> A.Prim (op, left, right) | _ -> problem (name ^ " requires two operands") in
  match name, args with
  | "add", _ -> binary A.Add | "mul", _ -> binary A.Mul | "div", _ -> binary A.Div | "mod", _ -> binary A.Mod | "eq", _ -> binary A.Eq | "gte", _ -> binary A.Gte | "lt", _ -> binary A.Lt | "bitand", _ -> binary A.BitAnd | "bitxor", _ -> binary A.BitXor
  | "tuple", fields when List.length fields = 2 -> A.TupleAlloc fields | "tuple3", fields when List.length fields = 3 -> A.TupleAlloc fields
  | "get", [source; A.IntLiteral (A.Int64 index)] -> A.TupleGet (source, Int32.to_int (Int64.to_int32 index))
  | "copy", [source] -> A.Atom source | "closure_call", closure :: args -> A.ClosureCall (closure, args) | "typed", [source] -> A.TypedAtom (source, valueType)
  | "body", fields when List.length fields = 2 -> A.RecordAlloc (bodyDescriptor, fields)
  | "field", [source; A.IntLiteral (A.Int64 index)] -> A.RecordGet (bodyDescriptor, source, Int32.to_int (Int64.to_int32 index))
  | "some", [value] -> A.RecordAlloc (optionDescriptor, [atom "0"; value]) | "none", [] -> A.RecordAlloc (optionDescriptor, [atom "1"; atom "0"])
  | "payload", [source] -> A.RecordGet (optionDescriptor, source, 1) | "some_float", [value] -> A.RecordAlloc (floatOptionDescriptor, [atom "0"; value])
  | "none_float", [] -> A.RecordAlloc (floatOptionDescriptor, [atom "1"; atom "0.0"]) | "payload_float", [source] -> A.RecordGet (floatOptionDescriptor, source, 1)
  | _ -> problem ("unsupported operation or operands '" ^ text ^ "'")
let range first last = if first > last then [] else List.init (last - first + 1) (fun index -> first + index)
let tempIndex text = let A.TempId index = temp text in index
(*
   Compact repetition covers the hot call patterns without copying dozens of
   near-identical lets into the fixture. Expansion produces ordinary DSL lines.
*)
let expandLine line =
 match matched {|^repeat_add\s+(t\d+)\.\.(t\d+)\s+from\s+(t\d+)$|} line,
       matched {|^project_chain\s+([A-Za-z_][A-Za-z_0-9]*)\s+(\d+)\s+from\s+(t\d+)\s+at\s+(t\d+)(?:\s+of\s+(Int64|Body))?(?:\s+aliases\s+(\d+))?$|} line,
       matched {|^call_chain\s+([A-Za-z_][A-Za-z_0-9.]*)\s+(\d+)\s+from\s+(t\d+)\s+at\s+(t\d+)$|} line with
 | Some values, _, _ ->
  let first = tempIndex (group values 1) and last = tempIndex (group values 2) and initial = tempIndex (group values 3) in
  if last < first || add last (-first) > 64 then problem "repeat_add requires 1..65 bindings";
  List.map (fun index -> let previous = if index = first then initial else add index (-1) in Printf.sprintf "let t%d:Int64 = add(t%d,1)" index previous) (range first last)
 | None, Some values, _ ->
  let callee = group values 1 and count = parse32 (group values 2) and initial = tempIndex (group values 3) and first = tempIndex (group values 4) in
  let elementType = if group values 5 = "Body" then "Body" else "Int64" in
  let aliases = if group values 6 = "" then 0 else parse32 (group values 6) in
  if count < 1 || count > 32 then problem "project_chain requires 1..32 sites";
  if aliases > 4 then problem "project_chain allows at most four aliases per projection";
  let stride = add 3 (mul 2 aliases) in
  List.concat_map (fun index -> let result = add first (mul index stride) in let input = if index = 0 then initial else add (add first (mul (index - 1) stride)) (1 + aliases) in
   Printf.sprintf "let t%d:Tuple<%s,%s> = call %s(t%d)" result elementType elementType callee input ::
   List.concat_map (fun projection -> let projected = add result (1 + mul projection (1 + aliases)) in
    Printf.sprintf "let t%d:%s = get(t%d,%d)" projected elementType result projection :: List.map (fun index -> let alias = add projected index in Printf.sprintf "let t%d:%s = typed(t%d)" alias elementType (add alias (-1))) (range 1 aliases)) [0;1]) (range 0 (count - 1)) @
   [Printf.sprintf "return t%d" (add (add first (mul (count - 1) stride)) (1 + aliases))]
 | None, None, Some values ->
  let callee = group values 1 and count = parse32 (group values 2) and initial = tempIndex (group values 3) and first = tempIndex (group values 4) in
  if count < 1 || count > 32 then problem "call_chain requires 1..32 sites";
  List.map (fun index -> let result = add first index in let input = if index = 0 then initial else add result (-1) in Printf.sprintf "let t%d:Int64 = call %s(t%d)" result callee input) (range 0 (count - 1)) @ [Printf.sprintf "return t%d" (add first (count - 1))]
 | None, None, None -> [line]
let parseBody lines =
 let lines = Array.of_list (List.concat_map expandLine lines) in
 let rec expression index =
  if index >= Array.length lines then problem "body must end with return";
  let line = lines.(index) in
  match matched {|^let\s+(t\d+)\s*:\s*(\S+)\s*=\s*(.+)$|} line with
  | Some values -> let next, after = expression (index + 1) in let valueType = typ (group values 2) in Bind (temp (group values 1), valueType, operation valueType (group values 3), next), after
  | None when String.starts_with ~prefix:"return " line -> Return (atom (String.sub line 7 (String.length line - 7))), index + 1
  | None when String.starts_with ~prefix:"if " line ->
    let yes, afterYes = expression (index + 1) in
    if afterYes >= Array.length lines || lines.(afterYes) <> "else" then problem "if requires else after its first return";
    let no, afterNo = expression (afterYes + 1) in
    if afterNo >= Array.length lines || lines.(afterNo) <> "endif" then problem "if requires endif after its second return";
    Branch (atom (String.sub line 3 (String.length line - 3)), yes, no), afterNo + 1
  | None -> problem ("expected let, if, or return; got '" ^ line ^ "'") in
 let result, consumed = expression 0 in
 if consumed <> Array.length lines then problem ("unexpected line after body: '" ^ lines.(consumed) ^ "'"); result
let rec toANF = function Return value -> A.Return value | Bind (id, _, operation, rest) -> A.Let (id, operation, toANF rest) | Branch (condition, yes, no) -> A.If (condition, toANF yes, toANF no)
type lowerState = {nextLabel : int; blocks : S.block S.LabelMap.t; types : AST.semanticType M.t}
let addBlock label operations terminator state = {state with blocks = S.LabelMap.add label {S.label; parameters = []; operations = List.rev operations; terminator} state.blocks}
let rec toSSA label operations expr state = match expr with
 | Return value -> addBlock label operations (S.Return value) state
 | Bind (id, valueType, operation, rest) ->
  if M.mem id state.types then (let A.TempId index = id in problem ("TempId " ^ string_of_int index ^ " is defined twice"));
  toSSA label ((id, operation) :: operations) rest {state with types = M.add id valueType state.types}
 | Branch (condition, yes, no) ->
  let yesLabel = S.Label state.nextLabel and noLabel = S.Label (add state.nextLabel 1) in
  let state = addBlock label operations (S.Branch (condition, yesLabel, noLabel)) {state with nextLabel = add state.nextLabel 2} in
  let state = toSSA yesLabel [] yes state in toSSA noLabel [] no state
let parseFunction isExternal = function
 | [] -> problem "empty FUNCTION section"
 | header :: bodyLines ->
  let values = match matched {|^([A-Za-z_][A-Za-z_0-9.]*)\((.*)\)\s*->\s*(\S+)$|} header with Some values -> values | None -> problem ("invalid function header '" ^ header ^ "'") in
  let name = group values 1 in
  let parameters = if HostText.trim (group values 2) = "" then [] else List.map (fun text ->
   let values = match matched {|^(t\d+)\s*:\s*(\S+)$|} (HostText.trim text) with Some values -> values | None -> problem ("invalid parameter '" ^ text ^ "'") in {A.id = temp (group values 1); typ = typ (group values 2)}) (String.split_on_char ',' (group values 2)) in
  let returnType = typ (group values 3) and body = parseBody bodyLines in
  let id = TestIds.functionIdForName name in
  let source : A.functionDef = {A.id; name; typedParams = parameters; returnType; returnOwnership = A.OwnedReturn; body = toANF body} in
  let lowered = toSSA (S.Label 0) [] body {nextLabel = 1; blocks = S.LabelMap.empty; types = M.of_list (List.map (fun (param : A.typedParam) -> param.A.id, param.A.typ) parameters)} in
  let ssa : S.functionDef = {S.id; name; typedParams = parameters; returnType; returnOwnership = A.OwnedReturn; entry = S.Label 0; blocks = lowered.blocks; freshValueTypes = lowered.types} in {source; ssa; isExternal}
let operations (func : S.functionDef) = S.LabelMap.bindings func.S.blocks |> List.concat_map (fun (_, body) -> body.S.operations)
let parseAssertion text =
 let count predicate func = List.fold_left (fun total (_, operation) -> add total (if predicate operation then 1 else 0)) 0 (operations func) in
 match matched {|^calls\s+([A-Za-z_][A-Za-z_0-9.]*)\s*(=|>=)\s*(\d+)$|} text,
       matched {|^ops\s+(add|mul|bitand|mod|not|tuple_get|record_get|closure_alloc|closure_call|option_alloc|tuple_alloc|body_alloc)\s*(=|>=)\s*(\d+)(\s+after escape)?$|} text,
       matched {|^blocks\s*(=|>=)\s*(\d+)$|} text with
 | Some values, _, _ ->
  let callee = TestIds.functionIdForName (group values 1) in {text; afterEscape = false; minimum = group values 2 = ">="; expected = parse32 (group values 3); measure = count (function A.Call (id, _) when id = callee -> true | _ -> false)}
 | None, Some values, _ ->
  let predicate = match group values 1 with
   | "mul" -> (function A.Prim (A.Mul, _, _) -> true | _ -> false) | "add" -> (function A.Prim (A.Add, _, _) -> true | _ -> false)
   | "bitand" -> (function A.Prim (A.BitAnd, _, _) -> true | _ -> false) | "mod" -> (function A.Prim (A.Mod, _, _) -> true | _ -> false) | "not" -> (function A.UnaryPrim (A.Not, _) -> true | _ -> false)
   | "tuple_get" -> (function A.TupleGet _ -> true | _ -> false) | "record_get" -> (function A.RecordGet _ -> true | _ -> false)
   | "closure_alloc" -> (function A.ClosureAlloc _ -> true | _ -> false) | "closure_call" -> (function A.ClosureCall _ -> true | _ -> false) | "tuple_alloc" -> (function A.TupleAlloc _ -> true | _ -> false)
   | "body_alloc" -> (function A.RecordAlloc (descriptor, _) when descriptor.A.valueType = typ "Body" -> true | A.RecordClone (descriptor, _, _) when descriptor.A.valueType = typ "Body" -> true | _ -> false)
   | _ -> (function A.RecordAlloc (descriptor, _) when descriptor.A.valueType = typ "Option<Int64>" || descriptor.A.valueType = typ "Option<Float>" -> true | _ -> false) in
  {text; afterEscape = group values 4 <> ""; minimum = group values 2 = ">="; expected = parse32 (group values 3); measure = count predicate}
 | None, None, Some values -> {text; afterEscape = false; minimum = group values 1 = ">="; expected = parse32 (group values 2); measure = fun (func : S.functionDef) -> S.LabelMap.cardinal func.S.blocks}
 | None, None, None -> problem ("invalid expectation '" ^ text ^ "'")
let emptyCase = {rawName = ""; rawFunctions = []; externals = []; expected = []; optimize = false; skip = false}
let parseSections content =
 let flushSection section lines current = let values = List.rev lines in match section with
  | "NAME" -> (match values with [name] -> {current with rawName = name} | _ -> problem "NAME requires one line")
  | "FUNCTION" -> {current with rawFunctions = values :: current.rawFunctions} | "EXTERNAL-FUNCTION" -> {current with externals = values :: current.externals}
  | "EXPECT" -> {current with expected = current.expected @ values} | "OPTIMIZE-SSA" -> {current with optimize = true} | "NO-INLINE" -> {current with skip = true} | "" -> current | other -> problem ("unknown section " ^ other) in
 let flushCase current cases = if current.rawName = "" then current, cases else (
  if current.rawFunctions = [] || current.expected = [] then problem ("case '" ^ current.rawName ^ "' requires FUNCTION and EXPECT sections");
  emptyCase, {current with rawFunctions = List.rev current.rawFunctions; externals = List.rev current.externals} :: cases) in
 let section, lines, current, cases = List.fold_left (fun (section, lines, current, cases) raw ->
  let line = HostText.trim raw in if line = "" || String.starts_with ~prefix:"#" line then section, lines, current, cases else
  match matched {|^---(NAME|FUNCTION|EXTERNAL-FUNCTION|EXPECT|OPTIMIZE-SSA|NO-INLINE)---$|} line with
  | Some values ->
    let current = flushSection section lines current in let marker = group values 1 in
    let current, cases = if marker = "NAME" then flushCase current cases else (if current.rawName = "" then problem "a case must start with NAME"; current, cases) in marker, [], current, cases
  | None when section = "" -> problem ("content outside a section: '" ^ line ^ "'")
  | None -> section, line :: lines, current, cases) ("", [], emptyCase, []) (String.split_on_char '\n' (Common.normalizeLineEndings content)) in
 let _, cases = flushCase (flushSection section lines current) cases in if cases = [] then problem "fixture file contains no cases"; List.rev cases
let parseCase raw =
 let functions = List.map (parseFunction true) raw.externals @ List.map (parseFunction false) raw.rawFunctions in
 let names = List.map (fun fixture -> fixture.source.A.id) functions in
 if List.length names <> Functions.cardinal (Functions.of_list names) then problem ("case '" ^ raw.rawName ^ "' defines a function twice");
 {name = raw.rawName; functions; assertions = List.map parseAssertion raw.expected; optimizeSSA = raw.optimize; skipInlining = raw.skip}
let runCase case =
 let externals, locals = List.partition (fun fixture -> fixture.isExternal) case.functions in
 let externalSources = List.map (fun fixture -> fixture.source) externals in
 let context : ANFConstants.optimizeContext = {ANFConstants.typeReg = Names.singleton "Body" bodyDescriptor.A.fields; recordTypeParams = Names.singleton "Body" []; sumShapeReg = Names.empty; functionNames = FunctionIdMap.ofList (List.map (fun fixture -> fixture.source.A.id, fixture.source.A.name) case.functions); functionIds = Names.of_list (List.map (fun fixture -> fixture.source.A.name, fixture.source.A.id) case.functions)} in
 let localSSA = List.map (fun fixture -> fixture.ssa) locals |> List.map (fun body -> if case.optimizeSSA then SSAOptimization.optimizeFunction context ANFConstants.defaultOptimizeOptions body else body) in
 let last values = List.hd (List.rev values) in
 let result = if case.skipInlining then last localSSA else SSAInlining.inlineProgramWithExternalCandidatesAndExclusions InliningCommon.defaultConfig (InliningCommon.buildExternalCandidateInfoMap InliningCommon.defaultConfig externalSources) (List.map (fun fixture -> fixture.ssa) externals) Functions.empty (List.map (fun fixture -> fixture.source) locals) localSSA |> last in
 let escaped = lazy (SSAEscapeAnalysis.optimizeFunction Names.empty Names.empty result) in
 match List.find_map (fun assertion -> let current = if assertion.afterEscape then Lazy.force escaped else result in let actual = assertion.measure current in if (if assertion.minimum then actual >= assertion.expected else actual = assertion.expected) then None else Some (assertion.text ^ ": got " ^ string_of_int actual)) case.assertions with None -> Ok () | Some error -> Error error
let testsFromFile path =
 try
  let channel = open_in_bin path in let content = Fun.protect ~finally:(fun () -> close_in channel) (fun () -> really_input_string channel (in_channel_length channel)) in
  let cases = List.map parseCase (parseSections content) in
  let names = List.map (fun case -> case.name) cases in
  if List.length names <> StringOrder.Set.cardinal (StringOrder.Set.of_list names) then problem "fixture file defines the same case name twice";
  List.map (fun case -> case.name, (fun () -> runCase case)) cases
 with error -> ["SSA inlining fixture format", (fun () -> Error ((match error with Failure message | Invalid_argument message | Sys_error message -> message | _ -> Printexc.to_string error)))]
