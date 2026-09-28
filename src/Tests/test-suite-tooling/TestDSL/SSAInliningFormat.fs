// Text fixtures for the production SSA inliner. Each function body is parsed
// once and lowered to the ANF analysis input and the SSA transformation input.
module TestDSL.SSAInliningFormat

open System
open System.IO
open System.Text.RegularExpressions
open ANF

type private Expr =
    | Return of Atom
    | Bind of TempId * AST.SemanticType * CExpr * Expr
    | Branch of Atom * Expr * Expr

type private Fixture = { Source: ANF.Function; SSA: SSAANF.Function; IsExternal: bool }
type private Assertion = { Text: string; AfterEscape: bool; Measure: SSAANF.Function -> int; Minimum: bool; Expected: int }
type private Case = {
    Name: string
    Functions: Fixture list
    Assertions: Assertion list
    OptimizeSSA: bool
    SkipInlining: bool
}
type private RawCase = {
    Name: string
    Functions: string list list
    ExternalFunctions: string list list
    Expected: string list
    OptimizeSSA: bool
    SkipInlining: bool
}

let private problem message = Crash.crash $"SSA inlining fixture: {message}"
let private temp (text: string) =
    let match_ = Regex.Match(text.Trim(), @"^t(\d+)$")
    if not match_.Success then problem $"expected a temp ID, got '{text}'"
    TempId (Int32.Parse match_.Groups.[1].Value)

let private atom (text: string) =
    let text = text.Trim()
    if text = "true" then BoolLiteral true
    elif text = "false" then BoolLiteral false
    elif Regex.IsMatch(text, @"^t\d+$") then Var (temp text)
    else
        match Int64.TryParse text with
        | true, value -> IntLiteral (Int64 value)
        | _ when Regex.IsMatch(text, @"^-?\d+\.\d+$") ->
            FloatLiteral (Double.Parse(text, Globalization.CultureInfo.InvariantCulture))
        | _ -> problem $"expected a literal or temp ID, got '{text}'"

let rec private typ (text: string) =
    match text.Trim() with
    | "Int64" -> AST.TInt64
    | "Float" -> AST.TFloat64
    | "Bool" -> AST.TBool
    | "Unit" -> AST.TUnit
    | "FnInt64" -> AST.TFunction ([AST.TInt64], AST.TInt64)
    | "Body" -> AST.TRecord ("Body", [])
    | "Option<Int64>" -> AST.TSum ("Darklang.Stdlib.Option.Option", [AST.TInt64])
    | "Option<Float>" -> AST.TSum ("Darklang.Stdlib.Option.Option", [AST.TFloat64])
    | "Tuple<Int64,Int64>" -> AST.TTuple [AST.TInt64; AST.TInt64]
    | "Tuple<Body,Body>" -> AST.TTuple [typ "Body"; typ "Body"]
    | "Tuple<Body,Body,Bool>" -> AST.TTuple [typ "Body"; typ "Body"; AST.TBool]
    | other -> problem $"unsupported type '{other}'"

let private optionDescriptor: RecordDescriptor =
    { SourceTypeName = "Darklang.Stdlib.Option.Option"
      RuntimeTypeName = "Darklang.Stdlib.Option.Option"
      TypeArgs = [AST.TInt64]
      Fields = ["tag", AST.TInt64; "payload", AST.TInt64]
      ValueType = typ "Option<Int64>" }

let private floatOptionDescriptor: RecordDescriptor =
    { optionDescriptor with
        TypeArgs = [AST.TFloat64]
        Fields = ["tag", AST.TInt64; "payload", AST.TFloat64]
        ValueType = typ "Option<Float>" }

let private bodyDescriptor: RecordDescriptor =
    { SourceTypeName = "Body"
      RuntimeTypeName = "Body"
      TypeArgs = []
      Fields = ["x", AST.TInt64; "y", AST.TInt64]
      ValueType = typ "Body" }

let private operands text =
    if String.IsNullOrWhiteSpace text then []
    else text.Split(',') |> Array.map atom |> Array.toList

let private operation valueType text =
    let call = Regex.Match(text, @"^call\s+([A-Za-z_][A-Za-z_0-9.]*)\((.*)\)$")
    let closure = Regex.Match(text, @"^closure\s+([A-Za-z_][A-Za-z_0-9.]*)$")
    if closure.Success then
        ClosureAlloc (TestIds.functionIdForName closure.Groups.[1].Value, [])
    elif call.Success then
        Call (TestIds.functionIdForName call.Groups.[1].Value, operands call.Groups.[2].Value)
    else
        let matched = Regex.Match(text, @"^([a-z_][a-z_0-9]*)\((.*)\)$")
        if not matched.Success then problem $"invalid operation '{text}'"
        let name = matched.Groups.[1].Value
        let args = operands matched.Groups.[2].Value
        let binary op =
            match args with
            | [left; right] -> Prim (op, left, right)
            | _ -> problem $"{name} requires two operands"
        match name, args with
        | "add", _ -> binary Add
        | "mul", _ -> binary Mul
        | "div", _ -> binary Div
        | "mod", _ -> binary Mod
        | "eq", _ -> binary Eq
        | "gte", _ -> binary Gte
        | "lt", _ -> binary Lt
        | "bitand", _ -> binary BitAnd
        | "bitxor", _ -> binary BitXor
        | "tuple", fields when List.length fields = 2 -> TupleAlloc fields
        | "tuple3", fields when List.length fields = 3 -> TupleAlloc fields
        | "get", [source; IntLiteral (Int64 index)] -> TupleGet (source, int index)
        | "copy", [source] -> Atom source
        | "closure_call", closureValue :: arguments ->
            ClosureCall (closureValue, arguments)
        | "typed", [source] -> TypedAtom (source, valueType)
        | "body", fields when List.length fields = 2 -> RecordAlloc (bodyDescriptor, fields)
        | "field", [source; IntLiteral (Int64 index)] -> RecordGet (bodyDescriptor, source, int index)
        | "some", [value] -> RecordAlloc (optionDescriptor, [atom "0"; value])
        | "none", [] -> RecordAlloc (optionDescriptor, [atom "1"; atom "0"])
        | "payload", [source] -> RecordGet (optionDescriptor, source, 1)
        | "some_float", [value] -> RecordAlloc (floatOptionDescriptor, [atom "0"; value])
        | "none_float", [] -> RecordAlloc (floatOptionDescriptor, [atom "1"; atom "0.0"])
        | "payload_float", [source] -> RecordGet (floatOptionDescriptor, source, 1)
        | _ -> problem $"unsupported operation or operands '{text}'"

// Compact repetition covers the hot call patterns without copying dozens of
// near-identical lets into the fixture. Expansion produces ordinary DSL lines.
let private expandLine line =
    let additions = Regex.Match(line, @"^repeat_add\s+(t\d+)\.\.(t\d+)\s+from\s+(t\d+)$")
    let projections =
        Regex.Match(line, @"^project_chain\s+([A-Za-z_][A-Za-z_0-9]*)\s+(\d+)\s+from\s+(t\d+)\s+at\s+(t\d+)(?:\s+of\s+(Int64|Body))?(?:\s+aliases\s+(\d+))?$")
    let calls =
        Regex.Match(line, @"^call_chain\s+([A-Za-z_][A-Za-z_0-9.]*)\s+(\d+)\s+from\s+(t\d+)\s+at\s+(t\d+)$")
    if additions.Success then
        let (TempId first) = temp additions.Groups.[1].Value
        let (TempId last) = temp additions.Groups.[2].Value
        let (TempId initial) = temp additions.Groups.[3].Value
        if last < first || last - first > 64 then problem "repeat_add requires 1..65 bindings"
        [for index in first..last ->
            let previous = if index = first then initial else index - 1
            $"let t{index}:Int64 = add(t{previous},1)"]
    elif projections.Success then
        let callee = projections.Groups.[1].Value
        let count = Int32.Parse projections.Groups.[2].Value
        let (TempId initial) = temp projections.Groups.[3].Value
        let (TempId first) = temp projections.Groups.[4].Value
        let elementType =
            if projections.Groups.[5].Value = "Body" then "Body" else "Int64"
        let aliases =
            if projections.Groups.[6].Success then Int32.Parse projections.Groups.[6].Value
            else 0
        let tupleType = $"Tuple<{elementType},{elementType}>"
        if count < 1 || count > 32 then problem "project_chain requires 1..32 sites"
        if aliases > 4 then problem "project_chain allows at most four aliases per projection"
        let stride = 3 + 2 * aliases
        [ for index in 0..count-1 do
            let result = first + index * stride
            let input =
                if index = 0 then initial
                else first + (index - 1) * stride + 1 + aliases
            yield $"let t{result}:{tupleType} = call {callee}(t{input})"
            for projectionIndex in 0..1 do
                let projected = result + 1 + projectionIndex * (1 + aliases)
                yield $"let t{projected}:{elementType} = get(t{result},{projectionIndex})"
                for aliasIndex in 1..aliases do
                    let alias = projected + aliasIndex
                    yield $"let t{alias}:{elementType} = typed(t{alias - 1})"
          yield $"return t{first + (count - 1) * stride + 1 + aliases}" ]
    elif calls.Success then
        let callee = calls.Groups.[1].Value
        let count = Int32.Parse calls.Groups.[2].Value
        let (TempId initial) = temp calls.Groups.[3].Value
        let (TempId first) = temp calls.Groups.[4].Value
        if count < 1 || count > 32 then problem "call_chain requires 1..32 sites"
        [ for index in 0..count-1 do
            let result = first + index
            let input = if index = 0 then initial else result - 1
            yield $"let t{result}:Int64 = call {callee}(t{input})"
          yield $"return t{first + count - 1}" ]
    else [line]

let private parseBody (lines: string list) =
    let lines = lines |> List.collect expandLine |> List.toArray
    let rec expression index =
        if index >= lines.Length then problem "body must end with return"
        let line = lines.[index]
        let binding = Regex.Match(line, @"^let\s+(t\d+)\s*:\s*(\S+)\s*=\s*(.+)$")
        if binding.Success then
            let next, after = expression (index + 1)
            Bind (temp binding.Groups.[1].Value,
                  typ binding.Groups.[2].Value,
                  operation (typ binding.Groups.[2].Value) binding.Groups.[3].Value, next), after
        elif line.StartsWith("return ", StringComparison.Ordinal) then
            Return (atom (line.Substring(7))), index + 1
        elif line.StartsWith("if ", StringComparison.Ordinal) then
            let yes, afterYes = expression (index + 1)
            if afterYes >= lines.Length || lines.[afterYes] <> "else" then
                problem "if requires else after its first return"
            let no, afterNo = expression (afterYes + 1)
            if afterNo >= lines.Length || lines.[afterNo] <> "endif" then
                problem "if requires endif after its second return"
            Branch (atom (line.Substring(3)), yes, no), afterNo + 1
        else problem $"expected let, if, or return; got '{line}'"
    let result, consumed = expression 0
    if consumed <> lines.Length then problem $"unexpected line after body: '{lines.[consumed]}'"
    result

let rec private toANF = function
    | Return value -> ANF.Return value
    | Bind (id, _, operation, next) -> Let (id, operation, toANF next)
    | Branch (condition, yes, no) -> If (condition, toANF yes, toANF no)

type private LowerState = {
    NextLabel: int
    Blocks: Map<SSAANF.Label, SSAANF.Block>
    Types: Map<TempId, AST.SemanticType>
}

let private addBlock label operations terminator state =
    let body: SSAANF.Block =
        { Label = label; Parameters = []; Operations = List.rev operations; Terminator = terminator }
    { state with Blocks = Map.add label body state.Blocks }

let rec private toSSA label operations expr state =
    match expr with
    | Return value -> addBlock label operations (SSAANF.Return value) state
    | Bind (id, valueType, operation, next) ->
        if Map.containsKey id state.Types then problem $"{id} is defined twice"
        let state = { state with Types = Map.add id valueType state.Types }
        toSSA label ((id, operation) :: operations) next state
    | Branch (condition, yes, no) ->
        let yesLabel = SSAANF.Label state.NextLabel
        let noLabel = SSAANF.Label (state.NextLabel + 1)
        let state = { state with NextLabel = state.NextLabel + 2 }
        let state = addBlock label operations (SSAANF.Branch (condition, yesLabel, noLabel)) state
        let state = toSSA yesLabel [] yes state
        toSSA noLabel [] no state

let private parseFunction isExternal (lines: string list) =
    match lines with
    | [] -> problem "empty FUNCTION section"
    | header :: bodyLines ->
        let headerMatch =
            Regex.Match(header, @"^([A-Za-z_][A-Za-z_0-9.]*)\((.*)\)\s*->\s*(\S+)$")
        if not headerMatch.Success then problem $"invalid function header '{header}'"
        let name = headerMatch.Groups.[1].Value
        let parameters =
            if String.IsNullOrWhiteSpace headerMatch.Groups.[2].Value then []
            else
                headerMatch.Groups.[2].Value.Split(',')
                |> Array.map (fun parameterText ->
                    let matched = Regex.Match(parameterText.Trim(), @"^(t\d+)\s*:\s*(\S+)$")
                    if not matched.Success then problem $"invalid parameter '{parameterText}'"
                    { Id = temp matched.Groups.[1].Value; Type = typ matched.Groups.[2].Value })
                |> Array.toList
        let returnType = typ headerMatch.Groups.[3].Value
        let body = parseBody bodyLines
        let id = TestIds.functionIdForName name
        let source: ANF.Function =
            { Id = id; Name = name; TypedParams = parameters; ReturnType = returnType
              ReturnOwnership = OwnedReturn; Body = toANF body }
        let initial =
            { NextLabel = 1
              Blocks = Map.empty
              Types = parameters |> List.map (fun parameter -> parameter.Id, parameter.Type) |> Map.ofList }
        let lowered = toSSA (SSAANF.Label 0) [] body initial
        let ssa: SSAANF.Function =
            { Id = id; Name = name; TypedParams = parameters; ReturnType = returnType
              ReturnOwnership = OwnedReturn; Entry = SSAANF.Label 0
              Blocks = lowered.Blocks; FreshValueTypes = lowered.Types }
        { Source = source; SSA = ssa; IsExternal = isExternal }

let private operations (func: SSAANF.Function) =
    func.Blocks |> Map.toList |> List.collect (fun (_, body) -> body.Operations)

let private parseAssertion text =
    let calls = Regex.Match(text, @"^calls\s+([A-Za-z_][A-Za-z_0-9.]*)\s*(=|>=)\s*(\d+)$")
    let ops = Regex.Match(text, @"^ops\s+(add|mul|bitand|mod|not|tuple_get|record_get|closure_alloc|closure_call|option_alloc|tuple_alloc|body_alloc)\s*(=|>=)\s*(\d+)(\s+after escape)?$")
    let blocks = Regex.Match(text, @"^blocks\s*(=|>=)\s*(\d+)$")
    let count predicate func =
        operations func |> List.sumBy (fun (_, operation) -> if predicate operation then 1 else 0)
    if calls.Success then
        let callee = TestIds.functionIdForName calls.Groups.[1].Value
        { Text = text; AfterEscape = false; Minimum = calls.Groups.[2].Value = ">="
          Expected = Int32.Parse calls.Groups.[3].Value
          Measure = count (function Call (name, _) when name = callee -> true | _ -> false) }
    elif ops.Success then
        let predicate =
            match ops.Groups.[1].Value with
            | "mul" -> function Prim (Mul, _, _) -> true | _ -> false
            | "add" -> function Prim (Add, _, _) -> true | _ -> false
            | "bitand" -> function Prim (BitAnd, _, _) -> true | _ -> false
            | "mod" -> function Prim (Mod, _, _) -> true | _ -> false
            | "not" -> function UnaryPrim (Not, _) -> true | _ -> false
            | "tuple_get" -> function TupleGet _ -> true | _ -> false
            | "record_get" -> function RecordGet _ -> true | _ -> false
            | "closure_alloc" -> function ClosureAlloc _ -> true | _ -> false
            | "closure_call" -> function ClosureCall _ -> true | _ -> false
            | "tuple_alloc" -> function TupleAlloc _ -> true | _ -> false
            | "body_alloc" -> function
                | RecordAlloc (descriptor, _) | RecordClone (descriptor, _, _)
                    when descriptor.ValueType = typ "Body" -> true
                | _ -> false
            | _ -> function
                | RecordAlloc (descriptor, _) when
                    descriptor.ValueType = typ "Option<Int64>"
                    || descriptor.ValueType = typ "Option<Float>" -> true
                | _ -> false
        { Text = text; AfterEscape = ops.Groups.[4].Success
          Minimum = ops.Groups.[2].Value = ">="
          Expected = Int32.Parse ops.Groups.[3].Value
          Measure = count predicate }
    elif blocks.Success then
        { Text = text; AfterEscape = false; Minimum = blocks.Groups.[1].Value = ">="
          Expected = Int32.Parse blocks.Groups.[2].Value
          Measure = fun func -> Map.count func.Blocks }
    else problem $"invalid expectation '{text}'"

let private parseSections (content: string) =
    let mutable section = ""
    let mutable lines: string list = []
    let mutable name = ""
    let mutable functions: string list list = []
    let mutable externalFunctions: string list list = []
    let mutable expected: string list = []
    let mutable optimizeSSA = false
    let mutable skipInlining = false
    let mutable cases: RawCase list = []
    let flushSection () =
        let values = List.rev lines
        match section with
        | "NAME" ->
            if List.length values <> 1 then problem "NAME requires one line"
            name <- List.head values
        | "FUNCTION" -> functions <- values :: functions
        | "EXTERNAL-FUNCTION" -> externalFunctions <- values :: externalFunctions
        | "EXPECT" -> expected <- expected @ values
        | "OPTIMIZE-SSA" -> optimizeSSA <- true
        | "NO-INLINE" -> skipInlining <- true
        | "" -> ()
        | other -> problem $"unknown section {other}"
        lines <- []
    let flushCase () =
        if name <> "" then
            if List.isEmpty functions || List.isEmpty expected then
                problem $"case '{name}' requires FUNCTION and EXPECT sections"
            cases <-
                { Name = name
                  Functions = List.rev functions
                  ExternalFunctions = List.rev externalFunctions
                  Expected = expected
                  OptimizeSSA = optimizeSSA
                  SkipInlining = skipInlining } :: cases
            name <- ""; functions <- []; externalFunctions <- []; expected <- []
            optimizeSSA <- false; skipInlining <- false
    for rawLine in content.Replace("\r\n", "\n").Split('\n') do
        let line = rawLine.Trim()
        if line <> "" && not (line.StartsWith("#", StringComparison.Ordinal)) then
            let marker = Regex.Match(line, @"^---(NAME|FUNCTION|EXTERNAL-FUNCTION|EXPECT|OPTIMIZE-SSA|NO-INLINE)---$")
            if marker.Success then
                flushSection ()
                if marker.Groups.[1].Value = "NAME" then flushCase ()
                elif name = "" then problem "a case must start with NAME"
                section <- marker.Groups.[1].Value
            elif section = "" then problem $"content outside a section: '{line}'"
            else lines <- line :: lines
    flushSection ()
    flushCase ()
    if List.isEmpty cases then problem "fixture file contains no cases"
    List.rev cases

let private parseCase (raw: RawCase) : Case =
    let functions =
        (raw.ExternalFunctions |> List.map (parseFunction true))
        @ (raw.Functions |> List.map (parseFunction false))
    let names = functions |> List.map (fun fixture -> fixture.Source.Id)
    if List.length names <> (names |> Set.ofList |> Set.count) then
        problem $"case '{raw.Name}' defines a function twice"
    { Name = raw.Name
      Functions = functions
      Assertions = List.map parseAssertion raw.Expected
      OptimizeSSA = raw.OptimizeSSA
      SkipInlining = raw.SkipInlining }

let private runCase (case: Case) : Result<unit, string> =
    let functions = case.Functions
    let externals, locals = functions |> List.partition (fun fixture -> fixture.IsExternal)
    let externalSources = List.map (fun fixture -> fixture.Source) externals
    let optimizeContext: ANFConstants.OptimizeContext =
        { TypeReg = Map.ofList ["Body", bodyDescriptor.Fields]
          RecordTypeParams = Map.ofList ["Body", []]
          SumShapeReg = Map.empty
          FunctionNames =
            functions
            |> List.map (fun fixture -> fixture.Source.Id, fixture.Source.Name)
            |> Map.ofList }
    let localSSA =
        locals
        |> List.map (fun fixture -> fixture.SSA)
        |> fun bodies ->
            if case.OptimizeSSA then
                List.map
                    (SSAOptimization.optimizeFunction
                        optimizeContext ANFConstants.defaultOptimizeOptions)
                    bodies
            else bodies
    let result =
        if case.SkipInlining then List.last localSSA
        else
            SSAInlining.inlineProgramWithExternalCandidatesAndExclusions
                InliningCommon.defaultConfig
                (InliningCommon.buildExternalCandidateInfoMap InliningCommon.defaultConfig externalSources)
                (List.map (fun fixture -> fixture.SSA) externals)
                Set.empty
                (List.map (fun fixture -> fixture.Source) locals)
                localSSA
            |> List.last
    let escaped = lazy (SSAEscapeAnalysis.optimizeFunction Map.empty Map.empty result)
    case.Assertions
    |> List.tryPick (fun assertion ->
        let current = if assertion.AfterEscape then escaped.Value else result
        let actual = assertion.Measure current
        let passed = if assertion.Minimum then actual >= assertion.Expected else actual = assertion.Expected
        if passed then None
        else Some $"{assertion.Text}: got {actual}")
    |> function None -> Ok () | Some error -> Error error

let testsFromFile path =
    try
        let cases = File.ReadAllText path |> parseSections |> List.map parseCase
        let names = cases |> List.map (fun case -> case.Name)
        if List.length names <> (names |> Set.ofList |> Set.count) then
            problem "fixture file defines the same case name twice"
        cases |> List.map (fun case -> case.Name, (fun () -> runCase case))
    with error ->
        ["SSA inlining fixture format", (fun () -> Error error.Message)]
