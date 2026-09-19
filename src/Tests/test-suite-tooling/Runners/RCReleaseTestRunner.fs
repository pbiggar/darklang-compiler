// RCReleaseTestRunner.fs - Executes semantic managed-graph release fixtures.
//
// Builds canonical LIR heap graphs and requires final release to leave no leaks.

module TestDSL.RCReleaseTestRunner

open System.IO
open TestDSL.RCReleaseFormat
open TestDSL.LIRExecutionFormat

type private TypedShape = {
    Shape: ManagedShape
    Type: AST.Type
    Path: string
    Children: TypedShape list
}

type private BuildState = {
    AvailableRegisters: LIR.PhysReg list
    Instructions: LIR.Instr list
}

let private fixtureRegisters =
    [ LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7; LIR.X8; LIR.X9
      LIR.X10; LIR.X11; LIR.X12; LIR.X13; LIR.X14; LIR.X15; LIR.X16; LIR.X17
      LIR.X19; LIR.X20; LIR.X21; LIR.X22; LIR.X23 ]

let private indexedChildren path shapes =
    shapes |> List.mapi (fun index shape -> $"{path}_{index}", shape)

let rec private describeShape path shape =
    let describeChildren shapes =
        indexedChildren path shapes
        |> List.map (fun (childPath, child) -> describeShape childPath child)

    match shape with
    | Int64Value -> { Shape = shape; Type = AST.TInt64; Path = path; Children = [] }
    | EnumValue -> { Shape = shape; Type = AST.TSum ($"RCFixtureEnum_{path}", []); Path = path; Children = [] }
    | DynamicString
    | LiteralString -> { Shape = shape; Type = AST.TString; Path = path; Children = [] }
    | DynamicBlob -> { Shape = shape; Type = AST.TBlob; Path = path; Children = [] }
    | ListValue element ->
        let child = describeShape $"{path}_item" element
        { Shape = shape; Type = AST.TList child.Type; Path = path; Children = [ child ] }
    | DictValue (key, value) ->
        let keyShape = describeShape $"{path}_key" key
        let valueShape = describeShape $"{path}_value" value
        { Shape = shape
          Type = AST.TDict (keyShape.Type, valueShape.Type)
          Path = path
          Children = [ keyShape; valueShape ] }
    | TupleValue fields ->
        let children = describeChildren fields
        { Shape = shape; Type = AST.TTuple (children |> List.map _.Type); Path = path; Children = children }
    | RecordValue fields ->
        let children = describeChildren fields
        { Shape = shape
          Type = AST.TRecord ($"RCFixtureRecord_{path}", [])
          Path = path
          Children = children }
    | SumValue payload ->
        let child = describeShape $"{path}_payload" payload
        { Shape = shape
          Type = AST.TSum ($"RCFixtureSum_{path}", [])
          Path = path
          Children = [ child ] }
    | ClosureValue captures ->
        let children = describeChildren captures
        { Shape = shape
          Type = AST.TFunction ([ AST.TInt64 ], AST.TInt64)
          Path = path
          Children = children }

let rec private collectRecords (typed: TypedShape) : LIR.RecordRegistry =
    let nested =
        typed.Children
        |> List.map collectRecords
        |> List.fold
            (fun records childRecords ->
                Map.fold (fun current name fields -> Map.add name fields current) records childRecords)
            Map.empty

    match typed.Shape with
    | RecordValue _ ->
        let name =
            match typed.Type with
            | AST.TRecord (name, _) -> name
            | _ -> Crash.crash "Described record fixture had a non-record type"
        let fields =
            typed.Children
            |> List.mapi (fun index child -> $"field{index}", child.Type)
        Map.add name fields nested
    | _ -> nested

let rec private collectVariants (typed: TypedShape) : LIR.VariantRegistry =
    let nested =
        typed.Children
        |> List.map collectVariants
        |> List.fold
            (fun variants childVariants ->
                Map.fold (fun current name cases -> Map.add name cases current) variants childVariants)
            Map.empty

    match typed.Shape with
    | EnumValue ->
        let name =
            match typed.Type with
            | AST.TSum (name, _) -> name
            | _ -> Crash.crash "Described enum fixture had a non-sum type"
        Map.add
            name
            { TypeParams = []
              Variants = [{ Name = $"{name}_case"; Tag = 0; Payload = None }] }
            nested
    | SumValue _ ->
        let name =
            match typed.Type with
            | AST.TSum (name, _) -> name
            | _ -> Crash.crash "Described sum fixture had a non-sum type"
        match typed.Children with
        | [ payload ] ->
            Map.add
                name
                { TypeParams = []
                  Variants = [{ Name = $"{name}_payload"; Tag = 0; Payload = Some payload.Type }] }
                nested
        | _ -> Crash.crash "Described sum fixture did not have one payload"
    | _ -> nested

let private sumShapes (variants: LIR.VariantRegistry) : MemoryModel.RcSumShapeRegistry =
    variants
    |> Map.map (fun _ typeVariants ->
        { TypeParams = typeVariants.TypeParams
          Payloads = typeVariants.Variants |> List.map (fun variant -> variant.Tag, variant.Payload) })

let private append instructions state =
    { state with Instructions = state.Instructions @ instructions }

let private acquireRegister context state =
    match state.AvailableRegisters with
    | register :: rest -> Ok (register, { state with AvailableRegisters = rest })
    | [] -> Error $"Reference-release fixture exhausted registers while building {context}"

let private releaseRegister register state =
    { state with AvailableRegisters = register :: state.AvailableRegisters }

let private physical register = LIR.Physical register

let rec private buildInto (typed: TypedShape) target state : Result<BuildState, string> =
    let buildAndStoreField offset child current =
        acquireRegister child.Path current
        |> Result.bind (fun (childRegister, afterAcquire) ->
            buildInto child childRegister afterAcquire
            |> Result.map (fun afterBuild ->
                afterBuild
                |> append [ LIR.HeapStore (physical target, offset, LIR.Reg (physical childRegister), Some child.Type) ]
                |> releaseRegister childRegister))

    let buildFields children startOffset current =
        children
        |> List.mapi (fun index child -> startOffset + index * 8, child)
        |> List.fold
            (fun result (offset, child) ->
                result |> Result.bind (buildAndStoreField offset child))
            (Ok current)

    let tagPointer current =
        acquireRegister $"{typed.Path} tag" current
        |> Result.map (fun (tagRegister, afterAcquire) ->
            afterAcquire
            |> append
                [ LIR.Mov (physical tagRegister, LIR.Imm 2L)
                  LIR.Orr (physical target, physical target, physical tagRegister) ]
            |> releaseRegister tagRegister)

    match typed.Shape with
    | Int64Value -> Ok (append [ LIR.Mov (physical target, LIR.Imm 42L) ] state)
    | EnumValue -> Ok (append [ LIR.Mov (physical target, LIR.Imm 1L) ] state)
    | LiteralString -> Ok (append [ LIR.Mov (physical target, LIR.StringSymbol "literal") ] state)
    | DynamicString
    | DynamicBlob ->
        Ok (append [ LIR.StringConcat (physical target, LIR.StringSymbol "left", LIR.StringSymbol "right", []) ] state)
    | TupleValue _
    | RecordValue _ ->
        state
        |> append [ LIR.HeapAlloc (physical target, List.length typed.Children * 8) ]
        |> buildFields typed.Children 0
    | SumValue _ ->
        match typed.Children with
        | [ payload ] ->
            state
            |> append
                [ LIR.HeapAlloc (physical target, 16)
                  LIR.HeapStore (physical target, 0, LIR.Imm 0L, None) ]
            |> buildAndStoreField 8 payload
        | _ -> Error "Sum fixture must contain exactly one payload"
    | ListValue _ ->
        match typed.Children with
        | [ element ] ->
            state
            |> append [ LIR.HeapAlloc (physical target, 8) ]
            |> buildAndStoreField 0 element
            |> Result.bind tagPointer
        | _ -> Error "List fixture must contain exactly one representative element"
    | DictValue _ ->
        match typed.Children with
        | [ key; value ] ->
            state
            |> append [ LIR.HeapAlloc (physical target, 16) ]
            |> buildAndStoreField 0 key
            |> Result.bind (buildAndStoreField 8 value)
            |> Result.bind tagPointer
        | _ -> Error "Dict fixture must contain one representative key and value"
    | ClosureValue _ ->
        let rec buildCaptures builtRegisters current captures =
            match captures with
            | [] -> Ok (List.rev builtRegisters, current)
            | capture :: rest ->
                acquireRegister capture.Path current
                |> Result.bind (fun (captureRegister, afterAcquire) ->
                    buildInto capture captureRegister afterAcquire
                    |> Result.bind (fun afterBuild ->
                        buildCaptures (captureRegister :: builtRegisters) afterBuild rest))

        buildCaptures [] state typed.Children
        |> Result.map (fun (captureRegisters, afterBuild) ->
            let operands = captureRegisters |> List.map (physical >> LIR.Reg)
            let withClosure =
                append [ LIR.ClosureAlloc (physical target, AST.functionIdForName $"rc_fixture_closure_{typed.Path}", operands) ] afterBuild
            captureRegisters |> List.fold (fun current register -> releaseRegister register current) withClosure)

let rec private collectClosureFunctions (typed: TypedShape) : LIR.Function list =
    let nested = typed.Children |> List.collect collectClosureFunctions
    match typed.Shape with
    | ClosureValue _ ->
        let name = $"rc_fixture_closure_{typed.Path}"
        let label = LIR.Label $"{name}_entry"
        let captureTuple = AST.TTuple (AST.TInt64 :: (typed.Children |> List.map _.Type))
        let func : LIR.Function =
            { Id = AST.functionIdForName name
              Name = name
              TypedParams = [{ Reg = physical LIR.X0; Type = captureTuple }]
              CFG =
                { Entry = label
                  Blocks = Map.ofList [ label, { Label = label; Instrs = []; Terminator = LIR.Ret } ] }
              StackSize = 0
              UsedCalleeSaved = []
              CodegenFacts = None }
        func :: nested
    | _ -> nested

let private rootReleaseInstruction typed rootRegister metadata =
    match typed.Shape with
    | DynamicString
    | LiteralString
    | DynamicBlob -> Ok (LIR.RefCountDecString (LIR.Reg (physical rootRegister)))
    | ListValue _ -> Ok (LIR.RefCountDec (physical rootRegister, 0, LIR.TaggedList, Some metadata))
    | DictValue _ -> Ok (LIR.RefCountDec (physical rootRegister, 0, LIR.DictHeap, Some metadata))
    | ClosureValue captures ->
        Ok (LIR.RefCountDec (physical rootRegister, (List.length captures + 1) * 8, LIR.ClosureHeap, Some metadata))
    | TupleValue fields
    | RecordValue fields ->
        Ok (LIR.RefCountDec (physical rootRegister, List.length fields * 8, LIR.GenericHeap, Some metadata))
    | SumValue _ -> Ok (LIR.RefCountDec (physical rootRegister, 16, LIR.GenericHeap, Some metadata))
    | Int64Value
    | EnumValue -> Error "ROOT must be a managed value"

let private buildProgram test =
    let typed = describeShape "root" test.Root
    let rootRegister, preserved =
        match test.Placement with
        | CanonicalRoot -> LIR.X19, []
        | ExplicitRoot (register, values) -> register, values
    let unavailable = rootRegister :: (preserved |> List.map _.Register) |> Set.ofList
    let initialState =
        { AvailableRegisters = fixtureRegisters |> List.filter (fun register -> not (Set.contains register unavailable))
          Instructions = [] }
    let records = collectRecords typed
    let variants = collectVariants typed
    let shapes = sumShapes variants
    let releasePlan = MemoryPlanning.rcReleasePlanOfTypeWithSums records shapes typed.Type
    let metadata : MemoryModel.RcMetadata =
        { ReleasePlanCacheKey = ReleasePlanFingerprint.rcReleasePlanCacheKey typed.Type releasePlan
          ReleasePlan = Some releasePlan
          SourceType = Some typed.Type }

    buildInto typed rootRegister initialState
    |> Result.bind (fun built ->
        rootReleaseInstruction typed rootRegister metadata
        |> Result.map (fun releaseInstruction ->
            let preservedSetup =
                preserved |> List.map (fun value -> LIR.Mov (physical value.Register, LIR.Imm value.Value))
            let preservedChecks =
                match preserved with
                | [] -> []
                | [ value ] -> [ LIR.PrintInt64 (physical value.Register) ]
                | first :: rest ->
                    let accumulator = rootRegister
                    LIR.Mov (physical accumulator, LIR.Reg (physical first.Register))
                    :: (rest
                        |> List.collect (fun value ->
                            [ LIR.Add
                                  (physical accumulator, physical accumulator, LIR.Reg (physical value.Register)) ]))
                    @ [ LIR.PrintInt64 (physical accumulator) ]
            let instructions = built.Instructions @ preservedSetup @ [ releaseInstruction ] @ preservedChecks
            let entry = LIR.Label "entry"
            let main : LIR.Function =
                { Id = AST.functionIdForName "_start"
                  Name = "_start"
                  TypedParams = []
                  CFG =
                    { Entry = entry
                      Blocks = Map.ofList [ entry, { Label = entry; Instrs = instructions; Terminator = LIR.Ret } ] }
                  StackSize = 0
                  UsedCalleeSaved = []
                  CodegenFacts = None }
            LIR.Program (main :: collectClosureFunctions typed, variants, records), preserved))

let runRCReleaseTest target test =
    buildProgram test
    |> Result.bind (fun (program, preserved) ->
        TestDSL.LIRExecutionTestRunner.executeProgram target program LeakCheckEnabled
        |> Result.bind (fun (exitCode, stdout, stderr) ->
            let expectedOutput =
                match preserved with
                | [] -> ""
                | values -> values |> List.sumBy _.Value |> string
            if exitCode <> 0 then Error $"Expected release fixture to exit 0, got {exitCode}: {stderr.Trim()}"
            elif stdout.Trim() <> expectedOutput then
                Error $"Preserved registers produced '{stdout.Trim()}', expected '{expectedOutput}'"
            elif stderr.Trim() <> "" then Error $"Release fixture leaked memory: {stderr.Trim()}"
            else Ok ()))

let loadRCReleaseTests path =
    if not (File.Exists path) then Error $"Reference-release fixture not found: {path}"
    else
        try File.ReadAllText path |> parseRCReleaseFileContent path
        with ex -> Error $"Failed to read reference-release fixture {path}: {ex.Message}"

let tests target (testFiles: string array) : (string * (unit -> Result<unit, string>)) list =
    let testsForFile path =
        match loadRCReleaseTests path with
        | Error msg -> [ $"parse {Path.GetFileName path}", fun () -> Error msg ]
        | Ok cases -> cases |> List.map (fun test -> test.Name, fun () -> runRCReleaseTest target test)
    testFiles |> Array.sort |> Array.toList |> List.collect testsForFile
