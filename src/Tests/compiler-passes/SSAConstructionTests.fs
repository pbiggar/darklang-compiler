// SSAConstructionTests.fs - Unit tests for MIR SSA construction invariants.
//
// These tests cover internal CFG invariants that cannot be exercised cleanly
// through source-level end-to-end programs.

module SSAConstructionTests

open MIR
open SSA_Construction

type TestResult = Result<unit, string>

let private label name = Label name
let private vreg id = VReg id

let private makeBlock label instrs terminator : BasicBlock =
    { Label = label
      Instrs = instrs
      Terminator = terminator }

let testComputeLivenessReportsMissingSuccessorBlock () : TestResult =
    let entry = label "entry"
    let missing = label "missing"
    let cfg =
        { Entry = entry
          Blocks =
            [ makeBlock entry [Mov (vreg 0, Int64Const 1L, Some AST.TInt64)] (Jump missing) ]
            |> List.map (fun block -> (block.Label, block))
            |> Map.ofList }

    try
        let _ = computeLiveness cfg
        Error "Expected computeLiveness to report the missing successor block"
    with ex ->
        if ex.Message.Contains("SSA: Missing CFG block missing while computing liveness successor") then
            Ok ()
        else
            Error $"Expected contextual SSA missing-block message, got: {ex.Message}"

let testSSAVersionsStartAboveParameterRegisters () : TestResult =
    let entry = label "entry"
    let parameter = { Reg = vreg 10000; Type = AST.TInt64 }
    let func =
        { Name = "test"
          TypedParams = [parameter]
          ReturnType = AST.TInt64
          CFG =
            { Entry = entry
              Blocks =
                [ makeBlock entry [RawAlloc (vreg 0, Int64Const 8L)] (Ret (Register parameter.Reg)) ]
                |> List.map (fun block -> (block.Label, block))
                |> Map.ofList }
          FloatRegs = Set.empty }

    let converted = convertFunctionToSSA func
    match Map.tryFind converted.CFG.Entry converted.CFG.Blocks with
    | Some { Instrs = RawAlloc (VReg destination, _) :: _ } when destination <> 10000 -> Ok ()
    | Some { Instrs = RawAlloc (VReg destination, _) :: _ } ->
        Error $"SSA construction reused parameter VReg 10000 for instruction destination {destination}"
    | Some block -> Error $"Expected converted entry block to start with RawAlloc, got {block.Instrs}"
    | None -> Error "Expected converted CFG to contain its entry block"

let tests = [
    ("computeLiveness reports missing successor block", testComputeLivenessReportsMissingSuccessorBlock)
    ("SSA versions start above parameter registers", testSSAVersionsStartAboveParameterRegisters)
]
