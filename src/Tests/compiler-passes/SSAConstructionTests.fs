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

let testGetBlockUsesCoversEveryOperandPosition () : TestResult =
    let destination = vreg 0
    let first = vreg 1
    let second = vreg 2
    let third = vreg 3
    let register reg = Register reg
    let expected regs = Set.ofList regs
    let target = label "target"
    let instructionCases = [
        ("Mov", Mov (destination, register first, Some AST.TInt64), expected [first])
        ("BinOp", BinOp (destination, Add, register first, register second, AST.TInt64), expected [first; second])
        ("UnaryOp", UnaryOp (destination, Neg, register first), expected [first])
        ("Call", Call (destination, "callee", [register first; Int64Const 1L; register second], [AST.TInt64; AST.TInt64; AST.TInt64], AST.TInt64), expected [first; second])
        ("TailCall", TailCall ("callee", [register first; Int64Const 1L; register second], [AST.TInt64; AST.TInt64; AST.TInt64], AST.TInt64), expected [first; second])
        ("IndirectCall", IndirectCall (destination, register first, [register second; Int64Const 1L; register third], [AST.TInt64; AST.TInt64; AST.TInt64], AST.TInt64), expected [first; second; third])
        ("IndirectTailCall", IndirectTailCall (register first, [register second; Int64Const 1L; register third], [AST.TInt64; AST.TInt64; AST.TInt64], AST.TInt64), expected [first; second; third])
        ("ClosureAlloc", ClosureAlloc (destination, "callee", [register first; Int64Const 1L; register second]), expected [first; second])
        ("ClosureCall", ClosureCall (destination, register first, [register second; Int64Const 1L; register third], [AST.TInt64; AST.TInt64; AST.TInt64], AST.TInt64), expected [first; second; third])
        ("ClosureTailCall", ClosureTailCall (register first, [register second; Int64Const 1L; register third], [AST.TInt64; AST.TInt64; AST.TInt64]), expected [first; second; third])
        ("HeapStore", HeapStore (first, 0, register second, Some AST.TInt64), expected [first; second])
        ("HeapLoad", HeapLoad (destination, first, 0, Some AST.TInt64), expected [first])
        ("StringConcat", StringConcat (destination, register first, register second), expected [first; second])
        ("RefCountInc", RefCountInc (first, 8, GenericHeap, None), expected [first])
        ("RefCountDec", RefCountDec (first, 8, GenericHeap, None), expected [first])
        ("Print", Print (register first, AST.TInt64), expected [first])
        ("FileReadText", FileReadText (destination, register first), expected [first])
        ("FileExists", FileExists (destination, register first), expected [first])
        ("FileWriteText", FileWriteText (destination, register first, register second), expected [first; second])
        ("FileAppendText", FileAppendText (destination, register first, register second), expected [first; second])
        ("FileDelete", FileDelete (destination, register first), expected [first])
        ("FileSetExecutable", FileSetExecutable (destination, register first), expected [first])
        ("FileWriteFromPtr", FileWriteFromPtr (destination, register first, register second, register third), expected [first; second; third])
        ("Phi", Phi (destination, [(register first, target); (Int64Const 1L, target); (register second, target)], Some AST.TInt64), expected [first; second])
        ("RawAlloc", RawAlloc (destination, register first), expected [first])
        ("RawFree", RawFree (register first), expected [first])
        ("RawGet", RawGet (destination, register first, register second, Some AST.TInt64), expected [first; second])
        ("RawGetByte", RawGetByte (destination, register first, register second), expected [first; second])
        ("RawWriteWord", RawWriteWord (register first, register second, register third), expected [first; second; third])
        ("RawWriteByte", RawWriteByte (register first, register second, register third), expected [first; second; third])
        ("RawSlotInit", RawSlotInit (register first, register second, register third, AST.TInt64), expected [first; second; third])
        ("StringToRawPtr", StringToRawPtr (destination, register first), expected [first])
        ("RawPtrToString", RawPtrToString (destination, register first), expected [first])
        ("BlobToRawPtr", BlobToRawPtr (destination, register first), expected [first])
        ("RawPtrToBlob", RawPtrToBlob (destination, register first), expected [first])
        ("DictToRawPtr", DictToRawPtr (destination, register first), expected [first])
        ("RawPtrToDict", RawPtrToDict (destination, register first, register second), expected [first; second])
        ("ListToRawPtr", ListToRawPtr (destination, register first), expected [first])
        ("RawPtrToList", RawPtrToList (destination, register first, register second), expected [first; second])
        ("FloatSqrt", FloatSqrt (destination, register first), expected [first])
        ("FloatAbs", FloatAbs (destination, register first), expected [first])
        ("FloatNeg", FloatNeg (destination, register first), expected [first])
        ("Int64ToFloat", Int64ToFloat (destination, register first), expected [first])
        ("FloatToInt64", FloatToInt64 (destination, register first), expected [first])
        ("FloatToBits", FloatToBits (destination, register first), expected [first])
        ("RefCountIncString", RefCountIncString (register first), expected [first])
        ("RefCountDecString", RefCountDecString (register first), expected [first])
        ("RefCountIncBlob", RefCountIncBlob (register first), expected [first])
        ("RefCountDecBlob", RefCountDecBlob (register first), expected [first])
        ("FloatToString", FloatToString (destination, register first), expected [first])
    ]
    let terminatorCases = [
        ("Ret", Ret (register first), expected [first])
        ("Branch", Branch (register first, target, target), expected [first])
        ("Jump", Jump target, Set.empty)
    ]
    let instructionFailure =
        instructionCases
        |> List.tryPick (fun (name, instr, expectedUses) ->
            let actual = getBlockUses (makeBlock (label name) [instr] (Jump target))
            if actual = expectedUses then None
            else Some $"{name}: expected uses {expectedUses}, got {actual}")
    match instructionFailure with
    | Some error -> Error error
    | None ->
        let terminatorFailure =
            terminatorCases
            |> List.tryPick (fun (name, terminator, expectedUses) ->
                let actual = getBlockUses (makeBlock (label name) [] terminator)
                if actual = expectedUses then None
                else Some $"{name}: expected uses {expectedUses}, got {actual}")
        match terminatorFailure with
        | Some error -> Error error
        | None -> Ok ()

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
    ("getBlockUses covers every operand position", testGetBlockUsesCoversEveryOperandPosition)
    ("computeLiveness reports missing successor block", testComputeLivenessReportsMissingSuccessorBlock)
    ("SSA versions start above parameter registers", testSSAVersionsStartAboveParameterRegisters)
]
