// ARM64CodeGenTests.fs - Unit tests for ARM64 code generation from LIR.
//
// These tests inspect symbolic ARM64 instructions for ownership-sensitive
// lowering decisions that do not need a full executable harness.

module ARM64CodeGenTests

type TestResult = Result<unit, string>

let private makeSimpleProgramWithVariants
    (instrs: LIR.Instr list)
    (variants: LIR.VariantRegistry)
    : LIR.Program =
    let label = LIR.Label "_start_entry"
    let block : LIR.BasicBlock = {
        Label = label
        Instrs = instrs
        Terminator = LIR.Ret
    }
    let func : LIR.Function = {
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = label
            Blocks = Map.ofList [(label, block)]
        }
        StackSize = 0
        UsedCalleeSaved = []
    }
    LIR.Program ([func], variants, Map.empty)

let testRawSetPureEnumDoesNotEmitGenericRetain () : TestResult =
    let enumType = AST.TSum ("RawSetPureEnum", [AST.TString])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            ("RawSetPureEnum",
                { TypeParams = ["a"]
                  Variants =
                    [
                        { Name = "RawSetPureA"; Tag = 0; Payload = None }
                        { Name = "RawSetPureB"; Tag = 1; Payload = None }
                    ] })
        ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RawSet (
                    LIR.Physical LIR.X0,
                    LIR.Physical LIR.X1,
                    LIR.Physical LIR.X3,
                    Some enumType)
            ]
            variants

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let emittedGenericRetain =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X15, ARM64.X3, 16s)
                | ARM64Symbolic.LDR (ARM64.X14, ARM64.X3, 16s) ->
                    true
                | _ ->
                    false)
        if emittedGenericRetain then
            Error "RawSet of a generic pure enum emitted a generic heap retain"
        else
            Ok ()

let tests : (string * (unit -> TestResult)) list = [
    ("RawSet pure enum skips generic retain", testRawSetPureEnumDoesNotEmitGenericRetain)
]
