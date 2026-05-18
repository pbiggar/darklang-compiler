// IRSymbolTests.fs - Unit tests for symbolic IR pool references
//
// Validates conversion between pooled refs and symbolic refs used for late pool resolution.

module IRSymbolTests

open MIR
open LIR

/// Test result type
type TestResult = Result<unit, string>

let testMirToLirSymbolicOperands () : TestResult =
    let label = MIR.Label "entry"
    let instrs = [
        MIR.Mov (MIR.VReg 0, MIR.StringSymbol "mir_symbolic", Some AST.TString)
        MIR.Mov (MIR.VReg 1, MIR.FloatSymbol 4.5, Some AST.TFloat64)
    ]
    let block: MIR.BasicBlock = { Label = label; Instrs = instrs; Terminator = MIR.Ret (MIR.Register (MIR.VReg 0)) }
    let cfg: MIR.CFG = { Entry = label; Blocks = Map.ofList [ (label, block) ] }
    let func: MIR.Function = {
        Name = "mir_symbolic_operands"
        TypedParams = []
        ReturnType = AST.TString
        CFG = cfg
        FloatRegs = Set.ofList [ 1 ]
    }
    let program = MIR.Program ([func], Map.empty, Map.empty)
    match MIR_to_LIR.toLIR program with
    | Error err -> Error $"MIR→LIR failed: {err}"
    | Ok (LIR.Program (funcs, _, _)) ->
        match funcs with
        | [lirFunc] ->
            let hasSymbolic =
                lirFunc.CFG.Blocks
                |> Map.toList
                |> List.collect (fun (_, block) -> block.Instrs)
                |> List.exists (function
                    | LIR.Mov (_, LIR.StringSymbol value) -> value = "mir_symbolic"
                    | LIR.Mov (_, LIR.FloatSymbol value) -> value = 4.5
                    | _ -> false)
            if hasSymbolic then Ok ()
            else Error "Expected MIR→LIR to preserve symbolic operands"
        | _ -> Error "Expected a single LIR function"

let tests = [
    ("mir → lir symbolic operands", testMirToLirSymbolicOperands)
]

/// Run all symbolic LIR unit tests
let runAll () : TestResult =
    tests
    |> List.fold
        (fun acc (name, test) ->
            match acc with
            | Error _ -> acc
            | Ok () ->
                match test () with
                | Ok () -> Ok ()
                | Error err -> Error $"IRSymbolTests - {name} failed: {err}")
        (Ok ())
