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

let testMirToLirReportsMissingEntryBlock () : TestResult =
    let entry = MIR.Label "entry"
    let actual = MIR.Label "actual"
    let block: MIR.BasicBlock =
        { Label = actual
          Instrs = [MIR.Mov (MIR.VReg 0, MIR.Int64Const 42L, Some AST.TInt64)]
          Terminator = MIR.Ret (MIR.Register (MIR.VReg 0)) }
    let cfg: MIR.CFG = { Entry = entry; Blocks = Map.ofList [ (actual, block) ] }
    let func: MIR.Function =
        { Name = "missing_entry"
          TypedParams = []
          ReturnType = AST.TInt64
          CFG = cfg
          FloatRegs = Set.empty }
    let program = MIR.Program ([func], Map.empty, Map.empty)

    match MIR_to_LIR.toLIR program with
    | Error err when err.Contains "missing entry block" -> Ok ()
    | Error err -> Error $"Expected missing entry block error, got '{err}'"
    | Ok _ -> Error "Expected MIR→LIR to reject a CFG whose entry block is absent"

/// Native 64-bit variable shifts already mask their count in both supported
/// instruction sets. Ensure lowering does not emit a redundant explicit mask.
let testMirToLirUsesNativeInt64ShiftMask () : TestResult =
    let label = MIR.Label "entry"
    let block: MIR.BasicBlock =
        { Label = label
          Instrs = [MIR.BinOp (MIR.VReg 2, MIR.Shl, MIR.Register (MIR.VReg 0), MIR.Register (MIR.VReg 1), AST.TInt64)
                    MIR.BinOp (MIR.VReg 3, MIR.Shr, MIR.Register (MIR.VReg 0), MIR.Register (MIR.VReg 1), AST.TInt64)]
          Terminator = MIR.Ret (MIR.Register (MIR.VReg 3)) }
    let cfg: MIR.CFG = { Entry = label; Blocks = Map.ofList [ (label, block) ] }
    let func: MIR.Function =
        { Name = "native_int64_shift_mask"
          TypedParams = [{ Reg = MIR.VReg 0; Type = AST.TInt64 }; { Reg = MIR.VReg 1; Type = AST.TInt64 }]
          ReturnType = AST.TInt64
          CFG = cfg
          FloatRegs = Set.empty }
    match MIR_to_LIR.toLIR (MIR.Program ([func], Map.empty, Map.empty)) with
    | Error err -> Error $"MIR→LIR failed: {err}"
    | Ok (LIR.Program ([lirFunc], _, _)) ->
        let instructions = lirFunc.CFG.Blocks |> Map.toList |> List.collect (fun (_, block) -> block.Instrs)
        if instructions |> List.exists (function | LIR.And_imm (_, _, 63L) -> true | _ -> false) then
            Error "Expected native Int64 variable shift lowering to omit AND #63"
        else if instructions |> List.exists (function | LIR.Lsl (_, _, _) -> true | _ -> false)
                && instructions |> List.exists (function | LIR.Asr (_, _, _) -> true | _ -> false) then
            Ok ()
        else
            Error "Expected native Int64 variable shift lowering to emit Lsl and Asr"
    | Ok _ -> Error "Expected a single LIR function"

let tests = [
    ("mir → lir symbolic operands", testMirToLirSymbolicOperands)
    ("mir → lir reports missing entry block", testMirToLirReportsMissingEntryBlock)
    ("mir → lir uses native Int64 shift mask", testMirToLirUsesNativeInt64ShiftMask)
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
