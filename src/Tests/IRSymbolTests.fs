// IRSymbolTests.fs - Unit tests for symbolic LIR pool references
//
// Validates conversion between pooled refs and symbolic refs used for late pool resolution.

module IRSymbolTests

open LIR
open MIR
open LIRSymbolic
open MIRSymbolic

/// Test result type
type TestResult = Result<unit, string>

let private buildSymbolicProgram () : Result<LIR.Program * string * float, string> =
    let stringValue = "hello"
    let floatValue = 1.25
    let (stringIdx, stringPool) = MIR.addString MIR.emptyStringPool stringValue
    let (floatIdx, floatPool) = MIR.addFloat MIR.emptyFloatPool floatValue
    match Map.tryFind stringIdx stringPool.Strings with
    | None -> Error $"Missing string pool entry for index {stringIdx}"
    | Some (_value, len) ->
        let label = LIR.Label "entry"
        let instrs = [
            LIR.PrintString (stringIdx, len)
            LIR.StringHash (LIR.Virtual 0, LIR.StringRef stringIdx)
            LIR.FLoad (LIR.FVirtual 0, floatIdx)
        ]
        let block: LIR.BasicBlock = { Label = label; Instrs = instrs; Terminator = LIR.Ret }
        let cfg: LIR.CFG = { Entry = label; Blocks = Map.ofList [ (label, block) ] }
        let func: LIR.Function = {
            Name = "symbolic_test"
            TypedParams = []
            CFG = cfg
            StackSize = 0
            UsedCalleeSaved = []
        }
        Ok (LIR.Program ([func], stringPool, floatPool), stringValue, floatValue)

let testSymbolizeResolveRoundTrip () : TestResult =
    match buildSymbolicProgram () with
    | Error err -> Error err
    | Ok (program, stringValue, floatValue) ->
        match LIRSymbolic.fromLIR program with
        | Error err -> Error $"Symbolize failed: {err}"
        | Ok (LIRSymbolic.Program symFuncs) ->
            let hasSymbols =
                match symFuncs with
                | [func] ->
                    func.CFG.Blocks
                    |> Map.toList
                    |> List.collect (fun (_, block) -> block.Instrs)
                    |> List.exists (function
                        | LIRSymbolic.PrintString value -> value = stringValue
                        | LIRSymbolic.FLoad (_, value) -> value = floatValue
                        | LIRSymbolic.StringHash (_, LIRSymbolic.StringSymbol value) -> value = stringValue
                        | _ -> false)
                | _ -> false
            if not hasSymbols then
                Error "Expected symbolized instructions to contain symbolic refs"
            else
                match LIRSymbolic.toLIR (LIRSymbolic.Program symFuncs) with
                | Error err -> Error $"Resolve failed: {err}"
                | Ok resolved ->
                    let (LIR.Program (resolvedFuncs, resolvedStrings, resolvedFloats)) = resolved
                    match Map.tryFind stringValue resolvedStrings.StringToId with
                    | None -> Error "Resolved string pool missing expected value"
                    | Some strIdx ->
                        match Map.tryFind floatValue resolvedFloats.FloatToId with
                        | None -> Error "Resolved float pool missing expected value"
                        | Some floatIdx ->
                            match resolvedFuncs with
                            | [func] ->
                                let instrs =
                                    func.CFG.Blocks
                                    |> Map.toList
                                    |> List.collect (fun (_, block) -> block.Instrs)
                                let hasPrint =
                                    instrs
                                    |> List.exists (function
                                        | LIR.PrintString (idx, _) -> idx = strIdx
                                        | _ -> false)
                                let hasLoad =
                                    instrs
                                    |> List.exists (function
                                        | LIR.FLoad (_, idx) -> idx = floatIdx
                                        | _ -> false)
                                if hasPrint && hasLoad then
                                    Ok ()
                                else
                                    Error "Resolved program did not reference pooled indices as expected"
                            | _ -> Error "Expected a single resolved function"

let private buildSymbolicMirProgram () : Result<MIR.Program * string * float, string> =
    let stringValue = "mir_symbolic"
    let floatValue = 2.5
    let (stringIdx, stringPool) = MIR.addString MIR.emptyStringPool stringValue
    let (floatIdx, floatPool) = MIR.addFloat MIR.emptyFloatPool floatValue
    let label = MIR.Label "entry"
    let instrs = [
        MIR.Mov (MIR.VReg 0, MIR.StringRef stringIdx, Some AST.TString)
        MIR.Mov (MIR.VReg 1, MIR.FloatRef floatIdx, Some AST.TFloat64)
    ]
    let block: MIR.BasicBlock = { Label = label; Instrs = instrs; Terminator = MIR.Ret (MIR.Register (MIR.VReg 0)) }
    let cfg: MIR.CFG = { Entry = label; Blocks = Map.ofList [ (label, block) ] }
    let func: MIR.Function = {
        Name = "mir_symbolic_test"
        TypedParams = []
        ReturnType = AST.TString
        CFG = cfg
        FloatRegs = Set.ofList [ 1 ]
    }
    let program = MIR.Program ([func], stringPool, floatPool, Map.empty, Map.empty)
    Ok (program, stringValue, floatValue)

let testMirSymbolizeResolveRoundTrip () : TestResult =
    match buildSymbolicMirProgram () with
    | Error err -> Error err
    | Ok (program, stringValue, floatValue) ->
        match MIRSymbolic.fromMIR program with
        | Error err -> Error $"Symbolize failed: {err}"
        | Ok (MIRSymbolic.Program (symFuncs, variants, records)) ->
            let hasSymbols =
                match symFuncs with
                | [func] ->
                    func.CFG.Blocks
                    |> Map.toList
                    |> List.collect (fun (_, block) -> block.Instrs)
                    |> List.exists (function
                        | MIRSymbolic.Mov (_, MIRSymbolic.StringSymbol value, _) -> value = stringValue
                        | MIRSymbolic.Mov (_, MIRSymbolic.FloatSymbol value, _) -> value = floatValue
                        | _ -> false)
                | _ -> false
            if not hasSymbols then
                Error "Expected symbolized MIR to contain symbolic refs"
            else
                match MIRSymbolic.toMIR (MIRSymbolic.Program (symFuncs, variants, records)) with
                | Error err -> Error $"Resolve failed: {err}"
                | Ok resolved ->
                    let (MIR.Program (_resolvedFuncs, resolvedStrings, resolvedFloats, _, _)) = resolved
                    match Map.tryFind stringValue resolvedStrings.StringToId with
                    | None -> Error "Resolved string pool missing expected value"
                    | Some _ ->
                        match Map.tryFind floatValue resolvedFloats.FloatToId with
                        | None -> Error "Resolved float pool missing expected value"
                        | Some _ -> Ok ()

let testMirNormalizePools () : TestResult =
    let stringValue = "pool_normalize"
    let floatValue = 3.25
    let stringLen = System.Text.Encoding.UTF8.GetByteCount stringValue
    let stringPool: MIR.StringPool = {
        Strings = Map.ofList [ (5, (stringValue, stringLen)) ]
        StringToId = Map.ofList [ (stringValue, 5) ]
        NextId = 6
    }
    let floatPool: MIR.FloatPool = {
        Floats = Map.ofList [ (8, floatValue) ]
        FloatToId = Map.ofList [ (floatValue, 8) ]
        NextId = 9
    }
    let label = MIR.Label "entry"
    let instrs = [
        MIR.Mov (MIR.VReg 0, MIR.StringRef 5, Some AST.TString)
        MIR.Mov (MIR.VReg 1, MIR.FloatRef 8, Some AST.TFloat64)
    ]
    let block: MIR.BasicBlock = { Label = label; Instrs = instrs; Terminator = MIR.Ret (MIR.Register (MIR.VReg 0)) }
    let cfg: MIR.CFG = { Entry = label; Blocks = Map.ofList [ (label, block) ] }
    let func: MIR.Function = {
        Name = "mir_pool_normalize"
        TypedParams = []
        ReturnType = AST.TString
        CFG = cfg
        FloatRegs = Set.ofList [ 1 ]
    }
    let program = MIR.Program ([func], stringPool, floatPool, Map.empty, Map.empty)
    match MIRSymbolic.normalizePools program with
    | Error err -> Error $"Normalize failed: {err}"
    | Ok (MIR.Program (normalizedFuncs, normalizedStrings, normalizedFloats, _, _)) ->
        match Map.tryFind stringValue normalizedStrings.StringToId with
        | None -> Error "Normalized string pool missing expected value"
        | Some stringIdx ->
            match Map.tryFind floatValue normalizedFloats.FloatToId with
            | None -> Error "Normalized float pool missing expected value"
            | Some floatIdx ->
                match normalizedFuncs with
                | [func] ->
                    let instrs =
                        func.CFG.Blocks
                        |> Map.toList
                        |> List.collect (fun (_, block) -> block.Instrs)
                    let hasStringRef =
                        instrs
                        |> List.exists (function
                            | MIR.Mov (_, MIR.StringRef idx, _) -> idx = stringIdx
                            | _ -> false)
                    let hasFloatRef =
                        instrs
                        |> List.exists (function
                            | MIR.Mov (_, MIR.FloatRef idx, _) -> idx = floatIdx
                            | _ -> false)
                    if hasStringRef && hasFloatRef then
                        Ok ()
                    else
                        Error "Normalized MIR did not use pooled indices from rebuilt pools"
                | _ -> Error "Expected a single normalized function"

/// Run all symbolic LIR unit tests
let runAll () : TestResult =
    let tests = [
        ("symbolize/resolve round trip", testSymbolizeResolveRoundTrip)
        ("mir symbolize/resolve round trip", testMirSymbolizeResolveRoundTrip)
        ("mir normalize pools", testMirNormalizePools)
    ]
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
