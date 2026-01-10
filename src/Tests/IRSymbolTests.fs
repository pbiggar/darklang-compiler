// IRSymbolTests.fs - Unit tests for symbolic LIR pool references
//
// Validates conversion between pooled refs and symbolic refs used for late pool resolution.

module IRSymbolTests

open LIR
open MIR
open LIRSymbolic

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

/// Run all symbolic LIR unit tests
let runAll () : TestResult =
    let tests = [
        ("symbolize/resolve round trip", testSymbolizeResolveRoundTrip)
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
