// StdlibOptimizationTests.fs - Check optimization in prebuilt stdlib output.

module StdlibOptimizationTests

type TestResult = Result<unit, string>

let private testStdlibPowerMask
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    let powerLoop =
        stdlib.AllocatedFunctions
        |> List.tryFind (fun func ->
            func.Name = "Darklang.Stdlib.Int64.__powerLoop")
    match powerLoop with
    | None -> Error "Missing prebuilt Stdlib.Int64.__powerLoop"
    | Some func ->
        let instructions =
            func.CFG.Blocks
            |> Map.toList
            |> List.collect (fun (_, block) -> block.Instrs)
        if instructions
           |> List.exists (function LIR.And_imm (_, _, 1L) -> true | _ -> false) then
            Ok ()
        else
            Error "Expected an exponent bit mask in prebuilt Stdlib.Int64.__powerLoop"

let tests (stdlib: CompilationContexts.StdlibResult) = [
    ("prebuilt stdlib power uses a bit mask", testStdlibPowerMask stdlib)
]
