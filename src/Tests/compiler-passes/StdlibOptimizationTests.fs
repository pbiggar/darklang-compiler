// StdlibOptimizationTests.fs - Whole-pipeline checks for optimized prebuilt stdlib ANF.

module StdlibOptimizationTests

open ANF

type TestResult = Result<unit, string>

let rec private containsBinOp (target: BinOp) (expr: AExpr) : bool =
    match expr with
    | Let (_, Prim (op, _, _), body) -> op = target || containsBinOp target body
    | Let (_, _, body) -> containsBinOp target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        containsBinOp target thenBranch || containsBinOp target elseBranch
    | Jump _ | Return _ -> false

let private testStdlibANFStrengthReduction
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    match Map.tryFind "Darklang.Stdlib.Int64.__powerLoop" stdlib.StdlibANFFunctions with
    | None -> Error "Missing Stdlib.Int64.__powerLoop from prebuilt stdlib ANF"
    | Some powerLoop ->
        if containsBinOp Mod powerLoop.Body then
            Error "Expected stdlib ANF optimization to strength-reduce exponent % 2"
        elif not (containsBinOp BitAnd powerLoop.Body) then
            Error "Expected strength-reduced exponent bit mask in Stdlib.Int64.__powerLoop"
        else
            Ok ()

let tests (stdlib: CompilationContexts.StdlibResult) = [
    ("prebuilt stdlib ANF applies strength reduction", testStdlibANFStrengthReduction stdlib)
]
