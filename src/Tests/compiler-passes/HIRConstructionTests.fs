// HIRConstructionTests.fs - Checked-function normalization and structured-edge laws.

module HIRConstructionTests

open HIR

let rec private dependencies = function
    | CheckedAST.Var name -> Set.singleton name
    | CheckedAST.Let (pattern, value, body) ->
        let bound = CheckedAST.letPatternBindings pattern |> Set.ofList
        Set.union (dependencies value) (Set.difference (dependencies body) bound)
    | CheckedAST.Sequence (first, next) -> Set.union (dependencies first) (dependencies next)
    | CheckedAST.If (condition, ifTrue, ifFalse) ->
        Set.unionMany [dependencies condition; dependencies ifTrue; dependencies ifFalse]
    | _ -> Set.empty

let rec private infer types = function
    | CheckedAST.UnitLiteral -> Ok AST.TUnit
    | CheckedAST.Int64Literal _ -> Ok AST.TInt64
    | CheckedAST.BoolLiteral _ -> Ok AST.TBool
    | CheckedAST.Var name ->
        match Map.tryFind name types with
        | Some typ -> Ok typ
        | None -> Error $"unknown value {name}"
    | CheckedAST.If (_, ifTrue, _) -> infer types ifTrue
    | expression -> Error $"unsupported test expression {expression}"

let private functionDefinition body : CheckedAST.FunctionDef = {
    Name = "choose"
    TypeParams = []
    Params = {
        Head = ("flag", AST.TBool)
        Tail = [("first", AST.TInt64); ("second", AST.TInt64)]
    }
    ReturnType = AST.TInt64
    Body = body
    Recursion = None
}

let private testConstructsOrderedStructuredFunction () =
    let definition =
        functionDefinition
            (CheckedAST.Let (
                CheckedAST.LPVariable "selected",
                CheckedAST.If (
                    CheckedAST.Var "flag",
                    CheckedAST.Var "first",
                    CheckedAST.Var "second"),
                CheckedAST.Sequence (CheckedAST.UnitLiteral, CheckedAST.Var "selected")))
    match ConstructHIRFunctions.constructFunction infer dependencies definition with
    | Error error -> Error $"Unexpected HIR construction failure: {error}"
    | Ok constructed ->
        let block = ConstructHIRFunctions.body constructed.Body
        match block.Parameters, block.Operations with
        | [flag; first; second],
          [HIR.Branch (result, condition, ifTrue, ifFalse); HIR.ScalarBinding (_, unitOperand)] ->
            let trueBlock = ConstructHIRFunctions.body ifTrue
            let falseBlock = ConstructHIRFunctions.body ifFalse
            let actualParameters =
                [(flag.Name, flag.Value.Type); (first.Name, first.Value.Type); (second.Name, second.Value.Type)]
            let expectedParameters =
                [("flag", AST.TBool); ("first", AST.TInt64); ("second", AST.TInt64)]
            let orderedParameters = actualParameters = expectedParameters
            let structuredEdges =
                condition.Inputs = Map.ofList [("flag", flag.Value)]
                && trueBlock.Result = first.Value
                && falseBlock.Result = second.Value
                && block.Result = result
                && unitOperand.Type = AST.TUnit
            let verified =
                VerifyHIR.verifyFunctions ConstructHIRFunctions.verificationDialect [constructed] = Ok ()
            if orderedParameters && structuredEdges && verified then Ok ()
            else Error $"Constructed function did not preserve its checked boundary and branch edges: {block}"
        | _ -> Error $"Expected one branch followed by the sequenced Unit evaluation, got {block}"

let private testReportsBindingInferenceFailure () =
    let definition =
        functionDefinition
            (CheckedAST.Let (
                CheckedAST.LPVariable "unsupported",
                CheckedAST.TupleLiteral [CheckedAST.Int64Literal 1L],
                CheckedAST.Var "first"))
    match ConstructHIRFunctions.constructFunction infer dependencies definition with
    | Error (ConstructHIRFunctions.CannotInferExpression ("choose", _)) -> Ok ()
    | actual -> Error $"Expected a scoped inference failure, got {actual}"

let tests = [
    "Checked functions construct ordered structured HIR", testConstructsOrderedStructuredFunction
    "Checked function construction reports scoped inference failures", testReportsBindingInferenceFailure
]
