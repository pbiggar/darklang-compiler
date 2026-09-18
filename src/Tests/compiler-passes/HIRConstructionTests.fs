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
    | CheckedAST.Call (_, arguments) ->
        arguments
        |> AST.NonEmptyList.toList
        |> List.map dependencies
        |> Set.unionMany
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

let private noCalls : ConstructHIRFunctions.CallContracts = {
    ExternalSignature = fun _ -> None
    Contract = fun _ -> None
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
    match ConstructHIRFunctions.constructFunction infer dependencies noCalls definition with
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
                VerifyHIR.verifyFunctions (ConstructHIRFunctions.verificationDialect noCalls) [constructed] = Ok ()
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
    match ConstructHIRFunctions.constructFunction infer dependencies noCalls definition with
    | Error (ConstructHIRFunctions.CannotInferExpression ("choose", _)) -> Ok ()
    | actual -> Error $"Expected a scoped inference failure, got {actual}"

let private callFunction name firstParameter remainingParameters body : CheckedAST.FunctionDef = {
    Name = name
    TypeParams = []
    Params = {
        Head = firstParameter
        Tail = remainingParameters
    }
    ReturnType = AST.TInt64
    Body = body
    Recursion = None
}

let private callContract aliasResult (call: HIR.FunctionCall) : HIR.PrimitiveContract = {
    Inputs = call.Arguments
    Operands = []
    Outputs = [{
        Value = call.Result
        Alias = if aliasResult then HIR.MayReuseInput call.Result else HIR.NoManagedAlias
    }]
    Effects = Set.singleton HIR.MayInvokeUserCode
}

let private contractedCalls aliasResult : ConstructHIRFunctions.CallContracts = {
    ExternalSignature = fun _ -> None
    Contract = fun target ->
        if target = "callee" then Some (callContract aliasResult)
        else None
}

let private testNormalizesContractedCallsInArgumentOrder () =
    let callee =
        callFunction
            "callee"
            ("first", AST.TInt64)
            [("second", AST.TInt64)]
            (CheckedAST.Var "first")
    let caller =
        callFunction
            "caller"
            ("unit", AST.TUnit)
            []
            (CheckedAST.Call (
                "callee",
                { Head = CheckedAST.Int64Literal 1L; Tail = [CheckedAST.Int64Literal 2L] }))
    let calls = contractedCalls false
    match ConstructHIRFunctions.constructFunctions infer dependencies calls [callee; caller] with
    | Error error -> Error $"Unexpected direct-call construction failure: {error}"
    | Ok ([_; constructedCaller] as constructed) ->
        let block = ConstructHIRFunctions.body constructedCaller.Body
        let orderedArguments =
            match block.Operations with
            | [HIR.ScalarBinding (_, first); HIR.ScalarBinding (_, second); HIR.Call _] ->
                first.Expression = CheckedAST.Int64Literal 1L
                && second.Expression = CheckedAST.Int64Literal 2L
            | _ -> false
        let verified =
            let verification =
                VerifyHIR.verifyFunctions
                    (ConstructHIRFunctions.verificationDialect calls)
                    constructed
            verification = Ok ()
        if orderedArguments && verified then Ok ()
        else Error $"Contracted call did not preserve argument order and typed verification: {block}"
    | Ok actual -> Error $"Expected two constructed functions, got {actual}"

let private testKeepsUncontractedCallsOpaque () =
    let callee =
        callFunction "callee" ("value", AST.TInt64) [] (CheckedAST.Var "value")
    let caller =
        callFunction
            "caller"
            ("value", AST.TInt64)
            []
            (CheckedAST.Call ("callee", AST.NonEmptyList.singleton (CheckedAST.Var "value")))
    match ConstructHIRFunctions.constructFunctions infer dependencies noCalls [callee; caller] with
    | Ok [_; constructedCaller] ->
        let block = ConstructHIRFunctions.body constructedCaller.Body
        match block.Parameters, block.Operations with
        | [parameter], [HIR.ScalarBinding (_, operand)]
            when operand.Expression = caller.Body
                 && operand.Inputs = Map.ofList [("value", parameter.Value)] -> Ok ()
        | _ -> Error $"Uncontracted direct call did not remain an opaque checked operand: {block}"
    | Ok actual -> Error $"Expected two constructed functions, got {actual}"
    | Error error -> Error $"Unexpected opaque-call construction failure: {error}"

let private testRejectsInvalidCallAliasContract () =
    let callee =
        callFunction "callee" ("value", AST.TInt64) [] (CheckedAST.Var "value")
    let caller =
        callFunction
            "caller"
            ("value", AST.TInt64)
            []
            (CheckedAST.Call ("callee", AST.NonEmptyList.singleton (CheckedAST.Var "value")))
    let calls = contractedCalls true
    match ConstructHIRFunctions.constructFunctions infer dependencies calls [callee; caller] with
    | Error error -> Error $"Unexpected direct-call construction failure: {error}"
    | Ok constructed ->
        match VerifyHIR.verifyFunctions (ConstructHIRFunctions.verificationDialect calls) constructed with
        | Error (VerifyHIR.InvalidAliasSource _) -> Ok ()
        | actual -> Error $"Expected invalid call alias provenance, got {actual}"

let tests = [
    "Checked functions construct ordered structured HIR", testConstructsOrderedStructuredFunction
    "Checked function construction reports scoped inference failures", testReportsBindingInferenceFailure
    "Contracted direct calls preserve argument evaluation order", testNormalizesContractedCallsInArgumentOrder
    "Uncontracted direct calls remain opaque", testKeepsUncontractedCallsOpaque
    "Direct-call alias contracts remain verifier-authoritative", testRejectsInvalidCallAliasContract
]
