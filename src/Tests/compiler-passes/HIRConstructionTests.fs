// HIRConstructionTests.fs - Checked-function normalization and structured-edge laws.

module HIRConstructionTests

open HIR

let private binding name =
    name |> Seq.fold (fun hash ch -> (hash * 31) + int ch) 17 |> AST.bindingId
let private local name = CheckedAST.Local (binding name)
let private variable name = CheckedAST.LPVariable (binding name)
let private parameter name typ = binding name, typ

let rec private dependencies = function
    | CheckedAST.Local id -> Set.singleton id
    | CheckedAST.Let (pattern, value, body) ->
        let bound = CheckedAST.letPatternBindings pattern |> Set.ofList
        Set.union (dependencies value) (Set.difference (dependencies body) bound)
    | CheckedAST.Sequence (first, next) -> Set.union (dependencies first) (dependencies next)
    | CheckedAST.If (condition, ifTrue, ifFalse) ->
        Set.unionMany [dependencies condition; dependencies ifTrue; dependencies ifFalse]
    | CheckedAST.BinOp (_, left, right) -> Set.union (dependencies left) (dependencies right)
    | CheckedAST.UnaryOp (_, operand) -> dependencies operand
    | CheckedAST.Call (_, arguments) ->
        arguments
        |> AST.NonEmptyList.toList
        |> List.map dependencies
        |> Set.unionMany
    | _ -> Set.empty

let rec private infer types = function
    | CheckedAST.UnitLiteral -> Ok AST.TUnit
    | CheckedAST.Int8Literal _ -> Ok AST.TInt8
    | CheckedAST.Int16Literal _ -> Ok AST.TInt16
    | CheckedAST.Int32Literal _ -> Ok AST.TInt32
    | CheckedAST.Int64Literal _ -> Ok AST.TInt64
    | CheckedAST.UInt8Literal _ -> Ok AST.TUInt8
    | CheckedAST.UInt16Literal _ -> Ok AST.TUInt16
    | CheckedAST.UInt32Literal _ -> Ok AST.TUInt32
    | CheckedAST.UInt64Literal _ -> Ok AST.TUInt64
    | CheckedAST.BoolLiteral _ -> Ok AST.TBool
    | CheckedAST.FloatLiteral _ -> Ok AST.TFloat64
    | CheckedAST.StringLiteral _ -> Ok AST.TString
    | CheckedAST.BigIntLiteral _ -> Ok AST.TInt
    | CheckedAST.Local id ->
        match Map.tryFind id types with
        | Some typ -> Ok typ
        | None -> Error $"unknown value {id}"
    | CheckedAST.If (_, ifTrue, _) -> infer types ifTrue
    | CheckedAST.Let (CheckedAST.LPVariable id, value, body) ->
        infer types value
        |> Result.bind (fun valueType -> infer (Map.add id valueType types) body)
    | CheckedAST.Let (_, _, body) -> infer types body
    | CheckedAST.UnaryOp (_, operand) -> infer types operand
    | CheckedAST.BinOp (op, left, _) ->
        match op with
        | AST.Eq | AST.Neq | AST.Lt | AST.Gt | AST.Lte | AST.Gte
        | AST.And | AST.Or -> Ok AST.TBool
        | AST.StringConcat -> Ok AST.TString
        | _ -> infer types left
    | expression -> Error $"unsupported test expression {expression}"

let private functionDefinition body : CheckedAST.FunctionDef = {
    Id = TestIds.functionIdForName "choose"
    Name = "choose"
    TypeParams = []
    Params = {
        Head = parameter "flag" AST.TBool
        Tail = [parameter "first" AST.TInt64; parameter "second" AST.TInt64]
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
                variable "selected",
                CheckedAST.If (
                    local "flag",
                    local "first",
                    local "second"),
                CheckedAST.Sequence (CheckedAST.UnitLiteral, local "selected")))
    match ConstructHIRFunctions.constructFunction infer dependencies noCalls definition with
    | Error error -> Error $"Unexpected HIR construction failure: {error}"
    | Ok constructed ->
        let block = ConstructHIRFunctions.body constructed.Body
        match block.Parameters, block.Operations with
        | [flag; first; second],
          [HIR.Branch (result, condition, ifTrue, ifFalse);
           HIR.Leaf (ConstructHIRFunctions.Literal (unitResult, ConstructHIRFunctions.UnitLiteral))] ->
            let trueBlock = ConstructHIRFunctions.body ifTrue
            let falseBlock = ConstructHIRFunctions.body ifFalse
            let actualParameters =
                [(flag.Binding, flag.Value.Type); (first.Binding, first.Value.Type); (second.Binding, second.Value.Type)]
            let expectedParameters =
                [parameter "flag" AST.TBool; parameter "first" AST.TInt64; parameter "second" AST.TInt64]
            let orderedParameters = actualParameters = expectedParameters
            let structuredEdges =
                condition.Inputs = Map.ofList [(binding "flag", flag.Value)]
                && trueBlock.Result = first.Value
                && falseBlock.Result = second.Value
                && block.Result = result
                && unitResult.Type = AST.TUnit
            let verified =
                VerifyHIR.verifyFunctions (ConstructHIRFunctions.verificationDialect noCalls) [constructed] = Ok ()
            if orderedParameters && structuredEdges && verified then Ok ()
            else Error $"Constructed function did not preserve its checked boundary and branch edges: {block}"
        | _ -> Error $"Expected one branch followed by the sequenced Unit evaluation, got {block}"

let private testReportsBindingInferenceFailure () =
    let definition =
        functionDefinition
            (CheckedAST.Let (
                variable "unsupported",
                CheckedAST.TupleLiteral [CheckedAST.Int64Literal 1L],
                local "first"))
    match ConstructHIRFunctions.constructFunction infer dependencies noCalls definition with
    | Error (ConstructHIRFunctions.CannotInferExpression ("choose", _)) -> Ok ()
    | actual -> Error $"Expected a scoped inference failure, got {actual}"

let private testFallsBackToOpaqueCheckedFunctionForScheduling () =
    let source =
        functionDefinition
            (CheckedAST.Let (
                variable "unsupported",
                CheckedAST.TupleLiteral [CheckedAST.Int64Literal 1L],
                local "first"))
    match ConstructHIRFunctions.constructFunctionsWithOpaqueFallback Map.empty infer dependencies noCalls [source] with
    | Ok [constructed] ->
        let block = ConstructHIRFunctions.body constructed.Body
        match block.Parameters, block.Operations with
        | [_; first; _], [HIR.ScalarBinding (result, operand)]
            when result = block.Result
                 && operand.Expression = source.Body
                 && operand.Inputs = Map.ofList [(first.Binding, first.Value)] -> Ok ()
        | _ -> Error $"Expected one boundary-typed opaque operation for the unsupported function, got {block}"
    | actual -> Error $"Expected conservative opaque construction, got {actual}"

let private callFunction name firstParameter remainingParameters body : CheckedAST.FunctionDef = {
    Id = TestIds.functionIdForName name
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
        if target = TestIds.functionIdForName "callee" then Some (callContract aliasResult)
        else None
}

let private testNormalizesContractedCallsInArgumentOrder () =
    let callee =
        callFunction
            "callee"
            (parameter "first" AST.TInt64)
            [parameter "second" AST.TInt64]
            (local "first")
    let caller =
        callFunction
            "caller"
            (parameter "unit" AST.TUnit)
            []
            (CheckedAST.Call (
                TestIds.functionIdForName "callee",
                { Head = CheckedAST.Int64Literal 1L; Tail = [CheckedAST.Int64Literal 2L] }))
    let calls = contractedCalls false
    match ConstructHIRFunctions.constructFunctions infer dependencies calls [callee; caller] with
    | Error error -> Error $"Unexpected direct-call construction failure: {error}"
    | Ok ([_; constructedCaller] as constructed) ->
        let block = ConstructHIRFunctions.body constructedCaller.Body
        let orderedArguments =
            match block.Operations with
            | [HIR.Leaf (ConstructHIRFunctions.Literal (_, ConstructHIRFunctions.Int64Literal 1L));
               HIR.Leaf (ConstructHIRFunctions.Literal (_, ConstructHIRFunctions.Int64Literal 2L));
               HIR.Call _] ->
                true
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
        callFunction "callee" (parameter "value" AST.TInt64) [] (local "value")
    let caller =
        callFunction
            "caller"
            (parameter "value" AST.TInt64)
            []
            (CheckedAST.Call (
                TestIds.functionIdForName "callee",
                AST.NonEmptyList.singleton (local "value")))
    match ConstructHIRFunctions.constructFunctions infer dependencies noCalls [callee; caller] with
    | Ok [_; constructedCaller] ->
        let block = ConstructHIRFunctions.body constructedCaller.Body
        match block.Parameters, block.Operations with
        | [parameter], [HIR.ScalarBinding (_, operand)]
            when operand.Expression = caller.Body
                 && operand.Inputs = Map.ofList [(binding "value", parameter.Value)] -> Ok ()
        | _ -> Error $"Uncontracted direct call did not remain an opaque checked operand: {block}"
    | Ok actual -> Error $"Expected two constructed functions, got {actual}"
    | Error error -> Error $"Unexpected opaque-call construction failure: {error}"

let private testRejectsInvalidCallAliasContract () =
    let callee =
        callFunction "callee" (parameter "value" AST.TInt64) [] (local "value")
    let caller =
        callFunction
            "caller"
            (parameter "value" AST.TInt64)
            []
            (CheckedAST.Call (
                TestIds.functionIdForName "callee",
                AST.NonEmptyList.singleton (local "value")))
    let calls = contractedCalls true
    match ConstructHIRFunctions.constructFunctions infer dependencies calls [callee; caller] with
    | Error error -> Error $"Unexpected direct-call construction failure: {error}"
    | Ok constructed ->
        match VerifyHIR.verifyFunctions (ConstructHIRFunctions.verificationDialect calls) constructed with
        | Error (VerifyHIR.InvalidAliasSource _) -> Ok ()
        | actual -> Error $"Expected invalid call alias provenance, got {actual}"

let private scalarFunction
    name
    parameter
    returnType
    body
    : CheckedAST.FunctionDef =
    {
        Id = TestIds.functionIdForName name
        Name = name
        TypeParams = []
        Params = AST.NonEmptyList.singleton parameter
        ReturnType = returnType
        Body = body
        Recursion = None
    }

let private testNormalizesScalarPrimitivesWithContracts () =
    let definition =
        scalarFunction
            "calculate"
            (parameter "unit" AST.TUnit)
            AST.TInt64
            (CheckedAST.BinOp (
                AST.Div,
                CheckedAST.BinOp (
                    AST.Add,
                    CheckedAST.Int64Literal 4L,
                    CheckedAST.UnaryOp (AST.Neg, CheckedAST.Int64Literal 2L)),
                CheckedAST.Int64Literal 3L))
    match ConstructHIRFunctions.constructFunction infer dependencies noCalls definition with
    | Error error -> Error $"Unexpected scalar primitive construction failure: {error}"
    | Ok constructed ->
        let block = ConstructHIRFunctions.body constructed.Body
        match block.Operations with
        | [HIR.Leaf (ConstructHIRFunctions.Literal (four, ConstructHIRFunctions.Int64Literal 4L));
           HIR.Leaf (ConstructHIRFunctions.Literal (two, ConstructHIRFunctions.Int64Literal 2L));
           HIR.Leaf (ConstructHIRFunctions.Unary (negative, AST.Neg, unaryInput));
           HIR.Leaf (ConstructHIRFunctions.Binary (sum, AST.Add, addLeft, addRight));
           HIR.Leaf (ConstructHIRFunctions.Literal (three, ConstructHIRFunctions.Int64Literal 3L));
           HIR.Leaf (ConstructHIRFunctions.Binary (quotient, AST.Div, dividend, divisor))] ->
            let additionContract =
                ConstructHIRFunctions.primitiveContract
                    (ConstructHIRFunctions.Binary (sum, AST.Add, addLeft, addRight))
            let divisionContract =
                ConstructHIRFunctions.primitiveContract
                    (ConstructHIRFunctions.Binary (quotient, AST.Div, dividend, divisor))
            let outputIsUnmanaged contract expected =
                contract.Outputs = [{ Value = expected; Alias = HIR.NoManagedAlias }]
            let orderedValues =
                unaryInput = two
                && addLeft = four
                && addRight = negative
                && dividend = sum
                && divisor = three
                && block.Result = quotient
            let exactContracts =
                additionContract.Inputs = [four; negative]
                && Set.isEmpty additionContract.Effects
                && outputIsUnmanaged additionContract sum
                && divisionContract.Inputs = [sum; three]
                && divisionContract.Effects = Set.singleton HIR.MayFail
                && outputIsUnmanaged divisionContract quotient
            let verified =
                (VerifyHIR.verifyFunctions
                    (ConstructHIRFunctions.verificationDialect noCalls)
                    [constructed]) = Ok ()
            if orderedValues && exactContracts && verified then Ok ()
            else Error $"Scalar primitive values or contracts were inconsistent: {block}"
        | _ -> Error $"Expected ordered literal, unary, arithmetic, and division leaves, got {block}"

let private testRestoresLexicalScopeBetweenPrimitiveOperands () =
    let definition =
        scalarFunction
            "shadow"
            (parameter "value" AST.TInt64)
            AST.TInt64
            (CheckedAST.BinOp (
                AST.Add,
                CheckedAST.Let (
                    variable "value",
                    CheckedAST.Int64Literal 1L,
                    local "value"),
                local "value"))
    match ConstructHIRFunctions.constructFunction infer dependencies noCalls definition with
    | Error error -> Error $"Unexpected scoped primitive construction failure: {error}"
    | Ok constructed ->
        let block = ConstructHIRFunctions.body constructed.Body
        match block.Parameters, block.Operations with
        | [parameter],
          [HIR.Leaf (ConstructHIRFunctions.Literal (inner, ConstructHIRFunctions.Int64Literal 1L));
           HIR.Leaf (ConstructHIRFunctions.Binary (_, AST.Add, left, right))]
            when left = inner && right = parameter.Value -> Ok ()
        | _ -> Error $"Primitive operands did not restore their outer lexical scope: {block}"

let private testNormalizesUnsignedBitwiseNot () =
    let definition =
        scalarFunction
            "invert"
            (parameter "value" AST.TUInt64)
            AST.TUInt64
            (CheckedAST.UnaryOp (AST.BitNot, local "value"))
    match ConstructHIRFunctions.constructFunction infer dependencies noCalls definition with
    | Error error -> Error $"Unexpected unsigned primitive construction failure: {error}"
    | Ok constructed ->
        let block = ConstructHIRFunctions.body constructed.Body
        match block.Parameters, block.Operations with
        | [parameter], [HIR.Leaf (ConstructHIRFunctions.Unary (result, AST.BitNot, operand))]
            when operand = parameter.Value && result = block.Result -> Ok ()
        | _ -> Error $"Unsigned bitwise operation was not normalized: {block}"

let private testKeepsUnsupportedPrimitivesOpaque () =
    let managedBody =
        CheckedAST.BinOp (
            AST.StringConcat,
            local "value",
            CheckedAST.StringLiteral "suffix")
    let managedDefinition =
        scalarFunction "append" (parameter "value" AST.TString) AST.TString managedBody
    let arbitraryPrecisionBody =
        CheckedAST.BinOp (
            AST.Add,
            CheckedAST.BigIntLiteral 1I,
            CheckedAST.BigIntLiteral 2I)
    let arbitraryPrecisionDefinition =
        scalarFunction "addInts" (parameter "unit" AST.TUnit) AST.TInt arbitraryPrecisionBody
    let opaqueBlock definition expectedExpression (expectedInputNames: Set<string>) =
        match ConstructHIRFunctions.constructFunction infer dependencies noCalls definition with
        | Error error -> Error $"Unexpected opaque primitive construction failure: {error}"
        | Ok constructed ->
            let block = ConstructHIRFunctions.body constructed.Body
            let expectedInputs =
                block.Parameters
                |> List.choose (fun parameter ->
                    if Set.contains parameter.Binding (expectedInputNames |> Set.map binding) then
                        Some (parameter.Binding, parameter.Value)
                    else None)
                |> Map.ofList
            match block.Operations with
            | [HIR.ScalarBinding (_, operand)]
                when operand.Expression = expectedExpression
                     && operand.Inputs = expectedInputs -> Ok ()
            | _ -> Error $"Unsupported primitive did not remain one opaque evaluation: {block}"
    match
        opaqueBlock managedDefinition managedBody (Set.singleton "value"),
        opaqueBlock arbitraryPrecisionDefinition arbitraryPrecisionBody Set.empty
    with
    | Ok (), Ok () -> Ok ()
    | Error error, _ | _, Error error -> Error error

let tests = [
    "Checked functions construct ordered structured HIR", testConstructsOrderedStructuredFunction
    "Checked function construction reports scoped inference failures", testReportsBindingInferenceFailure
    "Ownership scheduling can conservatively retain inference-resistant checked functions", testFallsBackToOpaqueCheckedFunctionForScheduling
    "Contracted direct calls preserve argument evaluation order", testNormalizesContractedCallsInArgumentOrder
    "Uncontracted direct calls remain opaque", testKeepsUncontractedCallsOpaque
    "Direct-call alias contracts remain verifier-authoritative", testRejectsInvalidCallAliasContract
    "Native scalar primitives expose ordered effects and aliases", testNormalizesScalarPrimitivesWithContracts
    "Primitive operands restore their outer lexical scope", testRestoresLexicalScopeBetweenPrimitiveOperands
    "Unsigned bitwise primitives are normalized", testNormalizesUnsignedBitwiseNot
    "Managed and call-lowered primitives remain opaque", testKeepsUnsupportedPrimitivesOpaque
]
