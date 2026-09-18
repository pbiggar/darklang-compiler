// HIRVerificationTests.fs - Normalized value identity and structured-edge verifier laws.

module HIRVerificationTests

type private TestBlock = TestBlock of HIR.Block<HIR.Operation<HIR.PrimitiveContract, TestBlock>>

let private value id typ : HIR.Value = { Id = HIR.ValueId id; Type = typ }
let private literal typ expression : HIR.Operand =
    { Expression = expression; Type = typ; Inputs = Map.empty }
let private reference name input typ : HIR.Operand =
    { Expression = AST.Var name; Type = typ; Inputs = Map.ofList [name, input] }
let private block parameters operations result =
    TestBlock { Parameters = parameters; Operations = operations; Result = result }
let private leaf inputs operands outputs =
    let outputs =
        outputs
        |> List.map (fun value -> ({ Value = value; Alias = HIR.NoManagedAlias }: HIR.OutputContract))
    let contract: HIR.PrimitiveContract = { Inputs = inputs; Operands = operands; Outputs = outputs; Effects = Set.empty }
    HIR.Leaf contract
let private contractedWithOperands inputs operands outputs effects =
    HIR.Leaf ({ Inputs = inputs; Operands = operands; Outputs = outputs; Effects = Set.ofList effects }: HIR.PrimitiveContract)
let private contracted inputs outputs effects = contractedWithOperands inputs [] outputs effects
let private verify root =
    let signature target =
        if target = "callee" || target = "recursive" || target = "uncontracted" then
            Some ({ Parameters = [AST.TInt64]; Result = AST.TBool }: HIR.FunctionSignature)
        else None
    let callContract (call: HIR.FunctionCall) =
        if call.Target = "callee" || call.Target = "recursive" then
            Some ({
                Inputs = call.Arguments
                Operands = []
                Outputs = [{ Value = call.Result; Alias = HIR.NoManagedAlias }]
                Effects = Set.singleton HIR.MayInvokeUserCode
            }: HIR.PrimitiveContract)
        else None
    let dialect: VerifyHIR.Dialect<HIR.PrimitiveContract, TestBlock> = {
        Body = fun (TestBlock body) -> body
        Leaf = id
        CallSignature = signature
        CallContract = callContract
    }
    VerifyHIR.verify dialect root
let private check expected root () =
    let actual = verify root
    if actual = expected then Ok () else Error $"Expected {expected}, got {actual}"

let tests = [
    let parameter = value 0 AST.TInt64
    let result = value 1 AST.TInt64
    let condition = literal AST.TBool (AST.BoolLiteral true)
    let branchResult = value 2 AST.TInt64
    let branchLocal = value 3 AST.TInt64
    let managedInput = value 5 (AST.TList AST.TInt64)
    let managedResult = value 6 (AST.TList AST.TInt64)
    let callResult = value 7 AST.TBool
    let call target arguments result = HIR.Call { Target = target; Arguments = arguments; Result = result }

    "HIR accepts normalized parameter and operand identities", check (Ok ())
        (block (Map.ofList ["input", parameter])
            [HIR.ScalarBinding (result, reference "input" parameter AST.TInt64)] result)
    "HIR rejects an operand with an unknown identity", check (Error (VerifyHIR.UnknownValue parameter.Id))
        (block Map.empty [HIR.ScalarBinding (result, reference "input" parameter AST.TInt64)] result)
    "HIR rejects sibling definitions with the same identity", check (Error (VerifyHIR.DuplicateDefinition branchLocal.Id))
        (block Map.empty
            [HIR.Branch (branchResult, condition,
                block Map.empty [leaf [] [] [branchLocal]] branchLocal,
                block Map.empty [leaf [] [] [branchLocal]] branchLocal)]
            branchResult)
    "HIR rejects a branch result whose type disagrees with its target", check (Error (VerifyHIR.InconsistentBranchResult branchResult.Id))
        (block Map.empty
            [HIR.Branch (branchResult, condition,
                block Map.empty [leaf [] [] [branchLocal]] branchLocal,
                let boolean = value 4 AST.TBool
                block Map.empty [leaf [] [] [boolean]] boolean)]
            branchResult)
    "HIR accepts a result that may reuse a typed input", check (Ok ())
        (block (Map.ofList ["input", managedInput])
            [contracted [managedInput]
                [{ Value = managedResult; Alias = HIR.MayReuseInput managedInput }]
                [HIR.ReadsOwnedStorage; HIR.WritesOwnedStorage]]
            managedResult)
    "HIR rejects reuse provenance outside primitive inputs", check (Error (VerifyHIR.InvalidAliasSource (managedResult.Id, managedInput.Id)))
        (block (Map.ofList ["input", managedInput])
            [contracted [] [{ Value = managedResult; Alias = HIR.MayReuseInput managedInput }] []]
            managedResult)
    "HIR rejects reuse provenance with incompatible types", check (Error (VerifyHIR.IncompatibleAliasTypes (result.Id, managedInput.Id)))
        (block (Map.ofList ["input", managedInput])
            [contracted [managedInput] [{ Value = result; Alias = HIR.MayReuseInput managedInput }] []]
            result)
    "HIR rejects duplicate may-alias candidates", check (Error (VerifyHIR.DuplicateAliasSource (managedResult.Id, managedInput.Id)))
        (block (Map.ofList ["input", managedInput])
            [contracted [managedInput]
                [{ Value = managedResult; Alias = HIR.MayAliasInputs (managedInput, [managedInput]) }]
                []]
            managedResult)
    "HIR rejects unaccounted opaque operand effects", check (Error VerifyHIR.UnaccountedOpaqueEffects)
        (block Map.empty
            [contractedWithOperands []
                [literal AST.TInt64 (AST.Int64Literal 1L)]
                [{ Value = result; Alias = HIR.NoManagedAlias }]
                []]
            result)
    "HIR accepts registered direct calls", check (Ok ())
        (block (Map.ofList ["input", parameter]) [call "callee" [parameter] callResult] callResult)
    "HIR resolves recursive calls through an explicit registry entry", check (Ok ())
        (block (Map.ofList ["input", parameter]) [call "recursive" [parameter] callResult] callResult)
    "HIR rejects calls without a typed registry entry", check (Error (VerifyHIR.UnknownCallTarget "opaque"))
        (block (Map.ofList ["input", parameter]) [call "opaque" [parameter] callResult] callResult)
    "HIR rejects registered calls without effect and alias contracts", check (Error (VerifyHIR.MissingCallContract "uncontracted"))
        (block (Map.ofList ["input", parameter]) [call "uncontracted" [parameter] callResult] callResult)
    "HIR rejects direct-call argument count mismatches", check (Error (VerifyHIR.InvalidCallArgumentCount "callee"))
        (block Map.empty [call "callee" [] callResult] callResult)
    "HIR rejects direct-call argument type mismatches", check (Error (VerifyHIR.InvalidCallArgumentType ("callee", 0)))
        (let boolean = value 8 AST.TBool
         block (Map.ofList ["input", boolean]) [call "callee" [boolean] callResult] callResult)
    "HIR rejects direct-call result type mismatches", check (Error (VerifyHIR.InvalidCallResultType "callee"))
        (let invalidResult = value 9 AST.TInt64
         block (Map.ofList ["input", parameter]) [call "callee" [parameter] invalidResult] invalidResult)
]
