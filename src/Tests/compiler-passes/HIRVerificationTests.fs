// HIRVerificationTests.fs - Normalized value identity and structured-edge verifier laws.

module HIRVerificationTests

type private TestBlock = TestBlock of HIR.Block<HIR.Operation<HIR.PrimitiveContract, TestBlock>>

let private value id typ : HIR.Value = { Id = HIR.ValueId id; Type = typ }
let private literal typ expression : HIR.Operand =
    { Expression = expression; Type = typ; Inputs = Map.empty }
let private reference name input typ : HIR.Operand =
    { Expression = CheckedAST.Var name; Type = typ; Inputs = Map.ofList [name, input] }
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
    let dialect: VerifyHIR.Dialect<HIR.PrimitiveContract, TestBlock> = {
        Body = fun (TestBlock body) -> body
        Leaf = id
    }
    VerifyHIR.verify dialect root
let private check expected root () =
    let actual = verify root
    if actual = expected then Ok () else Error $"Expected {expected}, got {actual}"

let tests = [
    let parameter = value 0 AST.TInt64
    let result = value 1 AST.TInt64
    let condition = literal AST.TBool (CheckedAST.BoolLiteral true)
    let branchResult = value 2 AST.TInt64
    let branchLocal = value 3 AST.TInt64
    let managedInput = value 5 (AST.TList AST.TInt64)
    let managedResult = value 6 (AST.TList AST.TInt64)

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
                [literal AST.TInt64 (CheckedAST.Int64Literal 1L)]
                [{ Value = result; Alias = HIR.NoManagedAlias }]
                []]
            result)
]
