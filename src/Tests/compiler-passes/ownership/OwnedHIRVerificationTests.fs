// OwnedHIRVerificationTests.fs - Joint typed, effect, alias, and ownership boundary laws.

module OwnedHIRVerificationTests

open OwnedIR

type private TestLeaf = TestLeaf

let private parameter : HIR.Value = { Id = HIR.ValueId 0; Type = AST.TInt64 }
let private alias : HIR.Value = { Id = HIR.ValueId 3; Type = AST.TInt64 }

let private call target result =
    Evaluate (HIR.Call { Target = target; Arguments = [parameter]; Result = result })

let private block operations result : Block<TestLeaf, string> = {
    Body = {
        Parameters = [{ Name = "value"; Value = parameter }]
        Operations = operations
        Result = result
    }
}

let private ownedFunction name operations result : Function<TestLeaf, string> = {
    Definition = { Name = name; Body = block operations result }
    Ownership = {
        Parameters = [BorrowedParameter "value"]
        Result = BorrowedResult "value"
    }
}

let private callContract (call: HIR.FunctionCall) : HIR.PrimitiveContract = {
    Inputs = call.Arguments
    Operands = []
    Outputs = [{ Value = call.Result; Alias = HIR.MayAliasInputs (parameter, []) }]
    Effects = Set.singleton HIR.MayInvokeUserCode
}

let private hirContracts includeCallContract : VerifyOwnedHIR.HIRContracts<TestLeaf> = {
    Leaf = fun TestLeaf -> {
        Inputs = []
        Operands = []
        Outputs = []
        Effects = Set.empty
    }
    CallSignature = fun _ -> None
    CallContract = fun call -> if includeCallContract then Some (callContract call) else None
}

let private ownership registered : Semantics<TestLeaf, string> = {
    Leaf = fun TestLeaf -> { Inputs = []; Outputs = [] }
    LeafUniqueness = fun TestLeaf -> { RequiredInputs = Set.empty; UniqueOutputs = Set.empty }
    CallOwnership = registered
    ScalarUses = fun operand -> operand.Inputs |> Map.keys |> Set.ofSeq
    ScalarEscapes = fun _ -> Set.empty
    BlockArgument = fun value ->
        match value.Id with
        | HIR.ValueId 0 | HIR.ValueId 3 -> Managed "value"
        | _ -> Unmanaged
}

let private verify hir ownership functions () =
    match VerifyOwnedHIR.verifyFunctions hir ownership functions with
    | Ok () -> Ok ()
    | Error error -> Error $"Unexpected owned HIR verification failure: {error}"

let private testDirectFunctionGroup () =
    let callee = ownedFunction "callee" [] parameter
    let caller = ownedFunction "caller" [call "callee" alias] alias
    verify (hirContracts true) (ownership (fun _ -> None)) [callee; caller] ()

let private testRecursiveFunctionGroup () =
    let recursive = ownedFunction "recursive" [call "recursive" alias] alias
    verify (hirContracts true) (ownership (fun _ -> None)) [recursive] ()

let private testRequiresIndependentCallContract () =
    let recursive = ownedFunction "recursive" [call "recursive" alias] alias
    let actual =
        VerifyOwnedHIR.verifyFunctions
            (hirContracts false)
            (ownership (fun _ -> None))
            [recursive]
    let expected =
        Error (VerifyOwnedHIR.HIRVerificationFailed (VerifyHIR.MissingCallContract "recursive"))
    if actual = expected then Ok () else Error $"Expected {expected}, got {actual}"

let private testRejectsPositionalOwnershipMismatch () =
    let invalid = {
        ownedFunction "invalid" [] parameter with
            Ownership = { Parameters = [UnmanagedParameter]; Result = UnmanagedResult }
    }
    let actual =
        VerifyOwnedHIR.verifyFunctions
            (hirContracts true)
            (ownership (fun _ -> None))
            [invalid]
    let expected =
        Error (VerifyOwnedHIR.OwnershipVerificationFailed InconsistentFunctionParameters)
    if actual = expected then Ok () else Error $"Expected {expected}, got {actual}"

let private testRejectsConflictingOwnershipRegistration () =
    let recursive = ownedFunction "recursive" [call "recursive" alias] alias
    let registered (call: HIR.FunctionCall) =
        if call.Target = "recursive" then
            Some { Parameters = [ConsumedCallParameter]; Result = ProducedCallResult }
        else None
    let actual =
        VerifyOwnedHIR.verifyFunctions
            (hirContracts true)
            (ownership registered)
            [recursive]
    let expected =
        Error (
            VerifyOwnedHIR.OwnershipVerificationFailed (
                InconsistentRegisteredCallOwnership "recursive"))
    if actual = expected then Ok () else Error $"Expected {expected}, got {actual}"

let tests = [
    "Owned HIR derives direct-call ownership from function definitions", testDirectFunctionGroup
    "Owned HIR derives recursive-call ownership from function definitions", testRecursiveFunctionGroup
    "Owned HIR requires independent call effect and alias contracts", testRequiresIndependentCallContract
    "Owned HIR rejects positional ownership mismatches", testRejectsPositionalOwnershipMismatch
    "Owned HIR rejects conflicting ownership registrations", testRejectsConflictingOwnershipRegistration
]
