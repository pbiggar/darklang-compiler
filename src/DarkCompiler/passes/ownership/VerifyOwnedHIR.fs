// VerifyOwnedHIR.fs - Jointly verify typed HIR and its independent ownership boundary.

module VerifyOwnedHIR

type HIRContracts<'leaf> = {
    Leaf: 'leaf -> HIR.PrimitiveContract
    CallSignature: string -> HIR.FunctionSignature option
    CallContract: HIR.FunctionCall -> HIR.PrimitiveContract option
}

type VerificationError<'id when 'id: comparison> =
    | HIRVerificationFailed of VerifyHIR.VerificationError
    | OwnershipVerificationFailed of OwnedIR.VerificationError<'id>

let private hirBody
    (block: OwnedIR.Block<'leaf, 'id>)
    : HIR.Block<HIR.Operation<'leaf, OwnedIR.Block<'leaf, 'id>>> =
    {
        Parameters = block.Body.Parameters
        Operations =
            block.Body.Operations
            |> List.choose (function
                | OwnedIR.Evaluate operation -> Some operation
                | OwnedIR.Dup _ | OwnedIR.Drop _ -> None)
        Result = block.Body.Result
    }

/// HIR contracts remain authoritative for types, effects, and aliases;
/// ownership contracts account only for unit transfer and uniqueness.
let verifyFunctions
    (hir: HIRContracts<'leaf>)
    (ownership: OwnedIR.Semantics<'leaf, 'id>)
    (functions: OwnedIR.Function<'leaf, 'id> list) =
    let dialect : VerifyHIR.Dialect<'leaf, OwnedIR.Block<'leaf, 'id>> = {
        Body = hirBody
        Leaf = hir.Leaf
        CallSignature = hir.CallSignature
        CallContract = hir.CallContract
    }
    let definitions = functions |> List.map (fun functionDefinition -> functionDefinition.Definition)
    VerifyHIR.verifyFunctions dialect definitions
    |> Result.mapError HIRVerificationFailed
    |> Result.bind (fun () ->
        VerifyOwnership.verifyFunctions ownership functions
        |> Result.mapError OwnershipVerificationFailed)
