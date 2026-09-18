// VerifyListOwnership.fs - Independently verify region ownership, layouts, and typed edges.

module VerifyListOwnership

open HIR
open OwnedIR
open ListRegion

/// Extraction proves scalars and callbacks cannot access the canonical list
/// identities. List-specific storage facts remain here; accounting is shared.
let private semantics : Semantics<Operation<Transform * Ownership>, ListId> = {
    Leaf = function
        | Construct (output, _) -> { Inputs = []; Outputs = [output.Id] }
        | Transform (output, input, (_, ownership)) ->
            let useMode = match ownership with Consume -> Consumed input.Id | BorrowAndCopy -> Borrowed input.Id
            { Inputs = [useMode]; Outputs = [output.Id] }
        | Fold (_, input, _, _) -> { Inputs = [Borrowed input.Id]; Outputs = [] }
    CallOwnership = fun _ -> None
    ScalarUses = fun operand ->
        operand.Inputs
        |> Map.values
        |> Seq.choose (fun value -> if value.Type = AST.TList AST.TInt64 then Some value.Id else None)
        |> Set.ofSeq
    BlockArgument = fun value ->
        if value.Type = AST.TList AST.TInt64 then Managed value.Id else Unmanaged
}

let verifyBlockOwnership (block: OwnedBlock) : Result<unit, string> =
    VerifyOwnership.verifyClosed semantics block
    |> Result.mapError (fun error -> $"List HIR: {error}")

let verify (OwnedRegion (block, layouts)) : Result<unit, string> =
    let rec blockValid block = immediate block.Body.Result.Type && List.forall typesValid block.Body.Operations
    and typesValid step =
        match step with
        | Dup _ -> false
        | Drop id -> Map.containsKey id layouts
        | Evaluate (Leaf (Construct (output, Literal elements))) ->
            elements |> List.forall (fun element -> element.Type = AST.TInt64)
            && output.Type = AST.TList AST.TInt64
            && extent (lookup "construction layout" output.Id layouts) = ConstantLength (List.length elements)
        | Evaluate (Leaf (Construct (output, Repeat (count, value)))) ->
            count.Type = AST.TInt && value.Type = AST.TInt64
            && output.Type = AST.TList AST.TInt64
            && extent (lookup "repeat layout" output.Id layouts) = RuntimeLength output.Id
        | Evaluate (Leaf (Transform (output, input, (operation, _)))) ->
            output.Type = AST.TList AST.TInt64 && input.Type = AST.TList AST.TInt64
            && lookup "output layout" output.Id layouts = lookup "input layout" input.Id layouts
            && (match operation with
                | Map callback -> callback.Type = AST.TFunction ([AST.TInt64], AST.TInt64)
                | Reverse -> true)
        | Evaluate (Leaf (Fold (output, input, initial, callback))) ->
            output.Type = AST.TInt64 && input.Type = AST.TList AST.TInt64
            && initial.Type = AST.TInt64 && callback.Type = AST.TFunction ([AST.TInt64; AST.TInt64], AST.TInt64)
        | Evaluate (ScalarBinding (output, value)) -> output.Type = value.Type && immediate value.Type
        | Evaluate (Call _) -> false
        | Evaluate (Branch (_, condition, yes, no)) ->
            condition.Type = AST.TBool && yes.Body.Result.Type = no.Body.Result.Type
            && blockValid yes && blockValid no
    if layouts |> Map.exists (fun _ layout ->
        match layout with
        | RecycledArray length -> length < 0 || length > recycledCapacityLimit
        | MappedArray length -> length <= recycledCapacityLimit || length > maxCapacity
        | RuntimeArray _ -> false) then
        Error "List HIR: unsupported allocation size"
    elif not (blockValid block) then
        Error "List HIR: invalid storage operand types"
    else verifyBlockOwnership block
