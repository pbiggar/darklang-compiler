// HIR.fs - Normalized values and typed structured control flow shared by semantic dialects.
// Source expressions remain opaque evaluation payloads. Their lexical inputs
// are explicit value identities, but they carry no purity or aliasing claim.

module HIR

type ValueId = ValueId of int

type Value = {
    Id: ValueId
    Type: AST.Type
}

type Operand = {
    Expression: CheckedAST.Expr
    Type: AST.Type
    Inputs: Map<string, Value>
}

/// Function signatures describe only the typed call boundary. Effects and
/// alias provenance remain separate primitive contracts, while ownership is
/// supplied by OwnedIR.
type FunctionSignature = {
    Parameters: AST.Type list
    Result: AST.Type
}

/// Only resolved direct calls enter normalized HIR. Unknown and indirect calls
/// remain inside opaque scalar evaluation until their boundaries are known.
type FunctionCall = {
    Target: string
    Arguments: Value list
    Result: Value
}

type Operation<'leaf, 'block> =
    | Leaf of 'leaf
    | ScalarBinding of result: Value * value: Operand
    | Call of FunctionCall
    | Branch of result: Value * condition: Operand * ifTrue: 'block * ifFalse: 'block

/// Parameters retain source names for operand resolution and declaration order
/// for function-call signature alignment. Value identities remain the semantic
/// authority after construction.
type Parameter = {
    Name: string
    Value: Value
}

/// Each branch produces the binding consumed by the enclosing continuation.
/// Keeping the continuation in this sequence avoids duplicating it per path.
type Block<'operation> = {
    Parameters: Parameter list
    Operations: 'operation list
    Result: Value
}

/// A normalized function owns one ordered entry block. Its typed signature is
/// derived from the parameter and result values rather than duplicated here.
type Function<'block> = {
    Name: string
    Body: 'block
}

/// Effects constrain reordering independently of ownership. Owned-storage
/// reads and writes describe compiler-selected representations, not visible
/// source mutation.
type PrimitiveEffect =
    | MayEvaluateOpaqueSource
    | MayAllocate
    | MayFail
    | MayInvokeUserCode
    | ReadsOwnedStorage
    | WritesOwnedStorage

/// Alias provenance is a storage-selection capability, not a mutation
/// guarantee. MayReuseInput permits either fresh storage or ownership transfer.
type ResultAlias =
    | NoManagedAlias
    | FreshManaged
    | MayReuseInput of Value
    | MayAliasInputs of first: Value * rest: Value list

type OutputContract = {
    Value: Value
    Alias: ResultAlias
}

/// A leaf interface exposes ordered value edges, opaque scalar operands,
/// execution effects, and result provenance without assigning ownership.
type PrimitiveContract = {
    Inputs: Value list
    Operands: Operand list
    Outputs: OutputContract list
    Effects: Set<PrimitiveEffect>
}

let managedOutputs contract =
    contract.Outputs
    |> List.choose (fun output ->
        match output.Alias with
        | NoManagedAlias -> None
        | FreshManaged | MayReuseInput _ | MayAliasInputs _ -> Some output.Value)
