// OwnedIR.fs - Structured region ownership and explicit unit-transfer contracts.

module OwnedIR

type Input<'id> = Borrowed of 'id | Consumed of 'id

type BlockArgument<'id> = Unmanaged | Managed of 'id

/// Function boundaries distinguish access from ownership transfer. Borrowed
/// parameters remain owned by the caller; consumed parameters transfer one
/// ownership unit into the function.
type ParameterOwnership<'id> =
    | BorrowedParameter of 'id
    | ConsumedParameter of 'id

/// A borrowed result transfers no unit, while a produced result transfers one
/// unit to the caller. Alias provenance remains a separate HIR contract.
type ResultOwnership<'id> =
    | UnmanagedResult
    | BorrowedResult of 'id
    | ProducedResult of 'id

type FunctionSignature<'id> = {
    Parameters: ParameterOwnership<'id> list
    Result: ResultOwnership<'id>
}

/// Call-site modes include unmanaged positions so they align exactly with the
/// typed HIR signature. A borrowed result names the borrowed parameter whose
/// ownership identity it aliases; produced results transfer a fresh unit.
type CallParameterOwnership =
    | UnmanagedCallParameter
    | BorrowedCallParameter
    | ConsumedCallParameter

type CallResultOwnership =
    | UnmanagedCallResult
    | BorrowedCallResult of parameterIndex: int
    | ProducedCallResult

type CallSignature = {
    Parameters: CallParameterOwnership list
    Result: CallResultOwnership
}

/// Lists retain multiplicity: consuming one unit twice or defining a duplicate
/// identity is invalid. This is unit ownership, not general RC credit arithmetic.
type Contract<'id> = {
    Inputs: Input<'id> list
    Outputs: 'id list
}

type Step<'leaf, 'id> = {
    Operation: HIR.Operation<'leaf, Block<'leaf, 'id>>
    Releases: 'id list
}
and Block<'leaf, 'id> = {
    EntryReleases: 'id list
    Body: HIR.Block<Step<'leaf, 'id>>
}

/// A dialect must describe every access, including opaque scalar operands and
/// typed block results. A managed result transfers its ownership identity to
/// the branch target; Unmanaged means the value has no ownership unit.
type Semantics<'leaf, 'id when 'id: comparison> = {
    Leaf: 'leaf -> Contract<'id>
    CallOwnership: HIR.FunctionCall -> CallSignature option
    ScalarUses: HIR.Operand -> Set<'id>
    BlockArgument: HIR.Value -> BlockArgument<'id>
}

type VerificationError<'id when 'id: comparison> =
    | InvalidUse of 'id
    | InvalidRelease of 'id
    | DuplicateDefinition of 'id
    | DuplicateParameter of 'id
    | InconsistentFunctionParameters
    | InconsistentFunctionResult
    | InvalidBorrowedResult of 'id
    | InvalidProducedResult of 'id
    | UnknownCallOwnership of target: string
    | InconsistentCallOwnershipParameters of target: string
    | InconsistentCallOwnershipArgument of target: string * parameterIndex: int
    | InconsistentCallOwnershipResult of target: string
    | InvalidBorrowedCallResult of target: string * parameterIndex: int
    | InconsistentJoin
    | InconsistentBlockArgument
    | UnreleasedValues of Set<'id>
