// OwnedIR.fs - Structured region ownership and explicit unit-transfer contracts.

module OwnedIR

type Input<'id> = Borrowed of 'id | Consumed of 'id

type BlockArgument<'id> = Unmanaged | Managed of 'id

/// Function parameters retain every typed call position. Unmanaged positions
/// stay outside ownership accounting; borrowed parameters remain owned by the
/// caller; consumed parameters transfer one unit without proving that external
/// aliases are absent; unique parameters additionally establish exclusivity
/// provenance.
type ParameterOwnership<'id> =
    | UnmanagedParameter
    | BorrowedParameter of 'id
    | ConsumedParameter of 'id
    | UniqueParameter of 'id

/// A borrowed result transfers no unit, while a produced result transfers one
/// unit to the caller. A unique produced result additionally certifies that no
/// aliases remain. HIR alias provenance remains a separate semantic contract.
type ResultOwnership<'id> =
    | UnmanagedResult
    | BorrowedResult of 'id
    | ProducedResult of 'id
    | UniqueProducedResult of 'id

type FunctionSignature<'id> = {
    Parameters: ParameterOwnership<'id> list
    Result: ResultOwnership<'id>
}

/// Call-site modes include unmanaged positions so they align exactly with the
/// typed HIR signature. A borrowed result names the borrowed parameter whose
/// ownership identity it aliases; produced results transfer a fresh unit, with
/// unique modes carrying the exclusivity certificate across the call boundary.
type CallParameterOwnership =
    | UnmanagedCallParameter
    | BorrowedCallParameter
    | ConsumedCallParameter
    | UniqueCallParameter

type CallResultOwnership =
    | UnmanagedCallResult
    | BorrowedCallResult of parameterIndex: int
    | ProducedCallResult
    | UniqueProducedCallResult

type CallSignature = {
    Parameters: CallParameterOwnership list
    Result: CallResultOwnership
}

/// Lists retain multiplicity: a duplicated unit can satisfy two consuming
/// inputs, while duplicate value definitions remain invalid.
type Contract<'id> = {
    Inputs: Input<'id> list
    Outputs: 'id list
}

/// Exclusivity provenance is independent of unit transfer. Required inputs
/// must have one local unit and no untracked aliases. Unique outputs establish
/// that fact for fresh storage or a verified ownership-preserving transfer.
type UniquenessContract<'id when 'id: comparison> = {
    RequiredInputs: Set<'id>
    UniqueOutputs: Set<'id>
}

/// Ownership actions are ordered alongside evaluation. Dup creates one
/// additional unit for an accessible identity; Drop destroys one owned unit.
/// Evaluation contracts may borrow or consume units and produce fresh ones.
type Step<'leaf, 'id> =
    | Evaluate of HIR.Operation<'leaf, Block<'leaf, 'id>>
    | Dup of 'id
    | Drop of 'id
and Block<'leaf, 'id> = {
    Body: HIR.Block<Step<'leaf, 'id>>
}

/// A dialect must describe every access and possible escape, including opaque
/// scalar operands and typed block results. A managed result transfers its
/// ownership identity to the branch target; Unmanaged means the value has no
/// ownership unit.
type Semantics<'leaf, 'id when 'id: comparison> = {
    Leaf: 'leaf -> Contract<'id>
    LeafUniqueness: 'leaf -> UniquenessContract<'id>
    CallOwnership: HIR.FunctionCall -> CallSignature option
    ScalarUses: HIR.Operand -> Set<'id>
    ScalarEscapes: HIR.Operand -> Set<'id>
    BlockArgument: HIR.Value -> BlockArgument<'id>
}

type VerificationError<'id when 'id: comparison> =
    | InvalidUse of 'id
    | InvalidDrop of 'id
    | NonUniqueUse of 'id
    | InvalidUniquenessContract of 'id
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
    | UndroppedValues of Set<'id>
