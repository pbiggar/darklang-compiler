// ListRegion.fs - Typed closed-list region stages, identities, and array layouts.

module ListRegion

type ListId = HIR.ValueId

type Scalar = HIR.Operand

type Transform =
    | Map of callback: Scalar
    | Reverse

type ReuseSelection = StaticReuse | RuntimeReuse

type Construction =
    | Literal of elements: Scalar list
    | Repeat of count: Scalar * value: Scalar

type Operation<'transform> =
    | Construct of result: HIR.Value * construction: Construction
    | Transform of result: HIR.Value * source: HIR.Value * operation: 'transform
    | Fold of result: HIR.Value * source: HIR.Value * initial: Scalar * callback: Scalar
type FunctionalBlock = internal FunctionalBlock of HIR.Block<HIR.Operation<Operation<Transform * ReuseSelection>, FunctionalBlock>>
type FunctionalRegion = internal FunctionalRegion of FunctionalBlock

/// Runtime extent identity survives aliases and consuming transformations.
/// It names a construction, never a source variable that could be rebound.
type ArrayExtent = ConstantLength of int | RuntimeLength of ListId

type ArrayLayout = internal RecycledArray of length:int | MappedArray of length:int | RuntimeArray of origin:ListId

let extent = function
    | RecycledArray length | MappedArray length -> ConstantLength length
    | RuntimeArray origin -> RuntimeLength origin
let elementOffset index = 24 + index * 8
let payloadSize length = elementOffset length
let allocationSize length = payloadSize length + 8
let internal recycledCapacityLimit = 28
// Literal offsets remain representable in signed 32-bit layout metadata.
// Runtime extents instead use checked 64-bit arithmetic in the constructor.
let internal maxCapacity = (System.Int32.MaxValue - 40) / 8

/// Exact piecewise requested-byte budget, excluding OS page rounding.
/// Each runtime buffer costs 256 bytes for n <= 28, otherwise 40 + 8*n.
/// Terms count physical buffers with the same validated construction extent.
type AllocationBytes = {
    ConstantBytes: int64
    RuntimeBuffers: Map<ListId, int64>
}

let internal constantBytes value = { ConstantBytes = value; RuntimeBuffers = Map.empty }
let internal requestedBytes = function
    | RecycledArray length -> constantBytes (int64 (allocationSize length))
    | MappedArray length -> constantBytes (int64 (allocationSize length) + 8L)
    | RuntimeArray origin -> { ConstantBytes = 0L; RuntimeBuffers = Map.ofList [origin, 1L] }

let internal addBytes left right =
    let coefficients =
        Map.fold (fun terms origin coefficient ->
            Map.change origin (fun current -> Some (coefficient + Option.defaultValue 0L current)) terms)
            left.RuntimeBuffers right.RuntimeBuffers
    { ConstantBytes = left.ConstantBytes + right.ConstantBytes; RuntimeBuffers = coefficients }

type StorageRegion = internal StorageRegion of FunctionalRegion * Map<ListId, ArrayLayout>

/// Consume transfers statically exclusive storage. BorrowAndCopy preserves a
/// statically visible source version. ConsumeOrCopy transfers one ownership
/// unit and asks the runtime RC whether the storage itself is exclusive.
type Ownership = Consume | BorrowAndCopy | ConsumeOrCopy

type OwnedOperation = OwnedIR.Step<Operation<Transform * Ownership>, ListId>
type OwnedBlock = OwnedIR.Block<Operation<Transform * Ownership>, ListId>

type OwnedRegion = internal OwnedRegion of OwnedBlock * Map<ListId, ArrayLayout>

type AllocationSummary = {
    Allocations: int
    AllocatedBytes: AllocationBytes
    Copies: int
    ReusedTransforms: int
    Releases: int
}

/// Preserve alternatives and their continuation without enumerating paths or
/// adding mutually exclusive costs. Callback/scalar allocations are excluded.
type AllocationBudget =
    | Complete of AllocationSummary
    | Conditional of prefix: AllocationSummary * ifTrue: AllocationBudget * ifFalse: AllocationBudget * continuation: AllocationBudget
    | RuntimeConditional of prefix: AllocationSummary * exclusive: AllocationBudget * shared: AllocationBudget * continuation: AllocationBudget

let internal lookup name key map =
    match Map.tryFind key map with
    | Some value -> value
    | None -> Crash.crash $"List HIR: missing {name} for {key}"

let primitiveContract (operation: Operation<Transform * ReuseSelection>) : HIR.PrimitiveContract =
    let output value alias : HIR.OutputContract = { Value = value; Alias = alias }
    let effects values = Set.ofList values
    match operation with
    | Construct (result, Literal elements) ->
        { Inputs = []
          Operands = elements
          Outputs = [output result HIR.FreshManaged]
          Effects = effects [HIR.MayEvaluateOpaqueSource; HIR.MayAllocate] }
    | Construct (result, Repeat (count, value)) ->
        { Inputs = []
          Operands = [count; value]
          Outputs = [output result HIR.FreshManaged]
          Effects = effects [HIR.MayEvaluateOpaqueSource; HIR.MayAllocate; HIR.MayFail] }
    | Transform (result, source, (Map callback, _)) ->
        { Inputs = [source]
          Operands = [callback]
          Outputs = [output result (HIR.MayReuseInput source)]
          Effects =
              effects [HIR.MayEvaluateOpaqueSource; HIR.MayAllocate; HIR.MayInvokeUserCode
                       HIR.ReadsOwnedStorage; HIR.WritesOwnedStorage] }
    | Transform (result, source, (Reverse, _)) ->
        { Inputs = [source]
          Operands = []
          Outputs = [output result (HIR.MayReuseInput source)]
          Effects = effects [HIR.MayAllocate; HIR.ReadsOwnedStorage; HIR.WritesOwnedStorage] }
    | Fold (result, source, initial, callback) ->
        { Inputs = [source]
          Operands = [initial; callback]
          Outputs = [output result HIR.NoManagedAlias]
          Effects = effects [HIR.MayEvaluateOpaqueSource; HIR.MayInvokeUserCode; HIR.ReadsOwnedStorage] }

let internal immediate = function
    | AST.TInt64 | AST.TBool -> true
    | _ -> false
