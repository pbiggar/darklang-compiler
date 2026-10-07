(* ListRegion.mli - Typed closed-list region stages, identities, and array layouts. *)
type listId = HIR.valueId
type scalar = HIR.operand
type transform = Map of scalar | Reverse
type reuseSelection = StaticReuse | RuntimeReuse
type construction = Literal of scalar list | Repeat of scalar * scalar

type 'transform operation =
  | Construct of HIR.value * construction
  | Transform of HIR.value * HIR.value * 'transform
  | Fold of HIR.value * HIR.value * scalar * scalar

type functionalBlock =
  | FunctionalBlock of
      ((transform * reuseSelection) operation, functionalBlock) HIR.operation
      HIR.block

type functionalRegion = FunctionalRegion of functionalBlock
type arrayExtent = ConstantLength of int | RuntimeLength of listId

type arrayLayout =
  | RecycledArray of int
  | MappedArray of int
  | RuntimeArray of listId

val extent : arrayLayout -> arrayExtent
val elementOffset : int -> int
val payloadSize : int -> int
val allocationSize : int -> int
val recycledCapacityLimit : int
val maxCapacity : int

type allocationBytes = {
  constantBytes : int64;
  runtimeBuffers : int64 HIR.ValueMap.t;
}

val constantBytes : int64 -> allocationBytes
val requestedBytes : arrayLayout -> allocationBytes
val addBytes : allocationBytes -> allocationBytes -> allocationBytes

type storageRegion =
  | StorageRegion of functionalRegion * arrayLayout HIR.ValueMap.t

type ownership = Consume | BorrowAndCopy | ConsumeOrCopy
type ownedOperation = ((transform * ownership) operation, listId) OwnedIR.step
type ownedBlock = ((transform * ownership) operation, listId) OwnedIR.block
type ownedRegion = OwnedRegion of ownedBlock * arrayLayout HIR.ValueMap.t

type allocationSummary = {
  allocations : int;
  allocatedBytes : allocationBytes;
  copies : int;
  reusedTransforms : int;
  releases : int;
}

type allocationBudget =
  | Complete of allocationSummary
  | Conditional of
      allocationSummary * allocationBudget * allocationBudget * allocationBudget
  | RuntimeConditional of
      allocationSummary * allocationBudget * allocationBudget * allocationBudget

val lookup : string -> listId -> 'a HIR.ValueMap.t -> 'a

val primitiveContract :
  (transform * reuseSelection) operation -> HIR.primitiveContract

val immediate : AST.semanticType -> bool
