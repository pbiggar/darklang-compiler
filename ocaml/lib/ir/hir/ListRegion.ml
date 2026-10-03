(*
   Exact piecewise requested-byte budget, excluding OS page rounding.
   Each runtime buffer costs 256 bytes for n <= 28, otherwise 40 + 8*n.
   Terms count physical buffers with the same validated construction extent.
*)
(* ListRegion.fs - Typed closed-list region stages, identities, and array layouts. *)
[@@@warning "-4"]
module H = HIR
module M = H.ValueMap
let add left right = Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))
let mul left right = Int32.to_int (Int32.mul (Int32.of_int left) (Int32.of_int right))
type listId = H.valueId
type scalar = H.operand
type transform = Map of scalar | Reverse
type reuseSelection = StaticReuse | RuntimeReuse
type construction = Literal of scalar list | Repeat of scalar * scalar
type 'transform operation = Construct of H.value * construction | Transform of H.value * H.value * 'transform | Fold of H.value * H.value * scalar * scalar
type functionalBlock = FunctionalBlock of ((transform * reuseSelection) operation, functionalBlock) H.operation H.block
type functionalRegion = FunctionalRegion of functionalBlock
(*
   Runtime extent identity survives aliases and consuming transformations.
   It names a construction, never a source variable that could be rebound.
*)
type arrayExtent = ConstantLength of int | RuntimeLength of listId
type arrayLayout = RecycledArray of int | MappedArray of int | RuntimeArray of listId
let extent = function RecycledArray length | MappedArray length -> ConstantLength length | RuntimeArray origin -> RuntimeLength origin
let elementOffset index = add 24 (mul index 8)
let payloadSize length = elementOffset length
let allocationSize length = add (payloadSize length) 8
let recycledCapacityLimit = 28
(*
   Literal offsets remain representable in signed 32-bit layout metadata.
   Runtime extents instead use checked 64-bit arithmetic in the constructor.
*)
let maxCapacity = (2147483647 - 40) / 8
 type allocationBytes = {constantBytes : int64; runtimeBuffers : int64 M.t}
let constantBytes value = {constantBytes = value; runtimeBuffers = M.empty}
let requestedBytes = function
 | RecycledArray length -> constantBytes (Int64.of_int (allocationSize length))
 | MappedArray length -> constantBytes (Int64.add (Int64.of_int (allocationSize length)) 8L)
 | RuntimeArray origin -> {constantBytes = 0L; runtimeBuffers = M.singleton origin 1L}
let addBytes left right =
 let coefficients = M.fold (fun origin coefficient terms -> M.update origin (fun current -> Some (Int64.add coefficient (Option.value current ~default:0L))) terms) right.runtimeBuffers left.runtimeBuffers in
 {constantBytes = Int64.add left.constantBytes right.constantBytes; runtimeBuffers = coefficients}
type storageRegion = StorageRegion of functionalRegion * arrayLayout M.t
(*
   Consume transfers statically exclusive storage. BorrowAndCopy preserves a
   statically visible source version. ConsumeOrCopy transfers one ownership
   unit and asks the runtime RC whether the storage itself is exclusive.
*)
type ownership = Consume | BorrowAndCopy | ConsumeOrCopy
type ownedOperation = ((transform * ownership) operation, listId) OwnedIR.step
type ownedBlock = ((transform * ownership) operation, listId) OwnedIR.block
type ownedRegion = OwnedRegion of ownedBlock * arrayLayout M.t
type allocationSummary = {allocations : int; allocatedBytes : allocationBytes; copies : int; reusedTransforms : int; releases : int}
(*
   Preserve alternatives and their continuation without enumerating paths or
   adding mutually exclusive costs. Callback/scalar allocations are excluded.
*)
type allocationBudget = Complete of allocationSummary | Conditional of allocationSummary * allocationBudget * allocationBudget * allocationBudget | RuntimeConditional of allocationSummary * allocationBudget * allocationBudget * allocationBudget
let lookup name key map = match M.find_opt key map with Some value -> value | None -> let H.ValueId id = key in Crash.crash ("List HIR: missing " ^ name ^ " for " ^ HostStructuralFormat.format (StructuralValue.Union ("ValueId", [StructuralValue.Scalar (string_of_int id)])))
let primitiveContract (operation : (transform * reuseSelection) operation) : H.primitiveContract =
 let output value alias : H.outputContract = {H.value = value; alias} in
 let effects values = H.EffectSet.of_list values in
 match operation with
 | Construct (result, Literal elements) -> {H.inputs = []; operands = elements; outputs = [output result H.FreshManaged]; effects = effects [H.MayEvaluateOpaqueSource;H.MayAllocate]}
 | Construct (result, Repeat (count, value)) -> {H.inputs = []; operands = [count;value]; outputs = [output result H.FreshManaged]; effects = effects [H.MayEvaluateOpaqueSource;H.MayAllocate;H.MayFail]}
 | Transform (result, source, (Map callback, _)) -> {H.inputs = [source]; operands = [callback]; outputs = [output result (H.MayReuseInput source)]; effects = effects [H.MayEvaluateOpaqueSource;H.MayAllocate;H.MayInvokeUserCode;H.ReadsOwnedStorage;H.WritesOwnedStorage]}
 | Transform (result, source, (Reverse, _)) -> {H.inputs = [source]; operands = []; outputs = [output result (H.MayReuseInput source)]; effects = effects [H.MayAllocate;H.ReadsOwnedStorage;H.WritesOwnedStorage]}
 | Fold (result, source, initial, callback) -> {H.inputs = [source]; operands = [initial;callback]; outputs = [output result H.NoManagedAlias]; effects = effects [H.MayEvaluateOpaqueSource;H.MayInvokeUserCode;H.ReadsOwnedStorage]}
let immediate = function AST.TInt64 | AST.TBool -> true | _ -> false
