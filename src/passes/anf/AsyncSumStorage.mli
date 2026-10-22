(* AsyncSumStorage.mli - Describe per-constructor payload layouts and their physical sum metadata. *)
type layout = {
  source : ANF.sumDescriptor;
  payloads : (int * AST.semanticType) list;
  emptyTags : int list;
  unaryTags : int list;
}

val layout :
  MemoryModel.rcSumShapeRegistry -> ANF.sumDescriptor -> (layout, string) result

val payloadIndex : layout -> int -> int option

val physical :
  layout ->
  AST.semanticType list ->
  ANF.sumDescriptor * MemoryModel.rcSumShapeInfo

(* Recursive components reserve nominal identities before resolving payloads. *)
val physicalNamed :
  name:string ->
  layout ->
  AST.semanticType list ->
  ANF.sumDescriptor * MemoryModel.rcSumShapeInfo

val representationIndex :
  MemoryModel.rcSumShapeRegistry -> LoweringPrimitives.sumRepresentationIndex

val includePhysicalShapes :
  into:MemoryModel.rcSumShapeRegistry ->
  MemoryModel.rcSumShapeRegistry ->
  (MemoryModel.rcSumShapeRegistry, string) result

val physicalVariantLookup :
  MemoryModel.rcSumShapeRegistry -> LoweringPrimitives.variantLookup
