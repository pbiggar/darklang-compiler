(* AsyncTransport.mli - Describe concrete, pending and independently pending field interfaces. *)
type storageKind =
  | TupleStorage
  | RecordStorage of ANF.recordDescriptor
  | ListStorage of AST.semanticType
  | DictStorage of AST.semanticType * AST.semanticType
  | SumStorage of AsyncSumStorage.layout

type 'a storedSlot =
  | PendingSlot of 'a
  | NestedStorage of 'a storage
  | PendingStorage of 'a * 'a storage

and 'a storage

(* Inspect only one level: callers must not depend on the representation used
   to share storage layouts or to close recursive nominal interfaces. *)
val storage : storageKind -> 'a storedSlot option list -> 'a storage
val kind : 'a storage -> storageKind
val fields : 'a storage -> 'a storedSlot option list
val field : 'a storage -> int -> 'a storedSlot option

type mode =
  | Concrete
  | Pending
  | Stored of unit storage
  | PendingStored of unit storage

val changed : mode -> bool
val slotMode : unit storedSlot option -> mode
val modeSlot : mode -> unit storedSlot option
val merge : mode -> mode -> (mode, string) result
val mergeList : mode list -> mode list -> (mode list, string) result
val mapStorage : ('a -> 'b) -> 'a storage -> 'b storage
val physicalType : mode -> AST.semanticType -> AST.semanticType

val collectSumShapes :
  into:MemoryModel.rcSumShapeRegistry ->
  'a storage ->
  (MemoryModel.rcSumShapeRegistry, string) result

val instantiateStorage :
  (AST.semanticType -> ('a, 'error) result) ->
  unit storage ->
  AST.semanticType ->
  ('a storage, 'error) result
