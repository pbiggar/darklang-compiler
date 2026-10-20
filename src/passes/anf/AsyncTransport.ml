(* AsyncTransport.ml - Merge field interfaces without demanding their payloads. *)
[@@@warning "-4"]

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

and 'a storage = { kind : storageKind; fields : 'a storedSlot option list }

let storage kind fields = { kind; fields }
let kind storage = storage.kind
let fields storage = storage.fields

let field storage index =
  let rec find offset = function
    | [] -> Crash.crash "Async storage projection is outside its layout"
    | value :: rest -> if offset = index then value else find (offset + 1) rest
  in
  find 0 storage.fields

type mode =
  | Concrete
  | Pending
  | Stored of unit storage
  | PendingStored of unit storage

let ( let* ) = Result.bind

let changed = function
  | Concrete -> false
  | Pending | Stored _ | PendingStored _ -> true

let slotMode = function
  | None -> Concrete
  | Some (PendingSlot ()) -> Pending
  | Some (NestedStorage storage) -> Stored storage
  | Some (PendingStorage ((), storage)) -> PendingStored storage

let modeSlot = function
  | Concrete -> None
  | Pending -> Some (PendingSlot ())
  | Stored storage -> Some (NestedStorage storage)
  | PendingStored storage -> Some (PendingStorage ((), storage))

let compatibleKind left right =
  match (left, right) with
  | TupleStorage, TupleStorage -> true
  | ListStorage left, ListStorage right -> left = right
  | DictStorage (leftKey, leftValue), DictStorage (rightKey, rightValue) ->
      leftKey = rightKey && leftValue = rightValue
  | SumStorage left, SumStorage right -> left = right
  | RecordStorage left, RecordStorage right ->
      left.ANF.valueType = right.ANF.valueType
      && left.ANF.fields = right.ANF.fields
  | _ -> false

let rec equal left right =
  match (left, right) with
  | Concrete, Concrete | Pending, Pending -> true
  | Stored left, Stored right | PendingStored left, PendingStored right ->
      left.kind = right.kind && List.equal equalSlot left.fields right.fields
  | _ -> false

and equalSlot left right = equal (slotMode left) (slotMode right)

let rec merge left right =
  match (left, right) with
  | Concrete, value | value, Concrete -> Ok value
  | Pending, Pending -> Ok Pending
  | Stored left, Stored right ->
      if not (compatibleKind left.kind right.kind) then
        Crash.crash "Async transport merged distinct source storage layouts";
      let* fields =
        mergeList
          (List.map slotMode left.fields)
          (List.map slotMode right.fields)
      in
      Ok (Stored { left with fields = List.map modeSlot fields })
  | Pending, Stored storage
  | Stored storage, Pending
  | Pending, PendingStored storage
  | PendingStored storage, Pending ->
      Ok (PendingStored storage)
  | PendingStored left, Stored right
  | Stored left, PendingStored right
  | PendingStored left, PendingStored right -> (
      let* value = merge (Stored left) (Stored right) in
      match value with
      | Stored storage -> Ok (PendingStored storage)
      | _ -> Crash.crash "Async stored interface merge lost its field layout")

and mergeList left right =
  match (left, right) with
  | [], [] -> Ok []
  | left :: lefts, right :: rights ->
      let* value = merge left right in
      let* values = mergeList lefts rights in
      Ok (value :: values)
  | _ -> Crash.crash "Async transport merged inconsistent interface arities"

let rec mapStorage transform storage =
  let fields =
    List.map
      (Option.map (function
        | PendingSlot value -> PendingSlot (transform value)
        | NestedStorage storage -> NestedStorage (mapStorage transform storage)
        | PendingStorage (value, storage) ->
            PendingStorage (transform value, mapStorage transform storage)))
      storage.fields
  in
  { storage with fields }

let sourceFields storage source =
  match (storage.kind, source) with
  | TupleStorage, AST.TTuple types -> types
  | ListStorage _, AST.TList element -> [ element ]
  | DictStorage (expectedKey, expectedValue), AST.TDict (key, value)
    when expectedKey = key && expectedValue = value ->
      [ value ]
  | RecordStorage descriptor, AST.TRecord _ ->
      List.map snd descriptor.ANF.fields
  | SumStorage layout, AST.TSum (name, args)
    when name = layout.AsyncSumStorage.source.ANF.sumName
         && args = layout.AsyncSumStorage.source.ANF.sumArgs ->
      List.map snd layout.AsyncSumStorage.payloads
  | _ -> Crash.crash "Async transport source type does not match its storage"

let carrier source = AST.TSum ("Darklang.Stdlib.__Async.__Value", [ source ])

let rec storageType storage source =
      let types = sourceFields storage source in
      if List.length types <> List.length storage.fields then
        Crash.crash "Async transport storage has an inconsistent field count";
      let types =
        List.map2 (fun typ slot -> slotType slot typ) types storage.fields
      in
      match (storage.kind, types) with
      | SumStorage layout, types ->
          let sum, _ = AsyncSumStorage.physical layout types in
          AST.TSum (sum.ANF.sumName, sum.ANF.sumArgs)
      | ListStorage _, [ element ] -> AST.TList element
      | ListStorage _, _ ->
          Crash.crash "Async list storage has an inconsistent element layout"
      | DictStorage (key, _), [ value ] -> AST.TDict (key, value)
      | DictStorage _, _ ->
          Crash.crash
            "Async dictionary storage has an inconsistent value layout"
      | _ -> AST.TTuple types

and slotType slot typ =
  match slot with
  | None -> typ
  | Some (PendingSlot _) -> carrier typ
  | Some (NestedStorage nested) -> storageType nested typ
  | Some (PendingStorage (_, nested)) -> carrier (storageType nested typ)

let physicalType mode source =
  match mode with
  | Concrete -> source
  | Pending -> carrier source
  | Stored storage -> storageType storage source
  | PendingStored storage -> carrier (storageType storage source)

let sumDescriptor storage =
  match storage.kind with
  | SumStorage layout -> (
      let source =
        AST.TSum
          (layout.AsyncSumStorage.source.ANF.sumName,
           layout.AsyncSumStorage.source.ANF.sumArgs)
      in
      match storageType storage source with
      | AST.TSum (sumName, sumArgs) -> { ANF.sumName; sumArgs }
      | _ -> Crash.crash "Async physical sum descriptor lost its nominal type")
  | _ -> Crash.crash "Async physical sum descriptor has a non-variant layout"

let collectSumShapes ~into storage =
  let rec collect ancestors into storage =
    let* ancestors =
      match storage.kind with
      | SumStorage _ | RecordStorage _ ->
          if List.exists (compatibleKind storage.kind) ancestors then
            Error
              "Async transport across a recursive aggregate layout is not \
               implemented"
          else Ok (storage.kind :: ancestors)
      | TupleStorage | ListStorage _ | DictStorage _ -> Ok ancestors
    in
    let* into =
      match storage.kind with
      | SumStorage layout ->
          let types = List.map snd layout.AsyncSumStorage.payloads in
          if List.length types <> List.length storage.fields then
            Crash.crash "Async sum storage has an inconsistent payload count";
          let types =
            List.map2
              (fun typ slot -> slotType slot typ)
              types storage.fields
          in
          let sum, shape = AsyncSumStorage.physical layout types in
          AsyncSumStorage.includePhysicalShapes ~into
            (StringOrder.Map.singleton sum.ANF.sumName shape)
      | TupleStorage | RecordStorage _ | ListStorage _ | DictStorage _ ->
          Ok into
    in
    List.fold_left
      (fun result slot ->
        let* into = result in
        match slot with
        | Some (NestedStorage nested) | Some (PendingStorage (_, nested)) ->
            collect ancestors into nested
        | None | Some (PendingSlot _) -> Ok into)
      (Ok into) storage.fields
  in
  collect [] into storage

let rec instantiateStorage transform storage source =
  let types = sourceFields storage source in
  let rec fields slots types =
    match (slots, types) with
    | [], [] -> Ok []
    | slot :: slots, typ :: types ->
        let* value =
          match slot with
          | None -> Ok None
          | Some (PendingSlot ()) ->
              Result.map (fun value -> Some (PendingSlot value)) (transform typ)
          | Some (PendingStorage ((), storage)) ->
              let* value = transform (physicalType (Stored storage) typ) in
              let* storage = instantiateStorage transform storage typ in
              Ok (Some (PendingStorage (value, storage)))
          | Some (NestedStorage storage) ->
              Result.map
                (fun value -> Some (NestedStorage value))
                (instantiateStorage transform storage typ)
        in
        let* values = fields slots types in
        Ok (value :: values)
    | _ -> Crash.crash "Async transport storage has an inconsistent field count"
  in
  let* fields = fields storage.fields types in
  Ok { storage with fields }
