(* AsyncTransport.ml - Merge field interfaces without demanding their payloads. *)
[@@@warning "-4"]

type storageKind = TupleStorage | RecordStorage of ANF.recordDescriptor

type 'a storedSlot = PendingSlot of 'a | NestedStorage of 'a storage
and 'a storage = { kind : storageKind; fields : 'a storedSlot option list }

type mode = Concrete | Pending | Stored of unit storage

let ( let* ) = Result.bind
let changed = function Concrete -> false | Pending | Stored _ -> true

let slotMode = function
  | None -> Concrete
  | Some (PendingSlot ()) -> Pending
  | Some (NestedStorage storage) -> Stored storage

let modeSlot = function
  | Concrete -> None
  | Pending -> Some (PendingSlot ())
  | Stored storage -> Some (NestedStorage storage)

let compatibleKind left right =
  match (left, right) with
  | TupleStorage, TupleStorage -> true
  | RecordStorage left, RecordStorage right ->
      left.ANF.valueType = right.ANF.valueType
      && left.ANF.fields = right.ANF.fields
  | _ -> false

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
  | Pending, Stored _ | Stored _, Pending ->
      Error
        "Async transport combining a pending aggregate with independently \
         pending fields is not implemented"

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
        | NestedStorage storage -> NestedStorage (mapStorage transform storage)))
      storage.fields
  in
  { storage with fields }

let sourceFields storage source =
  match (storage.kind, source) with
  | TupleStorage, AST.TTuple types -> types
  | RecordStorage descriptor, AST.TRecord _ ->
      List.map snd descriptor.ANF.fields
  | _ -> Crash.crash "Async transport source type does not match its storage"

let rec physicalType mode source =
  match mode with
  | Concrete -> source
  | Pending -> AST.TSum ("Darklang.Stdlib.__Async.__Value", [ source ])
  | Stored storage ->
      let types = sourceFields storage source in
      if List.length types <> List.length storage.fields then
        Crash.crash "Async transport storage has an inconsistent field count";
      AST.TTuple
        (List.map2
           (fun typ slot -> physicalType (slotMode slot) typ)
           types storage.fields)

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
