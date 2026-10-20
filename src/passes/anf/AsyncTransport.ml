(* AsyncTransport.ml - Merge finite storage interfaces without demanding their payloads. *)
[@@@warning "-4"]
module G = AsyncStorageGraph

type storageKind = AsyncStorageKind.t =
  | TupleStorage
  | RecordStorage of ANF.recordDescriptor
  | ListStorage of AST.semanticType
  | DictStorage of AST.semanticType * AST.semanticType
  | SumStorage of AsyncSumStorage.layout

type 'a storage = (storageKind, 'a) G.t
type 'a storedSlot =
  | PendingSlot of 'a
  | NestedStorage of 'a storage
  | PendingStorage of 'a * 'a storage

let storage kind fields =
  G.assemble kind
    (List.map
       (function
         | None -> None, None
         | Some (PendingSlot value) -> Some value, None
         | Some (NestedStorage child) -> None, Some child
         | Some (PendingStorage (value, child)) -> Some value, Some child)
       fields)

let kind storage = (G.node storage).G.kind
let slot storage slot =
  match slot.G.pending, slot.G.nested with
  | None, None -> None
  | Some value, None -> Some (PendingSlot value)
  | None, Some child -> Some (NestedStorage (G.at storage child))
  | Some value, Some child -> Some (PendingStorage (value, G.at storage child))

let fields storage = List.map (slot storage) (G.node storage).G.slots
let field storage index =
  let rec find offset = function
    | [] -> Crash.crash "Async storage projection is outside its layout"
    | value :: rest -> if offset = index then slot storage value else find (offset + 1) rest
  in
  find 0 (G.node storage).G.slots

type mode = Concrete | Pending | Stored of unit storage | PendingStored of unit storage
let ( let* ) = Result.bind
let changed = function Concrete -> false | Pending | Stored _ | PendingStored _ -> true
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

let equal left right =
  left == right ||
  match left, right with
  | Concrete, Concrete | Pending, Pending -> true
  | Stored left, Stored right | PendingStored left, PendingStored right ->
      G.equal ~kind:( = ) left right
  | _ -> false

let merge left right =
  if equal left right then Ok left else
  match left, right with
  | Concrete, value | value, Concrete -> Ok value
  | Pending, Pending -> Ok Pending
  | Stored left, Stored right ->
      Ok (Stored (G.join ~compatible:AsyncStorageKind.compatible left right))
  | Pending, Stored storage | Stored storage, Pending
  | Pending, PendingStored storage | PendingStored storage, Pending ->
      Ok (PendingStored storage)
  | PendingStored left, Stored right | Stored left, PendingStored right
  | PendingStored left, PendingStored right ->
      Ok (PendingStored (G.join ~compatible:AsyncStorageKind.compatible left right))

let rec mergeList left right =
  match left, right with
  | [], [] -> Ok []
  | left :: lefts, right :: rights ->
      let* value = merge left right in
      let* values = mergeList lefts rights in
      Ok (value :: values)
  | _ -> Crash.crash "Async transport merged inconsistent interface arities"

let mapStorage = G.map
let carrier source = AST.TSum ("Darklang.Stdlib.__Async.__Value", [source])

let storageType storage source =
  match AsyncGraphTypes.resolve storage source with
  | Ok (typ, _) -> typ
  | Error message -> Crash.crash message

let slotType slot typ =
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

let recordDescriptor storage (descriptor : ANF.recordDescriptor) =
  let slots = fields storage in
  if List.length descriptor.ANF.fields <> List.length slots then
    Crash.crash "Async typed storage has an inconsistent layout";
  let fields =
    List.map2 (fun (name, typ) slot -> name, slotType slot typ) descriptor.ANF.fields slots
  in
  { descriptor with ANF.fields; valueType = AST.TTuple (List.map snd fields) }

let sumDescriptor storage =
  match kind storage with
  | SumStorage layout -> (
      let source = AST.TSum (layout.AsyncSumStorage.source.ANF.sumName, layout.AsyncSumStorage.source.ANF.sumArgs) in
      match storageType storage source with
      | AST.TSum (sumName, sumArgs) -> { ANF.sumName; sumArgs }
      | _ -> Crash.crash "Async physical sum descriptor lost its nominal type")
  | _ -> Crash.crash "Async physical sum descriptor has a non-variant layout"

let collectSumShapes ~into storage =
  let rec collect ancestors into storage =
    let* ancestors =
      match kind storage with
      | SumStorage _ | RecordStorage _ ->
          if List.exists (AsyncStorageKind.compatible (kind storage)) ancestors then
            Error "Async transport across a recursive aggregate layout is not implemented"
          else Ok (kind storage :: ancestors)
      | TupleStorage | ListStorage _ | DictStorage _ -> Ok ancestors
    in
    let* into =
      match kind storage with
      | SumStorage layout ->
          let source = AST.TSum (layout.AsyncSumStorage.source.ANF.sumName, layout.AsyncSumStorage.source.ANF.sumArgs) in
          let* _, shapes = AsyncGraphTypes.resolve storage source in
          AsyncSumStorage.includePhysicalShapes ~into shapes
      | _ -> Ok into
    in
    List.fold_left
      (fun result slot ->
        let* into = result in
        match slot with
        | Some (NestedStorage nested) | Some (PendingStorage (_, nested)) -> collect ancestors into nested
        | None | Some (PendingSlot _) -> Ok into)
      (Ok into) (fields storage)
  in
  collect [] into storage

module Typed = Map.Make (struct
  type t = int * AST.semanticType
  let compare = Stdlib.compare
end)

let instantiateStorage transform storage source =
  let rec visit memo output next ((id, source) as key) =
    match Typed.find_opt key memo with
    | Some copied -> Ok (copied, memo, output, next)
    | None ->
        let copied = next in
        let memo = Typed.add key copied memo in
        let original = G.node (G.at storage id) in
        let types = AsyncStorageKind.sourceFields original.G.kind source in
        if List.length types <> List.length original.G.slots then
          Crash.crash "Async transport storage has an inconsistent field count";
        let* slots, memo, output, next =
          List.fold_left2
            (fun result source slot ->
              let* slots, memo, output, next = result in
              let* pending =
                match slot.G.pending with
                | None -> Ok None
                | Some () ->
                    let payload =
                      match slot.G.nested with None -> source
                      | Some child -> storageType (G.at storage child) source
                    in
                    Result.map Option.some (transform payload)
              in
              let* nested, memo, output, next =
                match slot.G.nested with
                | None -> Ok (None, memo, output, next)
                | Some child ->
                    let* child, memo, output, next = visit memo output next (child, source) in
                    Ok (Some child, memo, output, next)
              in
              Ok ({ G.pending; nested } :: slots, memo, output, next))
            (Ok ([], memo, output, next + 1)) types original.G.slots
        in
        let output = G.Nodes.add copied { G.kind = original.G.kind; slots = List.rev slots } output in
        Ok (copied, memo, output, next)
  in
  let* root, _, nodes, _ = visit Typed.empty G.Nodes.empty 0 (G.root storage, source) in
  Ok (G.make ~root nodes)
