(* AsyncStorageGraph.ml - Keep transport cycles as references, never cyclic OCaml values. *)
module Nodes = Map.Make (Int)

type 'a slot = { pending : 'a option; nested : int option }
type ('kind, 'a) node = { kind : 'kind; slots : 'a slot list }
type ('kind, 'a) t = { root : int; nodes : ('kind, 'a) node Nodes.t }

let find nodes id =
  match Nodes.find_opt id nodes with
  | Some node -> node
  | None -> Crash.crash "Async storage graph references an absent node"

let make ~root nodes =
  ignore (find nodes root);
  Nodes.iter
    (fun _ node ->
      List.iter
        (fun slot -> Option.iter (fun id -> ignore (find nodes id)) slot.nested)
        node.slots)
    nodes;
  { root; nodes }

let root graph = graph.root
let nodes graph = graph.nodes
let node graph = find graph.nodes graph.root

let at graph root =
  ignore (find graph.nodes root);
  { graph with root }

let reachable graph =
  let rec visit seen ordered id =
    if Nodes.mem id seen then (seen, ordered)
    else
      let seen = Nodes.add id () seen in
      List.fold_left
        (fun (seen, ordered) slot ->
          match slot.nested with
          | None -> (seen, ordered)
          | Some child -> visit seen ordered child)
        (seen, id :: ordered) (find graph.nodes id).slots
  in
  let _, ordered = visit Nodes.empty [] graph.root in
  List.rev ordered

(* Reserve the destination identity before copying children. The same operation
   handles trees, shared DAGs, and back edges; unreachable parent views are not
   imported when a projected child is stored in a new aggregate. *)
let copy transform graph start =
  let rec visit mapped output next id =
    match Nodes.find_opt id mapped with
    | Some copied -> (copied, mapped, output, next)
    | None ->
        let copied = next in
        let mapped = Nodes.add id copied mapped in
        let original = find graph.nodes id in
        let slots, mapped, output, next =
          List.fold_left
            (fun (slots, mapped, output, next) slot ->
              let nested, mapped, output, next =
                match slot.nested with
                | None -> (None, mapped, output, next)
                | Some id ->
                    let id, mapped, output, next = visit mapped output next id in
                    (Some id, mapped, output, next)
              in
              ({ pending = Option.map transform slot.pending; nested } :: slots,
               mapped, output, next))
            ([], mapped, output, next + 1) original.slots
        in
        let output =
          Nodes.add copied { kind = original.kind; slots = List.rev slots } output
        in
        (copied, mapped, output, next)
  in
  let root, _, output, next = visit Nodes.empty Nodes.empty start graph.root in
  (root, output, next)

let map transform graph =
  let root, nodes, _ = copy transform graph 0 in
  make ~root nodes

let assemble kind fields =
  let slots, nodes, _, _ =
    List.fold_left
      (fun (slots, nodes, next, imported) (pending, nested) ->
        let nested, nodes, next, imported =
          match nested with
          | None -> (None, nodes, next, imported)
          | Some graph -> (
              match
                List.find_map
                  (fun (source, target) ->
                    if source.nodes == graph.nodes && source.root = graph.root
                    then Some target else None)
                  imported
              with
              | Some id -> (Some id, nodes, next, imported)
              | None ->
                  let id, copied, next = copy Fun.id graph next in
                  let nodes =
                    Nodes.union
                      (fun _ _ _ -> Crash.crash "Async graph import reused an identity")
                      nodes copied
                  in
                  (Some id, nodes, next, (graph, id) :: imported))
        in
        ({ pending; nested } :: slots, nodes, next, imported))
      ([], Nodes.empty, 1, []) fields
  in
  make ~root:0 (Nodes.add 0 { kind; slots = List.rev slots } nodes)

module Pairs = Set.Make (struct
  type t = int * int
  let compare = Stdlib.compare
end)

let equal ~kind left right =
  let rec visit seen leftId rightId =
    if Pairs.mem (leftId, rightId) seen then (true, seen)
    else
      let seen = Pairs.add (leftId, rightId) seen in
      let left = find left.nodes leftId in
      let right = find right.nodes rightId in
      if not (kind left.kind right.kind) then (false, seen)
      else fields seen left.slots right.slots
  and fields seen left right =
    match left, right with
    | [], [] -> (true, seen)
    | left :: lefts, right :: rights ->
        if Option.is_some left.pending <> Option.is_some right.pending then
          (false, seen)
        else
          let same, seen =
            match left.nested, right.nested with
            | None, None -> (true, seen)
            | Some left, Some right -> visit seen left right
            | _ -> (false, seen)
          in
          if same then fields seen lefts rights else (false, seen)
    | _ -> (false, seen)
  in
  (left.nodes == right.nodes && left.root = right.root)
  || fst (visit Pairs.empty left.root right.root)

let rec representative parents id =
  match Nodes.find_opt id parents with
  | None -> id
  | Some parent -> representative parents parent

(* Publish each union before processing child constraints. Recursive joins can
   revisit the parent; recursively merging children first would either loop or
   overwrite information discovered through a back edge. *)
let unify ~compatible nodes constraints =
  let rec slots merged constraints left right =
    match left, right with
    | [], [] -> (List.rev merged, constraints)
    | left :: lefts, right :: rights ->
        let pending =
          if Option.is_some left.pending || Option.is_some right.pending then
            Some () else None
        in
        let nested, constraints =
          match left.nested, right.nested with
          | None, value | value, None -> (value, constraints)
          | Some left, Some right -> (Some left, (left, right) :: constraints)
        in
        slots ({ pending; nested } :: merged) constraints lefts rights
    | _ -> Crash.crash "Async graph join has inconsistent field arities"
  in
  let rec solve parents nodes = function
    | [] -> (parents, nodes)
    | (left, right) :: rest ->
        let left = representative parents left in
        let right = representative parents right in
        if left = right then solve parents nodes rest
        else
          let winner, loser = if left < right then (left, right) else (right, left) in
          let left = find nodes winner in
          let right = find nodes loser in
          if not (compatible left.kind right.kind) then
            Crash.crash "Async graph join merged distinct source layouts";
          let merged, rest = slots [] rest left.slots right.slots in
          solve (Nodes.add loser winner parents)
            (Nodes.add winner { kind = left.kind; slots = merged }
               (Nodes.remove loser nodes)) rest
  in
  solve Nodes.empty nodes constraints

let resolved ~root parents nodes =
  let nodes =
    Nodes.map
      (fun node ->
        { node with slots =
            List.map
              (fun slot -> { slot with nested = Option.map (representative parents) slot.nested })
              node.slots })
      nodes
  in
  map Fun.id (make ~root:(representative parents root) nodes)

let join ~compatible left right =
  let leftRoot, leftNodes, next = copy Fun.id left 0 in
  let rightRoot, rightNodes, _ = copy Fun.id right next in
  let combined =
    Nodes.union
      (fun _ _ _ -> Crash.crash "Async graph join reused an imported identity")
      leftNodes rightNodes
  in
  let parents, nodes = unify ~compatible combined [leftRoot, rightRoot] in
  resolved ~root:leftRoot parents nodes

let close ~recursive ~compatible graph =
  let constraints graph =
    let rec visit seen ancestors constraints id =
      let node = find graph.nodes id in
      let ancestor =
        if recursive node.kind then
          List.find_opt
            (fun ancestor -> compatible node.kind (find graph.nodes ancestor).kind)
            ancestors
        else None
      in
      match ancestor with
      | Some ancestor -> (ancestor, id) :: constraints
      | None when Nodes.mem id seen -> constraints
      | None ->
          let ancestors = if recursive node.kind then id :: ancestors else ancestors in
          let seen = Nodes.add id () seen in
          List.fold_left
            (fun constraints slot ->
              match slot.nested with
              | None -> constraints
              | Some child -> visit seen ancestors constraints child)
            constraints node.slots
    in
    visit Nodes.empty [] [] graph.root
    |> List.filter (fun (left, right) -> left <> right)
  in
  let rec converge graph =
    match constraints graph with
    | [] -> graph
    | pairs ->
        let parents, nodes = unify ~compatible graph.nodes pairs in
        converge (resolved ~root:graph.root parents nodes)
  in
  converge graph
