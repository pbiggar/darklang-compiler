(* async_storage_graph_regression.ml - Validate recursive capability joins and independent field modes. *)
[@@@warning "-4"]

open Dark_compiler
module G = AsyncStorageGraph
module K = AsyncStorageKind

let require condition message = if not condition then failwith message

let close =
  G.close ~recursive:(fun kind -> kind <> "Tuple") ~compatible:String.equal

let same = G.equal ~kind:String.equal
let join = G.join ~compatible:String.equal

let field graph index =
  let rec find offset = function
    | [] -> failwith "Missing graph regression field"
    | slot :: rest -> if offset = index then slot else find (offset + 1) rest
  in
  find 0 (G.node graph).G.slots

let child graph index =
  match (field graph index).G.nested with
  | Some id -> G.at graph id
  | None -> failwith "Missing graph regression child"

let pending graph index = Option.is_some (field graph index).G.pending

let physical graph source =
  match AsyncGraphTypes.resolve graph source with
  | Ok resolved -> resolved
  | Error message -> failwith message

let checkPhysicalCycles () =
  let source = AST.TSum ("__dark_async_cycle_ref_0", []) in
  let layout =
    {
      AsyncSumStorage.source =
        { ANF.sumName = "__dark_async_cycle_ref_0"; sumArgs = [] };
      payloads = [ (0, AST.TInt64); (1, source); (2, source) ];
      emptyTags = [];
      unaryTags = [ 0; 1; 2 ];
    }
  in
  let kind = K.SumStorage layout in
  let node second =
    {
      G.kind;
      slots =
        [
          { G.pending = Some (); nested = None };
          { G.pending = None; nested = Some 0 };
          { G.pending = None; nested = second };
        ];
    }
  in
  let cycle = G.make ~root:0 (G.Nodes.singleton 0 (node None)) in
  let typ, shapes = physical cycle source in
  let _, widerShapes =
    physical (G.make ~root:0 (G.Nodes.singleton 0 (node (Some 0)))) source
  in
  let wider, _ =
    physical (G.make ~root:0 (G.Nodes.singleton 0 (node (Some 0)))) source
  in
  require (typ <> wider)
    "Native recursive payloads alias symbolic back references";
  require
    (not (StringOrder.Map.equal ( = ) shapes widerShapes))
    "Different cyclic interfaces share ownership metadata";
  let name =
    match typ with
    | AST.TSum (name, []) -> name
    | _ -> failwith "Missing cyclic sum type"
  in
  let shape =
    match StringOrder.Map.find_opt name shapes with
    | Some shape -> shape
    | None -> failwith "Missing cyclic ownership metadata"
  in
  require
    (List.assoc_opt 1 shape.MemoryModel.payloads = Some (Some typ))
    "Recursive ownership payload does not refer to its physical sum";
  require
    (List.assoc_opt 2 shape.MemoryModel.payloads = Some (Some source))
    "Native recursive payload was incorrectly widened";
  let parent = G.assemble K.TupleStorage [ (None, Some cycle); (None, None) ] in
  let viewed = child parent 0 |> G.map Fun.id in
  require
    (fst (physical viewed source) = typ)
    "Physical cycle identity depends on parent context or node numbering";
  require
    (fst (physical parent (AST.TTuple [ source; AST.TInt64 ]))
    = AST.TTuple [ typ; AST.TInt64 ])
    "Cyclic payload changed its enclosing tuple layout"

let () =
  let leaf = G.assemble "Tree<Int64>" [ (Some (), None); (None, None) ] in
  let branch = G.assemble "Tree<Int64>" [ (None, None); (None, Some leaf) ] in
  let tree = close branch in
  require (pending tree 0) "Closing recursion lost a pending leaf";
  require
    (same tree (child tree 1))
    "Recursive child did not inherit the interface";
  require (same tree (close tree)) "Closing an interface is not idempotent";
  require (same tree (G.map Fun.id tree)) "Mapping expanded or changed a cycle";
  require
    (same tree (close (join leaf tree)))
    "Joining a recursive interface lost information";
  require
    (same (join leaf tree) (join tree leaf))
    "Recursive joins are not commutative";
  let ready = G.assemble "Tree<Int64>" [ (None, None); (None, None) ] in
  let siblings =
    close (G.assemble "Tuple" [ (None, Some leaf); (None, Some ready) ])
  in
  require
    (pending (child siblings 0) 0)
    "First independent sibling lost its pending field";
  require
    (not (pending (child siblings 1) 0))
    "Unrelated siblings were widened together";
  let inner = G.assemble "Box<Int64>" [ (Some (), None) ] in
  let outer = close (G.assemble "Box<Box<Int64>>" [ (None, Some inner) ]) in
  require
    (not (pending outer 0))
    "Distinct generic instantiations were folded together";
  require (pending (child outer 0) 0) "Finite generic nesting lost its leaf";
  let aLeaf = G.assemble "A" [ (Some (), None); (None, None) ] in
  let b = G.assemble "B" [ (None, Some aLeaf) ] in
  let a = close (G.assemble "A" [ (None, None); (None, Some b) ]) in
  require (pending a 0) "Mutual recursion lost a pending leaf";
  require (same a (child (child a 1) 0)) "Mutual recursion did not close";
  require
    (same a (close (join a aLeaf)))
    "Mutual recursive joins changed the interface";
  checkPhysicalCycles ();
  print_endline
    "Recursive storage joins preserve pending leaves, sibling independence and \
     generic instantiations"
