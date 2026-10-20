(* async_storage_graph_regression.ml - Validate recursive capability joins and independent field modes. *)
open Dark_compiler
module G = AsyncStorageGraph

let require condition message = if not condition then failwith message
let close = G.close ~recursive:(fun kind -> kind <> "Tuple") ~compatible:String.equal
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

let () =
  let leaf = G.assemble "Tree<Int64>" [Some (), None; None, None] in
  let branch = G.assemble "Tree<Int64>" [None, None; None, Some leaf] in
  let tree = close branch in
  require (pending tree 0) "Closing recursion lost a pending leaf";
  require (same tree (child tree 1)) "Recursive child did not inherit the interface";
  require (same tree (close tree)) "Closing an interface is not idempotent";
  require (same tree (G.map Fun.id tree)) "Mapping expanded or changed a cycle";
  require (same tree (close (join leaf tree))) "Joining a recursive interface lost information";
  require (same (join leaf tree) (join tree leaf)) "Recursive joins are not commutative";
  let ready = G.assemble "Tree<Int64>" [None, None; None, None] in
  let siblings = close (G.assemble "Tuple" [None, Some leaf; None, Some ready]) in
  require (pending (child siblings 0) 0) "First independent sibling lost its pending field";
  require (not (pending (child siblings 1) 0)) "Unrelated siblings were widened together";
  let inner = G.assemble "Box<Int64>" [Some (), None] in
  let outer = close (G.assemble "Box<Box<Int64>>" [None, Some inner]) in
  require (not (pending outer 0)) "Distinct generic instantiations were folded together";
  require (pending (child outer 0) 0) "Finite generic nesting lost its leaf";
  let aLeaf = G.assemble "A" [Some (), None; None, None] in
  let b = G.assemble "B" [None, Some aLeaf] in
  let a = close (G.assemble "A" [None, None; None, Some b]) in
  require (pending a 0) "Mutual recursion lost a pending leaf";
  require (same a (child (child a 1) 0)) "Mutual recursion did not close";
  require (same a (close (join a aLeaf))) "Mutual recursive joins changed the interface";
  print_endline "Recursive storage joins preserve pending leaves, sibling independence and generic instantiations"
