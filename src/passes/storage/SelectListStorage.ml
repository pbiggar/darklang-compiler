(* SelectListStorage.ml - Choose fixed-block or mapped storage for closed list regions. *)
[@@@warning "-4"]
module H = HIR
module L = ListRegion
(*
   Small fixed blocks use the recycling heap; larger arrays own a mapping.
*)
let selectStorage (L.FunctionalRegion block as region) =
 let rec select layouts (L.FunctionalBlock block) = List.fold_left (fun layouts operation -> match operation with
 | H.Leaf (L.Construct (value, L.Literal elements)) -> let length = List.length elements in H.ValueMap.add value.H.id (if length <= L.recycledCapacityLimit then L.RecycledArray length else L.MappedArray length) layouts
 | H.Leaf (L.Construct (value, L.Repeat _)) -> H.ValueMap.add value.H.id (L.RuntimeArray value.H.id) layouts
 | H.Leaf (L.Transform (value, input, _)) -> H.ValueMap.add value.H.id (L.lookup "layout" input.H.id layouts) layouts
 | H.Branch (_, _, yes, no) -> let layouts = select layouts yes in select layouts no
 | H.Leaf (L.Fold _) | H.ScalarBinding _ | H.Call _ -> layouts) layouts block.H.operations in
 L.StorageRegion (region, select H.ValueMap.empty block)
