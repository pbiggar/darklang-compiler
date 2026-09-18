// SelectListStorage.fs - Choose fixed-block or mapped storage for closed list regions.

module SelectListStorage

open HIR

open ListRegion

/// Small fixed blocks use the recycling heap; larger arrays own a mapping.
let selectStorage (FunctionalRegion block as region) : StorageRegion =
    let rec select layouts (FunctionalBlock block) =
        block.Operations
        |> List.fold (fun layouts operation ->
            match operation with
            | Leaf (Construct (id, Literal elements)) ->
                let length = List.length elements
                Map.add id.Id (if length <= recycledCapacityLimit then RecycledArray length else MappedArray length) layouts
            | Leaf (Construct (id, Repeat _)) -> Map.add id.Id (RuntimeArray id.Id) layouts
            | Leaf (Transform (id, input, _)) -> Map.add id.Id (lookup "layout" input.Id layouts) layouts
            | Branch (_, _, yes, no) -> select (select layouts yes) no
            | Leaf (Fold _) | ScalarBinding _ | Call _ -> layouts) layouts
    StorageRegion (region, select Map.empty block)
