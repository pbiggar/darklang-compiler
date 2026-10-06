(*
   Prelude.fs - Type alias required by the copied interpreter syntax modules.
*)
(* Prelude.ml - Preserve shared frontend definitions. *)
type 'a neList = 'a ParserDependencies.neList
module Map = struct
  let values groups =
    (* Group binding names in the native OCaml string order. *)
    let sorted = List.stable_sort (fun (left, _) (right, _) -> StringOrder.compare left right) groups in
    let rec collect = function
      | [] -> []
      | (key, _) :: ((nextKey, _) :: _ as rest) when key = nextKey -> collect rest
      | (_, value) :: rest -> value :: collect rest
    in collect sorted
end
