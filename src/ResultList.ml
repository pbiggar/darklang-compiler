(*
   ResultList.ml - Sequential helpers for list/result transforms
   Provides order-preserving sequential mapping helpers for compiler passes.
*)
(* ResultList.ml - Preserve traversal order and stop at the first error. *)
(*
   Map over a list sequentially, returning first error
*)
let mapResults f items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | item :: rest ->
        match f item with
        | Error error -> Error error
        | Ok result -> loop (result :: acc) rest
  in
  loop [] items

(*
   Map over a list sequentially and concatenate each successful result list.
*)
let collectResults f items =
  let rec prependReversed source target =
    match source with
    | [] -> target
    | head :: tail -> prependReversed tail (head :: target)
  in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | item :: rest ->
        match f item with
        | Error error -> Error error
        | Ok results -> loop (prependReversed results acc) rest
  in
  loop [] items

(*
   Sequence a list of results, returning the first error
*)
let sequenceResults items = mapResults Fun.id items
(*
   Conventional name for result-returning list mapping.
*)
let traverse = mapResults
(*
   Turn an optional result into a result containing an optional value.
*)
let sequenceOption = function
  | None -> Ok None
  | Some (Ok value) -> Ok (Some value)
  | Some (Error error) -> Error error
