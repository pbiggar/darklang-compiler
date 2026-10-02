(* ResultList.ml - Preserve traversal order and stop at the first error. *)
let mapResults f items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | item :: rest ->
        match f item with
        | Error error -> Error error
        | Ok result -> loop (result :: acc) rest
  in
  loop [] items

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

let sequenceResults items = mapResults Fun.id items
let traverse = mapResults
let sequenceOption = function
  | None -> Ok None
  | Some (Ok value) -> Ok (Some value)
  | Some (Error error) -> Error error
