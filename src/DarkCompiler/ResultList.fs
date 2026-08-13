// ResultList.fs - Sequential helpers for list/result transforms
//
// Provides order-preserving sequential mapping helpers for compiler passes.

module ResultList

/// Map over a list sequentially, returning first error
let mapResults (f: 'a -> Result<'b, 'error>) (items: 'a list) : Result<'b list, 'error> =
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            match f item with
            | Error err -> Error err
            | Ok result -> loop (result :: acc) rest
    loop [] items

/// Map over a list sequentially and concatenate each successful result list.
let collectResults (f: 'a -> Result<'b list, 'error>) (items: 'a list) : Result<'b list, 'error> =
    let rec prependReversed source target =
        match source with
        | [] -> target
        | head :: tail -> prependReversed tail (head :: target)

    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            match f item with
            | Error err -> Error err
            | Ok results -> loop (prependReversed results acc) rest
    loop [] items

/// Sequence a list of results, returning the first error
let sequenceResults (items: Result<'a, 'error> list) : Result<'a list, 'error> =
    mapResults id items

/// Conventional name for result-returning list mapping.
let traverse (f: 'a -> Result<'b, 'error>) (items: 'a list) : Result<'b list, 'error> =
    mapResults f items

/// Turn an optional result into a result containing an optional value.
let sequenceOption (item: Result<'a, 'error> option) : Result<'a option, 'error> =
    match item with
    | None -> Ok None
    | Some (Ok value) -> Ok (Some value)
    | Some (Error error) -> Error error
