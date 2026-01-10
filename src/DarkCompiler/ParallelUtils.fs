// ParallelUtils.fs - Parallel helpers for deterministic list/result transforms
//
// Provides order-preserving parallel mapping helpers for compiler passes.

module ParallelUtils

/// Map over a list in parallel while preserving order
let mapListParallel (items: 'a list) (f: 'a -> 'b) : 'b list =
    items
    |> List.toArray
    |> Array.Parallel.map f
    |> Array.toList

/// Map over a list in parallel, preserving order and returning first error
let mapResultsParallel (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
    let results = mapListParallel items f
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | result :: rest ->
            match result with
            | Ok value -> loop (value :: acc) rest
            | Error err -> Error err
    loop [] results
