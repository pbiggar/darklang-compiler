// Quicksort Benchmark
// Sorts a list and returns a checksum

let rec quicksort arr =
    match arr with
    | [] -> []
    | _ when List.length arr <= 1 -> arr
    | _ ->
        let pivot = List.item (List.length arr / 2) arr
        let left = List.filter (fun x -> x < pivot) arr
        let middle = List.filter (fun x -> x = pivot) arr
        let right = List.filter (fun x -> x > pivot) arr
        quicksort left @ middle @ quicksort right

let generateList n seed =
    let mutable x = seed
    [ for _ in 1 .. n do
        x <- (x * 1103515245UL + 12345UL) % (1UL <<< 31)
        yield int64 (x % 10000UL) ]

let checksum arr =
    arr
    |> List.mapi (fun i x -> (x * int64 (i + 1)) % 1000000007L)
    |> List.fold (fun acc x -> (acc + x) % 1000000007L) 0L

// Sort 100 elements (reduced to match Dark heap constraints)
let arr = generateList 100 42UL
let sortedArr = quicksort arr
printfn "%d" (checksum sortedArr)
