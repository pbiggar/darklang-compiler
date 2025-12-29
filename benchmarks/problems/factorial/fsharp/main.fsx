let rec factorial n : int64 =
    if n <= 1L then 1L
    else n * factorial (n - 1L)

let rec repeat n acc =
    if n <= 0 then acc
    else repeat (n - 1) (factorial 20L)

printfn "%d" (repeat 10000 0L)
