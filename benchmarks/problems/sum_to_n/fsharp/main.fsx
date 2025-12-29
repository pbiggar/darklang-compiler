let rec sumTo n acc =
    if n <= 0 then acc
    else sumTo (n - 1) (acc + n)

let rec repeat n acc =
    if n <= 0 then acc
    else repeat (n - 1) (sumTo 10000 0)

printfn "%d" (repeat 100 0)
