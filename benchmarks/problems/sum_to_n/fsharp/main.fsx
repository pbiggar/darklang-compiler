// main.fsx - Parameterized F# benchmark implementation.
let argument index = int fsi.CommandLineArgs.[index + 1]
let argument64 index = int64 fsi.CommandLineArgs.[index + 1]

let rec sumTo n acc =
    if n <= 0 then acc
    else sumTo (n - 1) (acc + n)

let rec repeat n sumInput acc =
    if n <= 0 then acc
    else repeat (n - 1) sumInput (sumTo sumInput 0)

printfn "%d" (repeat (argument 0) (argument 1) 0)
