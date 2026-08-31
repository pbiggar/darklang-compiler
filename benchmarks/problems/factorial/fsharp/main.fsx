// main.fsx - Parameterized F# benchmark implementation.
let argument index = int fsi.CommandLineArgs.[index + 1]
let argument64 index = int64 fsi.CommandLineArgs.[index + 1]

let rec factorial n : int64 =
    if n <= 1L then 1L
    else n * factorial (n - 1L)

let rec repeat n acc =
    if n <= 0 then acc
    else repeat (n - 1) (factorial (argument64 1))

printfn "%d" (repeat (argument 0) 0L)
