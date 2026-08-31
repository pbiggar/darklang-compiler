// main.fsx - Parameterized F# benchmark implementation.
let argument index = int fsi.CommandLineArgs.[index + 1]
let argument64 index = int64 fsi.CommandLineArgs.[index + 1]

let rec fib n =
    if n <= 1 then n
    else fib (n - 1) + fib (n - 2)

printfn "%d" (fib (argument 0))
