// main.fsx - Parameterized F# benchmark implementation.
let argument index = int fsi.CommandLineArgs.[index + 1]
let argument64 index = int64 fsi.CommandLineArgs.[index + 1]

let rec ackermann m n =
    if m = 0 then n + 1
    elif n = 0 then ackermann (m - 1) 1
    else ackermann (m - 1) (ackermann m (n - 1))

// A(3, 12) = 32765
printfn "%d" (ackermann (argument 0) (argument 1))
