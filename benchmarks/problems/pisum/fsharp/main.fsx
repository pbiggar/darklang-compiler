// main.fsx - Parameterized F# benchmark implementation.
let argument index = int fsi.CommandLineArgs.[index + 1]
let argument64 index = int64 fsi.CommandLineArgs.[index + 1]

// Pi Summation Benchmark
// Computes partial sum of 1/k^2 series (converges to pi^2/6)

let pisum rounds n =
    let mutable s = 0.0
    for _ in 1 .. rounds do
        s <- 0.0
        for k in 1 .. n do
            s <- s + 1.0 / float (k * k)
    s

// n=10000 gives reasonable runtime
let result = pisum (argument 0) (argument 1)
// Output as integer (multiply by large factor for precision)
printfn "%d" (int64 (result * 1000000000000.0))
