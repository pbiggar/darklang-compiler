// main.fsx - Parameterized F# benchmark implementation.
let argument index = int fsi.CommandLineArgs.[index + 1]
let argument64 index = int64 fsi.CommandLineArgs.[index + 1]

// Leibniz Pi Benchmark
// Computes pi using Leibniz formula: pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...

let rec leibnizLoop i n sum sign =
    if i >= n then sum * 4.0
    else
        let term = sign / float (2 * i + 1)
        leibnizLoop (i + 1) n (sum + term) (-sign)

let leibnizPi n =
    leibnizLoop 0 n 0.0 1.0

// Use 100 million iterations for timing
// Output as integer (multiply by large factor for precision)
printfn "%d" (int64 (leibnizPi (argument 0) * 100000000.0))
