// Tak (Takeuchi) Benchmark
// Tests recursion and function call overhead

let rec tak x y z =
    if x <= y then z
    else tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)

// Repeat multiple times for meaningful measurement
let rec repeat n acc =
    if n <= 0 then acc
    else repeat (n - 1) (tak 24 16 8)

printfn "%d" (repeat 10 0)
