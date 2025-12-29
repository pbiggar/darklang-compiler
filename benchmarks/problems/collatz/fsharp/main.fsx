// Collatz Benchmark
// Counts steps in Collatz sequences for numbers 1 to n

let rec collatzSteps n steps =
    if n = 1L then steps
    elif n % 2L = 0L then collatzSteps (n / 2L) (steps + 1)
    else collatzSteps (3L * n + 1L) (steps + 1)

let rec sumCollatzRange i limit total =
    if i > limit then total
    else sumCollatzRange (i + 1L) limit (total + collatzSteps i 0)

// Sum steps for numbers 1 to 100000
printfn "%d" (sumCollatzRange 1L 100000L 0)
