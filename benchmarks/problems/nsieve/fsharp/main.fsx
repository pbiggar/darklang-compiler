// Nsieve Benchmark - Sieve of Eratosthenes
// Counts primes up to n using sieve algorithm

let nsieve n =
    let isPrime = Array.create (n + 1) true
    let mutable count = 0

    for i in 2 .. n do
        if isPrime.[i] then
            count <- count + 1
            // Mark multiples as not prime
            let mutable j = i + i
            while j <= n do
                isPrime.[j] <- false
                j <- j + i
    count

// Run sieve multiple times for meaningful benchmark
let mutable total = 0
for _ in 1 .. 100 do
    total <- nsieve 100000
printfn "%d" total
