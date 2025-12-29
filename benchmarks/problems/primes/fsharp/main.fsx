// Prime Counting Benchmark
// Counts primes up to N using trial division

let rec isqrt n guess =
    if guess * guess > n then guess - 1
    elif (guess + 1) * (guess + 1) > n then guess
    else isqrt n (guess + 1)

let rec isDivisible n d limit =
    if d > limit then false
    elif n % d = 0 then true
    else isDivisible n (d + 1) limit

let isPrime n =
    if n < 2 then false
    elif n = 2 then true
    elif n % 2 = 0 then false
    else
        let limit = isqrt n 1
        not (isDivisible n 3 limit)

let rec countPrimes n count =
    if n <= 1 then count
    elif isPrime n then countPrimes (n - 1) (count + 1)
    else countPrimes (n - 1) count

// Count primes up to 10000
// Expected: 1229 primes
printfn "%d" (countPrimes 10000 0)
