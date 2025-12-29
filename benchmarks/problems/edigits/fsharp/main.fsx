// Edigits Benchmark - Computing digits of e
// Uses series expansion: e = sum(1/n!) for n=0 to infinity
// Computes checksum of first N digits

let computeEDigits numDigits =
    // We need extra precision for computation
    let precision = numDigits + 10
    let baseVal = 10

    // Work with scaled integers
    let eScaled = Array.zeroCreate (precision + 1)
    eScaled.[0] <- 1 // represents 1.0

    // Add 1/n! for n = 1, 2, 3, ...
    let term = Array.zeroCreate (precision + 1)
    term.[0] <- 1 // start with 1

    for n in 1 .. 50 do
        // term = term / n
        let mutable carry = 0
        for i in 0 .. precision do
            let current = carry * baseVal + term.[i]
            term.[i] <- current / n
            carry <- current % n

        // eScaled += term
        let mutable carry2 = 0
        for i in precision .. -1 .. 0 do
            let sum = eScaled.[i] + term.[i] + carry2
            eScaled.[i] <- sum % baseVal
            carry2 <- sum / baseVal

    // Extract digits
    Array.sub eScaled 0 numDigits

// Compute first 1000 digits of e multiple times
let mutable checksum = 0L
for _ in 1 .. 10 do
    let digits = computeEDigits 1000
    checksum <- 0L
    for i in 0 .. digits.Length - 1 do
        checksum <- (checksum + int64 digits.[i] * int64 (i + 1)) % 1000000007L

printfn "%d" checksum
