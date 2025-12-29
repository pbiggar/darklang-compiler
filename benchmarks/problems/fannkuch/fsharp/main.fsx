// Fannkuch Benchmark
// From: Computer Language Benchmarks Game

let fannkuch n =
    let perm = Array.init n id
    let count = Array.init n id
    let mutable maxFlips = 0

    let mutable running = true
    while running do
        // Count flips for current permutation
        if perm.[0] <> 0 then
            let p = Array.copy perm
            let mutable flips = 0
            while p.[0] <> 0 do
                let k = p.[0]
                // Reverse first k+1 elements
                for i in 0 .. k / 2 do
                    let tmp = p.[i]
                    p.[i] <- p.[k - i]
                    p.[k - i] <- tmp
                flips <- flips + 1
            if flips > maxFlips then
                maxFlips <- flips

        // Generate next permutation
        let mutable i = 1
        let mutable found = false
        while i < n && not found do
            // Rotate first i+1 elements left by 1
            let t = perm.[0]
            for j in 0 .. i - 1 do
                perm.[j] <- perm.[j + 1]
            perm.[i] <- t
            count.[i] <- count.[i] - 1
            if count.[i] > 0 then
                found <- true
            else
                count.[i] <- i
                i <- i + 1
        if i >= n then
            running <- false

    maxFlips

// n=9 gives reasonable runtime
printfn "%d" (fannkuch 9)
