// Matrix Multiplication Benchmark
// Multiplies two NxN matrices using naive O(n^3) algorithm

let matmul (a: int64[][]) (b: int64[][]) n =
    let c = Array.init n (fun _ -> Array.zeroCreate n)
    for i in 0 .. n - 1 do
        for j in 0 .. n - 1 do
            let mutable s = 0L
            for k in 0 .. n - 1 do
                s <- s + a.[i].[k] * b.[k].[j]
            c.[i].[j] <- s
    c

let generateMatrix n seed =
    let matrix = Array.init n (fun _ -> Array.zeroCreate n)
    let mutable x = seed
    for i in 0 .. n - 1 do
        for j in 0 .. n - 1 do
            x <- (x * 1103515245UL + 12345UL) % (1UL <<< 31)
            matrix.[i].[j] <- int64 (x % 100UL)
    matrix

let checksum (m: int64[][]) n =
    let mutable result = 0L
    for i in 0 .. n - 1 do
        for j in 0 .. n - 1 do
            result <- (result + m.[i].[j] * int64 (i * n + j + 1)) % 1000000007L
    result

// Use 100x100 matrices for reasonable runtime
let n = 100
let a = generateMatrix n 42UL
let b = generateMatrix n 123UL
let c = matmul a b n
printfn "%d" (checksum c n)
