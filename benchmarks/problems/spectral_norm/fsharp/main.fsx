// Spectral Norm Benchmark
// From: Computer Language Benchmarks Game

let a i j =
    1.0 / float ((i + j) * (i + j + 1) / 2 + i + 1)

let av n (v: float[]) (out: float[]) =
    for i in 0 .. n - 1 do
        let mutable s = 0.0
        for j in 0 .. n - 1 do
            s <- s + a i j * v.[j]
        out.[i] <- s

let atv n (v: float[]) (out: float[]) =
    for i in 0 .. n - 1 do
        let mutable s = 0.0
        for j in 0 .. n - 1 do
            s <- s + a j i * v.[j]
        out.[i] <- s

let atav n v out tmp =
    av n v tmp
    atv n tmp out

let spectralNorm n =
    let u = Array.create n 1.0
    let v = Array.zeroCreate n
    let tmp = Array.zeroCreate n

    for _ in 0 .. 9 do
        atav n u v tmp
        atav n v u tmp

    let mutable vBv = 0.0
    let mutable vv = 0.0
    for i in 0 .. n - 1 do
        vBv <- vBv + u.[i] * v.[i]
        vv <- vv + v.[i] * v.[i]

    sqrt (vBv / vv)

// n = 100 is a reasonable size
let result = spectralNorm 100
printfn "%d" (int64 (result * 1000000000.0))
