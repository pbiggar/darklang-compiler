// main.fsx - Parameterized F# benchmark implementation.
let argument index = int fsi.CommandLineArgs.[index + 1]
let argument64 index = int64 fsi.CommandLineArgs.[index + 1]

// Mandelbrot Benchmark

let mandelbrot cr ci maxIter =
    let mutable zr = 0.0
    let mutable zi = 0.0
    let mutable i = 0
    let mutable escaped = false
    while i < maxIter && not escaped do
        if zr * zr + zi * zi > 4.0 then
            escaped <- true
        else
            let newZr = zr * zr - zi * zi + cr
            let newZi = 2.0 * zr * zi + ci
            zr <- newZr
            zi <- newZi
            i <- i + 1
    if escaped then 0 else 1

let countMandelbrot size maxIter =
    let mutable count = 0
    for y in 0 .. size - 1 do
        let ci = 2.0 * float y / float size - 1.0
        for x in 0 .. size - 1 do
            let cr = 2.0 * float x / float size - 1.5
            count <- count + mandelbrot cr ci maxIter
    count

// Count points in 200x200 grid with 50 iterations
printfn "%d" (countMandelbrot (argument 0) (argument 1))
