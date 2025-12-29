// N-Queens Benchmark
// Counts solutions to the N-queens problem using bit manipulation

let nqueen n =
    let allOnes = (1 <<< n) - 1

    let rec solve cols diag1 diag2 =
        if cols = allOnes then 1L
        else
            let mutable count = 0L
            let mutable avail = allOnes &&& ~~~(cols ||| diag1 ||| diag2)
            while avail <> 0 do
                let pos = avail &&& (-avail)
                avail <- avail - pos
                count <- count + solve (cols ||| pos) ((diag1 ||| pos) <<< 1) ((diag2 ||| pos) >>> 1)
            count

    solve 0 0 0

// N=13 gives reasonable runtime
printfn "%d" (nqueen 13)
