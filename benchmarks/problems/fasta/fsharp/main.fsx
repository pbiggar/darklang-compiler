// Fasta Benchmark - DNA Sequence Generation
// Generates pseudo-random DNA sequences and computes checksum

let im = 139968UL
let ia = 3877UL
let ic = 29573UL

type Random(seed: uint64) =
    let mutable last = seed
    member this.Next(max: float) =
        last <- (last * ia + ic) % im
        max * float last / float im

let alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"B

type IUB = { c: byte; mutable p: float }

let makeCumulative (table: IUB[]) =
    let mutable cp = 0.0
    for item in table do
        cp <- cp + item.p
        item.p <- cp

let selectRandom (table: IUB[]) r =
    let mutable result = table.[table.Length - 1].c
    for item in table do
        if r < item.p && result = table.[table.Length - 1].c then
            result <- item.c
    result

let makeRandomFasta n (table: IUB[]) (rand: Random) =
    let mutable checksum = 0UL
    for i in 0 .. n - 1 do
        let r = rand.Next(1.0)
        let c = selectRandom table r
        checksum <- (checksum + uint64 c * uint64 (i + 1)) % 1000000007UL
    checksum

let makeRepeatFasta n (seq: byte[]) =
    let mutable checksum = 0UL
    let len = seq.Length
    for i in 0 .. n - 1 do
        let c = seq.[i % len]
        checksum <- (checksum + uint64 c * uint64 (i + 1)) % 1000000007UL
    checksum

let n = 100000

let iub = [|
    { c = byte 'a'; p = 0.27 }
    { c = byte 'c'; p = 0.12 }
    { c = byte 'g'; p = 0.12 }
    { c = byte 't'; p = 0.27 }
    { c = byte 'B'; p = 0.02 }
    { c = byte 'D'; p = 0.02 }
    { c = byte 'H'; p = 0.02 }
    { c = byte 'K'; p = 0.02 }
    { c = byte 'M'; p = 0.02 }
    { c = byte 'N'; p = 0.02 }
    { c = byte 'R'; p = 0.02 }
    { c = byte 'S'; p = 0.02 }
    { c = byte 'V'; p = 0.02 }
    { c = byte 'W'; p = 0.02 }
    { c = byte 'Y'; p = 0.02 }
|]

let homosapiens = [|
    { c = byte 'a'; p = 0.3029549426680 }
    { c = byte 'c'; p = 0.1979883004921 }
    { c = byte 'g'; p = 0.1975473066391 }
    { c = byte 't'; p = 0.3015094502008 }
|]

makeCumulative iub
makeCumulative homosapiens

let rand = Random(42UL)

// Generate sequences and compute combined checksum
let c1 = makeRepeatFasta (n * 2) alu
let c2 = makeRandomFasta (n * 3) iub rand
let c3 = makeRandomFasta (n * 5) homosapiens rand

let total = (c1 + c2 + c3) % 1000000007UL
printfn "%d" total
