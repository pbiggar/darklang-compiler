// foundations_reference.fsx - Observe frozen F# foundation semantics.
#load "../../src/DarkCompiler/Crash.fs"
#load "../../src/DarkCompiler/Bitset.fs"
#load "../../src/DarkCompiler/ResultList.fs"
#load "../../src/DarkCompiler/memory/RuntimeDataLayout.fs"

let integers xs = "[" + (xs |> List.map string |> String.concat ",") + "]"
let words (xs: uint64 array) = "[" + (xs |> Array.map (fun x -> "\"" + x.ToString("x16") + "\"") |> String.concat ",") + "]"
let observe bits = $"[{words bits},{integers (Bitset.indicesToList bits)},{Bitset.count bits}]"

for bitCount in [0; 1; 7; 63; 64; 65; 127; 128; 129; 257; 1024] do
    for seed in 0 .. 31 do
        let left = Bitset.empty (Bitset.wordCount bitCount)
        let right = Bitset.empty (Bitset.wordCount bitCount)
        for index in 0 .. bitCount - 1 do
            if (index * 17 + seed) % 5 = 0 then Bitset.addIndexInPlace index left
            if (index * 11 + seed) % 7 = 0 then Bitset.addIndexInPlace index right
        let merged = Bitset.union left right
        let difference = Bitset.diff left right
        let intersection = Bitset.intersectMany left [right]
        let mutableUnion = Bitset.clone left
        Bitset.unionInPlace mutableUnion right
        let mutableDiff = Bitset.clone left
        Bitset.diffInPlace mutableDiff right
        let mutableIntersection = Bitset.clone left
        Bitset.intersectInPlace mutableIntersection right
        printfn "[%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s]" bitCount seed (observe left) (observe right) (observe (Bitset.all bitCount)) (observe merged) (observe difference) (observe intersection) (observe mutableUnion) (observe mutableDiff) (observe mutableIntersection)

for endOffset in [0; 1; 65535; 65536; 65537; 1048576] do
    printfn "[%d,%d]" endOffset (RuntimeDataLayout.elfCounterOffset endOffset)

let events = ResizeArray<int>()
let result = ResultList.mapResults (fun value -> events.Add value; if value = 4 then Error "stop" else Ok (value * 2)) [1; 2; 3; 4; 5]
printfn "[%s,%s]" (integers (List.ofSeq events)) (if result = Error "stop" then "true" else "false")
match ResultList.collectResults (fun value -> Ok [value; value + 10]) [1; 2; 3] with
| Ok values -> printfn "%s" (integers values)
| Error _ -> Crash.crash "Foundation result collector failed"
