// Binary Trees Benchmark
// Counts nodes in complete binary trees

let rec countTree depth =
    if depth <= 0 then 1
    else 1 + countTree (depth - 1) + countTree (depth - 1)

let rec stressTest depth iterations acc =
    if iterations <= 0 then acc
    else
        let count = countTree depth
        stressTest depth (iterations - 1) (acc + count)

// Run stress test: create many trees of depth 15
// Each complete binary tree of depth 15 has 2^16 - 1 = 65535 nodes
// Do 100 iterations
printfn "%d" (stressTest 15 100 0)
