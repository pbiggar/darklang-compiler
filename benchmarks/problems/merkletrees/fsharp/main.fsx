// Merkle Trees Benchmark
// Builds binary Merkle trees and computes root hashes
// Uses a simple hash function for portability

// Simple hash function (FNV-1a variant)
let hashVal (data: uint64) =
    let mutable h = 14695981039346656037UL
    for _ in 0 .. 7 do
        h <- h ^^^ (data &&& 0xFFUL)
        h <- h * 1099511628211UL
    h

let hashPair left right =
    let combined = left + right * 31UL
    hashVal combined

// Build a complete binary Merkle tree of given depth
// Returns the root hash
let rec buildTree depth leafStart =
    if depth = 0 then hashVal leafStart
    else
        let leftSize = 1UL <<< (depth - 1)
        let left = buildTree (depth - 1) leafStart
        let right = buildTree (depth - 1) (leafStart + leftSize)
        hashPair left right

// Verify a tree by rebuilding and comparing
let verifyTree depth leafStart expectedRoot =
    buildTree depth leafStart = expectedRoot

let depth = 15
let iterations = 50

let mutable checksum = 0UL

for i in 0 .. iterations - 1 do
    // Build tree
    let root = buildTree depth (uint64 i)

    // Verify tree
    let verified = verifyTree depth (uint64 i) root

    // Accumulate checksum
    checksum <- (checksum + root) % 1000000007UL
    if verified then
        checksum <- (checksum + 1UL) % 1000000007UL

printfn "%d" checksum
