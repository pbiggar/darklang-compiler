// Merkle Trees Benchmark
// Builds binary Merkle trees and computes root hashes
// Uses a simple hash function for portability

// Simple hash function (FNV-1a variant)
// Using BigInt for 64-bit operations
function hashVal(data) {
    let h = 14695981039346656037n;
    const mask = 0xFFFFFFFFFFFFFFFFn;
    for (let i = 0; i < 8; i++) {
        h ^= BigInt(data) & 0xFFn;
        h = (h * 1099511628211n) & mask;
    }
    return h;
}

function hashPair(left, right) {
    const combined = (left + (right * 31n)) & 0xFFFFFFFFFFFFFFFFn;
    return hashVal(Number(combined & 0xFFFFFFFFFFFFFFFFn));
}

// Build a complete binary Merkle tree of given depth
// Returns the root hash
function buildTree(depth, leafStart) {
    if (depth === 0) {
        return hashVal(leafStart);
    }

    const leftSize = 1 << (depth - 1);
    const left = buildTree(depth - 1, leafStart);
    const right = buildTree(depth - 1, leafStart + leftSize);
    return hashPair(left, right);
}

// Verify a tree by rebuilding and comparing
function verifyTree(depth, leafStart, expectedRoot) {
    return buildTree(depth, leafStart) === expectedRoot;
}

const depth = 15;
const iterations = 50;

let checksum = 0n;

for (let i = 0; i < iterations; i++) {
    // Build tree
    const root = buildTree(depth, i);

    // Verify tree
    const verified = verifyTree(depth, i, root);

    // Accumulate checksum
    checksum = (checksum + root) % 1000000007n;
    if (verified) {
        checksum = (checksum + 1n) % 1000000007n;
    }
}

console.log(checksum.toString());
