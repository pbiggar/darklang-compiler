// Merkle Trees Benchmark
// Builds binary Merkle trees and computes root hashes
// Uses a simple hash function for portability

// Simple hash function (FNV-1a variant)
fn hash(data: u64) -> u64 {
    let mut h: u64 = 14695981039346656037;
    for _ in 0..8 {
        h ^= data & 0xFF;
        h = h.wrapping_mul(1099511628211);
    }
    h
}

fn hash_pair(left: u64, right: u64) -> u64 {
    let combined = left.wrapping_add(right.wrapping_mul(31));
    hash(combined)
}

// Build a complete binary Merkle tree of given depth
// Returns the root hash
fn build_tree(depth: u32, leaf_start: u64) -> u64 {
    if depth == 0 {
        return hash(leaf_start);
    }

    let left_size = 1u64 << (depth - 1);
    let left = build_tree(depth - 1, leaf_start);
    let right = build_tree(depth - 1, leaf_start + left_size);
    hash_pair(left, right)
}

// Verify a tree by rebuilding and comparing
fn verify_tree(depth: u32, leaf_start: u64, expected_root: u64) -> bool {
    build_tree(depth, leaf_start) == expected_root
}

fn main() {
    let depth = 15;
    let iterations = 50;

    let mut checksum: u64 = 0;

    for i in 0..iterations {
        // Build tree
        let root = build_tree(depth, i as u64);

        // Verify tree
        let verified = verify_tree(depth, i as u64, root);

        // Accumulate checksum
        checksum = (checksum.wrapping_add(root)) % 1000000007;
        if verified {
            checksum = (checksum.wrapping_add(1)) % 1000000007;
        }
    }

    println!("{}", checksum);
}
