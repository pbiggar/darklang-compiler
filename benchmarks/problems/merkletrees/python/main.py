#!/usr/bin/env python3
# Merkle Trees Benchmark
# Builds binary Merkle trees and computes root hashes
# Uses a simple hash function for portability

# Simple hash function (FNV-1a variant)
def hash_val(data):
    h = 14695981039346656037
    for _ in range(8):
        h ^= data & 0xFF
        h = (h * 1099511628211) & 0xFFFFFFFFFFFFFFFF
    return h

def hash_pair(left, right):
    combined = (left + (right * 31)) & 0xFFFFFFFFFFFFFFFF
    return hash_val(combined)

# Build a complete binary Merkle tree of given depth
# Returns the root hash
def build_tree(depth, leaf_start):
    if depth == 0:
        return hash_val(leaf_start)

    left_size = 1 << (depth - 1)
    left = build_tree(depth - 1, leaf_start)
    right = build_tree(depth - 1, leaf_start + left_size)
    return hash_pair(left, right)

# Verify a tree by rebuilding and comparing
def verify_tree(depth, leaf_start, expected_root):
    return build_tree(depth, leaf_start) == expected_root

depth = 15
iterations = 50

checksum = 0

for i in range(iterations):
    # Build tree
    root = build_tree(depth, i)

    # Verify tree
    verified = verify_tree(depth, i, root)

    # Accumulate checksum
    checksum = (checksum + root) % 1000000007
    if verified:
        checksum = (checksum + 1) % 1000000007

print(checksum)
