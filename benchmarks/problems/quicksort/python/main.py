#!/usr/bin/env python3
# Quicksort Benchmark - Python reference implementation
# Sorts a list and returns a checksum

def quicksort(arr):
    if len(arr) <= 1:
        return arr
    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    return quicksort(left) + middle + quicksort(right)

def generate_list(n, seed):
    """Generate a pseudo-random list using LCG"""
    result = []
    x = seed
    for _ in range(n):
        x = (x * 1103515245 + 12345) % (2**31)
        result.append(x % 10000)
    return result

def checksum(arr):
    """Compute a simple checksum of the sorted array"""
    result = 0
    for i, x in enumerate(arr):
        result = (result + x * (i + 1)) % 1000000007
    return result

# Sort 100 elements (reduced to match Dark heap constraints)
arr = generate_list(100, 42)
sorted_arr = quicksort(arr)
print(checksum(sorted_arr))
