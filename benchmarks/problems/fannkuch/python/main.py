#!/usr/bin/env python3
# Fannkuch Benchmark - Python reference implementation
# From: Computer Language Benchmarks Game
# Computes the maximum number of flips in Fannkuch sequence

def fannkuch(n):
    perm = list(range(n))
    count = list(range(n))
    max_flips = 0
    checksum = 0
    nperm = 0

    while True:
        # Count flips for current permutation
        if perm[0] != 0:
            flips = 0
            p = perm[:]
            while p[0] != 0:
                k = p[0]
                # Reverse first k+1 elements
                p[:k+1] = p[:k+1][::-1]
                flips += 1
            max_flips = max(max_flips, flips)
            checksum += flips if nperm % 2 == 0 else -flips

        # Generate next permutation
        i = 1
        while i < n:
            # Rotate first i+1 elements left by 1
            t = perm[0]
            perm[:i+1] = perm[1:i+1] + [t]
            count[i] -= 1
            if count[i] > 0:
                break
            count[i] = i
            i += 1
        else:
            break
        nperm += 1

    return max_flips

# n=9 gives reasonable runtime
result = fannkuch(9)
print(result)
