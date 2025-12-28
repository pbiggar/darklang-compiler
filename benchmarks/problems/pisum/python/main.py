#!/usr/bin/env python3
# Pi Summation Benchmark - Python reference implementation
# From: Julia micro-benchmarks
# Computes partial sum of 1/k^2 series (converges to pi^2/6)

def pisum(n):
    """Sum 1/k^2 for k=1 to n, repeated 500 times for timing."""
    s = 0.0
    for _ in range(500):
        s = 0.0
        for k in range(1, n + 1):
            s += 1.0 / (k * k)
    return s

# n=10000 gives reasonable runtime
result = pisum(10000)
# Output as integer (multiply by large factor for precision)
print(int(result * 1000000000000))
