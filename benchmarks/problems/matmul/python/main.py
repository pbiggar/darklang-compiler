#!/usr/bin/env python3
# Matrix Multiplication Benchmark - Python reference implementation
# From: plb2 and Julia micro-benchmarks
# Multiplies two NxN matrices using naive O(n^3) algorithm

def matmul(a, b, n):
    """Multiply two NxN matrices."""
    c = [[0] * n for _ in range(n)]
    for i in range(n):
        for j in range(n):
            s = 0
            for k in range(n):
                s += a[i][k] * b[k][j]
            c[i][j] = s
    return c

def generate_matrix(n, seed):
    """Generate an NxN matrix with pseudo-random values."""
    matrix = []
    x = seed
    for i in range(n):
        row = []
        for j in range(n):
            x = (x * 1103515245 + 12345) % (2**31)
            row.append(x % 100)
        matrix.append(row)
    return matrix

def checksum(m, n):
    """Compute checksum of matrix."""
    result = 0
    for i in range(n):
        for j in range(n):
            result = (result + m[i][j] * (i * n + j + 1)) % 1000000007
    return result

# Use 100x100 matrices for reasonable runtime
n = 100
a = generate_matrix(n, 42)
b = generate_matrix(n, 123)
c = matmul(a, b, n)
print(checksum(c, n))
