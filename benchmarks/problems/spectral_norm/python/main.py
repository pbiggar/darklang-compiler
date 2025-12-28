#!/usr/bin/env python3
# Spectral Norm Benchmark - Python reference implementation
# From: Computer Language Benchmarks Game

import math

def A(i, j):
    return 1.0 / ((i + j) * (i + j + 1) // 2 + i + 1)

def Av(n, v, out):
    for i in range(n):
        s = 0.0
        for j in range(n):
            s += A(i, j) * v[j]
        out[i] = s

def Atv(n, v, out):
    for i in range(n):
        s = 0.0
        for j in range(n):
            s += A(j, i) * v[j]
        out[i] = s

def AtAv(n, v, out, tmp):
    Av(n, v, tmp)
    Atv(n, tmp, out)

def spectral_norm(n):
    u = [1.0] * n
    v = [0.0] * n
    tmp = [0.0] * n

    for _ in range(10):
        AtAv(n, u, v, tmp)
        AtAv(n, v, u, tmp)

    vBv = vv = 0.0
    for i in range(n):
        vBv += u[i] * v[i]
        vv += v[i] * v[i]

    return math.sqrt(vBv / vv)

# n = 100 is a reasonable size
result = spectral_norm(100)
print(int(result * 1000000000))
