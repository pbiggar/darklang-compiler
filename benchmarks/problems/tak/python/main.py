#!/usr/bin/env python3
# Tak (Takeuchi) Benchmark - Python reference implementation
import sys
sys.setrecursionlimit(100000)

def tak(x, y, z):
    if x <= y:
        return z
    else:
        return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))

# Repeat multiple times for meaningful measurement
result = 0
for _ in range(10):
    result = tak(24, 16, 8)
print(result)
