#!/usr/bin/env python3
# Ackermann Benchmark - Python reference implementation
import sys
sys.setrecursionlimit(100000)

def ackermann(m, n):
    if m == 0:
        return n + 1
    elif n == 0:
        return ackermann(m - 1, 1)
    else:
        return ackermann(m - 1, ackermann(m, n - 1))

# A(3, 12) = 32765
result = ackermann(3, 12)
print(result)
