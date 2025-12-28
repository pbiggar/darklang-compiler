#!/usr/bin/env python3
# Collatz Benchmark - Python reference implementation
# Counts steps in Collatz sequences for numbers 1 to n

def collatz_steps(n):
    """Count steps to reach 1 in Collatz sequence."""
    steps = 0
    while n != 1:
        if n % 2 == 0:
            n = n // 2
        else:
            n = 3 * n + 1
        steps += 1
    return steps

def sum_collatz(limit):
    """Sum Collatz steps for all numbers from 1 to limit."""
    total = 0
    for i in range(1, limit + 1):
        total += collatz_steps(i)
    return total

# Sum steps for numbers 1 to 100000
result = sum_collatz(100000)
print(result)
