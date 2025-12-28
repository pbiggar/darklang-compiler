#!/usr/bin/env python3
# Prime Counting Benchmark - Python reference implementation
import math

def is_prime(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    limit = int(math.sqrt(n))
    for d in range(3, limit + 1):
        if n % d == 0:
            return False
    return True

def count_primes(n):
    count = 0
    for i in range(2, n + 1):
        if is_prime(i):
            count += 1
    return count

# Count primes up to 10000
result = count_primes(10000)
print(result)
