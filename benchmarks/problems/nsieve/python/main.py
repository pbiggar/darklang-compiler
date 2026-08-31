# main.py - Parameterized Python benchmark implementation.
import sys

def argument(index):
    return int(sys.argv[index + 1])

#!/usr/bin/env python3
# Nsieve Benchmark - Sieve of Eratosthenes
# Counts primes up to n using sieve algorithm

def nsieve(n):
    is_prime = [True] * (n + 1)
    count = 0

    for i in range(2, n + 1):
        if is_prime[i]:
            count += 1
            # Mark multiples as not prime
            j = i + i
            while j <= n:
                is_prime[j] = False
                j += i
    return count

# Run sieve multiple times for meaningful benchmark
total = 0
for _ in range(argument(1)):
    total = nsieve(argument(0))
print(total)
