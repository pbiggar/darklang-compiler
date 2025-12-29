#!/usr/bin/env python3
# Edigits Benchmark - Computing digits of e
# Uses series expansion: e = sum(1/n!) for n=0 to infinity
# Computes checksum of first N digits

def compute_e_digits(num_digits):
    # We need extra precision for computation
    precision = num_digits + 10
    base = 10

    # Work with scaled integers
    # Start with e = 1 (scaled by 10^precision)
    e_scaled = [0] * (precision + 1)
    e_scaled[0] = 1  # represents 1.0

    # Add 1/n! for n = 1, 2, 3, ...
    # 1/n! = (1/(n-1)!) / n
    term = [0] * (precision + 1)
    term[0] = 1  # start with 1

    for n in range(1, 51):
        # term = term / n
        carry = 0
        for i in range(precision + 1):
            current = carry * base + term[i]
            term[i] = current // n
            carry = current % n

        # e_scaled += term
        carry2 = 0
        for i in range(precision, -1, -1):
            total = e_scaled[i] + term[i] + carry2
            e_scaled[i] = total % base
            carry2 = total // base

    # Extract digits
    return e_scaled[:num_digits]

# Compute first 1000 digits of e multiple times
checksum = 0
for _ in range(10):
    digits = compute_e_digits(1000)
    checksum = 0
    for i, d in enumerate(digits):
        checksum = (checksum + d * (i + 1)) % 1000000007

print(checksum)
