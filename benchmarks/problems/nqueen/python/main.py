#!/usr/bin/env python3
# N-Queens Benchmark - Python reference implementation
# From: plb2 (Programming Language Benchmark v2)
# Counts solutions to the N-queens problem using bit manipulation

def nqueen(n):
    """Count all solutions to N-queens problem using bit manipulation."""
    count = 0
    all_ones = (1 << n) - 1

    def solve(cols, diag1, diag2):
        nonlocal count
        if cols == all_ones:
            count += 1
            return
        # Available positions (not attacked)
        avail = all_ones & ~(cols | diag1 | diag2)
        while avail:
            # Get rightmost available position
            pos = avail & -avail
            avail -= pos
            solve(cols | pos, (diag1 | pos) << 1, (diag2 | pos) >> 1)

    solve(0, 0, 0)
    return count

# N=13 gives reasonable runtime
result = nqueen(13)
print(result)
