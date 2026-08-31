# main.py - Parameterized Python benchmark implementation.
import sys

def argument(index):
    return int(sys.argv[index + 1])

import sys
sys.setrecursionlimit(11000)

def sum_to(n, acc):
    if n <= 0:
        return acc
    return sum_to(n - 1, acc + n)

def repeat(n, sum_input, acc):
    if n <= 0:
        return acc
    return repeat(n - 1, sum_input, sum_to(sum_input, 0))

print(repeat(argument(0), argument(1), 0))
