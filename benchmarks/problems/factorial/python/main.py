# main.py - Parameterized Python benchmark implementation.
import sys

def argument(index):
    return int(sys.argv[index + 1])

import sys
sys.setrecursionlimit(20000)

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def repeat(n, acc):
    if n <= 0:
        return acc
    return repeat(n - 1, factorial(argument(1)))

print(repeat(argument(0), 0))
