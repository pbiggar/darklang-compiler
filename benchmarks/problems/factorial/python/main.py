import sys
sys.setrecursionlimit(20000)

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def repeat(n, acc):
    if n <= 0:
        return acc
    return repeat(n - 1, factorial(20))

print(repeat(10000, 0))
