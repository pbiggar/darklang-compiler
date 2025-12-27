import sys
sys.setrecursionlimit(11000)

def sum_to(n, acc):
    if n <= 0:
        return acc
    return sum_to(n - 1, acc + n)

def repeat(n, acc):
    if n <= 0:
        return acc
    return repeat(n - 1, sum_to(10000, 0))

print(repeat(100, 0))
