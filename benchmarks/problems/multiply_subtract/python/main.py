# Exercises multiply-subtract fusion in a hot loop.


def multiply_subtract(minuend, left, right):
    return minuend - (left * right)


n = 1_000_000
result = 0
while n > 0:
    result = multiply_subtract(1_000_000, n, n)
    n -= 1

print(result)
