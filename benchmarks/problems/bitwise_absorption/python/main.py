# Exercises bitwise absorption in a hot loop.


n = 1_000_000
result = 0
while n > 0:
    absorbed_and = n & (n | result)
    absorbed_or = n | (n & result)
    result = absorbed_and + absorbed_or
    n -= 1

print(result)
