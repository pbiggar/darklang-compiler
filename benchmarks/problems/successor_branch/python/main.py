# Exercises repeated successor conditions in a hot loop.


def choose(condition):
    if condition:
        if condition:
            return 1
        return 0
    return 0


result = 0
for n in range(1_000_000, 0, -1):
    result += choose((n & 1) == 0)

print(result)
