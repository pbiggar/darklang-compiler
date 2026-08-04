# Exercises elimination of an identical-arm value selection in a hot loop.


def select_same(value):
    condition = value % 2 == 0
    return (value if condition else value) + 1


n = 1_000_000
total = 0
while n > 0:
    total += select_same(n)
    n -= 1

print(total)
