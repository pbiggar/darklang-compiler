# Exercises shared pure leading work across conditional branches.


def select(value):
    if value > 0:
        shared = value * 3
        return shared + 1
    else:
        shared = value * 3
        return shared - 1


n = 1_000_000
acc = 0
while n > 0:
    acc += select(n)
    n -= 1

print(acc)
