# Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
import cmath
import math
import sys
MOD = 1_000_000_007

def fft(values):
    if len(values) <= 1:
        return values
    even, odd = fft(values[::2]), fft(values[1::2])
    twiddled = [cmath.exp(complex(0, -math.tau*i/len(values))) * z
                for i, z in enumerate(odd)]
    return [a+b for a,b in zip(even,twiddled)] + [a-b for a,b in zip(even,twiddled)]

n, runs = map(int, sys.argv[1:])
values = [complex(math.sin(i*.017)+math.cos(i*.031),
                  math.cos(i*.013)-math.sin(i*.007)) for i in range(n)]
total = 0
for _ in range(runs):
    result = fft(values)
    total = (total + sum(int((z.real*3+z.imag*5)*1e6)*(i+1)
                         for i,z in enumerate(result))) % MOD
print(total)
