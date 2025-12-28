#!/usr/bin/env python3
# Leibniz Pi Benchmark - Python reference implementation
# Computes pi using Leibniz formula: pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...

def leibniz_pi(n):
    """Compute pi approximation using n terms of Leibniz series."""
    s = 0.0
    sign = 1.0
    for i in range(n):
        s += sign / (2 * i + 1)
        sign = -sign
    return s * 4

# Use 100 million iterations for timing
result = leibniz_pi(100000000)
# Output as integer (multiply by large factor for precision)
print(int(result * 100000000))
