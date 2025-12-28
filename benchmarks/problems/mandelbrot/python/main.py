#!/usr/bin/env python3
# Mandelbrot Benchmark - Python reference implementation
# Counts points in the Mandelbrot set
# From: Computer Language Benchmarks Game

def mandelbrot(cr, ci, max_iter):
    zr = zi = 0.0
    for i in range(max_iter):
        if zr * zr + zi * zi > 4.0:
            return 0
        zr, zi = zr * zr - zi * zi + cr, 2.0 * zr * zi + ci
    return 1

def count_mandelbrot(size, max_iter):
    count = 0
    for y in range(size):
        ci = 2.0 * y / size - 1.0
        for x in range(size):
            cr = 2.0 * x / size - 1.5
            count += mandelbrot(cr, ci, max_iter)
    return count

# Count points in 200x200 grid with 50 iterations
result = count_mandelbrot(200, 50)
print(result)
