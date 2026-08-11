# Benchmark Results

Latest routine-profile Dark performance vs audited Rust references (instruction counts).

**Last Updated:** 2026-08-11 21:03:42
**Commit:** `94173df3` - Reuse the type-checker module registry

| Benchmark     |           Dark (2.77x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 11,450,298,027 (2.46x) | 4,651,994,510 |
| binary_trees  |    685,135,033 (0.37x) | 1,842,791,955 |
| collatz       |     81,541,905 (0.83x) |    98,242,178 |
| edigits       |   6,667,246,322 (489x) |    13,637,551 |
| factorial     |         60,603 (0.24x) |       257,669 |
| fasta         |    729,121,675 (36.0x) |    20,252,213 |
| fib           |    642,005,209 (2.49x) |   257,598,132 |
| leibniz       |    900,000,145 (1.12x) |   800,257,637 |
| mandelbrot    |     20,790,992 (1.53x) |    13,595,721 |
| matmul        |   2,124,300,694 (133x) |    15,983,852 |
| merkletrees   |    724,163,397 (5.80x) |   124,776,610 |
| nbody         |  1,239,501,716 (5.00x) |   247,760,534 |
| nqueen        |    295,286,289 (2.11x) |   139,988,273 |
| pisum         |         93,199 (0.00x) |    50,258,602 |
| primes        |      2,075,441 (1.53x) |     1,358,980 |
| quicksort     |    378,606,804 (57.6x) |     6,574,976 |
| spectral_norm |    144,269,572 (27.2x) |     5,297,561 |
| sum_to_n      |         70,747 (0.27x) |       257,603 |
| tak           |     63,580,544 (1.88x) |    33,730,191 |
