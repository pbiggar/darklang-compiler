# Benchmark Results

Latest routine-profile Dark performance vs audited Rust references (instruction counts).

**Last Updated:** 2026-08-14 04:01:32
**Commit:** `c97f99a1` - dcb2: accept design for Implement Char, String, and Regex parity

| Benchmark     |           Dark (2.75x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 11,450,298,699 (2.46x) | 4,651,994,510 |
| binary_trees  |    675,306,237 (0.37x) | 1,842,791,955 |
| collatz       |     81,543,096 (0.83x) |    98,242,178 |
| edigits       |   6,667,246,322 (489x) |    13,637,551 |
| factorial     |         64,259 (0.25x) |       257,669 |
| fasta         |    726,627,731 (35.9x) |    20,252,213 |
| fib           |    642,006,247 (2.49x) |   257,598,132 |
| leibniz       |    900,001,537 (1.12x) |   800,257,637 |
| mandelbrot    |     17,701,943 (1.30x) |    13,595,721 |
| matmul        |   2,106,152,103 (132x) |    15,983,852 |
| merkletrees   |    724,164,752 (5.80x) |   124,776,610 |
| nbody         |  1,239,502,637 (5.00x) |   247,760,534 |
| nqueen        |    295,286,952 (2.11x) |   139,988,273 |
| pisum         |         95,458 (0.00x) |    50,258,602 |
| primes        |      2,075,933 (1.53x) |     1,358,980 |
| quicksort     |    378,608,163 (57.6x) |     6,574,976 |
| spectral_norm |    143,862,590 (27.2x) |     5,297,561 |
| sum_to_n      |         71,885 (0.28x) |       257,603 |
| tak           |     63,580,601 (1.88x) |    33,730,191 |
