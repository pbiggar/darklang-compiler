# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-08-14T18:40:59+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 1)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `6dbb096b37aaf32192bf960168fde271a2595f94f83653b43b94ec5b2e104758`
**Compiler commit:** `6452e96afa1528fb3a92208c44e5752d87d4c45f` - Repair CLI integration after grammar rebase

| Benchmark     |           Dark (2.75x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 11,450,298,696 (2.46x) | 4,651,994,510 |
| binary_trees  |    675,306,228 (0.37x) | 1,842,791,955 |
| collatz       |     81,543,084 (0.83x) |    98,242,178 |
| edigits       |   6,667,246,313 (489x) |    13,637,551 |
| factorial     |         64,214 (0.25x) |       257,669 |
| fasta         |    726,627,716 (35.9x) |    20,252,213 |
| fib           |    642,006,238 (2.49x) |   257,598,132 |
| leibniz       |    900,001,522 (1.12x) |   800,257,637 |
| mandelbrot    |     17,701,940 (1.30x) |    13,595,721 |
| matmul        |   2,043,852,085 (128x) |    15,983,852 |
| merkletrees   |    724,164,737 (5.80x) |   124,776,610 |
| nbody         |  1,239,502,646 (5.00x) |   247,760,534 |
| nqueen        |    295,286,949 (2.11x) |   139,988,273 |
| pisum         |         95,431 (0.00x) |    50,258,602 |
| primes        |      2,075,933 (1.53x) |     1,358,980 |
| quicksort     |    378,608,148 (57.6x) |     6,574,976 |
| spectral_norm |    143,862,572 (27.2x) |     5,297,561 |
| sum_to_n      |         71,873 (0.28x) |       257,603 |
| tak           |     63,580,610 (1.88x) |    33,730,191 |
