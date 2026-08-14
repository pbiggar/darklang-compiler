# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-08-14T17:03:11+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 1)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `6dbb096b37aaf32192bf960168fde271a2595f94f83653b43b94ec5b2e104758`
**Compiler commit:** `aa0c36de548eaaddd363b6497ac249ed9c2e3134` - Align identifier and qualified-name grammar

| Benchmark     |           Dark (2.75x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 11,450,298,687 (2.46x) | 4,651,994,510 |
| binary_trees  |    675,306,219 (0.37x) | 1,842,791,955 |
| collatz       |     81,543,075 (0.83x) |    98,242,178 |
| edigits       |   6,667,246,304 (489x) |    13,637,551 |
| factorial     |         64,205 (0.25x) |       257,669 |
| fasta         |    726,627,707 (35.9x) |    20,252,213 |
| fib           |    642,006,229 (2.49x) |   257,598,132 |
| leibniz       |    900,001,513 (1.12x) |   800,257,637 |
| mandelbrot    |     17,701,931 (1.30x) |    13,595,721 |
| matmul        |   2,043,852,076 (128x) |    15,983,852 |
| merkletrees   |    724,164,728 (5.80x) |   124,776,610 |
| nbody         |  1,239,502,637 (5.00x) |   247,760,534 |
| nqueen        |    295,286,940 (2.11x) |   139,988,273 |
| pisum         |         95,422 (0.00x) |    50,258,602 |
| primes        |      2,075,924 (1.53x) |     1,358,980 |
| quicksort     |    378,608,139 (57.6x) |     6,574,976 |
| spectral_norm |    143,862,563 (27.2x) |     5,297,561 |
| sum_to_n      |         71,864 (0.28x) |       257,603 |
| tak           |     63,580,601 (1.88x) |    33,730,191 |
