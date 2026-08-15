# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-08-15T01:21:49+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 1)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `367fbac8c4f6420951592af456195c7874d231b771326a90ff149b7d6ba8ab85`
**Compiler commit:** `1fdb9ef9b2d013a9bb34b38b56c05f736bae4e52` - Align List language and module with interpreter

| Benchmark     |           Dark (2.42x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 11,450,298,696 (2.46x) | 4,651,994,510 |
| binary_trees  |    675,306,228 (0.37x) | 1,842,791,955 |
| collatz       |     81,543,084 (0.83x) |    98,242,178 |
| edigits       |   4,664,674,103 (342x) |    13,637,551 |
| factorial     |         64,214 (0.25x) |       257,669 |
| fasta         |    549,966,883 (27.2x) |    20,252,213 |
| fib           |    642,006,238 (2.49x) |   257,598,132 |
| leibniz       |    850,001,522 (1.06x) |   800,257,637 |
| mandelbrot    |     17,621,940 (1.30x) |    13,595,721 |
| matmul        |   2,027,686,855 (127x) |    15,983,852 |
| merkletrees   |    416,150,237 (3.34x) |   124,776,610 |
| nbody         |  1,214,502,260 (4.90x) |   247,760,534 |
| nqueen        |    276,882,241 (1.98x) |   139,988,273 |
| pisum         |         95,431 (0.00x) |    50,258,602 |
| primes        |      2,075,933 (1.53x) |     1,358,980 |
| quicksort     |    226,373,955 (34.4x) |     6,574,976 |
| spectral_norm |     79,204,301 (15.0x) |     5,297,561 |
| sum_to_n      |         71,873 (0.28x) |       257,603 |
| tak           |     63,580,610 (1.88x) |    33,730,191 |
