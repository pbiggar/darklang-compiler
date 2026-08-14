# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-08-15T02:34:48+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 1)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `367fbac8c4f6420951592af456195c7874d231b771326a90ff149b7d6ba8ab85`
**Compiler commit:** `153d9588152ac5ba7b98ce8d699eacea1abc3b0e` - Eliminate redundant ARM64 entry parameter copies

| Benchmark     |           Dark (2.25x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 10,019,035,996 (2.15x) | 4,651,994,510 |
| binary_trees  |    649,092,194 (0.35x) | 1,842,791,955 |
| collatz       |     81,143,046 (0.83x) |    98,242,178 |
| edigits       |   4,528,733,711 (332x) |    13,637,551 |
| factorial     |         64,094 (0.25x) |       257,669 |
| fasta         |    514,839,089 (25.4x) |    20,252,213 |
| fib           |    418,050,947 (1.62x) |   257,598,132 |
| leibniz       |    850,001,482 (1.06x) |   800,257,637 |
| mandelbrot    |     17,459,510 (1.28x) |    13,595,721 |
| matmul        |   1,968,521,113 (123x) |    15,983,852 |
| merkletrees   |    389,936,193 (3.13x) |   124,776,610 |
| nbody         |  1,193,502,228 (4.82x) |   247,760,534 |
| nqueen        |    221,668,085 (1.58x) |   139,988,273 |
| pisum         |         95,371 (0.00x) |    50,258,602 |
| primes        |      2,045,919 (1.51x) |     1,358,980 |
| quicksort     |    222,570,011 (33.9x) |     6,574,976 |
| spectral_norm |     74,939,493 (14.1x) |     5,297,561 |
| sum_to_n      |         71,833 (0.28x) |       257,603 |
| tak           |     52,360,530 (1.55x) |    33,730,191 |
