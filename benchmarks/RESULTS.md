# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-08-15T07:46:18+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 1)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `367fbac8c4f6420951592af456195c7874d231b771326a90ff149b7d6ba8ab85`
**Compiler commit:** `671ab160ef4fb2de3829ee251285e09733e8c1e8` - Keep NFC-safe formatting on raw concat

| Benchmark     |           Dark (2.25x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 10,019,035,954 (2.15x) | 4,651,994,510 |
| binary_trees  |    649,092,132 (0.35x) | 1,842,791,955 |
| collatz       |     81,142,974 (0.83x) |    98,242,178 |
| edigits       |   4,528,733,649 (332x) |    13,637,551 |
| factorial     |         63,912 (0.25x) |       257,669 |
| fasta         |    514,839,007 (25.4x) |    20,252,213 |
| fib           |    418,050,885 (1.62x) |   257,598,132 |
| leibniz       |    850,001,400 (1.06x) |   800,257,637 |
| mandelbrot    |     17,459,468 (1.28x) |    13,595,721 |
| matmul        |   1,968,521,021 (123x) |    15,983,852 |
| merkletrees   |    389,936,111 (3.13x) |   124,776,610 |
| nbody         |  1,193,502,216 (4.82x) |   247,760,534 |
| nqueen        |    221,668,043 (1.58x) |   139,988,273 |
| pisum         |         95,249 (0.00x) |    50,258,602 |
| primes        |      2,045,887 (1.51x) |     1,358,980 |
| quicksort     |    222,569,929 (33.9x) |     6,574,976 |
| spectral_norm |     74,939,601 (14.1x) |     5,297,561 |
| sum_to_n      |         71,761 (0.28x) |       257,603 |
| tak           |     52,360,528 (1.55x) |    33,730,191 |
