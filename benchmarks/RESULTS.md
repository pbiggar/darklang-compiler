# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-08-23T03:09:12+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 1)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `3662376c639f80762e5f9672675eed03667cedd6b77b005bf3f624a7708f03db`
**Compiler commit:** `79a1101e1c422824de80032e0977cb9b70d1bfce` - Check pisum review changes

| Benchmark     |           Dark (2.27x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 10,019,036,010 (2.15x) | 4,651,994,510 |
| binary_trees  |    649,092,988 (0.35x) | 1,842,791,955 |
| collatz       |     81,143,030 (0.83x) |    98,242,178 |
| edigits       |   4,528,733,705 (332x) |    13,637,551 |
| factorial     |         63,968 (0.25x) |       257,669 |
| fasta         |    514,839,063 (25.4x) |    20,252,213 |
| fib           |    418,050,941 (1.62x) |   257,598,132 |
| leibniz       |    850,001,456 (1.06x) |   800,257,637 |
| mandelbrot    |     17,459,524 (1.28x) |    13,595,721 |
| matmul        |   1,968,521,077 (123x) |    15,983,852 |
| merkletrees   |    389,936,167 (3.13x) |   124,776,610 |
| nbody         |  1,394,002,414 (5.63x) |   247,760,534 |
| nqueen        |    221,668,099 (1.58x) |   139,988,273 |
| pisum         |         95,305 (0.00x) |    50,258,602 |
| primes        |      2,045,943 (1.51x) |     1,358,980 |
| quicksort     |    221,163,784 (33.6x) |     6,574,976 |
| spectral_norm |     74,939,665 (14.1x) |     5,297,561 |
| sum_to_n      |         71,817 (0.28x) |       257,603 |
| tak           |     52,360,584 (1.55x) |    33,730,191 |
