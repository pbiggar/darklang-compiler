# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-08-26T13:07:20+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 1)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `1b03683abfad94c4640c3e1ea843ca087c47281c1104818fb22c1df5bff63ee3`
**Compiler commit:** `166ae8a7bc17230b8fdaa6a16f8de3705539f9fe` - Repair multiplication recursion MIR snapshots

| Benchmark     |          Dark (2.91x) |          Rust |
|---------------|-----------------------|---------------|
| ackermann     | 9,303,371,891 (2.00x) | 4,651,994,510 |
| binary_trees  |   635,985,955 (0.35x) | 1,842,791,955 |
| collatz       |    70,189,152 (0.71x) |    98,242,178 |
| edigits       |  4,478,500,048 (328x) |    13,637,551 |
| factorial     |        53,662 (0.21x) |       257,669 |
| fasta         |   489,035,380 (24.1x) |    20,252,213 |
| fib           |   388,190,195 (1.51x) |   257,598,132 |
| leibniz       |   850,001,411 (1.06x) |   800,257,637 |
| mandelbrot    |    16,354,136 (1.20x) |    13,595,721 |
| matmul        |  1,947,578,291 (122x) |    15,983,852 |
| merkletrees   |   386,659,268 (3.10x) |   124,776,610 |
| nbody         | 1,074,002,348 (4.33x) |   247,760,534 |
| nqueen        |   212,392,007 (1.52x) |   139,988,273 |
| pisum         |    40,015,711 (0.80x) |    50,258,602 |
| primes        |     1,893,401 (1.39x) |     1,358,980 |
| quicksort     |   218,980,549 (33.3x) |     6,574,976 |
| spectral_norm |    71,806,745 (13.6x) |     5,297,561 |
| sum_to_n      |        61,695 (0.24x) |       257,603 |
| tak           |    47,997,201 (1.42x) |    33,730,191 |
