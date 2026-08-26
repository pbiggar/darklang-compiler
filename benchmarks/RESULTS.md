# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-08-26T03:33:20+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 1)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `d921dfef06da8fee41cb4b99ab1453aa37ec434352cc4b5ad50e69501f1667ed`
**Compiler commit:** `079a53371ea3102d0500ea7dbf2a86e79b8c9971` - dcb2: record trial result for Retire the obsolete leibniz loop-copy hypothesis

| Benchmark     |          Dark (3.37x) |          Rust |
|---------------|-----------------------|---------------|
| ackermann     | 9,303,371,891 (2.00x) | 4,651,997,427 |
| binary_trees  |   635,985,955 (0.35x) | 1,842,798,016 |
| collatz       |    70,189,152 (0.71x) |    98,245,076 |
| edigits       |  4,478,500,048 (328x) |    13,640,449 |
| factorial     |        53,892 (0.21x) |       260,577 |
| fasta         |   489,035,380 (24.1x) |    20,255,121 |
| fib           |   388,190,195 (1.51x) |   257,601,072 |
| leibniz       |   850,001,411 (1.06x) |   800,260,530 |
| mandelbrot    |    16,354,136 (1.20x) |    13,598,660 |
| matmul        |  1,947,578,291 (122x) |    15,986,760 |
| merkletrees   |   386,659,268 (3.10x) |   124,779,530 |
| nbody         | 1,074,002,348 (4.33x) |   247,763,450 |
| nqueen        |   212,392,007 (1.52x) |   139,991,186 |
| pisum         |    40,015,711 (0.80x) |    50,261,515 |
| primes        |     1,893,401 (1.39x) |     1,361,899 |
| quicksort     |   218,980,549 (33.3x) |     6,577,898 |
| spectral_norm |    71,806,745 (13.5x) |     5,300,493 |
| sum_to_n      |        61,695 (0.24x) |       260,493 |
| tak           |    47,997,201 (1.42x) |    33,733,128 |
| tinytemplate  | 1,454,922,616 (55.0x) |    26,437,523 |
