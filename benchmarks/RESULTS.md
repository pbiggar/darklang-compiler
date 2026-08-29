# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-08-29T12:41:44+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 1)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `566d904c072c803793f88226a36f96e1d5564580f78806a07e515bcb35c2a934`
**Compiler commit:** `9ba6dec7e17cc8d33a584a6f28c4c77ba7e8688d` - Add better benchmarks review changes

| Benchmark     |            Dark (4.27x) |          Rust |
|---------------|-------------------------|---------------|
| ackermann     |   9,303,371,891 (2.00x) | 4,651,997,427 |
| binary_trees  |     635,985,955 (0.35x) | 1,842,798,016 |
| collatz       |      70,189,152 (0.71x) |    98,245,076 |
| edigits       |    4,478,500,048 (328x) |    13,640,449 |
| factorial     |          53,662 (0.21x) |       260,577 |
| fasta         |     489,035,380 (24.1x) |    20,255,121 |
| fib           |     388,190,195 (1.51x) |   257,601,072 |
| leibniz       |     850,001,411 (1.06x) |   800,260,530 |
| mandelbrot    |      16,354,136 (1.20x) |    13,598,660 |
| matmul        |    1,947,578,291 (122x) |    15,986,760 |
| merkletrees   |     386,659,268 (3.10x) |   124,779,530 |
| nbody         |   1,003,502,308 (4.05x) |   247,763,450 |
| nqueen        |     212,392,007 (1.52x) |   139,991,186 |
| pisum         |      40,015,711 (0.80x) |    50,261,515 |
| primes        |       1,893,401 (1.39x) |     1,361,899 |
| quicksort     |     218,980,549 (33.3x) |     6,577,898 |
| spectral_norm |      71,806,745 (13.5x) |     5,300,493 |
| sum_to_n      |          61,695 (0.24x) |       260,493 |
| tak           |      47,997,201 (1.42x) |    33,733,128 |
| tinytemplate  | 179,543,416,420 (6791x) |    26,437,523 |
