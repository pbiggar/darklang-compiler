# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-09-04T01:12:46+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `b4e6ba180a5f68f107005cf8867371144b93d3ed5326517cea45284c74fa6cc2`
**Compiler commit:** `98dc8f6cfab6dfce694dc3e431560d437b8df723` - Fix benchmark argv execution on ARM64

| Benchmark | Dark (5.66x) | Rust |
|---|---:|---:|
| ackermann | 9,303,604,523 (1.86x) | 5,009,840,894 |
| binary_trees | 636,478,986 (0.35x) | 1,842,793,797 |
| collatz | 70,932,797 (0.92x) | 76,734,720 |
| edigits | 4,479,073,038 (329x) | 13,624,986 |
| factorial | 803,570 (3.12x) | 257,875 |
| fasta | 489,753,272 (22.8x) | 21,446,459 |
| fib | 388,363,493 (1.43x) | 272,528,736 |
| leibniz | 851,223,658 (1.22x) | 700,257,860 |
| mandelbrot | 16,847,176 (1.34x) | 12,554,856 |
| matmul | 1,947,897,742 (115x) | 16,960,641 |
| merkletrees | 387,006,062 (3.42x) | 113,305,941 |
| nbody | 942,745,925 (4.53x) | 208,256,292 |
| nqueen | 212,565,305 (1.29x) | 164,530,821 |
| pisum | 40,911,845 (0.90x) | 45,259,394 |
| primes | 2,469,835 (1.97x) | 1,251,693 |
| quicksort | 219,525,207 (33.5x) | 6,544,669 |
| spectral_norm | 72,292,779 (14.2x) | 5,095,834 |
| sum_to_n | 957,831 (3.72x) | 257,825 |
| tak | 48,576,857 (1.23x) | 39,338,221 |
| tinytemplate | 2,426,669,259 (3925x) | 618,292 |
