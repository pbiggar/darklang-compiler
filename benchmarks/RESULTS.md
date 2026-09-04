# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-09-04T08:17:14+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `b4e6ba180a5f68f107005cf8867371144b93d3ed5326517cea45284c74fa6cc2`
**Compiler commit:** `330ed87145a8547da711a18ad814cea985a941cd` - Fast-path Int64 CLI arguments

| Benchmark | Dark (4.28x) | Rust |
|---|---:|---:|
| ackermann | 9,303,378,209 (1.86x) | 5,009,840,894 |
| binary_trees | 635,992,363 (0.35x) | 1,842,793,797 |
| collatz | 70,192,376 (0.91x) | 76,734,720 |
| edigits | 4,478,506,504 (329x) | 13,624,986 |
| factorial | 60,174 (0.23x) | 257,875 |
| fasta | 489,038,565 (22.8x) | 21,446,459 |
| fib | 388,193,228 (1.42x) | 272,528,736 |
| leibniz | 850,004,779 (1.21x) | 700,257,860 |
| mandelbrot | 16,360,550 (1.30x) | 12,554,856 |
| matmul | 1,947,581,369 (115x) | 16,960,641 |
| merkletrees | 386,665,634 (3.41x) | 113,305,941 |
| nbody | 942,005,504 (4.52x) | 208,256,292 |
| nqueen | 212,395,040 (1.29x) | 164,530,821 |
| pisum | 40,022,269 (0.88x) | 45,259,394 |
| primes | 1,896,578 (1.52x) | 1,251,693 |
| quicksort | 218,987,023 (33.5x) | 6,544,669 |
| spectral_norm | 71,806,288 (14.1x) | 5,095,834 |
| sum_to_n | 68,255 (0.26x) | 257,825 |
| tak | 48,010,226 (1.22x) | 39,338,221 |
| tinytemplate | 2,426,443,047 (3924x) | 618,292 |
