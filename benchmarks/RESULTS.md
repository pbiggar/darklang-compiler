# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-09-04T14:46:28+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `b4e6ba180a5f68f107005cf8867371144b93d3ed5326517cea45284c74fa6cc2`
**Compiler commit:** `5d8d755cf8e9875096e5d81916cf725bdd5498ea` - Repair integer E2E typing and crash text

| Benchmark | Dark (4.13x) | Rust |
|---|---:|---:|
| ackermann | 9,303,384,974 (1.86x) | 5,009,840,894 |
| binary_trees | 708,086,821 (0.38x) | 1,842,793,797 |
| collatz | 70,196,426 (0.91x) | 76,734,720 |
| edigits | 4,473,904,210 (328x) | 13,624,986 |
| factorial | 67,531 (0.26x) | 257,875 |
| fasta | 477,042,606 (22.2x) | 21,446,459 |
| fib | 388,196,686 (1.42x) | 272,528,736 |
| leibniz | 850,009,273 (1.21x) | 700,257,860 |
| mandelbrot | 16,367,611 (1.30x) | 12,554,856 |
| matmul | 1,947,404,972 (115x) | 16,960,641 |
| merkletrees | 386,671,947 (3.41x) | 113,305,941 |
| nbody | 942,009,554 (4.52x) | 208,256,292 |
| nqueen | 212,398,498 (1.29x) | 164,530,821 |
| pisum | 40,029,774 (0.88x) | 45,259,394 |
| primes | 1,470,360 (1.17x) | 1,251,693 |
| quicksort | 218,891,730 (33.4x) | 6,544,669 |
| spectral_norm | 71,813,349 (14.1x) | 5,095,834 |
| sum_to_n | 75,760 (0.29x) | 257,825 |
| tak | 48,023,901 (1.22x) | 39,338,221 |
| tinytemplate | 1,118,121,215 (1808x) | 618,292 |
