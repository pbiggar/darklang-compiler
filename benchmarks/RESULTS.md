# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-09-04T08:51:00+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `b4e6ba180a5f68f107005cf8867371144b93d3ed5326517cea45284c74fa6cc2`
**Compiler commit:** `d0c8f9473a390f2169178522d407586ab99f21e4` - Optimize no-op string trimming

| Benchmark | Dark (4.16x) | Rust |
|---|---:|---:|
| ackermann | 9,303,384,354 (1.86x) | 5,009,840,894 |
| binary_trees | 635,998,749 (0.35x) | 1,842,793,797 |
| collatz | 70,195,999 (0.91x) | 76,734,720 |
| edigits | 4,478,513,012 (329x) | 13,624,986 |
| factorial | 66,807 (0.26x) | 257,875 |
| fasta | 489,042,185 (22.8x) | 21,446,459 |
| fib | 388,196,363 (1.42x) | 272,528,736 |
| leibniz | 850,008,768 (1.21x) | 700,257,860 |
| mandelbrot | 16,366,939 (1.30x) | 12,554,856 |
| matmul | 1,947,584,623 (115x) | 16,960,641 |
| merkletrees | 386,671,901 (3.41x) | 113,305,941 |
| nbody | 942,009,127 (4.52x) | 208,256,292 |
| nqueen | 212,398,175 (1.29x) | 164,530,821 |
| pisum | 40,029,024 (0.88x) | 45,259,394 |
| primes | 1,900,079 (1.52x) | 1,251,693 |
| quicksort | 218,993,531 (33.5x) | 6,544,669 |
| spectral_norm | 71,812,677 (14.1x) | 5,095,834 |
| sum_to_n | 75,010 (0.29x) | 257,825 |
| tak | 48,022,635 (1.22x) | 39,338,221 |
| tinytemplate | 1,117,947,731 (1808x) | 618,292 |
