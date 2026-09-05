# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-09-05T01:19:05+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `b4e6ba180a5f68f107005cf8867371144b93d3ed5326517cea45284c74fa6cc2`
**Compiler commit:** `0d7d60b76fda340de4055ed3ebbb1c95e0e1327d` - Transfer owned results into returned aggregates

| Benchmark | Dark (4.07x) | Rust |
|---|---:|---:|
| ackermann | 9,303,384,906 (1.86x) | 5,009,840,894 |
| binary_trees | 547,528,453 (0.30x) | 1,842,793,797 |
| collatz | 70,196,392 (0.91x) | 76,734,720 |
| edigits | 4,473,904,142 (328x) | 13,624,986 |
| factorial | 67,463 (0.26x) | 257,875 |
| fasta | 477,042,572 (22.2x) | 21,446,459 |
| fib | 388,196,652 (1.42x) | 272,528,736 |
| leibniz | 850,009,239 (1.21x) | 700,257,860 |
| mandelbrot | 16,367,543 (1.30x) | 12,554,856 |
| matmul | 1,947,404,938 (115x) | 16,960,641 |
| merkletrees | 386,671,879 (3.41x) | 113,305,941 |
| nbody | 942,009,466 (4.52x) | 208,256,292 |
| nqueen | 212,398,464 (1.29x) | 164,530,821 |
| pisum | 40,029,706 (0.88x) | 45,259,394 |
| primes | 1,470,326 (1.17x) | 1,251,693 |
| quicksort | 218,891,662 (33.4x) | 6,544,669 |
| spectral_norm | 71,813,281 (14.1x) | 5,095,834 |
| sum_to_n | 75,692 (0.29x) | 257,825 |
| tak | 48,023,765 (1.22x) | 39,338,221 |
| tinytemplate | 1,117,713,233 (1808x) | 618,292 |
