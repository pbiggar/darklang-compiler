# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-09-05T16:11:25+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `b4e6ba180a5f68f107005cf8867371144b93d3ed5326517cea45284c74fa6cc2`
**Compiler commit:** `2a6636b87a6aae82be6d4a1a1666e5a03e177d38` - Preserve Stream raw-slot ownership initialization

| Benchmark | Dark (3.98x) | Rust |
|---|---:|---:|
| ackermann | 9,303,384,874 (1.86x) | 5,009,840,894 |
| binary_trees | 547,528,421 (0.30x) | 1,842,793,797 |
| collatz | 70,196,377 (0.91x) | 76,734,720 |
| edigits | 3,528,831,110 (259x) | 13,624,986 |
| factorial | 67,431 (0.26x) | 257,875 |
| fasta | 477,017,163 (22.2x) | 21,446,459 |
| fib | 388,196,637 (1.42x) | 272,528,736 |
| leibniz | 850,009,224 (1.21x) | 700,257,860 |
| mandelbrot | 16,367,511 (1.30x) | 12,554,856 |
| matmul | 1,945,532,471 (115x) | 16,960,641 |
| merkletrees | 386,671,847 (3.41x) | 113,305,941 |
| nbody | 914,509,449 (4.39x) | 208,256,292 |
| nqueen | 212,398,449 (1.29x) | 164,530,821 |
| pisum | 40,029,674 (0.88x) | 45,259,394 |
| primes | 1,470,311 (1.17x) | 1,251,693 |
| quicksort | 187,792,880 (28.7x) | 6,544,669 |
| spectral_norm | 69,747,761 (13.7x) | 5,095,834 |
| sum_to_n | 75,660 (0.29x) | 257,825 |
| tak | 48,023,681 (1.22x) | 39,338,221 |
| tinytemplate | 1,115,881,107 (1805x) | 618,292 |
