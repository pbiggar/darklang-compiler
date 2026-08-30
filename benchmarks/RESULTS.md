# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-08-30T19:51:03+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `75fcaaf3d950644efb3baeabe8edb2344aee740ae3658719f4c217d5b618ae7d`
**Compiler commit:** `4ad9e194a0b29c5443599e3842f9573d05f56760` - Support targeted quick benchmark decisions

| Benchmark     |          Dark (4.23x) |          Rust |
|---------------|-----------------------|---------------|
| ackermann     | 9,303,371,891 (1.86x) | 5,009,840,894 |
| binary_trees  |   635,985,955 (0.35x) | 1,842,793,797 |
| collatz       |    70,189,152 (0.91x) |    76,734,720 |
| edigits       |  4,478,500,048 (329x) |    13,624,986 |
| factorial     |        53,662 (0.21x) |       257,875 |
| fasta         |   489,035,380 (22.8x) |    21,446,459 |
| fib           |   388,190,195 (1.42x) |   272,528,736 |
| leibniz       |   850,001,411 (1.21x) |   700,257,860 |
| mandelbrot    |    16,354,136 (1.30x) |    12,554,856 |
| matmul        |  1,947,578,291 (115x) |    16,960,641 |
| merkletrees   |   386,659,268 (3.41x) |   113,305,941 |
| nbody         |   942,002,278 (4.52x) |   208,256,292 |
| nqueen        |   212,392,007 (1.29x) |   164,530,821 |
| pisum         |    40,015,711 (0.88x) |    45,259,394 |
| primes        |     1,893,401 (1.51x) |     1,251,693 |
| quicksort     |   218,980,549 (33.5x) |     6,544,669 |
| spectral_norm |    71,806,745 (14.1x) |     5,095,834 |
| sum_to_n      |        61,695 (0.24x) |       257,825 |
| tak           |    47,997,201 (1.22x) |    39,338,221 |
| tinytemplate  | 2,426,463,044 (3924x) |       618,292 |
