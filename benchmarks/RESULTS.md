# Benchmark Results

Best-known compatible full-profile Dark performance vs audited Rust references, with diagnostic reference runtimes (instruction counts).

**Snapshot timestamp:** 2026-09-19T12:48:03+00:00
**Architecture:** `arm64`
**Profile:** `full` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`
**Workload contract:** `6751ca8977c639512cdd772867f7d2bd0c73833716cddfcb79aa6f15a08f8cfe`
**Compiler commit:** `12510e3f126627f5121f12d167434f6f049c7506` - Make semantic function identities collision-free
**Diagnostic references:** informational only; multipliers are instructions divided by Rust for the same workload.
Every displayed diagnostic row matched the profile's expected stdout.

| Benchmark | Dark (7.45x) | Rust | Darklang interpreter | Node | OCaml | Python |
|---|---:|---:|---:|---:|---:|---:|
| ackermann | 145,107,203 (1.62x) | 89,558,784 | - | - | - | - |
| binary_trees | 11,046,646 (0.17x) | 63,993,594 | - | - | - | - |
| collatz | 70,193,541 (0.91x) | 76,735,737 | - | - | - | - |
| edigits | 49,904,322 (72.5x) | 688,749 | - | - | - | - |
| factorial | 59,005 (0.06x) | 970,290 | - | - | - | - |
| fannkuch | 220,629,862 (125x) | 1,760,885 | - | - | - | - |
| fasta | 21,444,709 (31.2x) | 686,768 | - | - | - | - |
| fft | 19,971,086 (44.0x) | 454,262 | - | - | - | - |
| fib | 8,265,766 (1.37x) | 6,054,417 | - | - | - | - |
| huffman | 411,968,764 (144x) | 2,868,263 | - | - | - | - |
| leibniz | 17,006,465 (1.19x) | 14,258,894 | - | - | - | - |
| mandelbrot | 15,222,005 (1.21x) | 12,557,270 | - | - | - | - |
| matmul | 42,560,364 (65.5x) | 649,416 | - | - | - | - |
| merkletrees | 3,870,994 (1.53x) | 2,523,420 | - | - | - | - |
| myers_diff | 205,029,664 (298x) | 687,142 | - | - | - | - |
| nbody | 14,535,980 (3.42x) | 4,249,498 | - | - | - | - |
| nqueen | 7,417,604 (1.25x) | 5,914,962 | - | - | - | - |
| nsieve | 131,108,232 (345x) | 380,354 | - | - | - | - |
| pisum | 808,014 (0.70x) | 1,160,413 | - | - | - | - |
| primes | 1,672,085 (1.33x) | 1,260,125 | - | - | - | - |
| quicksort | 176,619,573 (29.0x) | 6,095,209 | - | - | - | - |
| raytracer | 47,317,991 (11.6x) | 4,067,427 | - | - | - | - |
| regex_lite | 64,887,050 (158x) | 409,856 | - | - | - | - |
| spectral_norm | 264,941,144 (51.9x) | 5,106,524 | - | - | - | - |
| string_equality | 1,178,270 (0.80x) | 1,479,564 | - | - | - | - |
| sum_to_n | 58,037 (0.22x) | 260,246 | - | - | - | - |
| tak | 48,004,384 (0.12x) | 391,110,808 | - | - | - | - |
| tinytemplate | 371,292,451 (883x) | 420,354 | - | - | - | - |
| warden | 44,213,596 (164x) | 270,345 | - | - | - | - |
