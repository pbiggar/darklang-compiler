# Benchmark Results

Best-known compatible full-profile Dark performance vs audited Rust references, with diagnostic reference runtimes (instruction counts).

**Snapshot timestamp:** 2026-09-19T07:01:45+00:00
**Architecture:** `arm64`
**Profile:** `full` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`
**Workload contract:** `6751ca8977c639512cdd772867f7d2bd0c73833716cddfcb79aa6f15a08f8cfe`
**Compiler commit:** `dbfea5c84ec6ac741124079df74e24789ba3e110` - Expand backend instruction combining
**Diagnostic references:** informational only; multipliers are instructions divided by Rust for the same workload.
Every displayed diagnostic row matched the profile's expected stdout.

| Benchmark | Dark (11.6x) | Rust | Darklang interpreter | Node | OCaml | Python |
|---|---:|---:|---:|---:|---:|---:|
| ackermann | 145,114,130 (1.62x) | 89,558,784 | - | - | - | - |
| binary_trees | 11,053,071 (0.17x) | 63,993,594 | - | - | - | - |
| collatz | 70,194,391 (0.91x) | 76,735,737 | - | - | - | - |
| edigits | 51,730,200 (75.1x) | 688,749 | - | - | - | - |
| factorial | 63,459 (0.07x) | 970,290 | - | - | - | - |
| fannkuch | 233,405,890 (133x) | 1,760,885 | - | - | - | - |
| fasta | 154,617,357 (225x) | 686,768 | - | - | - | - |
| fft | 407,131,142 (896x) | 454,262 | - | - | - | - |
| fib | 8,268,578 (1.37x) | 6,054,417 | - | - | - | - |
| huffman | 518,434,214 (181x) | 2,868,263 | - | - | - | - |
| leibniz | 17,006,822 (1.19x) | 14,258,894 | - | - | - | - |
| mandelbrot | 15,227,441 (1.21x) | 12,557,270 | - | - | - | - |
| matmul | 42,560,582 (65.5x) | 649,416 | - | - | - | - |
| merkletrees | 3,877,419 (1.54x) | 2,523,420 | - | - | - | - |
| myers_diff | 4,637,007,004 (6748x) | 687,142 | - | - | - | - |
| nbody | 14,537,321 (3.42x) | 4,249,498 | - | - | - | - |
| nqueen | 7,420,416 (1.25x) | 5,914,962 | - | - | - | - |
| nsieve | 131,101,386 (345x) | 380,354 | - | - | - | - |
| pisum | 812,468 (0.70x) | 1,160,413 | - | - | - | - |
| primes | 1,673,426 (1.33x) | 1,260,125 | - | - | - | - |
| quicksort | 188,808,240 (31.0x) | 6,095,209 | - | - | - | - |
| raytracer | 47,323,909 (11.6x) | 4,067,427 | - | - | - | - |
| regex_lite | 77,891,415 (190x) | 409,856 | - | - | - | - |
| spectral_norm | 916,603,981 (179x) | 5,106,524 | - | - | - | - |
| string_equality | 1,219,836 (0.82x) | 1,479,564 | - | - | - | - |
| sum_to_n | 62,000 (0.24x) | 260,246 | - | - | - | - |
| tak | 48,017,015 (0.12x) | 391,110,808 | - | - | - | - |
| tinytemplate | 5,864,163,657 (13951x) | 420,354 | - | - | - | - |
| warden | 47,043,550 (174x) | 270,345 | - | - | - | - |
