# Benchmark Results

Best-known compatible full-profile Dark performance vs audited Rust references, with diagnostic reference runtimes (instruction counts).

**Snapshot timestamp:** 2026-09-18T20:26:22+00:00
**Architecture:** `arm64`
**Profile:** `full` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`
**Workload contract:** `6751ca8977c639512cdd772867f7d2bd0c73833716cddfcb79aa6f15a08f8cfe`
**Compiler commit:** `1ea60810f10e81e8a282a20600549962497ba1a6` - Merge commit '86a26c383b243d8306bdbe8de5c62c5b38fc45eb' into HEAD
**Diagnostic references:** informational only; multipliers are instructions divided by Rust for the same workload.
Every displayed diagnostic row matched the profile's expected stdout.

| Benchmark | Dark (11.7x) | Rust | Darklang interpreter | Node | OCaml | Python |
|---|---:|---:|---:|---:|---:|---:|
| ackermann | 145,114,139 (1.62x) | 89,558,784 | - | - | - | - |
| binary_trees | 11,053,080 (0.17x) | 63,993,594 | - | - | - | - |
| collatz | 70,194,400 (0.91x) | 76,735,737 | - | - | - | - |
| edigits | 51,754,909 (75.1x) | 688,749 | - | - | - | - |
| factorial | 63,468 (0.07x) | 970,290 | - | - | - | - |
| fannkuch | 233,405,899 (133x) | 1,760,885 | - | - | - | - |
| fasta | 154,617,366 (225x) | 686,768 | - | - | - | - |
| fft | 407,133,425 (896x) | 454,262 | - | - | - | - |
| fib | 8,268,587 (1.37x) | 6,054,417 | - | - | - | - |
| huffman | 518,408,776 (181x) | 2,868,263 | - | - | - | - |
| leibniz | 17,006,832 (1.19x) | 14,258,894 | - | - | - | - |
| mandelbrot | 15,227,450 (1.21x) | 12,557,270 | - | - | - | - |
| matmul | 42,560,591 (65.5x) | 649,416 | - | - | - | - |
| merkletrees | 3,877,428 (1.54x) | 2,523,420 | - | - | - | - |
| myers_diff | 4,637,007,013 (6748x) | 687,142 | - | - | - | - |
| nbody | 14,537,331 (3.42x) | 4,249,498 | - | - | - | - |
| nqueen | 7,420,425 (1.25x) | 5,914,962 | - | - | - | - |
| nsieve | 131,101,395 (345x) | 380,354 | - | - | - | - |
| pisum | 812,478 (0.70x) | 1,160,413 | - | - | - | - |
| primes | 1,678,434 (1.33x) | 1,260,125 | - | - | - | - |
| quicksort | 188,808,249 (31.0x) | 6,095,209 | - | - | - | - |
| raytracer | 51,734,416 (12.7x) | 4,067,427 | - | - | - | - |
| regex_lite | 77,891,592 (190x) | 409,856 | - | - | - | - |
| spectral_norm | 931,017,197 (182x) | 5,106,524 | - | - | - | - |
| string_equality | 1,219,845 (0.82x) | 1,479,564 | - | - | - | - |
| sum_to_n | 62,009 (0.24x) | 260,246 | - | - | - | - |
| tak | 48,017,024 (0.12x) | 391,110,808 | - | - | - | - |
| tinytemplate | 5,864,163,741 (13951x) | 420,354 | - | - | - | - |
| warden | 47,041,843 (174x) | 270,345 | - | - | - | - |
