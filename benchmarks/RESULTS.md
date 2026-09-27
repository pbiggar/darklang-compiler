# Benchmark Results

Best-known compatible full-profile Dark performance vs audited Rust references, with diagnostic reference runtimes (instruction counts).

**Snapshot timestamp:** 2026-09-27T18:38:27+00:00
**Architecture:** `arm64`
**Profile:** `full` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`
**Workload contract:** `6751ca8977c639512cdd772867f7d2bd0c73833716cddfcb79aa6f15a08f8cfe`
**Compiler commit:** `40724da0e67655c9559d764852a237595bd42f5a` - Optimize Int64 list indexing before SSA ownership
**Diagnostic references:** informational only; multipliers are instructions divided by Rust for the same workload.
Every displayed diagnostic row matched the profile's expected stdout.

| Benchmark | Dark (6.49x) | Rust | Darklang interpreter | Node | OCaml | Python |
|---|---:|---:|---:|---:|---:|---:|
| ackermann | 122,786,823 (1.37x) | 89,558,784 | - | - | - | - |
| binary_trees | 10,522,516 (0.16x) | 63,993,594 | - | - | - | - |
| collatz | 70,193,414 (0.91x) | 76,735,737 | - | - | - | - |
| edigits | 48,977,755 (71.1x) | 688,749 | - | - | - | - |
| factorial | 58,966 (0.06x) | 970,290 | - | - | - | - |
| fannkuch | 221,172,600 (126x) | 1,760,885 | - | - | - | - |
| fasta | 11,494,373 (16.7x) | 686,768 | - | - | - | - |
| fft | 1,800,265 (3.96x) | 454,262 | - | - | - | - |
| fib | 7,630,193 (1.26x) | 6,054,417 | - | - | - | - |
| huffman | 403,079,661 (141x) | 2,868,263 | - | - | - | - |
| leibniz | 17,006,295 (1.19x) | 14,258,894 | - | - | - | - |
| mandelbrot | 15,142,866 (1.21x) | 12,557,270 | - | - | - | - |
| matmul | 41,611,234 (64.1x) | 649,416 | - | - | - | - |
| merkletrees | 3,871,136 (1.53x) | 2,523,420 | - | - | - | - |
| myers_diff | 204,545,196 (298x) | 687,142 | - | - | - | - |
| nbody | 14,595,833 (3.43x) | 4,249,498 | - | - | - | - |
| nqueen | 6,924,919 (1.17x) | 5,914,962 | - | - | - | - |
| nsieve | 130,435,271 (343x) | 380,354 | - | - | - | - |
| pisum | 808,068 (0.70x) | 1,160,413 | - | - | - | - |
| primes | 1,612,021 (1.28x) | 1,260,125 | - | - | - | - |
| quicksort | 172,486,153 (28.3x) | 6,095,209 | - | - | - | - |
| raytracer | 44,284,039 (10.9x) | 4,067,427 | - | - | - | - |
| regex_lite | 64,744,887 (158x) | 409,856 | - | - | - | - |
| spectral_norm | 215,930,782 (42.3x) | 5,106,524 | - | - | - | - |
| string_equality | 1,178,012 (0.80x) | 1,479,564 | - | - | - | - |
| sum_to_n | 57,968 (0.22x) | 260,246 | - | - | - | - |
| tak | 48,004,620 (0.12x) | 391,110,808 | - | - | - | - |
| tinytemplate | 305,729,478 (727x) | 420,354 | - | - | - | - |
| warden | 44,111,493 (163x) | 270,345 | - | - | - | - |
