# Benchmark Results

Best-known compatible full-profile Dark performance vs audited Rust references, with diagnostic reference runtimes (instruction counts).

**Snapshot timestamp:** 2026-09-29T10:09:56+00:00
**Architecture:** `arm64`
**Profile:** `full` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`
**Workload contract:** `2ead0323fc6859c952a3b8a83c56e0b0da61e1350b7ddd39ecc4a0b1da25e861`
**Compiler commit:** `4c46c96eacff55406a5761060be8ddfafea01b9e` - Document compile-time optimization investigation principles
**Diagnostic references:** informational only; multipliers are instructions divided by Rust for the same workload.
Every displayed diagnostic row matched the profile's expected stdout.

| Benchmark | Dark (6.38x) | Rust | Darklang interpreter | Node | OCaml | Python |
|---|---:|---:|---:|---:|---:|---:|
| ackermann | 122,786,825 (1.37x) | 89,558,784 | - | - | - | - |
| binary_trees | 10,522,518 (0.16x) | 63,993,594 | - | - | - | - |
| collatz | 70,193,415 (0.91x) | 76,735,737 | - | - | - | - |
| edigits | 48,970,108 (71.1x) | 688,749 | - | - | - | - |
| factorial | 58,968 (0.06x) | 970,290 | - | - | - | - |
| fannkuch | 221,209,585 (126x) | 1,760,885 | - | - | - | - |
| fasta | 10,284,685 (15.0x) | 686,768 | - | - | - | - |
| fft | 1,784,354 (3.93x) | 454,262 | - | - | - | - |
| fib | 7,630,194 (1.26x) | 6,054,417 | - | - | - | - |
| huffman | 400,353,974 (140x) | 2,868,263 | - | - | - | - |
| leibniz | 17,006,289 (1.19x) | 14,258,894 | - | - | - | - |
| mandelbrot | 15,142,868 (1.21x) | 12,557,270 | - | - | - | - |
| matmul | 41,471,414 (63.9x) | 649,416 | - | - | - | - |
| merkletrees | 3,871,138 (1.53x) | 2,523,420 | - | - | - | - |
| myers_diff | 204,588,087 (298x) | 687,142 | - | - | - | - |
| nbody | 10,805,793 (2.54x) | 4,249,498 | - | - | - | - |
| nqueen | 6,924,920 (1.17x) | 5,914,962 | - | - | - | - |
| nsieve | 130,174,724 (342x) | 380,354 | - | - | - | - |
| pisum | 808,063 (0.70x) | 1,160,413 | - | - | - | - |
| primes | 1,567,031 (1.24x) | 1,260,125 | - | - | - | - |
| quicksort | 172,560,188 (28.3x) | 6,095,209 | - | - | - | - |
| raytracer | 43,229,793 (10.6x) | 4,067,427 | - | - | - | - |
| regex_lite | 64,751,641 (158x) | 409,856 | - | - | - | - |
| spectral_norm | 215,931,384 (42.3x) | 5,106,524 | - | - | - | - |
| string_equality | 1,174,023 (0.79x) | 1,479,564 | - | - | - | - |
| sum_to_n | 57,970 (0.22x) | 260,246 | - | - | - | - |
| tak | 48,004,622 (0.12x) | 391,110,808 | - | - | - | - |
| tinytemplate | 305,936,131 (728x) | 420,354 | - | - | - | - |
| warden | 44,133,565 (163x) | 270,345 | - | - | - | - |
