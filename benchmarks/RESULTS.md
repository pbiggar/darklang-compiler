# Benchmark Results

Best-known compatible full-profile Dark performance vs audited Rust references, with diagnostic reference runtimes (instruction counts).

**Snapshot timestamp:** 2026-09-19T18:59:17+00:00
**Architecture:** `arm64`
**Profile:** `full` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`
**Workload contract:** `6751ca8977c639512cdd772867f7d2bd0c73833716cddfcb79aa6f15a08f8cfe`
**Compiler commit:** `00b50d971232bdf54e37aa60f6ddb84996453bf2` - Merge job #138: own persistent dictionary loop frontiers
**Diagnostic references:** informational only; multipliers are instructions divided by Rust for the same workload.
Every displayed diagnostic row matched the profile's expected stdout.

| Benchmark | Dark (7.41x) | Rust | Darklang interpreter | Node | OCaml | Python |
|---|---:|---:|---:|---:|---:|---:|
| ackermann | 145,107,565 (1.62x) | 89,558,784 | - | - | - | - |
| binary_trees | 11,047,008 (0.17x) | 63,993,594 | - | - | - | - |
| collatz | 70,193,722 (0.91x) | 76,735,737 | - | - | - | - |
| edigits | 49,904,684 (72.5x) | 688,749 | - | - | - | - |
| factorial | 59,367 (0.06x) | 970,290 | - | - | - | - |
| fannkuch | 220,630,043 (125x) | 1,760,885 | - | - | - | - |
| fasta | 21,444,890 (31.2x) | 686,768 | - | - | - | - |
| fft | 20,156,068 (44.4x) | 454,262 | - | - | - | - |
| fib | 8,265,947 (1.37x) | 6,054,417 | - | - | - | - |
| huffman | 411,969,307 (144x) | 2,868,263 | - | - | - | - |
| leibniz | 17,006,646 (1.19x) | 14,258,894 | - | - | - | - |
| mandelbrot | 15,222,367 (1.21x) | 12,557,270 | - | - | - | - |
| matmul | 42,560,545 (65.5x) | 649,416 | - | - | - | - |
| merkletrees | 3,871,356 (1.53x) | 2,523,420 | - | - | - | - |
| myers_diff | 205,030,207 (298x) | 687,142 | - | - | - | - |
| nbody | 14,536,161 (3.42x) | 4,249,498 | - | - | - | - |
| nqueen | 7,417,785 (1.25x) | 5,914,962 | - | - | - | - |
| nsieve | 131,108,594 (345x) | 380,354 | - | - | - | - |
| pisum | 808,376 (0.70x) | 1,160,413 | - | - | - | - |
| primes | 1,672,266 (1.33x) | 1,260,125 | - | - | - | - |
| quicksort | 176,619,935 (29.0x) | 6,095,209 | - | - | - | - |
| raytracer | 47,318,353 (11.6x) | 4,067,427 | - | - | - | - |
| regex_lite | 64,887,412 (158x) | 409,856 | - | - | - | - |
| spectral_norm | 264,941,506 (51.9x) | 5,106,524 | - | - | - | - |
| string_equality | 1,178,632 (0.80x) | 1,479,564 | - | - | - | - |
| sum_to_n | 58,399 (0.22x) | 260,246 | - | - | - | - |
| tak | 48,005,108 (0.12x) | 391,110,808 | - | - | - | - |
| tinytemplate | 310,047,074 (738x) | 420,354 | - | - | - | - |
| warden | 44,213,958 (164x) | 270,345 | - | - | - | - |
