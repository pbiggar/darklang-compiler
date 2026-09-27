# Benchmark Results

Best-known compatible full-profile Dark performance vs audited Rust references, with diagnostic reference runtimes (instruction counts).

**Snapshot timestamp:** 2026-09-27T14:22:35+00:00
**Architecture:** `arm64`
**Profile:** `full` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`
**Workload contract:** `6751ca8977c639512cdd772867f7d2bd0c73833716cddfcb79aa6f15a08f8cfe`
**Compiler commit:** `3ea97900597e7f6946a75042d90af7813805b5d1` - Summarize ARM64 callee register clobbers for direct calls
**Diagnostic references:** informational only; multipliers are instructions divided by Rust for the same workload.
Every displayed diagnostic row matched the profile's expected stdout.

| Benchmark | Dark (6.61x) | Rust | Darklang interpreter | Node | OCaml | Python |
|---|---:|---:|---:|---:|---:|---:|
| ackermann | 122,786,833 (1.37x) | 89,558,784 | - | - | - | - |
| binary_trees | 10,522,508 (0.16x) | 63,993,594 | - | - | - | - |
| collatz | 70,193,330 (0.91x) | 76,735,737 | - | - | - | - |
| edigits | 49,067,653 (71.2x) | 688,749 | - | - | - | - |
| factorial | 58,886 (0.06x) | 970,290 | - | - | - | - |
| fannkuch | 220,304,061 (125x) | 1,760,885 | - | - | - | - |
| fasta | 16,275,164 (23.7x) | 686,768 | - | - | - | - |
| fft | 1,762,569 (3.88x) | 454,262 | - | - | - | - |
| fib | 7,630,181 (1.26x) | 6,054,417 | - | - | - | - |
| huffman | 404,438,467 (141x) | 2,868,263 | - | - | - | - |
| leibniz | 17,006,193 (1.19x) | 14,258,894 | - | - | - | - |
| mandelbrot | 15,142,822 (1.21x) | 12,557,270 | - | - | - | - |
| matmul | 41,669,542 (64.2x) | 649,416 | - | - | - | - |
| merkletrees | 3,871,128 (1.53x) | 2,523,420 | - | - | - | - |
| myers_diff | 204,609,021 (298x) | 687,142 | - | - | - | - |
| nbody | 14,595,767 (3.43x) | 4,249,498 | - | - | - | - |
| nqueen | 6,924,907 (1.17x) | 5,914,962 | - | - | - | - |
| nsieve | 130,435,227 (343x) | 380,354 | - | - | - | - |
| pisum | 807,990 (0.70x) | 1,160,413 | - | - | - | - |
| primes | 1,611,955 (1.28x) | 1,260,125 | - | - | - | - |
| quicksort | 173,108,333 (28.4x) | 6,095,209 | - | - | - | - |
| raytracer | 44,752,766 (11.0x) | 4,067,427 | - | - | - | - |
| regex_lite | 64,753,188 (158x) | 409,856 | - | - | - | - |
| spectral_norm | 263,133,151 (51.5x) | 5,106,524 | - | - | - | - |
| string_equality | 1,177,881 (0.80x) | 1,479,564 | - | - | - | - |
| sum_to_n | 57,870 (0.22x) | 260,246 | - | - | - | - |
| tak | 48,004,591 (0.12x) | 391,110,808 | - | - | - | - |
| tinytemplate | 306,212,700 (728x) | 420,354 | - | - | - | - |
| warden | 44,145,346 (163x) | 270,345 | - | - | - | - |
