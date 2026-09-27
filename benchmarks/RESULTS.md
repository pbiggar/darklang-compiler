# Benchmark Results

Best-known compatible full-profile Dark performance vs audited Rust references, with diagnostic reference runtimes (instruction counts).

**Snapshot timestamp:** 2026-09-27T13:42:44+00:00
**Architecture:** `arm64`
**Profile:** `full` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`
**Workload contract:** `6751ca8977c639512cdd772867f7d2bd0c73833716cddfcb79aa6f15a08f8cfe`
**Compiler commit:** `b407d006473f4b63b65beae22ccecaa7a8e37a52` - Preserve ARM64 Float callee registers and compact call saves
**Diagnostic references:** informational only; multipliers are instructions divided by Rust for the same workload.
Every displayed diagnostic row matched the profile's expected stdout.

| Benchmark | Dark (6.63x) | Rust | Darklang interpreter | Node | OCaml | Python |
|---|---:|---:|---:|---:|---:|---:|
| ackermann | 122,786,845 (1.37x) | 89,558,784 | - | - | - | - |
| binary_trees | 10,522,533 (0.16x) | 63,993,594 | - | - | - | - |
| collatz | 70,193,407 (0.91x) | 76,735,737 | - | - | - | - |
| edigits | 49,068,939 (71.2x) | 688,749 | - | - | - | - |
| factorial | 58,967 (0.06x) | 970,290 | - | - | - | - |
| fannkuch | 220,304,099 (125x) | 1,760,885 | - | - | - | - |
| fasta | 17,171,193 (25.0x) | 686,768 | - | - | - | - |
| fft | 1,764,434 (3.88x) | 454,262 | - | - | - | - |
| fib | 7,630,200 (1.26x) | 6,054,417 | - | - | - | - |
| huffman | 404,719,446 (141x) | 2,868,263 | - | - | - | - |
| leibniz | 17,006,277 (1.19x) | 14,258,894 | - | - | - | - |
| mandelbrot | 15,141,681 (1.21x) | 12,557,270 | - | - | - | - |
| matmul | 41,669,561 (64.2x) | 649,416 | - | - | - | - |
| merkletrees | 3,871,153 (1.53x) | 2,523,420 | - | - | - | - |
| myers_diff | 204,613,245 (298x) | 687,142 | - | - | - | - |
| nbody | 14,595,827 (3.43x) | 4,249,498 | - | - | - | - |
| nqueen | 6,924,926 (1.17x) | 5,914,962 | - | - | - | - |
| nsieve | 130,435,278 (343x) | 380,354 | - | - | - | - |
| pisum | 808,021 (0.70x) | 1,160,413 | - | - | - | - |
| primes | 1,612,013 (1.28x) | 1,260,125 | - | - | - | - |
| quicksort | 173,108,397 (28.4x) | 6,095,209 | - | - | - | - |
| raytracer | 45,005,944 (11.1x) | 4,067,427 | - | - | - | - |
| regex_lite | 64,754,916 (158x) | 409,856 | - | - | - | - |
| spectral_norm | 263,137,690 (51.5x) | 5,106,524 | - | - | - | - |
| string_equality | 1,177,997 (0.80x) | 1,479,564 | - | - | - | - |
| sum_to_n | 57,964 (0.22x) | 260,246 | - | - | - | - |
| tak | 48,004,666 (0.12x) | 391,110,808 | - | - | - | - |
| tinytemplate | 306,444,918 (729x) | 420,354 | - | - | - | - |
| warden | 44,147,260 (163x) | 270,345 | - | - | - | - |
