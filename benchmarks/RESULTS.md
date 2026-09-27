# Benchmark Results

Best-known compatible full-profile Dark performance vs audited Rust references, with diagnostic reference runtimes (instruction counts).

**Snapshot timestamp:** 2026-09-27T04:10:03+00:00
**Architecture:** `arm64`
**Profile:** `full` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`
**Workload contract:** `6751ca8977c639512cdd772867f7d2bd0c73833716cddfcb79aa6f15a08f8cfe`
**Compiler commit:** `8cb9ff32600b1d0510cb01bba36df721926a0878` - Unbox eligible Options and transparent aggregate wrappers
**Diagnostic references:** informational only; multipliers are instructions divided by Rust for the same workload.
Every displayed diagnostic row matched the profile's expected stdout.

| Benchmark | Dark (6.77x) | Rust | Darklang interpreter | Node | OCaml | Python |
|---|---:|---:|---:|---:|---:|---:|
| ackermann | 145,107,511 (1.62x) | 89,558,784 | - | - | - | - |
| binary_trees | 11,046,954 (0.17x) | 63,993,594 | - | - | - | - |
| collatz | 70,193,695 (0.91x) | 76,735,737 | - | - | - | - |
| edigits | 49,904,630 (72.5x) | 688,749 | - | - | - | - |
| factorial | 59,313 (0.06x) | 970,290 | - | - | - | - |
| fannkuch | 220,630,016 (125x) | 1,760,885 | - | - | - | - |
| fasta | 18,703,388 (27.2x) | 686,768 | - | - | - | - |
| fft | 1,802,792 (3.97x) | 454,262 | - | - | - | - |
| fib | 8,265,920 (1.37x) | 6,054,417 | - | - | - | - |
| huffman | 410,744,587 (143x) | 2,868,263 | - | - | - | - |
| leibniz | 17,006,619 (1.19x) | 14,258,894 | - | - | - | - |
| mandelbrot | 15,222,313 (1.21x) | 12,557,270 | - | - | - | - |
| matmul | 42,560,518 (65.5x) | 649,416 | - | - | - | - |
| merkletrees | 3,871,302 (1.53x) | 2,523,420 | - | - | - | - |
| myers_diff | 205,029,599 (298x) | 687,142 | - | - | - | - |
| nbody | 14,536,070 (3.42x) | 4,249,498 | - | - | - | - |
| nqueen | 7,417,758 (1.25x) | 5,914,962 | - | - | - | - |
| nsieve | 131,108,540 (345x) | 380,354 | - | - | - | - |
| pisum | 808,322 (0.70x) | 1,160,413 | - | - | - | - |
| primes | 1,652,241 (1.31x) | 1,260,125 | - | - | - | - |
| quicksort | 176,619,881 (29.0x) | 6,095,209 | - | - | - | - |
| raytracer | 45,935,217 (11.3x) | 4,067,427 | - | - | - | - |
| regex_lite | 64,887,143 (158x) | 409,856 | - | - | - | - |
| spectral_norm | 264,941,352 (51.9x) | 5,106,524 | - | - | - | - |
| string_equality | 1,178,578 (0.80x) | 1,479,564 | - | - | - | - |
| sum_to_n | 58,345 (0.22x) | 260,246 | - | - | - | - |
| tak | 48,005,000 (0.12x) | 391,110,808 | - | - | - | - |
| tinytemplate | 309,438,946 (736x) | 420,354 | - | - | - | - |
| warden | 44,209,570 (164x) | 270,345 | - | - | - | - |
