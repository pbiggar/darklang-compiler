# Benchmark Results

Best-known compatible full-profile Dark performance vs audited Rust references, with diagnostic reference runtimes (instruction counts).

**Snapshot timestamp:** 2026-09-27T08:12:17+00:00
**Architecture:** `arm64`
**Profile:** `full` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`
**Workload contract:** `6751ca8977c639512cdd772867f7d2bd0c73833716cddfcb79aa6f15a08f8cfe`
**Compiler commit:** `bfb1b8098f1794f023d6a91ea1fccbcb26829920` - Update SCCP MIR expectations for SSA lowering
**Diagnostic references:** informational only; multipliers are instructions divided by Rust for the same workload.
Every displayed diagnostic row matched the profile's expected stdout.

| Benchmark | Dark (6.78x) | Rust | Darklang interpreter | Node | OCaml | Python |
|---|---:|---:|---:|---:|---:|---:|
| ackermann | 145,107,511 (1.62x) | 89,558,784 | - | - | - | - |
| binary_trees | 11,046,953 (0.17x) | 63,993,594 | - | - | - | - |
| collatz | 70,193,689 (0.91x) | 76,735,737 | - | - | - | - |
| edigits | 49,974,762 (72.6x) | 688,749 | - | - | - | - |
| factorial | 59,296 (0.06x) | 970,290 | - | - | - | - |
| fannkuch | 220,640,790 (125x) | 1,760,885 | - | - | - | - |
| fasta | 18,804,733 (27.4x) | 686,768 | - | - | - | - |
| fft | 1,793,111 (3.95x) | 454,262 | - | - | - | - |
| fib | 8,265,915 (1.37x) | 6,054,417 | - | - | - | - |
| huffman | 410,638,818 (143x) | 2,868,263 | - | - | - | - |
| leibniz | 17,006,612 (1.19x) | 14,258,894 | - | - | - | - |
| mandelbrot | 15,142,312 (1.21x) | 12,557,270 | - | - | - | - |
| matmul | 42,706,145 (65.8x) | 649,416 | - | - | - | - |
| merkletrees | 3,871,295 (1.53x) | 2,523,420 | - | - | - | - |
| myers_diff | 204,989,596 (298x) | 687,142 | - | - | - | - |
| nbody | 14,536,066 (3.42x) | 4,249,498 | - | - | - | - |
| nqueen | 7,417,756 (1.25x) | 5,914,962 | - | - | - | - |
| nsieve | 130,974,242 (344x) | 380,354 | - | - | - | - |
| pisum | 808,309 (0.70x) | 1,160,413 | - | - | - | - |
| primes | 1,652,239 (1.31x) | 1,260,125 | - | - | - | - |
| quicksort | 176,466,730 (29.0x) | 6,095,209 | - | - | - | - |
| raytracer | 45,983,570 (11.3x) | 4,067,427 | - | - | - | - |
| regex_lite | 64,886,726 (158x) | 409,856 | - | - | - | - |
| spectral_norm | 267,341,322 (52.4x) | 5,106,524 | - | - | - | - |
| string_equality | 1,178,543 (0.80x) | 1,479,564 | - | - | - | - |
| sum_to_n | 58,340 (0.22x) | 260,246 | - | - | - | - |
| tak | 48,005,008 (0.12x) | 391,110,808 | - | - | - | - |
| tinytemplate | 309,411,161 (736x) | 420,354 | - | - | - | - |
| warden | 44,209,521 (164x) | 270,345 | - | - | - | - |
