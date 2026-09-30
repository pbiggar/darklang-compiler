# arm64-full-cachegrind comparison

Architecture: `arm64`. Profile: `full`.

Measurement policy: `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`.

Current workload contract: `2ead0323fc6859c952a3b8a83c56e0b0da61e1350b7ddd39ecc4a0b1da25e861`.

Instruction counts include runtime and startup work; they are not elapsed-time speed ratios.

Snapshots are independent. Stale values are retained for provenance and excluded from ratios.

## Stored versions and coverage

| Language | Version / compiler commit | Measured | Current rows | Audited current rows | Build flags | Runtime flags |
| --- | --- | --- | --- | --- | --- | --- |
| Darklang | 4c46c96eacff55406a5761060be8ddfafea01b9e | 2026-09-29T10:09:56+00:00 | 29/29 | 29/29 | not recorded | not recorded |
| Rust | not recorded | not recorded | 29/29 | 29/29 | not recorded | not recorded |
| Haskell | not recorded | not recorded | 0/29 | 0/0 | not recorded | not recorded |
| Python | Python 3.14.4 | 2026-09-15T07:45:58.287134+00:00 | 0/29 | 0/0 | not recorded | not recorded |
| Node | v26.8.2 | 2026-09-15T07:45:58.287134+00:00 | 0/29 | 0/0 | not recorded | not recorded |
| Roc | not recorded | not recorded | 0/29 | 0/0 | not recorded | not recorded |
| OCaml 5 | 5.4.0 | 2026-09-15T07:45:58.287134+00:00 | 0/29 | 0/0 | not recorded | not recorded |
| Koka | not recorded | not recorded | 0/29 | 0/0 | not recorded | not recorded |
| Darklang interpreter | Darklang CLI alpha-0b3888d (unable to check for updates) | 2026-09-15T07:45:58.287134+00:00 | 0/29 | 0/0 | not recorded | not recorded |

Rust provenance: Imported audited counts from BASELINES.md using the track and contract named by the original RESULTS.md. Rust version, measurement date and per-source hashes were not recorded.

Python provenance: Imported from baselines/diagnostic-arm64-full-cachegrind.json; original suite contract retained. Per-source hashes and build flags were not recorded.

Node provenance: Imported from baselines/diagnostic-arm64-full-cachegrind.json; original suite contract retained. Per-source hashes and build flags were not recorded.

OCaml 5 provenance: Imported from baselines/diagnostic-arm64-full-cachegrind.json; original suite contract retained. Per-source hashes and build flags were not recorded.

Darklang interpreter provenance: Imported from baselines/diagnostic-arm64-full-cachegrind.json; original suite contract retained. Per-source hashes and build flags were not recorded.

Audited means the implementation's algorithm and source have a current reviewed parity contract.
Output validation alone does not establish algorithm parity.

## Absolute instructions

| Benchmark | Darklang | Rust | Haskell | Python | Node | Roc | OCaml 5 | Koka | Darklang interpreter |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ackermann | 122,786,825 | 89,558,784 | missing implementation | 7,435,285,696 (stale workload contract) | 2,988,949,523 (stale workload contract) | missing implementation | 162,425,405 (stale workload contract) | missing implementation | 295,450,197,862 (stale workload contract) |
| binary_trees | 10,522,518 | 63,993,594 | missing implementation | 340,787,116 (stale workload contract) | 571,585,402 (stale workload contract) | missing implementation | 2,708,791 (stale workload contract) | missing implementation | 10,539,988,398 (stale workload contract) |
| collatz | 70,193,415 | 76,735,737 | missing implementation | 8,205,426,289 (stale workload contract) | 2,333,314,226 (stale workload contract) | missing implementation | 259,431,304 (stale workload contract) | missing implementation | 424,757,265,260 (stale workload contract) |
| edigits | 48,970,108 | 688,749 | missing implementation | 59,056,689 (stale workload contract) | 547,241,171 (stale workload contract) | missing implementation | 1,714,617 (stale workload contract) | missing implementation | 9,579,126,978 (stale workload contract) |
| factorial | 58,968 | 970,290 | missing implementation | 174,922,629 (stale workload contract) | 716,988,606 (stale workload contract) | missing implementation | 11,869,471 (stale workload contract) | missing implementation | 5,403,174,630 (stale workload contract) |
| fannkuch | 221,209,585 | 1,760,885 | missing implementation | 121,114,833 (stale workload contract) | 582,055,776 (stale workload contract) | missing implementation | 3,336,708 (stale workload contract) | missing implementation | 12,839,313,429 (stale workload contract) |
| fasta | 10,284,685 | 686,768 | missing implementation | 101,484,096 (stale workload contract) | 593,012,606 (stale workload contract) | missing implementation | 34,651,837 (stale workload contract) | missing implementation | 7,739,275,817 (stale workload contract) |
| fft | 1,784,354 | 454,262 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 2,212,941,147 (stale workload contract) |
| fib | 7,630,194 | 6,054,417 | missing implementation | 328,935,849 (stale workload contract) | 634,144,326 (stale workload contract) | missing implementation | 10,715,807 (stale workload contract) | missing implementation | 15,095,723,230 (stale workload contract) |
| huffman | 400,353,974 | 2,868,263 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 9,087,251,458 (stale workload contract) |
| leibniz | 17,006,289 | 14,258,894 | missing implementation | 2,142,960,942 (stale workload contract) | 659,604,280 (stale workload contract) | missing implementation | 50,800,280 (stale workload contract) | missing implementation | 151,080,303,976 (stale workload contract) |
| mandelbrot | 15,142,868 | 12,557,270 | missing implementation | 1,379,292,050 (stale workload contract) | 1,288,160,359 (stale workload contract) | missing implementation | 27,495,820 (stale workload contract) | missing implementation | 76,696,314,807 (stale workload contract) |
| matmul | 41,471,414 | 649,416 | missing implementation | 53,587,209 (stale workload contract) | 534,382,802 (stale workload contract) | missing implementation | 1,389,225 (stale workload contract) | missing implementation | 7,760,332,418 (stale workload contract) |
| merkletrees | 3,871,138 | 2,523,420 | missing implementation | 2,037,355,255 (stale workload contract) | 2,013,094,566 (stale workload contract) | missing implementation | 21,267,710 (stale workload contract) | missing implementation | 51,421,932,780 (stale workload contract) |
| myers_diff | 204,588,087 | 687,142 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 4,670,031,121 (stale workload contract) |
| nbody | 10,805,793 | 4,249,498 | missing implementation | 893,653,392 (stale workload contract) | 1,291,011,435 (stale workload contract) | missing implementation | 12,868,834 (stale workload contract) | missing implementation | 27,005,569,055 (stale workload contract) |
| nqueen | 6,924,920 | 5,914,962 | missing implementation | 496,834,180 (stale workload contract) | 727,914,031 (stale workload contract) | missing implementation | 11,937,397 (stale workload contract) | missing implementation | 19,342,874,779 (stale workload contract) |
| nsieve | 130,174,724 | 380,354 | missing implementation | 41,209,623 (stale workload contract) | 526,439,017 (stale workload contract) | missing implementation | 806,276 (stale workload contract) | missing implementation | 1,938,134,698 (stale workload contract) |
| pisum | 808,063 | 1,160,413 | missing implementation | 108,169,809 (stale workload contract) | 564,837,012 (stale workload contract) | missing implementation | 2,146,514 (stale workload contract) | missing implementation | 10,805,308,243 (stale workload contract) |
| primes | 1,567,031 | 1,260,125 | missing implementation | 87,074,623 (stale workload contract) | 767,760,434 (stale workload contract) | missing implementation | 8,789,707 (stale workload contract) | missing implementation | 14,200,381,458 (stale workload contract) |
| quicksort | 172,560,188 | 6,095,209 | missing implementation | 105,043,240 (stale workload contract) | 630,143,924 (stale workload contract) | missing implementation | 41,909,401 (stale workload contract) | missing implementation | 8,610,104,455 (stale workload contract) |
| raytracer | 43,229,793 | 4,067,427 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 39,823,213,701 (stale workload contract) |
| regex_lite | 64,751,641 | 409,856 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 652,648,435 (stale workload contract) |
| spectral_norm | 215,931,384 | 5,106,524 | missing implementation | 748,807,741 (stale workload contract) | 953,697,743 (stale workload contract) | missing implementation | 22,765,979 (stale workload contract) | missing implementation | 60,452,108,981 (stale workload contract) |
| string_equality | 1,174,023 | 1,479,564 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 2,287,266,006 (stale workload contract) |
| sum_to_n | 57,970 | 260,246 | missing implementation | 942,028,993 (stale workload contract) | 888,926,608 (stale workload contract) | missing implementation | 9,549,026 (stale workload contract) | missing implementation | 29,651,727,831 (stale workload contract) |
| tak | 48,004,622 | 391,110,808 | missing implementation | 10,868,859,617 (stale workload contract) | 2,191,746,728 (stale workload contract) | missing implementation | 499,216,513 (stale workload contract) | missing implementation | 472,304,535,056 (stale workload contract) |
| tinytemplate | 305,936,131 | 420,354 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 5,054,772,489 (stale workload contract) |
| warden | 44,133,565 | 270,345 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 29,400,568,988 (stale workload contract) |

## Instructions relative to Rust

Ratios for unaudited implementations are informational comparisons of validated output.

| Benchmark | Darklang | Rust | Haskell | Python | Node | Roc | OCaml 5 | Koka | Darklang interpreter |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ackermann | 1.371× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| binary_trees | 0.164× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| collatz | 0.915× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| edigits | 71.100× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| factorial | 0.061× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| fannkuch | 125.624× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| fasta | 14.975× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| fft | 3.928× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| fib | 1.260× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| huffman | 139.581× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| leibniz | 1.193× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| mandelbrot | 1.206× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| matmul | 63.860× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| merkletrees | 1.534× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| myers_diff | 297.738× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| nbody | 2.543× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| nqueen | 1.171× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| nsieve | 342.246× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| pisum | 0.696× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| primes | 1.244× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| quicksort | 28.311× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| raytracer | 10.628× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| regex_lite | 157.986× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| spectral_norm | 42.285× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| string_equality | 0.793× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| sum_to_n | 0.223× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| tak | 0.123× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| tinytemplate | 727.806× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| warden | 163.249× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |

## Common-workload aggregate

All included languages use this exact workload set: `ackermann`, `binary_trees`, `collatz`, `edigits`, `factorial`, `fannkuch`, `fasta`, `fft`, `fib`, `huffman`, `leibniz`, `mandelbrot`, `matmul`, `merkletrees`, `myers_diff`, `nbody`, `nqueen`, `nsieve`, `pisum`, `primes`, `quicksort`, `raytracer`, `regex_lite`, `spectral_norm`, `string_equality`, `sum_to_n`, `tak`, `tinytemplate`, `warden`.

Geometric means below are informational unless every included row is audited.

| Language | Instructions / Rust | Parity |
| --- | --- | --- |
| Darklang | 6.383× | audited |
| Rust | 1.000× | audited |

Excluded because no current measurements exist: Haskell, Python, Node, Roc, OCaml 5, Koka, Darklang interpreter.
