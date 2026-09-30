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
| Rust | rustc 1.98.0 (88d9e12ae 2026-08-18) | 2026-09-30T18:18:10.032444+00:00 | 29/29 | 29/29 | rustc -C opt-level=3; Cargo --release --locked --offline | not recorded |
| Haskell | not recorded | not recorded | 0/29 | 0/0 | not recorded | not recorded |
| Python | Python 3.14.4 | 2026-09-30T18:20:48.547498+00:00 | 21/29 | 0/21 | not recorded | not recorded |
| Node | v26.10.0 | 2026-09-30T18:24:04.098744+00:00 | 21/29 | 0/21 | not recorded | --stack-size=400000 |
| Roc | not recorded | not recorded | 0/29 | 0/0 | not recorded | not recorded |
| OCaml 5 | 5.4.0 | 2026-09-30T18:24:11.832467+00:00 | 21/29 | 0/21 | -O3 | not recorded |
| Koka | not recorded | not recorded | 0/29 | 0/0 | not recorded | not recorded |
| Darklang interpreter | Darklang CLI alpha-0b3888d (unable to check for updates) | 2026-09-15T07:45:58.287134+00:00 | 0/29 | 0/0 | not recorded | not recorded |

Darklang interpreter provenance: Imported from baselines/diagnostic-arm64-full-cachegrind.json; original suite contract retained. Per-source hashes and build flags were not recorded.

Audited means the implementation's algorithm and source have a current reviewed parity contract.
Output validation alone does not establish algorithm parity.

## Absolute instructions

| Benchmark | Darklang | Rust | Haskell | Python | Node | Roc | OCaml 5 | Koka | Darklang interpreter |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ackermann | 122,786,825 | 89,563,240 | missing implementation | 7,435,704,574 | 1,661,723,975 | missing implementation | 162,424,721 | missing implementation | 295,450,197,862 (stale workload contract) |
| binary_trees | 10,522,518 | 63,998,051 | missing implementation | 341,126,991 | 579,774,945 | missing implementation | 2,708,104 | missing implementation | 10,539,988,398 (stale workload contract) |
| collatz | 70,193,415 | 76,740,336 | missing implementation | 8,205,793,113 | 3,305,592,343 | missing implementation | 259,426,785 | missing implementation | 424,757,265,260 (stale workload contract) |
| edigits | 48,970,108 | 690,647 | missing implementation | 59,381,256 | 571,031,817 | missing implementation | 1,713,937 | missing implementation | 9,579,126,978 (stale workload contract) |
| factorial | 58,968 | 974,828 | missing implementation | 175,310,447 | 731,860,721 | missing implementation | 11,868,786 | missing implementation | 5,403,174,630 (stale workload contract) |
| fannkuch | 221,209,585 | 1,718,612 | missing implementation | 121,459,169 | 592,000,703 | missing implementation | 3,336,015 | missing implementation | 12,839,313,429 (stale workload contract) |
| fasta | 10,284,685 | 702,933 | missing implementation | 101,856,084 | 607,530,667 | missing implementation | 34,651,136 | missing implementation | 7,739,275,817 (stale workload contract) |
| fft | 1,784,354 | 450,657 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 2,212,941,147 (stale workload contract) |
| fib | 7,630,194 | 5,862,527 | missing implementation | 329,254,205 | 745,085,135 | missing implementation | 10,715,091 | missing implementation | 15,095,723,230 (stale workload contract) |
| huffman | 400,353,974 | 2,654,673 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 9,087,251,458 (stale workload contract) |
| leibniz | 17,006,289 | 14,263,492 | missing implementation | 2,143,378,778 | 1,449,516,939 | missing implementation | 50,795,726 | missing implementation | 151,080,303,976 (stale workload contract) |
| mandelbrot | 15,142,868 | 12,561,718 | missing implementation | 1,379,592,067 | 1,598,795,677 | missing implementation | 27,495,155 | missing implementation | 76,696,314,807 (stale workload contract) |
| matmul | 41,471,414 | 649,315 | missing implementation | 53,991,465 | 549,119,175 | missing implementation | 1,388,545 | missing implementation | 7,760,332,418 (stale workload contract) |
| merkletrees | 3,871,138 | 2,525,684 | missing implementation | 2,037,739,708 | 1,564,950,434 | missing implementation | 21,267,006 | missing implementation | 51,421,932,780 (stale workload contract) |
| myers_diff | 204,588,087 | 680,117 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 4,670,031,121 (stale workload contract) |
| nbody | 10,805,793 | 4,254,050 | missing implementation | 894,052,104 | 1,304,099,402 | missing implementation | 12,868,135 | missing implementation | 27,005,569,055 (stale workload contract) |
| nqueen | 6,924,920 | 5,917,376 | missing implementation | 497,150,720 | 668,329,410 | missing implementation | 11,933,225 | missing implementation | 19,342,874,779 (stale workload contract) |
| nsieve | 130,174,724 | 384,968 | missing implementation | 41,551,604 | 539,634,556 | missing implementation | 805,596 | missing implementation | 1,938,134,698 (stale workload contract) |
| pisum | 808,063 | 1,164,968 | missing implementation | 108,569,346 | 581,574,634 | missing implementation | 2,145,813 | missing implementation | 10,805,308,243 (stale workload contract) |
| primes | 1,567,031 | 1,264,751 | missing implementation | 87,477,763 | 774,936,899 | missing implementation | 8,789,011 | missing implementation | 14,200,381,458 (stale workload contract) |
| quicksort | 172,560,188 | 6,083,933 | missing implementation | 105,397,936 | 642,796,524 | missing implementation | 41,908,736 | missing implementation | 8,610,104,455 (stale workload contract) |
| raytracer | 43,229,793 | 4,071,893 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 39,823,213,701 (stale workload contract) |
| regex_lite | 64,751,641 | 417,377 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 652,648,435 (stale workload contract) |
| spectral_norm | 215,931,384 | 4,910,603 | missing implementation | 749,209,899 | 969,228,137 | missing implementation | 22,765,301 | missing implementation | 60,452,108,981 (stale workload contract) |
| string_equality | 1,174,023 | 1,481,825 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 2,287,266,006 (stale workload contract) |
| sum_to_n | 57,970 | 264,898 | missing implementation | 942,410,530 | 900,216,037 | missing implementation | 9,548,341 | missing implementation | 29,651,727,831 (stale workload contract) |
| tak | 48,004,622 | 391,113,990 | missing implementation | 10,869,264,660 | 5,832,873,760 | missing implementation | 499,215,818 | missing implementation | 472,304,535,056 (stale workload contract) |
| tinytemplate | 305,936,131 | 421,512 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 5,054,772,489 (stale workload contract) |
| warden | 44,133,565 | 274,892 | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | missing implementation | 29,400,568,988 (stale workload contract) |

## Instructions relative to Rust

Ratios for unaudited implementations are informational comparisons of validated output.

| Benchmark | Darklang | Rust | Haskell | Python | Node | Roc | OCaml 5 | Koka | Darklang interpreter |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ackermann | 1.371× | 1.000× | unavailable | 83.022× | 18.554× | unavailable | 1.814× | unavailable | unavailable |
| binary_trees | 0.164× | 1.000× | unavailable | 5.330× | 9.059× | unavailable | 0.042× | unavailable | unavailable |
| collatz | 0.915× | 1.000× | unavailable | 106.929× | 43.075× | unavailable | 3.381× | unavailable | unavailable |
| edigits | 70.905× | 1.000× | unavailable | 85.979× | 826.807× | unavailable | 2.482× | unavailable | unavailable |
| factorial | 0.060× | 1.000× | unavailable | 179.837× | 750.759× | unavailable | 12.175× | unavailable | unavailable |
| fannkuch | 128.714× | 1.000× | unavailable | 70.673× | 344.464× | unavailable | 1.941× | unavailable | unavailable |
| fasta | 14.631× | 1.000× | unavailable | 144.902× | 864.280× | unavailable | 49.295× | unavailable | unavailable |
| fft | 3.959× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| fib | 1.302× | 1.000× | unavailable | 56.163× | 127.093× | unavailable | 1.828× | unavailable | unavailable |
| huffman | 150.811× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| leibniz | 1.192× | 1.000× | unavailable | 150.270× | 101.624× | unavailable | 3.561× | unavailable | unavailable |
| mandelbrot | 1.205× | 1.000× | unavailable | 109.825× | 127.275× | unavailable | 2.189× | unavailable | unavailable |
| matmul | 63.869× | 1.000× | unavailable | 83.151× | 845.690× | unavailable | 2.138× | unavailable | unavailable |
| merkletrees | 1.533× | 1.000× | unavailable | 806.807× | 619.615× | unavailable | 8.420× | unavailable | unavailable |
| myers_diff | 300.813× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| nbody | 2.540× | 1.000× | unavailable | 210.165× | 306.555× | unavailable | 3.025× | unavailable | unavailable |
| nqueen | 1.170× | 1.000× | unavailable | 84.015× | 112.944× | unavailable | 2.017× | unavailable | unavailable |
| nsieve | 338.144× | 1.000× | unavailable | 107.935× | 1401.765× | unavailable | 2.093× | unavailable | unavailable |
| pisum | 0.694× | 1.000× | unavailable | 93.195× | 499.219× | unavailable | 1.842× | unavailable | unavailable |
| primes | 1.239× | 1.000× | unavailable | 69.166× | 612.719× | unavailable | 6.949× | unavailable | unavailable |
| quicksort | 28.363× | 1.000× | unavailable | 17.324× | 105.655× | unavailable | 6.888× | unavailable | unavailable |
| raytracer | 10.617× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| regex_lite | 155.139× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| spectral_norm | 43.972× | 1.000× | unavailable | 152.570× | 197.375× | unavailable | 4.636× | unavailable | unavailable |
| string_equality | 0.792× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| sum_to_n | 0.219× | 1.000× | unavailable | 3557.636× | 3398.350× | unavailable | 36.045× | unavailable | unavailable |
| tak | 0.123× | 1.000× | unavailable | 27.791× | 14.913× | unavailable | 1.276× | unavailable | unavailable |
| tinytemplate | 725.806× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |
| warden | 160.549× | 1.000× | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |

## Common-workload aggregate

All included languages use this exact workload set: `ackermann`, `binary_trees`, `collatz`, `edigits`, `factorial`, `fannkuch`, `fasta`, `fib`, `leibniz`, `mandelbrot`, `matmul`, `merkletrees`, `nbody`, `nqueen`, `nsieve`, `pisum`, `primes`, `quicksort`, `spectral_norm`, `sum_to_n`, `tak`.

Geometric means below are informational unless every included row is audited.

| Language | Instructions / Rust | Parity |
| --- | --- | --- |
| Darklang | 2.976× | audited |
| Rust | 1.000× | audited |
| Python | 101.936× | unaudited |
| Node | 221.607× | unaudited |
| OCaml 5 | 3.220× | unaudited |

Excluded because no current measurements exist: Haskell, Roc, Koka, Darklang interpreter.
