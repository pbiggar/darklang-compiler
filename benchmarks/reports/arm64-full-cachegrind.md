# arm64-full-cachegrind comparison

Architecture: `arm64`. Profile: `full`.

Measurement policy: `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`.

Current workload contract: `2ead0323fc6859c952a3b8a83c56e0b0da61e1350b7ddd39ecc4a0b1da25e861`.

Instruction counts include runtime and startup work; they are not elapsed-time speed ratios.

Snapshots are independent. Stale values are retained for provenance and excluded from ratios.

## Stored versions and coverage

| Language             | Version / compiler commit                                                          | Measured                         | Current rows | Audited current rows | Build flags                                              | Runtime flags       |
| -------------------- | ---------------------------------------------------------------------------------- | -------------------------------- | ------------ | -------------------- | -------------------------------------------------------- | ------------------- |
| Darklang             | 4c46c96eacff55406a5761060be8ddfafea01b9e                                           | 2026-09-29T10:09:56+00:00        | 29/29        | 29/29                | not recorded                                             | not recorded        |
| Rust                 | rustc 1.98.0 (88d9e12ae 2026-08-18)                                                | 2026-09-30T22:49:24.673286+00:00 | 29/29        | 29/29                | rustc -C opt-level=3; Cargo --release --locked --offline | not recorded        |
| Haskell              | The Glorious Glasgow Haskell Compilation System, version 9.10.3                    | 2026-09-30T23:16:41.656847+00:00 | 29/29        | 0/29                 | -O2, -j1, -fno-full-laziness                             | not recorded        |
| Python               | Python 3.14.4                                                                      | 2026-09-30T22:51:43.939609+00:00 | 29/29        | 0/29                 | not recorded                                             | not recorded        |
| Node                 | v26.10.0                                                                           | 2026-09-30T22:53:19.949001+00:00 | 29/29        | 0/29                 | not recorded                                             | --stack-size=400000 |
| Roc                  | roc nightly pre-release, built from commit d73ea10 on Tue Sep  9 09:05:23 UTC 2025 | 2026-09-30T22:50:17.952015+00:00 | 29/29        | 0/29                 | build, --optimize, --max-threads=1                       | not recorded        |
| OCaml 5              | 5.4.0                                                                              | 2026-09-30T23:16:31.761763+00:00 | 29/29        | 0/29                 | -O3                                                      | not recorded        |
| Koka                 | Koka 3.2.9, 05:27:12 Sep 18 2026 (ghc release version)                             | 2026-09-30T23:07:50.973016+00:00 | 29/29        | 0/29                 | -O2, --compile, --target=c, --jobs=1                     | not recorded        |
| Darklang interpreter | Darklang CLI alpha-0b3888d (unable to check for updates)                           | 2026-09-30T23:50:09.003682+00:00 | 29/29        | 0/29                 | not recorded                                             | not recorded        |

Haskell provenance: Upstream adaptations and shared reference ports; see IMPLEMENTATIONS.md. Output-validated; algorithm parity requires separate review.

Roc provenance: Upstream adaptations and shared reference ports; see IMPLEMENTATIONS.md. Output-validated; algorithm parity requires separate review.

Koka provenance: Upstream adaptations and shared reference ports; see IMPLEMENTATIONS.md. Output-validated; algorithm parity requires separate review.

Darklang interpreter provenance: Shared Dark sources adapted for interpreter CLI arguments and compatibility syntax; private prepared rundir per workload.

Audited means the implementation's algorithm and source have a current reviewed parity contract.
Output validation alone does not establish algorithm parity.

## Absolute instructions

| Benchmark       |    Darklang |        Rust |     Haskell |         Python |          Node |         Roc |     OCaml 5 |        Koka | Darklang interpreter |
| --------------- | ----------: | ----------: | ----------: | -------------: | ------------: | ----------: | ----------: | ----------: | -------------------: |
| ackermann       | 122,786,825 |  89,563,151 | 233,400,994 |  7,435,668,667 | 1,516,835,463 | 106,273,515 | 162,424,125 | 212,597,040 |      289,145,313,294 |
| binary_trees    |  10,522,518 |  63,997,960 |   8,898,851 |    341,105,822 |   582,470,517 |  65,691,242 |   2,707,513 |  12,819,368 |        5,814,696,606 |
| collatz         |  70,193,415 |  76,740,243 | 128,727,727 |  8,205,848,631 |   833,796,066 | 102,247,557 | 259,426,181 | 335,589,458 |      425,260,861,507 |
| edigits         |  48,970,108 |     690,554 | 265,141,921 |     59,378,989 |   562,343,833 |   2,700,297 |   1,713,357 |   7,796,510 |        1,522,774,854 |
| factorial       |      58,968 |     974,735 | 125,292,326 |    175,304,082 |   730,737,936 |   4,801,889 |  11,868,178 |  19,634,045 |        5,357,225,486 |
| fannkuch        | 221,209,585 |   1,718,504 |   1,718,122 |    121,496,519 |   591,338,572 |  21,343,464 |   3,335,411 |  33,097,145 |        6,033,536,974 |
| fasta           |  10,284,685 |     702,823 |  57,213,413 |    101,857,934 |   609,808,280 |   3,609,682 |  34,650,553 |  21,762,398 |        6,014,617,633 |
| fft             |   1,784,354 |     450,077 |     973,158 |     38,591,395 |   536,253,760 |     428,098 |     722,584 |   1,137,083 |          278,252,826 |
| fib             |   7,630,194 |   5,862,444 |  11,719,410 |    329,222,938 |   748,863,281 |   7,209,452 |  10,714,525 |  13,857,117 |       14,454,720,917 |
| huffman         | 400,353,974 |   2,654,580 |  51,915,841 |    268,278,666 |   677,109,249 | 129,947,983 |  19,822,679 |  33,179,408 |        8,317,129,570 |
| leibniz         |  17,006,289 |  14,263,392 |  38,600,186 |  2,143,353,715 | 1,452,648,330 | 106,380,826 |  50,795,138 | 194,513,856 |      147,544,535,529 |
| mandelbrot      |  15,142,868 |  12,561,627 | 152,762,299 |  1,379,594,763 | 1,603,591,927 |  14,595,366 |  27,494,555 |  24,805,233 |       75,903,469,772 |
| matmul          |  41,471,414 |     649,218 |   2,291,323 |     53,970,927 |   553,659,724 |   3,526,241 |   1,387,961 |   4,612,594 |        2,849,175,849 |
| merkletrees     |   3,871,138 |   2,525,594 |  16,803,531 |  2,037,712,038 | 1,598,893,490 |  79,254,154 |  21,266,422 | 111,489,582 |       46,987,635,452 |
| myers_diff      | 204,588,087 |     680,030 |   2,015,809 |     43,307,251 |   541,452,321 |   5,086,274 |   1,886,025 |   4,607,829 |       49,678,067,699 |
| nbody           |  10,805,793 |   4,253,933 |  10,853,927 |    893,988,107 | 1,303,265,830 |  30,085,462 |  12,867,536 | 195,226,396 |       24,304,253,249 |
| nqueen          |   6,924,920 |   5,917,279 |   8,914,559 |    497,218,032 |   578,875,108 | 334,829,317 |  11,932,641 |   7,613,051 |       18,125,344,494 |
| nsieve          | 130,174,724 |     384,875 |   1,044,317 |     41,579,651 |   536,073,290 |     467,068 |     805,016 |   2,374,896 |        1,356,945,184 |
| pisum           |     808,063 |   1,164,875 |   2,029,711 |    108,606,163 |   576,664,517 |   5,651,904 |   2,145,234 |   8,619,235 |        4,600,202,460 |
| primes          |   1,567,031 |   1,264,647 |   4,260,329 |     87,452,499 |   774,931,671 |   1,653,765 |   8,788,415 |  29,391,711 |        4,415,323,348 |
| quicksort       | 172,560,188 |   6,083,846 |  12,134,973 |    105,361,021 |   640,449,577 |  17,954,163 |  41,908,136 |  15,403,032 |        3,248,091,566 |
| raytracer       |  43,229,793 |   4,071,804 |  12,276,041 |  1,813,642,936 | 1,405,135,093 |  11,651,665 |  25,835,658 | 310,136,778 |       32,144,683,021 |
| regex_lite      |  64,751,641 |     417,286 |     867,990 |     56,322,827 |   531,166,575 |     475,699 |     687,752 |     893,174 |          382,682,381 |
| spectral_norm   | 215,931,384 |   4,910,505 |   9,857,137 |    749,241,105 |   924,402,353 |  37,999,116 |  22,764,705 | 111,068,677 |       49,626,407,032 |
| string_equality |   1,174,023 |   1,481,761 | 194,765,556 |     66,991,847 |   543,387,761 |   6,803,761 |   2,981,067 |   5,951,367 |          769,239,043 |
| sum_to_n        |      57,970 |     264,794 |  10,940,691 |    942,413,274 |   901,837,453 |   4,250,461 |   9,547,733 |   9,589,288 |       26,390,505,010 |
| tak             |  48,004,622 | 391,113,710 | 431,125,109 | 10,869,237,783 | 2,843,356,975 | 359,863,181 | 499,215,239 | 692,436,379 |      474,568,017,657 |
| tinytemplate    | 305,936,131 |     421,440 |   2,164,752 |     65,544,848 |   538,603,746 |   1,250,860 |   1,258,729 |   2,850,619 |        5,677,129,280 |
| warden          |  44,133,565 |     274,791 |     737,195 |     58,390,299 |   530,637,964 |     248,878 |     645,878 |     583,637 |          292,693,946 |

## Instructions relative to Rust

Ratios for unaudited implementations are informational comparisons of validated output.

| Benchmark       | Darklang |   Rust |  Haskell |    Python |      Node |     Roc | OCaml 5 |    Koka | Darklang interpreter |
| --------------- | -------: | -----: | -------: | --------: | --------: | ------: | ------: | ------: | -------------------: |
| ackermann       |   1.371x | 1.000x |   2.606x |   83.022x |   16.936x |  1.187x |  1.814x |  2.374x |            3228.396x |
| binary_trees    |   0.164x | 1.000x |   0.139x |    5.330x |    9.101x |  1.026x |  0.042x |  0.200x |              90.858x |
| collatz         |   0.915x | 1.000x |   1.677x |  106.930x |   10.865x |  1.332x |  3.381x |  4.373x |            5541.563x |
| edigits         |  70.914x | 1.000x | 383.955x |   85.987x |  814.337x |  3.910x |  2.481x | 11.290x |            2205.150x |
| factorial       |   0.060x | 1.000x | 128.540x |  179.848x |  749.679x |  4.926x | 12.176x | 20.143x |            5496.084x |
| fannkuch        | 128.722x | 1.000x |   1.000x |   70.699x |  344.101x | 12.420x |  1.941x | 19.259x |            3510.924x |
| fasta           |  14.633x | 1.000x |  81.405x |  144.927x |  867.656x |  5.136x | 49.302x | 30.964x |            8557.799x |
| fft             |   3.965x | 1.000x |   2.162x |   85.744x | 1191.471x |  0.951x |  1.605x |  2.526x |             618.234x |
| fib             |   1.302x | 1.000x |   1.999x |   56.158x |  127.739x |  1.230x |  1.828x |  2.364x |            2465.648x |
| huffman         | 150.816x | 1.000x |  19.557x |  101.063x |  255.072x | 48.952x |  7.467x | 12.499x |            3133.124x |
| leibniz         |   1.192x | 1.000x |   2.706x |  150.270x |  101.845x |  7.458x |  3.561x | 13.637x |           10344.281x |
| mandelbrot      |   1.205x | 1.000x |  12.161x |  109.826x |  127.658x |  1.162x |  2.189x |  1.975x |            6042.487x |
| matmul          |  63.879x | 1.000x |   3.529x |   83.132x |  852.810x |  5.432x |  2.138x |  7.105x |            4388.627x |
| merkletrees     |   1.533x | 1.000x |   6.653x |  806.825x |  633.076x | 31.380x |  8.420x | 44.144x |           18604.588x |
| myers_diff      | 300.852x | 1.000x |   2.964x |   63.684x |  796.218x |  7.479x |  2.773x |  6.776x |           73052.759x |
| nbody           |   2.540x | 1.000x |   2.552x |  210.156x |  306.367x |  7.072x |  3.025x | 45.893x |            5713.361x |
| nqueen          |   1.170x | 1.000x |   1.507x |   84.028x |   97.828x | 56.585x |  2.017x |  1.287x |            3063.121x |
| nsieve          | 338.226x | 1.000x |   2.713x |  108.034x | 1392.850x |  1.214x |  2.092x |  6.171x |            3525.678x |
| pisum           |   0.694x | 1.000x |   1.742x |   93.234x |  495.044x |  4.852x |  1.842x |  7.399x |            3949.095x |
| primes          |   1.239x | 1.000x |   3.369x |   69.152x |  612.765x |  1.308x |  6.949x | 23.241x |            3491.348x |
| quicksort       |  28.364x | 1.000x |   1.995x |   17.318x |  105.271x |  2.951x |  6.888x |  2.532x |             533.888x |
| raytracer       |  10.617x | 1.000x |   3.015x |  445.415x |  345.089x |  2.862x |  6.345x | 76.167x |            7894.457x |
| regex_lite      | 155.173x | 1.000x |   2.080x |  134.974x | 1272.908x |  1.140x |  1.648x |  2.140x |             917.075x |
| spectral_norm   |  43.973x | 1.000x |   2.007x |  152.579x |  188.250x |  7.738x |  4.636x | 22.619x |           10106.172x |
| string_equality |   0.792x | 1.000x | 131.442x |   45.211x |  366.718x |  4.592x |  2.012x |  4.016x |             519.138x |
| sum_to_n        |   0.219x | 1.000x |  41.318x | 3559.043x | 3405.808x | 16.052x | 36.057x | 36.214x |           99664.286x |
| tak             |   0.123x | 1.000x |   1.102x |   27.790x |    7.270x |  0.920x |  1.276x |  1.770x |            1213.376x |
| tinytemplate    | 725.930x | 1.000x |   5.137x |  155.526x | 1278.008x |  2.968x |  2.987x |  6.764x |           13470.789x |
| warden          | 160.608x | 1.000x |   2.683x |  212.490x | 1931.060x |  0.906x |  2.350x |  2.124x |            1065.151x |

## Aggregate comparisons

Fixed workload sets keep averages comparable as language coverage grows.
Geometric means are informational unless every included row is audited.

### Original 21 workloads

`ackermann`, `binary_trees`, `collatz`, `edigits`, `factorial`, `fannkuch`, `fasta`, `fib`, `leibniz`, `mandelbrot`, `matmul`, `merkletrees`, `nbody`, `nqueen`, `nsieve`, `pisum`, `primes`, `quicksort`, `spectral_norm`, `sum_to_n`, `tak`.

| Language             | Instructions / Rust | Total instructions | Current rows | Parity    |
| -------------------- | ------------------: | -----------------: | -----------: | --------- |
| Darklang             |              2.976x |      1,155,982,702 |        21/21 | audited   |
| Rust                 |              1.000x |        685,608,709 |        21/21 | audited   |
| Haskell              |              4.454x |      1,533,730,856 |        21/21 | unaudited |
| Python               |            101.944x |     36,679,612,660 |        21/21 | unaudited |
| Node                 |            198.014x |     20,464,844,193 |        21/21 | unaudited |
| Roc                  |              3.970x |      1,310,388,122 |        21/21 | unaudited |
| OCaml 5              |              3.220x |      1,197,758,574 |        21/21 | unaudited |
| Koka                 |              7.286x |      2,064,297,011 |        21/21 | unaudited |
| Darklang interpreter |           3988.242x |  1,633,523,653,873 |        21/21 | unaudited |

### All 29 workloads

`ackermann`, `binary_trees`, `collatz`, `edigits`, `factorial`, `fannkuch`, `fasta`, `fft`, `fib`, `huffman`, `leibniz`, `mandelbrot`, `matmul`, `merkletrees`, `myers_diff`, `nbody`, `nqueen`, `nsieve`, `pisum`, `primes`, `quicksort`, `raytracer`, `regex_lite`, `spectral_norm`, `string_equality`, `sum_to_n`, `tak`, `tinytemplate`, `warden`.

| Language             | Instructions / Rust | Total instructions | Current rows | Parity    |
| -------------------- | ------------------: | -----------------: | -----------: | --------- |
| Darklang             |              6.402x |      2,221,934,270 |        29/29 | audited   |
| Rust                 |              1.000x |        696,060,478 |        29/29 | audited   |
| Haskell              |              4.808x |      1,799,447,198 |        29/29 | unaudited |
| Python               |            107.159x |     39,090,682,729 |        29/29 | unaudited |
| Node                 |            285.527x |     25,768,590,662 |        29/29 | unaudited |
| Roc                  |              3.773x |      1,466,281,340 |        29/29 | unaudited |
| OCaml 5              |              3.126x |      1,251,598,946 |        29/29 | unaudited |
| Koka                 |              6.949x |      2,423,636,906 |        29/29 | unaudited |
| Darklang interpreter |           3706.217x |  1,731,063,531,639 |        29/29 | unaudited |
