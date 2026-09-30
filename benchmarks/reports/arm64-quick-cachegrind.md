# arm64-quick-cachegrind comparison

Architecture: `arm64`. Profile: `quick`.

Measurement policy: `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`.

Current workload contract: `fbf9d7000df1263214d3375d12e0bb57ed9c8d565fc45ed7fcb499b927c71b39`.

Instruction counts include runtime and startup work; they are not elapsed-time speed ratios.

Snapshots are independent. Stale values are retained for provenance and excluded from ratios.

## Stored versions and coverage

| Language             | Version / compiler commit                | Measured                  | Current rows | Audited current rows | Build flags  | Runtime flags |
| -------------------- | ---------------------------------------- | ------------------------- | ------------ | -------------------- | ------------ | ------------- |
| Darklang             | 59042c7fea57a6cd8783131ecbd53900d35ad81b | 2026-09-13T22:26:19+00:00 | 0/29         | 0/0                  | not recorded | not recorded  |
| Rust                 | not recorded                             | not recorded              | 0/29         | 0/0                  | not recorded | not recorded  |
| Haskell              | not recorded                             | not recorded              | 0/29         | 0/0                  | not recorded | not recorded  |
| Python               | not recorded                             | not recorded              | 0/29         | 0/0                  | not recorded | not recorded  |
| Node                 | not recorded                             | not recorded              | 0/29         | 0/0                  | not recorded | not recorded  |
| Roc                  | not recorded                             | not recorded              | 0/29         | 0/0                  | not recorded | not recorded  |
| OCaml 5              | not recorded                             | not recorded              | 0/29         | 0/0                  | not recorded | not recorded  |
| Koka                 | not recorded                             | not recorded              | 0/29         | 0/0                  | not recorded | not recorded  |
| Darklang interpreter | not recorded                             | not recorded              | 0/29         | 0/0                  | not recorded | not recorded  |

Audited means the implementation's algorithm and source have a current reviewed parity contract.
Output validation alone does not establish algorithm parity.

## Absolute instructions

| Benchmark       |                                Darklang |         Rust |      Haskell |       Python |         Node |          Roc |      OCaml 5 |         Koka | Darklang interpreter |
| --------------- | --------------------------------------: | -----------: | -----------: | -----------: | -----------: | -----------: | -----------: | -----------: | -------------------: |
| ackermann       |     2,245,077 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| binary_trees    |       859,053 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| collatz         |       403,754 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| edigits         |     4,555,967 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| factorial       |        13,905 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fannkuch        |       589,671 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fasta           |     1,805,086 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fft             |   414,397,748 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fib             |       289,956 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| huffman         |     5,696,732 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| leibniz         |       856,762 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| mandelbrot      |       962,368 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| matmul          |        46,792 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| merkletrees     |       615,017 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| myers_diff      |   313,299,302 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| nbody           |       152,362 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| nqueen          |       370,110 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| nsieve          |        14,665 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| pisum           |        92,514 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| primes          |        74,565 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| quicksort       |       454,164 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| raytracer       |     1,797,142 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| regex_lite      |    14,690,492 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| spectral_norm   |       289,958 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| string_equality |       640,040 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| sum_to_n        |        61,600 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| tak             |    48,017,153 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| tinytemplate    | 1,021,356,010 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| warden          |    13,543,032 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |

## Instructions relative to Rust

Ratios for unaudited implementations are informational comparisons of validated output.

| Benchmark       |    Darklang |        Rust |     Haskell |      Python |        Node |         Roc |     OCaml 5 |        Koka | Darklang interpreter |
| --------------- | ----------: | ----------: | ----------: | ----------: | ----------: | ----------: | ----------: | ----------: | -------------------: |
| ackermann       | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| binary_trees    | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| collatz         | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| edigits         | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| factorial       | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| fannkuch        | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| fasta           | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| fft             | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| fib             | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| huffman         | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| leibniz         | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| mandelbrot      | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| matmul          | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| merkletrees     | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| myers_diff      | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| nbody           | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| nqueen          | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| nsieve          | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| pisum           | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| primes          | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| quicksort       | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| raytracer       | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| regex_lite      | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| spectral_norm   | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| string_equality | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| sum_to_n        | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| tak             | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| tinytemplate    | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |
| warden          | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable | unavailable |          unavailable |

## Aggregate comparisons

Fixed workload sets keep averages comparable as language coverage grows.
Geometric means are informational unless every included row is audited.

### Original 21 workloads

`ackermann`, `binary_trees`, `collatz`, `edigits`, `factorial`, `fannkuch`, `fasta`, `fib`, `leibniz`, `mandelbrot`, `matmul`, `merkletrees`, `nbody`, `nqueen`, `nsieve`, `pisum`, `primes`, `quicksort`, `spectral_norm`, `sum_to_n`, `tak`.

| Language             | Instructions / Rust | Total instructions | Current rows | Parity     |
| -------------------- | ------------------: | -----------------: | -----------: | ---------- |
| Darklang             |         unavailable |        unavailable |         0/21 | incomplete |
| Rust                 |         unavailable |        unavailable |         0/21 | incomplete |
| Haskell              |         unavailable |        unavailable |         0/21 | incomplete |
| Python               |         unavailable |        unavailable |         0/21 | incomplete |
| Node                 |         unavailable |        unavailable |         0/21 | incomplete |
| Roc                  |         unavailable |        unavailable |         0/21 | incomplete |
| OCaml 5              |         unavailable |        unavailable |         0/21 | incomplete |
| Koka                 |         unavailable |        unavailable |         0/21 | incomplete |
| Darklang interpreter |         unavailable |        unavailable |         0/21 | incomplete |

### All 29 workloads

`ackermann`, `binary_trees`, `collatz`, `edigits`, `factorial`, `fannkuch`, `fasta`, `fft`, `fib`, `huffman`, `leibniz`, `mandelbrot`, `matmul`, `merkletrees`, `myers_diff`, `nbody`, `nqueen`, `nsieve`, `pisum`, `primes`, `quicksort`, `raytracer`, `regex_lite`, `spectral_norm`, `string_equality`, `sum_to_n`, `tak`, `tinytemplate`, `warden`.

| Language             | Instructions / Rust | Total instructions | Current rows | Parity     |
| -------------------- | ------------------: | -----------------: | -----------: | ---------- |
| Darklang             |         unavailable |        unavailable |         0/29 | incomplete |
| Rust                 |         unavailable |        unavailable |         0/29 | incomplete |
| Haskell              |         unavailable |        unavailable |         0/29 | incomplete |
| Python               |         unavailable |        unavailable |         0/29 | incomplete |
| Node                 |         unavailable |        unavailable |         0/29 | incomplete |
| Roc                  |         unavailable |        unavailable |         0/29 | incomplete |
| OCaml 5              |         unavailable |        unavailable |         0/29 | incomplete |
| Koka                 |         unavailable |        unavailable |         0/29 | incomplete |
| Darklang interpreter |         unavailable |        unavailable |         0/29 | incomplete |
