# x86_64-quick-qemu comparison

Architecture: `x86_64`. Profile: `quick`.

Measurement policy: `qemu-tcg-plugin-guest-insns-v1:qemu-11.1.1:rustc-1.89.0`.

Current workload contract: `fbf9d7000df1263214d3375d12e0bb57ed9c8d565fc45ed7fcb499b927c71b39`.

Instruction counts include runtime and startup work; they are not elapsed-time speed ratios.

Snapshots are independent. Stale values are retained for provenance and excluded from ratios.

## Stored versions and coverage

| Language             | Version / compiler commit                | Measured                         | Current rows | Audited current rows | Build flags  | Runtime flags |
| -------------------- | ---------------------------------------- | -------------------------------- | ------------ | -------------------- | ------------ | ------------- |
| Darklang             | e7954cd317a4e9df891d8ccf93a03e47c243766c | 2026-09-15T20:45:50.212199+00:00 | 0/29         | 0/0                  | not recorded | not recorded  |
| Rust                 | a83739b5da5f8a013876fd478cad7b71877f8a6e | 2026-09-15T20:39:24.365696+00:00 | 0/29         | 0/0                  | not recorded | not recorded  |
| Haskell              | not recorded                             | not recorded                     | 0/29         | 0/0                  | not recorded | not recorded  |
| Python               | not recorded                             | not recorded                     | 0/29         | 0/0                  | not recorded | not recorded  |
| Node                 | not recorded                             | not recorded                     | 0/29         | 0/0                  | not recorded | not recorded  |
| Roc                  | not recorded                             | not recorded                     | 0/29         | 0/0                  | not recorded | not recorded  |
| OCaml 5              | not recorded                             | not recorded                     | 0/29         | 0/0                  | not recorded | not recorded  |
| Koka                 | not recorded                             | not recorded                     | 0/29         | 0/0                  | not recorded | not recorded  |
| Darklang interpreter | not recorded                             | not recorded                     | 0/29         | 0/0                  | not recorded | not recorded  |

Audited means the implementation's algorithm and source have a current reviewed parity contract.
Output validation alone does not establish algorithm parity.

## Absolute instructions

| Benchmark       |                                 Darklang |                                 Rust |      Haskell |       Python |         Node |          Roc |      OCaml 5 |         Koka | Darklang interpreter |
| --------------- | ---------------------------------------: | -----------------------------------: | -----------: | -----------: | -----------: | -----------: | -----------: | -----------: | -------------------: |
| ackermann       |      2,849,810 (stale workload contract) |  1,751,820 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| binary_trees    |      1,278,194 (stale workload contract) |  4,497,789 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| collatz         |        926,709 (stale workload contract) |    840,205 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| edigits         |      6,698,480 (stale workload contract) |    318,369 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| factorial       |         17,787 (stale workload contract) |    298,180 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fannkuch        |        845,201 (stale workload contract) |    237,180 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fasta           |     11,912,187 (stale workload contract) |    319,196 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fft             |    528,590,727 (stale workload contract) |    492,853 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fib             |        346,092 (stale workload contract) |    510,153 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| huffman         |     27,214,753 (stale workload contract) |    390,542 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| leibniz         |      2,158,704 (stale workload contract) |    988,845 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| mandelbrot      |      1,378,291 (stale workload contract) |  1,253,128 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| matmul          |         74,981 (stale workload contract) |    297,381 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| merkletrees     |        602,837 (stale workload contract) |    648,332 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| myers_diff      |  3,792,789,291 (stale workload contract) |    529,042 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| nbody           |        225,954 (stale workload contract) |    350,109 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| nqueen          |        533,649 (stale workload contract) |    690,214 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| nsieve          |      1,155,278 (stale workload contract) |    291,688 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| pisum           |        166,064 (stale workload contract) |    414,545 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| primes          |        174,137 (stale workload contract) |    382,856 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| quicksort       |        697,448 (stale workload contract) |    307,853 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| raytracer       |      2,903,079 (stale workload contract) |    432,190 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| regex_lite      |     31,947,900 (stale workload contract) |    285,453 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| spectral_norm   |      1,693,382 (stale workload contract) |    298,183 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| string_equality |        810,743 (stale workload contract) |    743,341 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| sum_to_n        |         64,787 (stale workload contract) |    290,038 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| tak             |     54,879,270 (stale workload contract) | 42,395,241 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| tinytemplate    | 10,486,538,239 (stale workload contract) |    670,683 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| warden          |     20,514,739 (stale workload contract) |    301,241 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |

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
