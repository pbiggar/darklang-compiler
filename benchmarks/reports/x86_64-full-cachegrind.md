# x86_64-full-cachegrind comparison

Architecture: `x86_64`. Profile: `full`.

Measurement policy: `cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs`.

Current workload contract: `2a3f42ac1a2085fe75562c15a454d4ec18429c61d8f22ee22c9619d5d0ef029f`.

Instruction counts include runtime and startup work; they are not elapsed-time speed ratios.

Snapshots are independent. Stale values are retained for provenance and excluded from ratios.

## Stored versions and coverage

| Language             | Version / compiler commit                | Measured                  | Current rows | Audited current rows | Build flags  | Runtime flags |
| -------------------- | ---------------------------------------- | ------------------------- | ------------ | -------------------- | ------------ | ------------- |
| Darklang             | a8f74665d1a6b33228c2b85ed3c6ff93ee4e71cf | 2026-10-07T11:50:03+00:00 | 0/29         | 0/0                  | not recorded | not recorded  |
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

| Benchmark       |                              Darklang |         Rust |      Haskell |       Python |         Node |          Roc |      OCaml 5 |         Koka | Darklang interpreter |
| --------------- | ------------------------------------: | -----------: | -----------: | -----------: | -----------: | -----------: | -----------: | -----------: | -------------------: |
| ackermann       | 184,178,128 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| binary_trees    |  12,916,641 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| collatz         | 185,401,083 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| edigits         | 123,233,687 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| factorial       |      63,446 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fannkuch        | 435,308,298 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fasta           |  23,431,795 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fft             |   2,465,221 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| fib             |   9,856,188 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| huffman         | 687,600,258 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| leibniz         |  43,009,535 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| mandelbrot      |  21,677,641 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| matmul          |  71,903,921 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| merkletrees     |   3,775,096 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| myers_diff      | 284,156,308 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| nbody           |  11,878,516 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| nqueen          |  10,714,474 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| nsieve          | 504,794,934 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| pisum           |   1,511,980 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| primes          |   3,692,310 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| quicksort       | 431,665,621 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| raytracer       |  66,754,325 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| regex_lite      | 108,258,533 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| spectral_norm   | 687,911,699 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| string_equality |   1,496,122 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| sum_to_n        |      61,844 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| tak             |  56,735,194 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| tinytemplate    | 461,239,820 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |
| warden          |  61,010,185 (stale workload contract) | not measured | not measured | not measured | not measured | not measured | not measured | not measured |         not measured |

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
