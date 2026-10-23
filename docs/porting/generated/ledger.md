# Current compiler port status

[How to read and update this ledger](../README.md).

Interpreter revision: `e76430be5cd9f6b77ed255430dc34a1b58dd8fc4`.

The complete corpus contains 433 source files. Items are counted directly from parsed source annotations.

A run disposition means selected, not a passing execution receipt. Replacement contracts are checked independently.

| Area | Source dispositions |
| --- | --- |
| packages | 3,240 blocked, 2 excluded, 35 replace, 426 run |
| stdlib | 848 blocked, 3 excluded, 1,142 replace, 5 run |
| tests | 16 blocked, 1,484 run, 859 xfail |

| Kind | Items |
| --- | --- |
| assertion | 239 |
| expression | 2 |
| function | 4,460 |
| implementation | 214 |
| named-test | 2,120 |
| trait | 21 |
| trait-method | 25 |
| type | 707 |
| val | 272 |

## Reasons

Tags are defined in [the glossary](../../../porting/compiler-reasons.json).

| Area | Tag | Items |
| --- | --- | --- |
| packages | `compiler-contract` | 12 |
| packages | `compiler-gap` | 7 |
| packages | `compiler-implementation` | 34 |
| packages | `constructor-identity` | 1 |
| packages | `container-adaptation` | 33 |
| packages | `missing-implementation` | 3,023 |
| packages | `runtime-reflection` | 2 |
| packages | `test-counter` | 1 |
| packages | `test-diagnostics` | 1 |
| packages | `upstream-dependency` | 161 |
| packages | `value-specialization` | 2 |
| stdlib | `compiler-contract` | 6 |
| stdlib | `compiler-implementation` | 1,139 |
| stdlib | `container-adaptation` | 490 |
| stdlib | `interpreter-intrinsic` | 1 |
| stdlib | `missing-implementation` | 350 |
| stdlib | `runtime-reflection` | 3 |
| stdlib | `test-counter` | 2 |
| stdlib | `test-diagnostics` | 2 |
| tests | `compiler-crash` | 1 |
| tests | `compiler-gap` | 262 |
| tests | `compiler-resource` | 3 |
| tests | `compiler-timeout` | 12 |
| tests | `test-counter` | 7 |
| tests | `test-diagnostics` | 2 |
| tests | `upstream-dependency` | 588 |

## Separate capability assessments

| Area | Assessments |
| --- | --- |
| features | 6 excluded, 9 missing, 1 partial, 199 supported |
| parser | 1 partial, 30 supported |
| builtins | 169 missing, 79 present, 533 unassessed |
