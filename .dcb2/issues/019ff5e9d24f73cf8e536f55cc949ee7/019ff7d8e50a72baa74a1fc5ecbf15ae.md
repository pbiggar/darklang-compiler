---
format: 1
event-id: 019ff7d8e50a72baa74a1fc5ecbf15ae
entity-id: 019ff5e9d24f73cf8e536f55cc949ee7
entity-kind: issue
event-type: revised
occurred-at: 2026-08-12T21:20:14.6024415+00:00
author: human:data-model-migration
previous: 019ff7d76bb671c4818fb5ccf4f8ed2f
batch: 019ff5e9d22f7ffeb8d9f1146a6f56f8
problem: 019ff54dc66279a38e5ae60d59e804a3
workflow: trial-first
---
# Index RC insertion let frames

Lower fasta Reference Count Insertion from the current 12.4 ms median while preserving ownership decisions.

## Constraints

- Limit scope to `sourceParentIsOwnedLocal` frame lookup in `2.5_RefCountInsertion.fs`; retain only one functional TempId-to-frame index after profiling confirms recursive `List.tryFind` walks are material.
- Collect 10 comparable before and after fasta `-vv` Reference Count Insertion timings with raw results, medians, absolute deltas, and percentage deltas; reject neutral or noisy results.
- Report before/after Reference Count Insertion and total-compilation deltas for every routine benchmark program.
- Collect 10 comparable before and after `./run-tests --ai` wall-clock runs with raw results, medians, and deltas.
- Require identical RC-annotated ANF and binaries, pass `./run-tests --ai` and `./benchmarks/run_benchmarks.sh --verify routine`, and report instruction counts and the RESULTS.md performance ratio.
- Remove all exploratory profiling and instrumentation from retained code.
