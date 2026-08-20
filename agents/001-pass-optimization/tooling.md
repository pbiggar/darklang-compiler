# Pass Optimization Tooling Plan

## Current Tool

`scripts/agent-tool pass-optimization inventory` was used first during
workflow discovery. It returns a concise static search inventory and writes its
493 full matches to `.dcb/tool-artifacts/`. It does not yet select a pass: it
neither parses `-vv` timings nor maps observed labels to compiler stages.

## Build Order

1. Extend `inventory` without changing its registered command. Add explicit
   benchmark/input flags, source-derived timing labels, strict `-vv` parsing,
   ranked JSON and parser fixtures for decimal stages, adjacency, suffix and
   arrow normalization, unrelated lines, missing pairs, and unknown labels.
2. Add read-only `inspect`, requiring `--pass` and `--benchmark`. Use bounded
   repository and history searches to report source/caller/test/docs locations,
   accepted and rejected hypotheses, and likely repeated traversals or
   collection operations. Keep full matches only in its artifact.
3. Add isolated `verify`, requiring pass, benchmark, baseline, candidate, and
   named focused checks. Reuse the parser, enforce workflow sample limits,
   compute the threshold, compare output fingerprints, and report facts plus a
   contract decision. Broad repository gates remain outside this tool.

## Shared Requirements

- Deterministic parsing and aggregation with explicit typed outcomes; no magic
  string control state.
- No compiler edits, instrumentation, active-worktree mutation, network access,
  or implicit benchmark/pass defaults.
- Structured success and failure JSON below 4096 bytes, with raw stdout,
  stderr, samples, fingerprints, and searches in a uniquely named
  `.dcb/tool-artifacts/pass-optimization-*` directory.
- Record every child command, timeout, exit status, and artifact path.
- Unit fixtures cover parser and decision math. Integration checks use a small
  existing input and never compile the entire standard library.

Tool-building completes only when all three roles are registered in
`agent.json`, help lists every required flag and stop limit, fixtures pass,
invalid or missing evidence exits nonzero, the stdout cap is tested, and a dry
run links complete artifacts without modifying tracked files.
