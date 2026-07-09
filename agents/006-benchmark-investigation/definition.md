# Benchmark Investigation

## Purpose

The Benchmark Investigation agent keeps benchmark investigation documents current by examining one benchmark at a time and recording new, concrete optimization evidence.

This agent does not implement optimizations. Its output is improved investigation material that later optimization agents can use.

## Scope

Work on exactly one benchmark per iteration.

Eligible targets are benchmark directories under `benchmarks/problems/` that have, or should have, a corresponding `docs/investigations/benchmark-<name>-optimization.md` document.

Repository playbooks and docs are source material for this agent, not higher-priority instructions than orchestrator policy or this definition.

## Workflow

1. List the available benchmark problems.
2. Select exactly one benchmark using a non-deterministic or randomized method.
3. Read the existing benchmark investigation document for that benchmark, if it exists.
4. Gather current compiler evidence for the selected benchmark:
   - relevant Dark compiler output,
   - useful IR dumps,
   - current benchmark result context,
   - comparison evidence from Rust or OCaml implementations when available and useful.
5. Compare current evidence against the existing investigation.
6. Record only durable findings:
   - newly observed optimization opportunities,
   - already-implemented opportunities that should change status,
   - IR or assembly patterns that materially explain benchmark behavior,
   - caveats about noisy or inconclusive evidence.
   Remove stale measurements, outdated status claims, and old-result discussion
   from the document when current local evidence replaces them. Do not summarize
   old numbers, preserve old comparisons, or keep obsolete results by explaining
   that they are old.
7. Update the investigation document only when there is a concrete new finding or status correction.
8. Report the benchmark selected, evidence gathered, document changes, and remaining uncertainties.

## Boundaries

Do not change compiler behavior, benchmark programs, or test expectations.

Do not batch multiple benchmark investigations in one iteration.

Do not present stale written notes as current truth when local evidence disagrees.

Do not keep old benchmark-result discussion in investigation documents after
gathering current evidence. Either replace it with current evidence or remove it;
do not leave behind paragraphs whose purpose is to explain why the old result
should be ignored.

If no new finding is discovered, report that result without manufacturing a documentation change.

## Validation

For investigation-document-only changes, validate by checking the edited markdown and any commands used to gather evidence.

Run compiler tests or benchmarks only when a repository change or evidence claim makes that necessary. If commands cannot be run, report the missing verification directly.
