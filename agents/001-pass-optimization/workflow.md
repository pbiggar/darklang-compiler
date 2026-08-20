# Pass Optimization Workflow

## Durable Contract

Reduce the compiler-reported time of exactly one compiler pass on one
representative full-size benchmark while preserving generated output. An
attempt is a bounded experiment: its valid outcome is either one measured,
review-ready candidate or a documented negative result with all exploratory
changes removed.

This contract is grounded in the current 493-match inventory and representative
SSA work: definition-summary reuse (`8f76577b93`), reachability queue removal
(`b8bf0a5467`), the restarted operand-folding analysis (`75e9bbabd3`), and the
later DCE traversal fusion (`03b4a79088`). Together with the reported edigits
SSA Construction run (6,035,750 input tokens, 77 commands), these show the
repeated loop: rank measured passes; inspect source and history; identify a
repeated traversal, allocation, or collection operation; pin a baseline; try a
small structural alternative; reject duplicate, noisy, slower, or semantically
different variants; validate the retained variant.

## Inputs

- A clean repository revision and detected architecture.
- One explicitly named full-size workload; the current baseline workload is
  `benchmarks/problems/edigits/dark/main.dark`.
- Compiler `-vv` output whose stage labels map to `CompilerLibrary.fs`.
- The selected pass, its direct timing caller, nearby tests and documentation,
  plus accepted and rejected history for the proposed idea.
- An explicit artifact directory below `.dcb/tool-artifacts/` for complete raw
  output, timing samples, fingerprints, and search results.

## Repeated Decisions And Searches

1. Rank only recognized, adjacent `-vv` stage/timing pairs. Current local
   evidence outranks historical nominations.
2. Search the selected pass, its caller, focused tests, documentation, and
   relevant `git log/show` history. Stop a hypothesis already implemented or
   rejected when its rejection conditions still hold.
3. Name the repeated traversal, persistent-collection operation, allocation,
   lookup, or fixed-point update and its expected complexity/allocation change.
4. Prefer one local representation or traversal change that preserves IR order
   and semantics. Never bundle independent hypotheses.
5. Retain only a clear selected-pass timing win with byte-identical executable
   output, explainable normalized IR, and passing focused checks. A neutral
   refactor is a negative result unless a human explicitly requests it.

## Procedure

1. Run the registered inventory tool first. Its timing parser must recognize
   integer and decimal stages, require the immediately following timing line,
   normalize arrow spelling and display suffixes, and reject unknown labels.
2. Select one pass and one exposing workload; record eligibility and revision.
3. Run the inspect tool and rank at most three hypotheses by likely cost and
   semantic risk.
4. Fingerprint the baseline executable and normalized requested IR. Warm up
   once, then save three selected-pass timing samples.
5. Implement and measure one hypothesis at a time. Performance-only work uses
   the pre-change measurement as failing evidence; behavior changes require a
   failing end-to-end test first.
6. Run focused verification with the same command and sampling protocol. It
   compares timing, executable bytes, normalized IR, and named focused checks.
7. Remove profiling and exploratory changes. Only for a retained candidate,
   read `docs/verification.md`, run `./run-tests --ai` once and
   `./benchmarks/run_benchmarks.sh --verify routine` once, and report the
   `RESULTS.md` performance ratio.
8. Commit one coherent retained candidate. Never merge, land, or push without
   explicit human authorization.

## Exact Stop Limits

- One pass, one workload, at most three implementation hypotheses, and at most
  one retained production candidate per attempt.
- At most two hypotheses may receive temporary micro-profiling, once each.
- Every timing set is exactly one warmup plus three measured runs. If ambiguous,
  allow one final interleaved confirmation of five baseline/candidate pairs;
  never continue sampling after it.
- Focused commands stop after 10 minutes. A full test or routine benchmark gate
  stops after 30 minutes. Record the command, exit status, and last artifact;
  do not silently retry.
- Stop immediately on parser/label mismatch, build or focused-test failure,
  changed executable bytes, unexplained normalized-IR change, or a selected-pass
  regression beyond the noise band.
- Stop negative after three rejected hypotheses, an ambiguous confirmation, or
  absence of a current measurable pass timing. Revert every exploratory compiler
  and test change.
- Broad tests and routine benchmarks run only for the retained candidate, never
  as a repeated timing benchmark.

A timing result is clear only when the candidate median improves and its
absolute delta exceeds `max(1.0ms, 3 * pooled_MAD)`, where `pooled_MAD` is the
larger median absolute deviation of the baseline and candidate samples.
Correctness and output equality remain mandatory regardless of timing.

## Failure And Negative-Result Behavior

Classify rejection as duplicate prior work, invalid evidence, build/test
failure, output/IR mismatch, regression, neutral/noisy timing, or disproportionate
complexity. Preserve the pass, revision, exact command, samples, artifact paths,
hypothesis, stop reason, and next eligible hypothesis. Revert the whole rejected
patch; never weaken tests, switch workloads, or update expected output to obtain
a pass. Do not spend broad-gate resources on a focused rejection.

## Outputs

Each tool prints concise structured JSON below 4096 bytes and links a complete
artifact. A win reports the selected pass/workload, commands, samples, medians,
absolute and percentage deltas, fingerprints, focused and broad validation,
benchmark ratio, rationale, and residual risk. A rejection reports the same
identity and measurement fields plus its stop reason and proof of cleanup.

## Tool-Building Inventory

The next phase must build these repository-backed roles behind
`scripts/agent-tool pass-optimization`:

1. `inventory`: upgrade the existing static search to discover timing labels
   from `CompilerLibrary.fs`, run or ingest full-size `-vv` samples, validate
   stage/timing adjacency, rank recognized passes, and retain raw output.
2. `inspect --pass PASS --benchmark PATH`: read the pass, timing caller, focused
   tests, documentation, commit/rejection history, and bounded structural hot
   spots. It must not edit or infer a default pass or benchmark.
3. `verify --pass PASS --benchmark PATH --baseline REF --candidate REF`: create
   isolated inputs, enforce the sampling limits, compute medians and pooled MAD,
   compare executable bytes and normalized IR, run explicitly named focused
   checks, and return `retain`, `reject`, or `invalid`. It must not run broad
   gates or modify the active worktree.

All roles require explicit flags, return nonzero for invalid evidence, cap
stdout at 4096 bytes, and retain complete evidence below `.dcb/tool-artifacts/`.
