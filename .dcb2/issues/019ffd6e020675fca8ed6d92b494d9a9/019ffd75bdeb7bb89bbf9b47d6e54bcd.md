---
format: 1
event-id: 019ffd75bdeb7bb89bbf9b47d6e54bcd
entity-id: 019ffd6e020675fca8ed6d92b494d9a9
entity-kind: issue
event-type: trial-result
occurred-at: 2026-08-13T23:29:39.8194382+00:00
author: worker:65eb66d2e522:3700365:019ffd6f885f73a9bba19cf201c3da12
previous: 019ffd6e0208797ca3aa36bee97c216e
attempt: 019ffd7133ab7f7589ad386eefa68132
constraints-hash: aee8208dcc7bd6f671db44238b83fa1e1a3c89cc0573e168ff168891d356eb21
result: no-improvement
revision: 019ffd6e0208797ca3aa36bee97c216e
---
# Trial result

Profiling showed sibling-list copying contributes only ~0.73% of median fasta AST-to-ANF time, below the predeclared 2% noise threshold. The accumulator trial was discarded before implementation or exhaustive validation.

## Evidence

Pinned baseline 9979123622fa9b81a0caf0458a4a9e2d794ff00a; Linux ARM64, 6 Apple cores, .NET 10.0.203. Six profiling runs: raw AST→ANF 12.2, 10.9, 10.6, 17.0, 12.8, 11.3 ms (median 11.75); raw total compilation 254.2, 249.6, 244.9, 254.8, 264.1, 252.2 ms (median 253.2). Raw timed sibling appends 0.085911, 0.082109, 0.086379, 0.088862, 0.084413, 0.085369 ms (median 0.085640 ms, 0.729% of median AST→ANF, including timing overhead). Calls performed 3,784 appends but copied only 300 prefix cells; tuples performed 163 appends and copied 28 cells. Temporary profiling produced a byte-identical fasta binary (SHA-256 5dfdfe2640e786cf5927728e4165bbef7edc4d11a1b0752dc2bd36c3c52dc) and identical output "830939461". Per the progressive stop rule, no candidate, 10-pair matrices, full tests, or routine benchmark verification were run; therefore no RESULTS.md ratio applies. All instrumentation was removed, no .dcb2 events changed, no commit was created, and the worktree is clean at the pinned baseline.

Item results:
- Measure sibling-binding copies (019ffd6e02067f619bec9b4772a51fcd): neutral — Sibling-binding copies do not materially contribute to fasta AST-to-ANF time.
  Exact attribution: calls copied 300 cells and tuples 28 cells. Median aggregate append time was 0.085640 ms versus 11.75 ms AST→ANF, or 0.729%.
- Trial chunked binding accumulation (019ffd6e020678b099bfeb1d5515c4b8): neutral — Reverse-chunk and difference-list alternatives were discarded at the profiling gate.
  Even eliminating all measured append cost could not reach the predeclared 2% improvement threshold. No accumulator implementation was retained or committed.
