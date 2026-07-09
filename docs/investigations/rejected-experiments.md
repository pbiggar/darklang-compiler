# Rejected Optimization Experiments

This file records benchmark optimization candidates that were investigated and removed from active investigation notes because measured evidence did not justify keeping the implementation.

## 2026-07-08: fasta general float division strength reduction

- Source candidate: `docs/investigations/benchmark-fasta-optimization.md`, "Float Division Strength Reduction"
- Target benchmark: `fasta`
- Attempt: inspected the ANF optimizer's current float strength-reduction rules and the fasta candidate's proposed replacement of division by `139968.0` with multiplication by its reciprocal.
- Correctness evidence: existing ANF optimizer coverage already proves the intentionally narrow exact-power-of-two cases (`x / 2.0`, `x / -2.0`, `x / 4.0`, and related powers) are rewritten to multiplication by exactly representable reciprocals.
- IR evidence: `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` limits float division strength reduction to exact powers of two and their negatives; `src/Tests/optimization/anf.opt` covers those cases.
- Runtime evidence: no implementation was kept, so benchmark evidence is verification-only for the documentation cleanup.
- Compile-time evidence: no compiler implementation was kept, so compile-time evidence is verification-only for the documentation cleanup.
- Reason rejected: the active candidate asked for general floating-point division by constant to become multiplication by a reciprocal such as `1.0 / 139968.0`; that transformation is not generally semantics-preserving under IEEE-754 rounding and special values, while the safe exact-power-of-two subset is already implemented.
- Outcome: no compiler code was changed; the active candidate was removed from the source investigation file.

## 2026-07-08: pisum adjacent post-RA move cleanup

- Source candidate: `docs/investigations/benchmark-pisum-optimization.md`, "Eliminate Redundant Register Moves (Post-RA Cleanup)"
- Target benchmark: `pisum`
- Attempt: added post-register-allocation LIR cleanup for adjacent overwritten integer moves, adjacent overwritten float moves, and floating-point self-moves, then wired register allocation to use the broader cleanup.
- Correctness evidence: a focused `LIRPeepholeTests` case failed before the helper existed and passed after implementation; full `./run-tests --ai` passed.
- IR evidence: `./dark --dump-lir benchmarks/problems/pisum/dark/main.dark` still showed useful `D0 <- FMov(D1)` traffic but no longer exposed the removed adjacent-overwrite pattern in the focused helper.
- Runtime evidence: `./benchmarks/run_benchmarks.sh all` completed with no failures, but `pisum` remained at `65,014,671` Dark instructions and the full benchmark table stayed at performance ratio `5.55x`.
- Compile-time evidence: `TIMEFORMAT='compile wall: %3R s'; time ./dark benchmarks/problems/pisum/dark/main.dark -o /tmp/pisum-post-ra-cleanup -q` reported `4.731 s`; no before/after compile-time benefit was established.
- Reason rejected: the implementation improved a local LIR cleanup helper but did not change emitted instruction counts for the target benchmark, so the measured benefit did not justify keeping code.
- Outcome: implementation and focused test were reverted; the active source investigation entry was narrowed to the remaining phi-copy coalescing opportunity.

## 2026-07-05: ackermann early raw-MIR unreachable block pruning

- Source candidate: `docs/investigations/benchmark-ackermann-optimization.md`, "Remove Unreachable MIR Blocks Before LIR"
- Target benchmark: `ackermann`
- Attempt: added program-level raw-MIR unreachable block pruning immediately after ANF-to-MIR conversion so `ackermann_L5` disappeared from `--dump-mir` before SSA construction and LIR lowering.
- Correctness evidence: the focused program-level MIR prune test failed before the helper existed and passed after implementation; full `./run-tests --ai` passed.
- IR evidence: `./dark --dump-mir benchmarks/problems/ackermann/dark/main.dark` no longer printed `ackermann_L5` with the attempted implementation.
- Runtime evidence: `./benchmarks/run_benchmarks.sh all` completed with default serialized jobs, but `ackermann` regressed from `11,450,298,027` to `11,808,113,697` Dark instructions.
- Compile-time evidence: `./dark -vv benchmarks/problems/ackermann/dark/main.dark -o /tmp/ackermann-prune-dark.out` reported compilation complete in `148.9ms`; no before/after compile-time win justified the runtime regression.
- Reason rejected: pruning before SSA changed downstream allocation/code shape enough to regress the target benchmark while only improving MIR dump clarity.
- Outcome: implementation and focused test were reverted; the active candidate was removed from the source investigation file.

## 2026-07-02: sum_to_n post-register-allocation self-move elimination

- Source candidate: `docs/investigations/benchmark-sum_to_n-optimization.md`, "Redundant Move Elimination in LIR"
- Target benchmark: `sum_to_n`
- Attempt: added a focused register-allocation test for physical self-moves, then added a post-allocation cleanup removing `Mov(dest, Reg src)` where `dest = src`.
- Correctness evidence: the focused test failed before the cleanup with `(Label "sumTo_L1", X3)` and passed after the cleanup; full `./run-tests --quiet` passed.
- IR evidence: after rebuilding the compiler, `./dark --dump-lir benchmarks/problems/sum_to_n/dark/main.dark -o /tmp/sum_to_n_probe_after -q` no longer showed the `sumTo_L1` and `repeat_L1` self-moves in post-allocation LIR.
- Runtime evidence: `./benchmarks/run_benchmarks.sh sum_to_n` reported Dark `7,002,526` instructions, unchanged from `benchmarks/RESULTS.md`.
- Compile-time evidence: timestamp measurement for compiling `benchmarks/problems/sum_to_n/dark/main.dark` reported `2.310s`; no before/after compile-time benefit was established.
- Reason rejected: ARM64 codegen already removes `MOV Xn, Xn` in `peepholeOptimize`, so the allocator cleanup improves LIR dumps but does not reduce emitted instructions for the target benchmark.
- Outcome: implementation and focused test were reverted; the active candidate was removed from the source investigation file.

## 2026-07-09: nqueen post-register-allocation self-move elimination

- Source candidate: `docs/investigations/benchmark-nqueen-optimization.md`, "Redundant Self-Move Elimination (Post-Register Allocation)"
- Target benchmark: `nqueen`
- Attempt: pre-checked the current generated LIR, ARM64 codegen, and generated-code tests before implementing another allocator cleanup.
- Correctness evidence: `src/Tests/compiler-passes/ARM64CodeGenTests.fs` has a focused generated-code regression test that feeds integer and floating-point physical self-moves through `CodeGen.generateARM64` and fails if the symbolic ARM64 output contains `MOV_reg(dest, src)` or `FMOV_reg(dest, src)` with `dest = src`. `src/Tests/compiler-passes/ParallelMoveTests.fs` also covers `TailArgMoves` self-move elimination.
- IR evidence: post-allocation LIR self-move markers can still appear in dumps, but `src/DarkCompiler/passes/arm64/6_CodeGen.fs` suppresses emitted integer self-moves whose destination and source lower to the same ARM64 register, and the generated-code test validates that these self-moves are absent from the ARM64 instruction stream.
- Runtime evidence: no new implementation was kept. The earlier `sum_to_n` post-register-allocation self-move cleanup proved this class of cleanup changes LIR dumps without reducing emitted ARM64 instructions, because codegen already suppresses `MOV Xn, Xn`.
- Compile-time evidence: no compiler implementation was kept, so compile-time evidence is verification-only for the documentation cleanup.
- Reason rejected: implementing a second post-allocation self-move cleanup for `nqueen` would add compiler-pass complexity for an emitted-code optimization that is already handled during ARM64 code generation and covered by generated-code tests.
- Outcome: the active candidate had already been removed from the source investigation file; this record preserves the generated-code validation evidence so the candidate is not reselected.
