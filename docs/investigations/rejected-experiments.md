# Rejected Optimization Experiments

This file records benchmark optimization candidates that were investigated and removed from active investigation notes because measured evidence did not justify keeping the implementation.

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
