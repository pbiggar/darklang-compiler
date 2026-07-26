# Rejected Optimization Experiments

This file records benchmark optimization candidates that were investigated and removed from active investigation notes because measured evidence did not justify keeping the implementation.

## 2026-07-20: primes positive-divisor modulo simplification

- Source candidate: `docs/investigations/benchmark-primes-optimization.md`, "Prove positive divisor ranges to remove general modulo correction"
- Target benchmark: `primes`
- Attempt: inspected the current optimizer and MIR-to-LIR modulo lowering to determine whether the benchmark-specific fact that `isDivisible` starts at divisor `3` can be used as a narrow local optimization.
- Correctness evidence: no implementation was kept. Existing modulo strength reduction is limited to constant divisors, with tests documenting semantic risk for negative dividends; there is no range-analysis IR state proving a variable divisor is positive at a modulo site.
- IR evidence: `src/DarkCompiler/passes/4_MIR_to_LIR.fs` lowers `MIR.Mod` through `shouldCheckNegativeDivisor`, `selectBlocksWithModuloChecks`, and `buildIntegerModuloParts`, while repository search found no positive-range analysis or nonnegative-value facts available to that lowering.
- Runtime evidence: no compiler implementation was kept, so benchmark evidence is verification-only for the documentation cleanup.
- Compile-time evidence: no compiler implementation was kept, so compile-time evidence is verification-only for the documentation cleanup.
- Commands run: `./run-tests --ai` passed; `./benchmarks/run_benchmarks.sh all` passed and reported Dark performance ratio `11.1x`.
- Reason rejected: this is not a narrow modulo-lowering optimization; it depends on adding and maintaining interprocedural or recursive-loop range facts before LIR lowering. Implementing an unsound benchmark-specific shortcut would risk changing Dark modulo semantics, and a correct range analysis is broader than this candidate's medium-complexity active entry suggests.
- Outcome: no compiler code was changed; the active candidate was removed from the source investigation file so future work can focus on the remaining square-root and helper-inlining opportunities.

## 2026-07-25: primes hardware-backed integer square root

- Source candidate: `docs/investigations/benchmark-primes-optimization.md`, "Replace recursive integer square root with a faster primitive"
- Target benchmark: `primes`
- Attempt: inspected the benchmark source, current stdlib intrinsics, current float intrinsic lowering, and the optimization path required to replace the recursive `isqrt` helper with a hardware-backed integer square-root operation.
- Correctness evidence: no implementation was kept. The existing language surface exposes `Stdlib.Float.sqrt`, `Stdlib.Int64.toFloat`, and `Stdlib.Float.toInt`, but not an integer square-root primitive with documented rounding and negative-input semantics.
- IR evidence: the benchmark still contains a source-level recursive `isqrt(n, guess)` helper; replacing it with hardware sqrt would require adding or committing to a new public `Int64` primitive or a benchmark-specific semantic rewrite rather than a localized optimizer cleanup.
- Runtime evidence: no compiler implementation was kept, so runtime evidence is verification-only for the documentation cleanup.
- Compile-time evidence: no compiler implementation was kept, so compile-time evidence is verification-only for the documentation cleanup.
- Reason rejected: the candidate is a real algorithmic gap, but as written it is a feature/API design task rather than a small optimization experiment. Adding a new integer sqrt primitive would need language and stdlib semantics review before it belongs in the optimization implementer queue.
- Outcome: no compiler code was changed; the active source investigation entry was removed so future optimization trials focus on localized compiler work such as positive-divisor modulo simplification or helper inlining.

## 2026-07-20: ackermann nested-recursion register allocation re-check

- Source candidate: `docs/investigations/benchmark-ackermann-optimization.md`, "Re-check Register Allocation Around Nested Recursion"
- Target benchmark: `ackermann`
- Attempt: pre-checked the current allocated LIR around the nested recursive call before implementing a register-allocation change.
- Correctness evidence: existing Ackermann benchmark E2E coverage in `src/Tests/e2e/benchmarks.e2e` covers the nested-recursive source shape through `ackermann(3, 3) = 61`; no compiler implementation was kept.
- IR evidence: `./dark --dump-lir benchmarks/problems/ackermann/dark/quick.dark` shows `X21 <- Sub(X20, Imm 1)` before the inner call, followed by `X19 <- Call(ackermann, [Reg X20, Reg X19])`, `X19 <- Mov(Reg X0)`, and `X20 <- Mov(Reg X21)`. The preserved `X21` value is the outer `m - 1` needed after the inner call returns, so the local register traffic is not a redundant cleanup opportunity.
- Runtime evidence: no implementation was kept, so benchmark evidence is verification-only for the documentation cleanup.
- Compile-time evidence: no compiler implementation was kept, so compile-time evidence is verification-only for the documentation cleanup.
- Reason rejected: the active note was a re-check rather than an absent optimization, and the concrete LIR evidence shows the suspect preservation is required for the nested-recursive control flow.
- Outcome: no compiler code was changed; the active candidate was removed from the source investigation file and the evidence was preserved here.

## 2026-07-20: LIR empty SaveRegs/RestoreRegs cleanup

- Source candidate: `docs/investigations/benchmark-ackermann-optimization.md`, empty `SaveRegs([], [])` and `RestoreRegs([], [])` placeholders visible in the nested recursive call LIR.
- Target benchmark: `ackermann`
- Attempt: changed the target-independent LIR peephole pass to remove empty `SaveRegs` and `RestoreRegs` instructions, and updated `src/Tests/optimization/lir.opt` so the call-loop LIR expectation no longer contained those placeholders.
- Correctness evidence: the focused optimization expectation failed before the implementation because the optimized LIR still contained the empty placeholders. After the attempted implementation, the optimization tests passed but full `./run-tests --ai` failed with 458 E2E failures, mostly exit code 139 in list and pattern-matching programs.
- IR evidence: the attempted implementation removed the empty placeholders from optimized LIR around calls such as `ArgMoves; Call`, but this changed later code generation behavior rather than only cleaning the dump.
- Runtime evidence: no benchmark run was performed because the attempted implementation failed correctness validation and was not safe to measure.
- Compile-time evidence: no compile-time comparison was performed because the attempted implementation failed correctness validation.
- Reason rejected: empty save/restore placeholders are not safe to remove in the target-independent LIR peephole stage; ARM64 call-argument lowering uses the call-save boundary even when no registers are saved.
- Outcome: implementation and test-expectation changes were reverted; future work should only remove these placeholders after call-argument lowering no longer depends on them, or in a backend-local stage where that invariant is explicit.

## 2026-07-10: fasta dedicated heap-end register

- Source candidate: `docs/investigations/benchmark-fasta-optimization.md`, "Hoist heap end into a dedicated register"
- Target benchmark: `fasta`
- Attempt: initialized `X26` as a process-wide heap-end register in ARM64 heap setup, changed allocation bounds checks to compare against `X26`, and removed `X26` from ARM64 register allocation so generated code would preserve it.
- Correctness evidence: a focused `ParallelMoveTests` assertion failed before the implementation because `RawAlloc` still compared against recomputed `X11`, then passed after the implementation; full `./run-tests --ai` passed after the attempted code change.
- IR evidence: the attempted codegen path removed the `MOVZ X11, #0x2000, LSL #16` and `ADD X11, X27, X11` heap-end recomputation from per-allocation bounds checks, replacing the compare with `CMP X14, X26`.
- Runtime evidence: `./benchmarks/run_benchmarks.sh all` completed, but the target benchmark regressed from `2,495,827,474` to `2,534,045,119` Dark instructions. Other regressions included `fannkuch` from `15,997,493,901` to `16,116,552,677`, `nqueen` from `304,488,643` to `322,893,353`, and `matmul` from `1,661,903,645` to `1,663,926,059`.
- Compile-time evidence: `time ./dark benchmarks/problems/fasta/dark/main.dark -o /tmp/dark-fasta-hoist -q` completed in `3.775s` wall time during the attempted implementation.
- Reason rejected: reserving `X26` reduced register-allocation flexibility enough to outweigh the two-instruction per-allocation saving, including a clear regression in the target benchmark.
- Outcome: implementation and focused test changes were reverted; the active source investigation entry was rewritten so future work does not retry a dedicated allocatable-register heap-end hoist.

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
