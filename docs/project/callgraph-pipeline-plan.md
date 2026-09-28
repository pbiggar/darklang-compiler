# Call-graph-directed compilation pipeline

## Implementation status (2026-09-28)

The migration is implemented in one callee-first native driver. It schedules distinct MIR
nodes by direct-call SCC and dependency depth, publishes structured purity,
typed constant returns, and ARM64/x64 clobber summaries, and carries consumed
callee facts into cross-unit dependency cache keys. Recursive and unresolved
calls keep conservative contracts. The old driver, selector, start-lowering
cache, and LIR constant-call list scan have been removed. The final ARM64 full
benchmark gate improved on the task parent (ratio 0.995683) without an
individual regression, and recording advanced the canonical snapshot. The
complete host suite passed 10,734/10,734; the complete x64 suite passed
10,690/10,690. The x64 QEMU quick comparison against the exact task parent
improved (ratio 0.999983; 29/29 programs measured).

On a three-run x64 raytracer compile, the task-parent median was 9.675s and
the new driver's median was 10.369s (7.2% longer). The verbose pass trace
shows seven callee-first batches for nine program functions; repeated batch
setup accounts for part of the additional compiler work. This is a measured
compile-time tradeoff alongside the generated-code improvement, and remains a
target for later scheduler tuning.

During migration, treating possible divergence as a reason to withhold every
MIR call optimization caused large recursive benchmark losses. The production
driver retains the established effect-free proof within a compilation unit,
while cross-unit optimization requires the stronger published purity proof.
Deleting a constant-return call always requires that stronger proof. ARM64
`Call.dest` is excluded from clobbers because its backend emits only `BL` and
a separate result move; x64 includes it because its backend emits the move
inside `Call`.

This plan builds a second native compilation pipeline around explicit function
dependencies. It reuses established frontend, IR, allocation, and backend
passes where possible, then removes the old orchestration after the new path
has complete coverage. The three required interprocedural facts are **purity**,
**constant return value**, and **callee register clobbers**. A compilation
cache may reuse proven facts, but cache contents must never decide which facts
the compiler attempts to establish.

## Current state and problem

`driver/ANFPipeline.fs` runs program-level ANF optimization, inlining,
specialization, escape analysis, and reference-count insertion. Then
`driver/NativePipeline.fs` advances each `lowerToAllocatedLir` function list
through ANF-to-MIR, per-function MIR optimization, MIR-to-LIR, LIR optimization,
and allocation. Stdlib, specializations, dependencies, program functions, and
`_start` can enter through separate calls. The list boundary therefore controls
which functions can inform later passes, although it is not a semantic call-
graph boundary.

- MIR effect analysis scans the current list, then removes callers of unknown
  or effectful callees to a fixed point. CSE and LICM consume its effect-free
  set. A direct callee compiled in another list remains unknown to this pass.
- ANF inlining can consume selected external candidates, including simple
  constant returns. The native driver does not call MIR's program-level
  constant-call propagation. LIR peephole scans the current list for a narrow
  zero-argument integer constant-return shape and replaces matching calls.
- ARM64 allocation first allocates every function in the list, computes
  transitive register-write summaries to a fixed point, and selectively
  reallocates callers. A callee outside the list gets the full caller-clobber
  set. Final ARM64 code generation recomputes summaries over the assembled
  program to prune saves. x64 has no callee-clobber summary path.
- Session caches reuse optimized and allocated functions. Some keys already
  include direct-callee facts, but there is no shared summary catalog or
  scheduler ensuring that available callees are finalized before callers.

These fallbacks preserve correctness. They make optimization quality depend on
compilation-list boundaries and duplicate some whole-list analysis work.

## Target contract

The new pipeline owns a graph of **function versions**, not names alone. Each
node identifies its compilation unit, canonical function identity, body
revision, relevant options, and target where the fact is target-specific.
Direct calls are resolved to exact nodes, including precompiled stdlib and
dependency nodes. Indirect or unresolved calls have explicit unknown edges.
The graph is rebuilt or updated after transformations that add, remove, or
retarget calls. Inlining's existing SCC detection and the existing ANF/LIR
reachability graphs are starting points, not competing authorities.

The scheduler condenses the graph into strongly connected components (SCCs)
and processes the component DAG callee-first. A caller runs only when each
direct callee outside its SCC has a valid summary or an explicit unknown
summary. Members of one recursive SCC use a documented local fixed point or a
conservative internal-call rule. An unproven fact never silently becomes true.

Publish an immutable summary record per function version. Its fields become
available at separate milestones:

| Fact | Proof and meaning | First consumers |
| --- | --- | --- |
| Purity | A MIR-level effect summary records observable effects, reads of mutable state, traps, and possible nontermination separately. Direct-call effects compose with callee summaries. `effect-free` alone must not imply referential transparency or safe call removal. | MIR CSE/LICM; call elimination only under the stronger applicable proof. |
| Constant return | A typed value and its exact representation when every normal return yields the same value. This says nothing by itself about effects, traps, or whether the function returns at all. Unknown and nonconstant are distinct. | Constant propagation may know the result after a returning call; deleting the call additionally requires the relevant purity and termination proof. |
| Register clobbers | Target-specific physical registers that final emitted callee code may write, including backend expansions and transitive calls. Argument setup is a separate caller-side write envelope. Unknown means the full ABI caller-clobber set. | Caller register choice and call-save pruning after the callee's allocation is final. |

Specify lattice/merge rules for all three facts before implementation. In
particular, recursion and a non-returning path must not accidentally prove a
constant return or removable call. Preserve exact Float bits, managed-value
identity and ownership, and existing effect order. A summary is valid only for
the function version and pass outputs from which it was derived.

## Pipeline boundaries

Keep whole-program frontend and ANF transformations that require a shared
function inventory. After call-changing ANF passes, freeze a versioned graph
for the MIR stage; update it if MIR optimization changes call edges. For an
acyclic component, take each callee through MIR optimization, LIR lowering,
and target-specific allocation before its callers, publishing the appropriate
facts at each milestone. Reuse the existing passes inside this new schedule;
retain a stage-wide barrier only where a pass actually requires one.

For an acyclic edge, the caller receives the callee's finalized clobber
summary before allocation. For calls inside a recursive SCC, begin with a
conservative clobber contract and improve it only if a bounded, verified
component algorithm is demonstrated safe and profitable. Recompute summaries
from final allocated LIR before call-save pruning, as the ARM64 path does
today. An allocation or backend change that invalidates a published summary
must invalidate dependent caller results before emission.

Publish summaries from precompiled stdlib and dependency units into the same
catalog used by later user compilation. Carry their exact version and target
identity. Do not recompile immutable libraries solely to obtain a summary;
use an explicit unknown fact until a compatible summary exists. Function IDs
can recur across separately compiled units, so identity and codegen cache
matching must remain unit-aware.

## Migration sequence

Each step is a separately reviewable task with its own correctness and
performance evidence. The old path remains selectable while the new path is
under construction; it is removed when the final coverage step passes.

1. **Freeze semantics and measurement.** Document the three fact lattices,
   the difference between result propagation and call removal, recursive
   behavior, unknown calls, summary identity, and invalidation. Record current
   compilation times and missed opportunities on focused programs with
   cross-unit direct calls, recursive calls, constant returns with effects,
   and call-live values. Include both ARM64 and x64 where supported.
2. **Introduce the parallel orchestrator.** Add a command-line pipeline
   selector, a new driver entry point, graph construction, SCC scheduling,
   and summary publication. Reuse existing frontend/ANF and lowering passes.
   Initially publish unknown facts and emit identical behavior. Cover stdlib,
   specializations, preamble, dependencies, program functions, `_start`,
   expression/test compilation, and session reuse; do not limit the new path
   to one CLI entry point.
3. **Move purity.** Derive local MIR effects, compose known callees in SCC
   order, and make the optimized caller consume the catalog. Keep conservative
   behavior for unresolved and indirect calls. Compare CSE/LICM results and
   check ownership, mutable reads, traps, recursion, and coverage mode.
4. **Move constant returns.** Publish typed constants from optimized MIR,
   propagate them to callers, and explicitly separate substitution of a
   returning call's result from deletion of the call. Replace the narrow LIR
   list scan only after equivalent behavior and new cross-unit cases pass.
   If optimizing a caller creates a new constant callee fact, reschedule only
   dependent callers rather than rescanning unrelated functions.
5. **Move clobbers.** Extend the catalog with final allocated-register writes.
   Make ARM64 allocation consume finalized callee summaries across former
   list boundaries. Preserve conservative recursive and unknown-call rules;
   verify final save pruning against final emitted writes. Add an x64-specific
   summary model and consumer as a separate reviewable step, accounting for
   its scratch registers, calling convention, and floating-point policy.
6. **Make reuse dependency-aware.** Cache a function result together with the
   exact summary versions consumed at that stage. Changed function bodies,
   target, options, call edges, or callee summaries invalidate only affected
   results. Instrument hit rates and invalidation causes; remove redundant
   second-pass and whole-list caches when the scheduler supersedes them.
7. **Switch and remove.** Make the new pipeline default only after all entry
   points and relevant targets pass. Remove the selector, old orchestration,
   dead analyses, superseded cache types, and duplicate documentation in the
   same migration. Keep one production path and one source of truth for each
   summary.

## Acceptance and validation

- Before each observable compiler change, add a focused failing E2E case.
  Cover direct calls within and across compilation units, recursive SCCs,
  unavailable and indirect callees, effectful constant-return functions,
  mutable reads, traps or divergence, Float constants, and call-live integer
  and Float values. Preserve ahead-of-time diagnostics and ownership behavior.
- Compare old and new pipeline outputs on the same programs while the selector
  exists. Track compiled function count, graph/SCC time, summary lookups,
  unknown-fact causes, recompilations, and session cache hits. Distinguish
  genuine cross-unit information gains from merely different inlining.
- For each completed task branch run `./build --ai`, the full already-built
  `./run-tests --ai`, and `./benchmarks/run_benchmarks.sh --verify-parent full`
  on the host target. Record a full benchmark improvement when required by
  the repository policy. Validate x64 with its target suite and benchmark gate
  only for steps that change x64. Compare compiler runtime with the task
  parent when the scheduler or cache path changes; diagnose significant
  increases rather than accepting them as the cost of the redesign.
- Removal is complete only when the new path handles every production entry
  point, no old-pipeline selector or fallback remains, summary dependencies
  are explicit, and correctness and performance gates pass.

## Related work

- [Compiler pipeline](../compiler/pipeline.md)
- [SSA boundary migration](ssa-boundary-migration.md): coordinate stage
  boundaries so the two migrations do not establish duplicate long-lived IRs.
- [Verification policy](../contributing/verification.md)
