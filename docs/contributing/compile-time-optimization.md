# Investigating compiler compile time

Compile-time improvements start by finding how often work runs and how much
input each run processes. A slow pass may have an expensive algorithm, or a
cheap algorithm may be called thousands of unnecessary times. Measure both
before changing it.

## Establish a comparable measurement

- Use the same target, test selection, build configuration, and batching for
  the baseline and candidate. Record the revision and the number of logical
  tests, physical compilations, functions, and compilation batches.
- Measure wall time and compiler phase time separately from execution of the
  generated programs. Check host contention before interpreting a wall-time
  difference. Repeat a measurement when the host is noisy.
- Treat nested phase timings as a hierarchy. A child phase is already included
  in its parent; adding both exaggerates the total.
- Compare an end-to-end workload as well as a focused case. A local improvement
  can shift work to another stage or change generated-code performance.

## Find repeated work first

- Trace a costly operation from its callers. Count invocations by compilation
  stage and input size, then ask which calls represent distinct semantic work.
- Order producers before consumers when one analysis establishes facts needed
  by later work. For example, a callee can publish a summary before a caller
  is optimized. This can avoid a second pass over the whole program.
- Batch related work at the smallest scope that preserves the needed facts.
  Measure batch setup as well as the work inside each batch; many tiny batches
  can cost more than the analysis they enable.
- Move reuse checks before expensive preparation when the required cache
  identity is already available. Report hits, misses, and invalidation causes;
  a cache that only saves the final step leaves earlier repeated work intact.

## Check how cost grows

- Measure input size, iteration count, and time per invocation. Compare small
  and large cases. Repeatedly scanning an entire graph or catalog for each
  function can turn linear work into quadratic work.
- Inspect fixed-point passes for full structure copies, equality checks, and
  analyses repeated after no relevant fact changed. Count productive and
  unchanged iterations before altering the algorithm.
- Distinguish a larger corpus from more work per compilation. A small increase
  in physical compilations cannot explain a much larger increase in pass
  invocations without another change in scheduling or reuse.

## Carry facts across boundaries

- If an earlier representation already knows a fact, pass a validated summary
  to the later stage instead of reconstructing it from a lower-level form that
  has lost context. Direct-call relationships, types, reachability, and effect
  summaries are common examples.
- Give each summary an explicit scope and identity. Include the inputs that
  can change its meaning in reuse decisions, and invalidate only consumers of
  changed facts. Unknown or recursive cases still need conservative behavior.
- Keep one source of truth for a fact. When a new summary path replaces an old
  scan, remove the redundant path after correctness and performance checks
  establish that all entry points use the new one.

For compiler behavior changes, establish the observable case with a focused
end-to-end test before the fix. Then run the full host suite and the relevant
benchmark gate. A compile-time win is complete only when correctness and
generated-code performance remain acceptable.
