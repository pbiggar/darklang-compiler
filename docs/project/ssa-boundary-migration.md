# Move the SSA boundary through ANF

This plan moves SSA construction from MIR lowering to the end of ANF, then
one pass at a time toward checked-AST lowering. The production pipeline has
one conversion boundary at each stage. Passes before it use structured ANF;
passes after it use typed high-level SSA. The final stage constructs SSA
directly and removes structured ANF.

## Target representation

High-level SSA retains the typed ANF operations, their source evaluation
order, function identities, and ownership-sensitive effects. A function has
an entry block and typed blocks. Each block has zero or more typed parameters,
ordered value definitions, and one `Return`, `Jump`, `Branch`, or terminal call.
An edge supplies exactly one argument per target parameter. Every value has
one definition, and that definition dominates every use. Block arguments
express joins; no instruction writes an existing value identity.

Effects, alias information, and ownership contracts remain distinct from SSA
identity. A managed edge argument transfers an owned unit or carries a proven
borrow. The ownership verifier checks that transfer and path-local cleanup.
The SSA verifier checks definitions, dominance, edge arity and types, and
terminators after construction and after each transforming pass.

## Migration sequence

1. **Establish SSA after all ANF passes.** Convert final ANF to typed
   high-level SSA after reference counting and tail-call detection. Preserve
   current control flow and ownership; do not introduce new managed joins in
   this step. Lower SSA blocks directly to SSA-form MIR. Lower block arguments
   to typed MIR phis and make all MIR expansion temporaries single-definition.
   Self tail-call backedges supply loop-header phi inputs, including parallel
   parameter swaps and Float64 values. Remove `SSA_Construction` from the
   production driver only after all user, expression, stdlib, and package
   compilation paths use direct SSA MIR lowering. Keep an independent verifier
   for the MIR SSA invariant.
2. **Move tail-call detection.** Convert immediately before this pass. Preserve
   ownership cleanup order and terminal-call behavior. Form self-recursive
   backedges with header parameters or preserve a terminal self call that the
   SSA MIR lowerer translates to the same phis.
3. **Move reference-count insertion.** Convert before this pass. Extend block
   arguments to managed values, with explicit ownership transfer or verified
   borrowing on each edge. Compute liveness and cleanup across CFG edges.
   Cover aliases, values live after a branch, and loops. Remove the ANF RC path
   when all production callers use the SSA pass.
4. **Move escape analysis.** Use SSA use sets and ownership facts for scalar
   replacement and unique block reuse. Preserve destruction and alias proofs.
5. **Move direct-call specialization.** Clone typed blocks and rewrite call
   sites and function signatures together.
6. **Move higher-order specialization.** Propagate callable facts through block
   arguments. Convert external optimization templates and cache identities.
7. **Move inlining.** Freshen block and value identities, then connect every
   inlined return to a typed continuation.
8. **Move ANF optimization.** Port its fixed-point scheduler without changing
   iteration order or effect barriers. Move component rewrites in focused steps;
   one fixed-point iteration must run entirely in one IR.
9. **Move reachability and generated output.** Convert every call-graph query,
   including repeated user and stdlib reachability, then move print insertion
   while retaining its consuming ownership boundary.
10. **Construct SSA directly.** At this point the boundary is immediately
    after checked-AST lowering. Make expression, lambda, monomorphization,
    list-region, and ownership-variant lowering emit typed SSA values and
    blocks directly. Remove the ANF-to-SSA converter, structured ANF types and
    production passes, obsolete dumps, and obsolete pass fixtures.

Each numbered step is a separately reviewable unit. A unit is complete only
when all applicable compiler entry points use its new boundary, its old
production path is removed, and behavior, ownership, and performance gates
pass. Do not keep a second production pipeline as a migration fallback.

### Reference counting boundary prerequisites

The current ANF reference-count pass also recovers the `TempId` type map used
by SSA construction. Move type recovery into its own pre-SSA analysis before
moving the boundary. Preserve the existing use-site inference for `TupleGet`
and untyped `RawGet`, closure-call return types, and branch-local identities.
The SSA builder must recover a type at each definition site before it
freshens duplicate ANF identities. Preserve an explicit unresolved type
variable where existing inference cannot yet determine the concrete type;
reject it before MIR lowering if later use-site information does not resolve
it.

The current ANF join verifier rejects managed join arguments. Before allowing
them, define each SSA edge argument as an owned transfer or a verified borrow.
An owned transfer consumes one path-local ownership unit. A borrow requires
the source owner to remain live through the successor's last use. At merges,
check every incoming edge independently; an edge may not inherit another
predecessor's ownership proof. Releases must run on every exit from a live
range, including branch returns and loop backedges, without crossing a use.

Port return/alias analysis and RC insertion over SSA definitions and edge uses
together. Preserve the existing special handling for returned accumulators,
unique record reuse, closure captures, raw slots, and terminal output before
removing the structured ANF RC path. Do not use an SSA-to-ANF round trip as a
production pass: it would retain both ownership algorithms and add conversion
cost at the boundary. Once RC produces typed, ownership-verified SSA, move
escape analysis. Its scalar replacement can use SSA use sets; its unique block
reuse still needs the destruction and last-use proofs supplied by RC.

## HIR and ownership

Current HIR consumes checked AST before ANF. Moving the downstream SSA
boundary does not change HIR's input. Keep HIR's semantic effect, alias, call,
and ownership analyses during the migration. Carry their contracts into the
SSA builder, and verify edge ownership independently after lowering and every
transformation that changes values or control flow. Monomorphized and cloned
functions need correspondingly instantiated contracts; an earlier proof does
not certify a changed function automatically.

When direct SSA construction is complete, keep HIR analyses that still supply
independent semantic or list-region facts. Move whole-function ownership work
onto the production SSA graph where that removes duplicate analysis. Remove
any general HIR representation that has become only a second copy of the same
function; do not create a second long-lived SSA CFG merely for HIR.

## Verification and performance

Add a focused failing E2E case before changing observable compiler behavior.
Test joins, non-returning arms, managed ownership transfer, aliases, closure
specialization, recursive parameter swaps, Float64 loop values, and both host
compilation paths. Keep ahead-of-time match diagnostics ahead of execution.
After source changes, run `./build --ai`, then `./run-tests --ai`, and validate
binary performance with `./benchmarks/run_benchmarks.sh --verify-parent full`.
Use pass timings and compiler-focused diagnostics to identify added graph or
analysis cost, but judge each completed unit against its task parent. Preserve
the emitted effect order and compare any changed workload rows, not only the
aggregate benchmark ratio.
