# Traits — implementation checkpoint

Compiler baseline: `d8e2d7161ecf6892079906cdc5d797d6aa87e7fb`.
Interpreter reference: darklang/dark `a9d93efc853486244630dad693fe79f6f4882cf6`.
Reference grammar: `backend/src/LibParser/{Parser,WrittenTypes}.fs`.
Reference semantics: `backend/src/LibExecution/Traits.fs` and
`packages/darklang/stdlib/traits.dark` at that revision.

This is an unfinished implementation checkpoint, not a compatibility claim.
M0 fixtures and M1 parser plumbing are preserved; M2 concrete checking and
selection are implemented but await a successful build and native execution.
No milestone exit gate has yet been demonstrated.

| Behavior | Checkpoint implementation | Evidence awaiting execution |
| --- | --- | --- |
| Trait and impl grammar, method aliases, multiple methods | Range-bearing syntax and source-unit flattening | `test/fixtures/syntax/traits.syntax` |
| Single, multiple, parameterized and type-declaration bounds | Parsed and retained; checking rejects unsupported bounds explicitly | `test/fixtures/syntax/traits.syntax` |
| Concrete named method calls | Select an implementation and emit ordinary checked function calls | `test/fixtures/e2e/traits.e2e` |
| Method aliases | Validate against the referenced ordinary function signature | `test/fixtures/e2e/traits-alias.e2e` |
| Unknown trait, missing impl, incomplete method set, wrong signature | Frontend compile errors | `traits-{unknown,missing-impl,method-set,wrong-method}.e2e` |
| Competing identical implementations | Compile-time ambiguity when selecting a call | `traits-ambiguous.e2e` |
| Traits as value types | Traits are excluded from the nominal type catalog | Additional E2E coverage required |
| Bounded generic specialization and conditional impls | Not implemented (M3–M4) | Required |
| Receivers and operators | Not implemented (M5) | Required |
| Upstream stdlib traits and hosted packages | Not implemented (M6) | Required |
| Full parity and readiness gates | Not executed (M7) | Required |

Intentional scoped differences: selection must finish at compilation; equally
applicable competing implementations are errors. Trait objects, runtime lookup,
newest-wins arbitration, trait effect ceilings and persistence/history pinning
are outside the work order. Existing ordinary function effect checks remain.

Validation so far: `git diff --check` passed. All changed OCaml source/interface
files passed syntax parsing using the bootstrap's pinned OCaml bytecode compiler
(`ocamlc -stop-after parsing`); this does not establish type correctness.
`./run-tests --ai` could not start because the native test executable is absent.
The prescribed toolchain bootstrap failed during its first native compiler build
without a compiler diagnostic; a clean prescribed retry is in progress.
