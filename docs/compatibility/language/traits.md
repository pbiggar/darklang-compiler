# Static trait contract

Semantic reference: `darklang/dark@a9d93efc853486244630dad693fe79f6f4882cf6`,
`backend/src/LibParser/{Parser,WrittenTypes}.fs`,
`backend/src/LibExecution/Traits.fs` and `packages/darklang/stdlib/traits.dark`.
The [source ledger](../../porting/README.md) owns current imported dispositions.

| Behavior | Static implementation | Regression coverage |
| --- | --- | --- |
| Trait and impl grammar, method aliases, multiple methods | Range-bearing syntax and source-unit flattening | `test/fixtures/syntax/traits.syntax` |
| Single, multiple, parameterized and type-declaration bounds | Parsed, retained and enforced on function calls and type uses | `test/fixtures/syntax/traits.syntax` |
| Concrete named method calls | Select an implementation and emit ordinary checked function calls | `test/fixtures/e2e/traits.e2e` |
| Method aliases | Validate against the referenced ordinary function signature | `test/fixtures/e2e/traits-alias.e2e` |
| Implementation methods as ordinary functions | Public `<Type>.<Trait>.<method>` spellings resolve to the existing method FunctionId when unique | `traits-upstream-shapes.e2e` |
| Trait method function values | Existing generic function references select static method targets before specialization planning | `traits-upstream-shapes.e2e`, `traits-method-values.e2e` |
| Identically shaped trait declarations | Coalesce identities using upstream hash-relevant fields; declaration/parameter names are cosmetic, binder names and method order are significant | `traits-content-identity.e2e` |
| Trait-aware sorting | Select `Compare.lessThan` statically for concrete list elements; retain structural fallback and stable equal ordering | `traits-sort.e2e` |
| Unknown trait, missing impl, incomplete method set, wrong signature | Frontend compile errors | `traits-{unknown,missing-impl,method-set,wrong-method}.e2e` |
| Competing identical implementations | Compile-time ambiguity when selecting a call | `traits-ambiguous.e2e` |
| Traits as value types | Traits are excluded from the nominal type catalog | `test/fixtures/e2e/traits-as-types.e2e` |
| Bounded generic specialization | Typed calls resolve after substitution, before ordinary dependency planning | `traits-generic.e2e`, `traits-multiple-bounds.e2e`, `traits-generic-values.e2e`, `traits-bound-failure.e2e` |
| Bounds on generic types | Check construction, signatures and declaration field types | `traits-type-bounds.e2e` |
| Generic/conditional implementations | Directional argument matching, recursive obligations, blanket fallback and static specialization | `traits-conditional.e2e`, `traits-concrete-arguments.e2e`, `traits-generic-alias.e2e`, `traits-patterns.e2e`, `traits-shared-alias.e2e` |
| Receivers and operators | Static receiver calls, record-field precedence, custom and bounded-generic operators; primitive fast paths preserved | `traits-receivers.e2e`, `traits-receiver-parameters.e2e`, `traits-receiver-ambiguity.e2e`, `traits-operators.e2e` |
| Upstream stdlib traits and hosted packages | Unchanged trait declarations/utilities; native primitive aliases; hosted trait/impl/bound rendering and dependency closure | `traits-stdlib.e2e`, `traits-stdlib-identities.e2e`, `trait_packages_regression.py`; package discovery limitations below |

## Static selection and identity

Trait obligations remain in the immutable checked catalog. After type
substitution, specialization resolves calls to canonical ordinary method
FunctionIds before dependency planning; final preparation rejects unresolved
obligations. Method partial values infer their self type and become ordinary
captured lambdas. There is no trait-specific runtime or backend ABI.

Conditional implementations use directional argument matching and recursive
obligations. Known unsatisfied bounds must be discharged before receiver-owner
ambiguity checks. Different implementations keep independent identities even
when they alias the same function. Equally applicable competitors are compile
errors, without newest-wins arbitration.

Trait coalescing follows upstream hash-relevant fields, including effect
ceilings, binder names and method order; declaration and parameter names are
cosmetic. Imported `permissionCeiling` is identity metadata, not an enforced
effect ceiling. Dropping it can select an implementation for a different trait.
Traits do not enter the ordinary nominal type namespace.

Primitive methods reuse existing arithmetic, comparison, bitwise and rendering
functions. Fixed-width powers use wrapping repeated squaring without allocating
an unbounded intermediate. Sorting resolves Compare statically before ANF and
retains stable equal ordering and structural fallback. Caller-local implementations
must not reuse an incompatible cached structural/blanket result.

The pinned blanket grammar uses `for ('a)`; it does not introduce a lexer
extension for a bare apostrophe immediately after `for`. Local pipeline
lookahead and checking share `WrittenPipeSupport.lower`, giving piped lambdas
the same contextual parameter types as ordinary calls. Unsatisfied inferred
bounds and mixed arguments still fail during checking.

## Hosted package boundary

Imports fetch trait/impl kinds by name/hash, retain canonical hashes and
locations, render bounds and follow method dependencies. The `/:location`
module browser discovers active implementations and child modules without an
explicit method reference. If a nominal leaf shadows a module, discovery uses
`/search?modules=...&searchDepth=direct&entityTypes=module` before browser leaves.
Alias-only impls have no method child module; discovery probes
`<module>[.<Type>].<Trait>` for known traits. Legacy services retain name/hash
lookup. Required missing dependencies are errors.

Already-loaded stdlib traits are supplied by the checked catalog rather than
fetched again. Caller type metadata composes into helper specialization without
importing caller implementation catalogs. Hosted regressions cover local HTTP
discovery, pipelines, stdlib bounds, method aliases, ambiguity/missing impls,
identity metadata and offline inventory reuse. These are controlled-service
checks, not a claim about a deployed remote service.

## Scope and regression boundaries

Selection finishes at compilation. Trait objects, runtime lookup, trait effect
ceilings and persistence/history pinning remain outside the static model;
ordinary function effect checks continue to apply. The source renderer does
not preserve saved `EFnName.boundImpls`, `EPipeFnCall.boundImpls` or historical
implementation choices as persistence semantics.

The upstream trait reference at `test/fixtures/upstream-reference/language/traits.dark`
and selected tests under `test/fixtures/e2e/upstream/trait-adoption/` retain their
source provenance. Adapted negative tests use AOT diagnostic expectations;
fixture-only equality adapters do not implement interpreter counters or
reflective mismatch rendering. Selected rendering tests provide regression
coverage, not evidence that traits introduced primitive rendering.

`float-callee-constants.e2e` covers NaN and signed-zero summary identity: summaries
must compare exact Float bits, not ordinary NaN equality. Keep this boundary
when changing generic utility specialization.
