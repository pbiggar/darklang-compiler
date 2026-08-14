# Diff and ValueSearch parity

This document records the observable parity contract for the compiler's
`Darklang.Stdlib.Diff` and `Darklang.Stdlib.ValueSearch` modules. The
comparison was revalidated directly between compiler HEAD
`1f5282a113b2abbc15622d5ad7187fb29199964b` and `darklang/dark`
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. The older compiler snapshots
`51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3` and
`d6b58ef4`, and DCB1 report commit `8a402797`, were starting inventory only;
no finding from them was retained without checking these exact revisions.

The interpreter sources used for the comparison are
`packages/darklang/stdlib/diff.dark:1-69`,
`packages/darklang/stdlib/valueSearch.dark:1-84`, and
`packages/darklang/languageTools/packageManager.dark:9-58`. Supporting type
identity comes from `programTypes.dark:26-29,366-371` and
`runtimeTypes.dark:8-16,77-110`. Package primitive behavior was rechecked at
`backend/src/Builtins/Builtins.Matter/Libs/PM/Packages.fs:192-258,599-621`,
`backend/src/LibDB/RuntimeTypes.fs:86-99`, and
`backend/src/LibDB/ProgramTypes.fs:106-133` in the same interpreter revision.

## Diff contract

`DiffLine` is the nominal `Darklang.Stdlib.Diff.DiffLine` sum, declared in the
order `Same String`, `Added String`, `Removed String`. `lines` splits both
inputs on `"\n"`; consequently an empty input is one empty line and leading or
trailing newlines produce empty line values.

`buildTable` constructs the interpreter's longest-common-subsequence table.
When the upper and left cells are equal it chooses the upper cell. `trace`
walks from the final cell recursively and appends classifications while
unwinding, so output remains in source order. On a trace tie it follows the
upper cell as `Added`; because the walk is backwards, a one-line replacement
is rendered as `Removed old` followed by `Added new`. `tableGet` and trace's
line reads retain the interpreter's zero or empty-string defaults for invalid
indices. Public table indices and cells remain `Int`; canonical `List.getAt`
performs the checked internal conversion and returns `None` when it cannot fit
the skew-list's machine-sized index.

The copied implementation is in `src/DarkCompiler/stdlib/Diff.dark:3-80`.
Exact ordered probes for identical text, additions, removals, replacements,
repeated-line ties, empty strings, boundary empty lines, and mixed edits are in
`src/Tests/e2e/interpreter/diff.e2e:4-13`.

## ValueSearch contract

The public result is the nominal generic record
`Darklang.Stdlib.ValueSearch.FoundValue<'a>` with `path` then `value` fields.
The query accepts a nominal `ProgramTypes.Hash`, converts its text into the
distinct nominal `RuntimeTypes.Hash`, and requests exactly
`Known(KTCustomType(Package hash, []))`. Catalog entries match only when the
custom-type hash is identical and the type-argument list is empty. A different
hash or any non-empty argument list does not match.

Namespace and location behavior follows the interpreter source exactly:

- An empty namespace becomes `[]` and matches every selected location.
- A non-empty namespace is split on dots and compared as a segment-exact
  prefix. Text prefixes within a segment do not match, and a value path shorter
  than the namespace does not match.
- Namespace filtering uses only `[owner] + modules`; it deliberately excludes
  the terminal value name.
- A returned path is `owner + modules + name`, joined with dots.
- Location selection happens before namespace filtering. `pickLocation`
  chooses the greatest current-module prefix score, then the shortest
  owner/module path, then the first location in the supplied order.
- Hashes retain package-query order. Each hash is processed by selecting one
  location, filtering that selected location, evaluating the value, and then
  dropping missing locations and absent or failed evaluations. A matching
  alternate location is not reconsidered after the selected location fails
  the namespace filter.

The Dark implementations are in
`src/DarkCompiler/stdlib/PackageManager.dark:3-34` and
`src/DarkCompiler/stdlib/ValueSearch.dark:3-50`. Helper probes are in
`src/Tests/e2e/interpreter/value_search_helpers.e2e:3-13`; prefix scoring,
shortest-path selection, and stable location ties are covered at
`src/Tests/e2e/upstream/stdlib/language-tools/pickLocation.dark:28-159`. The
catalog-backed native test at
`src/Tests/compiler-passes/ValueSearchCatalogTests.fs:51-180` covers type
identity, namespace filtering, location choice, branch visibility,
lookup/evaluation failures, static result validation, and result order.

## Compiler-only AOT boundary

The interpreter primitive is backed by its live dev-time package manager and
database. The compiler intentionally does not acquire that service: it has no
content-addressed package traversal or ordinary top-level value initialization,
as documented in `name-resolution.md:81-101`. Instead, each `CompileRequest`
contains an explicit immutable `PackageValueCatalog` snapshot
(`src/DarkCompiler/CompilerLibrary.fs:761-816`). This catalog is compiler-only
machinery, not a new Dark-visible package model.

The snapshot records value hash, recursive custom-type identity, branch-visible
locations in their already-prioritized order, and typed evaluator state. During
compilation, the bridge at `CompilerLibrary.fs:1528-1690` discovers reachable
`ValueSearch` specializations, retains only entries for their concrete result
types, and materializes the narrow `pmFindValuesByValueType`,
`pmGetLocationsByValue`, and concrete `pmEvaluateValue<'a>` functions. The
generated functions are typechecked before their values cross into the caller,
then normal monomorphization, tree shaking, and type erasure proceed. Runtime
type descriptors are retained only at this catalog boundary; no reflection
primitive was needed.

The explicit snapshot has two intentional differences from the live service:
catalog order is supplied by the caller rather than emerging from a database
query, and branch visibility is frozen at compile time rather than queried at
execution time. Duplicate value hashes are rejected as an invalid snapshot.
Within that boundary, absent hashes, missing branch-visible locations,
unavailable values, and failed evaluation all have the interpreter-observable
results described above. Performance-only differences are outside this
contract.

`RuntimeTypes.KnownType` is loaded from interpreter syntax so its custom-type
case preserves the interpreter's canonical two-field declaration. The private
hash/empty-argument matcher and the recursive replacement for the interpreter's
generic `filterMap` are compiler-source adaptations only; they preserve the
public type identities, ordering, and failure behavior. The copied
`findByType` source spells its branch identifier as `String` because this
compiler models `Uuid` as a `String` alias; it does not create a distinct
runtime identity or behavior.
