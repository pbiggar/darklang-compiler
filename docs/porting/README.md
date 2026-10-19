# Repeatable compatibility ledger

This ledger tracks the compiler port against the exact interpreter revision in
`porting/sources.yaml`, initially on x64 Linux. It uses existing sources and
existing tests: no new compatibility probes, expected values, differential
corpus, or runner-validation project.

Start with the [generated ledger](generated/ledger.md). Detailed mappings,
declarations, coverage witnesses, test counterparts and effective enablement are in
`porting/generated/ledger.json`.

Git stores the machine index as `porting/generated/ledger.json.gz`, compressed
with a fixed header and no timestamp. Regeneration expands the ordinary JSON
locally; `--check` compares the tracked archive and readable reports. The JSON
contains the same facts as the archive and is ignored by Git. This keeps frequent
checkpoints small while retaining the full index.

## Regenerate

Install the pinned Python dependency from `scripts/porting-requirements.txt`.
Supply a local interpreter checkout at the exact upstream pin. Source discovery
reads committed blobs, so dirty interpreter files cannot alter the comparison.

```bash
python3 scripts/update-ledger.py --upstream /path/to/interpreter
python3 scripts/update-ledger.py --upstream /path/to/interpreter --check

# After activating the pinned native environment:
env -u LD_PRELOAD ./build --ai
python3 scripts/update-ledger.py --upstream /path/to/interpreter --compile-packages
```

The default command reads sources and existing package compilation receipts.
`--check` compares generated outputs without writing or executing tests.
Test comparison uses source inventory and effective enabled/disabled metadata.
Enabled tests are assumed to pass; pass/fail receipts are not part of this ledger.

`--compile-packages` runs the already-built native declaration adapter, separately
from test execution. Default regeneration consumes its receipt without compiling.

`bash scripts/check-porting` runs the existing tooling/layout checks and checks
the generated ledger using an isolated checkout of the pin. It does not execute
the compiler suite.

## What each ledger records

| Ledger | Generated facts | Maintained decisions |
| --- | --- | --- |
| Language | A short feature list and hashes of its watched sources | Implemented, partial, missing, or not-assessed |
| Parser | Source components, known mappings and unmapped components | Split/combined component mappings and remaining gaps |
| Builtins | Literal upstream registrations, actual Builtin declarations and source implementation counterparts | Confirmed compiler builtin mappings |
| StdLib | Per-declaration byte identity, differences, missing/ambiguous counterparts and compiler-only declarations | Source mappings and accepted differences |
| Packages | Per-declaration source identity, containing-unit compilation and existing test references | Exceptional mappings and otherwise unresolved coverage |
| Tests | Every pinned named test/legacy assertion, compiler counterparts, source/expectation adaptations and enablement | Exceptional test mappings; existing exclusions and AOT expectations stay in the suite |

Packages include StdLib sources; the two counts overlap. A root-module match is
a mapping candidate, not proof of equivalent declarations. Source comparison
is exact declaration bytes, not normalized ASTs or concatenated approximations of
split files. The declaration span excludes surrounding comments/blank lines but
retains its indentation, internal comments and line endings. Functions, values,
types, traits, implementations and their methods have separate identities;
named test declarations belong in the test inventory. Trait/implementation spans
include their methods, so declaration counts are not non-overlapping byte totals.
Qualified kind/name matches work across file splits. Maintained file mappings
disambiguate duplicate names; unresolved alternatives remain ambiguous.

Machine schema 2 uses `declarations` as the primary inventory. Each row has its
upstream span/hash, compiler counterpart spans/hashes, source status, coverage
witness and compilation provenance. `files` retains supporting file mappings
and compilation units; file totals are not the source-equivalency score.
Builtin discovery does not invent registrations with computed names or claim
that a same-name declaration proves an assembled runtime binding.

Builtin counterparts follow qualified upstream wrappers into the compiler's
matching declaration. Numeric trait methods can map to the compiler's pre-trait
type module (`Int64.Add.add` to `Int64.add`). These identify implementation
locations, not callable `Builtin.int64Add` aliases. Generic intrinsic registrations
with the same short name are no longer counted as builtin bindings. Computed
constructors retain their source/line review entries; no runtime registry is built.

## Original-source package compilation

`tools/ledger/compile_packages.exe` uses the existing parser and native preamble
pipeline: checking, generic handling, ANF/SSA and allocated native LIR. It compiles
declarations, not a fabricated entry expression, and does not link or run them.
Generic bodies are checked; only naturally demanded concrete instances are lowered.

The update script materializes byte-identical pinned Git blobs in a temporary
directory. Each original package file is compiled with its indexed transitive
non-StdLib dependencies, against the embedded compiler StdLib. Named test bodies
are separated using `NamedPackageTests.partition`, not rewritten. Test-only files
can therefore have an empty declaration set; success says nothing about their tests.
Dependency discovery is lexical: unresolved dynamic references or unfamiliar syntax
remain limitations, not proof that a package's own body is defective. The receipt
includes dependency paths and actual compiler diagnostics for every original file.

`porting/package-results.json` records the upstream pin and source/build context.
Changed inputs make receipts stale; missing executables or adapter-wide failures
are blocked. Each declaration links to its containing-unit result. A compiled
unit provides compilation evidence for its declarations; a failed unit does not
identify which declarations fail independently. Compilation never changes source
identity or test enablement. Full logs stay in ignored
`TestResults/porting/package-compilation.log`.

## Maintained inputs and ChatGPT review

`porting/ledger-inputs.yaml` is the maintained input, not a generated report.
It holds features, source/parser/builtin mappings, accepted source differences,
and explicit references to existing tests.

Feature source paths are the watched implementation files. After reviewing a
feature, set its status and copy its generated `source_sha256` into
`reviewed_sha256`. Changed watched files preserve the decision but flag it for
review. Add newly introduced implementation paths to the maintained watch list.

Coverage analysis follows lexical references between existing declarations.
It records direct references, indirect references through helpers, maintained
coverage, or not-identified. Each declaration has a reference count and one
deterministic witness; indirect witnesses use the shortest discovered path.
This is a bounded source analysis, not proof a particular branch executed.
Unknown headers and ambiguous declarations stay in the review queue.
Source differences and missing counterparts remain on the individual declaration
rows instead of being reduced to file-level judgments.

Trait/impl methods and escaped declaration names are included in the inventory.
String interpolation expressions retain their references, while literal prose is
masked. Maintained references also propagate indirectly to helpers. The generated
[coverage-gap list](generated/coverage-gaps.md) lists every remaining unreferenced
declaration. Archived helper copies and implicit operator dispatch are not silently
counted as covered merely because a similarly named test exists.

Maintained coverage names a qualified declaration and supplies `tests` and
`reason`. Test references must be existing named-test IDs
(`source-path#qualified-name`) or existing imported fixture paths. No dedicated
helper tests need to be created.

ChatGPT can inspect a review item and update these maintained decisions, with
source/test references and an explanation. Ordinary regeneration requires no
model call and keeps ordering, names and hashes deterministic.

## Interpreter tests versus compiler tests

Each pinned named test and legacy execution assertion has one upstream row.
Counterparts come from the compiler's existing E2E fixtures (including
`test/fixtures/e2e/upstream/`) and named declarations. Qualified named identities
match directly; recognizable single `Stdlib.Test.equal` assertions and `Test.all`
groups can match older `source = expectation` fixtures. A group is fully mapped
only when every indexed assertion has a counterpart. Partial groups retain their
matched/total assertion counts. Duplicate candidates remain ambiguous. A missing
row means no counterpart was identified, not proof no equivalent test exists.

The existing upstream diff script supplies the established
`Builtin.testDerrorMessage`/`error=` adaptation. Compiler `#compileerror` overrides
remain visible and prevent an identical classification. Exact test spans are
identical; recognized migrations/maintained adaptations are adapted; changed
named counterparts are different. This is source comparison, not behavioral proof.
Helper/preamble equivalence is not inferred from matching assertion expressions.

Enablement is derived from `E2EDiscovery.ml` whole-file and original-line gates,
fixture skip attributes, and local expectation overrides. Named tests imported
under the E2E fixture directory are selected by the existing named fixture adapter;
declarations only present under package sources remain outside that selection. Every counterpart
records its exclusion origin/reason and expectation override. Ordinary command-line
filters used for individual test runs do not change the inventory.

Optional `test_mappings` in `porting/ledger-inputs.yaml` maps an upstream test ID
to existing compiler test IDs and a reason, for helper-driven or otherwise
unrecognized migrations. IDs use `source#L<number>` for legacy assertions and
`source#Qualified.Name` for named declarations. No new test programs are created.
Unmatched compiler assertions are listed separately with imported/compiler origin;
an old upstream fixture absent from the current pin is not assumed compiler-authored.
Unindexed source forms remain explicit review items. Backend F# harness sources
remain in the supporting inventory; this comparison covers Dark execution tests.

No tests run during regeneration. The previous `--run-tests` ledger option and
pass/fail receipts are superseded; existing runner exports remain independent tools.

## Follow-up port work (not ledger merge requirements)

- Finish current named-package test execution using the existing upstream tests.
- Prioritize compatibility gaps reported by declaration differences, missing mappings and compilation errors.
- Expand maintained coverage references only where source analysis cannot resolve them.

Behavioral details belong in the existing suite. Performance validation is not
part of this ledger work. New language differences require agreement; upstream
bugs must not silently exempt tests. Record upstream issues and obtain explicit
permission before changing `darklang/dark`.

The older [policy/source inventory](generated/index.md), category manifests,
`porting.py` and legacy discovery collector remain available as supporting
migration tools. Their old generic verification counts are not the current
ledger's completion metric and are not regenerated by the new command.
