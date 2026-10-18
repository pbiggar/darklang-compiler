# Repeatable compatibility ledger

This ledger tracks the compiler port against the exact interpreter revision in
`porting/sources.yaml`, initially on x64 Linux. It uses existing sources and
existing tests: no new compatibility probes, expected values, differential
corpus, or runner-validation project.

Start with the [generated ledger](generated/ledger.md). Detailed mappings,
declarations, coverage witnesses, and individual test outcomes are in
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
python3 scripts/update-ledger.py --upstream /path/to/interpreter --run-tests
python3 scripts/update-ledger.py --upstream /path/to/interpreter --compile-packages
```

The default command only reads sources and existing execution receipts.
`--check` compares generated outputs without writing or executing tests.
`--run-tests` executes the already-built compiler suite with its existing
expectations, consumes individual outcomes, and refreshes the ledger. It does
not build implicitly. A missing binary or unavailable result export is blocked,
not a pass or a test failure.

`--compile-packages` runs the already-built native declaration adapter, separately
from test execution. Default regeneration consumes its receipt without compiling.

`bash scripts/check-porting` runs the existing tooling/layout checks and checks
the generated ledger using an isolated checkout of the pin. It does not execute
the compiler suite; use `--run-tests` for that.

## What each ledger records

| Ledger | Generated facts | Maintained decisions |
| --- | --- | --- |
| Language | A short feature list and hashes of its watched sources | Implemented, partial, missing, or not-assessed |
| Parser | Source components, known mappings and unmapped components | Split/combined component mappings and remaining gaps |
| Builtins | Literal upstream registrations, actual Builtin declarations and source implementation counterparts | Confirmed compiler builtin mappings |
| StdLib | Exact byte identity, differing sources, missing mappings, compiler-only files | Source mappings and accepted differences |
| Packages | Source identity, native declaration compilation and existing test references for functions/types | Exceptional mappings and otherwise unresolved coverage |
| Tests | Actual pass/fail/skipped/blocked outcomes from existing imported tests | Existing exclusions and agreed AOT expectations stay in the test suite |

Packages include StdLib sources; the two counts overlap. A root-module match is
a mapping candidate, not proof of equivalent declarations. Source comparison
is exact bytes, not normalized ASTs or concatenated approximations of split files.
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
are blocked. Package errors are failures, not exclusions. Compilation never changes
the source-identity or test-outcome scores. Full logs stay in ignored
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
Unknown headers, ambiguous declarations and unresolved source differences stay
in the review queue instead of being guessed away.

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

## Existing test execution

The native runner's `--results-json=PATH` exports E2E outcomes from the same
callback that records existing individual and batched results. The update script
also requests the existing parser/gate inventory, then reconciles cases by
source path and full test name. It does not infer individual passes from timing
records, suite totals, or eligibility.

`porting/test-results.json` stores execution receipts with the upstream pin,
target and compiler/test source context hash. Changed execution inputs make
results stale. Timings, timestamps and temporary paths are not used as evidence;
full diagnostics remain under `TestResults/porting/`.

Skipped cases retain their existing reason. An eligible case without an actual
outcome is blocked. Parse errors remain explicit. Named package tests are
currently blocked until their existing Test helpers, test-local builtins and
execution adapter are implemented; discovery alone cannot mark them passing.
Runs with failures, missing outcomes, parse errors, or unsupported named-test
execution exit unsuccessfully.

## Remaining implementation

- Finish current named-package test execution using the existing upstream tests.
- Fix the compatibility gaps reported by source differences, missing mappings and compilation errors.
- Expand maintained coverage references only where source analysis cannot resolve them.

Behavioral details belong in the existing suite. Performance validation is not
part of this ledger work. New language differences require agreement; upstream
bugs must not silently exempt tests. Record upstream issues and obtain explicit
permission before changing `darklang/dark`.

The older [policy/source inventory](generated/index.md), category manifests,
`porting.py` and legacy discovery collector remain available as supporting
migration tools. Their old generic verification counts are not the current
ledger's completion metric and are not regenerated by the new command.
