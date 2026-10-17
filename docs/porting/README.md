# Repeatable compatibility ledger

This ledger tracks the compiler port against the exact interpreter revision in
`porting/sources.yaml`, initially on x64 Linux. It uses existing sources and
existing tests: no new compatibility probes, expected values, differential
corpus, or runner-validation project.

Start with the [generated ledger](generated/ledger.md). Detailed mappings,
declarations, coverage witnesses, and individual test outcomes are in
`porting/generated/ledger.json`.

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
```

The default command only reads sources and existing execution receipts.
`--check` compares generated outputs without writing or executing tests.
`--run-tests` executes the already-built compiler suite with its existing
expectations, consumes individual outcomes, and refreshes the ledger. It does
not build implicitly. A missing binary or unavailable result export is blocked,
not a pass or a test failure.

`bash scripts/check-porting` runs the existing tooling/layout checks and checks
the generated ledger using an isolated checkout of the pin. It does not execute
the compiler suite; use `--run-tests` for that.

## What each ledger records

| Ledger | Generated facts | Maintained decisions |
| --- | --- | --- |
| Language | A short feature list and hashes of its watched sources | Implemented, partial, missing, or not-assessed |
| Parser | Source components, known mappings and unmapped components | Split/combined component mappings and remaining gaps |
| Builtins | Literal upstream registrations and same-name compiler declarations | Confirmed aliases/counterparts where names differ |
| StdLib | Exact byte identity, differing sources, missing mappings, compiler-only files | Source mappings and accepted differences |
| Packages | Source identity and existing test references for functions/types | Exceptional mappings and otherwise unresolved coverage |
| Tests | Actual pass/fail/skipped/blocked outcomes from existing imported tests | Existing exclusions and agreed AOT expectations stay in the test suite |

Packages include StdLib sources; the two counts overlap. A root-module match is
a mapping candidate, not proof of equivalent declarations. Source comparison
is exact bytes, not normalized ASTs or concatenated approximations of split files.
Builtin discovery does not invent registrations with computed names or claim
that a same-name declaration proves an assembled runtime binding.

Package compilation is currently **not run**. A declaration-only compilation
adapter remains to be implemented; source identity, references from tests and
parsing are not substitutes for successful compilation.

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

- Add original-source package compilation, without synthesizing probe programs.
- Finish current named-package test execution using the existing upstream tests.
- Review/populate the coarse feature decisions and ambiguous mappings.
- Expand maintained coverage references only where source analysis cannot resolve them.

Behavioral details belong in the existing suite. Performance validation is not
part of this ledger work. New language differences require agreement; upstream
bugs must not silently exempt tests. Record upstream issues and obtain explicit
permission before changing `darklang/dark`.

The older [policy/source inventory](generated/index.md), category manifests,
`porting.py` and legacy discovery collector remain available as supporting
migration tools. Their old generic verification counts are not the current
ledger's completion metric and are not regenerated by the new command.
