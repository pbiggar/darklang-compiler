# Understanding the compiler port

This directory answers a practical question: **how much of the Darklang
interpreter has been brought into the native compiler, and what is left?**
It is written for people reviewing or working on the port. Coding agents use
the same instructions.

The *compatibility ledger* is a generated list of language capabilities,
standard-library and package declarations, builtin implementations, and tests.
It compares the compiler with one recorded version of the interpreter. That
version is in [sources.yaml](../../porting/sources.yaml), so the comparison
does not change whenever the interpreter's main branch moves.

## Find the current state

Start with the [current port status](generated/ledger.md). Its opening summary
shows the main counts. Below that are the specific language features, declaration
comparisons, test references, compilation information, and test-suite comparison.

For an individual function or type, search its full name in the plain-text
[detailed ledger](../../porting/generated/ledger.json), for example
`Darklang.Stdlib.List.map`. This file is committed as ordinary JSON and can be
reviewed in a Git diff. It contains the source locations behind the summary.

These are separate measurements; there is no single overall compatibility score.
A function can have different source, compile successfully, and have an enabled
test. Conversely, identical source can use a language feature the compiler lacks.

The report is a snapshot. Run the check command below to find out whether it
still agrees with the checked-out sources. Language assessments are maintained
by people and need review when the corresponding implementation changes.

## What the parts mean

| Part | The question it answers | How it is maintained |
| --- | --- | --- |
| Language features | Does the compiler support this particular capability? | A reviewed list with implementation references and limitations. |
| Parser | Where are the compiler counterparts of the interpreter's parser components? | Source-file discovery and explicit mappings for split or renamed components. |
| Builtins | Where is each low-level interpreter operation implemented in the compiler? | Scan existing declarations and follow known source counterparts. |
| StdLib | Which standard-library declarations have identical, changed, or missing compiler source? | Match declarations by qualified name and compare their own text. |
| Packages | Which package declarations match, compile, and have existing tests? | The same declaration comparison, plus compilation results and test references. |
| Tests | Which interpreter tests have compiler counterparts, and are those enabled? | Compare existing test sources and read the compiler's existing skip rules. |

Packages include StdLib, so their totals overlap. Named test declarations belong
to the test comparison rather than the function/type source counts.

### Language features

The [language list](../../porting/language-features.yaml) has a separate row for
each capability: for example `Int128`, partial application of a function value,
nested string interpolation, list-cons patterns, and the `Compare` trait.
Related rows are grouped for browsing.

`implemented` means the compiler has that capability. `partial` means the notes
describe a remaining limitation. `missing` means it has not been implemented.
`not-assessed` means the implementation has not yet been reviewed.
Individual behavioral cases remain in the existing test suite.

### StdLib and package declarations

A declaration is a named function, value, type, trait, implementation, or method.
Its identity is its qualified name, not a hash. Source paths and line numbers
tell you which definition is being compared when a name occurs more than once.

For example, moving `Darklang.Stdlib.List.map` into a different file does not
make it a new function. Changing a helper used by `map` does not change the source
comparison for `map`: only its own declaration text is compared.

- **Identical:** the declaration text matches exactly.
- **Different:** a counterpart exists but its text differs.
- **Missing:** no counterpart has been identified.
- **Ambiguous:** several possible counterparts need review.
- **Compiler-only:** a compiler declaration has no identified upstream counterpart.

The comparison preserves indentation, internal comments, and line endings;
surrounding comments and blank lines are excluded. It does not compare behavior.
Trait and implementation declarations include their methods, which also have
their own rows; these counts are not a measurement of non-overlapping source bytes.

Package rows also show existing test references. A *direct* reference means a
test mentions the declaration. An *indirect* reference follows calls through
other declarations: a test of a public function can cover its helper.
A *maintained* reference is an explicitly recorded relationship to an existing
test. *Not identified* means the source analysis found no such relationship.
The [reference-gap list](generated/coverage-gaps.md) identifies these declarations.

Compilation uses the original interpreter source files, with the dependencies
needed to compile them. Each declaration links to the result for its containing
file. If that file fails, the ledger does not claim that every declaration in it
fails independently. `not-run`, `blocked`, and `stale` distinguish absent,
unavailable, and outdated results.

Saved compilation results have a separate compiler-input fingerprint so results
from an older implementation are not presented as current. This is only about
reusing compilation results; it is not a declaration ID or a dependency-based
source comparison.

### Interpreter tests and compiler tests

The interpreter has newer named tests, such as:

```dark
test singleton =
  Stdlib.List.singleton 1L |> Stdlib.Test.equal [ 1L ]
```

Many compiler imports still use the older assertion format:

```dark
Stdlib.List.singleton 1L = [ 1L ]
```

The ledger can match these by the existing assertion content. Named tests can
also match by qualified name. For a named `Test.all` group, every assertion
needs a counterpart for a complete match; otherwise the row shows a partial match.

Each counterpart records whether it is enabled and the existing reason for
disabling it. Whole-file exclusions, individual line exclusions, and fixture skip
annotations all count. A compiler `#compileerror` expectation override is shown
as an adaptation rather than an identical test.

Enabled tests are assumed to pass. Regenerating the ledger does not execute them.
An unmatched row means no counterpart was identified; helper-based tests or
ambiguous matches may need a maintained mapping. Matching an assertion does not
establish that surrounding helper definitions are identical.

## Update the ledger

You need Python, the dependency in `scripts/porting-requirements.txt`, and a local
interpreter checkout at the revision recorded in `porting/sources.yaml`.
Commands below run from the compiler repository root:

```bash
python3 -m pip install -r scripts/porting-requirements.txt
python3 scripts/update-ledger.py --upstream /path/to/interpreter
python3 scripts/update-ledger.py --upstream /path/to/interpreter --check
```

The first update reads the pinned interpreter's committed files and the current
compiler sources, then rewrites the JSON and Markdown reports. The check command
reads the same inputs and reports whether the committed outputs need updating.
Neither command builds the compiler or runs tests.

To refresh compilation results, first build using the repository's native
toolchain instructions, then run:

```bash
python3 scripts/update-ledger.py --upstream /path/to/interpreter --compile-packages
```

Review and commit the changed reports. Missing declarations and disabled tests
remain useful recorded work; they do not need fixing to update the ledger.

## Where to make corrections

- Edit `porting/language-features.yaml` for specific language assessments.
- Edit `porting/ledger-inputs.yaml` for exceptional source/test mappings and
  references to existing tests.
- Change actual test enablement in the existing suite, then regenerate.
- Correct discovery/comparison logic in `scripts/update-ledger.py` and
  `scripts/ledger_inventory.py` when an existing source form is not recognized.

Human or ChatGPT review can resolve a mapping the scripts cannot determine.
Record the decision and the existing source/test references, then regenerate;
ordinary updates do not require a model.

The older [six-category policy inventory](generated/index.md) is supporting
historical material. Use the current port-status report above for current ledger
counts.
