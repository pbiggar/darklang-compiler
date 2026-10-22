# Understanding the compiler port

This ledger answers: what exists in the pinned Darklang interpreter, what has
been ported to the native compiler, and what remains? Start with the
[current status](generated/ledger.md), then open the relevant YAML file.

| Area | Entries |
| --- | --- |
| [Language](../../porting/language-features.yaml) | Specific capabilities and known limitations. |
| [Parser](../../porting/parser.yaml) | Syntax behaviors and remaining gaps. |
| [StdLib](../../porting/generated/stdlib.yaml) | Upstream declarations, grouped by relative source path. |
| [Packages](../../porting/generated/packages.yaml) | Other upstream package declarations; excludes StdLib. |
| [Builtins](../../porting/generated/builtins.yaml) | Interpreter operations and whether a compiler binding is identified. |
| [Tests](../../porting/generated/tests.yaml) | Upstream tests, compiler counterparts, exclusions and expectation changes. |

Each item is keyed by its name. A plain status is enough for an ordinary entry;
an entry with a gap or decision can also have notes. Repository, revision and
path roots are recorded once rather than repeated on every item. Paths in each
file are relative to its declared root. Test source keys start with `execution/`
or `packages/`, using the two upstream roots declared in that file; compiler
selectors use `compiler_root`. A rare `repo:` selector is repository-relative.

## What the statuses mean

**Language and parser:** `supported`, `partial`, `missing`, `unassessed`, or `excluded`.
These are maintained assessments, not inferred test outcomes. Excluded capabilities
are intentional compiler differences, not pending porting work. Partial, missing,
and excluded entries must explain their gap or decision. Runtime trait dispatch
and runtime reflection/dynamic execution are excluded from the compiler's language model.
Parser entries concern accepting and interpreting
syntax; language entries concern typing and execution. Replacing parser-file
mappings does not establish syntax support: entries without a separate review
are explicitly unassessed.

The parser assessment covers current source acceptance and written syntax at
the pin in `sources.yaml`. Ahead-of-time typing, name resolution, execution and
source-unit composition differences are explicitly out of bounds and are not
parser compatibility issues. Unicode source handling and positions, runtime
parser APIs, and historical accounting are outside this review. A reviewed
status can be based on source comparison and focused syntax evidence; it does
not claim exhaustive differential execution of both parsers.

**StdLib and packages:** `identical`, `different`, `missing`, or `ambiguous`.
These compare each declaration's own text with a same-name compiler counterpart.
They do not claim behavioral compatibility. Internal comments, whitespace and
line endings are significant; surrounding blank lines and comments are excluded.
A helper change does not change the comparison of its callers. Repeated names
are disambiguated by declaration kind and, only when necessary, source line.
Compiler-only declarations are outside the upstream port inventory.

**Builtins:** `present` means a compiler binding was identified; `missing` means
none was identified; `unassessed` means an existing wrapper counterpart needs
review before claiming a binding. Presence does not establish signature or
behavioral compatibility. Function names include their version; values have a
`value/` prefix. Computed registrations that cannot be enumerated safely are
reported during generation for review rather than silently counted as supported.

**Tests:** `enabled`, `disabled`, `partial`, `unmapped`, or `ambiguous`. Enabled
means included in the compiler suite, not executed successfully by this update.
Partial means only part of an upstream assertion group has counterparts. Disabled
counterparts retain the suite's exclusion reason; changed compiler expectations
remain explicit. Tests matched by name with different bodies carry a note.
Unmapped and ambiguous tests need investigation. Compiler-only tests remain in
the test suite, not this ledger.

The ledger stores no per-item test lists, paths to compiler implementations,
signatures, inferred coverage counts, file inventories, or dependency graphs.
Existing sources and tests own those facts. Compilation receipts belong to their
run, separately from port status; compiling a containing file does not establish
that each declaration behaves correctly.

## Update and check

Use an interpreter checkout at the revision in
[sources.yaml](../../porting/sources.yaml). From the compiler repository root:

```bash
python3 -m pip install -r scripts/porting-requirements.txt
python3 scripts/update-ledger.py --upstream /path/to/interpreter
python3 scripts/update-ledger.py --upstream /path/to/interpreter --check
```

Generation reads committed pinned upstream sources and current compiler sources.
It writes compact YAML inventories, a summary, and the Markdown status page.
Neither command builds the compiler or runs tests. Unfamiliar source forms are
printed for review; resolve them in the scanner or exceptional mappings.

Edit `porting/language-features.yaml` and `porting/parser.yaml` for reviewed
capabilities. Edit `porting/ledger-inputs.yaml` only for exceptional declaration
source mappings, builtin binding mappings, test mappings or gap explanations.
Change exclusions in the actual suite, then regenerate. Ordinary updates need
no model and do not require fixing every recorded gap.

Optional package compilation still uses original pinned source files and their
computed dependencies. After building the native adapter, run the generator with
`--compile-packages` to write `porting/package-results.json`, a separate run
receipt. Dependencies are compilation inputs, not declaration identities or
maintained compatibility entries.

Source discovery is limited to the inputs needed by these compact ledgers.
There is no separate file-inventory generator, policy framework, or report CLI.

Compiler libraries live under `packages/`, with StdLib under
`packages/darklang/stdlib/`, following the interpreter's directory casing.
Existing split modules and compiler-only helpers remain beside their packages.
Directory moves update exceptional source mappings; they do not change the
declaration statuses or add compiler paths to the generated ledgers.
