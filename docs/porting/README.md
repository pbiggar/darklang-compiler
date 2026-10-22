# Understanding the compiler port

The [current status](generated/ledger.md) counts declarations and tests directly
from the complete imported interpreter corpus in [`upstream/`](../../upstream/).
Every `.dark` file under interpreter `packages/` and
`backend/testfiles/execution/` is included at the revision in
[`sources.yaml`](../../porting/sources.yaml).

Compiler decisions live beside each imported item. There are no file-level
exclusions or separately maintained test/declaration ledgers:

- No annotation selects the original declaration or test.
- `// @compiler blocked: tag` retains an item awaiting compiler support.
- `// @compiler excluded: tag` records an intentional compiler exclusion.
- `// @compiler replace: packages/path.dark#Qualified.name reason=tag` selects
  a separately maintained compiler implementation with a checked public contract.
- `// @compiler xfail compile: "diagnostic" reason=tag` runs a test that must
  currently fail compilation with the specified diagnostic. `runtime` requires
  a runtime failure instead. Unexpected passes, wrong phases, unrelated output
  and abnormal process termination fail the check.

Reasons are short tags from the [glossary](../../porting/compiler-reasons.json).
The [import guide](upstream-import.md) describes source selection and validation.
Annotations are real lexer comments; marker text in strings and documentation
examples is not metadata. Named tests containing multiple assertions remain one
case. Test discovery and counting do not require successful compilation.

Existing adapted fixtures remain compiler regression tests. They are not used
to infer counterparts, partial test coverage or ambiguous source equivalence.
Execution receipts from `--results-json=...` belong to their individual runs;
source dispositions alone never claim successful execution.

## Update and check

Build the native adapters with `./build --ai`, then run:

```bash
python3 scripts/update-ledger.py
python3 scripts/update-ledger.py --check
```

Use `--build-dir=...` for a separate Dune directory. To verify that every imported
source body is byte-for-byte identical to the pin after removing only actual
annotation comments:

```bash
python3 scripts/update-ledger.py --check --upstream /path/to/interpreter
```

The check reads committed Git blobs, ignoring dirty and untracked interpreter
files, and verifies the complete source scope. `scripts/check-porting` runs the
helper tests and obtains an isolated pinned checkout when none is supplied.
The only generated source ledger artifacts are aggregate counts and the status
page; names, compiler targets and per-item dispositions remain in the sources.

Language and parser capability assessments remain in
[`language-features.yaml`](../../porting/language-features.yaml) and
[`parser.yaml`](../../porting/parser.yaml). Partial, missing and excluded
capabilities require explanatory notes. Interpreter host builtin assessments
and their explicit binding locations remain separately maintained in
[`builtins.yaml`](../../porting/builtins.yaml); host-language F# tests are outside
this Dark source import.

The native `check_sources` tool validates the selected original bodies and all
replacement contracts together; it runs as part of ledger updates and Dune
regression checks. `probe_helpers` can check blocked test functions, types and
values against the current library to identify portable candidates. A candidate
must also pass the complete selection check before its annotation is removed:
constructor identities and other constraints can conflict across files.

When `--trial-upstream` and `--results-json=PATH` are supplied together, the
runner also flushes each completed outcome to `PATH.jsonl` and logs the current
test before compiling it. These are execution receipts for investigation, not a
maintained ledger. They preserve completed results if an unsupported test hangs
or terminates the compiler process.
