# Importing the pinned upstream corpus

The import is being migrated to a direct inventory of upstream source items.
Parsing and inventory discovery do not require type checking or executing their
bodies. The source pin remains in `porting/sources.yaml`.

After `./build --ai`, check the entire pinned Dark source corpus with:

```bash
python3 scripts/check-upstream-parsing.py --upstream /path/to/interpreter
```

This reads committed blobs at the pin, including all `.dark` files under
`packages/` and `backend/testfiles/execution/`. It ignores dirty and untracked
checkout files. The report under `TestResults/porting/` accounts for every
selected file and lists its parsed declarations and tests. Types, functions,
`val`s, traits, implementations and their members are included; a named test
containing multiple assertions is still one test. Interpreter host tests written
in F# are outside this Dark source import.

## Per-item compiler annotations

Compiler dispositions are ordinary line comments before the affected item,
at the same indentation. There are no file-level annotations. No annotation
means the item is selected normally, not that an execution has passed.

```dark
// @compiler blocked: test-counter
test evaluatesOnce =
  ...

// @compiler excluded: runtime-reflection
let inspect ... = ...

// @compiler replace: compiler/base64.dark#decode reason=interpreter-intrinsic
let decode ... = ...

// @compiler xfail compile: "Unknown function" reason=test-counter
test unavailable =
  ...
```

Reasons are short tags defined in `porting/compiler-reasons.json`. Annotation
validation rejects unknown tags, duplicate dispositions, annotations inside
bodies or after an item, and orphaned annotations. Only real line comments are
read: text inside strings, doc comments and block comments is not metadata.
Expected failures apply only to tests and require a nonempty diagnostic matcher
and a compile/runtime phase. Replacement targets are repository-relative
`path#declaration` references and require a reason.

The annotation reader and source inventory are implemented first. Replacement
selection, contract checking, execution of expected failures, the complete
annotated source import and replacement of the old ledger are subsequent parts
of this migration. The existing test runner and ledger remain authoritative
until those parts are integrated; this parse-only report is not an execution
receipt.
