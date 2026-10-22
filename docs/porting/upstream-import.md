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

The existing fixture runner reads these annotations in both execution tests and
named package tests. Blocked and excluded tests remain discovered and are
skipped before preamble compilation. Expected failures run individually: an
ordinary passing test is an unexpected pass, even when the original test is a
negative test. A failure must match its phase and diagnostic; a signal cannot
satisfy a runtime expected failure. Compiler expectation overrides remain
separate from expected failures and can coexist with an annotation.

## Selecting declarations

`SourceSelection.select` receives parsed source files, root paths and a contract
context. It discards blocked/excluded bodies and resolves replacement targets
before body checking. Targets must preserve the original public identity and
provide a concrete implementation of the same declaration kind. Missing,
ambiguous and duplicate definitions are errors. Compiler-only `__` helpers in
replacement files are included without implicitly importing public siblings.

Function contracts compare resolved argument and return types, generic arity,
bounds and declared effect rows. Generic binder names can differ. Type
replacements preserve public record fields, enum cases and payloads, aliases
and bounds. `val`s have no declared type: their original initializer must have
a type the checker can establish in the supplied context. An unsupported
intrinsic initializer cannot be accepted by assuming its replacement's type.

Unavailable declarations retain their names and reason tags in the selection.
An unavailable trait/implementation member makes its container unavailable;
it never leaves a partially implemented container. Member and whole-container
replacements are not enabled yet; the supported replacement kinds are
functions, types and `val`s.

The complete corpus is imported under `upstream/` and embedded alongside the
compiler implementation sources. Root declarations select their replacements;
explicit compiler supplements retain APIs and helpers that are outside the
pinned interpreter contract. Compiler replacements preserve the source order of
those implementation files so constants remain available to their callers.

Both execution assertions and named tests are discovered from the parsed AST.
Documentation examples in string literals are never tests. A named test is
wrapped only for execution; its body and expected error remain unchanged. A
failed named assertion reports its upstream failure messages before returning a
failed comparison. The imported `Stdlib.Test` package supplies assertion values
and aggregation; the compiler replacements for value-rendering diagnostics are
marked explicitly with `test-diagnostics`.

The ledger now reads these sources directly. It has no per-item YAML test or
declaration mapping and no central exclusion table. Its readiness check rejects
pending test classifications. Compilation and execution receipts remain separate
from the source dispositions.
