# Importing the pinned upstream corpus

The import uses a direct inventory of upstream source items.
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

// @compiler blocked: upstream-dependency diagnostic="Unknown function or value 'Stdlib.LocalStore.path'"
let localDb () : String = Stdlib.LocalStore.path ()

// @compiler excluded: runtime-reflection
let inspect ... = ...

// @compiler replace: packages/darklang/stdlib/base64.dark#Darklang.Stdlib.Base64.decode reason=interpreter-intrinsic
let decode ... = ...

// @compiler xfail compile: "Unknown function" class=missing-support reason=test-counter
test unavailable =
  ...
```

Reasons are short tags defined in `porting/compiler-reasons.json`. Annotation
validation rejects unknown tags, duplicate dispositions, annotations inside
bodies or after an item, and orphaned annotations. Only real line comments are
read: text inside strings, doc comments and block comments is not metadata.
Blocked items may carry a nonempty JSON-quoted `diagnostic=` field recording
the observed compiler error. It is investigation evidence, not an executed
expected-failure contract. The native inventory preserves it. Use this to name
the actual unavailable dependency or compiler limitation rather than treating
an imported implementation as absent.
Expected failures apply only to tests and require a nonempty diagnostic matcher
and a compile/runtime phase, plus exactly one `class=aot-rejection` or
`class=missing-support` field before `reason=TAG`. An AOT classification with a
runtime phase is invalid. Replacement targets are repository-relative
`path#declaration` references and require a reason.

The existing fixture runner reads these annotations in both execution tests and
named package tests. Blocked and excluded tests remain discovered and are
skipped before preamble compilation. Expected failures run individually: an
ordinary passing test is an unexpected pass, even when the original test is a
negative test. A failure must match its phase and diagnostic; a signal cannot
satisfy a runtime expected failure. Compiler expectation overrides remain
separate from expected failures and can coexist with an annotation.

## Classifying expected failures

Read the original test, its selected dependencies and the actual diagnostic;
neither `raises` nor compilation failure alone determines the class.

- `aot-rejection`: the compiler rejects a program invalid under its intended
  static rules, and the diagnostic identifies the invalid construct. Use the
  specific `aot-*` reason describing that rule. For example, `true && 5L` is a
  type error; a match missing enum alternatives is non-exhaustive; a dictionary
  key containing a function is invalid. These remain individually executed
  rejection contracts with strict phase and diagnostic matching.
- `missing-support`: the observed failure represents incomplete or incorrect
  compiler/runtime/runner behavior, or prevents us from verifying the intended
  rejection. An undefined name caused by a blocked declaration is missing
  support, even if the eventual test expects a type error. `valDictSetRejectsWrongKeyType`
  is one example: the unavailable `valDict` currently prevents checking its
  invalid key. An enum-inference diagnostic on `None`, or failure to infer a
  lambda's type before checking its dictionary-key validity, also remains here.

Static validation checks all branches, match arms and callback types, including
code that the interpreter never executes. Consequently `filter [] (fun _ ->
"a")` is an AOT type rejection, and a named test containing that expression is
classified as a whole. `Stdlib.Test.raiseRuntimeError` explicitly returns
`Int64`; using it as a Bool, tuple or differently typed match arm is likewise a
static type error despite its never returning at runtime. A genuinely missing
helper is a support gap. Duplicate record *updates* that the interpreter
accepts, unresolved generic specialization, receiver methods and nominal versus
content-addressed type behavior remain support differences; this audit does
not establish new language exclusions for them.

Do not promote a test merely because its name sounds negative. For example,
`ambiguousReceiverRaises` still fails on generic field access rather than
checking trait-method ambiguity, and remains missing support. Conversely a
reference to the deliberately nonexistent `functionWhichDoesntExist` is an AOT
name rejection, not a missing upstream dependency. Nested forward calls are
outside the supported scope in both implementations and are name rejections.

When reclassifying, preserve the pinned source body. Change only the real
annotation comment, require the relevant diagnostic, and regenerate the ledger.
Signals, timeouts, leaks, unrelated errors and unexpected passes cannot satisfy
either class. AOT annotations describe verified *contracts*; only execution
receipts establish that a particular revision actually met those contracts.

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
An explicit exclusion suppresses matching declarations in compiler supplements
as well, so an excluded API cannot become callable through that layer.

`probe_helpers --packages ROOT MANIFEST REPORT` probes blocked package
functions, types and values against the selected library. It retries candidates
as dependencies become available and checks the accepted declarations together,
removing attributable combined failures and retrying their dependents. This is
a type-checking probe; promoting declarations still requires checking the real
source selection, native compilation and the test suite. The default invocation
without `--packages` retains the test-helper probe.

`check_sources --native ROOT` runs the selected on-disk declarations through
the host native library pipeline, including specialization and allocation.
This catches failures that source checking alone cannot establish. Run it before
embedding a newly enabled batch, then rebuild and run the complete host suite.
Executable script bodies remain blocked with `script-entrypoint`: importing a
script does not make its entry expressions reusable library declarations.

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

Application compilation selects upstream library packages. The runner builds a
separate imported-test library that also includes the selected original test
helpers, types and values. Each imported test uses that library context. This
keeps unrelated test constructors and helper functions out of application scope;
all imported tests remain discovered and controlled by their own annotations.
