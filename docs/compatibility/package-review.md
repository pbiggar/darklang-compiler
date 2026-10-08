# Interpreter package review registry

This registry records decisions and unresolved items from the item-by-item
review of interpreter packages. It does not change compiler behavior or test
selection. Review each new failure with the user before implementing a fix.

Sources: interpreter v0.0.35, commit
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`; compiler main commit
`aeef8bc93a4ff7f21c157625186283301ef7adef`.

## Deferred category: existing compiler implementation, interpreter builtin call

The interpreter package calls a `Builtin.*` entry point, but the compiler has
its own implementation of the corresponding public API. Per user direction,
record the mismatch and skip the affected package for now. Do not add adapters
or move existing implementations merely to make the original wrapper compile.

A source-level inventory identified 497 matching declarations across numeric,
collection, text, encoding, date/time, host, networking, and other modules.
These are review candidates, not 497 confirmed compilation failures or claims
of behavioral equivalence. Classify actual failures as each package is visited.
Entries using the same builtin in both implementations are not in this category.

## Decisions

| Package file | Item | Status | Reason and evidence |
|---|---|---|---|
| `packages/darklang/stdlib/base64.dark` | `Darklang.Stdlib.Base64.decode` | Package skipped; unresolved | Original body calls missing `Builtin.base64Decode`. Direct builtin probe fails with `Unknown function or value 'Builtin.base64Decode'`; compiler `Stdlib.Base64.decode "SGVsbG8="` compiles and runs. User deferred this category. |
| `packages/darklang/stdlib/cli/bash.dark` | `Darklang.Stdlib.Cli.Bash.overwriteBashrc` | Fixed and merged | Canonicalized Env and Cli.FileSystem module names, including FileError, and added qualified Option.Option/Result.Result type fallbacks. Original Bash package compiles. PR #14 merged to main at `aeef8bc93a4ff7f21c157625186283301ef7adef`. |
| `packages/darklang/stdlib/localStore.dark` | `Darklang.Stdlib.LocalStore.path` | Package skipped; missing SQLite support | Calls unavailable `Builtin.localDbPath`, which the interpreter implements using `LibDB.Sqlite.currentDbPath`. User classified this as missing SQLite support and directed deferral. Other LocalStore items remain unreviewed individually. |
| `packages/darklang/languageTools/interpreterStats.dark` | `Darklang.LanguageTools.InterpreterStats.reset` | Package skipped; missing interpreter instrumentation support | Original package compilation fails with `Unknown function or value 'Builtin.interpreterStatsReset'`. Interpreter implementation resets `vm.stats` and enables counting; compiler has no corresponding builtin or interpreter VM counters. User directed recording and skipping this package. Other items remain unreviewed individually. |

The Base64 package also contains `encode` and `urlEncode`, which call
`Builtin.base64Encode` and `Builtin.base64UrlEncode`; compiler implementations
already exist. These remain unreviewed individually because the package is
skipped as a whole. Nothing in this package is marked fixed.

## Most recent fix

`packages/darklang/languageTools/permissions.dark`:
`Darklang.LanguageTools.Permissions.tokens` previously failed with
`Unknown function or value 'walk'`. Local self-recursion detection now traverses
all written expression containers and recognizes function values, respecting
match, lambda and destructured-let shadowing. Recursive inference and closure
lowering also support those forms, including callbacks stored in lists.
Standard-library wrappers now release owned function results.

Branch `chatgpt/local-recursion-match`, commit
`5db9fdf8cd52e3e453ebb3f10817ef2c0dd65244`; PR #16 merged to main at
`df5fbb9079df2415c31ec9704faa792b66b8cbe5`.
Five match regressions and 23 expression/value/ownership regressions pass.
The unchanged tokens function and TokenMode type compile and run in isolation.
Full host suite: 11163/11163 passed. Native `dune runtest` passed after removing
generated read-only action stamps. All 58 quick/full benchmark workloads compile
and run with leak checks clean. Cachegrind equivalence was waived by the user.
Changed OCaml files pass formatting checks. Recursive function values can
allocate a closure; direct recursive calls retain their existing environment.

## Generic function-value fix

`Darklang.LanguageTools.Permissions.parseRule` failed when its HTTP branches
passed generic `Stdlib.List.singleton` directly to `Stdlib.Option.map`.
Fixed on branch `chatgpt/generic-function-values`, commit
`f7db8bb3bee2c455e3c60debd7f6bc8dc50cfc44`, PR #20 merged to main at
`21c0ec108d32cf019e2f4eb1e86cf98ec4d9d4a6`.

Checked function values now retain their type arguments and function type.
Per-reference fresh variables collect contextual and later-use constraints;
type substitution and monomorphization materialize the concrete callback.
Pure generic module function aliases retain the original identity and
specialize at each use. Explicit type arguments without value arguments form
specialized function values.

All 24 focused regressions pass, covering the 17 failing audit probes below,
explicit function-value specialization, ordinary-call controls, alias use at
two types, specialization identity equality, and wrong-argument rejection.
Full host suite: 11386/11386 passed. Native `dune runtest` and changed-file
formatting checks passed. All 58 canonical quick/full benchmark workloads
compile and run with leak checks clean; Cachegrind equivalence remains waived.

The whole unchanged original Permissions package compiles. An entry calling
`Permissions.parseRule ["http", "GET"]` runs with exit 0 and no leaks.
The missing closure-target blocker is resolved; other package declarations
have not been reviewed individually at runtime.

Local value annotations remain rejected, as in the pinned interpreter
v0.0.35: its parser explicitly rejects them and WrittenTypes.ELet has no
annotation field. This is a separate language syntax restriction, not a
remaining generic function-value compilation failure.

## Pre-fix generic function-value audit

Checked 22 focused probes against compiler main `df5fbb9079df2415c31ec9704faa792b66b8cbe5`.
Seventeen generic function-value probes failed compilation:

| Context | Observed result |
|---|---|
| User, qualified user, and library generic callbacks | Missing closure target |
| Callback passed to a user higher-order function | Missing closure target |
| Local alias; alias captured by a local function | Undefined function identity or lambda return inference failure |
| Module value; returned function value | Missing closure target |
| Lists, tuples, and record function fields | Undefined function identity |
| Option and dictionary payloads | Missing closure target |
| Conditional function value | Undefined function identity |
| Same generic callback at Int64 and String | Missing closure target |
| Generic callback inside another generic function body | Missing closure target |
| Callback after an earlier direct call already instantiated the generic | Missing closure target |

Direct generic calls, direct pipe calls, and lambda wrappers compiled and ran
with exit 0 and no leak diagnostics. Two additional probes were rejected for
separate reasons: local value annotations are unsupported; a bare
`identity<Int64>` is treated as a call with missing arguments rather than a
specialized function value. These two are not counted as confirmed instances
of the missing-specialization failure.

The checked function-reference node carries only a function identity.
Specialization discovery, specialization replacement and type substitution all
leave it untouched. The fix must preserve type arguments on function values,
substitute them inside generic bodies, and materialize the concrete targets.
Aliases also need inference from subsequent uses; expected callback types alone
do not cover the tested scope. Expression-container traversal for ordinary
generic calls is already present. No compiler or package changes were made
during that initial coverage investigation; the subsequent fix above resolves
the 17 generic function-value probes and bare explicit specialization.

## Completion rule

At the end of the package review, enumerate every skipped package and unresolved
function, test, or value, retaining the reason and the user's decision. A source
inventory match alone must not be reported as a passing compile or runtime test.
