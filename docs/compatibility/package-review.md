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
`5db9fdf8cd52e3e453ebb3f10817ef2c0dd65244`; PR #16; not merged.
Five match regressions and 23 expression/value/ownership regressions pass.
The unchanged tokens function and TokenMode type compile and run in isolation.
Full host suite: 11163/11163 passed. Native `dune runtest` passed after removing
generated read-only action stamps. All 58 quick/full benchmark workloads compile
and run with leak checks clean. Cachegrind equivalence was waived by the user.
Changed OCaml files pass formatting checks. Recursive function values can
allocate a closure; direct recursive calls retain their existing environment.

## Remaining package blocker

Compiling the whole original Permissions package, with either a trivial entry
or a call to tokens, now reaches a later backend failure:
`RefCountInsertion: ClosureAlloc target '776' not found in function registry`.
This does not occur with tokens and TokenMode alone. The failing declaration
has not yet been isolated; no fix for this separate failure is approved.

## Completion rule

At the end of the package review, enumerate every skipped package and unresolved
function, test, or value, retaining the reason and the user's decision. A source
inventory match alone must not be reported as a passing compile or runtime test.
