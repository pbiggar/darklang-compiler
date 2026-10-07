# Interpreter package review registry

This registry records decisions and unresolved items from the item-by-item
review of interpreter packages. It does not change compiler behavior or test
selection. Review each new failure with the user before implementing a fix.

Sources: interpreter v0.0.35, commit
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`; compiler reversion commit
`2482b54175a696341b00e331725ab62635c1c776`.

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

The Base64 package also contains `encode` and `urlEncode`, which call
`Builtin.base64Encode` and `Builtin.base64UrlEncode`; compiler implementations
already exist. These remain unreviewed individually because the package is
skipped as a whole. Nothing in this package is marked fixed.

## Current review item

`packages/darklang/stdlib/cli/bash.dark`:
`Darklang.Stdlib.Cli.Bash.overwriteBashrc` uses the sibling spelling
`FileSystem.overwriteFile`, which fails name resolution. The same isolated
function compiles when both `FileSystem` references are written as
`Stdlib.Cli.FileSystem`. Investigation is read-only; no fix is approved.

## Completion rule

At the end of the package review, enumerate every skipped package and unresolved
function, test, or value, retaining the reason and the user's decision. A source
inventory match alone must not be reported as a passing compile or runtime test.
