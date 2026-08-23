# Anti-Patterns to Avoid

This repository favors direct, explicit compiler behavior over defensive
workarounds that can hide real bugs. When code reaches an unexpected state,
prefer making that state visible immediately with `Crash.crash` or
`Crash.TODO()` rather than continuing with an invented fallback.

## Unnecessary Workarounds

Avoid adding compatibility shims, special cases, or recovery paths just to keep
the compiler moving past a broken or unsupported case. If something does not
work yet, crash with a clear message that explains the unsupported path.

Do not preserve defaults, shims, or special-case branches as maintained
workarounds. Compatibility is acceptable only as explicitly documented,
first-class supported behavior, rather than an undocumented fallback.

## Imprecise State Modeling

Do not represent invalid combinations with loosely related fields, flags, or
optional values. Define precise domain types and keep values that must vary
together in the same structural representation, so invalid combinations cannot
be constructed.

Use `Option` only when absence is a genuine part of the domain. When reviewing
an optional field, ask whether a discriminated union, a required field in the
relevant case, or removing the field altogether more accurately represents the
state.

## Incomplete Migrations

Do not leave both old and new representations, conversion paths, or callers in
place after a migration is complete. Finish the migration with one authoritative
representation and remove superseded code. If old behavior must remain
supported, document that compatibility explicitly and test it as a supported
behavior.

## Default Values for Unexpected Situations

Never fill in missing or unexpected compiler state with a convenient default
value. Defaults can turn one clear bug into a later, harder-to-debug codegen or
runtime failure. Model the state explicitly when possible; otherwise crash at
the point where the unknown value is discovered.

## Tests for Command-Line Plumbing

Avoid tests whose only purpose is to cover command-line argument parsing, test
runners, or test harness behavior. Compiler tests should mostly be end-to-end
language tests that demonstrate user-visible compiler behavior.

## Tests for Removed Features

Do not add tests that only prove removed features are still absent. Removed
syntax or behavior should not keep accumulating permanent negative tests unless
there is a concrete regression risk in an active migration.

## Awkward Tests for the Sake of Coverage

Avoid tests that exist only to increase test count or coverage without
describing meaningful compiler behavior. Prefer the smallest end-to-end example
that would fail if the intended behavior regressed.

## Parsing Unstructured Text

Do not parse logs, human-readable command output, or compiler dumps when
structured output is available or can reasonably be added. Prefer typed values,
explicit result types, JSON, or other structured formats over fragile string
matching.
