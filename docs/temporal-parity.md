# DateTime and Duration parity

This document records the temporal parity contract implemented by compiler
revision `882f633f4f01af85b89bd847c686d94815321955`. The comparison baseline is
darklang/dark revision `04fbe9dcc995c6188757d583e273cbd30a3e2d3d` from
2026-08-10. The pre-change compiler HEAD used to reproduce every retained gap
was `69f92d2f9cd8d47476c0123557175387e411366c`.

Integration rebased that implementation without semantic changes onto compiler
main `1f5282a113b2abbc15622d5ad7187fb29199964b`, producing implementation
revision `493d51575a842c6f38fb3e3721fa44518c834d6a`. The complete post-rebase
conformance run used compiler HEAD
`449b8a8bafe519fcd5c3f6778ddeddbb34cdb9f9` and the same exact interpreter
revision above.

The historical DCB1 report at
`8a402797ccccda0ca47b516b356ae1de4d670038`, the earlier compiler snapshots
`51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3` and
`87a850eed3c5298800df254ebb05d46684e0bd2c`, and their line references were
starting evidence only. Every statement below was rechecked against the exact
revisions above. Interpreter range constants were also checked against the
NodaTime 3.2.2 source at `e56daa2599a4d1917065c015bda923e7a352a236`.

Performance is outside this comparison except where it changes a returned
value or error.

## Representation and type boundary

`DateTime` is a primitive AST type, not an alias for `Int64`. Its runtime
payload is one signed machine word containing 100ns ticks since the Unix epoch.
The payload is normalized and limited to the pinned interpreter's NodaTime
`Instant` interval:

- minimum: `-3776735808000000000` ticks
  (`-9998-01-01T00:00:00Z`);
- maximum: `2534023007999999999` ticks
  (`9999-12-31T23:59:59.9999999Z` at compiler precision).

The distinct type is retained through parsing, name and alias resolution, type
checking and inference, monomorphization, ANF/MIR/LIR, ownership shape and
register allocation. It has immediate one-word layout in both backends, but
sharing a layout does not permit a source-level conversion. Equality is
available only between two `DateTime` values. Passing a `DateTime` to `Int` or
`Int64`, or either integer type to `DateTime`, is rejected statically.

The compiler's `Stdlib.DateTime.__fromUnixTimeTicks` and
`__toUnixTimeTicks` names are implementation intrinsics used by the portable
stdlib and deterministic internal tests. The double-underscore convention is a
compiler-only implementation surface; it is not part of the canonical public
DateTime API.

## Canonical public API

The compiler exposes the same unversioned wrapper names and numeric types as
the pinned package `packages/darklang/stdlib/dateTime.dark`. Obsolete public
`_v0` aliases were removed.

| Function family | Signature |
| --- | --- |
| `parse` | `String -> Result<DateTime, String>` |
| `toString`, `toStringISO8601BasicDate`, `toStringISO8601BasicDateTime` | `DateTime -> String` |
| `now`, `today` | `Unit -> DateTime` |
| `fromSeconds`, `fromMilliseconds` | `Int -> DateTime` |
| `toSeconds`, `toMilliseconds` | `DateTime -> Int` |
| `addSeconds`, `subtractSeconds`, `addMilliseconds`, `subtractMilliseconds` | `DateTime -> Int -> DateTime` |
| `year`, `month`, `day`, `weekday`, `hour`, `minute`, `second`, `millisecond` | `DateTime -> Int` |
| `atStartOfDay` | `DateTime -> DateTime` |
| `subtract`, `subtractMs` | `DateTime -> DateTime -> Int` |
| `lessThan`, `lessThanOrEqualTo`, `greaterThan`, `greaterThanOrEqualTo` | `DateTime -> DateTime -> Bool` |

All numeric parameters cross the native tick boundary through checked
arbitrary-precision `Int` to `Int64` conversion. Epoch values, fields and
differences return canonical `Int` values.

## DateTime behavior

Epoch extraction floors toward negative infinity before conversion. Thus an
instant one tick before the epoch has both `toMilliseconds = -1` and
`toSeconds = -1`. Construction accepts the full operation-specific NodaTime
range. A value outside `Int64`, outside the NodaTime interval, or arithmetic
that leaves that interval produces exactly:

```text
Encountered out-of-range value for type of Int
```

Calendar calculations use the proleptic Gregorian calendar in UTC and do not
read host timezone or locale data. Weekdays use ISO numbering, Monday `1`
through Sunday `7`. Parsing accepts exactly either
`yyyy-MM-ddTHH:mm:ssZ` or `yyyy-MM-ddTHH:mm:ss.fffZ`, validates leap years and
field ranges, and returns `Error("Invalid date format")` for every rejected
shape. Formatting emits the three-digit fractional portion only when the
millisecond is nonzero, matching the interpreter's public formatter.

`subtract` and `subtractMs` round the exact tick difference to the nearest
second or millisecond using midpoint-to-even rounding. DateTime equality uses
all stored ticks, so two unequal sub-millisecond instants do not collapse to
the same value even when an epoch conversion floors them to the same integer.

The native clock uses direct UTC host syscalls: `gettimeofday` on macOS and
`clock_gettime(CLOCK_REALTIME)` on Linux. macOS retains microsecond precision;
Linux ARM64 and x86-64 retain 100ns precision after nanosecond conversion.
`today` is the UTC start of the day containing `now`.

## Duration.parse behavior

`Stdlib.Duration.parse : String -> Result<Int, String>` interprets the last
character as a unit and the preceding substring with canonical `Int.parse`.
Recognized lowercase suffixes are `s`, `m`, `h`, and `d`, with arbitrary-
precision factors `1`, `60`, `3600`, and `86400`. `Int.parse` accepts an
optional `+` or `-` and leading or trailing ASCII numeric whitespace; internal
whitespace remains invalid unless it is trailing whitespace in the number
portion immediately before the suffix.

Errors are selected in this order and preserve the original input and final
unit character exactly:

1. Fewer than two characters:
   `Duration too short: '<input>'; need a number + unit (s/m/h/d), e.g. 5m`
2. Invalid numeric prefix:
   `Invalid duration '<input>'; need a number + unit (s/m/h/d), e.g. 5m`
3. Valid number with unknown suffix:
   `Unknown unit '<unit>' in '<input>'; use s, m, h, or d`

## Conformance evidence

| Contract | Compiler evidence at `882f633f…` | Interpreter evidence at `04fbe9dc…` | Result |
| --- | --- | --- | --- |
| Distinct value/type traversal | `AST.fs`, `1_InterpreterParser.fs`, `1.5_TypeChecking.fs`, `ANF.fs`, `2_AST_to_ANF.fs`, `2.5_RefCountInsertion.fs`, `4_MIR_to_LIR.fs`, `5_RegisterAllocation.fs` | `backend/src/LibExecution/RuntimeTypes.fs:133-153,948-969` | Aligned |
| Clock and tick precision | `Runtime.fs:2450-2525`, `passes/x64/6_CodeGen.fs:4102-4124` | `Builtins.Time/Libs/DateTime.fs:14-45`, `DarkDateTime.fs` | Aligned to available host precision |
| Public signatures and behavior | `stdlib/DateTime.dark:1-390` | `packages/darklang/stdlib/dateTime.dark:4-136`, `Builtins.Pure/Libs/DateTime.fs:13-536` | Aligned |
| Duration grammar and errors | `stdlib/Duration.dark:1-21`, `stdlib/Int.dark:399-416` | `packages/darklang/stdlib/duration.dark:6-28`, `Builtins.Pure/Libs/Int.fs:359-379` | Aligned for the documented grammar |
| Same-source DateTime corpus | `src/Tests/e2e/upstream/stdlib/date.dark`; no line allowlist | `backend/testfiles/execution/stdlib/date.dark` | Exact file, 222/222 compiler cases pass |
| Same-source Duration corpus | `src/Tests/e2e/upstream/stdlib/duration.dark`; no line allowlist | `backend/testfiles/execution/stdlib/duration.dark` | Exact file, 10/10 compiler cases pass |
| Type, range, rounding and grammar probes | `src/Tests/e2e/temporal-parity.e2e`, `stdlib-internal/datetime.e2e` | Focused expectations derived from the pinned builtins and NodaTime constants | 29/29 pass |
| Non-host x64 syscall lowering | `X86_64CodeGenTests.fs` DateTimeNow test | N/A | 100ns conversion inspected directly |

The copied date and duration fixtures have SHA-256 values
`3939d8b7e1d28fa82c381d287b54901913894d4b991cb3b82fc586073660dbbc`
and `303dcdc71d922f286d37454a024cdfd31d740aad52b3a141f72f9d41b6c403d0`
respectively, identical to the pinned interpreter files.

## Intentional AOT differences

- The compiler rejects type mismatches during compilation. The interpreter
  represents DateTime distinctly but may report an invalid application later
  while evaluating it. The accepted value boundary is the same; the rejection
  phase is intentionally earlier.
- The AOT binary reads the host clock directly because it has no interpreter
  capability/runtime service boundary. This is the only retained DateTime
  host-access extension; conversions themselves remain UTC, locale-free and
  timezone-free.
- Compiler syntax retains `I` as an explicit arbitrary-precision integer
  suffix. Current interpreter syntax uses unsuffixed `Int`; the canonical
  parser does the same. This spelling extension does not change the
  canonical DateTime or Duration signatures.
