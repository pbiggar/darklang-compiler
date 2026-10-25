# DateTime and Duration parity

Semantic comparison baseline: `darklang/dark@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
The [source ledger](../../porting/README.md) owns current support and test dispositions.

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

The compiler exposes the same wrapper names and numeric types as the pinned
package `packages/darklang/stdlib/dateTime.dark`.

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

| Contract | Compiler evidence | Interpreter evidence at `04fbe9dc…` | Result |
| --- | --- | --- | --- |
| Distinct value/type traversal | `src/AST.ml`, `src/frontend/interpreter/Parser.ml`, `src/frontend/WrittenChecking.ml`, `src/ir/anf/ANF.ml`, `src/passes/anf/AST_to_ANF.ml`, `src/passes/anf/RefCountInsertion.ml`, `src/passes/mir/MIR_to_LIR.ml`, `src/passes/lir/RegisterAllocation.ml` | `backend/src/LibExecution/RuntimeTypes.fs:133-153,948-969` | Aligned |
| Clock and tick precision | `src/backend/arm64/instructions/ARM64EmitNativeEffects.ml`, `src/backend/x64/instructions/X64EmitNativeEffects.ml` | `Builtins.Time/Libs/DateTime.fs:14-45`, `DarkDateTime.fs` | Aligned to available host precision |
| Public signatures and behavior | `packages/darklang/stdlib/dateTime.dark` | `packages/darklang/stdlib/dateTime.dark:4-136`, `Builtins.Pure/Libs/DateTime.fs:13-536` | Aligned |
| Duration grammar and errors | `packages/darklang/stdlib/duration.dark`, `packages/darklang/stdlib/int.dark` | `packages/darklang/stdlib/duration.dark:6-28`, `Builtins.Pure/Libs/Int.fs:359-379` | Aligned for the documented grammar |
| Same-source DateTime corpus | `test/fixtures/e2e/upstream/stdlib/date.dark`; no line allowlist | `backend/testfiles/execution/stdlib/date.dark` | Exact file, DateTime behavior cases |
| Same-source Duration corpus | `test/fixtures/e2e/upstream/stdlib/duration.dark`; no line allowlist | `backend/testfiles/execution/stdlib/duration.dark` | Exact file, Duration behavior cases |
| Type, range, rounding and grammar probes | `test/fixtures/e2e/temporal-parity.e2e`, `stdlib-internal/datetime.e2e` | Focused expectations derived from the pinned builtins and NodaTime constants | Type, range, rounding and grammar cases |
| Non-host x64 syscall lowering | `test/compiler-passes/X86_64CodeGenTests.ml` DateTimeNow test | N/A | 100ns conversion inspected directly |

## Intentional AOT differences

- The compiler rejects type mismatches during compilation. The interpreter
  represents DateTime distinctly but may report an invalid application later
  while evaluating it. The accepted value boundary is the same; the rejection
  phase is intentionally earlier.
- The AOT binary reads the host clock directly because it has no interpreter
  capability/runtime service boundary. This is the only retained DateTime
  host-access extension; conversions themselves remain UTC, locale-free and
  timezone-free.
