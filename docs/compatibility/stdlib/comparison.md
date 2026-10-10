# Equality, ordering, and comparability parity

Current compiler source review: 2026-10-07 at `7154b0ea9c1a3f53d30984ed17b9e0cc5d8f0dce`.
See the [current audit](../current-audit.md) for post-port status and validation.
Older revision pairs and executed counts below are historical evidence, not
a fresh test result for this revision.

The public prelude contract was revalidated between compiler starting HEAD
`c609b56ce1ec488afc3146c585b6f45a2fcf22a8` and darklang/dark
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. Implementation comparison commit
`ab81ead7f4b232b4ffa181d8ddb71e9381c510c8` was tested against that same exact
interpreter revision. Compiler evidence revision
`51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3`, DCB1 report commit `8a402797`, and
the previous parity document were starting evidence only; every retained
prelude finding was checked again at the exact revisions above.

JSON's separate serialization and parsing parity boundary is documented in
[the JSON ledger](json.md).

At the implementation comparison commit the focused matrix passed 140/140
cases. Performance is outside this contract unless it changes observable
behavior.

The executable matrix is `test/fixtures/e2e/comparison-parity.e2e`. It covers the
operators `==`, `!=`, `<`, `>`, `<=`, and `>=` plus the unversioned public
`Stdlib.equals` and `Stdlib.notEquals` functions.

## Contract

| Values | Equality | Ordering |
| --- | --- | --- |
| Unit, Boolean, character, string, DateTime | Same type and same value | Rejected |
| Signed and unsigned integers, including 128-bit, and `Int` | Numeric equality within one identical numeric type | Numeric ordering within one identical numeric type |
| Float | IEEE equality | IEEE ordering |
| Tuple and list | Recursive and structural | Rejected |
| Record | Recursive by compatible field layout, including separately named records; assignment remains nominal | Rejected |
| Constructor | Same nominal sum type, variant, and recursively equal payload | Rejected |
| Dict | Same mapping, independent of insertion order or HAMT shape; values compare recursively | Rejected |
| Function | Interpreter identity rules described below | Rejected |
| Blob | Handle identity, recursively inside equality-capable containers | Rejected |
| Stream | Handle identity without pulling, recursively inside equality-capable containers | Rejected |
| RawPtr, RuntimeError | Rejected by the compiler comparison type constraint | Rejected |

`Stdlib.equals<'a>(left, right)` and `Stdlib.notEquals<'a>(left, right)` expose
this exact contract. They are portable Dark definitions over the typed
equality operator; `notEquals` is Boolean negation of `equals`. Concrete AOT
specialization therefore reuses the operator's scalar operations, structural
helpers, function comparators, Dict mapping comparison, and Blob identity. It
does not inspect runtime tags and does not add a backend comparison path.
Arguments are evaluated once each from left to right.

Operands must resolve through aliases to compatible admissible types.
Consequently mixed numeric widths, integer/float pairs, Char/String pairs,
incompatible record field layouts, and distinct nominal sums are rejected.
The direct checker additionally permits equality between separately named
records with recursively compatible field types. Comparison
admissibility is recursive, including nested tuple, list, record, sum, Dict
value, and function types. Dict key admission remains the Dict subsystem's
responsibility; equality uses its existing key semantics without widening the
admitted key set.

Float behavior follows IEEE operations: NaN is unequal to itself and all four
ordering predicates involving NaN are false. Positive and negative zero are
equal. Int128 and UInt128 use numeric comparison after conversion to the
canonical arbitrary-integer implementation, rather than lexical comparison of
their internal decimal strings. Native signed and unsigned conditions remain
selected by the concrete integer type.

For function types used by equality, the AOT compiler adds one raw comparator
pointer to their closures. That pointer is also the semantic identity: ordinary
lambdas receive one per source expression, while named partials reuse one per
resolved specialization and applied-argument shape. The comparator ignores an
ordinary lambda's captures and recursively compares a named partial's already-
applied arguments. Function types that are never compared retain the ordinary
closure layout, so higher-order code pays no comparison-metadata cost. The raw
pointer is unmanaged; reference-count traversal continues to cover only the
operational captures.

Generic equality is a typed plan. It remains in the checked AST while type
variables are unresolved, is substituted during monomorphization, and is
materialized with its concrete helpers after specialization. It therefore has
the same behavior as a direct comparison and performs no runtime type dispatch.

## Failures and deliberate compiler differences

The interpreter reports the semantic failures `Cannot perform equality check
on <left> and <right>` and `Cannot perform numeric operation on <left> and
<right>`. The compiler uses the same language-visible text while deliberately
reporting it during AOT type checking. This phase difference is retained: the
compiler does not evaluate operands merely to reproduce an interpreter runtime
failure. Invalid comparisons are never constant-folded into Boolean values.

RawPtr is a compiler-only representation type and its intrinsic constructor is
not available in user syntax; the type checker nevertheless rejects RawPtr
comparison explicitly. RuntimeError is likewise an internal flow type rather
than an equality value. The interpreter's database-reference category has no native host-service parity
claim. Compiled Streams exist and compare by handle identity, including inside
equality-capable containers; ordering remains rejected. See [Streams](streams.md).

UUID parity was revalidated at compiler `84ecd026ef3ae8dc1dddbb693cd4adba1c94265f`
against darklang/dark `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`
(`packages/darklang/stdlib/uuid.dark`,
`backend/testfiles/execution/stdlib/uuid.dark`, and
`backend/src/Builtins/Builtins.Pure/Libs/Uuid.fs`). The interpreter's DUuid is represented in the compiler as the ordinary Dark
newtype `Uuid = UUID(UInt128)`. Canonical parsing and formatting are defined in
`Stdlib.Uuid`; ordinary sum equality supplies structural UUID equality. The current source exposes canonical `Uuid.parse`; the former public
`Stdlib.Uuid.Compatibility.parse_v0` extension is no longer present.

The post-rebase X-format repair was compared from exact compiler HEAD
`5b3c52db7c53a6c9ed5d3626b97f395b5a6b76d4` against that same exact
darklang/dark revision. At the interpreter revision,
`packages/darklang/stdlib/uuid.dark:4-16` defines the public type and API while
`backend/src/Builtins/Builtins.Pure/Libs/Uuid.fs:43-47` delegates validation to
`System.Guid.TryParse`. A focused probe of that same call established the
observable X-format rules retained in compiler source
`StdLib/Uuid.dark`: Unicode whitespace codepoints are ignored anywhere in X format,
leading zeroes and short fields are accepted, the second and third UInt32
components contribute their low 16 bits, and overflowing UInt32 or byte fields
return `BadFormat`. Focused same-source cases are in
`test/fixtures/e2e/stdlib/uuid.e2e:7-23`. The codepoint filter also removes CRLF
between fields, within `0x`, and between hexadecimal digits, matching Guid's
`EatAllWhitespace` behavior. Whitespace does not make adjacent combining marks
or zero-width spaces valid input; these boundaries are covered by
`test/fixtures/e2e/uuid-whitespace.e2e`.

Ahead-of-time rejection and the absent interpreter-only value categories are
the intentional public boundary retained here.

Performance differences are outside this contract unless they change an
observable result.

Blob identity and its canonical empty value are revision-pinned separately in
[binary and cryptographic compatibility](binary-and-crypto.md).

## Source anchors

The interpreter baseline is implemented in
`backend/src/Builtins/Builtins.Pure/Libs/NoModule.fs` (equality at lines 33-175
and numeric comparison at lines 600-707 in the pinned revision), with function
identity data in `backend/src/LibExecution/RuntimeTypes.fs` around lines
895-935. Public error rendering is in
`packages/darklang/prettyPrinter/runtimeError.dark` around lines 232-242.

Current source enforcement lives in `src/frontend/WrittenOperatorSupport.ml`,
`src/frontend/WrittenTypeSupport.ml`, and
`src/frontend/checking/ComparisonPlanning.ml`. Structural helpers and their
specializations are materialized through the checked preparation and ANF
pipeline; closure layout reaches native code through
`src/passes/mir/MIR_to_LIR.ml`.
Semantic Dict equality is lowered in the type checker through the typed
`Dict.toList` mapping view at
`StdLib/Dict.dark`,
using the layout and key helpers exposed from `src/DarkStdlib.ml`.
Float conditions and architecture-specific integer conditions remain in the
shared MIR-to-LIR pass and the ARM64/x64 backends. Focused executable evidence
is in `test/fixtures/e2e/comparison-parity.e2e`, alongside the existing
`equality.e2e` and `interpreter_behavior_parity.e2e` suites.

The root wrappers are `StdLib/Root.dark`, loaded by
`src/driver/StdlibCompilation.ml`. Separate stdlib specialization merges user
record/sum metadata before materializing structural helpers in
`src/driver/StdlibCompilation.ml`; the indexed record view is built in
`src/frontend/checking/Types.ml`. Public probes are at
`comparison-parity.e2e:35-110,148-156`.
