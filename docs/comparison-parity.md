# Equality, ordering, and comparability parity

The public prelude contract was revalidated between compiler starting HEAD
`c609b56ce1ec488afc3146c585b6f45a2fcf22a8` and darklang/dark
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. Implementation comparison commit
`ab81ead7f4b232b4ffa181d8ddb71e9381c510c8` was tested against that same exact
interpreter revision. Compiler evidence revision
`51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3`, DCB1 report commit `8a402797`, and
the previous parity document were starting evidence only; every retained
prelude finding was checked again at the exact revisions above.

At the implementation comparison commit the focused matrix passed 140/140
cases. Performance is outside this contract unless it changes observable
behavior.

The executable matrix is `src/Tests/e2e/comparison-parity.e2e`. It covers the
operators `==`, `!=`, `<`, `>`, `<=`, and `>=` plus the unversioned public
`Stdlib.equals` and `Stdlib.notEquals` functions.

## Contract

| Values | Equality | Ordering |
| --- | --- | --- |
| Unit, Boolean, character, string, DateTime | Same type and same value | Rejected |
| Signed and unsigned integers, including 128-bit, and `Int` | Numeric equality within one identical numeric type | Numeric ordering within one identical numeric type |
| Float | IEEE equality | IEEE ordering |
| Tuple and list | Recursive and structural | Rejected |
| Record | Recursive and structural within the same nominal record type | Rejected |
| Constructor | Same nominal sum type, variant, and recursively equal payload | Rejected |
| Dict | Same mapping, independent of insertion order or HAMT shape; values compare recursively | Rejected |
| Function | Interpreter identity rules described below | Rejected |
| Blob | Handle identity, recursively inside equality-capable containers | Rejected |
| RawPtr, RuntimeError | Rejected by the compiler comparison type constraint | Rejected |

`Stdlib.equals<'a>(left, right)` and `Stdlib.notEquals<'a>(left, right)` expose
this exact contract. They are portable Dark definitions over the typed
equality operator; `notEquals` is Boolean negation of `equals`. Concrete AOT
specialization therefore reuses the operator's scalar operations, structural
helpers, function comparators, Dict mapping comparison, and Blob identity. It
does not inspect runtime tags and does not add a backend comparison path.
Arguments are evaluated once each from left to right.

Both operands must resolve through aliases to the same admissible type.
Consequently mixed numeric widths, integer/float pairs, Char/String pairs,
distinct nominal records, and distinct nominal sums are rejected. Comparison
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

Generic equality is a typed plan. It remains in the typed AST while type
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
than an equality value. The interpreter's DDB values compare through database
reference equality and Streams compare handle identity; neither value category
exists in compiled programs, so both are interpreter-only and unsupported.

The interpreter's DUuid is a distinct scalar. The compiler currently declares
`Uuid = String`, and its UUID helpers return String. Equality consequently uses
String specialization rather than claiming distinct DUuid parity. This is an
explicit compiler extension recorded in `docs/diff-value-search-parity.md`, not
a new equality rule. Ahead-of-time rejection and the absent interpreter-only
value categories are the intentional public boundary retained here.

Performance differences are outside this contract unless they change an
observable result.

Blob identity and its canonical empty value are revision-pinned separately in
[blob-parity.md](blob-parity.md).

## Source anchors

The interpreter baseline is implemented in
`backend/src/Builtins/Builtins.Pure/Libs/NoModule.fs` (equality at lines 33-175
and numeric comparison at lines 600-707 in the pinned revision), with function
identity data in `backend/src/LibExecution/RuntimeTypes.fs` around lines
895-935. Public error rendering is in
`packages/darklang/prettyPrinter/runtimeError.dark` around lines 232-242.

Compiler enforcement and typed comparison plans live in
`src/DarkCompiler/passes/1.5_TypeChecking.fs` (type errors at 47-121,
classification at 1172-1345, helper construction at 5231-5552, and helper
materialization at 5553-5777). Specialization and structural lowering live in
`src/DarkCompiler/passes/2_AST_to_ANF.fs` (plan materialization at 26-69 and
closure identity and AOT layout selection at 2516-3204), with
post-specialization orchestration in `src/DarkCompiler/CompilerLibrary.fs` at
832-920. Closure layout reaches
native code through `src/DarkCompiler/passes/4_MIR_to_LIR.fs`.
Semantic Dict equality is lowered in the type checker through the public
String-keyed `Dict.toList` mapping view at
`src/DarkCompiler/stdlib/Dict.dark:111-113`,
using the layout and key helpers exposed from `src/DarkCompiler/Stdlib.fs`.
Float conditions and architecture-specific integer conditions remain in the
shared MIR-to-LIR pass and the ARM64/x64 backends. Focused executable evidence
is in `src/Tests/e2e/comparison-parity.e2e`, alongside the existing
`equality.e2e` and `interpreter_behavior_parity.e2e` suites.

The root wrappers are `src/DarkCompiler/stdlib/NoModule.dark:3-11`, loaded at
`CompilerLibrary.fs:1116-1118`. Separate stdlib specialization merges user
record/sum metadata before materializing structural helpers at
`CompilerLibrary.fs:1296-1378`; the indexed record view is built at
`passes/1.5_TypeChecking.fs:1046-1058`. Public probes are at
`comparison-parity.e2e:35-110,148-156`.
