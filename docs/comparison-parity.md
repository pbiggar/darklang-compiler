# Equality, ordering, and comparability parity

This contract was revalidated from the public interpreter behavior at
darklang/dark `04fbe9dcc995c6188757d583e273cbd30a3e2d3d` and the approved compiler
evidence revision `b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899`. Implementation work and
same-source native probes started from compiler HEAD
`2e5690c6d16f4e6f261ffdc86f075e52766753c8`. DCB1 report commit `8a402797`
was used only to locate candidates; every finding below was checked again at
these revisions.

The executable matrix is `src/Tests/e2e/comparison-parity.e2e`. Its probes use
only the interpreter spellings `==`, `!=`, `<`, `>`, `<=`, and `>=`.
Comparison aliases are not part of the language surface.

## Contract

| Values | Equality | Ordering |
| --- | --- | --- |
| Unit, Boolean, character, string | Same type and same value | Rejected |
| Signed and unsigned integers, including 128-bit, and `Int` | Numeric equality within one identical numeric type | Numeric ordering within one identical numeric type |
| Float | IEEE equality | IEEE ordering |
| Tuple and list | Recursive and structural | Rejected |
| Record | Recursive and structural within the same nominal record type | Rejected |
| Constructor | Same nominal sum type, variant, and recursively equal payload | Rejected |
| Dict | Same mapping, independent of insertion order or HAMT shape; values compare recursively | Rejected |
| Function | Interpreter identity rules described below | Rejected |
| Bytes, RawPtr, RuntimeError | Rejected by the compiler comparison type constraint | Rejected |

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

Function closures carry an explicit semantic identity and comparator in
addition to their operational code pointer. An ordinary lambda's identity is
its source expression, so repeated evaluation of that expression compares
equal and its captures are ignored. Two separate lambda expressions compare
unequal. A named function's identity contains the resolved specialized
function name; a partial application additionally compares its already-applied
arguments recursively. This representation is used by direct, nested, and
generic equality and is included in closure reference-count traversal.

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
than an equality value. These are documented compiler surface differences, not
extensions to comparison behavior. Ahead-of-time rejection is the only
intentional public behavior divergence retained here.

Performance differences are outside this contract unless they change an
observable result.

## Source anchors

The interpreter baseline is implemented in
`backend/src/Builtins/Builtins.Pure/Libs/NoModule.fs` (equality at lines 33-175
and numeric comparison at lines 600-707 in the pinned revision), with function
identity data in `backend/src/LibExecution/RuntimeTypes.fs` around lines
895-935. Public error rendering is in
`packages/darklang/prettyPrinter/runtimeError.dark` around lines 232-242.

Compiler enforcement and typed comparison plans live in
`src/DarkCompiler/passes/1.5_TypeChecking.fs` (type errors at 47-121,
classification at 1119-1239, helper construction at 4745-5025, and helper
materialization at 5223-5290). Specialization and structural lowering live in
`src/DarkCompiler/passes/2_AST_to_ANF.fs` (plan materialization at 26-69 and
closure identity at 2276-2410), with post-specialization orchestration in
`src/DarkCompiler/CompilerLibrary.fs` at 1682-1705. Closure layout reaches
native code through `src/DarkCompiler/passes/4_MIR_to_LIR.fs`.
The semantic Dict implementation is in `src/DarkCompiler/stdlib/Dict.dark` at
95-106,
using the layout and key helpers exposed from `src/DarkCompiler/Stdlib.fs`.
Float conditions and architecture-specific integer conditions remain in the
shared MIR-to-LIR pass and the ARM64/x64 backends. Focused executable evidence
is in `src/Tests/e2e/comparison-parity.e2e`, alongside the existing
`equality.e2e` and `interpreter_behavior_parity.e2e` suites.
