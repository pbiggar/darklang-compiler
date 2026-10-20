# Repeatable compatibility ledger

Current state of the compiler port. [Read what these measurements mean](../README.md).

Upstream: `e76430be5cd9f6b77ed255430dc34a1b58dd8fc4`; target: `linux-x64`.

## Current state

| Area | Recorded state |
| --- | --- |
| Language capabilities | 195 implemented, 2 partial, 16 missing, 0 not-assessed |
| StdLib declarations | 388 identical, 1010 different, 598 missing, 0 ambiguous |
| Package declarations (includes StdLib) | 393 identical, 1051 different, 4307 missing, 0 ambiguous |
| Upstream tests | 552 enabled, 82 disabled, 122 partial, 1593 missing, 10 ambiguous |
| Package compilation | not-run |

Counts are different measurements, not one overall compatibility score. Source comparisons use each declaration itself. Enabled tests are assumed to pass; this report does not run tests.

## Language features

Maintained assessments of specific language capabilities. See [how this ledger works](../README.md).


### Async

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Async failures | missing | Propagation of failures through futures is not implemented in this baseline. |
| Async function calls | missing | Native future-producing function calls are not implemented in this baseline. |
| Explicit await forces a pending argument | missing | Upstream await now calls Builtin.runtimeForce; ordinary user function calls may pass pending arguments onward. The separate native async work has not merged into this baseline. |
| Future value representation | missing | The merged baseline does not represent futures in the native language pipeline. |
| Implicit await on value use | missing | Future values are not resolved implicitly by this baseline. Upstream e76430be5 also carries pending reads inside containers and closure captures without waiting at construction. |

### Bindings and scope

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Captured local values | implemented | A closure retains the local values it references. |
| Let/in expressions | implemented | A binding can have an explicit continuation expression. |
| Lexical shadowing | implemented | Inner bindings can shadow outer names. |
| Local let bindings | implemented | Bind a value for later expressions. |
| Module value declarations | implemented | Module-scoped val declarations are checked. |
| Statement sequences | implemented | Earlier statements must have Unit type; the final expression supplies the result. |
| Tuple destructuring bindings | implemented | Bind several tuple elements, including nested patterns. |
| Unit binding patterns | implemented | Match a Unit value in a binding. |
| Wildcard bindings | implemented | Discard a binding with _. |

### Builtin types

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Blob | implemented | Source type Blob is resolved and represented by the native compiler. |
| Bool | implemented | Source type Bool is resolved and represented by the native compiler. |
| Char | implemented | A Unicode extended grapheme cluster, represented as UTF-8. |
| DateTime | implemented | Source type DateTime is resolved and represented by the native compiler. |
| Dict<'key, 'value> | implemented | Typed keys and values; supported key rules are checked by the compiler. |
| Float | implemented | Source type Float is resolved and represented by the native compiler. |
| Function types | implemented | Parameter and return types for named functions and function values. |
| Int | implemented | Arbitrary-precision integers; the unsuffixed integer type. |
| Int128 | implemented | Source type Int128 is resolved and represented by the native compiler. |
| Int16 | implemented | Source type Int16 is resolved and represented by the native compiler. |
| Int32 | implemented | Source type Int32 is resolved and represented by the native compiler. |
| Int64 | implemented | Source type Int64 is resolved and represented by the native compiler. |
| Int8 | implemented | Source type Int8 is resolved and represented by the native compiler. |
| List<'a> | implemented | Homogeneous lists, including generic element types. |
| Stream<'a> | implemented | Resolved to the native stream type; lifecycle operations live in StdLib.Stream. |
| String | implemented | Source type String is resolved and represented by the native compiler. |
| Tuple types | implemented | Two or more independently typed elements. |
| UInt128 | implemented | Source type UInt128 is resolved and represented by the native compiler. |
| UInt16 | implemented | Source type UInt16 is resolved and represented by the native compiler. |
| UInt32 | implemented | Source type UInt32 is resolved and represented by the native compiler. |
| UInt64 | implemented | Source type UInt64 is resolved and represented by the native compiler. |
| UInt8 | implemented | Source type UInt8 is resolved and represented by the native compiler. |
| Unit | implemented | Source type Unit is resolved and represented by the native compiler. |
| Uuid | implemented | Source type Uuid is resolved and represented by the native compiler. |

### Collections

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Comma and newline collection separators | implemented | Records, record updates, record type fields and dictionary entries use commas or newlines; semicolons are diagnosed, matching upstream cf4ef2c39. Statement sequences still accept semicolons. |
| Dictionary literals | implemented | Typed key/value entries. |
| Empty dictionary inference | implemented | Context can supply the key and value types. |
| Empty list literals | implemented | The expected type can determine the element type of []. |
| Nested collection inference | implemented | Expected types propagate through nested list/dictionary/tuple expressions. |
| Nonempty list literals | implemented | All list elements must share a type. |
| Tuple construction | implemented | Construct tuples with independently typed elements. |
| Tuple element access | implemented | Read tuple elements by position. |

### Control flow

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Boolean condition checking | implemented | Conditions and short-circuit operands must be Bool. |
| Elif chains | implemented | Conditional chains lower to nested conditionals. |
| If without else | implemented | A missing else branch has Unit type. |
| If/then/else | implemented | Both value-producing branches are checked for compatible types. |
| Short-circuit boolean and | implemented | The second operand is conditional on the first. |
| Short-circuit boolean or | implemented | The second operand is conditional on the first. |

### Effects

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Effect names | implemented | Recognize effect names in source annotations. |
| Effect propagation through calls | missing | Called-function effects are not propagated through the direct checker. |
| Effect row annotations | implemented | Parse function effect annotations. |
| Empty effect rows | implemented | The syntax for an explicitly empty effect set is parsed. |
| Function permission ceilings | missing | Declared effect ceilings are not enforced by the direct checker. |
| Higher-order effect propagation | missing | Effects through function values lack full propagation/enforcement. |

### Function calls

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Builtin function calls | implemented | Recognized Builtin operations use their checked intrinsic/native implementations. |
| Call argument and return types | implemented | Calls enforce declared argument and return types. |
| Capturing lambdas | implemented | Anonymous functions with an environment of captured values. |
| Direct positional calls | implemented | Call a declared function with positional arguments. |
| Generic partial application using inference alone | partial | Some generic partial applications require explicit type arguments. |
| Generic partial application with explicit types | implemented | Explicit type arguments can determine a generic partially applied function. |
| Higher-order arguments | implemented | Pass a function as an argument to another function. |
| Immediate curried lambda application | implemented | Extra arguments to an immediate lambda are applied to its returned function. |
| Immediate lambda application | implemented | An anonymous function expression can be called directly. |
| Lambda tuple parameters | implemented | A lambda parameter can destructure a tuple. |
| Local function-value calls | implemented | Invoke a function stored in a local binding. |
| Local recursion | implemented | Local recursive functions are handled by local binding checking. |
| Multiple lambda parameters | implemented | Anonymous functions can bind multiple parameters. |
| Mutually recursive functions | implemented | Recursive declaration groups are resolved together. |
| Named function overapplication | missing | Named calls require the declared arity; call a returned function in a separate application step. |
| Named function references | implemented | Use a declared function as a value. |
| Non-capturing lambdas | implemented | Anonymous functions without captured values. |
| Package function calls | implemented | Imported package declarations participate in named function resolution. |
| Partial application of function values | implemented | Partial application also works for an indirect function target. |
| Partial application of named functions | implemented | Supplying a prefix of arguments creates a function for the rest. |
| Polymorphic recursion | missing | Recursive calls that change a generic function's type arguments are rejected. |
| Qualified function calls | implemented | Resolve module-qualified function names. |
| Recursive functions | implemented | A declared function can call itself. |
| Returned functions | implemented | Return and subsequently call a function value. |
| Too many call arguments | implemented | Call arity is checked before lowering. |
| Unit function parameters | implemented | Functions can explicitly accept (). |

### Generics and declared types

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Enum constructor payloads | implemented | Enum cases can carry one or more typed values. |
| Enum type declarations | implemented | Named sum types with multiple constructors. |
| Explicit function type arguments | implemented | A caller can supply function type arguments. |
| Generic Option type | implemented | StdLib Option has Some and None constructors. |
| Generic Result type | implemented | StdLib Result has Ok and Error constructors. |
| Generic enum types | implemented | Payloads can contain type parameters. |
| Generic function declarations | implemented | Functions can declare type parameters. |
| Generic function values | implemented | Generic function references can be specialized as values. |
| Generic record types | implemented | Record fields may contain type parameters. |
| Generic type aliases | implemented | Aliases may have type parameters. |
| Inferred function type arguments | implemented | The checker infers type arguments from call context. |
| Native generic specialization | implemented | Concrete uses are specialized for native compilation. |
| Nested generic type arguments | implemented | Generic types can be nested inside other generic types. |
| Nullary enum constructors | implemented | Enum cases with no payload fields. |
| Qualified constructors | implemented | Constructor names can include the owning type/module. |
| Record construction | implemented | Construct records with checked field names and types. |
| Record field access | implemented | Read a field from a record value. |
| Record type declarations | implemented | Named record types with typed fields. |
| Record updates | implemented | Create an updated record while retaining the remaining fields. |
| Type aliases | implemented | Resolve an alias to its underlying type. |

### Literals

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Boolean literals | implemented | true and false. |
| Character literals | implemented | Single grapheme literals and escapes. |
| Float literals | implemented | Floating-point literal parsing and checking. |
| Literal type/range rejection | implemented | Invalid literal representations are rejected before native lowering. |
| Signed fixed-width integer literals | implemented | Suffixes select Int8, Int16, Int32, Int64 or Int128. |
| Unit literals | implemented | The value (). |
| Unsigned integer literals | implemented | Suffixes select UInt8, UInt16, UInt32, UInt64 or UInt128. |
| Unsuffixed integer literals | implemented | Arbitrary-precision Int literals. |

### Modules and names

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Escaped identifiers | implemented | Backtick-delimited identifiers can use otherwise reserved names. |
| File module declarations | implemented | Declare the root module for a source file. |
| Forward declaration references | implemented | Resolve references to later declarations. |
| Lexical name resolution | implemented | Resolve local and enclosing-module bindings. |
| Nested modules | implemented | Nested module declarations establish lexical scopes. |
| Package source loading | implemented | Load checked package declarations into the compiler environment. |
| Qualified name resolution | implemented | Resolve names using their module paths. |

### Pattern matching

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Arbitrary-precision integer patterns | implemented | Match Int literal values. |
| Binding consistency across alternatives | implemented | Alternative patterns must bind compatible names and types. |
| Boolean match patterns | implemented | Match true and false. |
| Character match patterns | implemented | Match a character/grapheme literal. |
| Constructor arity checking | implemented | Patterns must supply the correct number of constructor fields. |
| Empty-list patterns | implemented | Match []. |
| Enum constructor patterns | implemented | Match a constructor with its payload fields. |
| Exhaustiveness checking | implemented | Missing match cases are diagnosed during compilation. |
| Fixed-length list patterns | implemented | Match a list's shape and element patterns. |
| Float match patterns | implemented | Match floating-point literals. |
| Guard type checking | implemented | A guard must produce Bool. |
| Head/tail list patterns | implemented | Match head :: tail. |
| Match arm result types | implemented | Arm bodies must have compatible result types. |
| Match guards | implemented | A when expression restricts a pattern match. |
| Nested constructor patterns | implemented | Constructor fields can contain further patterns. |
| Or-pattern alternatives | implemented | Alternative patterns share one match arm. |
| Pattern/scrutinee type checking | implemented | Patterns must be compatible with the matched value's type. |
| Signed fixed-width integer patterns | implemented | Match Int8/16/32/64/128 literals. |
| String match patterns | implemented | Match a complete string value. |
| Tuple match patterns | implemented | Destructure tuples, including nested payload patterns. |
| Unit match patterns | implemented | Match (). |
| Unsigned integer patterns | implemented | Match UInt8/16/32/64/128 literals. |
| Variable match patterns | implemented | A pattern can bind the matched value. |
| Wildcard match patterns | implemented | _ matches without binding a name. |

### Pipelines

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Chained pipelines | implemented | Several pipeline segments compose left to right. |
| Generic pipeline inference | implemented | Expected types and later segments contribute to type inference. |
| Pipe into a function value | implemented | A local function binding can be a pipeline target. |
| Pipe into a lambda | implemented | An anonymous function can consume the piped value. |
| Pipe into a named function | implemented | The piped value becomes the first function argument. |
| Pipe into a qualified function | implemented | Pipeline segments can use module-qualified names. |
| Pipe into an enum constructor | implemented | The piped value supplies the first constructor field. |
| Pipe into an operator | implemented | Operator pipeline segments are lowered to infix expressions. |

### Reflection and dynamic execution

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Runtime expression evaluation | missing | No interpreter-style evaluation of arbitrary source/AST at runtime. |
| Runtime function lookup by name | missing | No general runtime lookup and invocation of program functions by string name. |
| Runtime program reflection | missing | The compiler's compile-time metadata is not an interpreter runtime program model. |

### Standard traits

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Darklang.Stdlib.Add | implemented | Static trait and add method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.BitwiseAnd | implemented | Static trait and bitwiseAnd method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.BitwiseNot | implemented | Static trait and bitwiseNot method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.BitwiseOr | implemented | Static trait and bitwiseOr method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.BitwiseXor | implemented | Static trait and bitwiseXor method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.Compare | implemented | Static trait and lessThan, lessThanOrEqualTo, greaterThan, greaterThanOrEqualTo methods; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.Divide | implemented | Static trait and divide method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.Modulo | implemented | Static trait and modulo method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.Multiply | implemented | Static trait and multiply method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.Negate | implemented | Static trait and negate method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.One | implemented | Static trait and one method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.Power | implemented | Static trait and power method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.ShiftLeft | implemented | Static trait and shiftLeft method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.ShiftRight | implemented | Static trait and shiftRight method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.Subtract | implemented | Static trait and subtract method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.ToString | implemented | Static trait and toString method; implementations are tracked individually in the declaration ledger. |
| Darklang.Stdlib.Zero | implemented | Static trait and zero method; implementations are tracked individually in the declaration ledger. |

### Strings and interpolation

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Braces inside interpolation expressions | implemented | Nested expression braces are scanned before finding the interpolation terminator. |
| Doubled literal braces | implemented | {{ and }} produce literal braces. |
| Escaped literal braces | implemented | Backslash brace escapes are handled in ordinary quoted strings. |
| Escaped quotes and backslashes | implemented | Escapes are recognized while scanning quoted strings. |
| Expressions inside interpolation | implemented | Embedded expressions use the expression parser, including calls and record expressions. |
| Interpolation requires String | implemented | Non-String embedded expressions are rejected by the type checker. |
| Interpolation source locations | implemented | Nested parsing diagnostics retain locations in the enclosing source. |
| Multiple interpolated expressions | implemented | Literal segments can alternate with several expressions. |
| Nested interpolation | implemented | An interpolation expression can itself contain an interpolated string; nesting is bounded. |
| Ordinary quoted strings | implemented | Quoted text and escape processing. |
| Quotes inside interpolation expressions | implemented | Quoted strings and characters do not terminate an embedded expression early. |
| Single-line interpolation | implemented | Expressions inside $"text {expression}". |
| String concatenation | implemented | String concatenation expressions are checked and lowered. |
| Triple-quoted interpolation | implemented | Expressions inside multiline interpolated strings. |
| Triple-quoted strings | implemented | Multiline string syntax. |
| Unicode string text | implemented | Unicode text is preserved through parsing and native representation. |

### Test declarations

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| General named-package test adoption | partial | Not every upstream named test has an enabled compiler counterpart. |
| Imported named-test fixtures | implemented | The existing E2E adapter selects named tests imported under the upstream fixture directory. |
| Named test declaration parsing | implemented | Parse named test declarations in package sources. |
| Named test extraction | implemented | Separate test bodies from ordinary package declarations. |

### Trait mechanisms

| Feature | Status | Scope / limitation |
| --- | --- | --- |
| Bounded generic specialization | implemented | Trait obligations are resolved during concrete generic specialization. |
| Bounds on declared types | implemented | Bounds on generic types are checked when those types are used. |
| Competing implementation rejection | implemented | Statically ambiguous implementations are rejected. |
| Conditional implementations | implemented | Implementation selection checks required bounds. |
| Generic implementations | implemented | Implementations may introduce type parameters. |
| Implementation declarations | implemented | An impl supplies methods for a concrete self type. |
| Implementation method aliases | implemented | An implementation method may reference an existing function. |
| Implementation method signature checking | implemented | Implementation methods must have the required types. |
| Missing implementation rejection | implemented | Reject a call when its required trait implementation is absent. |
| Multiple trait bounds | implemented | A type parameter can require several traits. |
| Multiple trait methods | implemented | A trait may declare more than one method. |
| Newest-implementation-wins dispatch | missing | The compiler rejects ambiguous implementations instead of choosing the latest at runtime. |
| Parameterized trait bounds | implemented | Bounds can refer to parameterized traits. |
| Receiver method syntax | implemented | Resolve method-like calls from a receiver's type. |
| Receiver/record-field precedence | implemented | A record function field and a trait receiver method are distinguished. |
| Runtime trait lookup | missing | Implementation selection must finish during compilation. |
| Single trait bounds | implemented | A type parameter can require one trait. |
| Static trait method dispatch | implemented | Resolve a trait method at compilation. |
| Trait declarations | implemented | Declare a trait separately from nominal record/enum types. |
| Trait effect ceilings | missing | Trait method permission ceilings are not enforced by the static trait model. |
| Trait method function values | implemented | A resolved trait method can be used as a function value. |
| Trait operators | implemented | Overloaded operators select a trait implementation. |

## Source equivalency by declaration

Counts are functions, values, types, traits and implementations. Named tests are in the test inventory. Packages include StdLib; file splits do not determine declaration matches. Identical means exact declaration source bytes, excluding surrounding comments/blank lines.

| Ledger | Identical | Different | Missing | Ambiguous | Compiler-only |
| --- | ---: | ---: | ---: | ---: | ---: |
| stdlib | 388 | 1010 | 598 | 0 | 3443 |
| packages | 393 | 1051 | 4307 | 0 | 3446 |

## Existing test references

These counts show references from existing tests, including calls through helpers. Each declaration in `porting/generated/ledger.json` lists its reference count and an example test relationship.

| Direct | Indirect | Maintained | Not identified |
| ---: | ---: | ---: | ---: |
| 1533 | 3133 | 170 | 915 |

Package compilation state: **not-run**. Results are recorded for each original source file. A file that compiles provides evidence for its declarations. A file that fails does not identify which declarations fail independently.

| Declarations in compiled files | In files that failed | In blocked files | Stale | Not run |
| ---: | ---: | ---: | ---: | ---: |
| 0 | 0 | 0 | 0 | 5751 |

## Parser and builtins

Builtin mappings distinguish callable Builtin declarations from source implementation counterparts. A counterpart is not a callable alias or a behavior pass. Computed registrations remain explicit review items.

- parser: compiler-only: 19, maintained: 9
- builtins: declared-builtin: 75, maintained: 4, missing: 175, source-counterpart: 527
- Computed builtin constructor templates: 7 (reviewed literal call sites expanded; not an assembled registry)

## Interpreter tests versus compiler tests

Source inventory only; enabled tests are assumed to pass. Compiler counterparts include existing upstream fixtures and named declarations. Recognizable named assertions and Test.all groups match legacy assertions by content; partial groups stay explicit. Helper-driven migrations require maintained mappings. Helper/preamble equivalence is not inferred from assertion content.

| Upstream format | Total | Identical | Adapted | Different | Partial | Missing | Enabled | Disabled | Ambiguous |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| legacy | 239 | 173 | 0 | 0 | 0 | 57 | 173 | 0 | 9 |
| named | 2120 | 93 | 367 | 1 | 122 | 1536 | 379 | 82 | 1 |

Unmatched compiler tests: 9581 (8780 enabled). Older imported fixtures absent from the current pin remain visible here; unmatched does not prove compiler authorship.

Each counterpart records whole-file exclusions, individual exclusions, skip annotations and changed compiler expectations. Changed expectations are never counted as identical. Source statements needing manual review: 1.

## Review queue

- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L13
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L130
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L137
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L17
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L6
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L67
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-syscall.dark#L7
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-syscall.dark#L8
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-system-syscall.dark#L4
- Ambiguous test counterpart: packages/darklang/tests/language/basic.dark#Darklang.Language.Tests.Basic.and
- Ambiguous test counterpart: packages/darklang/tests/language/basic.dark#Darklang.Language.Tests.Basic.or
- Ambiguous test counterpart: packages/darklang/tests/language/values.dark#Darklang.Language.Tests.Values.Package.values
- Ambiguous test counterpart: packages/darklang/tests/stdlib/bool.dark#Darklang.Stdlib.Bool.Tests.andTruthTable
- Ambiguous test counterpart: packages/darklang/tests/stdlib/bool.dark#Darklang.Stdlib.Bool.Tests.orTruthTable
- Ambiguous test counterpart: packages/darklang/tests/stdlib/char.dark#Darklang.Stdlib.Char.Tests.isWhitespace
- Ambiguous test counterpart: packages/darklang/tests/stdlib/dateTime.dark#Darklang.Stdlib.DateTime.Tests.fieldsIn1970
- Ambiguous test counterpart: packages/darklang/tests/stdlib/float.dark#Darklang.Stdlib.Float.Tests.toIntTruncatesWhereFloorRoundsDown
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.Option.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.Package.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.Result.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt128.Basic.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt128.UInt128Limits.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt16.Basic.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt16.UInt16Limits.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt32.Basic.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt32.UInt32Limits.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt64.Basic.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt64.UInt64Limits.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/list.dark#Darklang.Stdlib.List.Tests.map
- Ambiguous test counterpart: packages/darklang/tests/stdlib/string.dark#Darklang.Stdlib.String.Tests.dropFirst
- Ambiguous test counterpart: packages/darklang/tests/stdlib/string.dark#Darklang.Stdlib.String.Tests.length
- Ambiguous test counterpart: packages/darklang/tests/stdlib/string.dark#Darklang.Stdlib.String.Tests.lengthFastPath
- Ambiguous test counterpart: packages/darklang/tests/stdlib/string.dark#Darklang.Stdlib.String.Tests.toBytesLength
- Repeated declaration identity: packages/darklang/test/test_eself.dark#Darklang.ShadowingTest.valueShadowing
- Repeated declaration identity: packages/darklang/tests/language/enums.dark#Darklang.Language.Tests.Enums.Polymorphism.insert
- Repeated declaration identity: packages/darklang/tests/language/enums.dark#Darklang.Language.Tests.Enums.Polymorphism.toList
- ambiguous declaration: Darklang.Language.Tests.Enums.Polymorphism.insert
- ambiguous declaration: Darklang.Language.Tests.Enums.Polymorphism.toList
- ambiguous declaration: Darklang.ShadowingTest.valueShadowing
- ambiguous declaration: test1
- ambiguous declaration: test10
- ambiguous declaration: test11
- ambiguous declaration: test12
- ambiguous declaration: test13
- ambiguous declaration: test14
- ambiguous declaration: test2
- ambiguous declaration: test3
- ambiguous declaration: test4
- ambiguous declaration: test5
- ambiguous declaration: test6
- ambiguous declaration: test7
- ambiguous declaration: test8
- ambiguous declaration: test9
- ambiguous declaration: testList
- test/fixtures/e2e/upstream/language/basic/estring.dark:21: unindexed legacy test statement
