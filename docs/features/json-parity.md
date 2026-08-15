# AltJson and Json parity

## Revision baseline

This contract was revalidated from compiler implementation commit
`32a3839a4af3993a67b01eaae165999173eb2ae5`, based on compiler `main` at
`3aca5da61036239b036151233032515ffbdad74e`, and darklang/dark interpreter
revision `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. DCB1 report commit
`8a402797` and compiler evidence revision
`51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3` were navigation aids only; every
retained behavior below was checked again against the pinned interpreter
sources and execution fixtures. Performance is outside this contract unless it
changes an observable value or failure.

The same-source interpreter fixtures are
`src/Tests/e2e/upstream/stdlib/alt-json.dark` and
`src/Tests/e2e/upstream/stdlib/json.dark`. The compact native regression matrix
is `src/Tests/e2e/json-parity.e2e`. All three are in normal E2E discovery.

## Structural JSON

`Stdlib.AltJson.Json` is the public ordered tree:

| Case | Payload |
| --- | --- |
| `Null` | none |
| `Bool` | `Bool` |
| `Number` | `Float` |
| `String` | decoded `String` |
| `Array` | `List<Json>` |
| `Object` | ordered `List<(String, Json)>` |

Object order, duplicate names, and empty names are preserved. Internally, each
node also retains its original source slice, numbers retain their exact lexeme,
and strings retain their decoded value. That extra data is private: it exists
so typed integer conversion never rounds through `Float` and
`CantMatchWithType` can quote the original fragment.

The accepted grammar matches the pinned interpreter: JSON whitespace and
skipped line/block comments, strict literals, strict decimal/exponent syntax,
decoded JSON escapes and UTF-16 surrogate pairs, with no trailing commas or
trailing non-trivia input. Lexical, escape, numeric-conversion, and trailing
input failures are `AltJson.ParseError.NotJson`. `format` emits compact JSON,
preserves array/object order and duplicates, and escapes strings canonically.

The portable helper and builder surface is copied into
`src/DarkCompiler/stdlib/AltJson.dark`. Helpers use the first matching object
field and retain the interpreter's absent/wrong-shape `Option`, zero, and empty
list results. Builder fields append in call order; every optional adder omits
`None`; `Builder.empty` is a first-class empty value.

## Typed JSON wire contract

`Json.serialize<a>` and `Json.parse<a>` are AOT intrinsics. Specialization
materializes a recursive conversion plan for the concrete type; generated code
does not inspect runtime type metadata.

| Dark type | JSON wire shape |
| --- | --- |
| `Unit` | `null` |
| `Bool` | Boolean |
| signed/unsigned integers and `Int` | exact JSON integer number |
| finite `Float` | JSON number |
| NaN, positive/negative infinity | strings `"NaN"`, `"Infinity"`, `"-Infinity"` |
| `Char`, `String`, `Uuid`, `DateTime` | JSON string |
| tuple | fixed-arity array |
| `List<a>` | array |
| `Dict<a>` | object with String keys in ordinal order |
| record | object with fields in ordinal name order |
| enum | one-field object; case name maps to an argument array |
| alias | its resolved wire shape |

Integer parsing accepts decimal and exponent spellings only when their value is
integral and in range. Fixed-width integers and `Int` use the retained exact
lexeme. For `Int128` and `UInt128`, exponent-form input preserves the pinned
interpreter's binary64 fallback (and therefore its observable boundary
rounding); plain integer lexemes remain exact. Character parsing requires one
extended grapheme cluster. UUID and DateTime strings use their public parsers.
Tuple arity is exact. Lists, Dict values, record fields, and enum arguments add
their index/field to the error path. Records require every declared field,
reject duplicate declared fields, and ignore extra fields. Enum decoding
requires exactly one known case and the exact argument count, after converting
the common prefix so an earlier nested mismatch wins over a later arity error.

## Paths and failures

Paths are stored leaf-first as `Root`, `Index`, and `Field` parts and rendered
root-to-leaf (`root.items[2].name`). Public structured failures are:

- `CantMatchWithType`, carrying concrete type metadata, the raw JSON fragment,
  and path;
- `EnumExtraField`, `EnumMissingField`, `EnumInvalidCasename`, and
  `EnumTooManyCases`;
- `RecordDuplicateField` and `RecordMissingField`;
- `NotJson`.

`Part.toString`, `JsonPath.toString`, and `ParseError.toString` preserve the
pinned interpreter wording. Concrete primitive, tuple, list, Dict, function,
record, enum, and instantiated-generic context is represented by the public
`Darklang.LanguageTools.RuntimeTypes.TypeReference` tree. Custom names are
resolved from AOT metadata; the public branch-id argument remains accepted but
is intentionally unused by native code.

## Intentional AOT differences

Function, Stream, Bytes/Blob, RawPtr, RuntimeError, unresolved variables,
non-String Dict keys, and other unsupported runtime shapes are rejected during type
checking. The interpreter can reach corresponding failures at runtime; earlier
AOT diagnostic timing is intentional and prevents runtime type dispatch.

The compiler retains its existing nominal record and sum identities. The
interpreter revision can identify separately declared structurally identical
records by content hash; JSON does not add that structural type-identity rule
to the compiler. Error type names likewise use the compiler's existing nominal
canonical spelling; interpreter-only nested module qualification is not
invented by JSON. RawPtr and RuntimeError are compiler-internal shapes, not
JSON extensions. There are no JSON-specific compiler-only serialized types.

## Implementation anchors

- Public portable code and the lossless parser are in the module-scoped
  `src/DarkCompiler/stdlib/AltJson*.dark`, `Json*.dark`,
  `RuntimeTypes*.dark`, and `LanguageTools.dark` sources.
- AOT plan construction and recursive record/enum substitution:
  `src/DarkCompiler/passes/1.7_JsonPlanning.fs`.
- Generic intrinsic checking and unsupported-shape diagnostics:
  `src/DarkCompiler/passes/1.5_TypeChecking.fs`.
- Late specialization integration and stdlib loading:
  `src/DarkCompiler/CompilerLibrary.fs`.
- Recursive ownership shapes and native release helpers:
  `src/DarkCompiler/ANF.fs`,
  `src/DarkCompiler/passes/2.5_RefCountInsertion.fs`, and the architecture
  backends.

The interpreter sources revalidated for this work are
`packages/darklang/stdlib/alt-json.dark`,
`packages/darklang/stdlib/json.dark`,
`packages/darklang/languageTools/runtimeTypes.dark`, and
`backend/src/Builtins/Builtins.Pure/Libs/{AltJson,Json}.fs` at the revision
above.
