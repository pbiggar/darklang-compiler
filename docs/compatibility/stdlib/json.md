# AltJson and Json parity

Semantic comparison baseline: `darklang/dark@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
The [source ledger](../../porting/README.md) owns current support and test dispositions.

Focused coverage lives in `test/fixtures/e2e/json-parity.e2e` and the imported Json tests.

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
`packages/darklang/stdlib/altJson.dark`. Helpers use the first matching object
field and retain the interpreter's absent/wrong-shape `Option`, zero, and empty
list results. Builder fields append in call order; every optional adder omits
`None`; `Builder.empty` is a first-class empty value.

## Typed JSON wire contract

`Json.serialize<a>` and `Json.parse<a>` are AOT intrinsics. Specialization
materializes thin codecs for the concrete type; generated code does not inspect
runtime type metadata. Typed parsing lexically validates one complete root and
passes allocation-free packed source ranges through shared scalar and container
helpers, without constructing the private `AltJson.InternalRawJson` tree or
copying nested input. Lists consume a streaming index cursor, records build one
field index in source order, and the syntax scan carries a single-field-object
bit that avoids rescanning the normal enum envelope. Typed
serialization threads a shared writer through codecs, centralizing punctuation
and escaping instead of generating `StringConcat` trees. A caller-owned
compilation session reuses generated codec declarations by serialization
direction and the complete reachable shape of the resolved type; the cache is
bounded to that session and distinguishes same-named local declarations with
different fields or variants. Once converted, compiler-generated JSON codecs,
equality helpers, their generic specializations, and their lifted dependencies
are also compiled to allocated LIR once per session and shared by subsequent
programs. Code-generation metadata is summarized for those reusable groups and
merged with each program's small fresh summary.

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

JSON conversion uses concrete AOT type metadata without runtime reflection.
Empty dictionaries serialize as `{}` even when their key type cannot represent
JSON object keys. Nonempty dictionaries with non-String keys retain the
unsupported-type rejection, as do unsupported Function, Stream, Blob, RawPtr,
RuntimeError and unresolved shapes.

The compiler retains its existing nominal record and sum identities. The
interpreter revision can identify separately declared structurally identical
records by content hash; JSON does not add that structural type-identity rule
to the compiler. Error type names likewise use the compiler's existing nominal
canonical spelling; interpreter-only nested module qualification is not
invented by JSON. RawPtr and RuntimeError are compiler-internal shapes, not
JSON extensions. There are no JSON-specific compiler-only serialized types.

## Implementation anchors

- Public portable code and the lossless parser are in the module-scoped
  `packages/darklang/stdlib/altJson.dark`, `packages/darklang/stdlib/altJson/`, `packages/darklang/stdlib/json.dark`, `packages/darklang/stdlib/json/`,
  `RuntimeTypes*.dark`, and `LanguageTools.dark` sources.
- AOT plan construction and recursive record/enum substitution:
  `src/frontend/JsonPlanning.ml`.
- The shared packed-range reader, container traversal, scalar extraction, and writer
  primitives are in `packages/darklang/stdlib/json.dark`.
- Generic intrinsic checking and unsupported-shape diagnostics:
  `src/frontend/WrittenChecking.ml`.
- Late specialization integration and stdlib loading:
  `src/CompilerLibrary.ml`.
- Recursive ownership shapes and native release helpers:
  `src/ir/anf/ANF.ml`,
  `src/passes/anf/RefCountInsertion.ml`, and the architecture
  backends.

The interpreter sources revalidated for this work are
`packages/darklang/stdlib/alt-json.dark`,
`packages/darklang/stdlib/json.dark`,
`packages/darklang/languageTools/runtimeTypes.dark`, and
`backend/src/Builtins/Builtins.Pure/Libs/{AltJson,Json}.fs` at the revision
above.
