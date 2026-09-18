# Dict parity

The public dictionary type accepts both the historical `Dict<value>` spelling,
which implies `String` keys, and the interpreter-compatible
`Dict<key, value>` spelling. Literals accept the historical
`Dict { name = value }` form for String keys and
`Dict { expression: value }` for any admissible key type.

## Key contract

Primitive values, tuples, lists, records, sums, and Dicts can be keys when
their complete recursive shape is admissible. Functions, streams, Blobs,
database/opaque references, compiler runtime values, and containers that
contain them are rejected during type checking. Keys use typed structural
equality. Canonical key ordering drives `toList`, `keys`, rendering, and the
public traversal functions, so their result does not depend on HAMT shape.

The native HAMT retains scalar hash implementations for `Int64`, `Bool`,
`String`, and `Blob` internally. Other admitted key types use the collision
path with structural equality; this preserves results while leaving composite
hashing as a performance opportunity. Both native backends carry the concrete
key release plan through Dict ownership helpers.

## Public surface

`empty`, `singleton`, `isEmpty`, `size`, `get`, `set`,
`setOverridingDuplicates`, `remove`, `member`, `toList`, `keys`, `values`,
`fromList`, `fromListOverwritingDuplicates`, `merge`, `map`, `iter`, `filter`,
and `filterMap` preserve their key type. Dict equality and ordering compare the
ordered `(key, value)` view recursively. JSON object conversion remains
String-keyed by definition and rejects other Dict key types at compile time.

## Evidence

The unchanged pinned `src/Tests/e2e/upstream/stdlib/dict.dark` file is enabled
except for individually catalogued diagnostic, unavailable-builtin, invalid-key,
and numeric-edge cases. Its enabled cases cover primitive widths, tuple, list,
option, record, enum, nested Dict, and recursively nested keys. The formerly
disabled `language/collections/edict.dark` smoke file is also enabled in full.
Syntax roundtrips cover two-argument Dict types and expression-key literals.
