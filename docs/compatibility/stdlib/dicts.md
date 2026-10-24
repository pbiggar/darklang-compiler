# Dict parity

Dictionary key and value semantics reviewed on 2026-10-22 against interpreter
`e76430be5cd9f6b77ed255430dc34a1b58dd8fc4`.
The [source ledger](../../porting/README.md) owns current support and test dispositions.

The public dictionary type accepts both the historical `Dict<value>` spelling,
which implies `String` keys, and the interpreter-compatible
`Dict<key, value>` spelling. Literals accept the historical
`Dict { name = value }` form for String keys and
`Dict { expression: value }` for any admissible key type.

## Key contract

Primitive values, tuples, lists, records, sums, and Dicts can be keys when
their occupied payloads are admissible. Public operations reject streams,
Blobs and containers that contain them. Empty lists and payloadless enum cases
remain valid even when their type arguments mention a forbidden payload.
Actual function keys remain an AOT error: function-bearing containers must
be provably free of functions, including through reusable preamble helpers.
Explicit `RuntimeTypes.Dval` trees
are ordinary sums and records and can be structural keys; see
`dval_dict_lookup.e2e`. Key comparison recursively treats NaNs as the same key
and equates positive and negative zero. Dictionary values retain ordinary
equality, so dictionaries with NaN values compare unequal.
Literals reject duplicate keys under that same canonical key equality. Known
duplicate String literals fail during checking; computed and structural
duplicates raise the ordinary duplicate-key error after all entry expressions
have been evaluated once in source order. Explicit overwriting APIs retain
their last-value-wins behavior.
Canonical key ordering drives `toList`, `keys`, rendering, and the
public traversal functions, so their result does not depend on HAMT shape.

The native HAMT retains scalar hash implementations for `Int64`, `Bool`,
`String`, and `Blob` internally. Other admitted key types use the collision
path with canonical recursive key comparison; this preserves results while leaving composite
hashing as a performance opportunity. Both native backends carry the concrete
key release plan through Dict ownership helpers.

## Public surface

`empty`, `singleton`, `isEmpty`, `size`, `get`, `set`,
`setOverridingDuplicates`, `remove`, `member`, `toList`, `keys`, `values`,
`fromList`, `fromListOverwritingDuplicates`, `merge`, `map`, `iter`, `filter`,
and `filterMap` preserve their key type. Dict equality checks key membership
with key comparison and compares the corresponding values with ordinary equality.
Canonical key ordering supplies deterministic traversal. Public numeric ordering
operators reject Dict operands. JSON object conversion remains
String-keyed by definition and rejects other Dict key types at compile time.

## Evidence

The imported `test/fixtures/e2e/upstream/stdlib/dict.dark` file is enabled
except for individually catalogued diagnostic, unavailable-builtin, invalid-key,
and numeric-edge cases. Its enabled cases cover primitive widths, tuple, list,
option, record, enum, nested Dict, and recursively nested keys. The formerly
disabled `language/collections/edict.dark` smoke file is also enabled in full.
The NaN-ordering assertion has a local oracle adaptation recorded in the
[import annotations](../../porting/upstream-import.md). Syntax
roundtrips cover two-argument Dict types and expression-key literals.
