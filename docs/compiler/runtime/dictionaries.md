# Dict: public contract and private HAMT

This document distinguishes Dark's public dictionary contract from the native
HAMT that implements it.

## Parity baseline

The generic key contract follows darklang/dark `main` at
`e76430be5cd9f6b77ed255430dc34a1b58dd8fc4` (2026-10-19). Compiler regression
coverage is in `src/Tests/e2e/generic-dictionary-keys.e2e` and the existing
`dict_parity.e2e` String-key cases. The earlier String-only contract was pinned
to interpreter `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.

The public wrappers live in `src/DarkCompiler/stdlib/Dict.dark`, storage in
`__HAMT.dark`, and structural key hashes in
`frontend/checking/DictionaryKeyHelpers.fs`. Canonical comparison, equality,
and rendering are generated from the concrete key and value types.

## Public type and literals

Source programs expose `Dict<key, value>`:

```dark
Dict<Int64, String>
```

The older `Dict<value>` shorthand still means `Dict<String, value>`. Both
components are retained throughout type checking, specialization, and ownership
planning.

Dictionary literals are distinct from records:

```dark
Dict { }
Dict { name = "dark"; ``Content-Length`` = "0"; ___ = "empty key" }
Dict { 1L: "one", (1L + 1L): "two" }
```

Backticks quote non-identifier keys and `___` spells the empty String key.
Literal keys and values are evaluated in source order. Duplicate static String
keys are rejected during checking; expression keys use strict runtime insertion.
Mixed key or value types are rejected during checking.

## Public `Stdlib.Dict` surface

`Stdlib.Dict.empty` is a polymorphic module value, not a function. The complete
public callable surface is:

```dark
let isEmpty (dict: Dict<k, a>) : Bool
let singleton (key: k) (value: a) : Dict<k, a>
let size (dict: Dict<k, a>) : Int
let get (dict: Dict<k, a>) (key: k) : Option<a>
let set (dict: Dict<k, a>) (key: k) (value: a) : Dict<k, a>
let setOverridingDuplicates (dict: Dict<k, a>) (key: k) (value: a) : Dict<k, a>
let remove (dict: Dict<k, a>) (key: k) : Dict<k, a>
let merge (left: Dict<k, a>) (right: Dict<k, a>) : Dict<k, a>
let keys (dict: Dict<k, a>) : List<k>
let values (dict: Dict<k, a>) : List<a>
let toList (dict: Dict<k, a>) : List<(k * a)>
let fromListOverwritingDuplicates (entries: List<(k * a)>) : Dict<k, a>
let fromList (entries: List<(k * a)>) : Option<Dict<k, a>>
let member (dict: Dict<k, a>) (key: k) : Bool
let map (dict: Dict<k, a>) (fn: k -> a -> b) : Dict<k, b>
let iter (dict: Dict<k, a>) (fn: k -> a -> Unit) : Unit
let filter (dict: Dict<k, a>) (fn: k -> a -> Bool) : Dict<k, a>
let filterMap (dict: Dict<k, a>) (fn: k -> a -> Option<b>) : Dict<k, b>
```

The former compiler-only public names `entries`, `fold`, and `getOrDefault`
are not registered. Their implementation equivalents remain private where the
compiler needs them.

## Observable behavior

- `set` fails if the key already exists. The native error includes the dynamic
  key, for example: "Cannot add two dictionary entries with the same key
  `key`".
- `setOverridingDuplicates` replaces an existing value.
- `fromList` returns `None` on any duplicate.
- `fromListOverwritingDuplicates` returns a Dict and the last occurrence wins.
- `merge` is right-biased; removing an absent key is a no-op.
- `get` returns `Some(value)` or `None`.
- traversal, rendering, equality, and higher-order callbacks all use the same
  canonical key ordering (ordinal UTF-16 ordering for Strings). HAMT bitmap shape, collision layout, insertion
  history, and structural sharing are not observable.
- equality compares entry counts, key sets under canonical key equality, and values using the
  concrete value type's existing equality semantics.
- String-key rendering retains `Dict { key = renderedValue; ... }`; other keys
  render as `Dict { renderedKey: renderedValue; ... }`.
- NaNs identify one key and signed zeros identify one key. Values still use
  ordinary language equality.
- functions, streams, and blobs, including nested occurrences, are rejected as
  public keys. The compiler removes public validation for types that cannot
  contain Blobs; possible Blob occurrences are checked by walking the value.

The compiler intentionally reports statically knowable key, value, generic
arity, and callback-shape errors during AOT type checking. This differs from
the interpreter's runtime diagnostic timing and wording. Duplicate `set` with
a dynamic key remains a language-visible runtime failure.

## Private generic HAMT

`src/DarkCompiler/stdlib/__HAMT.dark` remains a generic persistent Hash Array
Mapped Trie. Its private operations retain Blob identity keys for compiler-owned
storage and lifetime testing; this capability is not exposed by the public API. Compiler-owned consumers such as Unicode tables use private
`Stdlib.Dict.__*` helpers for Int64 and other key types. User code cannot name
these helpers.

The private representation has four root tags:

- `0`: empty/null root
- `1`: bitmap-compressed internal node
- `2`: leaf containing key, value, and refcount
- `3`: full-hash collision node containing inline key/value pairs and refcount

Hashes are consumed six bits at a time, giving 64-way branching. Updates copy
only the modified path and retain shared children. Collision nodes, generic
hash/equality specializations, and typed retain/release plans remain intact.

### Native layouts

```text
internal:  [bitmap, child0, ..., childN, refcount]
leaf:      [key, value, refcount]
collision: [count, key0, value0, ..., refcount]
```

Tagged Dict roots use `__dict_to_rawptr` for a borrowed untagged view and
`__rawptr_to_dict` to create a managed tagged root. Typed `__raw_slot_init`
edges retain managed keys, values, and child Dict roots. Releasing a root walks
the concrete type-directed release plan, including collision payloads.

## Implementation and coverage

The contract is anchored in:

- `src/DarkCompiler/Stdlib.fs` for native intrinsic registration
- `src/DarkCompiler/Runtime.fs` and architecture code generation for native
  allocation, output, and failure behavior
- `src/DarkCompiler/frontend/TypeChecking.fs` for public typing and equality
- `src/DarkCompiler/stdlib/Dict.dark` for the public module
- `src/DarkCompiler/stdlib/__HAMT.dark` for private generic storage
- `src/Tests/e2e/dict_parity.e2e` and pinned upstream Dict/edict cases for the
  language boundary
- `src/Tests/e2e/stdlib-internal/dict-hamt.e2e`, refcounting tests, and
  architecture tests for private generic-key, collision, sharing, and ownership
  behavior

Performance differences from the interpreter are not parity requirements
unless they alter observable behavior. The compiler continues to use the HAMT;
it does not replace it with the interpreter's F# `Map` representation.
