# Dict: public contract and private HAMT

This document distinguishes Dark's public dictionary contract from the native
HAMT that implements it.

## Parity baseline

The public contract was revalidated against these exact revisions:

- compiler comparison point: `c97f99a1c953b4e6739daafc05934f98924e139f`
- darklang/dark interpreter: `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`
- DCB1 report `8a402797` was starting evidence only; retained findings were
  checked again against the revisions above.

The interpreter's F# Builtins Dict module and packaged `dict.dark` define the
language-visible contract. Ahead-of-time compilation and the HAMT layout are
compiler implementation details.

## Public type and literals

Source programs expose only a String-keyed, one-parameter type:

```dark
Dict<Int64>
```

`Dict<K, V>`, non-String public keys, and two-type-argument Dict calls are
compile-time errors. The compiler retains `TDict(keyType, valueType)` internally
because native HAMT helpers are monomorphized over both components.

Dictionary literals are distinct from records:

```dark
Dict { }
Dict { name = "dark"; ``Content-Length`` = "0"; ___ = "empty key" }
```

Backticks quote non-identifier keys and `___` spells the empty String key.
Literal entries are evaluated in source order. Duplicate literal keys and mixed
value types are rejected statically.

## Public `Stdlib.Dict` surface

`Stdlib.Dict.empty` is a polymorphic module value, not a function. The complete
public callable surface is:

```dark
isEmpty(dict: Dict<a>) : Bool
singleton(key: String, value: a) : Dict<a>
size(dict: Dict<a>) : Int
get(dict: Dict<a>, key: String) : Option<a>
set(dict: Dict<a>, key: String, value: a) : Dict<a>
setOverridingDuplicates(dict: Dict<a>, key: String, value: a) : Dict<a>
remove(dict: Dict<a>, key: String) : Dict<a>
merge(left: Dict<a>, right: Dict<a>) : Dict<a>
keys(dict: Dict<a>) : List<String>
values(dict: Dict<a>) : List<a>
toList(dict: Dict<a>) : List<(String, a)>
fromListOverwritingDuplicates(entries: List<(String, a)>) : Dict<a>
fromList(entries: List<(String, a)>) : Option<Dict<a>>
member(dict: Dict<a>, key: String) : Bool
map(dict: Dict<a>, fn: String -> a -> b) : Dict<b>
iter(dict: Dict<a>, fn: String -> a -> Unit) : Unit
filter(dict: Dict<a>, fn: String -> a -> Bool) : Dict<a>
filterMap(dict: Dict<a>, fn: String -> a -> Option<b>) : Dict<b>
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
  ordinal String-key ordering. HAMT bitmap shape, collision layout, insertion
  history, and structural sharing are not observable.
- equality compares entry counts, String key sets, and values using the
  concrete value type's existing equality semantics.
- rendering produces `Dict { key = renderedValue; ... }`, including for empty
  and nested dictionaries.

The compiler intentionally reports statically knowable key, value, generic
arity, and callback-shape errors during AOT type checking. This differs from
the interpreter's runtime diagnostic timing and wording. Duplicate `set` with
a dynamic key remains language-visible at runtime and is emitted safely on
both native architectures.

## Private generic HAMT

`src/DarkCompiler/stdlib/__HAMT.dark` remains a generic persistent Hash Array
Mapped Trie. Compiler-owned consumers such as Unicode tables use private
`Stdlib.__HAMT.__*` helpers for Int64 and other key types. User code cannot name
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
- `src/DarkCompiler/passes/1.5_TypeChecking.fs` for public typing and equality
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
