# Records

Records are nominal, immutable product types with named fields. Their public
behavior follows darklang/dark revision
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.

## Grammar

Declarations name the record and may declare ordered type parameters:

```dark
type Point = { x: Int64; y: Int64 }
type Box<'a> = { value: 'a }
```

Declarations and constructions accept commas, semicolons, or newlines between
fields. A declaration must contain at least one field. Construction always has
a type name and may carry explicit type arguments:

```dark
Point { x = 1, y = 2 }
Package.Box<String> { value = "kept" }
```

Anonymous literals, obsolete declaration/literal spellings, and record
patterns are rejected. Records can be matched by binding the whole value and
then using field access.

Postfix access composes normally (`outer.inner.value`). An update has at least
one field:

```dark
{ point with x = 3; y = 4 }
```

## Identity and metadata

The compiler resolves every record reference to a qualified nominal type name
and an ordered list of concrete type arguments. Aliases resolve to the same
runtime nominal identity; source spelling is retained in typed record
references where it is available. Distinct declarations are never compatible
merely because their fields have the same shape.

Record metadata preserves the nominal name, declared type parameters
(including phantoms), declaration-order fields and slots, first-declared field
lookup, and ordinal-name presentation order. Generic substitution always uses
declared parameter order. Explicit arguments require exact arity; omitted
arguments are inferred by the AOT checker from fields and the expected type.

## Validation and evaluation

Construction distinguishes duplicate source fields, missing fields, unknown
fields, empty normalized keys, and field type mismatches. Duplicate declaration
names follow the interpreter's first-declared lookup behavior. A valid
construction evaluates each initializer exactly once from left to right, then
places values into declaration-order native slots.

An update evaluates its base once and update expressions once from left to
right. Duplicate update names are allowed and the last value wins. Unknown or
empty names and wrong value types are rejected. Untouched fields and resolved
type arguments are preserved.

Access is type-directed through nominal metadata. Empty names, missing fields,
and non-record receivers are diagnosed at the earliest sound AOT boundary.

## Runtime layout and ownership

A record is a reference-counted fixed block:

```text
slot 0       stable nominal descriptor identity
slot 1..N    fields in declaration order
trailer      reference count (managed by the allocator)
```

The immutable compiler descriptor carries source and resolved names, concrete
type arguments, field-name-to-slot metadata, and presentation order. ANF has
distinct record allocation, projection, and clone operations, lowered to the
shared fixed-block heap instructions with the descriptor included in all
offsets and sizes. Tuples remain a separate representation.

Ownership shapes mark the descriptor immediate and recursively retain/release
concretely substituted fields. Nested records and managed generic fields are
therefore handled on ARM64 and x86-64 without treating tuples as records.

## Equality and rendering

The AOT checker admits equality only between compatible nominal types.
Generated helpers project the same named fields and apply recursive equality;
aliases of one resolved declaration compare as that declaration. The runtime
descriptor prevents records from being confused with tuple payloads.

Rendering uses the resolved qualified name and concrete generic arguments.
Fields are recursively rendered in ordinal key order as `field: value`. Values
within the interpreter's 80-character threshold use one line; longer values
use an indented field-per-line layout.

## Intentional compiler differences

The interpreter can discover some invalid access/update situations while
evaluating. The compiler reports the equivalent error during parsing or type
checking when the invalidity is statically known. AOT monomorphization and
ownership planning do not add record syntax or structural compatibility.

## Key files

| File | Purpose |
| --- | --- |
| `src/DarkCompiler/AST.fs` | nominal record references and public expressions |
| `src/DarkCompiler/passes/1_InterpreterParser.fs` | canonical interpreter-compatible grammar |
| `src/DarkCompiler/passes/1.5_TypeChecking.fs` | metadata, substitution, validation |
| `src/DarkCompiler/passes/1.6_ValueRendering.fs` | record rendering |
| `src/DarkCompiler/passes/2_AST_to_ANF.fs` | record allocation, clone, and projection |
| `src/DarkCompiler/ANF.fs` | descriptors and ownership shapes |
| `src/Tests/e2e/records.e2e` | public behavior regressions |

See [record parity](../record-parity.md) for revision-stamped evidence.
