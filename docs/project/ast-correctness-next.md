# AST correctness follow-up

The parsed/checked split and certified checked types are described in
[intermediate representations](../compiler/intermediate-representations.md).
Use [compiler identities](../compiler/identities.md) for identity and lookup
invariants. Do not repeat the completed split merely to resemble interpreter
representations.

## Remaining design choices

- **Backend representation and ABI types.** Inventory consumers of
  `TInferenceVar`, `TNever` and `TInternalRawPtr` before choosing another type
  boundary. Checked signatures already certify the absence of call-local
  inference identities. ANF/MIR still need nominal arguments for descriptors,
  specialization, ownership and diagnostics; a tiny machine-type union cannot
  replace semantic types everywhere. Give nonreturning expressions an explicit
  policy rather than inventing a runtime value for `TNever`.
- **Recursive binding shape.** `CheckedAST.RecursiveLet` accepts an arbitrary
  expression as its value. A lambda-specific checked value could encode the
  recursive-lambda invariant if producers and consumers migrate completely.
- **Tuple-pattern cardinality.** Source tuple expressions have two or more
  elements, but internal nullary-constructor payload patterns can be empty.
  Separate source patterns from internal payload patterns before tightening
  `PTuple`; zero-/one-element storage tuple types remain valid.
- **Prepared-expression boundary.** Introduce another tree only when a proven
  post-checking invariant excludes real invalid states. Public `WrittenTypes`
  already exclude compiler-generated operations.
- **Nominal IDs and constructor arity.** Require a concrete repeated lookup or
  lost checked fact before changing remaining strings or field lists. Preserve
  the distinction between layout-complete record literals and last-wins record
  updates, which allow duplicate updates.

## Constraints on further migrations

Pick one invariant, migrate its producer and consumers completely, and remove
the superseded representation. Preserve AOT match validation, privileged raw
pointer boundaries, generic type arguments, nonreturning match arms, dictionary
ownership and backend ABI behavior. Use the [verification policy](../contributing/verification.md).
Representation changes have no established speed or memory benefit without
measurement; additional conversions can make both worse.
