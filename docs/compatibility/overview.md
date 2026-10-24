# Darklang compatibility

The [source ledger](../porting/README.md) records compiler decisions beside
imported upstream declarations and tests. Its [aggregate report](../porting/generated/ledger.md)
counts the complete corpus at the pin in `porting/sources.yaml`.

Language and standard-library documents explain semantic contracts, edge cases,
and intentional AOT differences. Their named interpreter baselines identify the
sources compared; they are not execution receipts for the current revision.
Current selection and expected failures live in source annotations. The native
suite checks ordinary expectations and strict expected failures on each run.

- [Approved adoption decisions](upstream-adoption.md)
- [Deferred package-review decisions](package-review.md)
- [Trait semantics and scope](language/traits.md)
- [CLI compatibility](cli.md)

Retain contracts, unresolved work, decisions and their rationale. Keep raw run
receipts in ignored artifacts. Completed investigations, old failure counts,
and superseded plans are recoverable from Git history; do not maintain them as
parallel status inventories.
