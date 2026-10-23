# Darklang compatibility

The [source ledger](../porting/README.md) records current compiler decisions
beside every imported upstream declaration and test. Its
[aggregate report](../porting/generated/ledger.md) counts the complete pinned
Dark corpus directly, without matching adapted fixture formats or maintaining
separate exclusion tables.

The [historical compatibility audit](current-audit.md) preserves earlier native
runs of adapted fixtures and the compiler gaps they exposed. Those fixtures
remain regression tests; their per-test annotations replace the former central
gates. Ordinary expectations and strict expected failures are checked by the
current native suite.

The [trait compatibility record](language/traits.md) records the current
implementation scope, native test evidence, intentional AOT differences,
and unfinished milestones.
