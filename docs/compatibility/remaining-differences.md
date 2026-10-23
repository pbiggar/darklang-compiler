# Remaining compatibility work

The [source ledger](../porting/README.md) is the current inventory of imported
items. Per-item annotations in `upstream/` record blocked declarations and tests,
intentional exclusions, compiler replacements and phase-specific expected
failures. [Aggregate counts](../porting/generated/ledger.md) come from the native
parser and those source annotations.

The [historical failure audit](current-audit.md) preserves earlier execution
results for adapted fixtures. Current source dispositions do not claim an
execution receipt; the native test suite checks ordinary expectations and strict
expected failures on every run.
