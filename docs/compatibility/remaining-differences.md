# Remaining compatibility work

The [source ledger](../porting/README.md) is the current inventory of imported
items. Per-item annotations in `upstream/` record blocked declarations and tests,
intentional exclusions, compiler replacements and phase-specific expected
failures. [Aggregate counts](../porting/generated/ledger.md) come from the native
parser and those source annotations.

Source dispositions do not claim successful execution. The native suite checks
ordinary expectations and strict expected failures on each run. Keep policy
and deferral rationale in [package-review decisions](package-review.md), and
engineering work without a reproducer in the [roadmap](../project/roadmap.md).
