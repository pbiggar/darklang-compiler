# Known Issues

Language and standard-library differences from the interpreter are tracked in
the [compatibility overview](../compatibility/overview.md) and its linked
ledgers. Incomplete engineering work without a reproducer is tracked in the
[roadmap](roadmap.md).

Add an issue here only when it has:

- a minimal source reproduction;
- expected and actual behavior;
- the affected target or compiler pass; and
- a focused failing test, or a reason the test cannot yet be committed.

Move the durable behavior into the relevant compiler or compatibility document
and delete the issue entry when it is fixed. Git history retains the
investigation.
