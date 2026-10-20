# Current compiler port status

[How to read and update this ledger](../README.md).

Interpreter revision: `a9d93efc853486244630dad693fe79f6f4882cf6`.

| Area | Status counts |
| --- | --- |
| [features](../../../porting/language-features.yaml) | 194 supported, 16 missing, 2 partial |
| [parser](../../../porting/parser.yaml) | 18 unassessed, 1 supported |
| [stdlib](../../../porting/generated/stdlib.yaml) | 1010 different, 609 missing, 388 identical |
| [packages](../../../porting/generated/packages.yaml) | 3703 missing, 5 identical, 41 different |
| [builtins](../../../porting/generated/builtins.yaml) | 79 present, 174 missing, 527 unassessed |
| [tests](../../../porting/generated/tests.yaml) | 547 enabled, 1581 unmapped, 10 ambiguous, 83 disabled, 125 partial |

StdLib and package states compare declaration source, not behavior. Builtin presence identifies a binding, not behavioral parity. Enabled tests have not necessarily been executed in this checkout. Packages exclude StdLib; all counts refer to upstream items only.
