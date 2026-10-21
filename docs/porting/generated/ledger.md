# Current compiler port status

[How to read and update this ledger](../README.md).

Interpreter revision: `e76430be5cd9f6b77ed255430dc34a1b58dd8fc4`.

| Area | Status counts |
| --- | --- |
| [features](../../../porting/language-features.yaml) | 195 supported, 16 missing, 2 partial |
| [parser](../../../porting/parser.yaml) | 20 supported, 7 missing, 4 partial |
| [stdlib](../../../porting/generated/stdlib.yaml) | 1010 different, 598 missing, 388 identical |
| [packages](../../../porting/generated/packages.yaml) | 3709 missing, 5 identical, 41 different |
| [builtins](../../../porting/generated/builtins.yaml) | 79 present, 175 missing, 527 unassessed |
| [tests](../../../porting/generated/tests.yaml) | 552 enabled, 1593 unmapped, 10 ambiguous, 82 disabled, 122 partial |

StdLib and package states compare declaration source, not behavior. Builtin presence identifies a binding, not behavioral parity. Enabled tests have not necessarily been executed in this checkout. Packages exclude StdLib; all counts refer to upstream items only.
