# Architecture Decisions

- The compiler is designed to eventually be rewritten in Darklang itself. Using pure functional patterns makes this transition easier.

- Use multiple intermediate representations (AST → ANF → MIR → LIR → ARM64).

- Generate Mach-O binaries directly without using an assembler or linker
