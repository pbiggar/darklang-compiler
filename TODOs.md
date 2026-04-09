# TODOs

## Active

- **x86_64 reference counting** — 1 test failure remaining (memReclaimBurn).
  RC infrastructure is built but disabled. See [docs/x64-refcounting.md](docs/x64-refcounting.md).

## Next up

- x86_64 encoding test coverage — 36 of 47 instruction types untested at byte level
- Determine what % of PT.fs capabilities are handled
- Run compiler against existing package repo
- Additional architectures (arm32?)

## Short term

- int64 assumptions
- fix indentation to not nest so deeply
- add values

## Long term

- mutmut testing
- matching darklang language
- increasing code coverage
- completing benchmarks
- expanding to support full language
- support full darklang stdlib
- Json stdlib module (parsing/serialization)
- support full darklang test suite
- reimplement darklang compiler in Darklang
- reimplement test suite in Darklang
- complete Unicode string support
- add optimizations
- remove crashes
- end-to-end SSA
- SSA-based HIR (sub ANF?)
- SCCP-based HIR, MIR, and LIR optimizations
- remove non-functional idioms
- unify memory management (RawPtr, heap primitives, reference counting)
