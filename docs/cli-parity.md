# CLI parity ledger

This ledger records reproducible comparisons with the current interpreter CLI
surface. Historical inventories, including DCB1 report commit `8a402797`, are
starting evidence only and are not treated as current results.

## Revisions and host

- Compiler under test: `041f466c2827fd16de70ce34d3ca7910350f92b8`
- Integration base: `8025617cb935bb959308315a0b886228800d06fe`
- Original compiler parent inspected before editing:
  `42004a9a073dba5ed75c771cb7cb03d15e4aabe5`
- Interpreter source and expectation revision:
  `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`
- Host: Linux `6.8.0-100-generic`, `aarch64`
- Probe date: 2026-08-14 UTC

The interpreter revision was checked out directly from `darklang/dark`. The
portable implementations are derived from
`packages/darklang/stdlib/cli/path.dark` and the `globMatchSegments` workflow in
`packages/darklang/stdlib/cli/file.dark`. Expected values come from the exact
same-source probes at
`backend/testfiles/execution/stdlib/cli-path.dark` and
`backend/testfiles/execution/stdlib/cli-glob.dark`, copied in this repository
under `src/Tests/e2e/upstream/stdlib/`.

## Executed comparisons

| Surface | Probe and command | Returned values/errors | Side effects and lifecycle | Result/classification |
| --- | --- | --- | --- | --- |
| `Stdlib.Cli.Path.basename`, `parent`, `extension`, `withExtension`, `isAbsolute`, `join`, `normalize`, absolute `resolve`, and absolute `relativeTo` | `./run-tests --ai --filter=cli-path` at compiler revision above | All 40 values exactly matched the literals in `cli-path.dark`; no errors were expected | Pure; no filesystem or process effects | 40/40 parity on Linux arm64 |
| `Stdlib.Cli.File.globMatchSegments` for literal segments, `*`, `?`, and recursive `**` | `./run-tests --ai --filter=cli-glob` at compiler revision above | All 27 booleans exactly matched `cli-glob.dark`; no errors were expected | Pure; all recursion completed and the process exited normally | 27/27 parity on Linux arm64 |
| Multiple `*` wildcards within one segment | `./run-tests --ai --filter=cli_filesystem` at compiler revision above | `a*b*c` matched `axbyc` and rejected `axbyd` | Pure; matcher completed without recursive ownership faults | 2/2 compiler regression cases passed on Linux arm64 |

Both files are in the default executable upstream set, so these comparisons no
longer depend on a test filter.

## Repository verification

- `./run-tests --ai`: 6,057/6,057 tests passed in 33.7 seconds.
- `./benchmarks/run_benchmarks.sh --verify routine`: all 19 parity contracts
  and benchmark workloads passed. The current instruction counts were equal to
  the audited Dark baseline for every workload (geometric ratio `1.000000`).
  The routine Performance ratio reported by `benchmarks/RESULTS.md` is `2.75x`.

These commands exercised compiler revision
`041f466c2827fd16de70ce34d3ca7910350f92b8` on the Linux arm64 host recorded
above. The benchmark's pinned Dark baseline is
`aa0c36de548eaaddd363b6497ac249ed9c2e3134` with workload contract
`6dbb096b37aaf32192bf960168fde271a2595f94f83653b43b94ec5b2e104758`.

## Platform coverage

| Target | Probe availability | Classification |
| --- | --- | --- |
| Linux arm64 | Executed on the host above | Available and passing for the rows above |
| Linux x86_64 | No matching host in this run | Unavailable host capability; not compared |
| macOS arm64 | No matching host in this run | Unavailable host capability; not compared |
| macOS x86_64 | The compiler does not yet represent this target | Missing compiler target; not an intentional divergence |

## Boundaries and non-claims

The compiler still exposes the older `Stdlib.File` and `Stdlib.Path` intrinsics.
They are compiler-only extensions and are not evidence for the canonical CLI
contracts. `Stdlib.Cli.Path.resolve` currently has canonical behavior only for
absolute inputs: relative inputs require the process-state `getcwd` primitive.
The loaded `Stdlib.Cli.Posix.Error` and `StatResult` declarations establish the
canonical public shapes but do not stand in for native POSIX operations.

No filesystem mutation, environment, descriptor, shell, download,
decompression, watch, lock, ownership, signal, or daemon comparison was run in
this revision. Those surfaces therefore have no parity classification in this
ledger. In particular, absence of a row must not be read as either parity or an
intentional divergence.

The compiler remains ahead-of-time and may diagnose statically knowable errors
before execution. That timing difference is the retained AOT extension; it does
not authorize different successful values, errno records, side effects, or
resource lifecycles.
