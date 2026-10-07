# Dark Compiler — agent instructions

Read [`docs/index.md`](docs/index.md) first. It owns navigation. Follow the
shared repository rules in this file and exactly one environment file:

- ChatGPT app / Work: [`AGENTS.chatgpt.md`](AGENTS.chatgpt.md).
- Local Codex: [`AGENTS.codex.md`](AGENTS.codex.md).

Read the selected environment file in full; do not read the other file.
If the execution environment is unclear, ask before changing the repository.

## OCaml conventions

- The root `.ocamlformat` pins the standard default profile used for the
  post-port cleanup. To repeat that formatting pass, run
  `python3 scripts/format-ocaml.py`; use `--check` to inspect without editing.

- Preserve functional compiler algorithms and explicit interfaces. Localized
  parser, cache and host-I/O mutation is permitted. Do not use partial
  lookup helpers; host adapters may preserve caught host-API exceptions.
- Use `Option` only for semantic absence and `Result` for recoverable failure.
- Model invalid states out of existence; complete migrations and remove
  superseded representations rather than adding defaults or shims.
- Use `Crash.crash` for an impossible, undocumented state. Do not guess a
  default.

## Dark conventions

- Rely on type inference for module-scoped generic function and value uses.
  Do not write explicit type arguments such as `Stdlib.List.getAt<Int64>`
  unless inference is genuinely ambiguous and the explicit arguments are
  required for the program to compile.
- Standard library functions must not run shell commands or call executables
  on the user's system. The sole exception is a standard library function
  whose express purpose is to let the user make process calls.

## Change rules

- Create a failing, focused E2E test before fixing a compiler behavior.
- Preserve match validation as an ahead-of-time boundary: exhaustiveness,
  pattern and guard validity, binding consistency, and arm result types must
  fail during compilation when invalid. Never defer these failures to runtime.
  Use `compileerror=` or an upstream `#compileerror=` override when test
  correctness depends on the diagnostic phase.
- Test observable language behavior, not incidental compiler structure. Do not
  add IR or backend tests whose assertion is merely that a particular helper or
  call is present or absent. Use E2E tests for correctness and benchmarks for
  performance; add lower-level tests only when they validate independently
  meaningful backend behavior.
- Keep comments useful to a senior compiler engineer, including the required
  file-purpose comment.
- Use command-line flags rather than environment variables; use `python3` for
  scripts.
- Build the pinned native Dune graph with `./build --ai`; it keeps automated output
  bounded and retains complete failure logs under `TestResults/ai/`.
- `./run-tests` only executes an already-built test binary; it must never invoke
  a build. Run `./build --ai` first, and rebuild after changing
  source or project files before treating a test result as current.
- Before declaring a task branch ready, run the complete already-built host
  test suite with `./run-tests --ai`. Full-suite timing measurements are
  optional diagnostics and do not block integration when the host is contended.
- Do not run x64 tests on an ARM64 host unless explicitly testing x64 work.
  Likewise, do not run ARM64 tests on an x64 host unless explicitly testing
  ARM64 work.
- Run `dune runtest` for native regression and tooling checks as well; the
  complete host suite does not execute these aliases.
- Fix compiler warnings and errors before declaring work ready.
- Validate performance against the task branch's parent with
  `./benchmarks/run_benchmarks.sh --verify-parent full`. This performs the full
  benchmark measurement and fails on an aggregate regression or any individual
  benchmark regression of 0.1% or more against the snapshot stored by the
  upstream merge-base. Only submit a smaller individual regression
  when it cannot be avoided; investigate and explain that loss before handoff.
  Do not run or report
  `./benchmarks/run_benchmarks.sh --verify full` as a task-readiness gate: that
  command compares with the best-known snapshot rather than the branch parent.
- During merge-conflict recovery, never hand-merge generated benchmark reports
  or choose a conflicted side. Resolve sources and measurement snapshots first,
  then run `./benchmarks/bench report` and stage its generated files.
  Regeneration runs no benchmarks and requires no new performance improvement.
  Never combine measurement rows by hand: retain a complete compatible snapshot
  under its recording rules, or remeasure when neither is valid for the merged
  sources. Applicable Darklang regression gates remain independent.
  `./benchmarks/bench report --check` verifies presentation consistency only.

## Bounded inspection

- Search with `rg` before reading source, then inspect the smallest relevant
  range. Do not dump a whole large source file when a symbol or bounded range
  answers the question.
- Keep source and documentation reads to roughly 250 lines per command unless
  the additional context is demonstrably necessary.
- Start history and review inspection with bounded summaries such as
  `git status --short`, `git diff --stat`, `git diff --numstat`, or
  `git show --no-patch`. Never run an unbounded `git show`, `git diff`, or
  `git log -p`; select explicit paths or bounded ranges before reading patches.
- Capture verbose compiler, test, benchmark, disassembly, and profiling output
  in an ignored artifact. Return only a bounded diagnostic excerpt and the
  artifact path; expand it with a targeted search rather than reading it whole.
- Prefer `--dump-function`, `--dump-ir-summary`, and `--dump-ir-output` when
  inspecting compiler IR. Use complete program dumps only when the relationship
  between multiple functions is itself under investigation.

## Git workflow

- Distinguish investigation output from a lasting repository change. For an
  investigation, profiling run, or exploratory experiment, report findings to
  the user and keep raw measurements and temporary patches in ignored artifacts.
  Do not add a report to tracked documentation, add an index link, or commit
  solely to preserve the investigation. A user request to
  investigate does not by itself request a durable document. Commit and hand
  off an investigation document only when the user explicitly requests that
  document as a repository artifact. If the investigation produces an accepted
  code, test, benchmark, or durable documentation change, apply the ordinary
  readiness rules to that change.
- When work includes an intended lasting repository change and is complete,
  commit that change automatically. An investigation whose result is only
  findings for the user is complete after the findings are reported; it does
  not need a commit or handoff.

## Explaining technical work

- Pair abstract claims with a small, concrete example from the task when
  explaining behavior, design, or tradeoffs. Show the relevant input and
  outcome so the reader can see what the claim means in practice.
- For correctness, include a representative case and an important boundary
  case when useful. For example, name the missing match case and show that the
  program fails at compilation, rather than only saying match validation is
  ahead of time.
- For optimization, show a representative workload and what changes for it,
  such as fewer allocations or traversals. Include measured before/after
  numbers when available; label hypothetical examples clearly and do not
  present estimates as measurements.
- Use examples in progress updates and completion reports when they clarify
  the point, while keeping them short and relevant to the user's question.
