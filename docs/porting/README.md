# Porting framework

This framework supports incremental porting of the compiler into `darklang/dark`.
It records requirements, observed source state, intended destinations, and
verification evidence separately. It does not decide language behavior or approve
upstream changes. The initial execution target is x64 Linux.

Start with the [generated inventory](generated/index.md). It covers language
syntax, parser components, upstream test sources, checked-in packages, Builtins,
and StdLib. These initial discoveries are deliberately source-level: read the
report's discovery boundaries before interpreting a count as coverage.

## Reproduce and refresh

Install the small, pinned Python dependency in a virtual environment:

```bash
python3 -m venv /path/to/porting-venv
/path/to/porting-venv/bin/pip install -r scripts/porting-requirements.txt
```

Use an interpreter checkout at the exact commit in `porting/sources.yaml`.
Discovery reads committed blobs from that revision, not dirty upstream files.
It never contacts a hosted package manager, executes Dark code, or changes the
interpreter. Updating the pin is an explicit reviewed edit, not part of refresh.

```bash
python3 scripts/test_porting.py
python3 scripts/porting.py refresh --upstream /path/to/interpreter
python3 scripts/porting.py check --upstream /path/to/interpreter
python3 scripts/porting.py next --upstream /path/to/interpreter --category parser
python3 scripts/porting.py show --upstream /path/to/interpreter --item language:expr/EIf
python3 scripts/trial-package-layout.py --upstream /path/to/interpreter
```

Activate the virtual environment first, or use its Python executable. `check`
checks policy/output consistency only; existing compiler verification remains
independent. Behavioral CI enforcement is undecided. `next` lists candidates
for consultation, not automatically approved implementation tasks. A category
dependency remains blocked while any required item in it lacks current passing
evidence. Future execution adapters belong to the representative trials; there
is no pretend `verify` operation that turns source observations into passes.

CI runs `bash scripts/check-porting`: it creates a temporary Python environment,
fetches the exact pinned public upstream commit, runs the framework regressions,
and checks generated inventories. Network access is used only to materialize
pinned inputs and the pinned Python dependency. For offline checks, use the
commands above with an existing checkout and environment.

## First layout trial

`trial-package-layout.py` uses the actual checked-in upstream Env and FileSystem
files from the pin. It copies the compiler library into a temporary directory,
replaces their current manifest entries with unchanged files at upstream paths,
and invokes the real embedding generator. Three cases cover a simple file, an
intact nested module, and both together. Independent decoding of the emitted
OCaml byte strings checks exact source preservation and declaration order;
the trial checks production sources and their manifest remain unchanged.

Env, File, Process, Sys, and root Cli execution are now production copies under
`packages/darklang/stdlib/cli/`. Their sources are byte-identical to the pin,
the previous embedding slots are retained, and the old files are removed.
The sync script writes their mirrored paths. Production regressions verify
the copies and absence of old duplicates. The remaining FileSystem
trial still operates only in a temporary tree.

This establishes that the embedding generator accepts mirrored paths and
does not require splitting nested modules. Source inspection also finds nested
module flattening in `WrittenSource`, but only compilation can establish that
name resolution, type checking, and declaration dependencies work for these
unchanged files. The trial itself does not migrate files or claim runtime parity.

The next boundary is native compilation of the temporary library. VM setup
uses the compiler's `scripts/vm/setup-native-toolchain` and `scripts/vm/README.md`;
it does not require a separately provisioned environment. The latest retry reused
the pinned OCaml compiler, installed the native prerequisite libraries, and
initialized the pinned opam switch. Locked OCaml dependencies remain unavailable:
the network proxy returned HTTP 403 CONNECT failures for opam cache downloads
and package source downloads. Native build and execution remain unverified;
this is a validation blocker, not a package-source availability problem. Do not
bypass network restrictions or replace pinned sources/checksums to obtain a pass.

## Broad policies and nested exceptions

`porting/manifests/` has one YAML manifest per category. Roots state current
architecture, intended source/behavior relationship, destination, freshness,
decisions and open questions. StdLib extends the package policy, including its
applicable nested path exceptions. A child overrides only the fields it names.
Exclusions may themselves contain narrower exceptions:

```yaml
scope: packages/acme
policy:
  disposition: deferred
reason: Record the user's decision and the prerequisite here.
children:
  - scope: packages/acme/math.dark
    policy:
      disposition: required
    reason: This file can be ported independently.
```

All scopes are full relative upstream paths (or `compiler/` plus a compiler
path for compiler-only items), except the named-test module scopes below.
Path boundaries matter: `packages/acme` does not
match `packages/acmeOther`. Use `item: language:expr/EIf` for a precise item
exception. Nested item selectors must belong to their parent scope. Children
cannot overlap unless explicitly nested; overrides require a reason. Duplicate
YAML keys, unknown fields, unmatched exceptions, and orphaned evidence fail.

`depends_on` accepts category names or exact item IDs. Record dependencies when
learned, rather than inventing a complete graph before porting begins. Root
policies do not imply a section is complete. A deferred item retains its required
contract; deferral does not declare compatibility.

### Named upstream tests

The test inventory retains file groups and adds generated identities such as
`tests:packages/darklang/tests/language/traits.dark#Darklang.Language.Tests.Traits.Shapes.implMethodIsFn`.
An identity is the checked-in file plus its qualified declaration name, not its
line number. Moving a declaration between modules or renaming its file changes
the identity and requires reviewing any existing evidence or exceptions.

A file scope includes its named tests. Inside it, use synthetic module scopes
such as `packages/darklang/tests/language/traits.dark/named/Darklang/Language/Tests/Traits/Shapes`,
with narrower nested scopes or exact `item` exceptions as needed. These scopes
describe declarations, not filesystem directories. Names and module grouping
are generated; only decisions and exceptions need handwritten manifest entries.

Discovery masks comments/string literals and follows indentation-scoped modules.
It distinguishes declaration headers from record fields and local expressions.
Unsupported declaration-level module/test headers and duplicate qualified names
within a file fail refresh instead of silently dropping or merging items. This
is a bounded source scanner, not the interpreter parser or runner. Each named
record links to the current source line, records its file group, and inherits
the file's source fingerprint. Inserting lines preserves the ID but invalidates
evidence conservatively. No declaration is marked passing simply by discovery.

### Actual legacy runner discovery

After a successful native build, export the imported legacy corpus with:

```bash
./run-tests --target=linux-x86_64 --inventory-upstream=TestResults/upstream-discovery.yaml
```

This mode uses `E2EFormat.parseE2ETestFile` and the same `E2EDiscovery` gate
application as normal execution. It returns before StdLib compilation and test
execution, without building anything. Files are sorted; paths are relative to
the repository; the YAML contains no timestamps or timing data. Each parsed
case records its runner name, source line, AOT/error expectation, effective skip
reason, gate provenance, and eligibility. Fixture metadata takes precedence over
whole-file and line gates, preserving the existing execution contract. A line
gate matches the parser's `L<number>:` name, not an inferred source-line count.

Parse errors are retained in the output and make the command fail. Filtered or
duplicate inventory requests fail instead of exporting a misleading subset.
The export covers only legacy imported `.dark` fixtures, not current named
package tests, compiler-owned tests, or other test suites. Eligibility means
the runner would select a case; it does not establish compilation or a pass.

The command and native regressions are implemented but have not run in this VM:
the pinned native dependency setup remains blocked. Consequently the framework
still reports effective execution support as unverified. Do not commit a
guessed discovery output or turn configured gates into skip counts.

To collect and reconcile a real export into tracked results, run:

```bash
python3 scripts/collect-upstream-discovery.py --upstream /path/to/interpreter
```

The collector validates the pin, builds with the repository's bounded build
command, then exports through the already-built runner. It fingerprints compiler,
fixture, script, build-graph and manifest inputs before and after collection;
changing inputs aborts without replacing the previous ledger. It validates the
schema, complete corpus coverage, unique identities, source coordinates, skip
consistency and the runner's exit status. Native build failure also leaves the
previous ledger untouched. No performance checks run.

`porting/discovery.yaml` holds the captured export and its source provenance.
Its initial `null` entry means not collected, not zero tests. A successful
collection refreshes the inventories; a partial collection records parse errors
and exits unsuccessfully. Framework refresh/check never executes the collector.
Changes make discovery **stale** independently of behavioral evidence. Stale case
selectors remain available while their file exists, but their old eligibility
and counts never become current claims. Actual parsed cases inherit file scopes
under `compiler/FILE/runner/HASH`; use their generated IDs for precise exceptions.
These legacy IDs hash the parser's display name, including line coordinates,
unlike the line-independent current named-package-test identities.

Current, successfully parsed files also report configured line gates that match
no actual parser `L<number>:` identity. These are consultation candidates, not
automatic permission to delete gates. Failed or stale files do not infer them.

### Builtin registration candidates

Each builtin source remains a broad file group. Refresh also enumerates literal
`fn`/`value` registrations with kind, name, version and source links, under scopes
such as `FILE/declarations/fn/boolNot/v0`. File-wide rules apply automatically;
narrow exceptions do not require pre-writing a manifest for every function.
Computed names/versions remain counted source gaps. Literal constructors can be
called repeatedly, renamed or filtered, so none of these counts is an assembled
registry count. Same-spelling compiler declarations are only mapping candidates;
signatures, host registration, aliases and behavior still need actual adapters.

The List/Dict source destinations start undecided, while their behavior remains
required to match. This does not approve replacing their native representations
or permit behavioral differences. The desired package destination is a mirror
of upstream's tree, but changing today's embedding/layout requires a trial and
consultation. Root-module matches are only candidate source mappings.

## Evidence and bugs

`porting/evidence.yaml` holds recorded behavioral check outcomes. Copy an item's
fingerprint from `show` after running its check, and record:

```yaml
entries:
  - id: language:expr/EIf
    fingerprint: COPY_THE_CURRENT_FINGERPRINT
    target: linux-x64
    status: pass
    command: RECORD_THE_ACTUAL_COMMAND
    notes: Record the measured coverage and limitations.
```

Allowed outcomes are pass, fail, and blocked. Absence is unverified; changed
inputs make an entry stale. Invalidation is intentionally conservative across
compiler sources, test sources, manifest policy, and the upstream revision.
Changing evidence itself does not invalidate its fingerprint. A passing check
only establishes the coverage described in its notes, not universal equivalence.
Keep logs outside tracked reports; fingerprints and sorted relative paths avoid
timestamps, worktree locations, and output completion-order noise.

`porting/issues.yaml` records upstream bugs, with fields `id`, `items` (inventory
IDs), `observed`, `intended`, `decision`, `upstream_permission`, `upstream_pr`, and
`fix_revision`. Permission is not-requested, requested, or granted. Identifying
a bug does not authorize editing upstream or silently exempt its compiler test.
Discuss the correction, obtain explicit permission before upstream changes,
and track the eventual PR/fix revision. AOT type/match rejection rules are agreed;
new kinds of AOT differences still require consultation.

## Independent sessions and PR completion

Sessions work with the user independently. There is no agent messaging,
ownership service, or automatic orchestration. Shared knowledge is the merged
repository, not another session's conversation or unmerged branch.

For a module/file/category port:

1. Update from main and read its effective manifest, evidence, and open questions.
2. Reproduce the relevant case and inspect candidate mappings before relying on them.
3. Consult the user on meaningful choices, deferrals, destinations, or new differences.
4. Implement and run focused checks plus applicable repository validation.
5. Include agreed manifest decisions, evidence and refreshed reports in the PR.
6. After integration, regenerate aggregate reports from combined sources rather
   than hand-merging generated YAML or Markdown.

Do not wait for complete classification before porting a useful section. Start
representative trials in any of the six categories. They establish the missing
copying, parser comparison, test execution, builtin registry, or semantic probe
adapters as actual work exposes the requirements.

## Named package test syntax trial

The compiler now follows the pinned interpreter's distinction between a named
`DTest` declaration (`test name = body`) and a legacy `DAssertion`
(`actual = expected`). Named declarations preserve the body, documentation,
keyword and outcome ranges, and optional `=> raises "message"` expectation.
`test` remains a contextual identifier. Named tests are valid only for Package
purpose; legacy assertions retain their Test-only boundary.

`test/regression/named_test_parser_regression.ml` covers simple and nested tests,
multiline bodies, documentation/ranges, expected errors, malformed outcomes,
contextual identifiers, and both purpose boundaries. Build first, then run
`dune exec test/regression/named_test_parser_regression.exe` or `dune runtest`.
These native regressions have not run in this VM; syntax checking is not
behavioral evidence. Executable source flattening explicitly rejects named
tests until the execution adapter is implemented.

`NamedPackageTests.partition` now provides the explicit runner boundary. It
accepts validated Package source, retains tests in declaration order with their
qualified module scopes, and returns a separately validated package containing
the ordinary declarations. Test bodies, documentation, source ranges, and
expected errors remain unchanged. Duplicate qualified test identities are
rejected; identical short names in different modules remain distinct. Ordinary
compiler entry points still reject named tests instead of silently dropping them.

`test/regression/named_test_partition_regression.ml` covers ordered/nested scopes,
helper and value preservation, unchanged test declarations, duplicate names,
test-free packages, and the explicit Package boundary. These regressions have
not run: native setup was retried and failed downloading pinned native packages
with HTTP 503. OCaml parsing checks passed, which does not establish type-checking
or execution. The next step is unchanged `Stdlib.Test.Result` helper code and
test-local runtime context, followed by independent compilation and execution of
each extracted body with its original expected outcome.

`NamedPackageTests.executable` selects a test by its full qualified identity and
prepares a validated Script with that unchanged body as its only entry. The entry
stays at the test's original declaration position and module scope; all ordinary
declarations are retained and other test bodies are removed. The selected test's
original declaration, including its expected error, is returned separately for
the runner. Unknown identities, duplicate identities and non-Package inputs are
rejected. Preparation does not execute the body or interpret its result.

The partition regression now also checks independent preparation, source order,
expected-error preservation and lexical name resolution through the ordinary
checker. OCaml parsing checks and all 47 ledger-tooling regressions pass; the
native regressions still require the pinned dependency setup and build. The five
package-layout regressions also pass, establishing only byte preservation and
embedding, not compilation or runtime compatibility. The
unchanged Test helpers, test-local builtin context, result/error interpretation
and actual per-test execution remain unfinished.
