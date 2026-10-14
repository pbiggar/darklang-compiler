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
it does not require a separately provisioned environment. In the current VM,
OCaml builds successfully, but network policy denies the bootstrap's pinned
`snapshot.ubuntu.com` downloads, leaving the native dependencies unavailable.
This is a validation blocker, not a package-source availability problem.

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
path for compiler-only items). Path boundaries matter: `packages/acme` does not
match `packages/acmeOther`. Use `item: language:expr/EIf` for a precise item
exception. Nested item selectors must belong to their parent scope. Children
cannot overlap unless explicitly nested; overrides require a reason. Duplicate
YAML keys, unknown fields, unmatched exceptions, and orphaned evidence fail.

`depends_on` accepts category names or exact item IDs. Record dependencies when
learned, rather than inventing a complete graph before porting begins. Root
policies do not imply a section is complete. A deferred item retains its required
contract; deferral does not declare compatibility.

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
