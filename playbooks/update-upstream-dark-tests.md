# Upstream Dark Tests Refresh Playbook

Goal: refresh `src/Tests/e2e/upstream/` from `darklang/dark` and re-apply this repo's intentional local deltas in a repeatable way.

## Scope
This playbook updates:
- `src/Tests/e2e/upstream/` (copied upstream `.dark` execution tests)
- `scripts/upstream-execution-expected.patch` (expected local delta from upstream)
- `src/Tests/test-suite-tooling/TestRunner.fs` (upstream include list / line allowlist)

It does not change compiler behavior unless required to keep tests green.

## Prerequisites
1. Run from repo root.
2. Use `python3` for scripts.
3. Start on top of current `main`:
```bash
git rebase main
```

## 1) Pre-flight
1. Ensure clean tree:
```bash
git status --short --branch
```
2. Capture baseline diff report against upstream:
```bash
python3 scripts/diff-upstream-execution-tests.py \
  --ref main \
  --ignore-expected-diff \
  --output /tmp/upstream-refresh-before.txt
```

## 2) Sync Local Upstream Copy
1. Clone upstream execution test subtree:
```bash
tmp=$(mktemp -d)
git clone --depth 1 --filter=blob:none --sparse https://github.com/darklang/dark.git "$tmp/dark"
git -C "$tmp/dark" sparse-checkout set backend/testfiles/execution
git -C "$tmp/dark" checkout main
```
2. Replace local upstream directory with exact upstream snapshot:
```bash
rsync -a --delete \
  "$tmp/dark/backend/testfiles/execution/" \
  src/Tests/e2e/upstream/
```

## 3) Reapply Local Delta
1. Reapply expected patch:
```bash
git apply --reject --whitespace=nowarn \
  --directory=src/Tests/e2e/upstream \
  scripts/upstream-execution-expected.patch
```
2. Check for rejects:
```bash
find src/Tests/e2e/upstream -name '*.rej' -print
```
3. Resolve each reject manually, preserving the local intent and aligning with current upstream file shape.
4. Delete resolved `.rej` files.

## 4) Update Test Runner Wiring
1. Update upstream include list in:
- `src/Tests/test-suite-tooling/TestRunner.fs`
2. Specifically verify:
- `upstreamDarkPaths`
- `defaultUpstreamDarkPaths`
- `upstreamEnablementLineAllowlist` line numbers and file paths
3. Keep enablement policy unchanged unless intentionally widened.

## 5) Rebuild Expected Patch Baseline
1. Inspect current drift:
```bash
python3 scripts/diff-upstream-execution-tests.py --ref main --ignore-expected-diff
```
2. Regenerate expected baseline to match intentional local deltas:
```bash
python3 scripts/diff-upstream-execution-tests.py --ref main --regenerate-expected-diff
```
3. Verify baseline matches:
```bash
python3 scripts/diff-upstream-execution-tests.py --ref main
```

## 6) Validate
1. Run full tests:
```bash
./run-tests --ai
```
2. If failures appear:
- fix runner/patch/test data first
- only fix compiler/runtime when failure proves real semantic mismatch

## 7) Deliverables Checklist
- [ ] `src/Tests/e2e/upstream/` synced to current upstream
- [ ] local intentional deltas replayed
- [ ] no remaining `*.rej` files
- [ ] `scripts/upstream-execution-expected.patch` regenerated
- [ ] `python3 scripts/diff-upstream-execution-tests.py --ref main` exits clean
- [ ] `./run-tests --ai` passes
