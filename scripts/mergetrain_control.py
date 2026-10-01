#!/usr/bin/env python3
"""Integrate merge-train tooling ahead of ordinary jobs under the dispatch lock."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path


CONTROL_FILES = frozenset({
    ".mergetrain.yaml", "AGENTS.md", "AGENTS.mergetrain.md", "land",
    "mergetrain-status", "docs/contributing/mergetrain-integrator.md",
    "docs/contributing/verification.md",
    "benchmarks/check_sources_unchanged.py",
    "benchmarks/test_check_sources_unchanged.py",
    "benchmarks/run_benchmarks.sh",
    "benchmarks/infrastructure/deployed_baseline.py",
})
CONTROL_SCRIPTS = frozenset({
    "approve_attention_job.py", "mergetrain_control.py", "mergetrain_exception.py",
    "mergetrain_fifo.py", "mergetrain_recovery.py", "render_mergetrain_status.py",
    "run-mergetrain-integrator.sh", "test_approve_attention_job.py",
    "test_land.py", "test_mergetrain_control.py", "test_mergetrain_exception.py",
    "test_mergetrain_fifo.py", "test_mergetrain_integrator.py",
    "test_mergetrain_recovery.py", "test_mergetrain_status.py",
})
FOCUSED_TESTS = (
    "benchmarks.test_check_sources_unchanged",
    "scripts.test_land", "scripts.test_mergetrain_control", "scripts.test_mergetrain_fifo",
    "scripts.test_mergetrain_integrator", "scripts.test_mergetrain_recovery",
    "scripts.test_mergetrain_exception", "scripts.test_mergetrain_status",
    "scripts.test_approve_attention_job",
)
WORKTREE_PARENT = Path("/Users/paulbiggar/projects")


class ControlError(RuntimeError):
    pass


def run(repo: Path, *command: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(command, cwd=repo, capture_output=True, text=True, check=False)
    if check and result.returncode:
        raise ControlError(
            f"{' '.join(command)} failed: {(result.stderr or result.stdout).strip()[-1200:]}"
        )
    return result


def git(repo: Path, *args: str) -> str:
    return run(repo, "git", *args).stdout.strip()


def changed_paths(repo: Path, old: str, new: str) -> list[str]:
    output = run(repo, "git", "diff", "--name-only", "-z", old, new).stdout
    return [path for path in output.split("\0") if path]


def eligible_path(path: str) -> bool:
    return path in CONTROL_FILES or (
        path.startswith("scripts/") and path.removeprefix("scripts/") in CONTROL_SCRIPTS
    )


def classify(repo: Path, head: str) -> tuple[bool, list[str]]:
    base = git(repo, "merge-base", "main", head)
    paths = changed_paths(repo, base, head)
    if not paths:
        record = common_dir(repo) / "mergetrain-control" / f"{head}.json"
        try:
            payload = json.loads(record.read_text(encoding="utf-8"))
            if payload.get("source") == head and payload.get("status") in {"prepared", "landed"}:
                return True, []
        except (OSError, ValueError, AttributeError):
            pass
    return bool(paths) and all(map(eligible_path, paths)), paths


def common_dir(repo: Path) -> Path:
    return Path(git(repo, "rev-parse", "--path-format=absolute", "--git-common-dir"))


def audit(path: Path, values: dict[str, str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{os.getpid()}.tmp")
    temporary.write_text(json.dumps(values, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    temporary.replace(path)


def measured_code_unchanged(repo: Path, base: str, head: str) -> bool:
    """Prove an integration advance consists solely of recorded tooling landings."""
    if run(repo, "git", "merge-base", "--is-ancestor", base, head, check=False).returncode:
        return False
    receipts = {}
    for path in (common_dir(repo) / "mergetrain-control").glob("*.json"):
        record = json.loads(path.read_text(encoding="utf-8"))
        if record.get("status") in {"prepared", "landed"}:
            receipts[record.get("merged")] = record
    current = head
    while current != base:
        record = receipts.get(current)
        if record is None or not record.get("base"):
            return False
        previous = record["base"]
        if git(repo, "rev-parse", f"{current}^1") != previous:
            return False
        paths = changed_paths(repo, previous, current)
        # The benchmark runner can change the measurement itself. Its changes
        # require a fresh measurement even though they use the tooling path.
        if not all(eligible_path(path) and path != "benchmarks/run_benchmarks.sh" for path in paths):
            return False
        current = previous
    return True


def land_control(repo: Path, head: str, task: str) -> str:
    allowed, paths = classify(repo, head)
    if not allowed:
        raise ControlError(f"commit is not tooling-only: {', '.join(paths)}")
    if git(repo, "status", "--porcelain"):
        raise ControlError("tooling worktree must be clean")
    if git(repo, "rev-parse", "HEAD") != head:
        raise ControlError("tooling worktree HEAD changed")
    integration = git(repo, "rev-parse", "main")
    audit_path = common_dir(repo) / "mergetrain-control" / f"{head}.json"
    if run(repo, "git", "merge-base", "--is-ancestor", head, integration, check=False).returncode == 0:
        if not audit_path.exists():
            raise ControlError("tooling commit is integrated without a control receipt")
        record = json.loads(audit_path.read_text(encoding="utf-8"))
        if record.get("source") != head:
            raise ControlError("control receipt has a different source commit")
        audit(audit_path, {**record, "status": "landed"})
        return integration
    remote = git(repo, "ls-remote", "mergetrain-local", "refs/heads/main").split()
    if len(remote) != 2 or remote[0] != integration:
        raise ControlError("local main and integration destination differ; retry after refresh")
    with tempfile.TemporaryDirectory(prefix="c4d-mergetrain-control-",
                                     dir=WORKTREE_PARENT) as directory:
        worktree = Path(directory)
        run(repo, "git", "worktree", "add", "--detach", str(worktree), integration)
        try:
            run(worktree, "git", "merge", "--no-ff", "--no-edit", head)
            merged = git(worktree, "rev-parse", "HEAD")
            merged_paths = changed_paths(worktree, integration, merged)
            if not merged_paths or not all(map(eligible_path, merged_paths)):
                raise ControlError("merged tree contains changes outside merge-train tooling")
            run(worktree, "git", "diff", "--check", f"{integration}..{merged}")
            if ".mergetrain.yaml" in merged_paths:
                run(worktree, "mergetrain", "--repo", str(worktree), "status", "--json")
            run(worktree, "bash", "-n", "land", "mergetrain-status",
                "scripts/run-mergetrain-integrator.sh")
            test = run(worktree, "python3", "-m", "unittest", *FOCUSED_TESTS, check=False)
            log_path = common_dir(repo) / "mergetrain-control" / f"{head}.tests.log"
            log_path.parent.mkdir(parents=True, exist_ok=True)
            log_path.write_text(test.stdout + test.stderr, encoding="utf-8")
            if test.returncode:
                raise ControlError(f"tooling tests failed; log: {log_path}")
            record = {
                "schema": "1", "task": task, "source": head, "base": integration,
                "merged": merged, "status": "prepared", "test_log": str(log_path),
            }
            audit(audit_path, record)
            run(worktree, "git", "push", "--atomic",
                f"--force-with-lease=refs/heads/main:{integration}",
                "mergetrain-local", "HEAD:refs/heads/main")
            audit(audit_path, {**record, "status": "landed"})
            return merged
        finally:
            run(repo, "git", "worktree", "remove", "--force", str(worktree), check=False)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", type=Path, required=True)
    sub = parser.add_subparsers(dest="action", required=True)
    classify_command = sub.add_parser("classify")
    classify_command.add_argument("head")
    integrate = sub.add_parser("land")
    integrate.add_argument("head")
    integrate.add_argument("--task", required=True)
    args = parser.parse_args()
    try:
        repo = args.repo.resolve()
        if args.action == "classify":
            allowed, _paths = classify(repo, args.head)
            print("control" if allowed else "ordinary")
        else:
            print(land_control(repo, args.head, args.task))
    except (ControlError, OSError, ValueError) as error:
        print(f"Tooling landing: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
