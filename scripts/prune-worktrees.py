#!/usr/bin/env python3
"""prune-worktrees.py - Remove stale and already-integrated Git worktrees safely."""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class Worktree:
    path: Path
    head: str
    branch: str | None
    locked: bool
    prunable: bool


@dataclass(frozen=True)
class ProcessCwd:
    pid: str
    command: str
    path: Path


class EligibilityError(Exception):
    pass


def git(repo: Path, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["git", "-C", str(repo), *args],
        check=check,
        text=True,
        capture_output=True,
    )


def parse_worktrees(repo: Path) -> list[Worktree]:
    result = git(repo, "worktree", "list", "--porcelain", "-z", check=False)
    if result.returncode != 0:
        detail = result.stderr.strip() or "git worktree list failed"
        raise EligibilityError(f"cannot inspect registered worktrees: {detail}")
    output = result.stdout
    records: list[Worktree] = []
    fields: dict[str, str] = {}

    for item in output.split("\0"):
        if not item:
            if fields:
                records.append(
                    Worktree(
                        path=Path(fields["worktree"]),
                        head=fields.get("HEAD", ""),
                        branch=fields.get("branch"),
                        locked="locked" in fields,
                        prunable="prunable" in fields,
                    )
                )
                fields = {}
            continue

        key, separator, value = item.partition(" ")
        fields[key] = value if separator else ""

    if fields:
        raise EligibilityError("Git returned an unterminated worktree record")
    return records


def local_branch(worktree: Worktree) -> str | None:
    prefix = "refs/heads/"
    if worktree.branch and worktree.branch.startswith(prefix):
        return worktree.branch.removeprefix(prefix)
    return None


def is_ancestor(repo: Path, commit: str, integration_commit: str) -> bool:
    result = git(
        repo,
        "merge-base",
        "--is-ancestor",
        commit,
        integration_commit,
        check=False,
    )
    if result.returncode not in (0, 1):
        detail = result.stderr.strip() or "git merge-base failed"
        raise EligibilityError(f"cannot check whether {commit} is integrated: {detail}")
    return result.returncode == 0


def is_dirty(path: Path) -> bool:
    result = git(
        path,
        "status",
        "--porcelain",
        "--untracked-files=normal",
        check=False,
    )
    if result.returncode != 0:
        detail = result.stderr.strip() or "git status failed"
        raise EligibilityError(f"cannot inspect checkout {path}: {detail}")
    return bool(result.stdout)


def inspect_process_cwds() -> list[ProcessCwd]:
    if shutil.which("lsof") is None:
        raise EligibilityError("lsof is not available on PATH")

    try:
        result = subprocess.run(
            ["lsof", "-a", "-d", "cwd", "-F0pcn"],
            check=False,
            capture_output=True,
        )
    except OSError as error:
        raise EligibilityError(f"could not run lsof: {error}") from error
    stderr = result.stderr.decode(errors="replace").strip()
    if result.returncode != 0 or stderr:
        detail = stderr or f"lsof exited with status {result.returncode}"
        raise EligibilityError(detail)

    processes: list[ProcessCwd] = []
    pid = ""
    command = ""
    inspection_errors: list[str] = []
    for raw_field in result.stdout.split(b"\0"):
        field = raw_field.removeprefix(b"\n").decode(errors="surrogateescape")
        if not field:
            continue
        tag, value = field[0], field[1:]
        if tag == "p":
            pid = value
            command = ""
        elif tag == "c":
            command = value
        elif tag == "n":
            if "(readlink:" in value:
                inspection_errors.append(f"PID {pid or '?'}: {value}")
            elif not pid or not value:
                inspection_errors.append("lsof returned an incomplete cwd record")
            elif not Path(value).is_absolute():
                inspection_errors.append(
                    f"PID {pid}: lsof returned a non-absolute cwd: {value}"
                )
            else:
                processes.append(
                    ProcessCwd(
                        pid=pid,
                        command=command or "unknown",
                        path=Path(value).resolve(),
                    )
                )

    if inspection_errors:
        raise EligibilityError("; ".join(inspection_errors[:3]))
    if not processes:
        raise EligibilityError("lsof returned no process working directories")
    return processes


def processes_using(worktree: Worktree, processes: list[ProcessCwd]) -> list[ProcessCwd]:
    root = worktree.path.resolve()
    matches: list[ProcessCwd] = []
    for process in processes:
        try:
            process.path.relative_to(root)
            matches.append(process)
        except ValueError:
            pass
    return matches


def describe(worktree: Worktree) -> str:
    branch = local_branch(worktree)
    label = branch if branch else f"detached at {worktree.head[:12]}"
    return f"{worktree.path} ({label})"


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(
        description=(
            "Find secondary Git worktrees whose HEAD is already contained in the "
            "integration ref. By default, print the cleanup plan without changing anything."
        )
    )
    result.add_argument(
        "--apply",
        action="store_true",
        help=(
            "remove eligible checkouts, prune stale metadata, and delete their "
            "merged local branches"
        ),
    )
    result.add_argument(
        "--integration-ref",
        default="origin/main",
        metavar="REF",
        help="ref that must contain a worktree HEAD (default: origin/main)",
    )
    return result


def main() -> int:
    args = parser().parse_args()
    current_root_result = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        check=False,
        text=True,
        capture_output=True,
    )
    if current_root_result.returncode != 0:
        print("Run this script from inside a Git worktree", file=sys.stderr)
        return 1

    current_root = Path(current_root_result.stdout.strip()).resolve()
    integration_result = git(
        current_root,
        "rev-parse",
        "--verify",
        f"{args.integration_ref}^{{commit}}",
        check=False,
    )
    if integration_result.returncode != 0:
        print(f"Integration ref is not a commit: {args.integration_ref}", file=sys.stderr)
        return 1
    integration_commit = integration_result.stdout.strip()

    try:
        processes = inspect_process_cwds()
    except EligibilityError as error:
        print(
            f"Cannot verify worktree eligibility with lsof: {error}; no changes made",
            file=sys.stderr,
        )
        return 1

    worktrees = parse_worktrees(current_root)
    if not worktrees:
        print("Git reported no worktrees", file=sys.stderr)
        return 1

    primary_path = worktrees[0].path.resolve()
    removable: list[Worktree] = []
    stale: list[Worktree] = []
    blocked: list[Worktree] = []
    skipped = 0

    for worktree in worktrees:
        resolved_path = worktree.path.resolve()
        if resolved_path == primary_path:
            print(f"KEEP  {describe(worktree)}: primary worktree")
            skipped += 1
        elif resolved_path == current_root:
            print(f"KEEP  {describe(worktree)}: running worktree")
            skipped += 1
        elif not is_ancestor(current_root, worktree.head, integration_commit):
            print(
                f"KEEP  {describe(worktree)}: "
                f"HEAD is not contained in {args.integration_ref}"
            )
            skipped += 1
        elif worktree.prunable or not worktree.path.exists():
            if worktree.locked:
                print(f"BLOCK {describe(worktree)}: stale registration is locked")
                blocked.append(worktree)
            else:
                print(f"PRUNE {describe(worktree)}: checkout is missing")
                stale.append(worktree)
        else:
            reasons: list[str] = []
            if worktree.locked:
                reasons.append("integrated checkout is locked")
            if is_dirty(worktree.path):
                reasons.append("integrated checkout has tracked or untracked changes")
            if users := processes_using(worktree, processes):
                first = users[0]
                extra = f" and {len(users) - 1} more" if len(users) > 1 else ""
                reasons.append(f"used by PID {first.pid} ({first.command}){extra}")

            if reasons:
                print(f"BLOCK {describe(worktree)}: {'; '.join(reasons)}")
                blocked.append(worktree)
            else:
                print(
                    f"REMOVE {describe(worktree)}: "
                    f"HEAD is contained in {args.integration_ref}"
                )
                removable.append(worktree)

    branches_to_delete = {
        branch: worktree.head
        for worktree in [*removable, *stale]
        if (branch := local_branch(worktree)) is not None
        and is_ancestor(current_root, worktree.head, integration_commit)
    }

    if not args.apply:
        suffix = "; rerun with --apply to perform it"
        print(
            f"Dry run: {len(removable)} checkout(s) removable, "
            f"{len(stale)} stale registration(s), {len(blocked)} blocked, "
            f"{skipped} kept{suffix}"
        )
        print(f"Dry run: {len(branches_to_delete)} merged local branch(es) deletable")
        return 0

    if blocked:
        print(
            f"Refusing to apply: {len(blocked)} integrated worktree(s) failed "
            "eligibility checks; no changes made",
            file=sys.stderr,
        )
        return 1

    try:
        final_processes = inspect_process_cwds()
    except EligibilityError as error:
        print(
            f"Cannot reverify worktree eligibility with lsof: {error}; no changes made",
            file=sys.stderr,
        )
        return 1
    newly_used = [
        (worktree, users)
        for worktree in removable
        if (users := processes_using(worktree, final_processes))
    ]
    if newly_used:
        for worktree, users in newly_used:
            first = users[0]
            print(
                f"Worktree became active before cleanup: {describe(worktree)} is used "
                f"by PID {first.pid} ({first.command})",
                file=sys.stderr,
            )
        print("Refusing to apply; no changes made", file=sys.stderr)
        return 1

    for worktree in removable:
        git(current_root, "worktree", "remove", "--", str(worktree.path))

    if stale:
        git(current_root, "worktree", "prune", "--expire", "now")

    deleted_branches = 0
    for branch, expected_head in sorted(branches_to_delete.items()):
        delete_result = git(
            current_root,
            "update-ref",
            "-d",
            f"refs/heads/{branch}",
            expected_head,
            check=False,
        )
        if delete_result.returncode != 0:
            detail = delete_result.stderr.strip() or delete_result.stdout.strip()
            print(f"Could not delete branch {branch}: {detail}", file=sys.stderr)
            return 1
        deleted_branches += 1

    print(
        f"Applied: removed {len(removable)} checkout(s), pruned "
        f"{len(stale)} stale registration(s), deleted {deleted_branches} branch(es)"
    )
    return 0


if __name__ == "__main__":
    try:
        exit_code = main()
    except EligibilityError as error:
        print(
            f"Cannot verify worktree eligibility: {error}; no changes made",
            file=sys.stderr,
        )
        exit_code = 1
    raise SystemExit(exit_code)
