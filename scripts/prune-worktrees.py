#!/usr/bin/env python3
"""prune-worktrees.py - Remove stale and already-integrated Git worktrees safely."""

from __future__ import annotations

import argparse
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


def git(repo: Path, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["git", "-C", str(repo), *args],
        check=check,
        text=True,
        capture_output=True,
    )


def parse_worktrees(repo: Path) -> list[Worktree]:
    output = git(repo, "worktree", "list", "--porcelain", "-z").stdout
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
        raise RuntimeError("Git returned an unterminated worktree record")
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
        raise RuntimeError(result.stderr.strip() or "git merge-base failed")
    return result.returncode == 0


def is_dirty(path: Path) -> bool:
    return bool(git(path, "status", "--porcelain", "--untracked-files=normal").stdout)


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
        help="remove eligible checkouts and prune stale worktree metadata",
    )
    result.add_argument(
        "--delete-branches",
        action="store_true",
        help="also delete local branches after their eligible worktrees are removed",
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

    worktrees = parse_worktrees(current_root)
    if not worktrees:
        print("Git reported no worktrees", file=sys.stderr)
        return 1

    primary_path = worktrees[0].path.resolve()
    removable: list[Worktree] = []
    stale: list[Worktree] = []
    skipped = 0

    for worktree in worktrees:
        resolved_path = worktree.path.resolve()
        if resolved_path == primary_path:
            print(f"KEEP  {describe(worktree)}: primary worktree")
            skipped += 1
        elif resolved_path == current_root:
            print(f"KEEP  {describe(worktree)}: running worktree")
            skipped += 1
        elif worktree.locked:
            print(f"KEEP  {describe(worktree)}: locked")
            skipped += 1
        elif worktree.prunable or not worktree.path.exists():
            print(f"PRUNE {describe(worktree)}: checkout is missing")
            stale.append(worktree)
        elif is_dirty(worktree.path):
            print(f"KEEP  {describe(worktree)}: checkout has tracked or untracked changes")
            skipped += 1
        elif not is_ancestor(current_root, worktree.head, integration_commit):
            print(
                f"KEEP  {describe(worktree)}: "
                f"HEAD is not contained in {args.integration_ref}"
            )
            skipped += 1
        else:
            print(f"REMOVE {describe(worktree)}: HEAD is contained in {args.integration_ref}")
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
            f"{len(stale)} stale registration(s), {skipped} kept{suffix}"
        )
        if args.delete_branches:
            print(f"Dry run: {len(branches_to_delete)} merged local branch(es) deletable")
        return 0

    for worktree in removable:
        git(current_root, "worktree", "remove", "--", str(worktree.path))

    if stale:
        git(current_root, "worktree", "prune", "--expire", "now")

    deleted_branches = 0
    if args.delete_branches:
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
    raise SystemExit(main())
