#!/usr/bin/env python3
"""prune-worktrees.py - Remove stale and already-integrated Git worktrees safely."""

from __future__ import annotations

import argparse
import os
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


@dataclass(frozen=True)
class PlanEntry:
    worktree: Worktree
    reason: str


@dataclass(frozen=True)
class Palette:
    enabled: bool

    def paint(self, code: str, text: str) -> str:
        return f"\033[{code}m{text}\033[0m" if self.enabled else text

    def action(self, name: str, text: str) -> str:
        codes = {
            "REMOVE": "1;31",
            "PRUNE": "1;35",
            "BLOCK": "1;33",
            "KEEP": "2",
        }
        return self.paint(codes[name], text)

    def success(self, text: str) -> str:
        return self.paint("1;32", text)

    def warning(self, text: str) -> str:
        return self.paint("1;33", text)

    def error(self, text: str) -> str:
        return self.paint("1;31", text)


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
        "--ignored=matching",
        check=False,
    )
    if result.returncode != 0:
        detail = result.stderr.strip() or "git status failed"
        raise EligibilityError(f"cannot inspect checkout {path}: {detail}")
    return bool(result.stdout)


def inspect_process_cwds(user_id: int) -> list[ProcessCwd]:
    if shutil.which("lsof") is None:
        raise EligibilityError("lsof is not available on PATH")

    try:
        result = subprocess.run(
            ["lsof", "-a", "-u", str(user_id), "-d", "cwd", "-F0pcn"],
            check=False,
            capture_output=True,
        )
    except OSError as error:
        raise EligibilityError(f"could not run lsof: {error}") from error
    stderr = result.stderr.decode(errors="replace").strip()
    if result.returncode != 0 or stderr:
        detail = stderr or f"lsof exited with status {result.returncode}"
        raise EligibilityError(f"UID {user_id}: {detail}")

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


def color_enabled(mode: str, stream: object) -> bool:
    if mode == "always":
        return True
    if mode == "never":
        return False
    return (
        "NO_COLOR" not in os.environ
        and os.environ.get("TERM") != "dumb"
        and bool(getattr(stream, "isatty")())
    )


def allocated_size(path: Path) -> int:
    total = 0
    seen: set[tuple[int, int]] = set()
    pending = [path]
    while pending:
        current = pending.pop()
        try:
            stat = current.stat(follow_symlinks=False)
            identity = (stat.st_dev, stat.st_ino)
            if identity in seen:
                continue
            seen.add(identity)
            total += stat.st_blocks * 512
            if current.is_dir() and not current.is_symlink():
                pending.extend(Path(entry.path) for entry in os.scandir(current))
        except OSError as error:
            raise EligibilityError(f"cannot measure checkout {path}: {error}") from error
    return total


def format_size(size: int) -> str:
    value = float(size)
    units = ("B", "KiB", "MiB", "GiB", "TiB")
    for unit in units:
        if value < 1024 or unit == units[-1]:
            precision = 0 if unit == "B" else 1
            return f"{value:.{precision}f} {unit}"
        value /= 1024
    raise AssertionError("unreachable size unit")


def print_plan(groups: dict[str, list[PlanEntry]], palette: Palette) -> None:
    summaries = {
        "REMOVE": "clean, inactive, integrated checkouts",
        "PRUNE": "missing checkouts",
    }
    first_group = True
    for action in ("REMOVE", "PRUNE", "BLOCK", "KEEP"):
        entries = groups[action]
        if not entries:
            continue
        if not first_group:
            print()
        first_group = False
        summary = f" — {summaries[action]}" if action in summaries else ""
        heading = f"{action} ({len(entries)}){summary}"
        print(palette.action(action, heading))
        for entry in sorted(entries, key=lambda item: str(item.worktree.path)):
            print(f"  {describe(entry.worktree)}")
            if action not in summaries:
                print(f"    {entry.reason}")


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
    result.add_argument(
        "--color",
        choices=("auto", "always", "never"),
        default="auto",
        help="colorize output (default: auto; also honors NO_COLOR)",
    )
    return result


def main() -> int:
    args = parser().parse_args()
    output_palette = Palette(color_enabled(args.color, sys.stdout))
    error_palette = Palette(color_enabled(args.color, sys.stderr))
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
    checkout_user_id = os.stat(current_root).st_uid
    if os.geteuid() != checkout_user_id:
        print(
            f"Run this script as checkout owner UID {checkout_user_id}, not with sudo; "
            "no changes made",
            file=sys.stderr,
        )
        return 1
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
        processes = inspect_process_cwds(checkout_user_id)
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
    plan: dict[str, list[PlanEntry]] = {
        "REMOVE": [],
        "PRUNE": [],
        "BLOCK": [],
        "KEEP": [],
    }

    for worktree in worktrees:
        resolved_path = worktree.path.resolve()
        if resolved_path == primary_path:
            plan["KEEP"].append(PlanEntry(worktree, "primary worktree"))
            skipped += 1
        elif resolved_path == current_root:
            plan["KEEP"].append(PlanEntry(worktree, "running worktree"))
            skipped += 1
        elif not is_ancestor(current_root, worktree.head, integration_commit):
            plan["KEEP"].append(
                PlanEntry(
                    worktree,
                    f"HEAD is not contained in {args.integration_ref}",
                )
            )
            skipped += 1
        elif worktree.prunable or not worktree.path.exists():
            if worktree.locked:
                plan["BLOCK"].append(
                    PlanEntry(worktree, "stale registration is locked")
                )
                blocked.append(worktree)
            else:
                plan["PRUNE"].append(PlanEntry(worktree, "checkout is missing"))
                stale.append(worktree)
        else:
            reasons: list[str] = []
            if worktree.locked:
                reasons.append("integrated checkout is locked")
            if is_dirty(worktree.path):
                reasons.append(
                    "integrated checkout has tracked, untracked, or ignored files"
                )
            if users := processes_using(worktree, processes):
                first = users[0]
                extra = f" and {len(users) - 1} more" if len(users) > 1 else ""
                reasons.append(f"used by PID {first.pid} ({first.command}){extra}")

            if reasons:
                plan["BLOCK"].append(PlanEntry(worktree, "; ".join(reasons)))
                blocked.append(worktree)
            else:
                plan["REMOVE"].append(
                    PlanEntry(
                        worktree,
                        f"HEAD is contained in {args.integration_ref}",
                    )
                )
                removable.append(worktree)

    if args.apply:
        try:
            final_processes = inspect_process_cwds(checkout_user_id)
        except EligibilityError as error:
            print(
                f"Cannot reverify worktree eligibility with lsof: {error}; "
                "no changes made",
                file=sys.stderr,
            )
            return 1

        still_removable: list[Worktree] = []
        for worktree in removable:
            if users := processes_using(worktree, final_processes):
                first = users[0]
                extra = f" and {len(users) - 1} more" if len(users) > 1 else ""
                plan["REMOVE"] = [
                    entry
                    for entry in plan["REMOVE"]
                    if entry.worktree != worktree
                ]
                plan["BLOCK"].append(
                    PlanEntry(
                        worktree,
                        f"became active before cleanup: used by PID {first.pid} "
                        f"({first.command}){extra}",
                    )
                )
                blocked.append(worktree)
            else:
                still_removable.append(worktree)
        removable = still_removable

    print_plan(plan, output_palette)

    planned_branches = {
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
        print(f"Dry run: {len(planned_branches)} merged local branch(es) deletable")
        return 0

    removed: list[Worktree] = []
    errors: list[str] = []
    reclaimed_bytes = 0
    for worktree in removable:
        try:
            checkout_size = allocated_size(worktree.path)
        except EligibilityError as error:
            errors.append(f"Could not remove {describe(worktree)}: {error}")
            continue
        remove_result = git(
            current_root,
            "worktree",
            "remove",
            "--",
            str(worktree.path),
            check=False,
        )
        if remove_result.returncode == 0:
            removed.append(worktree)
            reclaimed_bytes += checkout_size
        else:
            detail = remove_result.stderr.strip() or remove_result.stdout.strip()
            preserved = (
                "branch was preserved"
                if local_branch(worktree) is not None
                else "detached HEAD was preserved"
            )
            errors.append(
                f"Could not remove {describe(worktree)}; {preserved}: "
                f"{detail or f'git exited with status {remove_result.returncode}'}"
            )

    pruned: list[Worktree] = []
    if stale:
        prune_result = git(
            current_root,
            "worktree",
            "prune",
            "--expire",
            "now",
            check=False,
        )
        if prune_result.returncode == 0:
            pruned = stale
        else:
            detail = prune_result.stderr.strip() or prune_result.stdout.strip()
            errors.append(
                "Could not prune stale worktree registrations: "
                f"{detail or f'git exited with status {prune_result.returncode}'}"
            )

    branches_to_delete = {
        branch: worktree.head
        for worktree in [*removed, *pruned]
        if (branch := local_branch(worktree)) is not None
    }

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
            errors.append(
                f"Could not delete branch {branch}: "
                f"{detail or f'git exited with status {delete_result.returncode}'}"
            )
        else:
            deleted_branches += 1

    summary = (
        f"Applied: removed {len(removed)} checkout(s), pruned "
        f"{len(pruned)} stale registration(s), deleted {deleted_branches} branch(es), "
        f"left {len(blocked)} blocked worktree(s)"
    )
    print(output_palette.warning(summary) if errors else output_palette.success(summary))
    reclaimed = f"Reclaimed checkout space: {format_size(reclaimed_bytes)}"
    print(output_palette.success(reclaimed))
    if errors:
        print(file=sys.stderr)
        print(error_palette.error(f"ERROR ({len(errors)})"), file=sys.stderr)
        for error in errors:
            print(f"  {error}", file=sys.stderr)
        print("Other eligible cleanup continued", file=sys.stderr)
        return 1
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
