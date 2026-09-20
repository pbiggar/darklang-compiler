#!/usr/bin/env python3
"""Render merge-train state with repository integration and benchmark history."""

from __future__ import annotations

import argparse
import json
import math
import subprocess
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


RESET = "\033[0m"
BOLD = "\033[1m"
DIM = "\033[2m"
RED = "\033[31m"
GREEN = "\033[32m"
YELLOW = "\033[33m"
CYAN = "\033[36m"


def styled(value: object, style: str, color: bool) -> str:
    text = str(value)
    return f"{style}{text}{RESET}" if color else text


def state_style(state: str) -> str:
    return {
        "attention": RED,
        "running": CYAN,
        "ready": GREEN,
        "waiting": YELLOW,
        "idle": DIM,
    }.get(state, RED)


def human_age(timestamp: str | datetime, *, now: datetime | None = None) -> str:
    instant = datetime.fromisoformat(timestamp) if isinstance(timestamp, str) else timestamp
    reference = now or datetime.now(timezone.utc)
    seconds = max(0, int((reference - instant).total_seconds()))
    units = (
        (365 * 24 * 60 * 60, "y"),
        (30 * 24 * 60 * 60, "mo"),
        (7 * 24 * 60 * 60, "w"),
        (24 * 60 * 60, "d"),
        (60 * 60, "h"),
        (60, "m"),
    )
    for duration, suffix in units:
        if seconds >= duration:
            return f"{seconds // duration}{suffix} ago"
    return f"{seconds}s ago"


def history_line(
    line: str,
    color: bool,
    *,
    now: datetime,
) -> str:
    parts = line.split(" ", 2)
    if len(parts) < 3:
        return line
    commit, timestamp, description = parts
    return (
        f"{styled(commit, CYAN, color)} "
        f"{styled(human_age(timestamp, now=now), DIM, color)} {description}"
    )


def git(repo: Path, *args: str) -> str:
    completed = subprocess.run(
        ["git", "-C", str(repo), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return completed.stdout.rstrip("\n")


def active_jobs(payload: dict[str, Any]) -> list[dict[str, Any]]:
    by_id = {
        int(job["id"]): job
        for job in [*payload.get("recent_jobs", []), *payload.get("attention_jobs", [])]
        if job.get("state") in {"waiting", "running", "ready", "attention"}
    }
    return [by_id[job_id] for job_id in sorted(by_id)]


def is_conflict_reason(reason: object) -> bool:
    return isinstance(reason, str) and "conflict" in reason.casefold()


def displayed_benchmark_ratio(contents: str) -> str | None:
    header_prefix = "| Benchmark | Dark ("
    for line in contents.splitlines():
        if line.startswith(header_prefix) and ") |" in line:
            return line[len(header_prefix) :].split(") |", 1)[0]
    return None


def benchmark_ratio(repo: Path) -> str | None:
    results_path = repo / "benchmarks" / "RESULTS.md"
    return (
        displayed_benchmark_ratio(results_path.read_text(encoding="utf-8"))
        if results_path.is_file()
        else None
    )


def benchmark_rows(contents: str) -> dict[str, tuple[int, int]]:
    rows: dict[str, tuple[int, int]] = {}
    for line in contents.splitlines():
        if not line.startswith("|"):
            continue
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if len(cells) < 3 or cells[0] in {"Benchmark", "---"}:
            continue
        dark_text = cells[1].split(" ", 1)[0].replace(",", "")
        rust_text = cells[2].replace(",", "")
        try:
            dark = int(dark_text)
            rust = int(rust_text)
        except ValueError:
            continue
        if dark > 0 and rust > 0:
            rows[cells[0]] = (dark, rust)
    return rows


def benchmark_identity(contents: str) -> tuple[str, ...]:
    prefixes = (
        "**Architecture:**",
        "**Profile:**",
        "**Measurement policy:**",
        "**Workload contract:**",
    )
    return tuple(line for line in contents.splitlines() if line.startswith(prefixes))


def git_file(repo: Path, revision: str) -> str | None:
    completed = subprocess.run(
        ["git", "-C", str(repo), "show", f"{revision}:benchmarks/RESULTS.md"],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    return completed.stdout if completed.returncode == 0 else None


def percentage(value: float) -> str:
    return f"{value:.2g}%"


def benchmark_improvement(
    repo: Path, commit: str, *, current_contents: str | None = None
) -> str:
    current_contents = current_contents or git_file(repo, commit)
    previous_contents = git_file(repo, f"{commit}^")
    if current_contents is None:
        return "n/a"
    if previous_contents is None:
        return "n/a"
    current_identity = benchmark_identity(current_contents)
    previous_identity = benchmark_identity(previous_contents)
    if current_identity and current_identity != previous_identity:
        return "n/a"
    current = benchmark_rows(current_contents)
    previous = benchmark_rows(previous_contents)
    if not current or current.keys() != previous.keys():
        return "n/a"

    current_dark = math.prod(dark for dark, _rust in current.values())
    current_rust = math.prod(rust for _dark, rust in current.values())
    previous_dark = math.prod(dark for dark, _rust in previous.values())
    previous_rust = math.prod(rust for _dark, rust in previous.values())
    exact_current = current_dark * previous_rust
    exact_previous = previous_dark * current_rust
    if exact_current == exact_previous:
        return "0%"
    log_change = math.fsum(
        math.log(current[name][0] / current[name][1])
        - math.log(previous[name][0] / previous[name][1])
        for name in current
    ) / len(current)
    improvement = (1 - math.exp(log_change)) * 100
    return percentage(improvement)


@dataclass(frozen=True)
class BenchmarkChange:
    commit: str
    short_commit: str
    date: str
    subject: str
    improvement: str
    ratio: str


def benchmark_changes(repo: Path, limit: int = 10) -> list[BenchmarkChange]:
    history = git(
        repo,
        "log",
        f"-{limit}",
        "--format=%H%x09%h%x09%cI%x09%s",
        "--",
        "benchmarks/RESULTS.md",
    )
    if not history:
        return []

    def parse(line: str) -> BenchmarkChange:
        commit, short_commit, date, subject = line.split("\t", 3)
        current_contents = git_file(repo, commit)
        improvement = benchmark_improvement(
            repo, commit, current_contents=current_contents
        )
        ratio = (
            displayed_benchmark_ratio(current_contents)
            if current_contents is not None
            else None
        )
        return BenchmarkChange(
            commit=commit,
            short_commit=short_commit,
            date=date,
            subject=subject,
            improvement=improvement,
            ratio=ratio or "n/a",
        )

    return [parse(line) for line in history.splitlines()]


def benchmark_detail(repo: Path, commit: str, *, color: bool) -> str:
    changes = benchmark_changes_for_commit(repo, commit)
    current_contents = git_file(repo, commit)
    ratio = (
        displayed_benchmark_ratio(current_contents)
        if current_contents is not None
        else None
    )
    short_commit = git(repo, "show", "-s", "--format=%h", commit)
    subject = git(repo, "show", "-s", "--format=%s", commit)
    lines = [
        styled("benchmark result:", BOLD, color),
        f"{styled(short_commit, CYAN, color)} {subject}",
        f"ratio: {styled(ratio or 'unavailable', CYAN, color)}",
        styled("improved benchmarks:", BOLD, color),
    ]
    if isinstance(changes, str):
        lines.append(changes)
    else:
        lines.extend(
            f"{name} {percentage(improvement)} {instructions:,} instructions"
            for name, improvement, instructions in changes
        )
        if not changes:
            lines.append("(none)")
    return "\n".join(lines)


def benchmark_changes_for_commit(
    repo: Path, commit: str
) -> list[tuple[str, float, int]] | str:
    current_contents = git_file(repo, commit)
    previous_contents = git_file(repo, f"{commit}^")
    if current_contents is None or previous_contents is None:
        return "individual improvements unavailable"
    current_identity = benchmark_identity(current_contents)
    previous_identity = benchmark_identity(previous_contents)
    if current_identity and current_identity != previous_identity:
        return "not comparable with the previous result"
    current = benchmark_rows(current_contents)
    previous = benchmark_rows(previous_contents)
    if not current or current.keys() != previous.keys():
        return "individual improvements unavailable"
    return [
        (name, (1 - current[name][0] / previous[name][0]) * 100, current[name][0])
        for name in current
        if current[name][0] < previous[name][0]
    ]


def merged_branch(repo: Path, commit: str) -> str:
    refs = git(
        repo,
        "for-each-ref",
        "--points-at",
        commit,
        "--format=%(refname)",
        "refs/heads",
        "refs/remotes",
    ).splitlines()
    local = sorted(
        ref.removeprefix("refs/heads/")
        for ref in refs
        if ref.startswith("refs/heads/") and ref != "refs/heads/main"
    )
    if local:
        return local[0]
    remote = sorted(
        ref.removeprefix("refs/remotes/").split("/", 1)[-1]
        for ref in refs
        if ref.startswith("refs/remotes/")
        and not ref.endswith("/main")
        and not ref.endswith("/HEAD")
    )
    return remote[0] if remote else commit[:10]


def recent_merges(repo: Path, limit: int = 5) -> list[str]:
    history = git(
        repo,
        "log",
        f"-{limit}",
        "--first-parent",
        "--merges",
        "--format=%h%x09%cI%x09%P",
        "--",
    )
    if not history:
        return []

    def render(line: str) -> str:
        merge, timestamp, parents_text = line.split("\t", 2)
        parents = parents_text.split()
        merged_commit = parents[1]
        branch = merged_branch(repo, merged_commit)
        subject = git(repo, "show", "-s", "--format=%s", merged_commit)
        return f"{merge} {timestamp} {branch} — {subject}"

    return [render(line) for line in history.splitlines()]


def render(
    payload: dict[str, Any],
    repo: Path,
    *,
    color: bool,
    show_conflicts: bool = False,
    conflict_toggle_hint: bool = False,
    merge_limit: int = 5,
    benchmark_detail_index: int | None = None,
) -> str:
    if payload.get("contract_version") != 4:
        raise ValueError(
            f"unsupported mergetrain contract version: {payload.get('contract_version')}"
        )

    if benchmark_detail_index is not None:
        changes = benchmark_changes(repo)
        if benchmark_detail_index < 1 or benchmark_detail_index > len(changes):
            return "benchmark result unavailable"
        return benchmark_detail(
            repo, changes[benchmark_detail_index - 1].commit, color=color
        )

    action = payload["next_action"]
    next_action = action.get("command") or str(action["code"]).replace("_", " ")
    health = str(payload["health"])
    health_style = GREEN if health == "healthy" else YELLOW
    state = str(payload["state"])
    lines = [
        f"health: {styled(health, health_style, color)}",
        f"{styled(state.upper(), state_style(state), color)}: {payload['summary']}",
        f"next: {styled(next_action, CYAN, color)}",
    ]
    if action.get("requires_approval") != "none":
        lines.append(f"approval: {action['requires_approval']}")
    lines.extend(
        styled(f"warning {warning['code']}: {warning['summary']}", YELLOW, color)
        for warning in payload.get("warnings", [])
    )

    lines.append(styled("in train:", BOLD, color))
    jobs = active_jobs(payload)
    has_conflict = False
    if jobs:
        for job in jobs:
            job_state = str(job["state"])
            line = (
                f"  #{job['id']} {styled(job_state, state_style(job_state), color)} "
                f"{job['task']} [{job['branch']}]"
            )
            reason = job.get("reason")
            if reason:
                conflict_reason = is_conflict_reason(reason)
                has_conflict = has_conflict or conflict_reason
                if conflict_reason and not show_conflicts:
                    reason = "conflict"
                line += f" — {reason}"
            lines.append(line)
    else:
        lines.append("  (empty)")
    if conflict_toggle_hint and has_conflict:
        action = "hide" if show_conflicts else "show full"
        lines.append(styled(f"  [c] {action} conflict details", DIM, color))

    now = datetime.now(timezone.utc)
    merges = recent_merges(repo, limit=merge_limit)
    lines.append("")
    lines.append(styled("recent merges:", BOLD, color))
    lines.extend(
        [history_line(merge, color, now=now) for merge in merges] or ["(none)"]
    )

    changes = benchmark_changes(repo)
    ratio = benchmark_ratio(repo)
    lines.append("")
    lines.append(f"benchmark ratio: {styled(ratio or 'unavailable', CYAN, color)}")
    lines.append(styled("recent benchmark results:", BOLD, color))
    lines.extend(
        [
            f"[{index if index < 10 else 0}] "
            + history_line(
                f"{change.short_commit} {change.date} "
                f"{change.improvement} {change.ratio} {change.subject}",
                color,
                now=now,
            )
            for index, change in enumerate(changes, start=1)
        ]
        or ["(none)"]
    )
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", required=True, type=Path)
    parser.add_argument("--color", action="store_true")
    parser.add_argument("--show-conflicts", action="store_true")
    parser.add_argument("--conflict-toggle-hint", action="store_true")
    parser.add_argument("--merge-limit", type=int, default=5)
    parser.add_argument("--benchmark-detail-index", type=int)
    args = parser.parse_args()
    try:
        payload = json.load(sys.stdin)
        print(
            render(
                payload,
                args.repo.resolve(),
                color=args.color,
                show_conflicts=args.show_conflicts,
                conflict_toggle_hint=args.conflict_toggle_hint,
                merge_limit=args.merge_limit,
                benchmark_detail_index=args.benchmark_detail_index,
            )
        )
    except (
        json.JSONDecodeError,
        KeyError,
        OSError,
        subprocess.CalledProcessError,
        ValueError,
    ) as exc:
        print(f"Unable to render merge-train status: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
