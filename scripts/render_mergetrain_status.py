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
            return f"{seconds // duration}{suffix}"
    return f"{seconds}s"


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


def running_train_step(repo: Path, jobs: list[dict[str, Any]]) -> str | None:
    running = next((job for job in jobs if job.get("state") == "running"), None)
    if running is None:
        return None
    completed = subprocess.run(
        [
            "mergetrain",
            "--repo",
            str(repo),
            "inspect",
            str(running["id"]),
            "--json",
        ],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    if completed.returncode != 0:
        return None
    try:
        inspection = json.loads(completed.stdout)
    except json.JSONDecodeError:
        return None
    if not isinstance(inspection, dict) or inspection.get("contract_version") != 4:
        return None
    progress = inspection.get("progress")
    if not isinstance(progress, dict):
        return None
    message = progress.get("message")
    if isinstance(message, str) and message:
        return message
    phase = progress.get("phase")
    return str(phase).replace("_", " ") if phase else None


def is_conflict_reason(reason: object) -> bool:
    return isinstance(reason, str) and "conflict" in reason.casefold()


def displayed_benchmark_ratio(contents: str) -> str | None:
    header_prefix = "| Benchmark | Dark ("
    for line in contents.splitlines():
        if line.startswith(header_prefix) and ") |" in line:
            return line[len(header_prefix) :].split(") |", 1)[0]
    return None


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


def percentage(value: float | None) -> str:
    if value is None:
        return "n/a"
    if value == 0:
        return "0%"
    if abs(value) < 0.01:
        return "~0%"
    return f"{value:+.2g}%"


def change_style(value: float | None) -> str:
    if value is None or value == 0:
        return DIM
    return GREEN if value < 0 else RED


@dataclass(frozen=True)
class BenchmarkWorkloadChange:
    name: str
    previous_instructions: int
    current_instructions: int
    previous_ratio: float
    current_ratio: float

    @property
    def saved_instructions(self) -> int:
        return self.previous_instructions - self.current_instructions

    @property
    def change(self) -> float:
        return (self.current_instructions / self.previous_instructions - 1) * 100


@dataclass(frozen=True)
class BenchmarkComparison:
    previous_ratio: str
    current_ratio: str
    aggregate_change: float
    total_benchmarks: int
    workload_changes: tuple[BenchmarkWorkloadChange, ...]


def benchmark_comparison(
    repo: Path, commit: str, *, current_contents: str | None = None
) -> BenchmarkComparison | str:
    current_contents = current_contents or git_file(repo, commit)
    previous_contents = git_file(repo, f"{commit}^")
    if current_contents is None or previous_contents is None:
        return "comparison unavailable"
    current_identity = benchmark_identity(current_contents)
    previous_identity = benchmark_identity(previous_contents)
    if current_identity and current_identity != previous_identity:
        return "not comparable with the previous result"
    current = benchmark_rows(current_contents)
    previous = benchmark_rows(previous_contents)
    if not current or current.keys() != previous.keys():
        return "comparison unavailable"

    current_dark = math.prod(dark for dark, _rust in current.values())
    current_rust = math.prod(rust for _dark, rust in current.values())
    previous_dark = math.prod(dark for dark, _rust in previous.values())
    previous_rust = math.prod(rust for _dark, rust in previous.values())
    exact_current = current_dark * previous_rust
    exact_previous = previous_dark * current_rust
    log_change = math.fsum(
        math.log(current[name][0] / current[name][1])
        - math.log(previous[name][0] / previous[name][1])
        for name in current
    ) / len(current)
    aggregate_change = (
        0.0
        if exact_current == exact_previous
        else (math.exp(log_change) - 1) * 100
    )
    workload_changes = tuple(
        BenchmarkWorkloadChange(
            name=name,
            previous_instructions=previous[name][0],
            current_instructions=current[name][0],
            previous_ratio=previous[name][0] / previous[name][1],
            current_ratio=current[name][0] / current[name][1],
        )
        for name in current
        if current[name][0] != previous[name][0]
    )
    return BenchmarkComparison(
        previous_ratio=displayed_benchmark_ratio(previous_contents) or "n/a",
        current_ratio=displayed_benchmark_ratio(current_contents) or "n/a",
        aggregate_change=aggregate_change,
        total_benchmarks=len(current),
        workload_changes=workload_changes,
    )


@dataclass(frozen=True)
class BenchmarkChange:
    commit: str
    short_commit: str
    date: str
    subject: str
    change: float | None
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
        comparison = benchmark_comparison(
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
            change=(
                comparison.aggregate_change
                if isinstance(comparison, BenchmarkComparison)
                else None
            ),
            ratio=ratio or "n/a",
        )

    return [parse(line) for line in history.splitlines()]


def benchmark_detail(repo: Path, commit: str, *, color: bool) -> str:
    comparison = benchmark_changes_for_commit(repo, commit)
    current_contents = git_file(repo, commit)
    short_commit = git(repo, "show", "-s", "--format=%h", commit)
    subject = git(repo, "show", "-s", "--format=%s", commit)
    lines = [
        styled("benchmark result:", BOLD, color),
        f"{styled(short_commit, CYAN, color)} {subject}",
    ]
    if current_contents is not None:
        metadata = [
            line.replace("**", "").replace("`", "")
            for line in current_contents.splitlines()
            if line.startswith(
                ("**Snapshot timestamp:**", "**Architecture:**", "**Profile:**")
            )
        ]
        if metadata:
            lines.append(styled(" · ".join(metadata), DIM, color))
    if isinstance(comparison, str):
        lines.extend(["", comparison])
        return "\n".join(lines)

    improved = tuple(
        change
        for change in comparison.workload_changes
        if change.saved_instructions > 0
    )
    disimproved = tuple(
        change
        for change in comparison.workload_changes
        if change.saved_instructions < 0
    )
    saved = sum(change.saved_instructions for change in improved)
    added = -sum(change.saved_instructions for change in disimproved)
    net = saved - added
    net_text = (
        f"{abs(net):,} {'saved' if net > 0 else 'added'}" if net else "even"
    )
    lines.extend(
        [
            "",
            "overall: "
            + styled(comparison.current_ratio, CYAN, color)
            + styled(" ← ", DIM, color)
            + styled(comparison.previous_ratio, DIM, color)
            + " "
            + styled(
                f"({percentage(comparison.aggregate_change)})",
                change_style(comparison.aggregate_change),
                color,
            ),
            f"changed: {len(comparison.workload_changes)}/{comparison.total_benchmarks}"
            f" · {len(improved)} improved · {len(disimproved)} disimproved",
            f"instructions: {saved:,} saved · {added:,} added · net {net_text}",
            "",
            styled("changed benchmarks:", BOLD, color),
        ]
    )
    if not comparison.workload_changes:
        lines.append("(none)")
        return "\n".join(lines)

    names_width = max(
        len("benchmark"),
        *(len(change.name) for change in comparison.workload_changes),
    )
    transitions = {
        change.name: (
            f"{change.previous_instructions:,} → {change.current_instructions:,}"
        )
        for change in comparison.workload_changes
    }
    impacts = {
        change.name: (
            f"{abs(change.saved_instructions):,} "
            f"{'saved' if change.saved_instructions > 0 else 'added'}"
        )
        for change in comparison.workload_changes
    }
    transition_width = max(
        len("instructions (before → after)"),
        *(len(value) for value in transitions.values()),
    )
    impact_width = max(len("impact"), *(len(value) for value in impacts.values()))
    lines.append(
        f"{'benchmark':<{names_width}}  "
        f"{'instructions (before → after)':<{transition_width}}  "
        f"{'impact':<{impact_width}}  {'change':>7}  ratio (before → after)"
    )
    for change in comparison.workload_changes:
        style = change_style(change.change)
        ratio_transition = (
            f"{change.previous_ratio:.3g}x → {change.current_ratio:.3g}x"
        )
        lines.append(
            styled(f"{change.name:<{names_width}}", BOLD, color)
            + "  "
            + styled(f"{transitions[change.name]:<{transition_width}}", style, color)
            + "  "
            + styled(f"{impacts[change.name]:<{impact_width}}", style, color)
            + "  "
            + styled(f"{percentage(change.change):>7}", style, color)
            + "  "
            + styled(ratio_transition, CYAN, color)
        )
    return "\n".join(lines)


def benchmark_changes_for_commit(
    repo: Path, commit: str
) -> BenchmarkComparison | str:
    return benchmark_comparison(repo, commit)


def benchmark_diff(repo: Path, commit: str, *, color: bool) -> str:
    short_commit = git(repo, "show", "-s", "--format=%h", commit)
    subject = git(repo, "show", "-s", "--format=%s", commit)
    patch = git(
        repo,
        "show",
        "--format=",
        "--first-parent",
        "--no-ext-diff",
        "--no-renames",
        commit,
        "--",
        ".",
        ":(exclude)benchmarks/RESULTS.md",
    )

    def diff_line(line: str) -> str:
        if line.startswith(("diff --git", "--- ", "+++ ")):
            return styled(line, BOLD, color)
        if line.startswith("@@"):
            return styled(line, CYAN, color)
        if line.startswith("+"):
            return styled(line, GREEN, color)
        if line.startswith("-"):
            return styled(line, RED, color)
        return line

    lines = [
        styled("benchmark commit diff:", BOLD, color),
        f"{styled(short_commit, CYAN, color)} {subject}",
        styled("benchmarks/RESULTS.md excluded (generated)", DIM, color),
        "",
    ]
    lines.extend(map(diff_line, patch.splitlines()))
    if not patch:
        lines.append("(no non-generated changes in this commit)")
    return "\n".join(lines)


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
    benchmark_diff_index: int | None = None,
) -> str:
    if payload.get("contract_version") != 4:
        raise ValueError(
            f"unsupported mergetrain contract version: {payload.get('contract_version')}"
        )

    benchmark_index = benchmark_detail_index or benchmark_diff_index
    if benchmark_index is not None:
        changes = benchmark_changes(repo)
        if benchmark_index < 1 or benchmark_index > len(changes):
            return "benchmark result unavailable"
        commit = changes[benchmark_index - 1].commit
        return (
            benchmark_diff(repo, commit, color=color)
            if benchmark_diff_index is not None
            else benchmark_detail(repo, commit, color=color)
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

    jobs = active_jobs(payload)
    step = running_train_step(repo, jobs)
    train_header = styled("in train:", BOLD, color)
    if step:
        train_header += f" {styled(step, CYAN, color)}"
    lines.append(train_header)
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
    lines.append("")
    lines.append(styled("recent benchmark results:", BOLD, color))
    ratio_width = max((len(change.ratio) for change in changes), default=0)
    changes_text = [percentage(change.change) for change in changes]
    change_width = max((len(value) + 2 for value in changes_text), default=0)
    ages = [human_age(change.date, now=now) for change in changes]
    age_width = max(map(len, ages), default=0)
    lines.extend(
        [
            f"{index}. "
            + styled(change.short_commit, CYAN, color)
            + " "
            + styled(
                f"{age:<{age_width}}",
                DIM,
                color,
            )
            + " "
            + styled(f"{change.ratio:>{ratio_width}}", CYAN, color)
            + " "
            + styled(
                f"({percentage(change.change)})".ljust(change_width),
                change_style(change.change),
                color,
            )
            + " "
            + change.subject
            for index, (change, age) in enumerate(
                zip(changes, ages, strict=True), start=1
            )
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
    parser.add_argument("--benchmark-diff-index", type=int)
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
                benchmark_diff_index=args.benchmark_diff_index,
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
