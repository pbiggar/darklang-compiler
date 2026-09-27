#!/usr/bin/env python3
"""Render merge-train state with repository integration and benchmark history."""

from __future__ import annotations

import argparse
import json
import math
import re
import subprocess
import sys
import unicodedata
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


def render_attention_job(
    repo: Path, job_id: int, *, color: bool, attempt_dir: Path
) -> str:
    completed = subprocess.run(
        ["mergetrain", "--repo", str(repo), "inspect", str(job_id), "--json"],
        check=False,
        capture_output=True,
        text=True,
    )
    if completed.returncode != 0:
        return f"attention job #{job_id}: inspection failed\n{completed.stderr.strip()}"
    details = json.loads(completed.stdout)
    job = details.get("job") or {}
    outcome = details.get("outcome") or {}
    progress = details.get("progress") or {}
    policy_change = "approval_execution_policy_changed" in str(outcome.get("message") or "")
    manual_policy_job = (
        job.get("status") in {"queued", "validated"}
        and job.get("auto_deploy") is False
        and policy_change
    )
    page_kind = "APPROVAL" if manual_policy_job else "ATTENTION"
    lines = [
        styled(f"{page_kind} — job #{job_id}: {job.get('task') or '(unknown task)'}", BOLD, color),
        f"task: {job.get('task') or '(unknown)'}",
        f"branch: {job.get('branch') or '(unknown)'}",
        f"commit: {job.get('head_sha') or '(unknown)'}",
        f"state: {job.get('status') or '(unknown)'}",
        f"failure: {outcome.get('failure_category') or '(unknown)'}",
        f"reason: {outcome.get('message') or job.get('note') or '(none recorded)'}",
    ]
    gate = progress.get("gate")
    if gate:
        lines.append(f"gate: {gate}")
    for event in details.get("events") or []:
        if event.get("state") in {"failure", "failed", "error"}:
            lines.append(f"event: {event.get('message') or '(unnamed)'}")
            if event.get("detail"):
                lines.append(f"detail: {event['detail']}")
    if job.get("log_path"):
        lines.append(f"full log: {job['log_path']}")

    if policy_change:
        base = str(job.get("base_sha") or "")
        head = str(job.get("head_sha") or "")
        lines.append("")
        lines.append("execution policy evidence:")
        if manual_policy_job:
            lines.append("This manual job awaits validation and exact-plan confirmation.")
        else:
            lines.append("Retry after a policy change creates a manual job; it does not renew --auto approval.")
        evidence = attempt_dir / f"{job_id}-{head}.policy.json"
        if evidence.is_file():
            recorded = json.loads(evidence.read_text(encoding="utf-8"))
            lines.append(f"recorded evidence: {evidence}")
            lines.append(
                "job changes .mergetrain.yaml: "
                + ("yes" if recorded.get("job_policy_diff") else "no")
            )
            lines.append("integrated policy change since enqueue:")
            lines.append(recorded.get("integration_policy_diff") or "(none recorded)")
            if recorded.get("job_policy_diff"):
                lines.append("policy changes in this job:")
                lines.append(recorded["job_policy_diff"])
            if recorded.get("policy_validation_result"):
                lines.append(f"current policy validation: {recorded['policy_validation_result']}")
            if recorded.get("policy_validation_failure"):
                lines.append(f"failed policy gate: {recorded['policy_validation_failure']}")
            if recorded.get("policy_validation_log"):
                lines.append(f"policy validation log: {recorded['policy_validation_log']}")
        elif base and head:
            changed_in_job = subprocess.run(
                ["git", "-C", str(repo), "diff", "--no-ext-diff", "--name-only",
                 f"{base}..{head}", "--", ".mergetrain.yaml"],
                check=False, capture_output=True, text=True,
            )
            job_policy_change = (
                "unavailable" if changed_in_job.returncode != 0
                else "yes" if changed_in_job.stdout.strip() else "no"
            )
            lines.append(f"job changes .mergetrain.yaml: {job_policy_change}")
            if job_policy_change == "yes":
                job_difference = subprocess.run(
                    ["git", "-C", str(repo), "diff", "--no-ext-diff", "--unified=3",
                     f"{base}..{head}", "--", ".mergetrain.yaml"],
                    check=False, capture_output=True, text=True,
                )
                if job_difference.returncode == 0:
                    lines.extend(("policy changes in this job:", job_difference.stdout.rstrip()))
            lines.append("control checkout policy change since enqueue:")
            difference = subprocess.run(
                ["git", "-C", str(repo), "diff", "--no-ext-diff", "--unified=3",
                 base, "--", ".mergetrain.yaml"],
                check=False, capture_output=True, text=True,
            )
            lines.append(
                difference.stdout.rstrip()
                if difference.returncode == 0 and difference.stdout.strip()
                else "(no configuration diff available)"
            )
        else:
            lines.append("(job base or commit identity unavailable)")
    actions = "" if manual_policy_job else "[r] retry this job"
    if policy_change and (manual_policy_job or outcome.get("failure_category") == "deploy_authorization_changed"):
        actions = "[A] approve and deploy  " + actions
    lines.extend(("", f"{actions}  [n/p] next/previous review page  [q/Esc] back"))
    return "\n".join(lines)


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


def history_items(repo: Path) -> list[dict[str, Any]]:
    completed = subprocess.run(
        ["mergetrain", "--repo", str(repo), "history", "--json", "--limit", "50"],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    if completed.returncode != 0:
        return []
    try:
        history = json.loads(completed.stdout)
    except json.JSONDecodeError:
        return []
    if not isinstance(history, dict) or not isinstance(history.get("items"), list):
        return []
    return [item for item in history["items"] if isinstance(item, dict)]


def last_test_runtime(
    repo: Path, *, history: list[dict[str, Any]] | None = None
) -> tuple[float, str] | None:
    items = history if history is not None else history_items(repo)
    gates = [
        gate
        for item in items
        if isinstance(item, dict) and isinstance(item.get("gates"), list)
        for gate in item["gates"]
        if isinstance(gate, dict)
        and gate.get("name") == "tests"
        and gate.get("state") == "success"
        and isinstance(gate.get("duration_seconds"), (int, float))
        and math.isfinite(gate["duration_seconds"])
        and gate["duration_seconds"] > 0
        and isinstance(gate.get("finished_at"), str)
    ]
    if not gates:
        return None
    for latest in sorted(gates, key=lambda gate: gate["finished_at"], reverse=True):
        try:
            finished = datetime.fromisoformat(latest["finished_at"])
        except ValueError:
            continue
        if finished.tzinfo is not None:
            return float(latest["duration_seconds"]), latest["finished_at"]
    return None


def merge_test_runtime(
    branch: str, merged_at: str, history: list[dict[str, Any]]
) -> str:
    # History has branch and completion time, but no Git merge SHA.
    try:
        merge_time = datetime.fromisoformat(merged_at)
    except ValueError:
        return "tests n/a"
    matches: list[tuple[datetime, dict[str, Any]]] = []
    for item in history:
        if item.get("status") != "deployed" or not any(
            isinstance(job, dict) and job.get("branch") == branch
            for job in item.get("jobs") or []
        ):
            continue
        finished_at = item.get("finished_at")
        if not isinstance(finished_at, str):
            continue
        try:
            finished = datetime.fromisoformat(finished_at)
        except ValueError:
            continue
        if finished.tzinfo is not None and merge_time.tzinfo is not None and finished >= merge_time:
            matches.append((finished, item))
    if not matches:
        return "tests n/a"
    _, selected = min(matches, key=lambda match: match[0])
    gates = [
        gate for gate in selected.get("gates") or []
        if isinstance(gate, dict) and gate.get("name") == "tests"
        and gate.get("state") == "success"
        and isinstance(gate.get("duration_seconds"), (int, float))
        and math.isfinite(gate["duration_seconds"])
        and gate["duration_seconds"] > 0
    ]
    if not gates:
        return "tests n/a"
    return f"tests {gates[-1]['duration_seconds']:.1f}s"


ANSI_SGR = re.compile(r"\x1b\[[0-9;]*m")


def wrap_display(text: str, columns: int) -> str:
    """Hard-wrap visible terminal cells so viewport scrolling counts real rows."""
    if columns < 1:
        return text
    rows: list[str] = []
    for logical_line in text.splitlines():
        parts: list[str] = []
        width = 0
        position = 0
        for match in ANSI_SGR.finditer(logical_line):
            for char in logical_line[position : match.start()]:
                char_width = 0 if unicodedata.combining(char) else (
                    2 if unicodedata.east_asian_width(char) in {"F", "W"} else 1
                )
                if width and width + char_width > columns:
                    rows.append("".join(parts))
                    parts = []
                    width = 0
                parts.append(char)
                width += char_width
            parts.append(match.group())
            position = match.end()
        for char in logical_line[position:]:
            char_width = 0 if unicodedata.combining(char) else (
                2 if unicodedata.east_asian_width(char) in {"F", "W"} else 1
            )
            if width and width + char_width > columns:
                rows.append("".join(parts))
                parts = []
                width = 0
            parts.append(char)
            width += char_width
        rows.append("".join(parts))
    return "\n".join(rows)


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
    source_subject: str | None
    change: float | None
    ratio: str


def benchmark_source(repo: Path, commit: str, subject: str) -> tuple[str, str] | None:
    """Find the adjacent source commit for a generated-only recording."""
    if not subject.startswith("Record ") or (
        "benchmark improvement" not in subject.lower()
    ):
        return None
    changed = set(
        git(repo, "diff-tree", "--no-commit-id", "--name-only", "-r", commit)
        .splitlines()
    )
    def generated(path: str) -> bool:
        return path == "benchmarks/RESULTS.md" or path.startswith(
            "benchmarks/baselines/"
        )

    if not changed or not all(map(generated, changed)):
        return None
    parents = git(repo, "show", "-s", "--format=%P", commit).split()
    if len(parents) != 1:
        return None
    parent = parents[0]
    parent_changes = set(
        git(repo, "diff-tree", "--no-commit-id", "--name-only", "-r", parent)
        .splitlines()
    )
    if not any(not generated(path) for path in parent_changes):
        return None
    return parent, git(repo, "show", "-s", "--format=%s", parent)


def benchmark_changes(
    repo: Path, limit: int = 10, *, skip: int = 0
) -> list[BenchmarkChange]:
    history = git(
        repo,
        "log",
        f"-{limit}",
        f"--skip={skip}",
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
        source = benchmark_source(repo, commit, subject)
        return BenchmarkChange(
            commit=commit,
            short_commit=short_commit,
            date=date,
            subject=subject,
            source_subject=source[1] if source else None,
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
    source = benchmark_source(repo, commit, subject)
    if source:
        source_commit, source_subject = source
        lines.append(
            f"recorded after: {styled(source_commit[:10], CYAN, color)} "
            f"{source_subject}"
        )
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
    source = benchmark_source(repo, commit, subject)
    diff_commit = source[0] if source else commit
    patch = git(
        repo,
        "show",
        "--format=",
        "--first-parent",
        "--no-ext-diff",
        "--no-renames",
        diff_commit,
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

    heading = "source change diff:" if source else "benchmark commit diff:"
    lines = [
        styled(heading, BOLD, color),
        f"{styled(short_commit, CYAN, color)} {subject}",
    ]
    if source:
        source_commit, source_subject = source
        lines.append(
            f"preceding source change: {styled(source_commit[:10], CYAN, color)} "
            f"{source_subject}"
        )
    lines.extend([styled("benchmarks/RESULTS.md excluded (generated)", DIM, color), ""])
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


def recent_merges(
    repo: Path, limit: int = 5, *, history: list[dict[str, Any]] | None = None
) -> list[str]:
    merge_log = git(
        repo,
        "log",
        f"-{limit}",
        "--first-parent",
        "--merges",
        "--format=%h%x09%cI%x09%P",
        "--",
    )
    if not merge_log:
        return []

    recorded_history = history if history is not None else history_items(repo)

    def render(line: str) -> str:
        merge, timestamp, parents_text = line.split("\t", 2)
        parents = parents_text.split()
        merged_commit = parents[1]
        branch = merged_branch(repo, merged_commit)
        subject = git(repo, "show", "-s", "--format=%s", merged_commit)
        runtime = merge_test_runtime(branch, timestamp, recorded_history)
        return f"{merge} {timestamp} {branch} [{runtime}] — {subject}"

    return [render(line) for line in merge_log.splitlines()]


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
    benchmark_page: int = 0,
    columns: int = 80,
) -> str:
    if payload.get("contract_version") != 4:
        raise ValueError(
            f"unsupported mergetrain contract version: {payload.get('contract_version')}"
        )
    if benchmark_page < 0:
        raise ValueError("benchmark page must be nonnegative")

    benchmark_index = benchmark_detail_index or benchmark_diff_index
    if benchmark_index is not None:
        changes = benchmark_changes(repo, limit=9, skip=benchmark_page * 9)
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
    recorded_history = history_items(repo)
    runtime = last_test_runtime(repo, history=recorded_history)
    if runtime:
        seconds, finished_at = runtime
        lines.append(
            f"test runtime: {seconds:.1f}s "
            f"({human_age(finished_at)} ago, last passed train gate)"
        )
    else:
        lines.append("test runtime: unavailable")
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
    merges = recent_merges(repo, limit=merge_limit, history=recorded_history)
    lines.append("")
    lines.append(styled("recent merges:", BOLD, color))
    lines.extend(
        [history_line(merge, color, now=now) for merge in merges] or ["(none)"]
    )

    changes = benchmark_changes(repo, limit=9, skip=benchmark_page * 9)
    lines.append("")
    heading = "recent benchmark results:"
    if benchmark_page:
        first = benchmark_page * 9 + 1
        heading = f"recent benchmark results ({first}–{first + len(changes) - 1}):"
    lines.append(styled(heading, BOLD, color))
    ratio_width = max((len(change.ratio) for change in changes), default=0)
    changes_text = [percentage(change.change) for change in changes]
    change_width = max((len(value) + 2 for value in changes_text), default=0)
    ages = [human_age(change.date, now=now) for change in changes]
    age_width = max(map(len, ages), default=0)
    for index, (change, age) in enumerate(zip(changes, ages, strict=True), start=1):
        age_text = f"{age:<{age_width}}"
        ratio_text = f"{change.ratio:>{ratio_width}}"
        change_text = f"({percentage(change.change)})".ljust(change_width)
        prefix_width = (
            len(f"{index}. ") + len(change.short_commit) + len(age_text)
            + len(ratio_text) + len(change_text) + 4
        )
        subject = change.source_subject or change.subject
        available = max(0, columns - prefix_width)
        if len(subject) > available:
            subject = subject[: max(0, available - 1)] + ("…" if available else "")
        lines.append(
            f"{index}. "
            + styled(change.short_commit, CYAN, color)
            + " " + styled(age_text, DIM, color)
            + " " + styled(ratio_text, CYAN, color)
            + " " + styled(change_text, change_style(change.change), color)
            + " " + subject
        )
    if not changes:
        lines.append("(none)")
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
    parser.add_argument("--benchmark-page", type=int, default=0)
    parser.add_argument("--columns", type=int, default=80)
    parser.add_argument("--attention-job-id", type=int)
    parser.add_argument(
        "--attempt-dir", type=Path,
        default=Path("/tmp/dark-compiler-mergetrain-codex-attempts"),
    )
    parser.add_argument("--wrap", action="store_true")
    args = parser.parse_args()
    try:
        payload = json.load(sys.stdin)
        rendered = (
            render_attention_job(
                args.repo.resolve(), args.attention_job_id,
                color=args.color, attempt_dir=args.attempt_dir,
            )
            if args.attention_job_id is not None else render(
                payload,
                args.repo.resolve(),
                color=args.color,
                show_conflicts=args.show_conflicts,
                conflict_toggle_hint=args.conflict_toggle_hint,
                merge_limit=args.merge_limit,
                benchmark_detail_index=args.benchmark_detail_index,
                benchmark_diff_index=args.benchmark_diff_index,
                benchmark_page=args.benchmark_page,
                columns=args.columns,
            )
        )
        print(wrap_display(rendered, args.columns) if args.wrap else rendered)
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
