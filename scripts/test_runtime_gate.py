#!/usr/bin/env python3
"""Run the full test suite on an uncontended CPU and compare it with its parent."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import resource
import subprocess
import sys
import tempfile
import time
from pathlib import Path


SCHEMA = 2
MAX_RATIO = 1.10
MAX_DELTA_SECONDS = 60.0
MAX_OTHER_CPU_FRACTION = 0.03
MAX_ATTEMPTS = 3


class GateError(Exception):
    """A required measurement or comparison could not be established."""


def git(repo: Path, *args: str) -> str:
    result = subprocess.run(
        ("git", "-C", str(repo), *args), capture_output=True, text=True, check=False
    )
    if result.returncode:
        raise GateError(result.stderr.strip() or f"git {' '.join(args)} failed")
    return result.stdout.strip()


def host_signature(repo: Path) -> str:
    # Kernel and CPU model changes make cached wall times incomparable.
    cpuinfo = Path("/proc/cpuinfo").read_text(encoding="utf-8")
    model = next(
        (line.split(":", 1)[1].strip() for line in cpuinfo.splitlines()
         if line.startswith(("model name", "Processor", "Hardware"))),
        platform.machine(),
    )
    machine_id = Path("/etc/machine-id")
    machine = (
        hashlib.sha256(machine_id.read_bytes()).hexdigest()[:16]
        if machine_id.is_file() else "unidentified"
    )
    runtime = subprocess.run(
        (str(repo / "scripts/dotnet-host"), "--version"),
        capture_output=True, text=True, check=False,
    )
    if runtime.returncode:
        raise GateError("cannot determine the .NET version for timing comparison")
    return f"{machine}|{platform.machine()}|{model}|{platform.release()}|{runtime.stdout.strip()}"


def require_clean_commit(repo: Path) -> str:
    head = git(repo, "rev-parse", "HEAD")
    if git(repo, "status", "--porcelain", "--untracked-files=normal"):
        raise GateError("timing requires a clean committed worktree")
    return head


def cpu_ticks(cpu: int) -> tuple[int, int]:
    prefix = f"cpu{cpu} "
    line = next(
        (line for line in Path("/proc/stat").read_text(encoding="utf-8").splitlines()
         if line.startswith(prefix)),
        None,
    )
    if line is None:
        raise GateError(f"CPU {cpu} has no /proc/stat record")
    values = [int(value) for value in line.split()[1:]]
    total = sum(values[:8])
    idle = values[3] + values[4]
    return total, total - idle


def pair_ticks(cpus: tuple[int, int]) -> tuple[int, int]:
    readings = [cpu_ticks(cpu) for cpu in cpus]
    return sum(value[0] for value in readings), sum(value[1] for value in readings)


def seconds(value: resource.struct_rusage) -> float:
    return value.ru_utime + value.ru_stime


def competing_cpu_seconds(busy_ticks: int, child_cpu_seconds: float) -> float:
    return max(0.0, busy_ticks / os.sysconf("SC_CLK_TCK") - child_cpu_seconds)


def exceeds_limit(parent_seconds: float, candidate_seconds: float) -> bool:
    return (candidate_seconds > parent_seconds * MAX_RATIO
            or candidate_seconds - parent_seconds > MAX_DELTA_SECONDS)


def select_cpus(excluded: set[int]) -> tuple[int, int]:
    if not hasattr(os, "sched_getaffinity"):
        raise GateError("CPU affinity is required for an uncontended timing sample")
    allowed = sorted(os.sched_getaffinity(0) - excluded)
    if len(allowed) < 2:
        raise GateError("fewer than two unused CPUs remain for an uncontended retry")
    before = {cpu: cpu_ticks(cpu) for cpu in allowed}
    time.sleep(1)
    after = {cpu: cpu_ticks(cpu) for cpu in allowed}
    ordered = sorted(
        allowed,
        key=lambda cpu: (
            (after[cpu][1] - before[cpu][1])
            / max(1, after[cpu][0] - before[cpu][0]),
            cpu,
        ),
    )
    return ordered[0], ordered[1]


def run_tests(repo: Path, output: Path) -> dict[str, object]:
    head = require_clean_commit(repo)
    output.parent.mkdir(parents=True, exist_ok=True)
    excluded: set[int] = set()
    for attempt in range(1, MAX_ATTEMPTS + 1):
        cpus = select_cpus(excluded)
        excluded.update(cpus)
        timing_path = output.with_suffix(".timings.json")
        timing_path.unlink(missing_ok=True)
        before_ticks = pair_ticks(cpus)
        before_usage = resource.getrusage(resource.RUSAGE_CHILDREN)
        started = time.monotonic()
        result = subprocess.run(
            ("taskset", "-c", ",".join(map(str, cpus)), "./run-tests", "--ai",
             f"--timings-json={timing_path}"),
            cwd=repo,
            check=False,
        )
        elapsed = time.monotonic() - started
        after_usage = resource.getrusage(resource.RUSAGE_CHILDREN)
        after_ticks = pair_ticks(cpus)
        if result.returncode:
            raise GateError(f"full test suite failed with exit code {result.returncode}")
        if not timing_path.is_file():
            raise GateError("test runner did not write timing JSON")
        timing = json.loads(timing_path.read_text(encoding="utf-8"))
        summary = timing.get("summary", {})
        if summary.get("failed") != 0 or summary.get("passed", 0) <= 0:
            raise GateError("test timing JSON does not describe a passing full suite")
        cpu_time = seconds(after_usage) - seconds(before_usage)
        busy_ticks = after_ticks[1] - before_ticks[1]
        total_ticks = after_ticks[0] - before_ticks[0]
        clock_ticks = os.sysconf("SC_CLK_TCK")
        other_cpu = competing_cpu_seconds(busy_ticks, cpu_time)
        # A single CPU must remain substantially available even if the test
        # process moves between the pair to execute native child processes.
        allowed_other = MAX_OTHER_CPU_FRACTION * total_ticks / (2 * clock_ticks) + 1.0
        if other_cpu > allowed_other:
            print(
                f"Test runtime sample {attempt} discarded: CPUs {cpus} had "
                f"{other_cpu:.1f}s of competing work (limit {allowed_other:.1f}s)",
                flush=True,
            )
            continue
        measurement: dict[str, object] = {
            "schema": SCHEMA,
            "commit": head,
            "host": host_signature(repo),
            "elapsed_seconds": elapsed,
            "cpu_seconds": cpu_time,
            "other_cpu_seconds": other_cpu,
            "cpus": cpus,
            "passed": summary["passed"],
            "total": summary["total"],
            "e2e_batch_size": summary["e2e_batch_size"],
        }
        temporary = output.with_suffix(".tmp")
        temporary.write_text(json.dumps(measurement, indent=2) + "\n", encoding="utf-8")
        temporary.replace(output)
        print(
            f"Uncontended test runtime: {elapsed:.1f}s on CPUs {cpus}; "
            f"{summary['passed']}/{summary['total']} passed",
            flush=True,
        )
        return measurement
    raise GateError("all test timing samples were contended; no valid runtime measurement")


def validate_metric(metric: object, commit: str, host: str) -> dict[str, object]:
    if not isinstance(metric, dict) or metric.get("schema") != SCHEMA:
        raise GateError("test runtime measurement has an unknown schema")
    if metric.get("commit") != commit or metric.get("host") != host:
        raise GateError("test runtime measurement has a mismatched commit or host")
    elapsed = metric.get("elapsed_seconds")
    if not isinstance(elapsed, (float, int)) or not 0 < elapsed < 86400:
        raise GateError("test runtime measurement has an invalid elapsed time")
    return metric


def cache_path(repo: Path, commit: str) -> Path:
    common = Path(git(repo, "rev-parse", "--git-common-dir"))
    if not common.is_absolute():
        common = repo / common
    return common.resolve() / "test-runtime-gate" / f"{commit}.json"


def parent_metric(repo: Path, base: str, host: str) -> dict[str, object]:
    cached = cache_path(repo, base)
    if cached.is_file():
        return validate_metric(json.loads(cached.read_text(encoding="utf-8")), base, host)
    # The first run for a parent needs one measured baseline. Later train jobs
    # reuse the successful candidate's cached measurement.
    with tempfile.TemporaryDirectory(prefix="dark-test-runtime-parent-") as directory:
        worktree = Path(directory) / "checkout"
        subprocess.run(("git", "-C", str(repo), "worktree", "add", "--detach",
                        str(worktree), base), check=True, capture_output=True, text=True)
        try:
            build = subprocess.run(("./build", "--ai"), cwd=worktree, check=False)
            if build.returncode:
                raise GateError("integration parent build failed during timing baseline setup")
            baseline = run_tests(worktree, worktree / "TestResults/ai/test-runtime.json")
        finally:
            subprocess.run(("git", "-C", str(repo), "worktree", "remove", "--force",
                            str(worktree)), check=True, capture_output=True, text=True)
    cached.parent.mkdir(parents=True, exist_ok=True)
    cached.write_text(json.dumps(baseline, indent=2) + "\n", encoding="utf-8")
    return baseline


def check(repo: Path, base_ref: str, output: Path) -> None:
    head = require_clean_commit(repo)
    base = git(repo, "merge-base", "HEAD", base_ref)
    if subprocess.run(("git", "-C", str(repo), "merge-base", "--is-ancestor", base,
                       head), check=False).returncode:
        raise GateError("test runtime base is not an ancestor of HEAD")
    host = host_signature(repo)
    candidate = validate_metric(json.loads(output.read_text(encoding="utf-8")), head, host)
    parent = parent_metric(repo, base, host)
    if candidate["e2e_batch_size"] != parent["e2e_batch_size"]:
        raise GateError("candidate and parent used different E2E batch sizes")
    parent_seconds = float(parent["elapsed_seconds"])
    candidate_seconds = float(candidate["elapsed_seconds"])
    delta = candidate_seconds - parent_seconds
    ratio = candidate_seconds / parent_seconds
    print(
        f"Test runtime: parent {parent_seconds:.1f}s, candidate {candidate_seconds:.1f}s, "
        f"delta {delta:+.1f}s, ratio {ratio:.3f}; limits +10% and +60s",
        flush=True,
    )
    if exceeds_limit(parent_seconds, candidate_seconds):
        raise GateError("test runtime significantly increased")
    destination = cache_path(repo, head)
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(json.dumps(candidate, indent=2) + "\n", encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("run", "check"))
    parser.add_argument("--base", help="integration ref or task parent for check mode")
    parser.add_argument("--output", type=Path,
                        default=Path("TestResults/ai/test-runtime.json"))
    parser.add_argument("--repo", type=Path,
                        default=Path(__file__).resolve().parent.parent)
    args = parser.parse_args()
    try:
        if args.mode == "run":
            run_tests(args.repo.resolve(), args.repo.resolve() / args.output)
        else:
            if not args.base:
                raise GateError("check mode requires --base")
            check(args.repo.resolve(), args.base, args.repo.resolve() / args.output)
    except (GateError, OSError, ValueError, KeyError, json.JSONDecodeError) as error:
        print(f"Test runtime gate failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
