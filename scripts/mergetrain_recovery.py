#!/usr/bin/env python3
"""Recover one blocked merge-train job without mutating its owning worktree."""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Sequence


class RecoveryError(RuntimeError):
    pass


@dataclass(frozen=True)
class Failure:
    category: str
    gate: str
    detail: str

    @property
    def transient(self) -> bool:
        match = re.search(r"exit_code=(-?\d+)", self.detail)
        signaled = match is not None and int(match.group(1)) < 0
        text = f"{self.category} {self.detail}".lower()
        return signaled or any(word in text for word in ("timeout", "timed out", "interrupted", "lost lease"))


def run(
    args: Sequence[str],
    *,
    cwd: Path,
    check: bool = True,
    input_text: str | None = None,
    log: Path | None = None,
) -> subprocess.CompletedProcess[str]:
    completed = subprocess.run(
        list(args),
        cwd=cwd,
        input=input_text,
        text=True,
        capture_output=True,
        check=False,
    )
    if log is not None:
        log.write_text(
            f"$ {' '.join(args)}\n{completed.stdout}{completed.stderr}",
            encoding="utf-8",
        )
    if check and completed.returncode != 0:
        detail = (completed.stderr or completed.stdout).strip()
        raise RecoveryError(f"{' '.join(args)} failed: {detail[-1200:]}")
    return completed


def mergetrain(repo: Path, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    return run(("mergetrain", "--repo", str(repo), *args), cwd=repo, check=check)


def load_json(completed: subprocess.CompletedProcess[str], operation: str) -> dict[str, Any]:
    try:
        value = json.loads(completed.stdout)
    except json.JSONDecodeError as error:
        raise RecoveryError(f"{operation} returned invalid JSON: {error}") from error
    if not isinstance(value, dict):
        raise RecoveryError(f"{operation} returned a non-object JSON payload")
    return value


def inspect_job(repo: Path, job_id: int) -> dict[str, Any]:
    return load_json(mergetrain(repo, "inspect", str(job_id), "--json"), "inspect")


def preserve_failure_evidence(
    details: dict[str, Any], attempts: Path, job_id: int, head_sha: str
) -> Path | None:
    job = details.get("job") or {}
    source_text = str(job.get("log_path") or "")
    if not source_text:
        return None
    source = Path(source_text)
    if not source.is_file():
        return None
    destination = attempts / f"{job_id}-{head_sha}.train.log"
    shutil.copyfile(source, destination)
    details["recovery_evidence"] = {"train_log": str(destination)}
    return destination


def failure_from(details: dict[str, Any]) -> Failure:
    outcome = details.get("outcome") or {}
    category = str(outcome.get("failure_category") or "unknown")
    for event in reversed(details.get("events") or []):
        message = str(event.get("message") or "")
        match = re.fullmatch(r"Failed gate \d+/\d+: (.+)", message)
        if event.get("state") in {"failure", "failed", "error"} and match:
            return Failure(category, match.group(1), str(event.get("detail") or ""))
    return Failure(category, "", str(outcome.get("message") or ""))


def configured_integration(repo: Path) -> tuple[str, str]:
    config = repo / ".mergetrain.yaml"
    if not config.exists():
        raise RecoveryError(".mergetrain.yaml is required for recovery")
    remote = ""
    branch = ""
    in_git = False
    for raw_line in config.read_text(encoding="utf-8").splitlines():
        line = raw_line.split("#", 1)[0].rstrip()
        if line == "git:":
            in_git = True
            continue
        if in_git and line and not line.startswith(" "):
            in_git = False
        if in_git:
            match = re.match(r"\s+remote:\s*(\S+)\s*$", line)
            if match:
                remote = match.group(1)
            match = re.match(r"\s+integration_branch:\s*(\S+)\s*$", line)
            if match:
                branch = match.group(1)
    if not remote or not branch:
        raise RecoveryError("git.remote and git.integration_branch are required")
    fetched = run(("git", "fetch", remote, branch), cwd=repo)
    del fetched
    ref = f"refs/remotes/{remote}/{branch}"
    sha = run(("git", "rev-parse", ref), cwd=repo).stdout.strip()
    return ref, sha


def unique_commits(repo: Path, integration_sha: str, head_sha: str) -> tuple[list[str], list[str]]:
    result = run(("git", "cherry", "-v", integration_sha, head_sha), cwd=repo)
    unique: list[str] = []
    equivalent: list[str] = []
    for line in result.stdout.splitlines():
        fields = line.split(maxsplit=2)
        if len(fields) < 2:
            continue
        if fields[0] == "+":
            unique.append(fields[1])
        elif fields[0] == "-":
            equivalent.append(fields[1])
    return unique, equivalent


def exact_retry_once(repo: Path, attempts: Path, job_id: int, head_sha: str, failure: Failure) -> bool:
    marker = attempts / f"transient-{head_sha}-{failure.gate or 'unknown'}.attempted"
    if marker.exists():
        return False
    marker.write_text(f"job={job_id}\ndetail={failure.detail}\n", encoding="utf-8")
    mergetrain(repo, "retry", str(job_id), "--json")
    print(f"Job #{job_id}: transient {failure.gate or 'gate'} failure retried at unchanged {head_sha[:10]}", file=sys.stderr)
    return True


def dismiss_superseded(repo: Path, job_id: int, integration_sha: str, equivalent: list[str]) -> None:
    note = (
        f"superseded by integration {integration_sha[:12]}; "
        f"{len(equivalent)} commit(s) are patch-equivalent"
    )
    mergetrain(repo, "dismiss", str(job_id), "--note", note, "--json")
    print(f"Job #{job_id}: dismissed as fully patch-equivalent to {integration_sha[:10]}", file=sys.stderr)


def create_recovery_worktree(
    repo: Path,
    attempts: Path,
    job_id: int,
    old_head: str,
    integration_sha: str,
    tracking_ref: str,
    commits: list[str],
) -> tuple[Path, str, bool]:
    short = old_head[:10]
    worktree = attempts / f"recovery-{job_id}-{short}"
    branch = f"mergetrain-repair/{job_id}-{short}"
    if worktree.exists():
        raise RecoveryError(f"recovery worktree already exists: {worktree}")
    run(("git", "worktree", "add", "-b", branch, str(worktree), integration_sha), cwd=repo)
    run(("git", "branch", "--set-upstream-to", tracking_ref, branch), cwd=worktree)
    conflict = False
    for commit in commits:
        parents = run(("git", "rev-list", "--parents", "-n", "1", commit), cwd=worktree).stdout.split()
        cherry_pick_args = ["git", "cherry-pick"]
        if len(parents) > 2:
            cherry_pick_args.extend(("-m", "1"))
        cherry_pick_args.append(commit)
        cherry_pick = run(cherry_pick_args, cwd=worktree, check=False)
        if cherry_pick.returncode != 0:
            conflict = True
            break
    return worktree, branch, conflict


def codex_instructions(
    details: dict[str, Any],
    failure: Failure,
    conflict: bool,
    integration_sha: str,
) -> str:
    job = details.get("job") or {}
    benchmark_rule = ""
    if failure.gate == "benchmarks":
        benchmark_rule = """
Run ./benchmarks/run_benchmarks.sh full in recording mode. Commit the regenerated
snapshot and benchmarks/RESULTS.md only if it proves an aggregate improvement.
If it reports a regression, produces no tracked benchmark change, or cannot
complete, do not commit a repair.

If benchmarks/RESULTS.md is conflicted, never hand-merge it or choose a side.
Resolve source conflicts first; recording mode is the only valid resolution.
"""
    elif failure.category in {"merge_conflict", "semantic_conflict"}:
        benchmark_rule = """
If benchmarks/RESULTS.md is conflicted, never hand-merge it or choose a side.
Resolve source conflicts first, then run ./benchmarks/run_benchmarks.sh full.
Only commit generated benchmark files when recording mode proves an aggregate
improvement and regenerates RESULTS.md. Abort the cherry-pick otherwise.
"""
    state = (
        "A cherry-pick is stopped at conflicts. Resolve the semantic merge, preserving both changes, then continue it."
        if conflict
        else "The unique job commits have been replayed. Diagnose and repair the reproducible gate failure."
    )
    return f"""Recover merge-train job #{job.get('id', '')} ({job.get('task', '')}).

Read AGENTS.md, AGENTS.mergetrain.md, and docs/index.md completely before editing.
This is a fresh recovery worktree based on integration commit {integration_sha}.
The original branch and owning worktree must remain untouched. {state}

Failure category: {failure.category}
Failed gate: {failure.gate or 'not reported'}
Failure detail: {failure.detail or 'not reported'}

Run relevant focused checks while repairing and commit the completed recovery.
Do not invoke ./land or any mergetrain command. Do not push, enqueue, retry,
dismiss, deploy, or modify queue state. The integrator owns verification and replacement.
{benchmark_rule}
Inspection JSON is provided on stdin. If a safe repair is not possible, leave
the worktree unchanged and explain the exact blocker.
"""


def invoke_codex(
    repo: Path,
    worktree: Path,
    attempts: Path,
    job_id: int,
    old_head: str,
    details: dict[str, Any],
    failure: Failure,
    conflict: bool,
    integration_sha: str,
) -> None:
    marker = attempts / f"{job_id}-{old_head}.attempted"
    if marker.exists():
        raise RecoveryError(f"Codex already attempted job #{job_id} revision {old_head}")
    marker.write_text(f"worktree={worktree}\n", encoding="utf-8")
    output = attempts / f"{job_id}-{old_head}.last-message.txt"
    log = attempts / f"{job_id}-{old_head}.codex.log"
    common_dir = run(
        ("git", "rev-parse", "--path-format=absolute", "--git-common-dir"),
        cwd=worktree,
    ).stdout.strip()
    args = (
        "codex", "exec", "-C", str(worktree), "--sandbox", "workspace-write",
        "--add-dir", common_dir, "--add-dir", str(attempts),
        "--ephemeral", "--output-last-message", str(output),
        codex_instructions(details, failure, conflict, integration_sha),
    )
    completed = run(args, cwd=repo, check=False, input_text=json.dumps(details), log=log)
    if completed.returncode != 0:
        summary = output.read_text(encoding="utf-8").strip() if output.exists() else "Codex failed"
        raise RecoveryError(f"Codex repair failed: {summary}; full log: {log}")


def isolated_gate_reproduces(
    worktree: Path,
    attempts: Path,
    job_id: int,
    old_head: str,
    failure: Failure,
) -> bool:
    if failure.gate == "build":
        commands = [("./build", "--ai")]
    elif failure.gate == "tests":
        commands = [("./build", "--ai"), ("./run-tests", "--ai")]
    else:
        return True
    for index, command in enumerate(commands, start=1):
        log = attempts / f"{job_id}-{old_head}.reproduce-{index}.log"
        completed = run(command, cwd=worktree, check=False, log=log)
        if completed.returncode != 0:
            print(
                f"Job #{job_id}: reproduced {failure.gate} failure in isolation; log: {log}",
                file=sys.stderr,
            )
            return True
    return False


def discard_recovery_worktree(repo: Path, worktree: Path, branch: str) -> None:
    run(("git", "worktree", "remove", "--force", str(worktree)), cwd=repo)
    run(("git", "branch", "-D", branch), cwd=repo)


def verify_repair(
    worktree: Path,
    attempts: Path,
    job_id: int,
    integration_sha: str,
) -> tuple[str, Path]:
    head = run(("git", "rev-parse", "HEAD"), cwd=worktree).stdout.strip()
    status = run(("git", "status", "--porcelain"), cwd=worktree).stdout.strip()
    if head == integration_sha:
        raise RecoveryError("recovery produced no commit")
    if status:
        raise RecoveryError("recovery worktree is not clean")

    commands: list[tuple[str, ...]] = [
        ("git", "diff", "--check", f"{integration_sha}..HEAD"),
        ("python3", "scripts/check_e2e_temp_paths.py"),
        ("python3", "benchmarks/check_sources_unchanged.py", "--base", integration_sha),
        ("./build", "--ai"),
        ("./run-tests", "--ai"),
        ("./benchmarks/run_benchmarks.sh", "--verify-parent", "full"),
    ]
    results: list[dict[str, Any]] = []
    for index, command in enumerate(commands, start=1):
        command_log = attempts / f"{job_id}-{head}.verify-{index}.log"
        completed = run(command, cwd=worktree, check=False, log=command_log)
        results.append({
            "command": list(command),
            "exit_code": completed.returncode,
            "log": str(command_log),
        })
        if completed.returncode != 0:
            raise RecoveryError(f"verification failed: {' '.join(command)}; full log: {command_log}")

    receipt = attempts / f"{job_id}-{head}.verification.json"
    receipt.write_text(
        json.dumps(
            {
                "contract_version": 1,
                "job_id": job_id,
                "integration_sha": integration_sha,
                "head_sha": head,
                "worktree": str(worktree),
                "commands": results,
            },
            indent=2,
            sort_keys=True,
        ) + "\n",
        encoding="utf-8",
    )
    return head, receipt


def supports_atomic_replace(repo: Path) -> bool:
    return mergetrain(repo, "replace", "--help", check=False).returncode == 0


def replace_job(
    repo: Path,
    details: dict[str, Any],
    job_id: int,
    branch: str,
    worktree: Path,
    head: str,
    integration_sha: str,
    receipt: Path,
) -> int:
    job = details.get("job") or {}
    task = str(job.get("task") or f"repair job {job_id}")
    note = f"verified replacement {head[:12]} for job #{job_id}; receipt={receipt}"
    if supports_atomic_replace(repo):
        payload = load_json(
            mergetrain(
                repo, "replace", str(job_id), "--task", task, "--branch", branch,
                "--worktree", str(worktree), "--head", head, "--parent", integration_sha,
                "--verification-receipt", str(receipt), "--json",
            ),
            "replace",
        )
        replacement = payload.get("replacement") or payload.get("job") or {}
        return int(replacement.get("id") or 0)

    # Compatibility path for mergetrain v3. The integrator is the only active
    # runner here. Enqueue first so a crash cannot lose the repaired work, then
    # dismiss the blocked row. A native replace command makes this transactional.
    enqueue_args = [
        "enqueue", "--task", task, "--branch", branch, "--worktree", str(worktree),
        "--note", note,
    ]
    if bool(job.get("auto_deploy")):
        enqueue_args.append("--auto")
    enqueue_args.append("--json")
    payload = load_json(mergetrain(repo, *enqueue_args), "enqueue replacement")
    replacement = payload.get("job") or {}
    replacement_id = int(replacement.get("id") or 0)
    try:
        mergetrain(
            repo, "dismiss", str(job_id), "--note",
            f"replaced by job #{replacement_id} at {head[:12]}", "--json",
        )
    except RecoveryError as error:
        raise RecoveryError(
            f"replacement #{replacement_id} was safely enqueued but old job #{job_id} "
            f"could not be dismissed: {error}"
        ) from error
    return replacement_id


def recover(repo: Path, attempts: Path, job_id: int) -> None:
    details = inspect_job(repo, job_id)
    job = details.get("job") or {}
    if not bool(job.get("auto_deploy")):
        raise RecoveryError("job does not carry bounded unattended deployment approval")
    old_head = str(job.get("head_sha") or "")
    if not old_head:
        raise RecoveryError("inspection omitted job.head_sha")
    owning_worktree_text = str(job.get("worktree_path") or "")
    owning_worktree = Path(owning_worktree_text).resolve() if owning_worktree_text else None
    owning_snapshot: tuple[str, str] | None = None
    if owning_worktree is not None and owning_worktree.is_dir():
        owning_snapshot = (
            run(("git", "rev-parse", "HEAD"), cwd=owning_worktree).stdout.strip(),
            run(("git", "status", "--porcelain"), cwd=owning_worktree).stdout,
        )
        if owning_snapshot != (old_head, ""):
            raise RecoveryError("owning worktree is not clean at the enqueued commit")
    failure = failure_from(details)
    preserve_failure_evidence(details, attempts, job_id, old_head)

    if failure.category == "gate_failed" and failure.transient:
        if exact_retry_once(repo, attempts, job_id, old_head, failure):
            return

    recoverable = {"merge_conflict", "semantic_conflict", "push_rejected", "gate_failed"}
    if failure.category not in recoverable:
        raise RecoveryError(f"operator-only failure category: {failure.category}")
    if failure.category == "push_rejected" and "non-fast-forward" not in failure.detail.lower():
        raise RecoveryError(f"operator-only push rejection: {failure.detail}")
    if failure.category == "gate_failed" and failure.gate not in {"build", "tests", "benchmarks"}:
        raise RecoveryError(f"operator-only gate failure: {failure.gate or 'unknown'}")

    tracking_ref, integration_sha = configured_integration(repo)
    unique, equivalent = unique_commits(repo, integration_sha, old_head)
    if not unique:
        dismiss_superseded(repo, job_id, integration_sha, equivalent)
        return

    worktree, branch, conflict = create_recovery_worktree(
        repo, attempts, job_id, old_head, integration_sha, tracking_ref, unique
    )
    print(
        f"Job #{job_id}: fresh recovery worktree {worktree}; "
        f"{len(equivalent)} superseded, {len(unique)} unique commit(s)",
        file=sys.stderr,
    )
    if not conflict and failure.gate in {"build", "tests"}:
        if not isolated_gate_reproduces(worktree, attempts, job_id, old_head, failure):
            discard_recovery_worktree(repo, worktree, branch)
            if exact_retry_once(repo, attempts, job_id, old_head, failure):
                print(
                    f"Job #{job_id}: {failure.gate} passed in isolation; retried unchanged as flaky/interaction evidence",
                    file=sys.stderr,
                )
                return
            raise RecoveryError(
                f"{failure.gate} failure does not reproduce on the isolated job; "
                "automatic semantic repair would target the wrong change"
            )
    invoke_codex(
        repo, worktree, attempts, job_id, old_head, details, failure, conflict, integration_sha
    )
    head, receipt = verify_repair(worktree, attempts, job_id, integration_sha)
    if owning_worktree is not None and owning_snapshot is not None:
        current_owning = (
            run(("git", "rev-parse", "HEAD"), cwd=owning_worktree).stdout.strip(),
            run(("git", "status", "--porcelain"), cwd=owning_worktree).stdout,
        )
        if current_owning != owning_snapshot:
            raise RecoveryError("Codex changed the original owning worktree")
    replacement_id = replace_job(
        repo, details, job_id, branch, worktree, head, integration_sha, receipt
    )
    print(
        f"Job #{job_id}: verified Codex repair {head[:10]} and replaced it with job #{replacement_id}",
        file=sys.stderr,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", type=Path, required=True)
    parser.add_argument("--attempt-dir", type=Path, required=True)
    parser.add_argument("--job-id", type=int, required=True)
    args = parser.parse_args()
    repo = args.repo.resolve()
    attempts = args.attempt_dir.resolve()
    attempts.mkdir(parents=True, exist_ok=True)
    try:
        recover(repo, attempts, args.job_id)
        return 0
    except RecoveryError as error:
        print(f"Job #{args.job_id}: operator attention required — {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
