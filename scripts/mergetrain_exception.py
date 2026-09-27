#!/usr/bin/env python3
"""Request and approve gate exceptions for exact merge-train candidates."""

from __future__ import annotations

import argparse
from collections import deque
import hashlib
import json
import os
import re
import subprocess
import sys
from pathlib import Path
from typing import Any, Sequence


ELIGIBLE_GATES = frozenset({"benchmark-sources", "benchmarks", "leaks"})
SCHEMA = 1


class ExceptionFlowError(RuntimeError):
    pass


def run(repo: Path, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    completed = subprocess.run(
        [*args], cwd=repo, check=False, capture_output=True, text=True
    )
    if check and completed.returncode:
        raise ExceptionFlowError(
            f"{' '.join(args)} failed: {(completed.stderr or completed.stdout).strip()[-500:]}"
        )
    return completed


def git(repo: Path, *args: str) -> str:
    return run(repo, "git", *args).stdout.strip()


def state_dir(repo: Path) -> Path:
    common = Path(git(repo, "rev-parse", "--git-common-dir"))
    if not common.is_absolute():
        common = repo / common
    return common.resolve() / "mergetrain-exceptions"


def write_json(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{os.getpid()}.tmp")
    temporary.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    temporary.replace(path)


def read_json(path: Path) -> dict[str, Any] | None:
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return None
    return payload if isinstance(payload, dict) and payload.get("schema") == SCHEMA else None


def request_path(repo: Path, head: str) -> Path:
    return state_dir(repo) / "requests" / f"{head}.json"


def gate_request_path(repo: Path, head: str, gate: str) -> Path:
    return state_dir(repo) / "requests" / f"{head}-{gate}.json"


def review_path(repo: Path, job_id: int) -> Path:
    return state_dir(repo) / "reviews" / f"{job_id}.json"


def approval_path(repo: Path, tree: str, gate: str) -> Path:
    return state_dir(repo) / "approvals" / f"{tree}-{gate}.json"


def policy_identity(repo: Path) -> tuple[str, str]:
    config = (repo / ".mergetrain.yaml").read_bytes()
    remote = git(repo, "remote", "get-url", "mergetrain-local")
    digest = hashlib.sha256(config + b"\0" + remote.encode()).hexdigest()
    return digest, remote


def create_request(
    repo: Path, *, head: str, branch: str, gate: str, reason: str
) -> Path:
    if gate not in ELIGIBLE_GATES:
        raise ExceptionFlowError(f"gate {gate!r} is not eligible for an exception")
    if not reason.strip() or len(reason) > 500:
        raise ExceptionFlowError("exception reason must contain 1–500 characters")
    if git(repo, "rev-parse", "HEAD") != head:
        raise ExceptionFlowError("request head differs from the committed worktree")
    if git(repo, "branch", "--show-current") != branch:
        raise ExceptionFlowError("request branch differs from the current branch")
    path = gate_request_path(repo, head, gate)
    existing = read_json(path)
    payload = {
        "schema": SCHEMA,
        "head_sha": head,
        "branch": branch,
        "gate": gate,
        "reason": reason.strip(),
    }
    if existing is not None and existing != payload:
        raise ExceptionFlowError("a different exception request already exists for this commit")
    write_json(path, payload)
    return path


def failed_gate(details: dict[str, Any]) -> tuple[str, str]:
    for event in reversed(details.get("events") or []):
        message = str(event.get("message") or "")
        if event.get("state") in {"failure", "failed", "error"} and message.startswith(
            "Failed gate "
        ):
            return message.split(": ", 1)[-1], str(event.get("detail") or "")
    return "", ""


def stage_if_requested(repo: Path, details: dict[str, Any]) -> bool:
    job = details.get("job") or {}
    head = str(job.get("head_sha") or "")
    gate, gate_command = failed_gate(details)
    request = read_json(gate_request_path(repo, head, gate)) if head and gate else None
    if request is None and head:
        # Requests made before multiple gate reviews used one file per commit.
        request = read_json(request_path(repo, head))
    if request is None or request.get("branch") != job.get("branch"):
        return False
    if gate not in ELIGIBLE_GATES or gate != request.get("gate"):
        return False
    job_id = int(job.get("id") or 0)
    candidate = str(job.get("deploy_sha") or "")
    if not job_id or not candidate or not bool(job.get("auto_deploy")):
        raise ExceptionFlowError("requested exception has no exact auto-approved candidate")
    tree = git(repo, "rev-parse", f"{candidate}^{{tree}}")
    base = git(repo, "merge-base", candidate, "refs/remotes/mergetrain-local/main")
    policy, destination = policy_identity(repo)
    review = {
        "schema": SCHEMA,
        "job_id": job_id,
        "head_sha": head,
        "branch": request["branch"],
        "gate": gate,
        "reason": request["reason"],
        "failure": str(job.get("note") or gate_command),
        "gate_command": gate_command,
        "candidate_sha": candidate,
        "candidate_tree": tree,
        "base_sha": base,
        "policy_sha": policy,
        "destination": destination,
        "other_gate_events": [
            {"state": event.get("state"), "message": event.get("message")}
            for event in details.get("events") or []
            if str(event.get("message") or "").startswith(
                ("Passed gate ", "Failed gate ", "Skipped gate ")
            )
        ],
    }
    path = review_path(repo, job_id)
    existing = read_json(path)
    if existing is not None and existing != review:
        raise ExceptionFlowError("candidate changed after exception review was staged")
    if existing == review:
        return True
    write_json(path, review)
    print(
        f"Job #{job_id}: {gate} waiver pending; mergetrain-status [a]",
        file=sys.stderr,
    )
    return True


def inspect(repo: Path, job_id: int) -> dict[str, Any]:
    completed = run(repo, "mergetrain", "--repo", str(repo), "inspect", str(job_id), "--json")
    try:
        payload = json.loads(completed.stdout)
    except json.JSONDecodeError as error:
        raise ExceptionFlowError("mergetrain inspect returned invalid JSON") from error
    if not isinstance(payload, dict):
        raise ExceptionFlowError("mergetrain inspect returned non-object JSON")
    return payload


def approve(repo: Path, job_id: int) -> None:
    review = read_json(review_path(repo, job_id))
    if review is None:
        raise ExceptionFlowError("no staged exception review for this job")
    if review.get("gate") not in ELIGIBLE_GATES:
        raise ExceptionFlowError("gate is no longer eligible for an exception")
    details = inspect(repo, job_id)
    job = details.get("job") or {}
    gate, _failure = failed_gate(details)
    if (
        job.get("status") not in {"blocked", "failed"}
        or job.get("head_sha") != review["head_sha"]
        or job.get("branch") != review["branch"]
        or job.get("auto_deploy") is not True
        or job.get("deploy_sha") != review["candidate_sha"]
        or gate != review["gate"]
        or git(repo, "rev-parse", f"{review['candidate_sha']}^{{tree}}")
        != review["candidate_tree"]
        or git(repo, "rev-parse", "refs/remotes/mergetrain-local/main")
        != review["base_sha"]
        or policy_identity(repo) != (review["policy_sha"], review["destination"])
    ):
        raise ExceptionFlowError("job, candidate, base, or destination changed; review again")
    print(f"Job #{job_id}: {review['branch']} at {review['head_sha']}")
    print(f"Candidate: {review['candidate_sha']} (tree {review['candidate_tree']})")
    print(f"Integration base: {review['base_sha']}")
    print(f"Gate config digest: {review['policy_sha']}")
    print(f"Destination: {review['destination']}")
    print(f"Waive only: {review['gate']}")
    print(f"Reason: {review['reason']}")
    print(f"Failure: {review['failure']}")
    print(f"Gate command: {review['gate_command']}")
    for event in review["other_gate_events"]:
        print(f"  {event['state']}: {event['message']}")
    log_text = str(job.get("log_path") or "")
    if log_text:
        log_path = Path(log_text)
        print(f"Gate log: {log_path}")
        try:
            with log_path.open(encoding="utf-8", errors="replace") as stream:
                for line in deque(stream, maxlen=12):
                    print(f"  {line.rstrip()[:160]}")
        except OSError as error:
            print(f"  (could not read gate log: {error})")
    print("Other gates still run on retry; any failure blocks deployment.")
    challenge = f"approve {job_id} {review['head_sha'][:12]} {review['gate']}"
    if not sys.stdin.isatty():
        raise ExceptionFlowError("approval requires an interactive terminal")
    if input(f"Type '{challenge}' to approve: ").strip() != challenge:
        raise ExceptionFlowError("approval was not granted")
    approval = {**review, "approved": True}
    path = approval_path(repo, review["candidate_tree"], review["gate"])
    if read_json(path) is not None:
        raise ExceptionFlowError("an approval already exists for this candidate and gate")
    prior_approvals: list[tuple[Path, dict[str, Any]]] = []
    for prior_path in (state_dir(repo) / "approvals").glob(
        f"{review['candidate_tree']}-*.json"
    ):
        prior = read_json(prior_path)
        if prior is None or prior.get("replacement_job_id") != job_id:
            continue
        for key in (
            "head_sha", "branch", "candidate_tree", "base_sha", "policy_sha", "destination"
        ):
            if prior.get(key) != review[key]:
                raise ExceptionFlowError("earlier approval does not match this exact candidate")
        prior_approvals.append((prior_path, prior))
    try:
        write_json(path, approval)
        replacement = run(
            repo, "mergetrain", "--repo", str(repo), "retry", str(job_id), "--json"
        )
        replacement_job = json.loads(replacement.stdout)["job"]
        replacement_id = int(replacement_job["id"])
        if replacement_id <= 0:
            raise ExceptionFlowError("retry returned an invalid replacement job ID")
        if replacement_job.get("auto_deploy") is not True:
            raise ExceptionFlowError(
                f"replacement job #{replacement_id} lost unattended approval; "
                "gate exception was not retained"
            )
        for prior_path, prior in prior_approvals:
            write_json(prior_path, {**prior, "replacement_job_id": replacement_id})
        approval["replacement_job_id"] = replacement_id
        write_json(path, approval)
    except ExceptionFlowError:
        path.unlink(missing_ok=True)
        raise
    except (KeyError, ValueError, TypeError, json.JSONDecodeError) as error:
        path.unlink(missing_ok=True)
        raise ExceptionFlowError("retry did not return a replacement job") from error
    review_path(repo, job_id).unlink(missing_ok=True)
    print(f"Approved {review['gate']} for job #{job_id}; retried as job #{replacement_id}")


def review_pending(repo: Path) -> None:
    if not sys.stdin.isatty():
        raise ExceptionFlowError("approval review requires an interactive terminal")
    reviews = state_dir(repo) / "reviews"
    pending = sorted(
        (
            (int(path.stem), review)
            for path in reviews.glob("*.json")
            if (review := read_json(path)) is not None
            and review.get("gate") in ELIGIBLE_GATES
            and path.stem.isdecimal()
        ),
        key=lambda item: item[0],
    )
    if not pending:
        print("No pending gate exceptions")
        return
    if len(pending) == 1:
        approve(repo, pending[0][0])
        return
    print("Pending gate exceptions:")
    for job_id, review in pending:
        prefix = f"  #{job_id} {review['gate']}: "
        reason = " ".join(str(review["reason"]).split())
        print(prefix + reason[:max(0, 80 - len(prefix))])
    selected = input("Job ID to review (blank to cancel): ").strip()
    if not selected:
        return
    if not selected.isdecimal() or int(selected) not in {job_id for job_id, _ in pending}:
        raise ExceptionFlowError("select a listed job ID")
    approve(repo, int(selected))


def run_gate(repo: Path, gate: str, command: Sequence[str]) -> int:
    if gate not in ELIGIBLE_GATES or not command:
        raise ExceptionFlowError("gate wrapper needs one eligible gate and its command")
    candidate = Path.cwd().resolve()
    worktree = os.environ.get("MERGETRAIN_WORKTREE")
    if worktree is None or Path(worktree).resolve() != candidate:
        raise ExceptionFlowError("gate wrapper must run in the merge-train worktree")
    result = subprocess.run(list(command), cwd=candidate, check=False)
    if result.returncode == 0:
        return 0
    tree = git(candidate, "rev-parse", "HEAD^{tree}")
    approval = read_json(approval_path(repo, tree, gate))
    if approval is not None and approval.get("approved") is True:
        job_match = re.search(r"-mergetrain-(\d+)-[0-9a-f]{8}$", candidate.name)
        policy, destination = policy_identity(repo)
        base = git(repo, "rev-parse", "refs/remotes/mergetrain-local/main")
        ancestor = run(
            candidate, "git", "merge-base", "--is-ancestor", approval["head_sha"], "HEAD",
            check=False,
        ).returncode == 0
        if (
            approval.get("gate") == gate
            and job_match is not None
            and approval.get("replacement_job_id") == int(job_match.group(1))
            and approval.get("candidate_tree") == tree
            and approval.get("policy_sha") == policy
            and approval.get("destination") == destination
            and approval.get("base_sha") == base
            and ancestor
        ):
            print(
                f"Human-approved exception: {gate} for candidate tree {tree[:12]} "
                f"(original job #{approval['job_id']})"
            )
            return 0
    return result.returncode


def finalize(repo: Path) -> None:
    approvals = state_dir(repo) / "approvals"
    if not approvals.is_dir():
        return
    for path in approvals.glob("*.json"):
        approval = read_json(path)
        if approval is None or not isinstance(approval.get("replacement_job_id"), int):
            continue
        details = inspect(repo, approval["replacement_job_id"])
        status = (details.get("job") or {}).get("status")
        if status not in {"deployed", "canceled", "blocked", "failed"}:
            continue
        approval["final_status"] = status
        archive = state_dir(repo) / "used" / path.name
        write_json(archive, approval)
        path.unlink()
        print(
            f"Job #{approval['replacement_job_id']}: archived {approval['gate']} "
            f"exception after {status}",
            file=sys.stderr,
        )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=Path(__file__).resolve().parent.parent)
    commands = parser.add_subparsers(dest="action", required=True)
    request = commands.add_parser("request")
    request.add_argument("--head", required=True)
    request.add_argument("--branch", required=True)
    request.add_argument("--gate", required=True)
    request.add_argument("--reason", required=True)
    commands.add_parser("review")
    gate_command = commands.add_parser("gate")
    gate_command.add_argument("gate")
    gate_command.add_argument("command", nargs=argparse.REMAINDER)
    commands.add_parser("finalize")
    args = parser.parse_args()
    repo = args.repo.resolve()
    try:
        if args.action == "request":
            create_request(
                repo, head=args.head, branch=args.branch, gate=args.gate,
                reason=args.reason,
            )
        elif args.action == "review":
            review_pending(repo)
        elif args.action == "finalize":
            finalize(repo)
        else:
            command = args.command[1:] if args.command[:1] == ["--"] else args.command
            return run_gate(repo, args.gate, command)
    except (EOFError, KeyboardInterrupt):
        print("Merge-train exception review canceled", file=sys.stderr)
        return 1
    except (ExceptionFlowError, OSError, KeyError, ValueError) as error:
        print(f"Merge-train exception: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
