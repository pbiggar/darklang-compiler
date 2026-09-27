#!/usr/bin/env python3
"""Let an operator retry one blocked policy job and confirm its exact deploy plan."""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path
from typing import Any


class ApprovalError(Exception):
    pass


def mergetrain(repo: Path, *args: str) -> dict[str, Any]:
    completed = subprocess.run(
        ["mergetrain", "--repo", str(repo), *args, "--json"],
        cwd=repo,
        capture_output=True,
        text=True,
        check=False,
    )
    if completed.returncode != 0:
        raise ApprovalError((completed.stderr or completed.stdout).strip() or "mergetrain failed")
    try:
        payload = json.loads(completed.stdout)
    except json.JSONDecodeError as error:
        raise ApprovalError(f"mergetrain returned invalid JSON: {error}") from error
    if not isinstance(payload, dict) or payload.get("contract_version") != 4:
        raise ApprovalError("mergetrain returned an unsupported contract")
    return payload


def git_diff(repo: Path, base: str, head: str) -> str:
    completed = subprocess.run(
        ["git", "-C", str(repo), "diff", "--no-ext-diff", "--unified=3",
         f"{base}..{head}", "--", ".mergetrain.yaml"],
        capture_output=True,
        text=True,
        check=False,
    )
    if completed.returncode != 0:
        raise ApprovalError("unable to show the job's policy change")
    return completed.stdout.rstrip()


def approve(repo: Path, job_id: int) -> None:
    if not sys.stdin.isatty() or not sys.stdout.isatty():
        raise ApprovalError("approval requires an interactive terminal")
    status = mergetrain(repo, "status")
    attention_ids = {job.get("id") for job in status.get("attention_jobs") or []}
    waiting_ids = {
        job.get("id") for job in status.get("recent_jobs") or []
        if job.get("state") in {"waiting", "ready"}
    }
    if job_id not in attention_ids | waiting_ids:
        raise ApprovalError(f"job #{job_id} is no longer awaiting review")
    details = mergetrain(repo, "inspect", str(job_id))
    job = details.get("job") or {}
    outcome = details.get("outcome") or {}
    if "approval_execution_policy_changed" not in str(outcome.get("message") or ""):
        raise ApprovalError("this job was blocked for a different authorization reason")
    blocked = job.get("status") == "blocked" and outcome.get("failure_category") == "deploy_authorization_changed"
    manual = job.get("status") in {"queued", "validated"} and job.get("auto_deploy") is False
    if not (blocked or manual):
        raise ApprovalError("this action requires a blocked policy job or its manual replacement")
    if blocked and (status.get("counts") or {}).get("ready", 0):
        raise ApprovalError("another train is ready; deploy or resolve it before retrying this job")
    base = str(job.get("base_sha") or "")
    head = str(job.get("head_sha") or "")
    if not base or not head:
        raise ApprovalError("job base or commit identity is missing")
    policy_diff = git_diff(repo, base, head)

    print(f"Job #{job_id}: {job.get('task') or '(unknown task)'}")
    print(f"Branch: {job.get('branch') or '(unknown)'}")
    print(f"Commit: {head}")
    print("Policy changes in this job:")
    print(policy_diff or "(none in .mergetrain.yaml)")
    if blocked:
        print("\nThis will retry the job as a manual replacement, validate the resulting train,")
    else:
        print("\nThis will validate a train containing this manual job,")
    print("then ask you to confirm the exact deploy plan. The plan may contain other jobs.")
    challenge = f"approve {job_id} {head[:12]}"
    if input(f"Type '{challenge}' to continue: ").strip() != challenge:
        print("Approval canceled; no train action was taken.")
        return

    if blocked:
        retry = mergetrain(repo, "retry", str(job_id))
        replacement = retry.get("job") or {}
        replacement_id = replacement.get("id")
        if not isinstance(replacement_id, int) or replacement.get("head_sha") != head:
            raise ApprovalError("retry returned a different commit or no replacement job")
        if replacement.get("auto_deploy") is not False:
            raise ApprovalError("replacement retained unattended approval; review its state before proceeding")
    else:
        replacement_id = job_id

    print(f"Validating replacement #{replacement_id} under the current train policy...", flush=True)
    preview = mergetrain(repo, "deploy")
    planned_ids = {item.get("id") for item in preview.get("jobs") or []}
    if preview.get("result") != "confirmation_required" or replacement_id not in planned_ids:
        raise ApprovalError(
            f"replacement #{replacement_id} is not in a ready deploy plan; inspect the train before continuing"
        )
    print(f"\nReplacement #{replacement_id} validated. Review the exact plan below.")
    deployed = subprocess.run(["mergetrain", "--repo", str(repo), "deploy"], cwd=repo, check=False)
    if deployed.returncode != 0:
        raise ApprovalError("deploy did not complete; inspect the train for its current state")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", required=True, type=Path)
    parser.add_argument("--job-id", required=True, type=int)
    args = parser.parse_args()
    try:
        approve(args.repo.resolve(), args.job_id)
    except ApprovalError as error:
        print(f"Approval stopped: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
