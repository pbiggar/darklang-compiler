#!/usr/bin/env python3
"""Keep ordinary merge-train jobs in arrival order across native retries."""

from __future__ import annotations

import argparse
import fcntl
import json
import os
import re
import subprocess
import sys
from pathlib import Path
from typing import Any


class DispatchError(RuntimeError):
    pass


def git(repo: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", "-C", str(repo), *args], capture_output=True, text=True, check=False
    )
    if result.returncode:
        raise DispatchError(result.stderr.strip() or "git failed")
    return result.stdout.strip()


def ledger_path(repo: Path) -> Path:
    common = Path(git(repo, "rev-parse", "--git-common-dir"))
    if not common.is_absolute():
        common = repo / common
    return common.resolve() / "mergetrain-fifo.json"


def load(repo: Path) -> dict[str, Any]:
    try:
        state = json.loads(ledger_path(repo).read_text(encoding="utf-8"))
    except FileNotFoundError:
        return {"schema": 1, "head": None, "pending": [], "completed": []}
    except (OSError, json.JSONDecodeError) as error:
        raise DispatchError(f"cannot read FIFO ledger: {error}") from error
    if not isinstance(state, dict) or state.get("schema") != 1:
        raise DispatchError("unsupported FIFO ledger schema")
    if not isinstance(state.get("pending"), list):
        raise DispatchError("invalid FIFO ledger pending jobs")
    return state


def save(repo: Path, state: dict[str, Any]) -> None:
    path = ledger_path(repo)
    temporary = path.with_name(f".{path.name}.{os.getpid()}.tmp")
    temporary.write_text(json.dumps(state, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    temporary.replace(path)


def train(repo: Path, *args: str) -> dict[str, Any]:
    result = subprocess.run(
        ["mergetrain", "--repo", str(repo), *args, "--json"],
        capture_output=True, text=True, check=False,
    )
    if result.returncode:
        raise DispatchError(result.stderr.strip() or result.stdout.strip() or "mergetrain failed")
    try:
        payload = json.loads(result.stdout)
    except json.JSONDecodeError as error:
        raise DispatchError(f"mergetrain returned invalid JSON: {error}") from error
    if not isinstance(payload, dict):
        raise DispatchError("mergetrain returned non-object JSON")
    return payload


def inspect(repo: Path, job_id: int) -> dict[str, Any]:
    return train(repo, "inspect", str(job_id)).get("job") or {}


def active_ids(snapshot: dict[str, Any]) -> list[int]:
    jobs = snapshot.get("recent_jobs", [])
    active = {
        job["id"] for job in jobs
        if isinstance(job.get("id"), int)
        and job.get("state") in {"waiting", "running", "ready", "attention"}
    }
    active.update(
        job["id"] for job in snapshot.get("attention_jobs", [])
        if isinstance(job.get("id"), int)
    )
    return sorted(active)


def entry(job_id: int, job: dict[str, Any]) -> dict[str, Any]:
    if not all(job.get(field) for field in ("branch", "worktree_path", "head_sha")):
        raise DispatchError(f"job #{job_id} lacks branch, worktree, or exact commit")
    return {
        "order": job_id,
        "native_id": job_id,
        "task": str(job.get("task") or f"job {job_id}"),
        "branch": str(job["branch"]),
        "worktree": str(job["worktree_path"]),
        "head_sha": str(job["head_sha"]),
        "base_sha": str(job.get("base_sha") or ""),
        "auto": job.get("auto_deploy") is True,
    }


def policy_sections(source: str) -> dict[str, str]:
    sections: dict[str, list[str]] = {"(preamble)": []}
    current = "(preamble)"
    for line in source.splitlines(keepends=True):
        heading = re.match(r"^([A-Za-z][A-Za-z0-9_-]*):", line)
        if heading:
            current = heading.group(1)
            sections[current] = []
        sections[current].append(line)
    return {name: "".join(lines) for name, lines in sections.items()}


def safe_policy_change(before: dict[str, str], after: dict[str, str]) -> bool:
    """Allow gate changes and a standalone runner worktree location change."""
    changed = {
        key for key in before.keys() | after.keys() if before.get(key) != after.get(key)
    }
    if "state" in changed:
        def worktree_root_only(section: str | None) -> bool:
            if section is None:
                return True
            lines = [line for line in section.splitlines() if line.strip()]
            return (
                len(lines) == 2
                and lines[0] == "state:"
                and lines[1].startswith("  worktree_root: ")
                and bool(lines[1].removeprefix("  worktree_root: ").strip())
            )

        if not (worktree_root_only(before.get("state")) and
                worktree_root_only(after.get("state"))):
            return False
        changed.remove("state")
    return changed <= {"gates", "gate_parallelism"}


def auto_approval_still_safe(repo: Path, deferred: dict[str, Any]) -> bool:
    """Renew approval only when changed settings are covered by the new run."""
    if not deferred["auto"]:
        return False
    base = deferred.get("base_sha")
    if not base:
        raise DispatchError(f"job #{deferred['order']} lacks its approval base")
    current = git(repo, "rev-parse", "main")
    before = policy_sections(git(repo, "show", f"{base}:.mergetrain.yaml"))
    after = policy_sections(git(repo, "show", f"{current}:.mergetrain.yaml"))
    if not safe_policy_change(before, after):
        raise DispatchError(
            f"job #{deferred['order']} policy changed outside safe renewal settings; "
            "operator manual admission required: "
            f"python3 scripts/mergetrain_fifo.py --repo . manual "
            f"{deferred['order']} {deferred['head_sha']}"
        )
    return True


def prepare(repo: Path) -> dict[str, Any]:
    """Admit only the oldest unfinished ordinary job to the native daemon."""
    state = load(repo)
    snapshot = train(repo, "status", "--limit", "1000")
    if snapshot.get("contract_version") != 4:
        raise DispatchError("unsupported mergetrain contract version")
    current = state.get("head")
    if current and not current.get("admitting"):
        job = inspect(repo, int(current["native_id"]))
        status = job.get("status")
        if status == "deployed" and job.get("verify_status") not in {"failed", "unknown"}:
            state.setdefault("completed", []).append({
                "order": current["order"], "outcome": "deployed",
            })
            state["completed"] = state["completed"][-100:]
            state["head"] = None
            save(repo, state)
            current = None
        elif status == "canceled":
            note = str(job.get("note") or "")
            successor = re.fullmatch(r"retried as job (\d+)", note) or re.fullmatch(
                r"replaced by job #(\d+) at [0-9a-f]{12}", note
            )
            if successor is None:
                raise DispatchError(f"FIFO head #{current['order']} was canceled without landing")
            successor_id = int(successor.group(1))
            successor_job = inspect(repo, successor_id)
            if successor_job.get("status") not in {"queued", "validated", "in_progress", "blocked", "failed"}:
                raise DispatchError(f"FIFO successor #{successor_id} is not active")
            current.update({
                "native_id": successor_id,
                "branch": successor_job["branch"],
                "worktree": successor_job["worktree_path"],
                "head_sha": successor_job["head_sha"],
                "base_sha": successor_job.get("base_sha") or "",
                "auto": successor_job.get("auto_deploy") is True,
            })
            current.pop("recovery_failed", None)
            save(repo, state)

    ids = active_ids(snapshot)
    counts = snapshot.get("counts") or {}
    if all(isinstance(counts.get(key), int) for key in ("waiting", "running", "ready", "attention")):
        expected = sum(counts[key] for key in ("waiting", "running", "ready", "attention"))
        if expected > len(ids):
            raise DispatchError("status omitted active jobs; cannot prove FIFO order")
    known = {int(item["order"]) for item in state["pending"]}
    if current:
        known.add(int(current["order"]))
        known.add(int(current["native_id"]))
    fresh = [job_id for job_id in ids if job_id not in known]
    if current is None:
        earliest_pending = min(state["pending"], key=lambda item: int(item["order"]), default=None)
        earliest_fresh = fresh[0] if fresh else None
        if earliest_pending is not None and (
            earliest_fresh is None or int(earliest_pending["order"]) < earliest_fresh
        ):
            current = earliest_pending
            state["pending"].remove(current)
            current["admitting"] = True
            state["head"] = current
            save(repo, state)
        elif earliest_fresh is not None:
            current = entry(earliest_fresh, inspect(repo, earliest_fresh))
            state["head"] = current
            fresh.remove(earliest_fresh)
            save(repo, state)

    if current and current.get("admitting"):
        candidates = []
        for job_id in fresh:
            native = inspect(repo, job_id)
            if (native.get("branch") == current["branch"]
                    and native.get("head_sha") == current["head_sha"]):
                candidates.append(job_id)
        if len(candidates) > 1:
            raise DispatchError(f"multiple admission rows for FIFO job #{current['order']}")
        if candidates:
            current["native_id"] = candidates[0]
            fresh.remove(candidates[0])
        else:
            args = ["enqueue", "--task", current["task"], "--branch", current["branch"],
                    "--worktree", current["worktree"]]
            try:
                if auto_approval_still_safe(repo, current):
                    args.append("--auto")
            except DispatchError as error:
                current["recovery_failed"] = str(error)
                save(repo, state)
                raise
            replacement = train(repo, *args).get("job") or {}
            if replacement.get("head_sha") != current["head_sha"]:
                raise DispatchError(f"deferred job #{current['order']} changed commit")
            current["native_id"] = int(replacement["id"])
        current.pop("admitting", None)
        current.pop("recovery_failed", None)
        save(repo, state)

    for job_id in fresh:
        details = train(repo, "inspect", str(job_id))
        job = details.get("job") or {}
        status = str(job.get("status") or "")
        if status == "in_progress":
            raise DispatchError(f"later job #{job_id} is already running; stop the other runner")
        deferred = entry(job_id, job)
        if status in {"blocked", "failed"}:
            deferred["prior_failure"] = str((details.get("outcome") or {}).get("message") or "")
        state["pending"].append(deferred)
        state["pending"].sort(key=lambda item: int(item["order"]))
        save(repo, state)  # Record intent before canceling a native queue row.
        if status in {"queued", "validated"}:
            train(repo, "cancel", str(job_id), "--note", "deferred behind FIFO head")
        elif status in {"blocked", "failed"}:
            train(repo, "dismiss", str(job_id), "--note", "deferred behind FIFO head")
        elif status != "canceled":
            raise DispatchError(f"cannot defer job #{job_id} in state {status}")

    # A crash after recording deferral but before cancellation is replayable.
    for deferred in state["pending"]:
        native = inspect(repo, int(deferred["native_id"]))
        status = native.get("status")
        if status in {"queued", "validated"}:
            train(repo, "cancel", str(deferred["native_id"]), "--note", "deferred behind FIFO head")
        elif status in {"blocked", "failed"}:
            train(repo, "dismiss", str(deferred["native_id"]), "--note", "deferred behind FIFO head")
        elif status == "in_progress":
            raise DispatchError(f"later job #{deferred['order']} is running")
        elif status == "deployed":
            raise DispatchError(f"later job #{deferred['order']} landed ahead of FIFO head")

    save(repo, state)
    return state


def note_replacement(repo: Path, old_id: int, new_id: int) -> None:
    state = load(repo)
    head = state.get("head")
    if head is None:
        return  # Recovery can also run outside the FIFO integrator.
    if int(head["native_id"]) != old_id:
        raise DispatchError(f"replacement of #{old_id} is not the FIFO head")
    head["native_id"] = new_id
    head.pop("recovery_failed", None)
    save(repo, state)


def note_recovery_failure(repo: Path, job_id: int, reason: str) -> None:
    state = load(repo)
    head = state.get("head")
    if head is not None and int(head["native_id"]) == job_id:
        head["recovery_failed"] = reason
        save(repo, state)


def note_resolved(repo: Path, job_id: int, outcome: str) -> None:
    state = load(repo)
    head = state.get("head")
    if head is None:
        return
    if int(head["native_id"]) != job_id:
        raise DispatchError(f"resolved job #{job_id} is not the FIFO head")
    state.setdefault("completed", []).append({"order": head["order"], "outcome": outcome})
    state["completed"] = state["completed"][-100:]
    state["head"] = None
    save(repo, state)


def retry_head(repo: Path, job_id: int) -> dict[str, Any]:
    state = load(repo)
    head = state.get("head")
    if head is None and state.get("pending"):
        raise DispatchError("a deferred FIFO job must be admitted before retry")
    if head is not None and int(head["native_id"]) != job_id:
        raise DispatchError(f"job #{job_id} is behind FIFO head #{head['order']}")
    result = train(repo, "retry", str(job_id))
    replacement = result.get("replacement") or result.get("job") or {}
    replacement_id = int(replacement.get("id") or 0)
    if replacement_id <= 0:
        raise DispatchError("retry returned no replacement job ID")
    note_replacement(repo, job_id, replacement_id)
    return result


def admit_manual(repo: Path, order: int, head_sha: str) -> None:
    """Let an operator stage a deferred head for native exact-plan review."""
    state = load(repo)
    head = state.get("head")
    if head is None or int(head["order"]) != order or head.get("head_sha") != head_sha:
        raise DispatchError("manual admission does not match the exact FIFO head")
    if not head.get("admitting") or not head.get("recovery_failed"):
        raise DispatchError("FIFO head is not paused for admission")
    head["auto"] = False
    head["recovery_failed"] = "manual validation and exact deploy-plan confirmation required"
    save(repo, state)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", type=Path, required=True)
    sub = parser.add_subparsers(dest="action", required=True)
    sub.add_parser("prepare")
    sub.add_parser("show")
    replacement = sub.add_parser("replacement")
    replacement.add_argument("old_id", type=int)
    replacement.add_argument("new_id", type=int)
    failure = sub.add_parser("failure")
    failure.add_argument("job_id", type=int)
    failure.add_argument("reason")
    resolved = sub.add_parser("resolved")
    resolved.add_argument("job_id", type=int)
    resolved.add_argument("outcome")
    retry = sub.add_parser("retry")
    retry.add_argument("job_id", type=int)
    manual = sub.add_parser("manual")
    manual.add_argument("order", type=int)
    manual.add_argument("head_sha")
    args = parser.parse_args()
    try:
        repo = args.repo.resolve()
        if args.action == "prepare":
            result = prepare(repo)
        elif args.action == "replacement":
            note_replacement(repo, args.old_id, args.new_id)
            result = load(repo)
        elif args.action == "failure":
            note_recovery_failure(repo, args.job_id, args.reason)
            result = load(repo)
        elif args.action == "resolved":
            note_resolved(repo, args.job_id, args.outcome)
            result = load(repo)
        elif args.action == "retry":
            with (ledger_path(repo).parent / "mergetrain-dispatch.lock").open("a") as lock:
                fcntl.flock(lock, fcntl.LOCK_EX)
                result = retry_head(repo, args.job_id)
        elif args.action == "manual":
            with (ledger_path(repo).parent / "mergetrain-dispatch.lock").open("a") as lock:
                fcntl.flock(lock, fcntl.LOCK_EX)
                admit_manual(repo, args.order, args.head_sha)
                result = load(repo)
        else:
            result = load(repo)
        if args.action == "retry":
            replacement = result.get("replacement") or result.get("job") or {}
            print(f"retried job {args.job_id} as {replacement['id']}")
        else:
            print(json.dumps(result))
    except (DispatchError, OSError, KeyError, TypeError, ValueError) as error:
        print(f"FIFO dispatch: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
