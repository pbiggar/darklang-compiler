"""Observable FIFO dispatch tests with a fake native merge-train queue."""

import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from scripts.mergetrain_fifo import (
    DispatchError, admit_manual, load, policy_sections, prepare, retry_head,
    safe_policy_change, save,
)


class FifoDispatchTests(unittest.TestCase):
    def test_automatic_retry_renews_approval_and_preserves_fifo_and_owner(self) -> None:
        for approved in (True, False):
            with self.subTest(approved=approved), tempfile.TemporaryDirectory() as directory:
                repo = Path(directory) / "repo"
                repo.mkdir()
                subprocess.run(["git", "init", "-q", "-b", "main"], cwd=repo, check=True)
                subprocess.run(["git", "-c", "user.name=Test", "-c", "user.email=test@example.invalid",
                                "commit", "-q", "--allow-empty", "-m", "base"], cwd=repo, check=True)
                sha = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo, text=True).strip()
                original = {"id": 17, "branch": "main", "head_sha": sha, "base_sha": sha,
                            "worktree_path": str(repo), "status": "failed", "auto_deploy": False}
                state = {"schema": 1, "head": {"native_id": 17, "order": 16, "recovery_failed": "old failure"},
                         "pending": [{"order": 18}], "completed": []}
                save(repo, state)
                calls = []
                queued = {}
                def fake_train(_repo: Path, *args: str) -> dict:
                    calls.append(args)
                    if args[0] == "inspect":
                        return {"job": queued if args[1] == "21" else original}
                    if args[0] == "status":
                        return {"recent_jobs": [{"id": 21, "state": "waiting"}]}
                    if args[0] == "enqueue":
                        self.assertIn("--auto", args)
                        worktree = args[args.index("--worktree") + 1]
                        self.assertEqual(subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=worktree, text=True).strip(), sha)
                        queued.update({**original, "id": 21, "auto_deploy": approved,
                                       "branch": args[args.index("--branch") + 1],
                                       "worktree_path": worktree})
                        return {"job": queued}
                    if args[0] == "dismiss":
                        if sum(call[0] == "dismiss" for call in calls) == 1:
                            raise DispatchError("interrupted dismissal")
                        return {"ok": True}
                    raise AssertionError(args)
                with patch("scripts.mergetrain_fifo.train", side_effect=fake_train):
                    if approved:
                        with self.assertRaisesRegex(DispatchError, "interrupted dismissal"):
                            retry_head(repo, 17, auto=True, worktree_root=Path(directory) / "retries")
                        self.assertEqual(load(repo), state)
                        result = retry_head(repo, 17, auto=True, worktree_root=Path(directory) / "retries")
                        self.assertTrue(result["job"]["auto_deploy"])
                        updated = load(repo)
                        self.assertEqual(updated["head"]["native_id"], 21)
                        self.assertEqual(updated["head"]["order"], 16)
                        self.assertTrue(updated["head"]["auto"])
                        self.assertNotIn("recovery_failed", updated["head"])
                        self.assertEqual(updated["pending"], state["pending"])
                        self.assertEqual([call[0] for call in calls],
                                         ["inspect", "enqueue", "dismiss", "inspect", "status", "inspect", "dismiss"])
                    else:
                        with self.assertRaisesRegex(DispatchError, "automatic approval"):
                            retry_head(repo, 17, auto=True, worktree_root=Path(directory) / "retries")
                        self.assertEqual(load(repo), state)
                        self.assertNotIn("dismiss", [call[0] for call in calls])
                self.assertEqual(subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo, text=True).strip(), sha)
                self.assertEqual(subprocess.check_output(["git", "status", "--porcelain"], cwd=repo, text=True), "")

    def test_automatic_retry_refuses_uncertain_push_before_enqueue(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            repo = Path(directory)
            subprocess.run(["git", "init", "-q", "-b", "main"], cwd=repo, check=True)
            with patch("scripts.mergetrain_fifo.train", return_value={"job": {
                "status": "blocked", "pending_deploy_sha": "a" * 40,
            }}) as train:
                with self.assertRaisesRegex(DispatchError, "reconcile"):
                    retry_head(repo, 17, auto=True)
                train.assert_called_once_with(repo, "inspect", "17")

    def test_worktree_location_only_can_renew_approval(self) -> None:
        base = "version: 2\ngates:\n  - name: tests\n"
        relocated = base + "state:\n  worktree_root: /tmp/mergetrain-worktrees\n\n"
        self.assertTrue(safe_policy_change(policy_sections(base), policy_sections(relocated)))
        self.assertFalse(safe_policy_change(
            policy_sections(base),
            policy_sections(relocated + "  db: /tmp/other-state.db\n"),
        ))
        self.assertFalse(safe_policy_change(
            policy_sections(base),
            policy_sections(relocated + "deploy:\n  verify: []\n"),
        ))

    def test_attention_head_stays_ahead_of_later_jobs_and_replacement(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            repo = Path(directory)
            subprocess.run(["git", "init", "-q", "-b", "main"], cwd=repo, check=True)
            subprocess.run(["git", "config", "user.email", "fifo@example.invalid"], cwd=repo, check=True)
            subprocess.run(["git", "config", "user.name", "FIFO Test"], cwd=repo, check=True)
            (repo / ".mergetrain.yaml").write_text("version: 2\ngates:\n  - name: tests\n", encoding="utf-8")
            subprocess.run(["git", "add", ".mergetrain.yaml"], cwd=repo, check=True)
            subprocess.run(["git", "commit", "-q", "-m", "base"], cwd=repo, check=True)
            base = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo, text=True).strip()
            jobs = {
                number: {
                    "id": number, "status": "queued", "branch": f"task/{number}",
                    "worktree_path": str(repo), "head_sha": f"commit-{number}",
                    "base_sha": base,
                    "task": f"job {number}", "auto_deploy": True,
                }
                for number in (1, 2, 3)
            }
            jobs[3]["status"] = "blocked"

            def fake_train(_repo: Path, *args: str) -> dict:
                command = args[0]
                if command == "status":
                    state = {
                        "queued": "waiting", "in_progress": "running",
                        "validated": "ready", "blocked": "attention",
                    }
                    return {
                        "contract_version": 4,
                        "recent_jobs": [
                            {"id": item["id"], "state": state[item["status"]]}
                            for item in jobs.values() if item["status"] in state
                        ],
                    }
                if command == "cancel":
                    jobs[int(args[1])]["status"] = "canceled"
                    return {"job": jobs[int(args[1])]}
                if command == "dismiss":
                    jobs[int(args[1])]["status"] = "canceled"
                    return {"job": jobs[int(args[1])]}
                if command == "inspect":
                    return {"job": jobs[int(args[1])],
                            "outcome": {"message": "failed policy gate: tests"}}
                if command == "enqueue":
                    number = max(jobs) + 1
                    branch = args[args.index("--branch") + 1]
                    original = next(item for item in jobs.values() if item["branch"] == branch)
                    jobs[number] = {**original, "id": number, "status": "queued",
                                    "auto_deploy": "--auto" in args}
                    return {"job": jobs[number]}
                raise AssertionError(command)

            with patch("scripts.mergetrain_fifo.train", side_effect=fake_train), patch(
                "scripts.mergetrain_fifo.inspect", side_effect=lambda _repo, number: jobs[number]
            ):
                first = prepare(repo)
                self.assertEqual(first["head"]["order"], 1)
                self.assertEqual([item["order"] for item in first["pending"]], [2, 3])
                self.assertEqual([jobs[2]["status"], jobs[3]["status"]], ["canceled"] * 2)
                self.assertEqual(first["pending"][1]["prior_failure"], "failed policy gate: tests")

                jobs[1]["status"] = "blocked"
                self.assertEqual(prepare(repo)["head"]["native_id"], 1)
                jobs[1]["status"] = "canceled"
                jobs[1]["note"] = "retried as job 4"
                jobs[4] = {**jobs[1], "id": 4, "status": "queued"}
                # Simulate a crash after native retry, before the ledger update.
                self.assertEqual(prepare(repo)["head"]["native_id"], 4)
                self.assertEqual(prepare(repo)["head"]["order"], 1)
                jobs[4]["status"] = "deployed"
                jobs[4]["verify_status"] = "failed"
                self.assertEqual(prepare(repo)["head"]["order"], 1)
                jobs[4]["verify_status"] = "success"
                config = repo / ".mergetrain.yaml"
                config.write_text(config.read_text() + "verify:\n  command: changed\n", encoding="utf-8")
                subprocess.run(["git", "add", ".mergetrain.yaml"], cwd=repo, check=True)
                subprocess.run(["git", "commit", "-q", "-m", "change verify policy"], cwd=repo, check=True)
                with self.assertRaisesRegex(DispatchError, "outside safe renewal settings"):
                    prepare(repo)
                self.assertEqual(load(repo)["completed"], [
                    {"order": 1, "outcome": "deployed"}
                ])
                self.assertEqual(load(repo)["head"]["order"], 2)
                self.assertTrue(load(repo)["head"]["admitting"])
                with self.assertRaisesRegex(DispatchError, "exact FIFO head"):
                    admit_manual(repo, 2, "wrong-commit")
                admit_manual(repo, 2, "commit-2")
                second = prepare(repo)
                self.assertFalse(jobs[5]["auto_deploy"])
                config.write_text("version: 2\ngates:\n  - name: tests\n", encoding="utf-8")
                subprocess.run(["git", "add", ".mergetrain.yaml"], cwd=repo, check=True)
                subprocess.run(["git", "commit", "-q", "-m", "restore verify policy"], cwd=repo, check=True)
                self.assertEqual(second["head"]["order"], 2)
                self.assertEqual(second["head"]["native_id"], 5)
                self.assertNotIn("recovery_failed", second["head"])
                self.assertEqual([item["order"] for item in second["pending"]], [3])
                self.assertEqual(load(repo)["head"]["order"], 2)

                # A native retry creates a new row at the tail, but retains
                # the first job's original FIFO position.
                jobs[5]["status"] = "blocked"
                original_train = fake_train
                def retrying_train(_repo: Path, *args: str) -> dict:
                    if args[0] == "retry":
                        jobs[5]["status"] = "canceled"
                        jobs[6] = {**jobs[5], "id": 6, "status": "queued"}
                        return {"job": jobs[6]}
                    return original_train(_repo, *args)
                with patch("scripts.mergetrain_fifo.train", side_effect=retrying_train):
                    with self.assertRaises(DispatchError):
                        retry_head(repo, 3)
                    retry_head(repo, 5)
                self.assertEqual(load(repo)["head"]["order"], 2)
                self.assertEqual(load(repo)["head"]["native_id"], 6)


if __name__ == "__main__":
    unittest.main()
