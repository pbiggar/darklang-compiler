"""End-to-end checks for exact candidate gate exceptions."""

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from scripts.mergetrain_exception import (
    ExceptionFlowError,
    approval_path,
    create_request,
    finalize,
    read_json,
    review_pending,
    review_path,
    run_gate,
    stage_if_requested,
    state_dir,
)


class MergetrainExceptionTests(unittest.TestCase):
    def git(self, repo: Path, *args: str) -> str:
        return subprocess.check_output(["git", *args], cwd=repo, text=True).strip()

    def test_request_approval_exact_tree_and_archive(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo = root / "dark-compiler-mergetrain-8-deadbeef"
            remote = root / "remote.git"
            fake_bin = root / "bin"
            repo.mkdir()
            fake_bin.mkdir()
            subprocess.run(["git", "init", "-q", "--bare", str(remote)], check=True)
            self.git(repo, "init", "-q", "-b", "main")
            self.git(repo, "config", "user.email", "exception-test@example.invalid")
            self.git(repo, "config", "user.name", "Exception Test")
            (repo / ".mergetrain.yaml").write_text("version: 2\n", encoding="utf-8")
            (repo / "source.txt").write_text("base\n", encoding="utf-8")
            self.git(repo, "add", ".")
            self.git(repo, "commit", "-q", "-m", "base")
            self.git(repo, "remote", "add", "mergetrain-local", str(remote))
            self.git(repo, "push", "-q", "-u", "mergetrain-local", "main")
            self.git(repo, "fetch", "-q", "mergetrain-local", "main")
            self.git(repo, "switch", "-q", "-c", "task/exception")
            (repo / "source.txt").write_text("candidate\n", encoding="utf-8")
            self.git(repo, "add", ".")
            self.git(repo, "commit", "-q", "-m", "candidate")
            head = self.git(repo, "rev-parse", "HEAD")
            tree = self.git(repo, "rev-parse", "HEAD^{tree}")
            create_request(
                repo, head=head, branch="task/exception", gate="benchmarks",
                reason="Known aggregate regression accepted for this change",
            )
            details = {
                "job": {
                    "id": 7, "status": "blocked", "branch": "task/exception",
                    "head_sha": head, "deploy_sha": head, "auto_deploy": True,
                },
                "events": [{
                    "state": "failure", "message": "Failed gate 5/5: benchmarks",
                    "detail": "exit_code=1",
                }],
            }
            self.assertTrue(stage_if_requested(repo, details))
            self.assertEqual(read_json(review_path(repo, 7))["candidate_tree"], tree)
            self.assertFalse(stage_if_requested(repo, {
                **details, "events": [{
                    "state": "failure", "message": "Failed gate 5/5: tests",
                    "detail": "exit_code=1",
                }],
            }))

            details_path = root / "details.json"
            details_path.write_text(json.dumps(details), encoding="utf-8")
            fake_mergetrain = fake_bin / "mergetrain"
            fake_mergetrain.write_text(
                "#!/usr/bin/env python3\n"
                "import json, pathlib, sys\n"
                f"details = json.loads(pathlib.Path({str(details_path)!r}).read_text())\n"
                "if 'inspect' in sys.argv:\n"
                "    job_id = sys.argv[sys.argv.index('inspect') + 1]\n"
                "    print(json.dumps(details if job_id == '7' else "
                "{'job': {'id': 8, 'status': 'deployed'}}))\n"
                "else:\n"
                "    print(json.dumps({'job': {'id': 8, 'auto_deploy': True}}))\n",
                encoding="utf-8",
            )
            fake_mergetrain.chmod(0o755)
            environment = {"PATH": f"{fake_bin}:{os.environ['PATH']}",
                           "MERGETRAIN_WORKTREE": str(repo)}
            fallback = ["python3", "-c", "raise SystemExit(7)"]
            previous_cwd = Path.cwd()
            try:
                os.chdir(repo)
                with patch.dict(os.environ, environment):
                    self.assertEqual(run_gate(repo, "benchmarks", fallback), 7)
                    with patch("sys.stdin") as stdin, patch(
                        "builtins.input", return_value=f"approve 7 {head[:12]} benchmarks"
                    ):
                        stdin.isatty.return_value = True
                        review_pending(repo)
                    self.assertFalse(review_path(repo, 7).exists())
                    self.assertEqual(run_gate(repo, "benchmarks", fallback), 0)
                    approval_file = approval_path(repo, tree, "benchmarks")
                    approved = read_json(approval_file)
                    self.assertIsNotNone(approved)
                    approval_file.write_text(json.dumps({
                        **approved, "replacement_job_id": 9,
                    }), encoding="utf-8")
                    self.assertEqual(run_gate(repo, "benchmarks", fallback), 7)
                    approval_file.write_text(json.dumps(approved), encoding="utf-8")
                    (repo / ".mergetrain.yaml").write_text(
                        "version: 2\nchanged: true\n", encoding="utf-8"
                    )
                    self.assertEqual(run_gate(repo, "benchmarks", fallback), 7)
                    (repo / ".mergetrain.yaml").write_text(
                        "version: 2\n", encoding="utf-8"
                    )
                    finalize(repo)
                    self.assertIsNone(read_json(approval_path(repo, tree, "benchmarks")))
                    self.assertTrue((state_dir(repo) / "used" / f"{tree}-benchmarks.json").is_file())
                    self.assertEqual(run_gate(repo, "benchmarks", fallback), 7)
            finally:
                os.chdir(previous_cwd)

    def test_request_rejects_ineligible_gate(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            with self.assertRaisesRegex(ExceptionFlowError, "not eligible"):
                create_request(
                    Path(temp_dir), head="deadbeef", branch="task/x",
                    gate="build", reason="skip it",
                )

    def test_status_review_selects_one_pending_job(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            repo = Path(temp_dir)
            self.git(repo, "init", "-q")
            for job_id in (7, 8):
                path = review_path(repo, job_id)
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(json.dumps({
                    "schema": 1, "job_id": job_id, "gate": "benchmarks",
                    "reason": "Expected regression",
                }), encoding="utf-8")
            with patch("sys.stdin") as stdin, patch(
                "builtins.input", return_value="8"
            ), patch("scripts.mergetrain_exception.approve") as approve_job:
                stdin.isatty.return_value = True
                review_pending(repo)
            approve_job.assert_called_once_with(repo, 8)

    def test_status_review_requires_a_terminal(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            with patch("sys.stdin") as stdin:
                stdin.isatty.return_value = False
                with self.assertRaisesRegex(ExceptionFlowError, "interactive terminal"):
                    review_pending(Path(temp_dir))


if __name__ == "__main__":
    unittest.main()
