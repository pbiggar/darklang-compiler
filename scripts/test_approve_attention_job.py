"""Exercise interactive approval before retry and exact-plan deployment."""

from __future__ import annotations

import os
import pty
import select
import subprocess
import tempfile
import time
import unittest
from pathlib import Path
from unittest.mock import patch

from scripts.approve_attention_job import ApprovalError, approve


class ApprovalTests(unittest.TestCase):
    def test_manual_successor_reaches_plan_without_retrying_again(self) -> None:
        calls: list[tuple[str, ...]] = []

        def fake_mergetrain(_repo: Path, *args: str) -> dict[str, object]:
            calls.append(args)
            if args[0] == "status":
                return {"attention_jobs": [], "recent_jobs": [{"id": 308, "state": "waiting"}],
                        "counts": {"ready": 0}}
            if args[0] == "inspect":
                return {
                    "job": {"status": "queued", "auto_deploy": False,
                            "base_sha": "a", "head_sha": "b"},
                    "outcome": {"message": "approval_execution_policy_changed"},
                }
            return {"result": "confirmation_required", "jobs": [{"id": 308}]}

        with patch("scripts.approve_attention_job.mergetrain", side_effect=fake_mergetrain), \
             patch("scripts.approve_attention_job.git_diff", return_value="policy diff"), \
             patch("scripts.approve_attention_job.sys.stdin.isatty", return_value=True), \
             patch("scripts.approve_attention_job.sys.stdout.isatty", return_value=True), \
             patch("builtins.input", return_value="approve 308 b"), \
             patch("scripts.approve_attention_job.subprocess.run") as deploy:
            deploy.return_value.returncode = 0
            approve(Path("."), 308)

        self.assertEqual(calls, [("status",), ("inspect", "308"), ("deploy",)])
        self.assertEqual(deploy.call_args.args[0][-1], "deploy")

    def test_declining_challenge_keeps_job_untouched(self) -> None:
        calls: list[tuple[str, ...]] = []

        def fake_mergetrain(_repo: Path, *args: str) -> dict[str, object]:
            calls.append(args)
            if args[0] == "status":
                return {"attention_jobs": [{"id": 299}], "counts": {"ready": 0}}
            return {
                "job": {"status": "blocked", "base_sha": "a", "head_sha": "b"},
                "outcome": {"failure_category": "deploy_authorization_changed",
                            "message": "approval_execution_policy_changed"},
            }

        with patch("scripts.approve_attention_job.mergetrain", side_effect=fake_mergetrain), \
             patch("scripts.approve_attention_job.git_diff", return_value="policy diff"), \
             patch("scripts.approve_attention_job.sys.stdin.isatty", return_value=True), \
             patch("scripts.approve_attention_job.sys.stdout.isatty", return_value=True), \
             patch("builtins.input", return_value="no"):
            approve(Path("."), 299)

        self.assertEqual(calls, [("status",), ("inspect", "299")])

    def test_missing_replacement_in_plan_stops_before_deploy(self) -> None:
        calls: list[tuple[str, ...]] = []

        def fake_mergetrain(_repo: Path, *args: str) -> dict[str, object]:
            calls.append(args)
            if args[0] == "status":
                return {"attention_jobs": [{"id": 299}], "counts": {"ready": 0}}
            if args[0] == "inspect":
                return {
                    "job": {"status": "blocked", "base_sha": "a", "head_sha": "b"},
                    "outcome": {"failure_category": "deploy_authorization_changed",
                                "message": "approval_execution_policy_changed"},
                }
            if args[0] == "retry":
                return {"job": {"id": 300, "head_sha": "b", "auto_deploy": False}}
            return {"result": "confirmation_required", "jobs": [{"id": 400}]}

        with patch("scripts.approve_attention_job.mergetrain", side_effect=fake_mergetrain), \
             patch("scripts.approve_attention_job.git_diff", return_value="policy diff"), \
             patch("scripts.approve_attention_job.sys.stdin.isatty", return_value=True), \
             patch("scripts.approve_attention_job.sys.stdout.isatty", return_value=True), \
             patch("builtins.input", return_value="approve 299 b"), \
             patch("scripts.approve_attention_job.subprocess.run") as deploy:
            with self.assertRaisesRegex(ApprovalError, "not in a ready deploy plan"):
                approve(Path("."), 299)

        self.assertEqual(calls, [("status",), ("inspect", "299"), ("retry", "299"), ("deploy",)])
        deploy.assert_not_called()

    def test_operator_confirms_job_then_exact_deploy_plan(self) -> None:
        source_root = Path(__file__).resolve().parent.parent
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo = root / "repo"
            fake_bin = root / "bin"
            repo.mkdir()
            fake_bin.mkdir()
            subprocess.run(["git", "init", "-q", "-b", "main"], cwd=repo, check=True)
            subprocess.run(["git", "config", "user.email", "approval-test@example.invalid"], cwd=repo, check=True)
            subprocess.run(["git", "config", "user.name", "Approval Test"], cwd=repo, check=True)
            config = repo / ".mergetrain.yaml"
            config.write_text("version: 2\ngates: []\n", encoding="utf-8")
            subprocess.run(["git", "add", ".mergetrain.yaml"], cwd=repo, check=True)
            subprocess.run(["git", "commit", "-q", "-m", "base"], cwd=repo, check=True)
            base = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo, text=True).strip()
            config.write_text("version: 2\ngates:\n  - name: tests\n    run: ./run-tests --ai\n", encoding="utf-8")
            subprocess.run(["git", "add", ".mergetrain.yaml"], cwd=repo, check=True)
            subprocess.run(["git", "commit", "-q", "-m", "policy"], cwd=repo, check=True)
            head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo, text=True).strip()

            fake = fake_bin / "mergetrain"
            fake.write_text(
                "#!/usr/bin/env python3\n"
                "import json, pathlib, sys\n"
                "root = pathlib.Path(__file__).parent\n"
                "command = next(arg for arg in sys.argv if arg in {'status', 'inspect', 'retry', 'deploy'})\n"
                "with (root / 'calls').open('a') as stream:\n"
                "    stream.write(command + ('-json' if '--json' in sys.argv else '') + '\\n')\n"
                "if command == 'status':\n"
                "    print(json.dumps({'contract_version': 4, 'attention_jobs': [{'id': 299}],\n"
                "                      'counts': {'ready': 0}}))\n"
                "elif command == 'inspect':\n"
                "    print(json.dumps({'contract_version': 4, 'job': {'id': 299, 'status': 'blocked',\n"
                "        'task': 'Review gate waivers', 'branch': 'task/review',\n"
                f"        'base_sha': '{base}', 'head_sha': '{head}'"
                "}, 'outcome': {'failure_category': 'deploy_authorization_changed',\n"
                "                     'message': 'approval_execution_policy_changed'}}))\n"
                "elif command == 'retry':\n"
                "    print(json.dumps({'contract_version': 4, 'job': {'id': 300,\n"
                f"        'head_sha': '{head}', 'auto_deploy': False"
                "}}))\n"
                "elif '--json' in sys.argv:\n"
                "    print(json.dumps({'contract_version': 4, 'result': 'confirmation_required',\n"
                "                      'jobs': [{'id': 300}]}))\n"
                "else:\n"
                "    print('Deploy this exact plan? [y/N] ', end='', flush=True)\n"
                "    answer = input().strip().lower()\n"
                "    with (root / 'calls').open('a') as stream:\n"
                "        stream.write('approved\\n' if answer == 'y' else 'declined\\n')\n",
                encoding="utf-8",
            )
            fake.chmod(0o755)
            environment = dict(os.environ)
            environment["PATH"] = f"{fake_bin}:{environment['PATH']}"
            master, slave = pty.openpty()
            process = subprocess.Popen(
                ["python3", str(source_root / "scripts" / "approve_attention_job.py"),
                 "--repo", str(repo), "--job-id", "299"],
                cwd=repo, env=environment, stdin=slave, stdout=slave, stderr=slave,
                close_fds=True,
            )
            os.close(slave)

            def read_until(expected: bytes) -> bytes:
                deadline = time.monotonic() + 10
                output = b""
                while expected not in output and time.monotonic() < deadline:
                    readable, _, _ = select.select([master], [], [], 0.2)
                    if readable:
                        output += os.read(master, 65536)
                self.assertIn(expected, output)
                return output

            try:
                first = read_until(f"Type 'approve 299 {head[:12]}'".encode())
                self.assertIn(b"+  - name: tests", first)
                self.assertNotIn("retry-json", (fake_bin / "calls").read_text(encoding="utf-8"))
                os.write(master, f"approve 299 {head[:12]}\n".encode())
                read_until(b"Deploy this exact plan? [y/N]")
                calls = (fake_bin / "calls").read_text(encoding="utf-8")
                self.assertLess(calls.index("retry-json"), calls.index("deploy-json"))
                self.assertNotIn("approved", calls)
                os.write(master, b"y\n")
                self.assertEqual(process.wait(timeout=10), 0)
                self.assertIn("approved", (fake_bin / "calls").read_text(encoding="utf-8"))
            finally:
                if process.poll() is None:
                    process.terminate()
                    process.wait(timeout=10)
                os.close(master)


if __name__ == "__main__":
    unittest.main()
