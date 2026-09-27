"""test_mergetrain_status.py - E2E tests for the repository status summary."""

import json
import os
import pty
import select
import subprocess
import tempfile
import time
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest.mock import MagicMock, patch

from scripts.render_mergetrain_status import (
    benchmark_changes,
    benchmark_changes_for_commit,
    benchmark_detail,
    benchmark_diff,
    human_age,
    last_test_runtime,
    merge_test_runtime,
    percentage,
    render,
    render_attention_job,
    wrap_display,
)


class MergetrainStatusTests(unittest.TestCase):
    def test_attention_view_shows_policy_diff_and_confirms_retry(self) -> None:
        source_root = Path(__file__).resolve().parent.parent
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo = root / "repo"
            fake_bin = root / "bin"
            repo.mkdir()
            fake_bin.mkdir()
            subprocess.run(["git", "init", "-q", "-b", "main"], cwd=repo, check=True)
            subprocess.run(["git", "config", "user.email", "status-test@example.invalid"], cwd=repo, check=True)
            subprocess.run(["git", "config", "user.name", "Status Test"], cwd=repo, check=True)
            config = repo / ".mergetrain.yaml"
            config.write_text("version: 2\ngates:\n  - name: build\n    run: ./build --ai\n", encoding="utf-8")
            subprocess.run(["git", "add", ".mergetrain.yaml"], cwd=repo, check=True)
            subprocess.run(["git", "commit", "-q", "-m", "old policy"], cwd=repo, check=True)
            base = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo, text=True).strip()
            with config.open("a", encoding="utf-8") as stream:
                stream.write("  - name: tests\n    run: ./run-tests --ai\n")
            subprocess.run(["git", "add", ".mergetrain.yaml"], cwd=repo, check=True)
            subprocess.run(["git", "commit", "-q", "-m", "new policy"], cwd=repo, check=True)

            details = {
                "contract_version": 4,
                "job": {"id": 299, "task": "Review gate waivers", "branch": "task/review",
                        "base_sha": base, "head_sha": base, "status": "blocked"},
                "outcome": {"failure_category": "deploy_authorization_changed",
                            "message": "approval_execution_policy_changed: policy changed"},
                "events": [],
            }
            details_path = root / "details.json"
            details_path.write_text(json.dumps(details), encoding="utf-8")
            calls_path = root / "calls"
            fake = fake_bin / "mergetrain"
            fake.write_text(
                "#!/usr/bin/env python3\n"
                "import json, os, pathlib, sys\n"
                "command = next(arg for arg in sys.argv if arg in {'status', 'inspect', 'retry'})\n"
                "with pathlib.Path(os.environ['STATUS_TEST_CALLS']).open('a') as calls:\n"
                "    calls.write(command + '\\n')\n"
                "if command == 'inspect':\n"
                "    details = json.loads(pathlib.Path(os.environ['STATUS_TEST_DETAILS']).read_text())\n"
                "    if '308' in sys.argv:\n"
                "        details['job'].update(id=308, status='queued', auto_deploy=False)\n"
                "    print(json.dumps(details))\n"
                "elif command == 'retry':\n"
                "    print('retried job 299 as 300')\n"
                "else:\n"
                "    print(json.dumps({'contract_version': 4, 'health': 'healthy',\n"
                "        'state': 'attention', 'summary': '1 job needs attention',\n"
                "        'next_action': {'code': 'fix_blocked_job', 'command': 'mergetrain inspect 299',\n"
                "                        'requires_approval': 'none'}, 'warnings': [],\n"
                "        'attention_jobs': [{'id': 299, 'state': 'attention', 'task': 'Review gate waivers',\n"
                "                            'branch': 'task/review', 'reason': 'approval_execution_policy_changed: policy changed'}],\n"
                "        'recent_jobs': [{'id': 308, 'state': 'waiting', 'task': 'Review gate waivers',\n"
                "                         'branch': 'task/review'}]}))\n",
                encoding="utf-8",
            )
            fake.chmod(0o755)
            environment = dict(os.environ)
            environment["PATH"] = f"{fake_bin}:{environment['PATH']}"
            environment["STATUS_TEST_CALLS"] = str(calls_path)
            environment["STATUS_TEST_DETAILS"] = str(details_path)
            with patch.dict(os.environ, environment):
                detail = render_attention_job(repo, 299, color=False, attempt_dir=root)
            self.assertIn("+  - name: tests", detail)
            self.assertIn("job changes .mergetrain.yaml: no", detail)
            (root / f"299-{base}.policy.json").write_text(
                json.dumps({
                    "job_policy_diff": "",
                    "integration_policy_diff": "+  - name: tests\n",
                    "policy_validation_result": "failed",
                    "policy_validation_failure": "gate tests failed",
                    "policy_validation_log": str(root / "gate.log"),
                }), encoding="utf-8",
            )
            with patch.dict(os.environ, environment):
                recorded_detail = render_attention_job(repo, 299, color=False, attempt_dir=root)
            self.assertIn("failed policy gate: gate tests failed", recorded_detail)
            self.assertIn("policy validation log:", recorded_detail)

            master, slave = pty.openpty()
            process = subprocess.Popen(
                [str(source_root / "mergetrain-status"), "--repo", str(repo),
                 "--interval", "30", "--color", "never"],
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
                self.assertIn(b"[a] attention", read_until(b"health: healthy"))
                os.write(master, b"v")
                approval_page = read_until(b"[A] approve & deploy")
                self.assertIn(b"APPROVAL 2/2  #308 Review gate waivers", approval_page)
                self.assertIn(b"APPROVAL", approval_page)
                os.write(master, b"j")
                self.assertIn(
                    b"APPROVAL 2/2  #308 Review gate waivers",
                    read_until(b"[A] approve & deploy"),
                )
                os.write(master, b"A")
                read_until(f"Type 'approve 308 {base[:12]}'".encode())
                self.assertNotIn("retry\n", calls_path.read_text(encoding="utf-8"))
                os.write(master, b"no\n")
                read_until(b"Press any key to return to merge-train status")
                os.write(master, b"x")
                read_until(b"[a] attention")
                os.write(master, b"a")
                attention_page = read_until(b"[A] approve & deploy")
                self.assertIn(b"ATTENTION 1/2  #299 Review gate waivers", attention_page)
                os.write(master, b"j")
                self.assertIn(
                    b"ATTENTION 1/2  #299 Review gate waivers",
                    read_until(b"[A] approve & deploy"),
                )
                os.write(master, b"A")
                read_until(f"Type 'approve 299 {base[:12]}'".encode())
                self.assertNotIn("retry\n", calls_path.read_text(encoding="utf-8"))
                os.write(master, b"no\n")
                read_until(b"Press any key to return to merge-train status")
                os.write(master, b"x")
                read_until(b"[a] attention")
                os.write(master, b"a")
                read_until(b"[r] retry")
                os.write(master, b"r")
                read_until(b"Retry #299? Approval may become manual.")
                self.assertNotIn("retry\n", calls_path.read_text(encoding="utf-8"))
                os.write(master, b"y")
                read_until(b"retried job 299 as 300")
                self.assertIn("retry\n", calls_path.read_text(encoding="utf-8"))
                os.write(master, b"q")
                self.assertEqual(process.wait(timeout=10), 0)
            finally:
                if process.poll() is None:
                    process.terminate()
                    process.wait(timeout=10)
                os.close(master)

            subprocess.run(["git", "switch", "-q", "-c", "task/policy", base], cwd=repo, check=True)
            with config.open("a", encoding="utf-8") as stream:
                stream.write("  - name: runtime\n    run: ./runtime-check\n")
            subprocess.run(["git", "add", ".mergetrain.yaml"], cwd=repo, check=True)
            subprocess.run(["git", "commit", "-q", "-m", "job policy"], cwd=repo, check=True)
            details["job"]["head_sha"] = subprocess.check_output(
                ["git", "rev-parse", "HEAD"], cwd=repo, text=True
            ).strip()
            details_path.write_text(json.dumps(details), encoding="utf-8")
            with patch.dict(os.environ, environment):
                job_detail = render_attention_job(repo, 299, color=False, attempt_dir=root)
            self.assertIn("policy changes in this job:", job_detail)
            self.assertIn("+  - name: runtime", job_detail)

    def test_human_age_uses_compact_units(self) -> None:
        now = datetime(2026, 9, 15, 12, 0, tzinfo=timezone.utc)

        self.assertEqual(human_age(now - timedelta(seconds=12), now=now), "12s")
        self.assertEqual(human_age(now - timedelta(minutes=5), now=now), "5m")
        self.assertEqual(human_age(now - timedelta(hours=2), now=now), "2h")
        self.assertEqual(human_age(now - timedelta(days=3), now=now), "3d")

    def test_tiny_benchmark_changes_render_as_approximately_zero(self) -> None:
        self.assertEqual(percentage(-0.009), "~0%")
        self.assertEqual(percentage(0.009), "~0%")
        self.assertEqual(percentage(-0.01), "-0.01%")

    @patch("scripts.render_mergetrain_status.git_file")
    def test_benchmark_detail_data_contains_only_changed_workloads(
        self, mock_git_file: MagicMock
    ) -> None:
        identity = (
            "**Architecture:** `arm64`\n"
            "**Profile:** `full`\n"
            "**Measurement policy:** `test`\n"
            "**Workload contract:** `test`\n"
        )
        previous = (
            identity
            + "| Benchmark | Dark (2.0x) | Rust |\n"
            + "|---|---:|---:|\n"
            + "| faster | 200 (2.0x) | 100 |\n"
            + "| slower | 100 (1.0x) | 100 |\n"
            + "| same | 100 (1.0x) | 100 |\n"
        )
        current = (
            identity
            + "| Benchmark | Dark (1.9x) | Rust |\n"
            + "|---|---:|---:|\n"
            + "| faster | 150 (1.5x) | 100 |\n"
            + "| slower | 120 (1.2x) | 100 |\n"
            + "| same | 100 (1.0x) | 100 |\n"
        )
        mock_git_file.side_effect = lambda _repo, revision: (
            current if revision == "current" else previous
        )

        comparison = benchmark_changes_for_commit(Path("."), "current")

        self.assertNotIsInstance(comparison, str)
        assert not isinstance(comparison, str)
        self.assertEqual(comparison.total_benchmarks, 3)
        self.assertEqual(len(comparison.workload_changes), 2)
        improvement = comparison.workload_changes[0]
        self.assertEqual(improvement.name, "faster")
        self.assertEqual(improvement.previous_instructions, 200)
        self.assertEqual(improvement.current_instructions, 150)
        self.assertEqual(improvement.saved_instructions, 50)
        self.assertEqual(improvement.change, -25.0)
        self.assertEqual(improvement.previous_ratio, 2.0)
        self.assertEqual(improvement.current_ratio, 1.5)
        disimprovement = comparison.workload_changes[1]
        self.assertEqual(disimprovement.name, "slower")
        self.assertAlmostEqual(disimprovement.change, 20.0)
        self.assertEqual(disimprovement.saved_instructions, -20)

        with patch("scripts.render_mergetrain_status.git", return_value="test"):
            detail = benchmark_detail(Path("."), "current", color=True)
        self.assertIn("faster", detail)
        self.assertIn("slower", detail)
        self.assertNotIn("same ", detail)
        self.assertIn("\x1b[32m", detail)
        self.assertIn("\x1b[31m", detail)

    @patch("scripts.render_mergetrain_status.git", return_value="")
    def test_benchmark_history_requests_latest_ten(
        self, mock_git: MagicMock
    ) -> None:
        self.assertEqual(benchmark_changes(Path(".")), [])
        self.assertIn("-10", mock_git.call_args.args)

    def test_wrap_display_counts_colored_and_wide_terminal_cells(self) -> None:
        wrapped = wrap_display("\x1b[32mabcdef\x1b[0m 中文", 4)
        self.assertEqual(wrapped, "\x1b[32mabcd\nef\x1b[0m \n中文")

    @patch("scripts.render_mergetrain_status.subprocess.run")
    def test_runtime_uses_latest_successful_tests_gate(
        self, mock_run: MagicMock
    ) -> None:
        mock_run.return_value.returncode = 0
        mock_run.return_value.stdout = json.dumps(
            {"items": [
                {"gates": [
                    {"name": "tests", "state": "success", "duration_seconds": 42.5,
                     "finished_at": "2026-09-27T10:00:00+00:00"},
                    {"name": "tests", "state": "reused", "duration_seconds": 0,
                     "finished_at": "2026-09-27T11:00:00+00:00"},
                ]},
                {"gates": [
                    {"name": "tests", "state": "success", "duration_seconds": 41.2,
                     "finished_at": "2026-09-27T09:00:00+00:00"},
                ]},
            ]}
        )
        self.assertEqual(
            last_test_runtime(Path(".")),
            (42.5, "2026-09-27T10:00:00+00:00"),
        )

    def test_merge_runtime_matches_its_deployed_branch_and_time(self) -> None:
        history = [
            {"status": "deployed", "finished_at": "2026-09-27T11:00:00+00:00",
             "jobs": [{"branch": "task/one"}],
             "gates": [{"name": "tests", "state": "success", "duration_seconds": 42.5}]},
            {"status": "deployed", "finished_at": "2026-09-27T13:00:00+00:00",
             "jobs": [{"branch": "task/two"}],
             "gates": [{"name": "tests", "state": "success", "duration_seconds": 51.2}]},
        ]
        self.assertEqual(
            merge_test_runtime("task/one", "2026-09-27T10:00:00+00:00", history),
            "tests 42.5s",
        )
        self.assertEqual(
            merge_test_runtime("task/two", "2026-09-27T12:00:00+00:00", history),
            "tests 51.2s",
        )
        self.assertEqual(
            merge_test_runtime("task/three", "2026-09-27T12:00:00+00:00", history),
            "tests n/a",
        )

    def test_interactive_keys_respond_while_status_refresh_is_slow(self) -> None:
        source_root = Path(__file__).resolve().parent.parent
        with tempfile.TemporaryDirectory() as temp_dir:
            fake_bin = Path(temp_dir)
            fake = fake_bin / "mergetrain"
            fake.write_text(
                "#!/usr/bin/env python3\n"
                "import json, sys, time\n"
                "if 'status' in sys.argv:\n"
                "    time.sleep(2)\n"
                "    print(json.dumps({'contract_version': 4, 'health': 'healthy',\n"
                "        'state': 'idle', 'summary': 'No active jobs',\n"
                "        'next_action': {'code': 'queue_empty', 'requires_approval': 'none'},\n"
                "        'warnings': [], 'attention_jobs': [], 'recent_jobs': []}))\n"
                "else:\n"
                "    print(json.dumps({'items': []}))\n",
                encoding="utf-8",
            )
            fake.chmod(0o755)
            environment = dict(os.environ)
            environment["PATH"] = f"{fake_bin}:{environment['PATH']}"
            master, slave = pty.openpty()
            process = subprocess.Popen(
                [str(source_root / "mergetrain-status"), "--repo", str(source_root),
                 "--interval", "30", "--color", "never"],
                cwd=source_root, env=environment, stdin=slave, stdout=slave,
                stderr=slave, close_fds=True,
            )
            os.close(slave)

            def read_until(expected: bytes, timeout: float = 1.0) -> bytes:
                deadline = time.monotonic() + timeout
                output = b""
                while expected not in output and time.monotonic() < deadline:
                    readable, _, _ = select.select([master], [], [], 0.05)
                    if readable:
                        output += os.read(master, 65536)
                self.assertIn(expected, output)
                return output

            try:
                read_until(b"refreshing")
                started = time.monotonic()
                os.write(master, b"m")
                changed = read_until(b"[m] fewer merges")
                self.assertIn(b"refreshing", changed)
                self.assertLess(time.monotonic() - started, 1.0)
                read_until(b"health: healthy", timeout=12.0)
                os.write(master, b"q")
                self.assertEqual(process.wait(timeout=10), 0)
            finally:
                if process.poll() is None:
                    process.terminate()
                    process.wait(timeout=10)
                os.close(master)

    def test_generated_result_uses_preceding_source_title_within_80_columns(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            repo = Path(temp_dir)
            (repo / "benchmarks").mkdir()
            subprocess.run(["git", "init", "-q", "-b", "main"], cwd=repo, check=True)
            subprocess.run(
                ["git", "config", "user.email", "status-test@example.invalid"],
                cwd=repo,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.name", "Status Test"], cwd=repo, check=True
            )
            results = repo / "benchmarks" / "RESULTS.md"
            results.write_text(
                "| Benchmark | Dark (3.0x) | Rust |\n"
                "|---|---:|---:|\n"
                "| sample | 300 (3.0x) | 100 |\n",
                encoding="utf-8",
            )
            subprocess.run(["git", "add", "."], cwd=repo, check=True)
            subprocess.run(
                ["git", "commit", "-q", "-m", "Initial results"],
                cwd=repo,
                check=True,
            )
            source_title = (
                "Propagate executable-edge facts in MIR SCCP across every branch"
            )
            (repo / "optimization.txt").write_text("optimized\n", encoding="utf-8")
            subprocess.run(["git", "add", "."], cwd=repo, check=True)
            subprocess.run(
                ["git", "commit", "-q", "-m", source_title],
                cwd=repo,
                check=True,
            )
            results.write_text(
                results.read_text(encoding="utf-8").replace("300", "280").replace(
                    "3.0x", "2.8x"
                ),
                encoding="utf-8",
            )
            subprocess.run(["git", "add", "."], cwd=repo, check=True)
            subprocess.run(
                [
                    "git", "commit", "-q", "-m",
                    "Record MIR SCCP aggregate benchmark improvement",
                ],
                cwd=repo,
                check=True,
            )

            changes = benchmark_changes(repo)
            self.assertEqual(changes[0].source_subject, source_title)
            payload = {
                "contract_version": 4,
                "health": "healthy",
                "state": "idle",
                "summary": "No active jobs",
                "next_action": {"code": "queue_empty", "requires_approval": "none"},
            }
            output = render(payload, repo, color=False, columns=80)
            row = next(line for line in output.splitlines() if line.startswith("1. "))
            self.assertLessEqual(len(row), 80)
            self.assertIn("Propagate executable-edge facts", row)
            self.assertNotIn("Record MIR SCCP", row)
            self.assertTrue(row.endswith("…"))
            detail = benchmark_detail(repo, changes[0].commit, color=False)
            self.assertIn(source_title, detail)
            self.assertIn("Record MIR SCCP aggregate benchmark improvement", detail)
            diff = benchmark_diff(repo, changes[0].commit, color=False)
            self.assertIn("source change diff:", diff)
            self.assertIn("diff --git a/optimization.txt", diff)
            self.assertIn(source_title, diff)

    def test_interactive_keys_scroll_expand_and_open_benchmark_details(self) -> None:
        source_root = Path(__file__).resolve().parent.parent
        with tempfile.TemporaryDirectory() as temp_dir:
            fake_bin = Path(temp_dir)
            fake_mergetrain = fake_bin / "mergetrain"
            calls_path = fake_bin / "calls"
            fake_mergetrain.write_text(
                "#!/usr/bin/env python3\n"
                "import json\n"
                "import sys\n"
                "if 'history' in sys.argv:\n"
                "    print(json.dumps({'items': []}))\n"
                "    raise SystemExit(0)\n"
                f"with open({str(calls_path)!r}, 'a', encoding='utf-8') as calls:\n"
                "    calls.write('status\\n')\n"
                "print(json.dumps({\n"
                "    'contract_version': 4,\n"
                "    'health': 'healthy',\n"
                "    'state': 'idle',\n"
                "    'summary': 'No active jobs',\n"
                "    'next_action': {\n"
                "        'code': 'queue_empty',\n"
                "        'command': None,\n"
                "        'requires_approval': 'none',\n"
                "    },\n"
                "    'warnings': [],\n"
                "    'attention_jobs': [],\n"
                "    'recent_jobs': [],\n"
                "}))\n",
                encoding="utf-8",
            )
            fake_mergetrain.chmod(0o755)
            environment = dict(os.environ)
            environment["PATH"] = f"{fake_bin}:{environment['PATH']}"
            environment["LINES"] = "8"
            master, slave = pty.openpty()
            process = subprocess.Popen(
                [
                    str(source_root / "mergetrain-status"),
                    "--repo",
                    str(source_root),
                    "--interval",
                    "30",
                    "--color",
                    "never",
                ],
                cwd=source_root,
                env=environment,
                stdin=slave,
                stdout=slave,
                stderr=slave,
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
                read_until(b"health: healthy")
                self.assertEqual(calls_path.read_text(encoding="utf-8"), "status\n")
                os.write(master, b"j")
                read_until(b"\x1b[H\x1b[Jtest runtime:")
                os.write(master, b"\x1b[A")
                read_until(b"\x1b[H\x1b[Jhealth: healthy")
                self.assertEqual(calls_path.read_text(encoding="utf-8"), "status\n")
                os.write(master, b"m")
                read_until(b"[m] fewer merges")
                os.write(master, b"0")
                read_until(b"benchmarks p2")
                os.write(master, b"G")
                bottom = read_until(b"benchmarks p2")
                self.assertNotIn(b"\x1b[H\x1b[Jhealth: healthy", bottom)
                os.write(master, b"b")
                read_until(b"benchmarks p1")
                os.write(master, b"g")
                read_until(b"[m] fewer merges")
                os.write(master, b"1")
                detail_output = read_until(b"benchmark result:")
                self.assertIn(b"benchmark result:", detail_output)
                self.assertEqual(calls_path.read_text(encoding="utf-8"), "status\n")
                os.write(master, b"d")
                diff_output = read_until(b"benchmark commit diff:")
                self.assertIn(b"benchmarks/RESULTS.md excluded", diff_output)
                os.write(master, b"d")
                read_until(b"benchmark result:")
                os.write(master, b"q")
                status_output = read_until(b"[m] fewer merges")
                self.assertNotIn(b"benchmark result:", status_output)
                os.write(master, b"q")
                self.assertEqual(process.wait(timeout=10), 0)
            finally:
                if process.poll() is None:
                    process.terminate()
                    process.wait(timeout=10)
                os.close(master)

    @patch("scripts.render_mergetrain_status.last_test_runtime",
           return_value=(42.5, "2026-09-27T10:00:00+00:00"))
    @patch("scripts.render_mergetrain_status.benchmark_changes", return_value=[])
    @patch("scripts.render_mergetrain_status.recent_merges", return_value=[])
    def test_conflict_details_are_collapsed_and_can_be_toggled(
        self,
        _recent_merges: object,
        _benchmark_changes: object,
        _last_test_runtime: object,
    ) -> None:
        reason = "merge conflict in src/Compiler.fs\nfull conflicting hunk"
        payload = {
            "contract_version": 4,
            "health": "healthy",
            "state": "attention",
            "summary": "1 job needs attention",
            "next_action": {
                "code": "fix_blocked_job",
                "command": None,
                "requires_approval": "none",
            },
            "warnings": [],
            "attention_jobs": [
                {
                    "id": 3,
                    "task": "Compiler work",
                    "branch": "agent/compiler",
                    "state": "attention",
                    "reason": reason,
                }
            ],
            "recent_jobs": [],
        }

        collapsed = render(
            payload,
            Path("."),
            color=False,
            conflict_toggle_hint=True,
        )
        expanded = render(payload, Path("."), color=False, show_conflicts=True)

        self.assertIn("— conflict", collapsed)
        self.assertIn("test runtime: 42.5s", collapsed)
        self.assertNotIn("full conflicting hunk", collapsed)
        self.assertIn("[c] show full conflict details", collapsed)
        self.assertIn(reason, expanded)

    def test_shows_active_train_recent_merge_and_benchmark_changes(self) -> None:
        source_root = Path(__file__).resolve().parent.parent

        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo = root / "repo"
            fake_bin = root / "bin"
            (repo / "benchmarks").mkdir(parents=True)
            fake_bin.mkdir()

            subprocess.run(["git", "init", "-q", "-b", "main"], cwd=repo, check=True)
            subprocess.run(
                ["git", "config", "user.email", "status-test@example.invalid"],
                cwd=repo,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.name", "Status Test"], cwd=repo, check=True
            )
            (repo / "benchmarks" / "RESULTS.md").write_text(
                "**Architecture:** `arm64`\n"
                "**Profile:** `full`\n"
                "**Measurement policy:** `test`\n"
                "**Workload contract:** `test`\n"
                "| Benchmark | Dark (3.0x) | Rust |\n"
                "|---|---:|---:|\n"
                "| sample | 300 (3.0x) | 100 |\n",
                encoding="utf-8",
            )
            subprocess.run(["git", "add", "."], cwd=repo, check=True)
            subprocess.run(
                ["git", "commit", "-q", "-m", "Initial benchmark results"],
                cwd=repo,
                check=True,
            )

            for feature_number in range(1, 7):
                branch = f"feature-{feature_number}"
                feature_path = repo / f"{branch}.txt"
                subprocess.run(
                    ["git", "switch", "-q", "-c", branch], cwd=repo, check=True
                )
                feature_path.write_text("ready\n", encoding="utf-8")
                subprocess.run(["git", "add", feature_path.name], cwd=repo, check=True)
                subprocess.run(
                    ["git", "commit", "-q", "-m", f"Add feature {feature_number}"],
                    cwd=repo,
                    check=True,
                )
                subprocess.run(["git", "switch", "-q", "main"], cwd=repo, check=True)
                subprocess.run(
                    [
                        "git",
                        "merge",
                        "-q",
                        "--no-ff",
                        branch,
                        "-m",
                        f"Merge feature train {feature_number}",
                    ],
                    cwd=repo,
                    check=True,
                )
            (repo / "benchmarks" / "RESULTS.md").write_text(
                "**Architecture:** `arm64`\n"
                "**Profile:** `full`\n"
                "**Measurement policy:** `test`\n"
                "**Workload contract:** `test`\n"
                "| Benchmark | Dark (2.8x) | Rust |\n"
                "|---|---:|---:|\n"
                "| sample | 280 (2.8x) | 100 |\n",
                encoding="utf-8",
            )
            (repo / "optimization.txt").write_text(
                "optimized implementation\n", encoding="utf-8"
            )
            subprocess.run(["git", "add", "benchmarks/RESULTS.md"], cwd=repo, check=True)
            subprocess.run(["git", "add", "optimization.txt"], cwd=repo, check=True)
            subprocess.run(
                ["git", "commit", "-q", "-m", "Record benchmark improvement"],
                cwd=repo,
                check=True,
            )
            results_path = repo / "benchmarks" / "RESULTS.md"
            results_path.write_text(
                results_path.read_text(encoding="utf-8").replace(
                    "**Workload contract:** `test`",
                    "**Workload contract:** `test-v2`",
                ),
                encoding="utf-8",
            )
            subprocess.run(["git", "add", "benchmarks/RESULTS.md"], cwd=repo, check=True)
            subprocess.run(
                ["git", "commit", "-q", "-m", "Change benchmark contract"],
                cwd=repo,
                check=True,
            )

            fake_mergetrain = fake_bin / "mergetrain"
            fake_mergetrain.write_text(
                """#!/usr/bin/env python3
import json
import sys

assert "--json" in sys.argv
if "inspect" in sys.argv:
    assert sys.argv[sys.argv.index("inspect") + 1] == "10"
    print(json.dumps({
        "contract_version": 4,
        "progress": {
            "phase": "gating",
            "message": "Running gate 2/4: tests"
        }
    }))
elif "history" in sys.argv:
    print(json.dumps({"items": [{
        "status": "deployed", "finished_at": "9999-01-01T00:00:00+00:00",
        "jobs": [{"branch": "feature-6"}],
        "gates": [{"name": "tests", "state": "success",
                   "duration_seconds": 42.5,
                   "finished_at": "2026-09-27T10:00:00+00:00"}],
    }]}))
else:
    assert "status" in sys.argv
    assert sys.argv[sys.argv.index("--limit") + 1] == "1000"
    print(json.dumps({
    "contract_version": 4,
    "health": "healthy",
    "state": "running",
    "summary": "1 job(s) are running",
    "next_action": {
        "code": "runner_active",
        "command": None,
        "requires_approval": "none"
    },
    "warnings": [],
    "attention_jobs": [{
        "id": 9,
        "task": "Repair benchmark conflict",
        "branch": "agent/repair",
        "state": "attention",
        "reason": "merge conflict in benchmarks/RESULTS.md\\n"
                  "CONFLICT (content): both branches changed the benchmark table"
    }],
    "recent_jobs": [
        {
            "id": 12,
            "task": "Already deployed",
            "branch": "agent/done",
            "state": "done"
        },
        {
            "id": 11,
            "task": "Optimize calls",
            "branch": "agent/calls",
            "state": "waiting"
        },
        {
            "id": 10,
            "task": "Compile tuples",
            "branch": "agent/tuples",
            "state": "running"
        }
    ]
    }))
""",
                encoding="utf-8",
            )
            fake_mergetrain.chmod(0o755)

            process_environment = dict(os.environ)
            process_environment["PATH"] = f"{fake_bin}:{process_environment['PATH']}"
            completed = subprocess.run(
                [
                    str(source_root / "mergetrain-status"),
                    "--repo",
                    str(repo),
                    "--once",
                ],
                cwd=repo,
                env=process_environment,
                text=True,
                capture_output=True,
                check=False,
            )

            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertEqual(completed.stderr, "")
            self.assertNotIn("\x1b", completed.stdout)
            self.assertIn("health: healthy", completed.stdout)
            self.assertIn("RUNNING: 1 job(s) are running", completed.stdout)
            self.assertIn("in train: Running gate 2/4: tests\n", completed.stdout)
            self.assertIn("\n\nrecent merges:\n", completed.stdout)
            self.assertIn("\nrecent benchmark results:\n", completed.stdout)
            attention = completed.stdout.index(
                "  #9 attention Repair benchmark conflict [agent/repair] — conflict"
            )
            running = completed.stdout.index(
                "  #10 running Compile tuples [agent/tuples]"
            )
            waiting = completed.stdout.index(
                "  #11 waiting Optimize calls [agent/calls]"
            )
            self.assertLess(attention, running)
            self.assertLess(running, waiting)
            self.assertNotIn("Already deployed", completed.stdout)
            self.assertNotIn("benchmark ratio:", completed.stdout)
            self.assertRegex(
                completed.stdout,
                r"recent merges:\n[0-9a-f]{7,12} \d+s feature-6 \[tests 42\.5s\] — Add feature 6",
            )
            self.assertIn("feature-5 [tests n/a] — Add feature 5", completed.stdout)
            self.assertEqual(completed.stdout.count(" — Add feature "), 5)
            self.assertNotIn("feature-1 [tests n/a] — Add feature 1", completed.stdout)
            self.assertRegex(
                completed.stdout,
                r"1\. [0-9a-f]{7,12} +\d+s +2.8x \(n/a\) +"
                r"Change benchmark contract",
            )
            self.assertRegex(
                completed.stdout,
                r"2\. [0-9a-f]{7,12} +\d+s +2.8x \(-6.7%\) +"
                r"Record benchmark improvement",
            )
            benchmark_lines = [
                line
                for line in completed.stdout.splitlines()
                if line.startswith(("1. ", "2. ", "3. "))
            ]
            subject_columns = {
                line.index(subject)
                for line, subject in zip(
                    benchmark_lines,
                    (
                        "Change benchmark contract",
                        "Record benchmark improvement",
                        "Initial benchmark results",
                    ),
                    strict=True,
                )
            }
            self.assertEqual(len(subject_columns), 1)

            changes = benchmark_changes(repo)
            self.assertEqual(len(changes), 3)
            self.assertEqual(changes[0].ratio, "2.8x")
            self.assertIsNone(changes[0].change)
            self.assertAlmostEqual(changes[1].change or 0, -6.6666667)

            detail = benchmark_detail(repo, changes[1].commit, color=False)
            self.assertIn("benchmark result:", detail)
            self.assertIn("overall: 2.8x ← 3.0x (-6.7%)", detail)
            self.assertIn("changed: 1/1 · 1 improved · 0 disimproved", detail)
            self.assertIn("instructions: 20 saved · 0 added · net 20 saved", detail)
            self.assertRegex(
                detail,
                r"sample +300 → 280 +20 saved +-6.7% +3x → 2.8x",
            )

            diff = benchmark_diff(repo, changes[1].commit, color=False)
            self.assertIn("benchmark commit diff:", diff)
            self.assertIn("benchmarks/RESULTS.md excluded", diff)
            self.assertIn("diff --git a/optimization.txt b/optimization.txt", diff)
            self.assertNotIn("diff --git a/benchmarks/RESULTS.md", diff)

            colored = subprocess.run(
                [
                    str(source_root / "mergetrain-status"),
                    "--repo",
                    str(repo),
                    "--color=always",
                    "--once",
                ],
                cwd=repo,
                env=process_environment,
                text=True,
                capture_output=True,
                check=False,
            )

            self.assertEqual(colored.returncode, 0, colored.stderr)
            self.assertIn("\x1b[1m", colored.stdout)
            self.assertIn("\x1b[32mhealthy\x1b[0m", colored.stdout)
            self.assertIn("\x1b[31mattention\x1b[0m", colored.stdout)
            self.assertIn("\x1b[36mrunning\x1b[0m", colored.stdout)
            self.assertIn("\x1b[32m(-6.7%)\x1b[0m", colored.stdout)

            detailed = subprocess.run(
                [
                    str(source_root / "mergetrain-status"),
                    "--repo",
                    str(repo),
                    "--show-conflicts",
                    "--once",
                ],
                cwd=repo,
                env=process_environment,
                text=True,
                capture_output=True,
                check=False,
            )

            self.assertEqual(detailed.returncode, 0, detailed.stderr)
            self.assertIn("merge conflict in benchmarks/RESULTS.md", detailed.stdout)
            self.assertIn("CONFLICT (content): both branches changed", detailed.stdout)


if __name__ == "__main__":
    unittest.main()
