"""test_mergetrain_status.py - E2E tests for the repository status summary."""

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
    human_age,
    render,
)


class MergetrainStatusTests(unittest.TestCase):
    def test_human_age_uses_compact_units(self) -> None:
        now = datetime(2026, 9, 15, 12, 0, tzinfo=timezone.utc)

        self.assertEqual(human_age(now - timedelta(seconds=12), now=now), "12s ago")
        self.assertEqual(human_age(now - timedelta(minutes=5), now=now), "5m ago")
        self.assertEqual(human_age(now - timedelta(hours=2), now=now), "2h ago")
        self.assertEqual(human_age(now - timedelta(days=3), now=now), "3d ago")

    @patch("scripts.render_mergetrain_status.git_file")
    def test_benchmark_detail_data_contains_only_improvements(
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

        changes = benchmark_changes_for_commit(Path("."), "current")

        self.assertEqual(changes, [("faster", 25.0, 150)])

    @patch("scripts.render_mergetrain_status.git", return_value="")
    def test_benchmark_history_requests_latest_ten(
        self, mock_git: MagicMock
    ) -> None:
        self.assertEqual(benchmark_changes(Path(".")), [])
        self.assertIn("-10", mock_git.call_args.args)

    def test_interactive_keys_scroll_expand_and_open_benchmark_details(self) -> None:
        source_root = Path(__file__).resolve().parent.parent
        with tempfile.TemporaryDirectory() as temp_dir:
            fake_bin = Path(temp_dir)
            fake_mergetrain = fake_bin / "mergetrain"
            fake_mergetrain.write_text(
                "#!/usr/bin/env python3\n"
                "import json\n"
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
                read_until(b"[m] more merges")
                os.write(master, b"j")
                read_until(b"\x1b[H\x1b[JIDLE: No active jobs")
                os.write(master, b"\x1b[A")
                read_until(b"\x1b[H\x1b[Jhealth: healthy")
                os.write(master, b"m")
                read_until(b"[m] fewer merges")
                os.write(master, b"1")
                detail_output = read_until(b"[q/Esc] back")
                self.assertIn(b"benchmark result:", detail_output)
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

    @patch("scripts.render_mergetrain_status.benchmark_ratio", return_value=None)
    @patch("scripts.render_mergetrain_status.benchmark_changes", return_value=[])
    @patch("scripts.render_mergetrain_status.recent_merges", return_value=[])
    def test_conflict_details_are_collapsed_and_can_be_toggled(
        self,
        _recent_merges: object,
        _benchmark_changes: object,
        _benchmark_ratio: object,
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
            subprocess.run(["git", "add", "benchmarks/RESULTS.md"], cwd=repo, check=True)
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

assert "status" in sys.argv
assert "--json" in sys.argv
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
            self.assertIn("in train:\n", completed.stdout)
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
            self.assertIn("benchmark ratio: 2.8x", completed.stdout)
            self.assertRegex(
                completed.stdout,
                r"recent merges:\n[0-9a-f]{7,12} \d+s ago feature-6 — Add feature 6",
            )
            self.assertEqual(completed.stdout.count(" — Add feature "), 5)
            self.assertNotIn("feature-1 — Add feature 1", completed.stdout)
            self.assertRegex(
                completed.stdout,
                r"\[1\] [0-9a-f]{7,12} \d+s ago n/a 2.8x Change benchmark contract",
            )
            self.assertRegex(
                completed.stdout,
                r"\[2\] [0-9a-f]{7,12} \d+s ago 6.7% 2.8x "
                r"Record benchmark improvement",
            )

            changes = benchmark_changes(repo)
            self.assertEqual(len(changes), 3)
            self.assertEqual(changes[0].ratio, "2.8x")
            self.assertEqual(changes[0].improvement, "n/a")
            self.assertEqual(changes[1].improvement, "6.7%")

            detail = benchmark_detail(repo, changes[1].commit, color=False)
            self.assertIn("benchmark result:", detail)
            self.assertIn("ratio: 2.8x", detail)
            self.assertIn("sample 6.7% 280 instructions", detail)
            self.assertNotIn("300 instructions", detail)

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
