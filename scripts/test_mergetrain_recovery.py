"""End-to-end tests for safe merge-train recovery orchestration."""

from __future__ import annotations

import json
import os
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path

from scripts.mergetrain_recovery import (
    Failure,
    codex_instructions,
    preserve_failure_evidence,
)


class RecoveryFixture:
    def __init__(
        self, root: Path, *, conflict: bool = False, tests_fail: bool = True
    ) -> None:
        self.root = root
        self.repo = root / "repo"
        self.remote = root / "remote.git"
        self.bin = root / "bin"
        self.attempts = root / "attempts"
        self.calls = root / "mergetrain-calls.jsonl"
        self.codex_marker = root / "codex-called"
        self.details = root / "details.json"
        self.tests_fail = tests_fail
        self.bin.mkdir()
        self.repo.mkdir()
        self.git("init", "-q", "-b", "main")
        self.git("config", "user.email", "recovery-test@example.invalid")
        self.git("config", "user.name", "Recovery Test")
        self.write_base_files()
        self.git("add", ".")
        self.git("commit", "-q", "-m", "base")
        subprocess.run(["git", "init", "-q", "--bare", str(self.remote)], check=True)
        self.git("remote", "add", "mergetrain-local", str(self.remote))
        self.git("push", "-q", "-u", "mergetrain-local", "main")
        self.git("switch", "-q", "-c", "task/job")
        (self.repo / "feature.txt").write_text("job\n", encoding="utf-8")
        self.git("add", "feature.txt")
        self.git("commit", "-q", "-m", "job change")
        self.old_head = self.git_output("rev-parse", "HEAD")
        if conflict:
            self.git("switch", "-q", "main")
            (self.repo / "feature.txt").write_text("main\n", encoding="utf-8")
            self.git("add", "feature.txt")
            self.git("commit", "-q", "-m", "main change")
            self.git("push", "-q", "mergetrain-local", "main")
            self.git("switch", "-q", "task/job")
        self.write_fake_commands()

    def git(self, *args: str) -> None:
        subprocess.run(
            ["git", *args], cwd=self.repo, check=True, capture_output=True, text=True
        )

    def git_output(self, *args: str) -> str:
        return subprocess.check_output(["git", *args], cwd=self.repo, text=True).strip()

    def executable(self, path: Path, source: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(source, encoding="utf-8")
        path.chmod(path.stat().st_mode | stat.S_IXUSR)

    def write_base_files(self) -> None:
        (self.repo / "feature.txt").write_text("base\n", encoding="utf-8")
        (self.repo / ".mergetrain.yaml").write_text(
            "version: 2\ngit:\n  remote: mergetrain-local\n  integration_branch: main\n",
            encoding="utf-8",
        )
        self.executable(
            self.repo / "scripts" / "check_e2e_temp_paths.py",
            "#!/usr/bin/env python3\nraise SystemExit(0)\n",
        )
        self.executable(
            self.repo / "benchmarks" / "check_sources_unchanged.py",
            "#!/usr/bin/env python3\nraise SystemExit(0)\n",
        )
        self.executable(
            self.repo / "benchmarks" / "run_benchmarks.sh",
            "#!/bin/sh\necho 'Dark candidate/parent: equal; current/parent geometric ratio 1.000000'\n",
        )
        self.executable(self.repo / "build", "#!/bin/sh\nexit 0\n")
        test_condition = (
            "if [ ! -f repair.txt ]; then echo 'reproducible failure' >&2; exit 1; fi\n"
            if self.tests_fail
            else ""
        )
        self.executable(
            self.repo / "run-tests",
            f"#!/bin/sh\n{test_condition}echo 'success: 1/1 passed'\n",
        )

    def write_fake_commands(self) -> None:
        self.executable(
            self.bin / "mergetrain",
            """#!/usr/bin/env python3
import json, os, pathlib, sys
commands = {"inspect", "retry", "dismiss", "enqueue", "replace"}
command = next(arg for arg in sys.argv if arg in commands)
calls = pathlib.Path(os.environ["RECOVERY_TEST_CALLS"])
with calls.open("a", encoding="utf-8") as stream:
    stream.write(json.dumps({"command": command, "args": sys.argv[1:]}) + "\\n")
if command == "inspect":
    print(pathlib.Path(os.environ["RECOVERY_TEST_DETAILS"]).read_text(encoding="utf-8"))
elif command == "replace" and "--help" in sys.argv:
    if os.environ.get("RECOVERY_TEST_NO_REPLACE") == "1":
        raise SystemExit(2)
    print("replace help")
elif command == "replace":
    print(json.dumps({"replacement": {"id": 99}}))
elif command == "enqueue":
    print(json.dumps({"job": {"id": 99}}))
else:
    print(json.dumps({"ok": True}))
""",
        )
        self.executable(
            self.bin / "codex",
            """#!/usr/bin/env python3
import os, pathlib, subprocess, sys
worktree = pathlib.Path(sys.argv[sys.argv.index("-C") + 1])
pathlib.Path(os.environ["RECOVERY_TEST_CODEX_MARKER"]).write_text(str(worktree), encoding="utf-8")
feature = worktree / "feature.txt"
if "<<<<<<<" in feature.read_text(encoding="utf-8"):
    feature.write_text("main\\njob\\n", encoding="utf-8")
    (worktree / "repair.txt").write_text("merged\\n", encoding="utf-8")
    subprocess.run(["git", "add", "feature.txt", "repair.txt"], cwd=worktree, check=True)
    subprocess.run(["git", "cherry-pick", "--continue"], cwd=worktree, check=True)
else:
    repair = worktree / "repair.txt"
    repair.write_text("repaired\\n", encoding="utf-8")
    subprocess.run(["git", "add", "repair.txt"], cwd=worktree, check=True)
    subprocess.run(["git", "commit", "-q", "-m", "repair gate failure"], cwd=worktree, check=True)
output = pathlib.Path(sys.argv[sys.argv.index("--output-last-message") + 1])
output.write_text("Recovery committed.\\n", encoding="utf-8")
""",
        )

    def environment(self) -> dict[str, str]:
        env = dict(os.environ)
        env["PATH"] = f"{self.bin}:{env['PATH']}"
        env["RECOVERY_TEST_CALLS"] = str(self.calls)
        env["RECOVERY_TEST_DETAILS"] = str(self.details)
        env["RECOVERY_TEST_CODEX_MARKER"] = str(self.codex_marker)
        return env

    def set_failure(self, category: str, gate: str = "", detail: str = "") -> None:
        events = []
        if gate:
            events.append(
                {
                    "id": 10,
                    "state": "failure",
                    "message": f"Failed gate 3/4: {gate}",
                    "detail": detail,
                }
            )
        self.details.write_text(
            json.dumps(
                {
                    "job": {
                        "id": 4,
                        "task": "recover task",
                        "branch": "task/job",
                        "worktree_path": str(self.repo),
                        "head_sha": self.old_head,
                        "auto_deploy": True,
                    },
                    "outcome": {
                        "failure_category": category,
                        "message": detail or "merge conflict",
                    },
                    "events": events,
                }
            ),
            encoding="utf-8",
        )

    def execute(self) -> subprocess.CompletedProcess[str]:
        source_root = Path(__file__).resolve().parent.parent
        return subprocess.run(
            [
                "python3",
                str(source_root / "scripts" / "mergetrain_recovery.py"),
                "--repo",
                str(self.repo),
                "--attempt-dir",
                str(self.attempts),
                "--job-id",
                "4",
            ],
            env=self.environment(),
            text=True,
            capture_output=True,
            check=False,
        )

    def recorded_calls(self) -> list[dict[str, object]]:
        return [json.loads(line) for line in self.calls.read_text(encoding="utf-8").splitlines()]


class MergetrainRecoveryTests(unittest.TestCase):
    def test_failure_log_is_copied_into_recovery_artifacts(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            source = root / "job.log"
            source.write_text("full gate failure\n", encoding="utf-8")
            details: dict[str, object] = {"job": {"log_path": str(source)}}
            copied = preserve_failure_evidence(details, root, 4, "abc123")
            self.assertIsNotNone(copied)
            assert copied is not None
            self.assertEqual(copied.read_text(encoding="utf-8"), "full gate failure\n")
            self.assertEqual(
                details["recovery_evidence"], {"train_log": str(copied)}
            )

    def test_signal_terminated_gate_gets_one_exact_retry(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture = RecoveryFixture(Path(temp_dir))
            fixture.set_failure("gate_failed", "tests", "exit_code=-15")
            completed = fixture.execute()
            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertIn("unchanged", completed.stderr)
            self.assertFalse(fixture.codex_marker.exists())
            self.assertEqual(
                [call["command"] for call in fixture.recorded_calls()],
                ["inspect", "retry"],
            )

    def test_repeated_signal_failure_advances_to_codex_recovery(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture = RecoveryFixture(Path(temp_dir))
            fixture.set_failure("gate_failed", "tests", "exit_code=-15")
            first = fixture.execute()
            second = fixture.execute()
            self.assertEqual(first.returncode, 0, first.stderr)
            self.assertEqual(second.returncode, 0, second.stderr)
            commands = [call["command"] for call in fixture.recorded_calls()]
            self.assertEqual(commands.count("retry"), 1)
            self.assertIn("replace", commands)
            self.assertTrue(fixture.codex_marker.exists())

    def test_nonreproducible_test_failure_retries_unchanged_without_codex(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture = RecoveryFixture(Path(temp_dir), tests_fail=False)
            fixture.set_failure("gate_failed", "tests", "exit_code=1")
            completed = fixture.execute()
            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertIn("passed in isolation", completed.stderr)
            self.assertFalse(fixture.codex_marker.exists())
            commands = [call["command"] for call in fixture.recorded_calls()]
            self.assertIn("retry", commands)
            self.assertNotIn("replace", commands)

    def test_patch_equivalent_job_is_dismissed_without_codex(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture = RecoveryFixture(Path(temp_dir))
            fixture.git("switch", "-q", "main")
            fixture.git("cherry-pick", fixture.old_head)
            fixture.git("push", "-q", "mergetrain-local", "main")
            fixture.git("switch", "-q", "task/job")
            fixture.set_failure("merge_conflict")
            completed = fixture.execute()
            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertIn("patch-equivalent", completed.stderr)
            self.assertFalse(fixture.codex_marker.exists())
            self.assertIn("dismiss", [call["command"] for call in fixture.recorded_calls()])

    def test_codex_resolves_conflict_in_fresh_worktree_and_replacement_is_verified(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture = RecoveryFixture(Path(temp_dir), conflict=True)
            fixture.set_failure("merge_conflict")
            original_head = fixture.git_output("rev-parse", "HEAD")
            completed = fixture.execute()
            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertIn("fresh recovery worktree", completed.stderr)
            self.assertIn("verified Codex repair", completed.stderr)
            recovery_path = Path(fixture.codex_marker.read_text(encoding="utf-8"))
            self.assertNotEqual(recovery_path, fixture.repo)
            self.assertEqual(fixture.git_output("rev-parse", "HEAD"), original_head)
            self.assertEqual(fixture.git_output("status", "--porcelain"), "")
            self.assertEqual((recovery_path / "feature.txt").read_text(encoding="utf-8"), "main\njob\n")
            self.assertEqual(len(list(fixture.attempts.glob("*.verification.json"))), 1)
            self.assertIn("replace", [call["command"] for call in fixture.recorded_calls()])

    def test_reproducible_test_failure_uses_codex_and_full_verification(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture = RecoveryFixture(Path(temp_dir))
            fixture.set_failure("gate_failed", "tests", "exit_code=1")
            completed = fixture.execute()
            self.assertEqual(completed.returncode, 0, completed.stderr)
            receipt_path = next(fixture.attempts.glob("*.verification.json"))
            receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
            commands = [item["command"] for item in receipt["commands"]]
            self.assertIn(["./build", "--ai"], commands)
            self.assertIn(["./run-tests", "--ai"], commands)
            self.assertIn(
                ["./benchmarks/run_benchmarks.sh", "--verify-parent", "full"],
                commands,
            )

    def test_partial_supersession_replays_only_unique_commits(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture = RecoveryFixture(Path(temp_dir))
            first = fixture.old_head
            (fixture.repo / "second.txt").write_text("unique\n", encoding="utf-8")
            fixture.git("add", "second.txt")
            fixture.git("commit", "-q", "-m", "unique second change")
            fixture.old_head = fixture.git_output("rev-parse", "HEAD")
            fixture.git("switch", "-q", "main")
            fixture.git("cherry-pick", first)
            fixture.git("push", "-q", "mergetrain-local", "main")
            fixture.git("switch", "-q", "task/job")
            fixture.set_failure("gate_failed", "tests", "exit_code=1")
            completed = fixture.execute()
            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertIn("1 unique commit(s)", completed.stderr)
            recovery_path = Path(fixture.codex_marker.read_text(encoding="utf-8"))
            self.assertTrue((recovery_path / "second.txt").exists())

    def test_benchmark_repair_prompt_preserves_generated_result_rule(self) -> None:
        instructions = codex_instructions(
            {"job": {"id": 7, "task": "benchmark"}},
            Failure("gate_failed", "benchmarks", "unrecorded improvement"),
            False,
            "a" * 40,
        )
        self.assertIn("./benchmarks/run_benchmarks.sh full", instructions)
        self.assertIn("never hand-merge", instructions)
        self.assertIn("fresh recovery worktree", instructions)

    def test_v3_replacement_enqueues_before_dismiss_and_preserves_auto_approval(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture = RecoveryFixture(Path(temp_dir))
            fixture.set_failure("gate_failed", "tests", "exit_code=1")
            environment = fixture.environment()
            environment["RECOVERY_TEST_NO_REPLACE"] = "1"
            source_root = Path(__file__).resolve().parent.parent
            completed = subprocess.run(
                [
                    "python3",
                    str(source_root / "scripts" / "mergetrain_recovery.py"),
                    "--repo",
                    str(fixture.repo),
                    "--attempt-dir",
                    str(fixture.attempts),
                    "--job-id",
                    "4",
                ],
                env=environment,
                text=True,
                capture_output=True,
                check=False,
            )
            self.assertEqual(completed.returncode, 0, completed.stderr)
            calls = fixture.recorded_calls()
            commands = [call["command"] for call in calls]
            self.assertLess(commands.index("enqueue"), commands.index("dismiss"))
            enqueue = next(call for call in calls if call["command"] == "enqueue")
            self.assertIn("--auto", enqueue["args"])


if __name__ == "__main__":
    unittest.main()
