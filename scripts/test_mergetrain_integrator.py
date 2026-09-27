"""Focused output tests for the local merge-train integrator."""

import os
import pty
import re
import select
import subprocess
import tempfile
import time
import unicodedata
import unittest
from pathlib import Path


def visible_terminal_lines(output: bytes) -> list[str]:
    """Apply the cursor and clear controls used by the integrator."""
    rows: list[list[str]] = [[]]
    row = 0
    column = 0
    tokens = re.finditer(r"\x1b\[(\d*)([ABJK])|\r|\n|.", output.decode(), re.DOTALL)
    for token in tokens:
        value = token.group()
        if token.group(2) == "A":
            row = max(0, row - int(token.group(1) or "1"))
        elif token.group(2) == "B":
            row += int(token.group(1) or "1")
        elif token.group(2) == "J":
            rows = rows[: row + 1]
            rows[row] = rows[row][:column]
        elif token.group(2) == "K":
            rows[row] = []
        elif value == "\r":
            column = 0
        elif value == "\n":
            row += 1
            column = 0
        else:
            while len(rows) <= row:
                rows.append([])
            while len(rows[row]) < column:
                rows[row].append(" ")
            if len(rows[row]) == column:
                rows[row].append(value)
            else:
                rows[row][column] = value
            column += 1
        while len(rows) <= row:
            rows.append([])
    return ["".join(chars).rstrip() for chars in rows]


class MergetrainIntegratorOutputTests(unittest.TestCase):
    def make_fixture(self, root: Path) -> tuple[Path, dict[str, str]]:
        source_root = Path(__file__).resolve().parent.parent
        repo = root / "repo"
        fake_bin = root / "bin"
        repo.mkdir()
        fake_bin.mkdir()

        subprocess.run(["git", "init", "-q", "-b", "task/test"], cwd=repo, check=True)
        subprocess.run(
            ["git", "config", "user.email", "integrator-test@example.invalid"],
            cwd=repo,
            check=True,
        )
        subprocess.run(
            ["git", "config", "user.name", "Integrator Test"], cwd=repo, check=True
        )
        (repo / "change.txt").write_text("ready\n", encoding="utf-8")
        subprocess.run(["git", "add", "change.txt"], cwd=repo, check=True)
        subprocess.run(["git", "commit", "-q", "-m", "ready"], cwd=repo, check=True)

        fake_mergetrain = fake_bin / "mergetrain"
        fake_mergetrain.write_text(
            """#!/usr/bin/env python3
import json
import os
import pathlib
import subprocess
import sys
import time

command = next(arg for arg in sys.argv if arg in {"daemon", "status", "inspect", "retry"})
if command == "daemon":
    pass_file = os.environ.get("INTEGRATOR_TEST_DAEMON_PASS_FILE")
    if pass_file:
        path = pathlib.Path(pass_file)
        pass_count = int(path.read_text(encoding="utf-8")) if path.exists() else 0
        path.write_text(str(pass_count + 1), encoding="utf-8")
    fail_once_file = os.environ.get("INTEGRATOR_TEST_DAEMON_FAIL_ONCE_FILE")
    if fail_once_file and not pathlib.Path(fail_once_file).exists():
        pathlib.Path(fail_once_file).write_text("failed", encoding="utf-8")
        print("temporary daemon problem")
        raise SystemExit(1)
    recovered_file = os.environ.get("INTEGRATOR_TEST_RECOVERED_FILE")
    if recovered_file:
        pathlib.Path(recovered_file).write_text("recovered", encoding="utf-8")
    if os.environ.get("INTEGRATOR_TEST_PROGRESS") in {"1", "fast"}:
        progress_file = pathlib.Path(os.environ["INTEGRATOR_TEST_PROGRESS_FILE"])
        stages = ("assembling", "gating", "deploying", "done") if os.environ.get("INTEGRATOR_TEST_PROGRESS") == "1" else ("done",)
        for stage in stages:
            progress_file.write_text(stage, encoding="utf-8")
            time.sleep(0.35)
    for index in range(40):
        print(f"daemon noise {index}")
    raise SystemExit(int(os.environ.get("INTEGRATOR_TEST_DAEMON_EXIT", "0")))
if command == "status":
    if os.environ.get("INTEGRATOR_TEST_PROGRESS") in {"1", "fast"}:
        progress_file = pathlib.Path(os.environ["INTEGRATOR_TEST_PROGRESS_FILE"])
        stage = progress_file.read_text(encoding="utf-8") if progress_file.exists() else "waiting"
        running = stage not in {"waiting", "done"}
        job_state = "waiting" if stage == "waiting" else "running" if running else "done"
        task = "界" * 40 if os.environ.get("INTEGRATOR_TEST_WIDE_TASK") == "1" else "test job"
        print(json.dumps({
            "contract_version": 4,
            "counts": {"attention": 0, "ready": 0, "running": 2 if running else 0,
                       "waiting": 2 if stage == "waiting" else 0},
            "health": "healthy",
            "next_action": {"code": "wait_for_runner" if running else "enqueue_clean_branch", "target_job_id": None},
            "recent_jobs": [
                {"id": 7, "state": job_state, "task": task},
                {"id": 8, "state": job_state, "task": task},
            ],
            "state": "running" if running else "idle",
            "summary": "2 job(s) are running" if running else "Queue is idle",
        }))
        raise SystemExit(0)
    next_action = os.environ.get("INTEGRATOR_TEST_NEXT_ACTION", "fix_blocked_job")
    attention_ids = [4, 5] if os.environ.get("INTEGRATOR_TEST_MULTI_ATTENTION") == "1" else [4]
    print(json.dumps({
        "contract_version": 4,
        "attention_jobs": [{"id": job_id} for job_id in attention_ids],
        "counts": {"attention": len(attention_ids), "ready": 0, "running": 0, "waiting": 2},
        "health": "healthy",
        "next_action": {"code": next_action, "target_job_id": 4},
        "state": "attention",
        "summary": "1 job(s) need attention",
    }))
elif command == "inspect":
    if os.environ.get("INTEGRATOR_TEST_PROGRESS") in {"1", "fast"}:
        progress_file = pathlib.Path(os.environ["INTEGRATOR_TEST_PROGRESS_FILE"])
        stage = progress_file.read_text(encoding="utf-8")
        stage_index = ("assembling", "gating", "deploying", "done").index(stage)
        job_id = int(sys.argv[sys.argv.index("inspect") + 1])
        shared_events = [
            {"id": 201, "message": "Assembling train with 2 job(s)", "detail": "", "state": "active"},
            {"id": 204, "message": "Running gate 1/1: tests", "detail": "./run-tests --ai", "state": "active"},
            {"id": 205, "message": "Passed gate 1/1: tests", "detail": "./run-tests --ai", "state": "success"},
            {"id": 206, "message": "Deploying train", "detail": "", "state": "active"},
            {"id": 207, "message": "Deployed train", "detail": "", "state": "success"},
        ]
        merge_event = {
            "id": 195 + job_id,
            "job_id": job_id,
            "message": f"Merged task/progress-{job_id}",
            "detail": "",
            "state": "success",
        }
        limits = (1, 3, 4, 5)
        events = [shared_events[0], merge_event, *shared_events[1:limits[stage_index]]]
        print(json.dumps({"events": events, "job": {"id": job_id}}, indent=2))
        raise SystemExit(0)
    job_id = int(sys.argv[sys.argv.index("inspect") + 1])
    inspect_marker = os.environ.get("INTEGRATOR_TEST_INSPECT_MARKER")
    if inspect_marker:
        with pathlib.Path(inspect_marker).open("a", encoding="utf-8") as stream:
            stream.write(f"{job_id}\\n")
    repo = os.environ["INTEGRATOR_TEST_REPO"]
    head = subprocess.check_output(["git", "-C", repo, "rev-parse", "HEAD"], text=True).strip()
    branch = subprocess.check_output(
        ["git", "-C", repo, "branch", "--show-current"], text=True
    ).strip()
    category = os.environ.get("INTEGRATOR_TEST_FAILURE_CATEGORY", "merge_conflict")
    failed_gate = os.environ.get("INTEGRATOR_TEST_FAILED_GATE", "")
    events = []
    if failed_gate:
        events.append({
            "id": 211,
            "phase": "gating",
            "state": "failure",
            "message": f"Failed gate 4/4: {failed_gate}",
            "detail": "exit_code=1",
        })
    print(json.dumps({
        "job": {"worktree_path": repo, "branch": branch, "head_sha": head},
        "outcome": {"failure_category": category, "message": "failed train gate"},
        "events": events,
    }))
else:
    retry_marker = os.environ.get("INTEGRATOR_TEST_RETRY_MARKER")
    if retry_marker:
        pathlib.Path(retry_marker).write_text("called\\n", encoding="utf-8")
    print(json.dumps({"job": {"id": 4}}))
""",
            encoding="utf-8",
        )
        fake_mergetrain.chmod(0o755)

        fake_codex = fake_bin / "codex"
        fake_codex.write_text(
            """#!/usr/bin/env python3
import os
import pathlib
import sys

pathlib.Path(os.environ["INTEGRATOR_TEST_CODEX_ARGS"]).write_text(
    "\\n".join(sys.argv), encoding="utf-8"
)
output_index = sys.argv.index("--output-last-message") + 1
if os.environ.get("INTEGRATOR_TEST_CODEX_SUCCESS") == "1":
    repo = pathlib.Path(sys.argv[sys.argv.index("-C") + 1])
    generated = repo / "recorded-benchmark.txt"
    generated.write_text("recorded\\n", encoding="utf-8")
    import subprocess
    subprocess.run(["git", "add", generated.name], cwd=repo, check=True)
    subprocess.run(
        ["git", "commit", "-q", "-m", "Record integrated benchmark improvement"],
        cwd=repo,
        check=True,
    )
    pathlib.Path(sys.argv[output_index]).write_text(
        "Recorded benchmark improvement.\\n", encoding="utf-8"
    )
    raise SystemExit(0)
pathlib.Path(sys.argv[output_index]).write_text(
    "Could not resolve safely. Manual semantic decision required.\\n",
    encoding="utf-8",
)
for index in range(40):
    print(f"codex noise {index}", file=sys.stderr)
raise SystemExit(1)
""",
            encoding="utf-8",
        )
        fake_codex.chmod(0o755)

        environment = dict(os.environ)
        environment["PATH"] = f"{fake_bin}:{environment['PATH']}"
        environment["INTEGRATOR_TEST_REPO"] = str(repo)
        environment["INTEGRATOR_TEST_CODEX_ARGS"] = str(root / "codex-args.txt")
        environment["INTEGRATOR_TEST_PROGRESS_FILE"] = str(root / "progress.txt")
        environment["INTEGRATOR_TEST_RETRY_MARKER"] = str(root / "retry-called.txt")
        environment["INTEGRATOR_TEST_INSPECT_MARKER"] = str(root / "inspected.txt")
        environment["INTEGRATOR_SCRIPT"] = str(
            source_root / "scripts" / "run-mergetrain-integrator.sh"
        )
        return repo, environment

    def test_running_daemon_reports_merge_gate_and_deploy_progress(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            environment["INTEGRATOR_TEST_PROGRESS"] = "1"

            completed = subprocess.run(
                [
                    environment["INTEGRATOR_SCRIPT"],
                    "--repo",
                    str(repo),
                    "--attempt-dir",
                    str(root / "attempts"),
                    "--once",
                ],
                env=environment,
                text=True,
                capture_output=True,
                check=False,
            )

            self.assertEqual(completed.returncode, 0, completed.stderr)
            for job_id in (7, 8):
                self.assertRegex(
                    completed.stderr,
                    rf"\[\d{{2}}:\d{{2}}:\d{{2}}\] "
                    rf"Job #{job_id} OK: deployed",
                )
                self.assertEqual(completed.stderr.count(f"Job #{job_id}"), 1)
            self.assertNotIn("Assembling train with 2 job(s)", completed.stderr)

    def test_publishes_current_job_for_status_while_daemon_runs(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            environment["INTEGRATOR_TEST_PROGRESS"] = "1"
            attempts = root / "attempts"
            process = subprocess.Popen(
                [environment["INTEGRATOR_SCRIPT"], "--repo", str(repo),
                 "--attempt-dir", str(attempts), "--once"],
                env=environment, text=True, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            activity = attempts / f"integrator-activity-{process.pid}.txt"
            observed = ""
            try:
                deadline = time.monotonic() + 10
                while time.monotonic() < deadline and process.poll() is None:
                    if activity.exists():
                        observed = activity.read_text(encoding="utf-8")
                        if "Job #" in observed:
                            break
                    time.sleep(0.05)
                self.assertIn(str(repo), observed)
                self.assertIn("Job #", observed)
                _stdout, stderr = process.communicate(timeout=10)
                self.assertEqual(process.returncode, 0, stderr)
                self.assertFalse(activity.exists())
            finally:
                if process.poll() is None:
                    process.terminate()
                    process.wait(timeout=10)

    def test_terminal_updates_each_job_line_in_place(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            environment["INTEGRATOR_TEST_PROGRESS"] = "1"
            environment["TERM"] = "xterm"
            environment["INTEGRATOR_TEST_WIDE_TASK"] = "1"
            master, slave = pty.openpty()
            process = subprocess.Popen(
                [environment["INTEGRATOR_SCRIPT"], "--repo", str(repo),
                 "--attempt-dir", str(root / "attempts"), "--color", "never", "--once"],
                env=environment, stdin=slave, stdout=slave, stderr=slave,
                close_fds=True,
            )
            os.close(slave)
            output = b""
            try:
                deadline = time.monotonic() + 10
                while process.poll() is None and time.monotonic() < deadline:
                    readable, _, _ = select.select([master], [], [], 0.1)
                    if readable:
                        try:
                            output += os.read(master, 65536)
                        except OSError:
                            break
                self.assertEqual(process.wait(timeout=5), 0)
                while select.select([master], [], [], 0)[0]:
                    try:
                        output += os.read(master, 65536)
                    except OSError:
                        break
            finally:
                if process.poll() is None:
                    process.terminate()
                    process.wait(timeout=5)
                os.close(master)
            self.assertIn(b"Job #7", output)
            self.assertIn(b"Job #8", output)
            self.assertIn(b"\x1b[2A\r\x1b[2K", output)
            first_row = re.search(r"\[[^\r\n]+Job #7 WAIT:[^\r\n]*", output.decode())
            self.assertIsNotNone(first_row)
            assert first_row is not None
            cells = sum(
                0 if unicodedata.combining(char) else
                2 if unicodedata.east_asian_width(char) in {"F", "W"} else 1
                for char in first_row.group()
            )
            self.assertLess(cells, 80)
            visible = visible_terminal_lines(output)
            for job_id in (7, 8):
                matches = [line for line in visible if f"Job #{job_id}" in line]
                self.assertEqual(len(matches), 1, visible)
                self.assertIn(f"Job #{job_id} OK: deployed", matches[0])

    def test_fast_jobs_still_get_one_final_line_each(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            environment["INTEGRATOR_TEST_PROGRESS"] = "fast"
            completed = subprocess.run(
                [environment["INTEGRATOR_SCRIPT"], "--repo", str(repo),
                 "--attempt-dir", str(root / "attempts"), "--once"],
                env=environment, text=True, capture_output=True, check=False,
            )
            self.assertEqual(completed.returncode, 0, completed.stderr)
            for job_id in (7, 8):
                self.assertEqual(completed.stderr.count(f"Job #{job_id}"), 1)
                self.assertIn(f"Job #{job_id} OK: deployed", completed.stderr)

    def test_daemon_failure_prints_only_a_bounded_excerpt(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            attempts = root / "attempts"
            environment["INTEGRATOR_TEST_DAEMON_EXIT"] = "1"

            completed = subprocess.run(
                [
                    environment["INTEGRATOR_SCRIPT"],
                    "--repo",
                    str(repo),
                    "--attempt-dir",
                    str(attempts),
                    "--once",
                ],
                env=environment,
                text=True,
                capture_output=True,
                check=False,
            )

            self.assertEqual(completed.returncode, 0)
            self.assertEqual(completed.stdout, "")
            self.assertIn("Mergetrain daemon command failed", completed.stderr)
            self.assertIn("daemon noise 39", completed.stderr)
            self.assertNotIn("daemon noise 0\n", completed.stderr)
            self.assertLessEqual(len(completed.stderr.splitlines()), 13)
            daemon_logs = list(attempts.glob("daemon-failed-*.log"))
            self.assertEqual(len(daemon_logs), 1)
            self.assertIn("daemon noise 0", daemon_logs[0].read_text(encoding="utf-8"))

    def test_every_attention_job_is_inspected_even_when_recovery_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            environment["INTEGRATOR_TEST_MULTI_ATTENTION"] = "1"
            completed = subprocess.run(
                [
                    environment["INTEGRATOR_SCRIPT"],
                    "--repo",
                    str(repo),
                    "--attempt-dir",
                    str(root / "attempts"),
                    "--once",
                ],
                env=environment,
                text=True,
                capture_output=True,
                check=False,
            )
            self.assertEqual(completed.returncode, 0, completed.stderr)
            inspected = Path(environment["INTEGRATOR_TEST_INSPECT_MARKER"]).read_text(
                encoding="utf-8"
            )
            self.assertEqual(inspected.splitlines(), ["4", "5"])
            for job_id in (4, 5):
                self.assertEqual(completed.stderr.count(f"Job #{job_id}"), 1)
                self.assertIn(f"Job #{job_id} ERROR: recovery needs attention", completed.stderr)

    def test_merge_train_requires_recorded_benchmark_results(self) -> None:
        source_root = Path(__file__).resolve().parent.parent
        config = (source_root / ".mergetrain.yaml").read_text(encoding="utf-8")

        self.assertIn(
            "gate benchmarks -- ./benchmarks/run_benchmarks.sh --verify-fresh full", config
        )
        self.assertIn(
            "gate benchmark-sources -- python3 benchmarks/check_sources_unchanged.py", config
        )
        self.assertNotIn("test-runtime", config)
        self.assertIn("run: python3 scripts/check_e2e_temp_paths.py", config)
        self.assertNotIn(
            "run: ./benchmarks/run_benchmarks.sh --verify full", config
        )

    def test_daemon_failure_does_not_stop_the_integrator(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            environment["INTEGRATOR_TEST_DAEMON_FAIL_ONCE_FILE"] = str(
                root / "failed-once"
            )
            recovered_file = root / "recovered"
            environment["INTEGRATOR_TEST_RECOVERED_FILE"] = str(recovered_file)
            environment["INTEGRATOR_TEST_NEXT_ACTION"] = "enqueue_clean_branch"

            process = subprocess.Popen(
                [
                    environment["INTEGRATOR_SCRIPT"],
                    "--repo",
                    str(repo),
                    "--attempt-dir",
                    str(root / "attempts"),
                    "--interval",
                    "1",
                ],
                env=environment,
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            try:
                for _ in range(50):
                    if recovered_file.exists():
                        break
                    time.sleep(0.1)
                self.assertTrue(recovered_file.exists(), "integrator did not retry")
            finally:
                process.terminate()
                _stdout, stderr = process.communicate(timeout=5)

            self.assertIn("Mergetrain daemon command failed", stderr)

    def test_successful_idle_tick_reports_readable_status_without_subprocess_noise(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            attempts = root / "attempts"
            environment["INTEGRATOR_TEST_NEXT_ACTION"] = "enqueue_clean_branch"

            completed = subprocess.run(
                [
                    environment["INTEGRATOR_SCRIPT"],
                    "--repo",
                    str(repo),
                    "--attempt-dir",
                    str(attempts),
                    "--once",
                ],
                env=environment,
                text=True,
                capture_output=True,
                check=False,
            )

            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertEqual(completed.stdout, "")
            self.assertIn("Integrator started", completed.stderr)
            self.assertIn("Queue: 1 attention, 0 running, 2 waiting", completed.stderr)
            self.assertIn("1 job(s) need attention", completed.stderr)
            self.assertNotIn("daemon noise", completed.stderr)
            self.assertEqual(list(attempts.iterdir()), [])

    def test_validate_queued_jobs_continues_to_the_next_daemon_pass(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            pass_file = root / "daemon-passes"
            environment["INTEGRATOR_TEST_DAEMON_PASS_FILE"] = str(pass_file)
            environment["INTEGRATOR_TEST_NEXT_ACTION"] = "validate_queued_jobs"

            process = subprocess.Popen(
                [
                    environment["INTEGRATOR_SCRIPT"],
                    "--repo",
                    str(repo),
                    "--attempt-dir",
                    str(root / "attempts"),
                    "--interval",
                    "1",
                ],
                env=environment,
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            try:
                for _ in range(50):
                    passes = (
                        int(pass_file.read_text(encoding="utf-8"))
                        if pass_file.exists()
                        else 0
                    )
                    if passes >= 2:
                        break
                    time.sleep(0.1)
                self.assertGreaterEqual(passes, 2, "integrator stopped after one pass")
            finally:
                process.terminate()
                _stdout, stderr = process.communicate(timeout=5)

            self.assertNotIn("requires operator action", stderr)

    def test_color_mode_controls_ansi_output(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            environment["INTEGRATOR_TEST_NEXT_ACTION"] = "enqueue_clean_branch"
            environment["INTEGRATOR_TEST_PROGRESS"] = "fast"

            colored = subprocess.run(
                [
                    environment["INTEGRATOR_SCRIPT"],
                    "--repo",
                    str(repo),
                    "--attempt-dir",
                    str(root / "colored-attempts"),
                    "--color",
                    "always",
                    "--once",
                ],
                env=environment,
                text=True,
                capture_output=True,
                check=False,
            )
            plain = subprocess.run(
                [
                    environment["INTEGRATOR_SCRIPT"],
                    "--repo",
                    str(repo),
                    "--attempt-dir",
                    str(root / "plain-attempts"),
                    "--color",
                    "never",
                    "--once",
                ],
                env=environment,
                text=True,
                capture_output=True,
                check=False,
            )

            self.assertEqual(colored.returncode, 0, colored.stderr)
            self.assertRegex(colored.stderr, r"\x1b\[2m\[\d{2}:\d{2}:\d{2}\]\x1b\[0m")
            self.assertIn("\x1b[32mOK\x1b[0m", colored.stderr)
            self.assertEqual(plain.returncode, 0, plain.stderr)
            self.assertNotIn("\x1b[", plain.stderr)


if __name__ == "__main__":
    unittest.main()
