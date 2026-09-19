"""Focused output tests for the local merge-train integrator."""

import os
import subprocess
import tempfile
import unittest
from pathlib import Path


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
    if os.environ.get("INTEGRATOR_TEST_PROGRESS") == "1":
        progress_file = pathlib.Path(os.environ["INTEGRATOR_TEST_PROGRESS_FILE"])
        for stage in ("assembling", "gating", "deploying", "done"):
            progress_file.write_text(stage, encoding="utf-8")
            time.sleep(0.35)
    for index in range(40):
        print(f"daemon noise {index}")
    raise SystemExit(int(os.environ.get("INTEGRATOR_TEST_DAEMON_EXIT", "0")))
if command == "status":
    if os.environ.get("INTEGRATOR_TEST_PROGRESS") == "1":
        progress_file = pathlib.Path(os.environ["INTEGRATOR_TEST_PROGRESS_FILE"])
        stage = progress_file.read_text(encoding="utf-8") if progress_file.exists() else "waiting"
        running = stage not in {"waiting", "done"}
        print(json.dumps({
            "contract_version": 4,
            "counts": {"attention": 0, "ready": 0, "running": 2 if running else 0, "waiting": 0},
            "health": "healthy",
            "next_action": {"code": "wait_for_runner" if running else "enqueue_clean_branch", "target_job_id": None},
            "recent_jobs": [
                {"id": 7, "state": "running" if running else "done"},
                {"id": 8, "state": "running" if running else "done"},
            ],
            "state": "running" if running else "idle",
            "summary": "2 job(s) are running" if running else "Queue is idle",
        }))
        raise SystemExit(0)
    next_action = os.environ.get("INTEGRATOR_TEST_NEXT_ACTION", "fix_blocked_job")
    print(json.dumps({
        "contract_version": 4,
        "counts": {"attention": 1, "ready": 0, "running": 0, "waiting": 2},
        "health": "healthy",
        "next_action": {"code": next_action, "target_job_id": 4},
        "state": "attention",
        "summary": "1 job(s) need attention",
    }))
elif command == "inspect":
    if os.environ.get("INTEGRATOR_TEST_PROGRESS") == "1":
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
            self.assertIn("Assembling train with 2 job(s)", completed.stderr)
            self.assertEqual(completed.stderr.count("Assembling train with 2 job(s)"), 1)
            self.assertIn("Job #7: Merged task/progress-7", completed.stderr)
            self.assertIn("Job #8: Merged task/progress-8", completed.stderr)
            self.assertNotIn("Job #7: Assembling train", completed.stderr)
            self.assertIn("Running gate 1/1: tests — ./run-tests --ai", completed.stderr)
            self.assertIn("Passed gate 1/1: tests — ./run-tests --ai", completed.stderr)
            self.assertIn("Deploying train", completed.stderr)
            self.assertIn("Deployed train", completed.stderr)

    def test_codex_failure_is_concise_and_links_full_logs(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            attempts = root / "attempts"

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

            self.assertEqual(completed.returncode, 1)
            self.assertEqual(completed.stdout, "")
            self.assertIn("Integrator started", completed.stderr)
            self.assertIn("Job #4: Repairing task/test after merge conflict", completed.stderr)
            self.assertIn("Job #4: Starting Codex repair", completed.stderr)
            self.assertIn("Job #4: Codex repair failed (merge_conflict)", completed.stderr)
            self.assertIn(
                "Job #4: Codex summary: Could not resolve safely.", completed.stderr
            )
            self.assertIn("Job #4: Final message:", completed.stderr)
            self.assertIn("Job #4: Full execution log:", completed.stderr)
            self.assertIn("Job #4: Daemon log:", completed.stderr)
            self.assertNotIn("codex noise", completed.stderr)
            self.assertNotIn("daemon noise", completed.stderr)
            codex_logs = list(attempts.glob("4-*.codex.log"))
            self.assertEqual(len(codex_logs), 1)
            self.assertIn("codex noise 0", codex_logs[0].read_text(encoding="utf-8"))
            daemon_logs = list(attempts.glob("4-*.daemon.log"))
            self.assertEqual(len(daemon_logs), 1)
            self.assertIn("daemon noise 0", daemon_logs[0].read_text(encoding="utf-8"))
            codex_args = Path(environment["INTEGRATOR_TEST_CODEX_ARGS"]).read_text(
                encoding="utf-8"
            )
            self.assertIn("benchmarks/RESULTS.md", codex_args)
            self.assertIn("./benchmarks/run_benchmarks.sh full", codex_args)
            self.assertIn("must prove an aggregate improvement", codex_args)
            self.assertIn("do not hand-merge", codex_args)

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

            self.assertEqual(completed.returncode, 1)
            self.assertEqual(completed.stdout, "")
            self.assertIn("Mergetrain daemon command failed", completed.stderr)
            self.assertIn("daemon noise 39", completed.stderr)
            self.assertNotIn("daemon noise 0\n", completed.stderr)
            self.assertLessEqual(len(completed.stderr.splitlines()), 13)
            daemon_logs = list(attempts.glob("daemon-failed-*.log"))
            self.assertEqual(len(daemon_logs), 1)
            self.assertIn("daemon noise 0", daemon_logs[0].read_text(encoding="utf-8"))

    def test_benchmark_gate_failure_requests_recording_repair(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            environment["INTEGRATOR_TEST_FAILURE_CATEGORY"] = "gate_failed"
            environment["INTEGRATOR_TEST_FAILED_GATE"] = "benchmarks"

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

            self.assertEqual(completed.returncode, 1)
            self.assertIn("after benchmark gate failure", completed.stderr)
            codex_args = Path(environment["INTEGRATOR_TEST_CODEX_ARGS"]).read_text(
                encoding="utf-8"
            )
            self.assertIn("./benchmarks/run_benchmarks.sh full", codex_args)
            self.assertIn("commit the regenerated benchmark files", codex_args)
            self.assertIn("If the recording run reports a regression", codex_args)

    def test_non_benchmark_gate_failure_is_not_automatically_repaired(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            environment["INTEGRATOR_TEST_FAILURE_CATEGORY"] = "gate_failed"
            environment["INTEGRATOR_TEST_FAILED_GATE"] = "tests"

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

            self.assertEqual(completed.returncode, 1)
            self.assertIn(
                "needs operator attention (gate_failed)", completed.stderr
            )
            self.assertFalse(
                Path(environment["INTEGRATOR_TEST_CODEX_ARGS"]).exists()
            )

    def test_recorded_benchmark_improvement_retries_the_new_commit(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            old_head = subprocess.check_output(
                ["git", "rev-parse", "HEAD"], cwd=repo, text=True
            ).strip()
            environment["INTEGRATOR_TEST_FAILURE_CATEGORY"] = "gate_failed"
            environment["INTEGRATOR_TEST_FAILED_GATE"] = "benchmarks"
            environment["INTEGRATOR_TEST_CODEX_SUCCESS"] = "1"

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
            self.assertIn("Job #4: Retried after Codex committed a repair", completed.stderr)
            new_head = subprocess.check_output(
                ["git", "rev-parse", "HEAD"], cwd=repo, text=True
            ).strip()
            self.assertNotEqual(new_head, old_head)
            self.assertEqual(
                Path(environment["INTEGRATOR_TEST_RETRY_MARKER"]).read_text(
                    encoding="utf-8"
                ),
                "called\n",
            )

    def test_merge_train_requires_recorded_benchmark_results(self) -> None:
        source_root = Path(__file__).resolve().parent.parent
        config = (source_root / ".mergetrain.yaml").read_text(encoding="utf-8")

        self.assertIn(
            "run: ./benchmarks/run_benchmarks.sh --verify-fresh full", config
        )
        self.assertNotIn(
            "run: ./benchmarks/run_benchmarks.sh --verify full", config
        )

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

    def test_color_mode_controls_ansi_output(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo, environment = self.make_fixture(root)
            environment["INTEGRATOR_TEST_NEXT_ACTION"] = "enqueue_clean_branch"

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
            self.assertIn("\x1b[", colored.stderr)
            self.assertEqual(plain.returncode, 0, plain.stderr)
            self.assertNotIn("\x1b[", plain.stderr)


if __name__ == "__main__":
    unittest.main()
