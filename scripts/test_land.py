"""test_land.py - Focused tests for the branch-facing ./land command."""

import json
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


class LandScriptTests(unittest.TestCase):
    def test_queues_without_inspection_and_keeps_queue_deferral_opaque(self) -> None:
        source_root = Path(__file__).resolve().parent.parent

        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo = root / "repo"
            fake_bin = root / "bin"
            repo.mkdir()
            fake_bin.mkdir()
            (repo / "scripts").mkdir()

            subprocess.run(["git", "init", "-q", "-b", "main"], cwd=repo, check=True)
            subprocess.run(
                ["git", "config", "user.email", "land-test@example.invalid"],
                cwd=repo,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.name", "Land Test"], cwd=repo, check=True
            )
            shutil.copy2(source_root / "land", repo / "land")
            shutil.copy2(
                source_root / "scripts" / "mergetrain_exception.py",
                repo / "scripts" / "mergetrain_exception.py",
            )
            subprocess.run(
                ["git", "add", "land", "scripts/mergetrain_exception.py"],
                cwd=repo, check=True,
            )
            subprocess.run(["git", "commit", "-q", "-m", "base"], cwd=repo, check=True)
            subprocess.run(["git", "switch", "-q", "-c", "task/test"], cwd=repo, check=True)

            marker = repo / "change.txt"
            marker.write_text("ready\n", encoding="utf-8")
            subprocess.run(["git", "add", "change.txt"], cwd=repo, check=True)
            subprocess.run(["git", "commit", "-q", "-m", "ready change"], cwd=repo, check=True)

            fake_mergetrain = fake_bin / "mergetrain"
            fake_mergetrain.write_text(
                """#!/usr/bin/env python3
import json
import os
import subprocess
import sys

command = next(arg for arg in sys.argv if arg in {"status", "enqueue", "inspect"})
if command == "status":
    attention = int(os.environ.get("LAND_TEST_ATTENTION", "0"))
    print(json.dumps({
        "contract_version": 4,
        "counts": {"attention": attention},
        "health": "healthy" if attention == 0 else "unhealthy",
    }))
elif command == "enqueue":
    if os.environ.get("LAND_TEST_ENQUEUE_FAIL") == "1":
        print("unrelated queue diagnostics", file=sys.stderr)
        raise SystemExit(1)
    head = subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip()
    print(json.dumps({"job": {"id": 17, "head_sha": head}}))
else:
    raise AssertionError("land must not inspect an accepted job")
""",
                encoding="utf-8",
            )
            fake_mergetrain.chmod(0o755)

            process_environment = dict(os.environ)
            process_environment["PATH"] = f"{fake_bin}:{process_environment['PATH']}"
            completed = subprocess.run(
                [str(repo / "land"), "--task", "test queued output"],
                cwd=repo,
                env=process_environment,
                text=True,
                capture_output=True,
                check=False,
            )

            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertEqual(completed.stdout, "queued\n")
            self.assertEqual(completed.stderr, "")

            requested = subprocess.run(
                [
                    str(repo / "land"), "--exception-gate", "benchmarks",
                    "--exception-reason", "Intentional aggregate regression",
                ],
                cwd=repo,
                env=process_environment,
                text=True,
                capture_output=True,
                check=False,
            )
            self.assertEqual(requested.returncode, 0, requested.stderr)
            self.assertEqual(requested.stdout, "queued\n")
            head = subprocess.check_output(
                ["git", "rev-parse", "HEAD"], cwd=repo, text=True
            ).strip()
            request_file = repo / ".git" / "mergetrain-exceptions" / "requests" / f"{head}.json"
            self.assertEqual(
                json.loads(request_file.read_text(encoding="utf-8"))["gate"],
                "benchmarks",
            )

            process_environment["LAND_TEST_ATTENTION"] = "1"
            queued_behind_attention = subprocess.run(
                [str(repo / "land"), "--task", "test independent handoff"],
                cwd=repo,
                env=process_environment,
                text=True,
                capture_output=True,
                check=False,
            )

            self.assertEqual(
                queued_behind_attention.returncode,
                0,
                queued_behind_attention.stderr,
            )
            self.assertEqual(queued_behind_attention.stdout, "queued\n")
            self.assertEqual(queued_behind_attention.stderr, "")

            process_environment["LAND_TEST_ENQUEUE_FAIL"] = "1"
            deferred = subprocess.run(
                [str(repo / "land"), "--task", "test opaque deferral"],
                cwd=repo,
                env=process_environment,
                text=True,
                capture_output=True,
                check=False,
            )

            self.assertEqual(deferred.returncode, 1)
            self.assertEqual(deferred.stdout, "")
            self.assertEqual(
                deferred.stderr,
                "Landing handoff is pending; retry ./land later\n",
            )


if __name__ == "__main__":
    unittest.main()
