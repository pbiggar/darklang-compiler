"""Tests for the tooling-only integration path."""

import subprocess
import sys
import tempfile
import unittest
from dataclasses import dataclass
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

from scripts.mergetrain_control import classify, land_control, measured_code_unchanged, run as control_run

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "benchmarks" / "infrastructure"))
import deployed_baseline


class ControlLandingTests(unittest.TestCase):
    def git(self, repo: Path, *args: str) -> str:
        return subprocess.check_output(["git", *args], cwd=repo, text=True).strip()

    def test_tooling_commit_lands_ahead_of_an_ordinary_change(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            main = root / "main"
            task = root / "task"
            main.mkdir()
            self.git(main, "init", "-q", "-b", "main")
            self.git(main, "config", "user.email", "control-test@example.invalid")
            self.git(main, "config", "user.name", "Control Test")
            self.git(main, "config", "receive.denyCurrentBranch", "updateInstead")
            (main / "land").write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
            (main / "mergetrain-status").write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
            (main / "scripts").mkdir()
            (main / "scripts" / "run-mergetrain-integrator.sh").write_text(
                "#!/bin/sh\nexit 0\n", encoding="utf-8"
            )
            self.git(main, "add", ".")
            self.git(main, "commit", "-q", "-m", "base")
            base = self.git(main, "rev-parse", "HEAD")
            self.git(main, "worktree", "add", "-q", "-b", "task/tooling", str(task))
            self.git(task, "remote", "add", "mergetrain-local", str(main))
            (task / "land").write_text("#!/bin/sh\necho updated\n", encoding="utf-8")
            self.git(task, "add", "land")
            self.git(task, "commit", "-q", "-m", "update tooling")
            head = self.git(task, "rev-parse", "HEAD")
            self.assertEqual(classify(task, head)[0], True)
            def without_fixture_tests(repo: Path, *command: str, check: bool = True):
                if command[:3] == ("python3", "-m", "unittest"):
                    return subprocess.CompletedProcess(command, 0, "", "")
                return control_run(repo, *command, check=check)

            with patch("scripts.mergetrain_control.WORKTREE_PARENT", root), \
                 patch("scripts.mergetrain_control.run", side_effect=without_fixture_tests):
                landed = land_control(task, head, "update tooling")
                self.assertEqual(land_control(task, head, "update tooling"), landed)
            self.assertEqual(self.git(main, "rev-parse", "HEAD"), landed)
            self.assertTrue(measured_code_unchanged(task, base, landed))
            self.assertFalse(measured_code_unchanged(task, landed, base))
            self.assertEqual(classify(task, head)[0], True)
            self.assertIn("updated", (main / "land").read_text(encoding="utf-8"))
            (task / "compiler.fs").write_text("new compiler code\n", encoding="utf-8")
            self.git(task, "add", "compiler.fs")
            self.git(task, "commit", "-q", "-m", "compiler change")
            self.assertEqual(classify(task, self.git(task, "rev-parse", "HEAD"))[0], False)
            self.assertFalse(measured_code_unchanged(task, base, self.git(task, "rev-parse", "HEAD")))

            self.git(task, "switch", "-q", "-c", "unsafe-tooling", landed)
            runner = task / "benchmarks" / "run_benchmarks.sh"
            runner.parent.mkdir()
            runner.write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
            self.git(task, "add", ".")
            self.git(task, "commit", "-q", "-m", "change measurement runner")
            changed = self.git(task, "rev-parse", "HEAD")
            # Even a control receipt cannot carry counts across a compiler or
            # measurement change. Check actual paths rather than trusting it.
            from scripts.mergetrain_control import audit, common_dir
            audit(common_dir(task) / "mergetrain-control" / "unsafe.json",
                  {"base": landed, "merged": changed, "status": "landed"})
            self.assertFalse(measured_code_unchanged(task, landed, changed))

    def test_alignment_preserves_counts_and_rejects_unproven_advance(self) -> None:
        @dataclass(frozen=True)
        class Measured:
            compiler: object
            benchmarks: tuple

        baseline = Measured(deployed_baseline.CompilerAttribution("old", "measured"), (123, 456))
        repo = Path("/fixture")
        track = SimpleNamespace(id="test")
        with patch.object(deployed_baseline, "git", side_effect=["new", "tooling"]), \
             patch.object(deployed_baseline, "state_dir", return_value=repo), \
             patch.object(deployed_baseline, "load_snapshot", return_value=baseline), \
             patch.object(deployed_baseline, "measured_code_unchanged", return_value=True), \
             patch.object(deployed_baseline, "write_snapshot") as write:
            aligned = deployed_baseline.aligned_baseline(repo, repo, track)
            self.assertEqual(aligned.benchmarks, baseline.benchmarks)
            self.assertEqual(aligned.compiler.commit, "new")
            write.assert_called_once_with(repo / "test.json", aligned)
        with patch.object(deployed_baseline, "git", return_value="new"), \
             patch.object(deployed_baseline, "state_dir", return_value=repo), \
             patch.object(deployed_baseline, "load_snapshot", return_value=baseline), \
             patch.object(deployed_baseline, "measured_code_unchanged", return_value=False), \
             patch.object(deployed_baseline, "write_snapshot") as write:
            with self.assertRaises(deployed_baseline.BaselineError):
                deployed_baseline.aligned_baseline(repo, repo, track)
            write.assert_not_called()


if __name__ == "__main__":
    unittest.main()
