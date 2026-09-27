"""Tests for the tooling-only integration path."""

import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from scripts.mergetrain_control import classify, land_control, run as control_run


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
            self.assertEqual(classify(task, head)[0], True)
            self.assertIn("updated", (main / "land").read_text(encoding="utf-8"))
            (task / "compiler.fs").write_text("new compiler code\n", encoding="utf-8")
            self.git(task, "add", "compiler.fs")
            self.git(task, "commit", "-q", "-m", "compiler change")
            self.assertEqual(classify(task, self.git(task, "rev-parse", "HEAD"))[0], False)


if __name__ == "__main__":
    unittest.main()
