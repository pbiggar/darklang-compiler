"""test_prune_worktrees.py - Focused tests for safe worktree cleanup."""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


class PruneWorktreesTests(unittest.TestCase):
    def git(self, repo: Path, *args: str) -> str:
        return subprocess.run(
            ["git", "-C", str(repo), *args],
            check=True,
            text=True,
            capture_output=True,
        ).stdout.strip()

    def branch_exists(self, repo: Path, branch: str) -> bool:
        result = subprocess.run(
            ["git", "-C", str(repo), "show-ref", "--verify", f"refs/heads/{branch}"],
            check=False,
            capture_output=True,
        )
        return result.returncode == 0

    def test_prunes_only_missing_and_integrated_worktrees(self) -> None:
        source_script = Path(__file__).with_name("prune-worktrees.py")

        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repo = root / "repo"
            fake_bin = root / "bin"
            repo.mkdir()
            fake_bin.mkdir()
            self.git(repo, "init", "-q", "-b", "main")
            self.git(repo, "config", "user.email", "worktree-test@example.invalid")
            self.git(repo, "config", "user.name", "Worktree Test")
            (repo / "base.txt").write_text("base\n", encoding="utf-8")
            self.git(repo, "add", "base.txt")
            self.git(repo, "commit", "-q", "-m", "base")
            self.git(repo, "update-ref", "refs/remotes/origin/main", "HEAD")

            paths = {
                name: root / name
                for name in (
                    "merged",
                    "stale",
                    "dirty",
                    "locked",
                    "busy",
                    "unmerged",
                )
            }
            for name in paths:
                self.git(repo, "branch", name)
                self.git(repo, "worktree", "add", "-q", str(paths[name]), name)

            (paths["dirty"] / "untracked.txt").write_text("keep me\n", encoding="utf-8")
            self.git(repo, "worktree", "lock", str(paths["locked"]))
            (paths["unmerged"] / "change.txt").write_text("change\n", encoding="utf-8")
            self.git(paths["unmerged"], "add", "change.txt")
            self.git(paths["unmerged"], "commit", "-q", "-m", "unmerged")
            shutil.rmtree(paths["stale"])

            fake_lsof = fake_bin / "lsof"
            fake_lsof.write_text(
                """#!/usr/bin/env python3
import os
import sys

if os.environ.get("TEST_LSOF_FAIL") == "1":
    print("permission denied while inspecting processes", file=sys.stderr)
    raise SystemExit(1)

expected_uid = os.environ["TEST_LSOF_UID"]
uid_index = sys.argv.index("-u")
if sys.argv[uid_index + 1] != expected_uid:
    print("wrong UID selected for lsof", file=sys.stderr)
    raise SystemExit(1)

records = [("100", "test-shell", os.environ["TEST_PRIMARY"])]
busy = os.environ.get("TEST_BUSY_WORKTREE")
if busy:
    records.append(("4242", "terminal", busy))
for pid, command, path in records:
    fields = [f"p{pid}", f"c{command}", f"n{path}"]
    sys.stdout.buffer.write(("\\0".join(fields) + "\\0\\n").encode())
""",
                encoding="utf-8",
            )
            fake_lsof.chmod(0o755)
            environment = dict(os.environ)
            environment["PATH"] = f"{fake_bin}:{environment['PATH']}"
            environment["TEST_PRIMARY"] = str(repo)
            environment["TEST_BUSY_WORKTREE"] = str(paths["busy"])
            environment["TEST_LSOF_UID"] = str(os.stat(repo).st_uid)

            dry_run = subprocess.run(
                [sys.executable, str(source_script)],
                cwd=repo,
                env=environment,
                check=True,
                text=True,
                capture_output=True,
            )
            self.assertIn(f"REMOVE {paths['merged']}", dry_run.stdout)
            self.assertIn(f"PRUNE {paths['stale']}", dry_run.stdout)
            self.assertIn(f"KEEP  {paths['unmerged']}", dry_run.stdout)
            self.assertIn("Dry run: 1 checkout(s) removable", dry_run.stdout)
            self.assertIn(f"BLOCK {paths['dirty']}", dry_run.stdout)
            self.assertIn(f"BLOCK {paths['locked']}", dry_run.stdout)
            self.assertIn(f"BLOCK {paths['busy']}", dry_run.stdout)
            self.assertIn("used by PID 4242 (terminal)", dry_run.stdout)
            self.assertTrue(paths["merged"].exists())
            self.assertTrue(self.branch_exists(repo, "merged"))

            from_linked_worktree = subprocess.run(
                [sys.executable, str(source_script)],
                cwd=paths["merged"],
                env=environment,
                check=True,
                text=True,
                capture_output=True,
            )
            self.assertIn(
                f"KEEP  {paths['merged']} (merged): running worktree",
                from_linked_worktree.stdout,
            )

            failed_environment = dict(environment)
            failed_environment["TEST_LSOF_FAIL"] = "1"
            failed_inspection = subprocess.run(
                [sys.executable, str(source_script), "--apply"],
                cwd=repo,
                env=failed_environment,
                check=False,
                text=True,
                capture_output=True,
            )
            self.assertEqual(failed_inspection.returncode, 1)
            self.assertIn(
                "Cannot verify worktree eligibility with lsof",
                failed_inspection.stderr,
            )
            self.assertIn("no changes made", failed_inspection.stderr)
            self.assertTrue(paths["merged"].exists())

            blocked_apply = subprocess.run(
                [sys.executable, str(source_script), "--apply"],
                cwd=repo,
                env=environment,
                check=False,
                text=True,
                capture_output=True,
            )
            self.assertEqual(blocked_apply.returncode, 1)
            self.assertIn("failed eligibility checks", blocked_apply.stderr)
            self.assertIn("no changes made", blocked_apply.stderr)
            self.assertTrue(paths["merged"].exists())

            self.git(paths["dirty"], "add", "untracked.txt")
            self.git(paths["dirty"], "commit", "-q", "-m", "preserve dirty branch")
            (paths["locked"] / "locked.txt").write_text("keep me\n", encoding="utf-8")
            self.git(paths["locked"], "add", "locked.txt")
            self.git(paths["locked"], "commit", "-q", "-m", "preserve locked branch")
            eligible_environment = dict(environment)
            eligible_environment.pop("TEST_BUSY_WORKTREE")

            applied = subprocess.run(
                [sys.executable, str(source_script), "--apply"],
                cwd=repo,
                env=eligible_environment,
                check=True,
                text=True,
                capture_output=True,
            )
            self.assertIn("Applied: removed 2 checkout(s)", applied.stdout)
            self.assertFalse(paths["merged"].exists())
            self.assertFalse(paths["busy"].exists())
            self.assertFalse(self.branch_exists(repo, "merged"))
            self.assertFalse(self.branch_exists(repo, "busy"))
            self.assertFalse(self.branch_exists(repo, "stale"))
            registrations = self.git(repo, "worktree", "list", "--porcelain")
            self.assertNotIn(str(paths["stale"]), registrations)
            for name in ("dirty", "locked", "unmerged"):
                self.assertTrue(paths[name].exists())
                self.assertIn(str(paths[name]), registrations)


if __name__ == "__main__":
    unittest.main()
