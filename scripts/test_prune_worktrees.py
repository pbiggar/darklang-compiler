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
                    "ignored",
                    "interactive_delete",
                    "interactive_override",
                    "interactive_stale",
                    "remove_fail",
                    "unmerged",
                )
            }
            paths["remove_fail"] = root / "protected" / "remove-fail"
            paths["remove_fail"].parent.mkdir()
            for name in paths:
                self.git(repo, "branch", name)
                self.git(repo, "worktree", "add", "-q", str(paths[name]), name)

            (paths["dirty"] / "untracked.txt").write_text("keep me\n", encoding="utf-8")
            with (repo / ".git" / "info" / "exclude").open("a", encoding="utf-8") as file:
                file.write("artifact.cache\n")
            (paths["ignored"] / "artifact.cache").write_text(
                "keep me\n", encoding="utf-8"
            )
            (paths["interactive_override"] / "artifact.cache").write_text(
                "delete me\n", encoding="utf-8"
            )
            self.git(repo, "worktree", "lock", str(paths["locked"]))
            (paths["unmerged"] / "change.txt").write_text("change\n", encoding="utf-8")
            self.git(paths["unmerged"], "add", "change.txt")
            self.git(paths["unmerged"], "commit", "-q", "-m", "unmerged")
            (repo / "same-subject-main.txt").write_text(
                "different change\n", encoding="utf-8"
            )
            self.git(repo, "add", "same-subject-main.txt")
            self.git(repo, "commit", "-q", "-m", "unmerged")
            self.git(repo, "update-ref", "refs/remotes/origin/main", "HEAD")
            shutil.rmtree(paths["stale"])
            shutil.rmtree(paths["interactive_stale"])

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
            real_git = shutil.which("git")
            self.assertIsNotNone(real_git)
            fake_git = fake_bin / "git"
            fake_git.write_text(
                """#!/usr/bin/env python3
import os
import sys

failure_target = os.environ.get("TEST_REMOVE_FAILURE")
arguments = sys.argv[1:]
if failure_target and "worktree" in arguments and "remove" in arguments and arguments[-1] == failure_target:
    print("simulated checkout removal failure", file=sys.stderr)
    raise SystemExit(255)
os.execv(os.environ["TEST_REAL_GIT"], [os.environ["TEST_REAL_GIT"], *arguments])
""",
                encoding="utf-8",
            )
            fake_git.chmod(0o755)
            environment = dict(os.environ)
            environment["PATH"] = f"{fake_bin}:{environment['PATH']}"
            environment["TEST_REAL_GIT"] = str(real_git)
            environment["TEST_PRIMARY"] = str(repo)
            environment["TEST_BUSY_WORKTREE"] = str(paths["busy"])
            environment["TEST_LSOF_UID"] = str(os.stat(repo).st_uid)

            stopped = subprocess.run(
                [sys.executable, str(source_script), "--interactive"],
                cwd=repo,
                env=environment,
                input="",
                check=False,
                text=True,
                capture_output=True,
            )
            self.assertEqual(stopped.returncode, 1)
            self.assertIn("Interactive cleanup stopped", stopped.stdout)
            self.assertIn(
                "interactive input ended before a choice was made",
                stopped.stderr,
            )

            interactive = subprocess.run(
                [sys.executable, str(source_script), "--interactive"],
                cwd=repo,
                env=environment,
                input="k\nk\nk\nd\nd\nd\nk\nk\nk\nk\nk\n",
                check=True,
                text=True,
                capture_output=True,
            )
            self.assertIn("Branch: unmerged", interactive.stdout)
            self.assertIn(f"Directory: {paths['unmerged']} (present)", interactive.stdout)
            self.assertIn("Checkout:", interactive.stdout)
            self.assertIn("Last commit:", interactive.stdout)
            self.assertIn("less than a minute ago", interactive.stdout)
            self.assertIn("Merged into origin/main: no", interactive.stdout)
            self.assertIn(
                "Same-subject commit on origin/main: yes",
                interactive.stdout,
            )
            self.assertIn("Locked: yes", interactive.stdout)
            self.assertIn("?? untracked.txt", interactive.stdout)
            self.assertIn("!! artifact.cache", interactive.stdout)
            self.assertIn("created/updated:", interactive.stdout)
            self.assertIn("Active processes: 4242 (terminal)", interactive.stdout)
            self.assertIn("Recommendation: KEEP", interactive.stdout)
            self.assertIn("Recommendation: DELETE", interactive.stdout)
            self.assertIn(
                "Interactive cleanup: deleted 3 checkout(s), deleted 3 branch(es), "
                "kept 9 worktree(s)",
                interactive.stdout,
            )
            self.assertIn("Reclaimed checkout space:", interactive.stdout)
            self.assertFalse(paths["interactive_delete"].exists())
            self.assertFalse(self.branch_exists(repo, "interactive_delete"))
            self.assertFalse(paths["interactive_override"].exists())
            self.assertFalse(self.branch_exists(repo, "interactive_override"))
            self.assertFalse(self.branch_exists(repo, "interactive_stale"))

            dry_run = subprocess.run(
                [sys.executable, str(source_script)],
                cwd=repo,
                env=environment,
                check=True,
                text=True,
                capture_output=True,
            )
            self.assertIn(
                "REMOVE (2) — clean, inactive, integrated checkouts",
                dry_run.stdout,
            )
            self.assertIn(f"  {paths['merged']} (merged)", dry_run.stdout)
            self.assertIn(
                f"  {paths['remove_fail']} (remove_fail)", dry_run.stdout
            )
            self.assertIn("PRUNE (1) — missing checkouts", dry_run.stdout)
            self.assertIn(f"  {paths['stale']} (stale)", dry_run.stdout)
            self.assertIn("KEEP (2)", dry_run.stdout)
            self.assertIn(f"  {paths['unmerged']} (unmerged)", dry_run.stdout)
            self.assertIn("Dry run: 2 checkout(s) removable", dry_run.stdout)
            self.assertIn("BLOCK (4)", dry_run.stdout)
            self.assertIn(f"  {paths['dirty']} (dirty)", dry_run.stdout)
            self.assertIn(f"  {paths['locked']} (locked)", dry_run.stdout)
            self.assertIn(f"  {paths['busy']} (busy)", dry_run.stdout)
            self.assertIn(f"  {paths['ignored']} (ignored)", dry_run.stdout)
            self.assertIn("tracked, untracked, or ignored files", dry_run.stdout)
            self.assertIn("used by PID 4242 (terminal)", dry_run.stdout)
            self.assertNotIn("\033[", dry_run.stdout)
            self.assertLess(
                dry_run.stdout.index("REMOVE (2)"),
                dry_run.stdout.index("PRUNE (1)"),
            )
            self.assertLess(
                dry_run.stdout.index("PRUNE (1)"),
                dry_run.stdout.index("BLOCK (4)"),
            )
            self.assertLess(
                dry_run.stdout.index(f"  {paths['busy']} (busy)"),
                dry_run.stdout.index(f"  {paths['dirty']} (dirty)"),
            )
            self.assertLess(
                dry_run.stdout.index("BLOCK (4)"),
                dry_run.stdout.index("KEEP (2)"),
            )
            self.assertTrue(paths["merged"].exists())
            self.assertTrue(self.branch_exists(repo, "merged"))

            colored = subprocess.run(
                [sys.executable, str(source_script), "--color", "always"],
                cwd=repo,
                env=environment,
                check=True,
                text=True,
                capture_output=True,
            )
            self.assertIn("\033[1;31mREMOVE (2)", colored.stdout)
            self.assertIn("\033[1;35mPRUNE (1)", colored.stdout)
            self.assertIn("\033[1;33mBLOCK (4)\033[0m", colored.stdout)
            self.assertIn("\033[2mKEEP (2)\033[0m", colored.stdout)

            from_linked_worktree = subprocess.run(
                [sys.executable, str(source_script)],
                cwd=paths["merged"],
                env=environment,
                check=True,
                text=True,
                capture_output=True,
            )
            self.assertIn(
                f"  {paths['merged']} (merged)\n    running worktree",
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

            removal_failure_environment = dict(environment)
            removal_failure_environment["TEST_REMOVE_FAILURE"] = str(
                paths["remove_fail"]
            )
            blocked_apply = subprocess.run(
                [sys.executable, str(source_script), "--apply"],
                cwd=repo,
                env=removal_failure_environment,
                check=False,
                text=True,
                capture_output=True,
            )
            self.assertEqual(blocked_apply.returncode, 1)
            self.assertIn(
                "Applied: removed 1 checkout(s), pruned 1 stale registration(s), "
                "deleted 2 branch(es), left 4 blocked worktree(s)",
                blocked_apply.stdout,
            )
            self.assertRegex(
                blocked_apply.stdout,
                r"Reclaimed checkout space: (?!0 B)\d+(?:\.\d+)? [KMGT]?i?B",
            )
            self.assertIn("ERROR (1)", blocked_apply.stderr)
            self.assertIn("simulated checkout removal failure", blocked_apply.stderr)
            self.assertIn("Other eligible cleanup continued", blocked_apply.stderr)
            self.assertFalse(paths["merged"].exists())
            self.assertFalse(self.branch_exists(repo, "merged"))
            self.assertFalse(self.branch_exists(repo, "stale"))
            for name in ("dirty", "locked", "busy", "ignored", "remove_fail"):
                self.assertTrue(paths[name].exists())
                self.assertTrue(self.branch_exists(repo, name))

            self.git(paths["dirty"], "add", "untracked.txt")
            self.git(paths["dirty"], "commit", "-q", "-m", "preserve dirty branch")
            (paths["locked"] / "locked.txt").write_text("keep me\n", encoding="utf-8")
            self.git(paths["locked"], "add", "locked.txt")
            self.git(paths["locked"], "commit", "-q", "-m", "preserve locked branch")
            (paths["ignored"] / "artifact.cache").unlink()
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
            self.assertIn("Applied: removed 3 checkout(s)", applied.stdout)
            self.assertRegex(
                applied.stdout,
                r"Reclaimed checkout space: (?!0 B)\d+(?:\.\d+)? [KMGT]?i?B",
            )
            self.assertFalse(paths["busy"].exists())
            self.assertFalse(paths["ignored"].exists())
            self.assertFalse(paths["remove_fail"].exists())
            self.assertFalse(self.branch_exists(repo, "busy"))
            self.assertFalse(self.branch_exists(repo, "ignored"))
            self.assertFalse(self.branch_exists(repo, "remove_fail"))
            registrations = self.git(repo, "worktree", "list", "--porcelain")
            self.assertNotIn(str(paths["stale"]), registrations)
            for name in ("dirty", "locked", "unmerged"):
                self.assertTrue(paths[name].exists())
                self.assertIn(str(paths[name]), registrations)


if __name__ == "__main__":
    unittest.main()
