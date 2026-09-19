"""test_prune_worktrees.py - Focused tests for safe worktree cleanup."""

from __future__ import annotations

import shutil
import subprocess
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
            repo.mkdir()
            self.git(repo, "init", "-q", "-b", "main")
            self.git(repo, "config", "user.email", "worktree-test@example.invalid")
            self.git(repo, "config", "user.name", "Worktree Test")
            (repo / "base.txt").write_text("base\n", encoding="utf-8")
            self.git(repo, "add", "base.txt")
            self.git(repo, "commit", "-q", "-m", "base")
            self.git(repo, "update-ref", "refs/remotes/origin/main", "HEAD")

            paths = {
                name: root / name
                for name in ("merged", "stale", "dirty", "locked", "unmerged")
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

            dry_run = subprocess.run(
                ["python3", str(source_script), "--delete-branches"],
                cwd=repo,
                check=True,
                text=True,
                capture_output=True,
            )
            self.assertIn(f"REMOVE {paths['merged']}", dry_run.stdout)
            self.assertIn(f"PRUNE {paths['stale']}", dry_run.stdout)
            self.assertIn(f"KEEP  {paths['dirty']}", dry_run.stdout)
            self.assertIn(f"KEEP  {paths['locked']}", dry_run.stdout)
            self.assertIn(f"KEEP  {paths['unmerged']}", dry_run.stdout)
            self.assertIn("Dry run: 1 checkout(s) removable", dry_run.stdout)
            self.assertTrue(paths["merged"].exists())
            self.assertTrue(self.branch_exists(repo, "merged"))

            from_linked_worktree = subprocess.run(
                ["python3", str(source_script)],
                cwd=paths["merged"],
                check=True,
                text=True,
                capture_output=True,
            )
            self.assertIn(
                f"KEEP  {paths['merged']} (merged): running worktree",
                from_linked_worktree.stdout,
            )

            applied = subprocess.run(
                [
                    "python3",
                    str(source_script),
                    "--apply",
                    "--delete-branches",
                ],
                cwd=repo,
                check=True,
                text=True,
                capture_output=True,
            )
            self.assertIn("Applied: removed 1 checkout(s)", applied.stdout)
            self.assertFalse(paths["merged"].exists())
            self.assertFalse(self.branch_exists(repo, "merged"))
            self.assertFalse(self.branch_exists(repo, "stale"))
            registrations = self.git(repo, "worktree", "list", "--porcelain")
            self.assertNotIn(str(paths["stale"]), registrations)
            for name in ("dirty", "locked", "unmerged"):
                self.assertTrue(paths[name].exists())
                self.assertIn(str(paths[name]), registrations)


if __name__ == "__main__":
    unittest.main()
