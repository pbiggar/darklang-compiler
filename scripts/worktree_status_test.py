#!/usr/bin/env python3
"""Unit tests for the worktree status helper script."""

import importlib.util
import pathlib
import unittest


SCRIPT_PATH = pathlib.Path(__file__).with_name("worktree-status.py")


def load_module():
    spec = importlib.util.spec_from_file_location("worktree_status", SCRIPT_PATH)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class WorktreeStatusTests(unittest.TestCase):
    def test_render_status_only_queries_visible_worktrees(self):
        module = load_module()
        queried_paths = []

        worktrees = [
            {
                "path": f"/repo/worktree-{i}",
                "branch": "main" if i == 0 else f"branch-{i:02d}",
                "display": "main" if i == 0 else f"branch-{i:02d}",
                "prunable": False,
            }
            for i in range(30)
        ]

        module.resolve_repo_root = lambda: "/repo/main"
        module.get_worktrees = lambda repo_root: worktrees
        module.get_log_entries = lambda repo_root: []
        module.os.path.isdir = lambda path: True

        def fake_run_git(args, cwd=None, check=False):
            if args == ["rev-parse", "HEAD"]:
                queried_paths.append(cwd)
                return "abc123"
            if args == ["status", "--porcelain=1"]:
                return ""
            if args == ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"]:
                return ""
            if args[:3] == ["rev-list", "--count", "main..HEAD"]:
                return "0"
            if args[:3] == ["rev-list", "--count", "HEAD..main"]:
                return "0"
            return ""

        module.run_git = fake_run_git

        module.render_status()

        self.assertEqual(len(queried_paths), 10)
        self.assertNotIn("/repo/worktree-10", queried_paths)


if __name__ == "__main__":
    unittest.main()
