#!/usr/bin/env python3
"""Source pin helpers reject duplicate YAML keys and preserve committed bytes."""
from pathlib import Path
import subprocess
import tempfile
import unittest
import ledger_sources as sources


class SourceInputs(unittest.TestCase):
    def test_yaml_duplicate_keys_are_not_silently_discarded(self):
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / 'input.yaml'
            path.write_text('same: first\nsame: second\n')
            with self.assertRaisesRegex(ValueError, 'Duplicate YAML key'):
                sources.read_yaml(path)

    def test_git_reads_the_pin_and_ignores_worktree_edits(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            def git(*args):
                return subprocess.check_output(['git', '-C', str(root), *args], stderr=subprocess.PIPE).decode().strip()
            git('init', '-q')
            (root / 'packages').mkdir()
            original = b'val text = "a\\nb"\r\n'
            (root / 'packages/source.dark').write_bytes(original)
            git('add', 'packages/source.dark')
            git('-c', 'user.name=Source pin test', '-c', 'user.email=pin@example.invalid', 'commit', '-qm', 'Pin')
            pin = git('rev-parse', 'HEAD')
            (root / 'packages/source.dark').write_text('dirty')
            (root / 'untracked.dark').write_text('untracked')
            self.assertEqual(sources.checked_files(root, pin), {'packages/source.dark': original})


if __name__ == '__main__':
    unittest.main()
