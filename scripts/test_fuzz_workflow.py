#!/usr/bin/env python3
"""Verify separate fix/landing approvals and the committed native-runtime handoff."""
from pathlib import Path
import os
import shutil
import subprocess
import tempfile
import unittest


FUZZ = Path(__file__).resolve().parents[1] / "fuzz"


class WorkflowTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="dark-fuzz-workflow-")
        self.addCleanup(self.temporary.cleanup)
        self.repo = Path(self.temporary.name) / "repo"
        self.repo.mkdir()
        self.bin = Path(self.temporary.name) / "bin"
        self.bin.mkdir()
        self.git("init", "-q", "-b", "main")
        self.git("config", "user.name", "Fuzzer Test")
        self.git("config", "user.email", "fuzzer@example.invalid")
        shutil.copy2(FUZZ, self.repo / "fuzz")
        (self.repo / ".mergetrain.yaml").write_text("git:\n  remote: local\n  integration_branch: main\n")
        (self.repo / ".gitignore").write_text("_build/\nfuzz-results/\n*-calls.txt\n")
        self.executable(self.repo / "build", '#!/bin/sh\nmkdir -p _build/default/tools/fuzzer\ncp worker.py _build/default/tools/fuzzer/main.exe\nchmod +x _build/default/tools/fuzzer/main.exe\n')
        self.executable(self.repo / "run-tests", "#!/bin/sh\nexit 0\n")
        self.executable(self.repo / "benchmarks/run_benchmarks.sh", "#!/bin/sh\nexit 0\n")
        self.executable(self.repo / "land", "#!/bin/sh\ngit update-ref refs/remotes/local/main HEAD\necho queued\n")
        self.executable(self.bin / "darklang-interpreter", "#!/bin/sh\necho 1\n")
        self.executable(self.repo / "worker.py", f'''#!/usr/bin/env python3
from pathlib import Path
import sys
VERSION = 0
root = Path({str(self.repo)!r})
args = sys.argv[1:]
if '--minimize' in args:
    source = Path(args[args.index('--minimize') + 1])
    source.with_suffix('.min.dark').write_text(source.read_text())
    sys.exit(0)
if '--replay' in args:
    source = Path(args[args.index('--replay') + 1]).read_text()
    version = int(source.splitlines()[0].split()[-1])
    sys.exit(0 if VERSION > version else 1)
if '--artifacts' in args:
    calls = root / 'fuzz-calls.txt'
    with calls.open('a') as output:
        output.write(str(VERSION) + ':' + sys.argv[0] + '\\n')
    if VERSION > 0 and not ((root / 'two-findings').exists() and VERSION == 1):
        sys.exit(0)
    artifact = Path(args[args.index('--artifacts') + 1])
    (artifact / 'seed-1-case-0.dark').write_text('// finding version ' + str(VERSION) + '\\n1L\\n')
    sys.exit(1)
sys.exit(2)
''')
        self.executable(self.bin / "codex", '''#!/usr/bin/env python3
from pathlib import Path
import subprocess
import sys
args = sys.argv[1:]
worktree = Path(args[args.index('-C') + 1])
assert 'Do not run ./land' in sys.stdin.read()
worker = worktree / 'worker.py'
text = worker.read_text()
version = int(text.split('VERSION = ', 1)[1].splitlines()[0])
worker.write_text(text.replace('VERSION = ' + str(version), 'VERSION = ' + str(version + 1)))
subprocess.run(['git', 'add', 'worker.py'], cwd=worktree, check=True)
subprocess.run(['git', 'commit', '-qm', 'fix fuzzer case ' + str(version + 1)], cwd=worktree, check=True)
Path(args[args.index('--output-last-message') + 1]).write_text('Fix committed.\\n')
''')
        self.git("add", ".")
        self.git("commit", "-qm", "initial")
        self.git("remote", "add", "local", str(self.repo))
        self.git("update-ref", "refs/remotes/local/main", "HEAD")

    def executable(self, path, source):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(source)
        path.chmod(0o755)

    def git(self, *arguments):
        return subprocess.check_output(["git", *arguments], cwd=self.repo, text=True).strip()

    def run_fuzz(self, answers):
        environment = dict(os.environ, PATH=str(self.bin) + ":" + os.environ["PATH"])
        return subprocess.run(["./fuzz", "--seed", "1", "--worktree-dir", self.temporary.name],
                              cwd=self.repo, env=environment, input=answers, text=True,
                              capture_output=True, timeout=30, check=False)

    def test_fix_and_landing_approvals_use_committed_runtime(self):
        result = self.run_fuzz("yes\nyes\n")
        self.assertIn("Fix queued at", result.stdout, result.stdout + result.stderr)
        self.assertEqual(self.git("log", "-1", "--format=%s", "local/main"), "fix fuzzer case 1")
        calls = (self.repo / "fuzz-calls.txt").read_text().splitlines()
        self.assertTrue(calls[0].startswith("0:") and calls[0].endswith("main.exe"), calls)
        self.assertTrue(calls[1].startswith("1:") and "codex-runtime." in calls[1], calls)

    def test_declining_landing_preserves_fix_off_integration(self):
        result = self.run_fuzz("yes\nno\n")
        self.assertIn("Fix retained without landing", result.stderr, result.stdout + result.stderr)
        self.assertEqual(self.git("log", "-1", "--format=%s", "local/main"), "initial")

    def test_declining_fix_preserves_finding(self):
        result = self.run_fuzz("no\n")
        self.assertIn("Finding preserved without a compiler fix", result.stdout)
        self.assertEqual(self.git("branch", "--list", "fuzz/fix-*"), "")
        self.assertTrue(list((self.repo / "fuzz-results").glob("run.*/seed-*-case-*.min.dark")))

    def test_second_fix_starts_from_integrated_first_fix(self):
        (self.repo / "two-findings").write_text("yes\n")
        result = self.run_fuzz("yes\nyes\nyes\nyes\n")
        self.assertEqual(result.stdout.count("Fix queued at"), 2, result.stdout + result.stderr)
        self.assertEqual(self.git("log", "-2", "--format=%s", "local/main").splitlines(),
                         ["fix fuzzer case 2", "fix fuzzer case 1"])


if __name__ == "__main__":
    unittest.main()
