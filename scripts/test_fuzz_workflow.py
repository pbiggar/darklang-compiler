"""test_fuzz_workflow.py - Exercise the fuzzer's two approvals and fixed-runtime handoff."""

from pathlib import Path
import os
import shutil
import subprocess
import tempfile
import unittest


FUZZ = Path(__file__).resolve().parents[1] / "fuzz"


class FuzzWorkflowTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.repo = Path(self.temporary.name) / "repo"
        self.repo.mkdir()
        self.bin = Path(self.temporary.name) / "bin"
        self.bin.mkdir()
        self.git("init", "-q", "-b", "main")
        self.git("config", "user.name", "Fuzzer Test")
        self.git("config", "user.email", "fuzzer@example.invalid")
        shutil.copy2(FUZZ, self.repo / "fuzz")
        (self.repo / ".mergetrain.yaml").write_text(
            "git:\n  remote: local\n  integration_branch: main\n", encoding="utf-8"
        )
        (self.repo / "build").write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
        (self.repo / "run-tests").write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
        benchmarks = self.repo / "benchmarks"
        benchmarks.mkdir()
        helper = self.repo / "scripts"
        helper.mkdir()
        (helper / "dotnet-host").write_text("#!/bin/sh\nexec dotnet \"$@\"\n", encoding="utf-8")
        (benchmarks / "run_benchmarks.sh").write_text(
            "#!/bin/sh\nexit 0\n", encoding="utf-8"
        )
        (self.repo / "land").write_text(
            "#!/bin/sh\n"
            "git update-ref refs/remotes/local/main HEAD\n"
            "echo queued\n",
            encoding="utf-8",
        )
        for executable in ["fuzz", "build", "run-tests", "land", "benchmarks/run_benchmarks.sh", "scripts/dotnet-host"]:
            (self.repo / executable).chmod(0o755)
        self.git("add", ".")
        self.git("commit", "-qm", "initial")
        self.git("remote", "add", "local", str(self.repo))
        self.git("update-ref", "refs/remotes/local/main", "HEAD")

        self.fake("darklang-interpreter", "#!/bin/sh\necho 1\n")
        self.fake("dotnet", f'''#!/usr/bin/env python3
from pathlib import Path
import sys
args = sys.argv[1:]
root = Path({str(self.repo)!r})
if args and args[0] == 'publish':
    output = Path(args[args.index('--output') + 1])
    output.mkdir(parents=True, exist_ok=True)
    (output / 'Fuzzer.dll').write_text('fixed', encoding='utf-8')
    count_file = root / 'publish-count.txt'
    count = int(count_file.read_text(encoding='utf-8')) if count_file.exists() else 0
    count_file.write_text(str(count + 1), encoding='utf-8')
    sys.exit(0)
if '--minimize' in args:
    source = Path(args[args.index('--minimize') + 1])
    source.with_suffix('.min.dark').write_text(source.read_text(encoding='utf-8'), encoding='utf-8')
    sys.exit(0)
if '--replay' in args:
    if (root / 'two-findings').exists() and (root / 'fuzz-calls.txt').exists():
        calls = (root / 'fuzz-calls.txt').read_text(encoding='utf-8').splitlines()
        published = int((root / 'publish-count.txt').read_text(encoding='utf-8')) if (root / 'publish-count.txt').exists() else 0
        if len(calls) == 2 and published == 1:
            sys.exit(1)
    sys.exit(0 if args and args[0].endswith('Fuzzer.dll') else 1)
if '--artifacts' in args:
    with (root / 'fuzz-calls.txt').open('a', encoding='utf-8') as calls:
        calls.write(args[0] + '\\n')
    call_count = len((root / 'fuzz-calls.txt').read_text(encoding='utf-8').splitlines())
    if args and args[0].endswith('Fuzzer.dll') and not ((root / 'two-findings').exists() and call_count == 2):
        sys.exit(0)
    artifact = Path(args[args.index('--artifacts') + 1])
    (artifact / 'seed-1-case-0.dark').write_text('1 + 0\\n', encoding='utf-8')
    sys.exit(1)
sys.exit(2)
''')
        self.fake("codex", '''#!/usr/bin/env python3
from pathlib import Path
import subprocess
import sys
args = sys.argv[1:]
worktree = Path(args[args.index('-C') + 1])
prompt = sys.stdin.read()
assert 'Do not run ./land' in prompt
number = worktree.name.rsplit('-', 1)[-1]
filename = f'fix-{number}.txt'
(worktree / filename).write_text('fixed\\n', encoding='utf-8')
subprocess.run(['git', 'add', filename], cwd=worktree, check=True)
subprocess.run(['git', 'commit', '-qm', f'fix fuzzer case {number}'], cwd=worktree, check=True)
Path(args[args.index('--output-last-message') + 1]).write_text('Fix committed.\\n', encoding='utf-8')
''')

    def git(self, *args: str) -> str:
        return subprocess.check_output(["git", *args], cwd=self.repo, text=True).strip()

    def fake(self, name: str, source: str) -> None:
        path = self.bin / name
        path.write_text(source, encoding="utf-8")
        path.chmod(0o755)

    def run_fuzz(self, answers: str) -> subprocess.CompletedProcess[str]:
        environment = dict(os.environ)
        environment["PATH"] = f"{self.bin}:{environment['PATH']}"
        return subprocess.run(
            ["./fuzz", "--seed", "1"], cwd=self.repo, env=environment,
            input=answers, text=True, capture_output=True, timeout=30,
        )

    def test_start_and_land_approvals_publish_the_committed_runtime(self) -> None:
        result = self.run_fuzz("yes\nyes\n")
        self.assertIn("Fix queued at", result.stdout)
        self.assertIn("continuing with its published compiler", result.stdout)
        self.assertEqual(self.git("log", "-1", "--format=%s", "local/main"), "fix fuzzer case 1")
        calls = (self.repo / "fuzz-calls.txt").read_text(encoding="utf-8").splitlines()
        self.assertEqual(calls[0], "run")
        self.assertTrue(calls[1].endswith("Fuzzer.dll"))

    def test_declining_landing_keeps_the_commit_off_integration(self) -> None:
        result = self.run_fuzz("yes\nno\n")
        self.assertIn("Fix retained without landing", result.stderr)
        self.assertEqual(self.git("log", "-1", "--format=%s", "local/main"), "initial")
        calls = (self.repo / "fuzz-calls.txt").read_text(encoding="utf-8").splitlines()
        self.assertEqual(calls, ["run"])

    def test_declining_start_preserves_finding_without_creating_a_branch(self) -> None:
        result = self.run_fuzz("no\n")
        self.assertIn("Finding preserved without a compiler fix", result.stdout)
        self.assertEqual(self.git("branch", "--list", "fuzz/fix-*"), "")
        self.assertEqual(self.git("log", "-1", "--format=%s", "local/main"), "initial")
        self.assertGreaterEqual(len(list((self.repo / "fuzz-results").glob("run.*/seed-*-case-*.min.dark"))), 1)

    def test_second_fix_branches_from_integrated_first_fix(self) -> None:
        (self.repo / "two-findings").write_text("yes\n", encoding="utf-8")
        result = self.run_fuzz("yes\nyes\nyes\nyes\n")
        self.assertEqual(result.stdout.count("Fix queued at"), 2, result.stdout + result.stderr)
        self.assertEqual(self.git("log", "-1", "--format=%s", "local/main"), "fix fuzzer case 2")
        self.assertEqual(self.git("log", "-2", "--format=%s", "local/main").splitlines(),
                         ["fix fuzzer case 2", "fix fuzzer case 1"])
        self.assertTrue((self.repo / "fuzz-calls.txt").read_text(encoding="utf-8").splitlines()[1].endswith("Fuzzer.dll"))


if __name__ == "__main__":
    unittest.main()
