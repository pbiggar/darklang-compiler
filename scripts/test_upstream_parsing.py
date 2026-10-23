#!/usr/bin/env python3
"""Exercise pinned-source completeness and diagnostics through the native parser."""
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


def run():
    adapter = Path(sys.argv[1]).resolve()
    scripts = Path(__file__).resolve().parent
    sys.path.insert(0, str(scripts))
    spec = importlib.util.spec_from_file_location('check_upstream_parsing', scripts / 'check-upstream-parsing.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    with tempfile.TemporaryDirectory() as temporary:
        directory = Path(temporary)
        upstream = directory / 'interpreter'
        root = directory / 'compiler'
        upstream.mkdir()
        (root / 'porting').mkdir(parents=True)
        executable = root / '_build/default/tools/ledger/parse_sources.exe'
        executable.parent.mkdir(parents=True)
        shutil.copy2(adapter, executable)
        (root / 'porting/compiler-reasons.json').write_text('{"test-counter":"Per-test counter is missing"}')

        def git(*args):
            return subprocess.check_output(['git', '-C', str(upstream), *args], text=True, stderr=subprocess.PIPE).strip()

        def write(path, body):
            target = upstream / path
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(body)

        def commit():
            git('add', '.')
            git('-c', 'user.name=Parser regression', '-c', 'user.email=parser@example.invalid', 'commit', '-qm', 'Fixture source pin')
            pin = git('rev-parse', 'HEAD')
            (root / 'porting/sources.yaml').write_text('upstream_revision: ' + pin + '\n')
            return pin

        git('init', '-q')
        write('packages/example.dark', 'module Example\nlet wrong () : String = 42\ntest unavailable = Unknown.fn ()')
        write('backend/testfiles/execution/example.dark', 'Unknown.fn () = 42')
        write('backend/testfiles/other/ignored.dark', 'intentionally invalid syntax [')
        write('backend/src/host.fs', 'Host tests are not Dark source')
        pin = commit()
        write('packages/example.dark', 'invalid dirty source [')
        write('packages/untracked.dark', 'invalid untracked source [')
        module.sources.ROOT = root
        report = directory / 'report.json'
        sys.argv = ['check-upstream-parsing.py', '--upstream', str(upstream), '--report', str(report)]
        with contextlib.redirect_stdout(io.StringIO()):
            assert not module.main()
        result = json.loads(report.read_text())
        assert result['upstream_revision'] == pin
        assert [row['source'] for row in result['sources']] == ['backend/testfiles/execution/example.dark', 'packages/example.dark']
        assert all(row['parsed'] for row in result['sources'])
        assert [item['kind'] for item in result['sources'][1]['items']] == ['function', 'named-test']
        (upstream / 'packages/untracked.dark').unlink()
        commit()
        with contextlib.redirect_stdout(io.StringIO()):
            assert module.main()
        assert any(not row['parsed'] and row['diagnostics'] for row in json.loads(report.read_text())['sources'])
    print('Pinned upstream parsing checks preserve scope, committed bytes and complete diagnostics')


if __name__ == '__main__':
    run()
