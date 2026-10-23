#!/usr/bin/env python3
"""Require native source inventory and runner discovery to agree on every test."""
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile


def main():
    parser, runner = (Path(arg).resolve() for arg in sys.argv[1:3])
    root = Path(sys.argv[3]).resolve()
    sys.path.insert(0, str(root / 'scripts'))
    spec = importlib.util.spec_from_file_location('source_ledger', root / 'scripts/update-ledger.py')
    ledger = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(ledger)
    paths = ['upstream/' + p for p in ledger.imported_paths(root)]
    with tempfile.TemporaryDirectory(prefix='imported-inventory-') as temporary:
        manifest = Path(temporary) / 'manifest.json'
        source_report = Path(temporary) / 'source.json'
        test_report = Path(temporary) / 'tests.json'
        manifest.write_text(json.dumps(paths))
        subprocess.run([str(parser), str(root), str(manifest),
                        str(root / 'porting/compiler-reasons.json'), str(source_report)], check=True, cwd=root)
        subprocess.run([str(runner), str(manifest), str(test_report)], check=True, cwd=root)
        sources = json.loads(source_report.read_text())['sources']
        tests = json.loads(test_report.read_text())
    assert [r['source'] for r in sources] == paths == [r['source'] for r in tests]
    for source, discovered in zip(sources, tests):
        assert source['parsed'], (source['source'], source['diagnostics'])
        assert 'error' not in discovered, discovered
        expected = [i['line'] for i in source['items'] if i['kind'] in ledger.TEST_KINDS]
        actual = [t['line'] for t in discovered['tests']]
        assert actual == expected, (source['source'], expected, actual)
    summary = ledger.summarize(root, sources)
    total = sum(summary['dispositions']['tests'].values())
    print(f'Complete imported corpus: {len(paths)} files, {total} tests, matching native discovery and no pending classifications')


if __name__ == '__main__':
    main()
