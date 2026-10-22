#!/usr/bin/env python3
"""Parse every pinned upstream package and execution fixture without compiling it."""
from __future__ import annotations

import argparse
import json
from pathlib import Path
import subprocess
import tempfile

import ledger_sources as sources


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--upstream', required=True, type=Path)
    parser.add_argument('--build-dir', type=Path, default=Path('_build'))
    parser.add_argument('--report', type=Path, default=Path('TestResults/porting/parsing.json'))
    args = parser.parse_args()
    root = sources.ROOT
    pin = sources.read_yaml(root / 'porting/sources.yaml')['upstream_revision']
    blobs = sources.checked_files(args.upstream.resolve(), pin)
    paths = sorted(path for path in blobs if path.endswith('.dark')
                   and path.startswith(('packages/', 'backend/testfiles/execution/')))
    executable = root / args.build_dir / 'default/tools/ledger/parse_sources.exe'
    if not executable.is_file():
        parser.error('Build the native parser adapter with ./build --ai first.')
    report = args.report.resolve()
    report.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='upstream-parsing-') as temporary:
        directory = Path(temporary)
        for path in paths:
            destination = directory / path
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_bytes(blobs[path])
        manifest = directory / 'manifest.json'
        manifest.write_text(json.dumps(paths))
        subprocess.run([str(executable), str(directory), str(manifest),
                        str(root / 'porting/compiler-reasons.json'), str(report)],
                       cwd=root, check=True)
    result = json.loads(report.read_text())
    if [row['source'] for row in result['sources']] != paths:
        raise ValueError('Parser report did not account for every pinned source')
    result['upstream_revision'] = pin
    report.write_text(json.dumps(result, indent=2) + '\n')
    failed = [row for row in result['sources'] if not row['parsed']]
    print(f'Parsed {len(paths) - len(failed)}/{len(paths)} pinned upstream files; report: {report}')
    for row in failed[:10]:
        print(row['source'] + ': ' + (row['diagnostics'][0] if row['diagnostics'] else 'No syntax tree'))
    if len(failed) > 10:
        print(f'{len(failed) - 10} more failing files are recorded in the report.')
    return bool(failed)


if __name__ == '__main__':
    raise SystemExit(main())
