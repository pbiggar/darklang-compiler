#!/usr/bin/env python3
"""Count compiler dispositions directly from imported Dark source annotations."""
from __future__ import annotations
import argparse
from collections import Counter, defaultdict
import json
from pathlib import Path
import subprocess
import tempfile
import yaml
import ledger_sources as sources

ROOT = sources.ROOT
OUTPUT = 'porting/generated/summary.yaml'
REPORT = 'docs/porting/generated/ledger.md'
TEST_KINDS = {'named-test', 'assertion'}


def imported_paths(root):
    paths = [line.strip() for line in (root / 'upstream/sources.list').read_text().splitlines()
             if line.strip() and not line.startswith('#')]
    if paths != sorted(set(paths)):
        raise ValueError('Imported paths must be unique and sorted')
    for path in paths:
        p = Path(path)
        if p.is_absolute() or '..' in p.parts or p.suffix != '.dark' or not path.startswith(
                ('packages/', 'backend/testfiles/execution/')):
            raise ValueError('Invalid imported path: ' + path)
    actual = {str(p.relative_to(root / 'upstream')) for directory in ('packages', 'backend/testfiles/execution')
              for p in (root / 'upstream' / directory).rglob('*.dark')}
    if set(paths) != actual:
        raise ValueError('Imported manifest differs from the complete source tree')
    return paths


def inventory(root, build_dir):
    paths = imported_paths(root)
    adapter = root / build_dir / 'default/tools/ledger/parse_sources.exe'
    with tempfile.TemporaryDirectory(prefix='source-ledger-') as temporary:
        manifest, report = (Path(temporary) / name for name in ('manifest.json', 'inventory.json'))
        manifest.write_text(json.dumps(['upstream/' + p for p in paths]))
        subprocess.run([str(adapter), str(root), str(manifest),
                        str(root / 'porting/compiler-reasons.json'), str(report)], check=True, cwd=root)
        rows = json.loads(report.read_text())['sources']
    if [r['source'] for r in rows] != ['upstream/' + p for p in paths]:
        raise ValueError('Native inventory did not account for every imported source')
    failures = [r for r in rows if not r['parsed']]
    if failures:
        raise ValueError('\n'.join(r['source'] + ': ' + '; '.join(r['diagnostics']) for r in failures))
    return rows


def verify_pin(root, upstream, rows):
    pin = sources.read_yaml(root / 'porting/sources.yaml')['upstream_revision']
    blobs = sources.checked_files(upstream, pin)
    expected = {p for p in blobs if p.endswith('.dark') and p.startswith(
        ('packages/', 'backend/testfiles/execution/'))}
    if expected != {r['source'].removeprefix('upstream/') for r in rows}:
        raise ValueError('Imported inventory does not contain the complete pinned corpus')
    for row in rows:
        path = row['source']
        # Real lexer comment positions exclude annotation-like text in literals.
        annotations = set(row['annotation_lines'])
        original = b''.join(line for number, line in enumerate((root / path).read_bytes().splitlines(keepends=True), 1)
                            if number not in annotations)
        if original != blobs[path.removeprefix('upstream/')]:
            raise ValueError('Imported source body differs from the pin: ' + path)


def capabilities(root, filename):
    value = sources.read_yaml(root / filename)
    if set(value) != {'schema', 'groups'} or value['schema'] != 2:
        raise ValueError('Invalid capability ledger: ' + filename)
    counts = Counter()
    for rows in value['groups'].values():
        for name, row in rows.items():
            status = row if isinstance(row, str) else row.get('status')
            if status not in {'supported', 'partial', 'missing', 'unassessed', 'excluded'}:
                raise ValueError('Invalid capability status: ' + name)
            if status in {'partial', 'missing', 'excluded'} and (not isinstance(row, dict) or not row.get('notes')):
                raise ValueError('Explain capability gap: ' + name)
            counts[status] += 1
    return dict(sorted(counts.items()))


def summarize(root, rows):
    counts, reasons, kinds, identities = defaultdict(Counter), defaultdict(Counter), Counter(), set()
    for row in rows:
        for item in row['items']:
            identity = (row['source'], item['kind'], item['name'], item['line'], item['column'])
            if identity in identities:
                raise ValueError('Duplicate native inventory entry: ' + repr(identity))
            identities.add(identity)
            area = 'tests' if item['kind'] in TEST_KINDS else 'stdlib' if row['source'].startswith(
                'upstream/packages/darklang/stdlib/') else 'packages'
            disposition = item['compiler']
            if disposition.get('reason') == 'pending-validation':
                raise ValueError('Unclassified imported test: ' + row['source'] + '#' + item['name'])
            counts[area][disposition['status']] += 1
            if 'reason' in disposition:
                reasons[area][disposition['reason']] += 1
            kinds[item['kind']] += 1
    builtin = sources.read_yaml(root / 'porting/builtins.yaml')
    builtin_counts = Counter(status if isinstance(status, str) else status['status']
                             for items in builtin['files'].values() for status in items.values())
    return dict(upstream=sources.read_yaml(root / 'porting/sources.yaml'), source_files=len(rows),
                kinds=dict(sorted(kinds.items())), features=capabilities(root, 'porting/language-features.yaml'),
                parser=capabilities(root, 'porting/parser.yaml'), builtins=dict(sorted(builtin_counts.items())),
                dispositions={area: dict(sorted(values.items())) for area, values in sorted(counts.items())},
                reasons={area: dict(sorted(values.items())) for area, values in sorted(reasons.items())})


def markdown(summary):
    lines = ['# Current compiler port status', '', '[How to read and update this ledger](../README.md).', '',
             'Interpreter revision: `' + summary['upstream']['upstream_revision'] + '`.', '',
             f"The complete corpus contains {summary['source_files']:,} source files. Items are counted directly from parsed source annotations.", '',
             'A run disposition means selected, not a passing execution receipt. Replacement contracts are checked independently.', '',
             '| Area | Source dispositions |', '| --- | --- |']
    for area, counts in summary['dispositions'].items():
        lines.append('| ' + area + ' | ' + ', '.join(f'{n:,} {status}' for status, n in counts.items()) + ' |')
    lines += ['', '| Kind | Items |', '| --- | --- |']
    lines += [f'| {kind} | {n:,} |' for kind, n in summary['kinds'].items()]
    lines += ['', '## Reasons', '', 'Tags are defined in [the glossary](../../../porting/compiler-reasons.json).', '',
              '| Area | Tag | Items |', '| --- | --- | --- |']
    lines += [f'| {area} | `{reason}` | {n:,} |' for area, reasons in summary['reasons'].items() for reason, n in reasons.items()]
    lines += ['', '## Separate capability assessments', '', '| Area | Assessments |', '| --- | --- |']
    for area in ('features', 'parser', 'builtins'):
        lines.append('| ' + area + ' | ' + ', '.join(f'{n:,} {status}' for status, n in summary[area].items()) + ' |')
    return '\n'.join(lines) + '\n'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-dir', type=Path, default=Path('_build'))
    parser.add_argument('--upstream', type=Path, help='Also verify source bodies against the pinned Git blobs')
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    subprocess.run([str(ROOT / args.build_dir / 'default/tools/ledger/check_sources.exe'), str(ROOT)], check=True, cwd=ROOT)
    rows = inventory(ROOT, args.build_dir)
    if args.upstream:
        verify_pin(ROOT, args.upstream.resolve(), rows)
    summary = summarize(ROOT, rows)
    stale = []
    for name, text in {OUTPUT: yaml.safe_dump(summary, sort_keys=False), REPORT: markdown(summary)}.items():
        path = ROOT / name
        if args.check:
            if not path.exists() or path.read_text() != text:
                stale.append(name)
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text)
    if stale:
        raise ValueError('Stale generated ledger: ' + ', '.join(stale))
    print(f"Source ledger: {len(rows)} files, {sum(summary['kinds'].values())} items")


if __name__ == '__main__':
    main()
