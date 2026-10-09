#!/usr/bin/env python3
"""Render the imported upstream file inventory from the live E2E runner gates."""
import argparse
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUTPUT = ROOT / 'docs/compatibility/upstream-test-inventory.md'


def render():
    runner = (ROOT / 'test/test-suite-tooling/TestRunner.ml').read_text()
    files_block = runner.split('let disabledUpstreamFiles =', 1)[1].split('\n    in', 1)[0]
    lines_block = runner.split('let disabledUpstreamLines =', 1)[1].split('\n    in', 1)[0]
    disabled = re.findall(r'"(test/fixtures/e2e/upstream/[^"\n]+)"', files_block)
    entries = re.findall(r'\(\s*"([^"]+)"\s*,\s*\[([^]]*)\]\s*\)', lines_block)
    lines = {path: [int(n) for n in re.findall(r'\d+', numbers)] for path, numbers in entries}
    paths = sorted(p.relative_to(ROOT).as_posix() for p in (ROOT / 'test/fixtures/e2e/upstream').rglob('*.dark'))
    if len(disabled) != len(set(disabled)) or len(entries) != len(lines):
        raise ValueError('Duplicate upstream gate paths')
    unknown = (set(disabled) | set(lines)) - set(paths)
    if unknown:
        raise ValueError(f'Gate paths absent from corpus: {sorted(unknown)}')
    inert = []
    for path, numbers in lines.items():
        source = (ROOT / path).read_text().splitlines()
        if len(numbers) != len(set(numbers)):
            raise ValueError(f'Duplicate line entries in {path}')
        for number in numbers:
            if not 1 <= number <= len(source):
                raise ValueError(f'Out-of-range gate: {path}:{number}')
            text = source[number - 1].strip()
            if not text or text.startswith('//'):
                inert.append((path, number, 'blank' if not text else 'comment'))
    total = sum(map(len, lines.values()))
    out = [
        '# Imported upstream test inventory', '',
        'Generated from `test/test-suite-tooling/TestRunner.ml` and the imported',
        '`test/fixtures/e2e/upstream/**/*.dark` files. Regenerate with',
        '`python3 scripts/audit-upstream-gates.py`; verify with `--check`.', '',
        f'**{len(paths)} files; {len(disabled)} whole-file exclusions; {total} line-number entries',
        f'across {len(lines)} files.** A line entry is not a skipped-test count.',
        'The runner matches the `L<number>:` assertion name produced by the fixture',
        'parser. Declarations and multiline assertions require parser inspection;',
        'the source line alone does not establish whether a gate suppresses a test.',
        'Other skip metadata and runtime filters are outside this inventory.', '',
        '| Imported file | Whole-file gate | Individual line entries |',
        '| --- | --- | --- |',
    ]
    prefix = 'test/fixtures/e2e/upstream/'
    for path in paths:
        name = path.removeprefix(prefix)
        numbers = ', '.join(map(str, lines.get(path, []))) or '—'
        out.append(f'| [{name}](../../{path}) | {"disabled" if path in disabled else "enabled"} | {numbers} |')
    out += ['', '## Blank and comment line entries', '',
            'These stale-looking entries need assertion-location revalidation before',
            'editing the runner. They must not be counted as unsupported expressions.', '',
            '| File | Line | Source line |', '| --- | --- | --- |']
    for path, number, kind in inert:
        out.append(f'| [{path.removeprefix(prefix)}](../../{path}) | {number} | {kind} |')
    out += ['', f'{len(inert)} of the {total} entries point to blank or comment lines.', '']
    return '\n'.join(out)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true', help='check without writing')
    args = parser.parse_args()
    content = render()
    if args.check:
        if not OUTPUT.exists() or OUTPUT.read_text() != content:
            parser.exit(1, 'Upstream inventory is stale; regenerate it.\n')
        print('Upstream inventory matches the live corpus and runner gates.')
    else:
        OUTPUT.write_text(content)
        print(f'Wrote {OUTPUT.relative_to(ROOT)}')


if __name__ == '__main__':
    main()
