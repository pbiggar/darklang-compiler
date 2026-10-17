#!/usr/bin/env python3
"""Regenerate the lean porting ledger from pinned sources and existing tests.

No probes, test programs, expected values, or differential corpus are generated.
Maintained decisions live in ledger-inputs.yaml; execution receipts are separate.
"""
from __future__ import annotations

import argparse
from collections import Counter, defaultdict, deque
import json
from pathlib import Path
import re
import subprocess
import sys
import tempfile

import porting as p

ROOT = p.ROOT
INPUTS = 'porting/ledger-inputs.yaml'
RECEIPT = 'porting/test-results.json'
OUTPUT = 'porting/generated/ledger.json'
REPORT = 'docs/porting/generated/ledger.md'


def encoded(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + '\n'


def source_path(value):
    if not isinstance(value, str) or not value or value.startswith('/') or '..' in Path(value).parts:
        raise ValueError(f'Expected a repository-relative source path: {value!r}')
    return value


def read_inputs(root):
    value = p.read_yaml(root / INPUTS)
    fields = {'schema', 'features', 'source_mappings', 'parser_mappings',
              'builtin_mappings', 'coverage', 'source_exceptions'}
    if set(value) != fields or value['schema'] != 1 or not isinstance(value['features'], list):
        raise ValueError('Invalid ledger-inputs.yaml schema')
    for key in fields - {'schema', 'features'}:
        if not isinstance(value[key], dict):
            raise ValueError(f'{key} must be a mapping')
    seen = set()
    for feature in value['features']:
        if (not isinstance(feature, dict) or set(feature) - {'name', 'status', 'sources', 'notes', 'reviewed_sha256'}
                or feature.get('status') not in {'implemented', 'partial', 'missing', 'not-assessed'}
                or not isinstance(feature.get('name'), str) or feature['name'] in seen
                or not isinstance(feature.get('sources'), list) or not feature['sources']):
            raise ValueError('Invalid or duplicate maintained language feature')
        seen.add(feature['name'])
        for path in feature['sources']:
            source_path(path)
    return value


def declarations(path, text):
    """Bounded source index using the existing scanner's module/offside rules.

    References below are lexical coverage hints, not proof that a branch ran.
    Unknown declaration headers are returned for review instead of discarded.
    """
    clean = p.strip_comments(text, True).splitlines()
    root, modules, rows, review = (), [], [], []
    for number, line in enumerate(clean, 1):
        if not line.strip():
            continue
        indent = len(line) - len(line.lstrip())
        while modules and indent <= modules[-1][0]:
            modules.pop()
        if modules and modules[-1][2] is None:
            col, names, _ = modules[-1]
            modules[-1] = (col, names, indent)
        column = modules[-1][2] if modules else 0
        if indent != column:
            continue
        statement = line.strip()
        if statement.startswith('module '):
            match = re.fullmatch(r'module\s+([\w\x27]+(?:\.[\w\x27]+)*)(\s*=)?\s*', statement)
            if not match:
                review.append(f'{path}:{number}: unindexed module header')
                continue
            names = tuple(match[1].split('.'))
            if match[2]:
                modules.append((indent, names, None))
            else:
                root, modules = names, []
            continue
        match = re.match(r'(let|val|type|test)\s+([\w\x27]+)\b', statement)
        if not match:
            if re.match(r'(let|val|type|test|trait|impl)\b', statement):
                review.append(f'{path}:{number}: unindexed declaration header')
            continue
        scope = root + tuple(part for _, names, _ in modules for part in names)
        end = number
        while end < len(clean) and (not clean[end].strip() or len(clean[end]) - len(clean[end].lstrip()) > indent):
            end += 1
        name = '.'.join(scope + (match[2],))
        rows.append(dict(id=path + '#' + name, name=name, source=path, line=number,
                         kind={'let': 'function', 'val': 'value', 'type': 'type', 'test': 'test'}[match[1]],
                         scope=scope, body='\n'.join(clean[number - 1:end])))
    return rows, review


def coverage_index(upstream, compiler, maintained):
    rows, review = [], []
    for path, body in sorted(upstream.items()):
        if path.startswith('packages/') and path.endswith('.dark'):
            found, warnings = declarations(path, body.decode())
            rows.extend(found)
            review.extend(warnings)
    by_name = defaultdict(list)
    for row in rows:
        by_name[row['name']].append(row['id'])
    unique = {name: ids[0] for name, ids in by_name.items() if len(ids) == 1}
    review.extend('ambiguous declaration: ' + name for name, ids in sorted(by_name.items()) if len(ids) > 1)

    def references(body, scope):
        result = set()
        # Avoid attributing a local binding/parameter to a package declaration.
        locals_ = set(re.findall(r'\blet\s+([\w\x27]+)|\(([\w\x27]+)\s*:', body))
        shadowed = {name for pair in locals_ for name in pair if name}
        for token in set(re.findall(r"(?<![\w'])\b[A-Za-z_][\w\x27]*(?:\.[A-Za-z_][\w\x27]*)*", body)):
            if token.split('.')[0] in shadowed:
                continue
            token = 'Darklang.' + token if token.startswith('Stdlib.') else token
            choices = [token] + ['.'.join(scope[:n] + (token,)) for n in range(len(scope), 0, -1)]
            for choice in choices:
                # A constructor/field can reference a type through its prefix.
                while choice:
                    if choice in unique:
                        result.add(unique[choice])
                        break
                    choice = choice.rpartition('.')[0]
                else:
                    continue
                break
        return result

    graph = {row['id']: references(row['body'], row['scope']) - {row['id']} for row in rows}
    roots = {row['id']: graph[row['id']] for row in rows if row['kind'] == 'test'}
    for path, body in sorted(compiler.items()):
        if path.startswith('test/fixtures/e2e/upstream/') and path.endswith('.dark'):
            roots[path] = references(p.strip_comments(body.decode(), True), ())
    witnesses = defaultdict(dict)
    for test, initial in sorted(roots.items()):
        queue = deque((target, []) for target in sorted(initial))
        visited = set()
        while queue:
            target, via = queue.popleft()
            if target in visited:
                continue
            visited.add(target)
            witnesses[target][test] = via
            for child in sorted(graph.get(target, ())):
                queue.append((child, via + [target]))
    result = defaultdict(list)
    for row in rows:
        if row['kind'] == 'test':
            continue
        hints = witnesses[row['id']]
        direct = sorted(test for test, via in hints.items() if not via)
        indirect = sorted((test for test, via in hints.items() if via), key=lambda t: (len(hints[t]), t))
        manual = maintained.get(row['name'])
        if manual is not None:
            if not isinstance(manual, dict) or set(manual) != {'tests', 'reason'} or not manual['reason']:
                raise ValueError(f'Invalid maintained coverage for {row["name"]}')
            if not isinstance(manual['tests'], list) or not manual['tests']:
                raise ValueError('Maintained coverage must name existing tests')
            for test in manual['tests']:
                if test not in roots:
                    raise ValueError(f'Unknown existing test reference: {test}')
        kind = 'direct' if direct else 'indirect' if indirect else 'maintained' if manual else 'not-identified'
        examples = direct or indirect
        result[row['source']].append(dict(name=row['name'], kind=row['kind'], line=row['line'],
            coverage=dict(kind=kind, reference_count=len(hints),
                          examples=[dict(test=t, via=hints[t]) for t in examples[:1]],
                          maintained=manual)))
    unknown = set(maintained) - set(by_name)
    if unknown:
        raise ValueError('Coverage entries refer to unknown declarations: ' + ', '.join(sorted(unknown)))
    return result, review


def context(root, upstream, records, compiler):
    source = p.read_yaml(root / 'porting/sources.yaml')
    return dict(upstream_revision=source['upstream_revision'], target=source['target'],
                compiler_context_sha256=p.compiler_context(root, compiler))


def execute_tests(root, expected_context):
    """Execute the existing suite and consume actual per-case outcomes, not totals."""
    executable = root / '_build/default/test/tests_main.exe'
    if not executable.is_file():
        return dict(context=expected_context, state='blocked',
                    reason='Build the pinned native graph with ./build --ai first.', tests=[])
    with tempfile.TemporaryDirectory(prefix='ledger-tests-') as temp:
        raw, inventory = Path(temp) / 'results.json', Path(temp) / 'inventory.yaml'
        log_dir = root / 'TestResults/porting'
        log_dir.mkdir(parents=True, exist_ok=True)
        log = log_dir / 'existing-tests.log'
        with log.open('w') as output:
            discovered = subprocess.run(['./run-tests', '--target=linux-x86_64',
                '--inventory-upstream=' + str(inventory)], cwd=root, stdout=output, stderr=subprocess.STDOUT)
            if discovered.returncode not in (0, 1) or not inventory.is_file():
                return dict(context=expected_context, state='blocked', reason='Existing test discovery failed; see TestResults/porting/existing-tests.log.', tests=[])
            discovery = p.read_yaml(inventory)
            p.validate_runner_export(discovery)
            run = subprocess.run(['./run-tests', '--ai', '--target=linux-x86_64',
                '--results-json=' + str(raw)], cwd=root, stdout=output, stderr=subprocess.STDOUT)
        if not raw.is_file():
            return dict(context=expected_context, state='blocked', reason='Runner did not produce per-test outcomes; see TestResults/porting/existing-tests.log.', tests=[])
        outcomes = json.loads(raw.read_text())
        if (outcomes.get('schema') != 1 or outcomes.get('mode') != 'execution'
                or outcomes.get('filter') != '' or outcomes.get('target') != expected_context['target']):
            raise ValueError('Execution report has incompatible target, schema, or filter')
        cases = {}
        expected = {(row['source_file'], row['name']) for row in discovery['tests']}
        for row in outcomes['tests']:
            if not row['source_file'].startswith('test/fixtures/e2e/upstream/'):
                continue
            key = (row['source_file'], row['name'])
            if key not in expected or key in cases or row['status'] not in ('pass', 'fail'):
                raise ValueError('Duplicate or invalid executed test outcome')
            cases[key] = row
        tests = []
        for row in discovery['tests']:
            key = (row['source_file'], row['name'])
            actual = cases.get(key)
            if not row['eligible']:
                if actual:
                    raise ValueError('Excluded test unexpectedly appears in executed results')
                status, reason = 'skipped', row['effective_skip']
            elif actual:
                status = actual['status']
                reason = actual['message'].replace(str(root), '<compiler>').replace(str(Path(temp)), '<run>')
            else:
                status, reason = 'blocked', 'Selected test has no execution result'
            tests.append(dict(source_file=row['source_file'], name=row['name'], status=status, reason=reason))
        return dict(context=expected_context, state='recorded', suite_exit_code=run.returncode,
                    parse_errors=discovery['parse_errors'], tests=tests)


def build(root, upstream, run_tests=False):
    config = read_inputs(root)
    sources = p.read_yaml(root / 'porting/sources.yaml')
    if p.git_revision(upstream) != sources['upstream_revision']:
        raise ValueError('Upstream checkout must match porting/sources.yaml exactly')
    upstream_files = p.checked_files(upstream, sources['upstream_revision'])
    records, compiler = p.discover(root, upstream_files)
    provenance = context(root, upstream, records, compiler)
    coverage, review = coverage_index(upstream_files, compiler, config['coverage'])
    features = []
    for feature in sorted(config['features'], key=lambda f: f['name']):
        hashes = {path: p.digest(compiler[path]) if path in compiler else None for path in feature['sources']}
        source_hash = p.digest(encoded(hashes).encode())
        changed = feature.get('reviewed_sha256') != source_hash
        features.append(dict(feature, source_sha256=source_hash, review_required=changed))
        if changed:
            review.append('Review feature status: ' + feature['name'])

    source_rows, parser, builtins = [], [], []
    used_mappings = set()
    used_parser, used_builtins = set(), set()
    for record in sorted(records, key=lambda r: r['id']):
        category, path = record['category'], record['upstream']
        if category in ('packages', 'stdlib'):
            candidates = config['source_mappings'].get(path, record['compiler'])
            if path in config['source_mappings']:
                used_mappings.add(path)
            if not isinstance(candidates, list) or any(not isinstance(c, str) or c not in compiler for c in candidates):
                raise ValueError(f'Invalid source mapping: {path}')
            identical = [c for c in candidates if path and compiler[c] == upstream_files[path]]
            status = ('compiler-only' if path is None else 'identical' if identical else
                      'different' if candidates else 'missing')
            exception = config['source_exceptions'].get(path)
            source_rows.append(dict(id=record['id'], category=category, upstream=path,
                compiler=sorted(candidates), source_status=status, identical=sorted(identical),
                exception=exception, declarations=coverage.get(path, []),
                compilation=dict(status='not-run', reason='Declaration-only package compilation adapter pending')
                    if category == 'packages' else None))
            if status in ('different', 'missing') and not exception:
                review.append('Source mapping/difference: ' + record['id'])
        elif category == 'parser':
            paths = config['parser_mappings'].get(record['id'], record['compiler'])
            if record['id'] in config['parser_mappings']:
                used_parser.add(record['id'])
            if not isinstance(paths, list) or any(path not in compiler for path in paths):
                raise ValueError('Invalid parser mapping: ' + record['id'])
            status = 'maintained' if record['id'] in used_parser else 'candidate' if paths else 'unmapped'
            parser.append(dict(id=record['id'], compiler=sorted(paths), mapping=status))
        elif category == 'builtins' and 'builtin' in record:
            paths = config['builtin_mappings'].get(record['id'], record['compiler'])
            if record['id'] in config['builtin_mappings']:
                used_builtins.add(record['id'])
            if not isinstance(paths, list) or any(path not in compiler for path in paths):
                raise ValueError('Invalid builtin mapping: ' + record['id'])
            builtins.append(dict(id=record['id'], **record['builtin'], compiler=sorted(paths),
                                 mapping='maintained' if record['id'] in used_builtins else 'same-name' if paths else 'missing'))
    for key, used in [('source_mappings', used_mappings), ('parser_mappings', used_parser), ('builtin_mappings', used_builtins)]:
        if set(config[key]) - used:
            raise ValueError('Unused maintained mappings: ' + ', '.join(sorted(set(config[key]) - used)))
    for path, reason in config['source_exceptions'].items():
        if path not in upstream_files or not isinstance(reason, str) or not reason.strip():
            raise ValueError('Invalid source exception: ' + str(path))

    receipt_path = root / RECEIPT
    receipt = json.loads(receipt_path.read_text()) if receipt_path.exists() and not run_tests else None
    if receipt is not None and (receipt.get('schema') != 1
            or receipt.get('state') not in ('recorded', 'blocked')
            or not isinstance(receipt.get('tests'), list)):
        raise ValueError('Invalid test-results.json receipt')
    if run_tests:
        receipt = dict(schema=1, **execute_tests(root, provenance))
        # Never attribute a run to inputs that changed while it was executing.
        _, after = p.discover(root, upstream_files)
        if context(root, upstream, records, after) != provenance:
            raise ValueError('Inputs changed during test execution; previous results are unchanged')
        receipt_path.write_text(encoded(receipt))
    current = receipt is not None and receipt.get('context') == provenance
    named = [dict(id=r['id'], source_file=r['upstream'], name=r['named_test'], status='blocked',
                  reason='Named package test execution adapter is not implemented')
             for r in records if 'named_test' in r]
    named.sort(key=lambda r: r['id'])
    tests = dict(state=receipt['state'] if current else 'stale' if receipt else 'not-run',
                 reason=receipt.get('reason', '') if current else '',
                 suite_exit_code=receipt.get('suite_exit_code') if current else None,
                 outcomes=receipt.get('tests', []) if current else [],
                 parse_errors=receipt.get('parse_errors', []) if current else [],
                 named_tests=dict(count=len(named), cases=named, status='blocked',
                     reason='Named package test execution is not implemented; no pass inferred from discovery.'))
    builtin_sources = [r for r in records if r['category'] == 'builtins' and 'builtin_declaration_counts' in r]
    return dict(schema=1, context=provenance, features=features, parser=parser, builtins=builtins,
                computed_builtin_registrations=sum(r['builtin_declaration_counts']['computed'] for r in builtin_sources),
                sources=source_rows, tests=tests, review=sorted(set(review)))


def markdown(value):
    lines = ['# Repeatable compatibility ledger', '',
             'Generated from pinned sources and maintained decisions. No compatibility probes or new tests are generated.', '',
             'Upstream: `' + value['context']['upstream_revision'] + '`; target: `' + value['context']['target'] + '`.', '',
             '## Language features', '', '| Feature | Status | Review needed |', '| --- | --- | --- |']
    for feature in value['features']:
        lines.append(f"| {p.cell(feature['name'])} | {feature['status']} | {'yes' if feature['review_required'] else 'no'} |")
    lines += ['', '## Source equivalency', '', '| Ledger | Identical | Different | Missing | Compiler-only |', '| --- | ---: | ---: | ---: | ---: |']
    for category in ('stdlib', 'packages'):
        counts = Counter(r['source_status'] for r in value['sources'] if r['category'] == category)
        lines.append('| ' + category + ' | ' + ' | '.join(str(counts[k]) for k in ('identical', 'different', 'missing', 'compiler-only')) + ' |')
    package_rows = [r for r in value['sources'] if r['category'] == 'packages' and r['upstream']]
    counts = Counter(d['coverage']['kind'] for r in package_rows for d in r['declarations'])
    lines += ['', '## Existing test references', '',
              'Lexical references are coverage hints, not evidence of execution. One deterministic witness (the shortest indirect path) and the total reference count per declaration are in `porting/generated/ledger.json`.', '',
              '| Direct | Indirect | Maintained | Not identified |', '| ---: | ---: | ---: | ---: |',
              '| ' + ' | '.join(str(counts[k]) for k in ('direct', 'indirect', 'maintained', 'not-identified')) + ' |', '',
              'Package compilation: not run; a declaration-only compilation adapter remains to be implemented.', '',
              '## Parser and builtins', '',
              'Builtin rows are literal registration candidates. Same-name matches do not establish assembled bindings; aliases and computed registrations remain review items.', '']
    for category in ('parser', 'builtins'):
        lines.append('- ' + category + ': ' + ', '.join(f'{k}: {v}' for k, v in sorted(Counter(r['mapping'] for r in value[category]).items())))
    lines += [f"- Computed builtin registrations needing review: {value['computed_builtin_registrations']}", '',
              '## Existing test execution', '', 'State: **' + value['tests']['state'] + '**.', '']
    if value['tests']['reason']:
        lines += [value['tests']['reason'], '']
    counts = Counter(t['status'] for t in value['tests']['outcomes'])
    lines += ['Imported legacy cases with current execution receipts:', '',
              '| Pass | Fail | Skipped | Blocked |', '| ---: | ---: | ---: | ---: |',
              '| ' + ' | '.join(str(counts[k]) for k in ('pass', 'fail', 'skipped', 'blocked')) + ' |', '',
              f"Named package tests: {value['tests']['named_tests']['count']} declarations, execution blocked (not silently skipped).", '',
              '## Review queue', '']
    lines += ['- ' + p.cell(item) for item in value['review']]
    return '\n'.join(lines) + '\n'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--upstream', type=Path, required=True)
    parser.add_argument('--run-tests', action='store_true', help='Execute existing tests; does not build the compiler')
    parser.add_argument('--check', action='store_true', help='Compare deterministic generated outputs without writing')
    args = parser.parse_args()
    if args.check and args.run_tests:
        parser.error('--check does not execute tests')
    try:
        value = build(ROOT, args.upstream.resolve(), args.run_tests)
        outputs = {OUTPUT: encoded(value), REPORT: markdown(value)}
        for name, contents in outputs.items():
            path = ROOT / name
            if args.check:
                if not path.exists() or path.read_text() != contents:
                    raise ValueError('Ledger is stale: ' + name)
            else:
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(contents)
        print('Ledger is current.' if args.check else 'Regenerated source/status/coverage ledgers; test execution state: ' + value['tests']['state'])
        if args.run_tests and (value['tests']['state'] != 'recorded' or value['tests']['parse_errors']
                or value['tests']['suite_exit_code'] != 0 or value['tests']['named_tests']['count'] > 0
                or any(t['status'] in ('fail', 'blocked') for t in value['tests']['outcomes'])):
            return 1
        return 0
    except (OSError, ValueError, KeyError, TypeError, subprocess.CalledProcessError) as error:
        print('ledger: ' + str(error), file=sys.stderr)
        return 2


if __name__ == '__main__':
    sys.exit(main())
