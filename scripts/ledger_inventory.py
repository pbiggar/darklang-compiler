"""Source-only declaration and test comparisons for the repeatable ledger.

This indexes existing sources and enablement metadata; it never executes tests.
Unknown source forms remain explicit review items rather than inferred passes.
"""
from collections import Counter, defaultdict
import importlib.util
import re

import porting as p


def declaration_rows(upstream, compiler, index, coverage, files):
    """Match qualified declarations across file splits; keep ambiguous names visible."""
    native, originals, review = [], [], []
    for sources, target in ((upstream, originals), (compiler, native)):
        for path, body in sorted(sources.items()):
            if not path.endswith('.dark') or not path.startswith(('packages/', 'StdLib/')):
                continue
            found, warnings = index(path, body.decode(), containers=True)
            target.extend(row for row in found if row['kind'] != 'test')
            review.extend(warnings)
    for rows in (originals, native):
        identities = Counter(row['id'] for row in rows)
        for row in rows:
            if identities[row['id']] > 1:
                review.append('Repeated declaration identity: ' + row['id'])
                row['id'] += '@L' + str(row['line'])
    by_name = defaultdict(list)
    for row in native:
        by_name[(row['kind'], row['name'])].append(row)
    mappings = {r['upstream']: r for r in files if r['category'] == 'packages' and r['upstream']}
    covered = {(path, row['name'], row['line']): row['coverage']
               for path, rows in coverage.items() for row in rows}
    used, result = set(), []

    def location(row):
        return dict(source=row['source'], line=row['line'], end_line=row['end_line'])

    for row in originals:
        mapping = mappings[row['source']]
        choices = by_name[(row['kind'], row['name'])]
        local = [r for r in choices if r['source'] in mapping['compiler']]
        choices = local or choices
        exact = [r for r in choices if r['body'] == row['body']]
        # Repeated identical fragments are legitimate counterparts. Different
        # same-name bodies need review instead of selecting an arbitrary file.
        selected = exact or choices
        status = ('identical' if exact else 'missing' if not choices else
                  'different' if len(choices) == 1 else 'ambiguous')
        used.update(r['id'] for r in selected)
        if status == 'ambiguous':
            review.append('Ambiguous declaration counterpart: ' + row['id'])
        categories = ['packages']
        if row['source'].startswith('packages/darklang/stdlib/'):
            categories.append('stdlib')
        result.append(dict(id=row['name'], name=row['name'], kind=row['kind'],
            categories=categories, upstream=location(row), compiler=[location(r) for r in selected],
            source_status=status, source_exception=mapping['exception'],
            coverage=covered.get((row['source'], row['name'], row['line']),
                dict(kind='not-identified', reference_count=0, examples=[], maintained=None)),
            compilation=dict(**mapping['compilation'], basis='containing-source-file',
                             source=row['source'])))
    for row in native:
        if row['id'] in used:
            continue
        result.append(dict(id=row['name'], name=row['name'], kind=row['kind'],
            categories=['packages'] + (['stdlib'] if row['source'].startswith('StdLib/') else []),
            upstream=None, compiler=[location(row)], source_status='compiler-only',
            coverage=None, compilation=None, source_exception=None))
    return sorted(result, key=lambda r: (r['name'], r['kind'], str(r['upstream']), str(r['compiler']))), review


# Preserve string contents and token boundaries when comparing source formats.
TOKEN = re.compile(r'\$?"""[\s\S]*?"""|\$?"(?:\\.|[^"\\])*"|``[^`]+``|'
                   r"[\w']+|[!$%&*+\-/<=>?@^|~:]+|[^\s]", re.UNICODE)


def tokens(text):
    return TOKEN.findall(p.strip_comments(text))


def separator(parts):
    depth = 0
    for i, part in enumerate(parts):
        if part in ('(', '[', '{'):
            depth += 1
        elif part in (')', ']', '}'):
            depth -= 1
        elif part == '=' and depth == 0:
            return i
    return None


def signature(text, named=False):
    parts = tokens(text)
    if named:
        # Only the established single-assertion migration is automatic. Test.all,
        # Test.error and helper-driven tests require explicit mappings/review.
        marker = ['|>', 'Stdlib', '.', 'Test', '.', 'equal']
        positions = [i for i in range(len(parts)) if parts[i:i + len(marker)] == marker]
        if len(positions) != 1 or parts[:3] == ['Stdlib', '.', 'Test']:
            return None
        i = positions[0]
        left, right = parts[:i], parts[i + len(marker):]
    else:
        i = separator(parts)
        if i is None:
            return None
        left, right = parts[:i], parts[i + 1:]
    if not left or not right:
        return None
    return tuple(left), tuple(right)


def named_assertions(body):
    """Recognize Test.equal and offside Test.all lists from existing sources."""
    single = signature(body, named=True)
    if single:
        return [single]
    text = p.strip_comments(body)
    match = re.match(r'\s*Stdlib\.Test\.all\s*\[', text)
    if not match or not text.rstrip().endswith(']'):
        return []
    opening = match.end() - 1
    line_start = text.rfind('\n', 0, opening) + 1
    tail = text[opening + 1:text.rfind(']')]
    first = tail.split('\n', 1)[0]
    if not first.strip():
        return []
    column = opening - line_start + 1 + len(first) - len(first.lstrip())
    lines = tail.splitlines()
    current, chunks = [lines[0]], []
    for line in lines[1:]:
        parts = tokens('\n'.join(current))
        balanced = sum(t in ('(', '[', '{') for t in parts) == sum(t in (')', ']', '}') for t in parts)
        if (line.strip() and len(line) - len(line.lstrip()) == column
                and balanced and signature('\n'.join(current), named=True)):
            chunks.append('\n'.join(current))
            current = []
        current.append(line)
    chunks.append('\n'.join(current))
    found = [signature(chunk, named=True) for chunk in chunks]
    return found if all(found) else []


def legacy_cases(path, text, diff):
    """Index assertion spans in the existing offside fixture format.

    This is a source index, not a substitute for runner parsing. Unrecognized
    statements are retained in the review queue. Line gates use original lines.
    """
    raw = text.splitlines(keepends=True)
    clean = p.strip_comments(text).splitlines()
    result, review, scopes, i = [], [], [], 0
    while i < len(raw):
        line = clean[i].strip() if i < len(clean) else ''
        indent = len(raw[i]) - len(raw[i].lstrip())
        if not line or line.startswith('#'):
            i += 1
            continue
        while scopes and indent <= scopes[-1]:
            scopes.pop()
        if re.match(r'module\s+', line):
            if line.endswith('='):
                scopes.append(indent)
            i += 1
            continue
        column = len(scopes) * 2
        if indent != column or re.match(r'(let|val|type|def|trait|impl)\b|\[<', line):
            i += 1
            continue
        start, end = i, i + 1
        while end <= len(raw):
            body = ''.join(raw[start:end])
            parts = tokens(body)
            sep = separator(parts)
            balanced = sum(t in ('(', '[', '{') for t in parts) == sum(t in (')', ']', '}') for t in parts)
            if sep is not None and sep + 1 < len(parts) and balanced:
                # Hanging expected values and multiline expectation lists belong
                # to this assertion even if the first line already has '='.
                if end < len(raw) and clean[end].strip() and len(raw[end]) - len(raw[end].lstrip()) > column:
                    end += 1
                    continue
                break
            if end == len(raw):
                break
            next_line = clean[end].strip()
            next_indent = len(raw[end]) - len(raw[end].lstrip())
            if balanced and next_line and next_indent <= column and (next_line.startswith('module ') or re.match(r'(let|type|val)\b', next_line)):
                break
            end += 1
        body = ''.join(raw[start:end]).rstrip()
        normalized = diff.normalize_expected_derror_differences(body)
        sig = signature(normalized)
        if sig is None:
            review.append(f'{path}:{start + 1}: unindexed legacy test statement')
        else:
            override = raw[start - 1].strip() if start and raw[start - 1].lstrip().startswith('#compileerror') else None
            skip = next((i for i, part in enumerate(sig[1]) if part == 'skip'), None)
            result.append(dict(id=f'{path}#L{start + 1}', source=path, line=start + 1,
                end_line=end, name=f'L{start + 1}', body=body, signature=sig,
                override=override, metadata_skip=' '.join(sig[1][skip:skip + 3]) if skip is not None else None))
        i = end
    return result, review


def test_inventory(root, upstream, compiler, index, records, maintained):
    spec = importlib.util.spec_from_file_location('ledger_upstream_diff', root / 'scripts/diff-upstream-execution-tests.py')
    diff = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(diff)
    gates = {r['compiler'][0]: r['configured_gates'] for r in records if 'configured_gates' in r}
    originals, native, review = [], [], []

    def named_case(row, text):
        body = row['body'].split('=', 1)[1].strip()
        raw = text.splitlines()
        previous = raw[row['line'] - 2].strip() if row['line'] > 1 else ''
        return dict(row, format='named', signature=signature(body, named=True),
                    assertions=named_assertions(body),
                    override=previous if previous.startswith('#compileerror') else None,
                    metadata_skip=None)

    for sources, target, prefix in ((upstream, originals, 'backend/testfiles/execution/'),
                                     (compiler, native, 'test/fixtures/e2e/')):
        for path, raw in sorted(sources.items()):
            if not path.startswith(prefix) or not path.endswith(('.dark', '.e2e')):
                continue
            text = raw.decode()
            declared, _ = index(path, text)
            named = [r for r in declared if r['kind'] == 'test']
            if named:
                target.extend(named_case(row, text) for row in named)
                continue
            found, warnings = legacy_cases(path, raw.decode(), diff)
            review.extend(warnings)
            for row in found:
                row['format'] = 'legacy'
                target.append(row)
    for sources, target in ((upstream, originals), (compiler, native)):
        for path, raw in sorted(sources.items()):
            if not path.startswith(('packages/', 'StdLib/')) or not path.endswith('.dark'):
                continue
            found, warnings = index(path, raw.decode())
            review.extend(warnings)
            for row in found:
                if row['kind'] != 'test':
                    continue
                target.append(named_case(row, raw.decode()))
    by_id = {r['id']: r for r in native}
    if set(maintained) - {r['id'] for r in originals}:
        raise ValueError('Unknown upstream test mapping: ' + ', '.join(sorted(set(maintained) - {r['id'] for r in originals})))
    names, signatures = defaultdict(list), defaultdict(list)
    for row in native:
        if row['format'] == 'named':
            names[row['name']].append(row)
        if row['signature']:
            signatures[row['signature']].append(row)

    def describe(row):
        gate = gates.get(row['source'], {})
        selectable = (row['source'].startswith('test/fixtures/e2e/') and row['source'].endswith('.e2e')
                      or row['source'].startswith('test/fixtures/e2e/upstream/') and row['source'].endswith('.dark'))
        reason = ('fixture-metadata' if row['metadata_skip'] and not row['override'] else
                  'whole-file-gate' if gate.get('whole_file') else
                  'line-gate' if row['line'] in gate.get('lines', []) else None)
        return dict(id=row['id'], source=row['source'], line=row['line'], name=row['name'],
            format=row['format'], enabled=selectable and reason is None,
            origin='imported-upstream' if row['source'].startswith('test/fixtures/e2e/upstream/') else 'compiler',
            exclusion_origin='outside-runner-fixtures' if not selectable else reason,
            reason='Source is outside the compiler runner fixture directories' if not selectable else row['metadata_skip'] if reason == 'fixture-metadata' else reason,
            expectation_override=row['override'])

    used, result = set(), []
    for row in sorted(originals, key=lambda r: r['id']):
        basis, choices = 'none', []
        assertions = row.get('assertions', []) or ([row['signature']] if row['signature'] else [])
        matched = 0
        if row['id'] in maintained:
            decision = maintained[row['id']]
            if set(decision) != {'compiler', 'reason'} or not decision['reason'] or not decision['compiler']:
                raise ValueError('Invalid maintained test mapping: ' + row['id'])
            if any(key not in by_id for key in decision['compiler']):
                raise ValueError('Unknown compiler test in mapping: ' + row['id'])
            choices, basis = [by_id[key] for key in decision['compiler']], 'maintained'
        elif row['format'] == 'named' and len(names[row['name']]) == 1:
            choices, basis = names[row['name']], 'qualified-name'
        elif assertions:
            for assertion in assertions:
                candidates = signatures[assertion]
                # Prefer the same upstream path, then imported fixtures, over
                # coincidental regressions with the same assertion.
                relative = row['source'].removeprefix('backend/testfiles/execution/')
                same_path = [r for r in candidates if r['source'] == 'test/fixtures/e2e/upstream/' + relative]
                imported = [r for r in candidates if r['source'].startswith('test/fixtures/e2e/upstream/')]
                candidates = same_path or imported or candidates
                if len(candidates) == 1:
                    choices.extend(candidates)
                    matched += 1
                elif candidates:
                    review.append('Ambiguous test counterpart: ' + row['id'])
            choices = list({r['id']: r for r in choices}.values())
            basis = ('assertion-content' if len(assertions) == 1 else 'assertion-group') if choices else 'ambiguous' if any(signatures[a] for a in assertions) else 'none'
        used.update(r['id'] for r in choices)
        equality = ('missing' if not choices else 'identical' if all(r['body'] == row['body'] and not r['override'] for r in choices)
                    else 'adapted' if basis in ('assertion-content', 'assertion-group', 'maintained') or all(r['body'] == row['body'] for r in choices) or all(r['signature'] and r['signature'] == row['signature'] for r in choices)
                    else 'different')
        partial = bool(choices) and basis == 'assertion-group' and matched < len(assertions)
        if partial:
            equality = 'partial'
        result.append(dict(id=row['id'], upstream=dict(id=row['id'], source=row['source'],
            line=row['line'], name=row['name'], format=row['format']), compiler=[describe(r) for r in choices],
            equivalency=equality, mapping_basis=basis,
            matched_assertions=matched if basis in ('assertion-content', 'assertion-group') else None,
            indexed_assertions=len(assertions),
            expectation_adapted=any(r['override'] for r in choices),
            status='partial' if partial else 'enabled' if choices and all(describe(r)['enabled'] for r in choices) else 'disabled' if choices else 'ambiguous' if basis == 'ambiguous' else 'missing',
            reason=maintained.get(row['id'], {}).get('reason')))
    return dict(mode='source-inventory', cases=result,
                compiler_only=[describe(r) for r in sorted(native, key=lambda r: r['id']) if r['id'] not in used],
                unindexed=sorted(set(r for r in review if 'unindexed' in r))), review
