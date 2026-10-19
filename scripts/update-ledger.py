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
import ledger_inventory as inventory

ROOT = p.ROOT
INPUTS = 'porting/ledger-inputs.yaml'
PACKAGE_RECEIPT = 'porting/package-results.json'
OUTPUT = 'porting/generated/ledger.json'
REPORT = 'docs/porting/generated/ledger.md'
GAPS = 'docs/porting/generated/coverage-gaps.md'


def encoded(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + '\n'


def source_path(value):
    if not isinstance(value, str) or not value or value.startswith('/') or '..' in Path(value).parts:
        raise ValueError(f'Expected a repository-relative source path: {value!r}')
    return value


def read_inputs(root):
    value = p.read_yaml(root / INPUTS)
    value.setdefault('test_mappings', {})
    fields = {'schema', 'features_file', 'source_mappings', 'parser_mappings',
              'builtin_mappings', 'coverage', 'source_exceptions', 'test_mappings'}
    if set(value) != fields or value['schema'] != 1:
        raise ValueError('Invalid ledger-inputs.yaml schema')
    for key in fields - {'schema', 'features_file'}:
        if not isinstance(value[key], dict):
            raise ValueError(f'{key} must be a mapping')
    language = p.read_yaml(root / source_path(value.pop('features_file')))
    if set(language) != {'schema', 'features'} or language['schema'] != 1 or not isinstance(language['features'], list):
        raise ValueError('Invalid language-features.yaml schema')
    value['features'] = language['features']
    seen = set()
    for feature in value['features']:
        if (not isinstance(feature, dict) or set(feature) != {'group', 'name', 'status', 'sources', 'notes'}
                or feature.get('status') not in {'implemented', 'partial', 'missing', 'not-assessed'}
                or not isinstance(feature.get('group'), str) or not feature['group']
                or not isinstance(feature.get('notes'), str) or not feature['notes']
                or not isinstance(feature.get('name'), str) or feature['name'] in seen
                or not isinstance(feature.get('sources'), list) or not feature['sources']):
            raise ValueError('Invalid or duplicate maintained language feature')
        seen.add(feature['name'])
        for path in feature['sources']:
            source_path(path)
    return value


def declarations(path, text, containers=False):
    """Bounded source index using the existing scanner's module/offside rules.

    References below are lexical coverage hints, not proof that a branch ran.
    Unknown declaration headers are returned for review instead of discarded.
    """
    original_lines = text.splitlines(keepends=True)
    clean = p.strip_comments(text, True).splitlines()
    comment_lines = p.strip_comments(text).splitlines()
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
        scope = root + tuple(part for _, names, _ in modules for part in names)
        trait = re.match(r'trait\s+([\w\x27]+)(?:<[^>]*>)?\s*=\s*$', statement)
        impl = re.match(r'impl(?:<[^>]*>)?\s+([\w.]+)(?:<[^>]*>)?\s+for\s+([\w.]+)(?:<[^>]*>)?\s*=\s*$', statement)
        if containers and (trait or impl):
            end = number
            while end < len(clean) and (not clean[end].strip() or len(clean[end]) - len(clean[end].lstrip()) > indent):
                end += 1
            while end > number and not comment_lines[end - 1].strip():
                end -= 1
            label = trait[1] if trait else re.sub(r'\s+', ' ', statement.removesuffix('=').strip())
            name = '.'.join(scope + (label,))
            rows.append(dict(id=path + '#' + name, name=name, source=path, line=number,
                end_line=end, kind='trait' if trait else 'impl', scope=scope,
                body=''.join(original_lines[number - 1:end])))
        if trait:
            modules.append((indent, (trait[1],), None))
            continue
        if impl:
            typ, trait_name = impl[2].split('.')[-1], impl[1].split('.')[-1]
            names = (trait_name,) if scope and scope[-1] == typ else (typ, trait_name)
            modules.append((indent, names, None))
            continue
        match = re.match(r'(let|val|type|test)\s+(?:``([^`]+)``|([\w\x27]+)\b)', statement)
        if not match:
            if re.match(r'(let|val|type|test|trait|impl)\b', statement):
                review.append(f'{path}:{number}: unindexed declaration header')
            continue
        end = number
        while end < len(clean) and (not clean[end].strip() or len(clean[end]) - len(clean[end].lstrip()) > indent):
            end += 1
        while end > number and not comment_lines[end - 1].strip():
            end -= 1
        name = '.'.join(scope + (match[2] or match[3],))
        rows.append(dict(id=path + '#' + name, name=name, source=path, line=number,
                         kind={'let': 'function', 'val': 'value', 'type': 'type', 'test': 'test'}[match[1]],
                         scope=scope, end_line=end, body=''.join(original_lines[number - 1:end])))
    return rows, review


def reference_text(text):
    """Mask literal text/comments but retain expressions inside interpolated strings.

    This is only a lexical reference index, not a parser or an execution claim.
    """
    text = p.strip_comments(text)
    output = ['\n' if char == '\n' else ' ' for char in text]

    def code(index, nested=False):
        depth = 0
        while index < len(text):
            char = text[index]
            if nested and char == '}' and depth == 0:
                return index + 1
            if char == '"' or text.startswith('$"', index):
                interpolated = char == '$'
                index += int(interpolated)
                delimiter = '"""' if text.startswith('"""', index) else '"'
                index += len(delimiter)
                while index < len(text) and not text.startswith(delimiter, index):
                    if text[index] == '\\':
                        index += 2
                    elif interpolated and text[index] == '{':
                        index = code(index + 1, True)
                    else:
                        index += 1
                index += len(delimiter)
                continue
            if nested:
                depth += int(char == '{') - int(char == '}')
            output[index] = char
            index += 1
        return index

    code(0)
    return ''.join(output).replace('``', '')


def declaration_graph(upstream):
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
        body = reference_text(body)
        # Avoid attributing a local binding/parameter to a package declaration.
        locals_ = set(re.findall(r'\blet\s+([\w\x27]+)|\(([\w\x27]+)\s*:', body))
        shadowed = {name for pair in locals_ for name in pair if name}
        for token in set(re.findall(r"(?<![\w'])\b[A-Za-z_][\w\x27]*(?:\.[A-Za-z_][\w\x27]*)*", body)):
            if token.split('.')[0] in shadowed:
                continue
            token = 'Darklang.' + token if token.startswith('Stdlib.') else token
            choices = ['.'.join(scope[:n] + (token,)) for n in range(len(scope), 0, -1)] + [token]
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
    return rows, graph, references, review


def coverage_index(upstream, compiler, maintained):
    rows, graph, references, review = declaration_graph(upstream)
    by_name = {row['name'] for row in rows}
    roots = {row['id']: graph[row['id']] for row in rows if row['kind'] == 'test'}
    for path, body in sorted(compiler.items()):
        if path.startswith('test/fixtures/e2e/upstream/') and path.endswith('.dark'):
            roots[path] = references(body.decode(), ())
    maintained_targets = defaultdict(set)
    for row in rows:
        manual = maintained.get(row['name'])
        if manual is not None:
            if (not isinstance(manual, dict) or set(manual) != {'tests', 'reason'}
                    or not manual['reason'] or not isinstance(manual['tests'], list) or not manual['tests']):
                raise ValueError('Invalid maintained coverage for ' + row['name'])
            for test in manual['tests']:
                if test not in roots:
                    raise ValueError('Unknown existing test reference: ' + str(test))
                maintained_targets[test].add(row['id'])
    witnesses = defaultdict(dict)
    for test, initial in sorted(roots.items()):
        queue = deque((target, [], 'lexical') for target in sorted(initial))
        queue.extend((target, [], 'maintained') for target in sorted(maintained_targets[test] - initial))
        visited = set()
        while queue:
            target, via, basis = queue.popleft()
            if target in visited:
                continue
            visited.add(target)
            witnesses[target][test] = dict(via=via, basis=basis)
            for child in sorted(graph.get(target, ())):
                queue.append((child, via + [target], basis))
    result = defaultdict(list)
    for row in rows:
        if row['kind'] == 'test':
            continue
        hints = witnesses[row['id']]
        direct = sorted(test for test, witness in hints.items() if not witness['via'] and witness['basis'] == 'lexical')
        indirect = sorted((test for test, witness in hints.items() if witness['via']), key=lambda t: (len(hints[t]['via']), t))
        manual = maintained.get(row['name'])
        kind = 'direct' if direct else 'maintained' if manual else 'indirect' if indirect else 'not-identified'
        examples = direct or (sorted(manual['tests']) if manual else indirect)
        result[row['source']].append(dict(name=row['name'], kind=row['kind'], line=row['line'],
            coverage=dict(kind=kind, reference_count=len(hints),
                          examples=[dict(test=t, **hints[t]) for t in examples[:1]],
                          maintained=manual)))
    unknown = set(maintained) - set(by_name)
    if unknown:
        raise ValueError('Coverage entries refer to unknown declarations: ' + ', '.join(sorted(unknown)))
    return result, review


def package_manifest(upstream):
    """File dependencies from existing declaration references, not generated probes.

    Embedded StdLib supplies standard dependencies. The requested original file
    is always compiled, even when it replaces an embedded module.
    """
    rows, graph, _, _ = declaration_graph(upstream)
    paths = {row['id']: row['source'] for row in rows}
    dependencies = defaultdict(set)
    for row in rows:
        if row['kind'] != 'test':
            dependencies[row['source']].update(paths[key] for key in graph[row['id']]
                if paths[key] != row['source'] and not paths[key].startswith('packages/darklang/stdlib/'))
    result = []
    for path in sorted(upstream):
        if not path.startswith('packages/') or not path.endswith('.dark'):
            continue
        found, ordered = {path}, []
        def visit(dependency):
            if dependency in found:
                return
            found.add(dependency)
            for child in sorted(dependencies[dependency]):
                visit(child)
            ordered.append(dependency)
        for dependency in sorted(dependencies[path]):
            visit(dependency)
        result.append(dict(source=path, dependencies=ordered))
    return result


def execute_packages(root, upstream, expected_context):
    executable = root / '_build/default/tools/ledger/compile_packages.exe'
    if not executable.is_file():
        return dict(context=expected_context, state='blocked', packages=[],
                    reason='Build the native package adapter with ./build --ai first.')
    manifest = package_manifest(upstream)
    with tempfile.TemporaryDirectory(prefix='ledger-packages-') as temp:
        directory = Path(temp)
        # Byte-for-byte pinned blobs, never dirty checkout files or probe source.
        for path, body in sorted(upstream.items()):
            if path.startswith('packages/') and path.endswith('.dark'):
                destination = directory / path
                destination.parent.mkdir(parents=True, exist_ok=True)
                destination.write_bytes(body)
        inputs, report = directory / 'manifest.json', directory / 'report.json'
        inputs.write_text(encoded(manifest))
        log = root / 'TestResults/porting/package-compilation.log'
        log.parent.mkdir(parents=True, exist_ok=True)
        with log.open('w') as output:
            run = subprocess.run([str(executable), str(directory), str(inputs), str(report)],
                                 cwd=root, stdout=output, stderr=subprocess.STDOUT)
        if run.returncode != 0 or not report.is_file():
            return dict(context=expected_context, state='blocked', packages=[],
                reason='Native package adapter could not finish; see TestResults/porting/package-compilation.log.')
        value = json.loads(report.read_text())
        if (value.get('schema') != 1 or value.get('stage') != 'native-declarations'
                or value.get('target') != expected_context['target']):
            raise ValueError('Invalid native package compilation report')
        expected = {row['source']: row['dependencies'] for row in manifest}
        seen = set()
        for row in value['packages']:
            path = row['source']
            if (path in seen or path not in expected or row['dependencies'] != expected[path]
                    or row['status'] not in ('compiled', 'fail')):
                raise ValueError('Unexpected or duplicate native package result')
            seen.add(path)
            row['reason'] = row['reason'].replace(str(directory), '<pinned-sources>').replace(str(root), '<compiler>')
        if seen != set(expected):
            raise ValueError('Native package compilation report is incomplete')
        return dict(context=expected_context, state='recorded', stage=value['stage'],
                    packages=sorted(value['packages'], key=lambda row: row['source']))


def builtin_counterparts(upstream, compiler):
    """Map implementation counterparts, not source-callable Builtin bindings.

    A qualified upstream wrapper that calls Builtin.foo identifies a counterpart
    only when the compiler declares that same qualified wrapper. A builtin name
    prefix alone is never enough (notably for integer trait methods).
    """
    native = defaultdict(set)
    for path, body in sorted(compiler.items()):
        if path.startswith(('StdLib/', 'packages/')) and path.endswith('.dark'):
            for row in declarations(path, body.decode())[0]:
                if row['kind'] in ('function', 'value'):
                    native[row['name']].add(path)
    wrappers = defaultdict(list)
    for path, body in sorted(upstream.items()):
        if path.startswith('packages/') and path.endswith('.dark'):
            for row in declarations(path, body.decode())[0]:
                names = [row['name']]
                # Upstream moved numeric operators into trait impl namespaces;
                # the pre-trait compiler retains the method at its type module.
                parts = row['name'].split('.')
                operator_traits = {'Add', 'Subtract', 'Multiply', 'Divide', 'Modulo', 'Power', 'Negate',
                    'Compare', 'BitwiseAnd', 'BitwiseOr', 'BitwiseXor', 'BitwiseNot', 'ShiftLeft', 'ShiftRight', 'ToString'}
                if len(parts) >= 5 and parts[-2] in operator_traits and parts[:2] == ['Darklang', 'Stdlib']:
                    names.append('.'.join(parts[:-2] + parts[-1:]))
                matches = sorted({path for name in names for path in native.get(name, ())})
                if row['kind'] in ('function', 'value') and matches:
                    for name in sorted(set(re.findall(r'\bBuiltin\.([\w\x27]+)', reference_text(row['body'])))):
                        wrappers[name].append(dict(name=row['name'], upstream=path,
                                                   compiler=matches))
    return native, wrappers


def computed_builtin_calls(upstream):
    """Reviewed local constructor families with literal first-argument names.

    This expands source call sites only; it does not validate an assembled runner
    or registry. Unfamiliar computed constructors remain visible in the ledger.
    """
    families = {
        'Builtins.Cli/Libs/RuntimeChild.fs': ['make', 'child'],
        'Builtins.Cli/Libs/Syscall.fs': ['make', 'timestamp', 'clock', 'clockUnit', 'poll'],
        'Builtins.Time/Libs/RuntimeClock.fs': ['make'],
        'Builtins.Matter/Libs/PM/Packages.fs': ['locationsByHashFn', 'findByLocationFn', 'getByHashFn'],
        'Builtins.Matter/Libs/PM/Permissions.fs': ['policyFn', 'hostOnly'],
    }
    rows = []
    for suffix, helpers in sorted(families.items()):
        path = 'backend/src/Builtins/' + suffix
        if path not in upstream:
            continue
        text = p.strip_comments(upstream[path].decode())
        templates = re.findall(r'\bname\s*=\s*fn\s+(?:name|builtinName)\s+(\d+)\b', text)
        if not templates or set(templates) != {'0'}:
            raise ValueError('Review changed computed builtin constructor versions: ' + path)
        pattern = r'(?<![\w.])(' + '|'.join(helpers) + r')\s+"([^"\\\n]+)"'
        for match in re.finditer(pattern, text):
            rows.append(dict(category='builtins', upstream=path, compiler=[],
                id=f'builtins:{path}#fn/{match[2]}/v0',
                builtin=dict(kind='fn', name=match[2], version=0),
                constructor=match[1], source_line=text.count('\n', 0, match.start()) + 1))
    return rows


def context(root, upstream, records, compiler):
    source = p.read_yaml(root / 'porting/sources.yaml')
    return dict(upstream_revision=source['upstream_revision'], target=source['target'])




def build(root, upstream, compile_packages=False):
    config = read_inputs(root)
    sources = p.read_yaml(root / 'porting/sources.yaml')
    if p.git_revision(upstream) != sources['upstream_revision']:
        raise ValueError('Upstream checkout must match porting/sources.yaml exactly')
    upstream_files = p.checked_files(upstream, sources['upstream_revision'])
    records, compiler = p.discover(root, upstream_files)
    known_builtins = {record['id'] for record in records}
    records += [record for record in computed_builtin_calls(upstream_files) if record['id'] not in known_builtins]
    provenance = context(root, upstream, records, compiler)
    coverage, review = coverage_index(upstream_files, compiler, config['coverage'])
    native_names, wrapper_counterparts = builtin_counterparts(upstream_files, compiler)
    features = sorted(config['features'], key=lambda f: (f['group'], f['name']))
    for feature in features:
        if any(path not in compiler for path in feature['sources']):
            raise ValueError('Unknown feature source: ' + feature['name'])

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
            # Exact identity can recover moved files without a maintained guess.
            exact = [c for c, body in compiler.items() if c.startswith(('StdLib/', 'packages/'))
                     and c.endswith('.dark') and path and body == upstream_files[path]]
            if exact:
                candidates = exact
            exception = config['source_exceptions'].get(path)
            source_rows.append(dict(id=record['id'], category=category, upstream=path,
                compiler=sorted(candidates), exception=exception,
                compilation=dict(status='not-run', reason='Run --compile-packages to collect source-unit compilation')
                    if category == 'packages' else None))
        elif category == 'parser':
            paths = config['parser_mappings'].get(record['id'], record['compiler'])
            if record['id'] in config['parser_mappings']:
                used_parser.add(record['id'])
            if not isinstance(paths, list) or any(path not in compiler for path in paths):
                raise ValueError('Invalid parser mapping: ' + record['id'])
            status = ('maintained' if record['id'] in used_parser else 'compiler-only' if path is None
                      else 'candidate' if paths else 'unmapped')
            parser.append(dict(id=record['id'], compiler=sorted(paths), mapping=status))
        elif category == 'builtins' and 'builtin' in record:
            # A generic fn "toString" registration elsewhere is not Builtin.toString.
            paths = config['builtin_mappings'].get(record['id'],
                sorted(native_names.get('Builtin.' + record['builtin']['name'], ()))
                if record['builtin']['version'] == 0 else [])
            if record['id'] in config['builtin_mappings']:
                used_builtins.add(record['id'])
            if not isinstance(paths, list) or any(path not in compiler for path in paths):
                raise ValueError('Invalid builtin mapping: ' + record['id'])
            counterparts = wrapper_counterparts.get(record['builtin']['name'], [])
            builtins.append(dict(id=record['id'], **record['builtin'], compiler=sorted(paths),
                mapping='maintained' if record['id'] in used_builtins else 'declared-builtin' if paths else 'source-counterpart' if counterparts else 'missing',
                discovery='constructor-call' if 'constructor' in record else 'literal-registration',
                source_line=record.get('source_line'),
                source_counterparts=counterparts,
                binding='mapped' if paths else 'not-mapped',
                note='Source counterparts identify implementation locations, not callable Builtin aliases or behavior.'))
    for key, used in [('source_mappings', used_mappings), ('parser_mappings', used_parser), ('builtin_mappings', used_builtins)]:
        if set(config[key]) - used:
            raise ValueError('Unused maintained mappings: ' + ', '.join(sorted(set(config[key]) - used)))
    for path, reason in config['source_exceptions'].items():
        if path not in upstream_files or not isinstance(reason, str) or not reason.strip():
            raise ValueError('Invalid source exception: ' + str(path))

    # Overrides can map split files that the root-module inventory called compiler-only.
    mapped_compiler = {path for row in source_rows if row['upstream'] for path in row['compiler']}
    source_rows = [row for row in source_rows if row['upstream'] or not set(row['compiler']) & mapped_compiler]

    package_path = root / PACKAGE_RECEIPT
    package_receipt = json.loads(package_path.read_text()) if package_path.exists() and not compile_packages else None
    # Compilation freshness is separate from declaration names/source comparison.
    # Only compilation receipts need the compiler/dependency fingerprint.
    compilation_context = (dict(**provenance, compiler_context_sha256=p.compiler_context(root, compiler))
                           if compile_packages or package_receipt else None)
    if package_receipt is not None and (package_receipt.get('schema') != 1
            or package_receipt.get('state') not in ('recorded', 'blocked')
            or not isinstance(package_receipt.get('packages'), list)):
        raise ValueError('Invalid package-results.json receipt')
    if compile_packages:
        package_receipt = dict(schema=1, **execute_packages(root, upstream_files, compilation_context))
        _, after = p.discover(root, upstream_files)
        if p.compiler_context(root, after) != compilation_context['compiler_context_sha256']:
            raise ValueError('Inputs changed during package compilation; previous results are unchanged')
        package_path.write_text(encoded(package_receipt))
    package_current = package_receipt is not None and package_receipt.get('context') == compilation_context
    compilation_state = package_receipt['state'] if package_current else 'stale' if package_receipt else 'not-run'
    results = {row['source']: row for row in package_receipt['packages']} if package_current else {}
    for row in source_rows:
        if row['category'] == 'packages':
            row['compilation'] = results.get(row['upstream'], dict(status='not-applicable' if row['upstream'] is None else compilation_state,
                reason='Compiler-only support file' if row['upstream'] is None else package_receipt.get('reason', '') if package_current else 'Run --compile-packages for current source inputs'))

    declaration_rows, warnings = inventory.declaration_rows(upstream_files, compiler,
        declarations, coverage, source_rows)
    review.extend(warnings)
    tests, warnings = inventory.test_inventory(root, upstream_files, compiler, declarations,
                                              records, config['test_mappings'])
    review.extend(warnings)
    builtin_sources = [r for r in records if r['category'] == 'builtins' and 'builtin_declaration_counts' in r]
    computed = [dict(source=path, **item) for path, body in sorted(upstream_files.items())
                if path.startswith('backend/src/Builtins/') and path.endswith('.fs')
                for item in p.builtin_declarations(body.decode())[1]]
    return dict(schema=3, context=provenance, features=features, parser=parser, builtins=builtins,
                computed_builtin_registrations=sum(r['builtin_declaration_counts']['computed'] for r in builtin_sources),
                computed_builtin_review=computed,
                declarations=declaration_rows,
                files=[{key: row[key] for key in ('id', 'category', 'upstream', 'compiler', 'compilation', 'exception')}
                       for row in source_rows],
                tests=tests, compilation=dict(state=compilation_state,
                    reason=package_receipt.get('reason', '') if package_current else '',
                    stage='native-declarations'), review=sorted(set(review)))


def markdown(value):
    feature_counts = Counter(f['status'] for f in value['features'])
    lines = ['# Repeatable compatibility ledger', '',
             'Current state of the compiler port. [Read what these measurements mean](../README.md).', '',
             'Upstream: `' + value['context']['upstream_revision'] + '`; target: `' + value['context']['target'] + '`.', '',
             '## Current state', '',
             '| Area | Recorded state |', '| --- | --- |',
             '| Language capabilities | ' + ', '.join(f'{feature_counts[status]} {status}' for status in ('implemented', 'partial', 'missing', 'not-assessed')) + ' |']
    for category in ('stdlib', 'packages'):
        rows = [r for r in value['declarations'] if category in r['categories'] and r['upstream']]
        counts = Counter(r['source_status'] for r in rows)
        lines.append('| ' + ('StdLib declarations' if category == 'stdlib' else 'Package declarations (includes StdLib)') + ' | '
                     + ', '.join(f'{counts[status]} {status}' for status in ('identical', 'different', 'missing', 'ambiguous')) + ' |')
    states = Counter(r['status'] for r in value['tests']['cases'])
    lines += ['| Upstream tests | ' + ', '.join(f'{states[status]} {status}' for status in ('enabled', 'disabled', 'partial', 'missing', 'ambiguous')) + ' |',
              '| Package compilation | ' + value['compilation']['state'] + ' |', '',
             'Counts are different measurements, not one overall compatibility score. Source comparisons use each declaration itself. Enabled tests are assumed to pass; this report does not run tests.', '',
             '## Language features', '',
             'Maintained assessments of specific language capabilities. See [how this ledger works](../README.md).', '']
    group = None
    for feature in value['features']:
        if feature['group'] != group:
            group = feature['group']
            lines += ['### ' + group, '', '| Feature | Status | Scope / limitation |', '| --- | --- | --- |']
        lines.append(f"| {p.cell(feature['name'])} | {feature['status']} | {p.cell(feature['notes'])} |")
    lines += ['', '## Source equivalency by declaration', '',
              'Counts are functions, values, types, traits and implementations. Named tests are in the test inventory. Packages include StdLib; file splits do not determine declaration matches. Identical means exact declaration source bytes, excluding surrounding comments/blank lines.', '',
              '| Ledger | Identical | Different | Missing | Ambiguous | Compiler-only |', '| --- | ---: | ---: | ---: | ---: | ---: |']
    for category in ('stdlib', 'packages'):
        counts = Counter(r['source_status'] for r in value['declarations'] if category in r['categories'])
        lines.append('| ' + category + ' | ' + ' | '.join(str(counts[k]) for k in ('identical', 'different', 'missing', 'ambiguous', 'compiler-only')) + ' |')
    package_rows = [r for r in value['declarations'] if r['upstream']]
    counts = Counter(r['coverage']['kind'] for r in package_rows)
    lines += ['', '## Existing test references', '',
              'Lexical references are coverage hints, not evidence of execution. One deterministic witness (the shortest indirect path) and the total reference count per declaration are in `porting/generated/ledger.json`.', '',
              '| Direct | Indirect | Maintained | Not identified |', '| ---: | ---: | ---: | ---: |',
              '| ' + ' | '.join(str(counts[k]) for k in ('direct', 'indirect', 'maintained', 'not-identified')) + ' |', '',
              'Package compilation state: **' + value['compilation']['state'] + '**. Each declaration links to its containing source-unit receipt. A failed unit does not establish that every declaration fails independently. Original units are parsed, checked and lowered through the native preamble pipeline.', '',
              '| Declarations in compiled units | In failed units | In blocked units | Stale | Not run |', '| ---: | ---: | ---: | ---: | ---: |',
              '| ' + ' | '.join(str(Counter(r['compilation']['status'] for r in package_rows)[k]) for k in ('compiled', 'fail', 'blocked', 'stale', 'not-run')) + ' |', '',
              '## Parser and builtins', '',
              'Builtin mappings distinguish callable Builtin declarations from source implementation counterparts. A counterpart is not a callable alias or a behavior pass. Computed registrations remain explicit review items.', '']
    for category in ('parser', 'builtins'):
        lines.append('- ' + category + ': ' + ', '.join(f'{k}: {v}' for k, v in sorted(Counter(r['mapping'] for r in value[category]).items())))
    lines += [f"- Computed builtin constructor templates: {value['computed_builtin_registrations']} (reviewed literal call sites expanded; not an assembled registry)", '',
              '## Interpreter tests versus compiler tests', '',
              'Source inventory only; enabled tests are assumed to pass. Compiler counterparts include existing upstream fixtures and named declarations. Recognizable named assertions and Test.all groups match legacy assertions by content; partial groups stay explicit. Helper-driven migrations require maintained mappings. Helper/preamble equivalence is not inferred from assertion content.', '',
              '| Upstream format | Total | Identical | Adapted | Different | Partial | Missing | Enabled | Disabled | Ambiguous |',
              '| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |']
    for form in ('legacy', 'named'):
        cases = [r for r in value['tests']['cases'] if r['upstream']['format'] == form]
        equivalency = Counter(r['equivalency'] for r in cases)
        states = Counter(r['status'] for r in cases)
        # Ambiguous cases have no selected counterpart and are excluded from
        # the definite missing count.
        missing = sum(r['status'] == 'missing' for r in cases)
        numbers = [len(cases), equivalency['identical'], equivalency['adapted'], equivalency['different'], equivalency['partial'],
                   missing, states['enabled'], states['disabled'], states['ambiguous']]
        lines.append('| ' + form + ' | ' + ' | '.join(map(str, numbers)) + ' |')
    extras = value['tests']['compiler_only']
    lines += ['', f"Unmatched compiler tests: {len(extras)} ({sum(r['enabled'] for r in extras)} enabled). Older imported fixtures absent from the current pin remain visible here; unmatched does not prove compiler authorship.", '',
              'Whole-file gates, individual line gates, fixture skip metadata and compiler expectation overrides are recorded per counterpart. Overrides are never counted as identical. Source forms that could not be indexed: ' + str(len(value['tests']['unindexed'])) + '.', '',
              '## Review queue', '']
    lines += ['- ' + p.cell(item) for item in value['review']]
    return '\n'.join(lines) + '\n'


def coverage_gaps(value):
    lines = ['# Existing-test reference gaps', '',
        'Regenerated from the pinned source index. These are not failing tests or proof that no test exists.', '',
        'Review found three recurring cases: numeric trait methods reached implicitly by operators '
        '(maintained references record the existing typed suites); archived `darklang/test/test_*` helpers '
        'whose newer tests redeclare their own functions; and CLI/host APIs without a discovered path '
        'from an existing test. None is silently exempted or supplied with a new test.', '',
        'Remaining implicit dispatch, reflection, and unfamiliar syntax can require maintained references. '
        'References do not establish runtime execution.', '']
    groups = defaultdict(list)
    for row in value['declarations']:
        if row['upstream'] and row['coverage']['kind'] == 'not-identified':
            groups[row['upstream']['source']].append(row)
    for path, gaps in sorted(groups.items()):
        if gaps:
            lines += ['## ' + path, '', '| Declaration | Kind | Line |', '| --- | --- | ---: |']
            lines += [f"| {p.cell(d['name'])} | {d['kind']} | {d['upstream']['line']} |" for d in gaps]
            lines += ['']
    return '\n'.join(lines) + '\n'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--upstream', type=Path, required=True)
    parser.add_argument('--compile-packages', action='store_true', help='Compile pinned original declarations; no probes and no test execution')
    parser.add_argument('--check', action='store_true', help='Compare deterministic generated outputs without writing')
    args = parser.parse_args()
    if args.check and args.compile_packages:
        parser.error('--check does not execute compilation')
    try:
        value = build(ROOT, args.upstream.resolve(), args.compile_packages)
        machine = encoded(value)
        machine_path = ROOT / OUTPUT
        if args.check:
            if not machine_path.exists() or machine_path.read_text() != machine:
                raise ValueError('Ledger is stale: ' + OUTPUT)
        else:
            machine_path.parent.mkdir(parents=True, exist_ok=True)
            machine_path.write_text(machine)
        outputs = {REPORT: markdown(value), GAPS: coverage_gaps(value)}
        for name, contents in outputs.items():
            path = ROOT / name
            if args.check:
                if not path.exists() or path.read_text() != contents:
                    raise ValueError('Ledger is stale: ' + name)
            else:
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(contents)
        print('Ledger is current.' if args.check else 'Regenerated declaration, test inventory and coverage ledgers.')
        if args.compile_packages and (value['compilation']['state'] != 'recorded'
                or any(row['compilation']['status'] != 'compiled' for row in value['files']
                       if row['category'] == 'packages' and row['upstream'])):
            return 1
        return 0
    except (OSError, ValueError, KeyError, TypeError, subprocess.CalledProcessError) as error:
        print('ledger: ' + str(error), file=sys.stderr)
        return 2


if __name__ == '__main__':
    sys.exit(main())
