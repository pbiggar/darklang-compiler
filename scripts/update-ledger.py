#!/usr/bin/env python3
"""Regenerate the lean porting ledger from pinned sources and existing tests.

No probes, test programs, expected values, or differential corpus are generated.
Maintained decisions live in ledger-inputs.yaml; execution receipts are separate.
"""
from __future__ import annotations

import argparse
from collections import Counter, defaultdict
import json
from pathlib import Path
import re
import subprocess
import sys
import tempfile

import yaml

import ledger_sources as p
import ledger_inventory as inventory

ROOT = p.ROOT
INPUTS = 'porting/ledger-inputs.yaml'
PACKAGE_RECEIPT = 'porting/package-results.json'
OUTPUT = 'porting/generated/summary.yaml'
REPORT = 'docs/porting/generated/ledger.md'


def encoded(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True, indent=1) + '\n'


def read_inputs(root):
    value = p.read_yaml(root / INPUTS)
    fields = {'schema', 'source_mappings', 'builtin_mappings',
              'source_exceptions', 'test_mappings'}
    if set(value) != fields or value['schema'] != 2:
        raise ValueError('Invalid ledger-inputs.yaml schema')
    for key in fields - {'schema'}:
        if not isinstance(value[key], dict):
            raise ValueError(f'{key} must be a mapping')
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
        if path.startswith('packages/') and path.endswith('.dark'):
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
    known_builtins = {record['id'] for record in records if 'id' in record}
    records += [record for record in computed_builtin_calls(upstream_files) if record['id'] not in known_builtins]
    provenance = context(root, upstream, records, compiler)
    review = []
    native_names, wrapper_counterparts = builtin_counterparts(upstream_files, compiler)
    source_rows, builtins = [], []
    used_mappings = set()
    used_builtins = set()
    for record in sorted((r for r in records if 'id' in r), key=lambda r: r['id']):
        category, path = record['category'], record['upstream']
        if category in ('packages', 'stdlib'):
            candidates = config['source_mappings'].get(path, record['compiler'])
            if path in config['source_mappings']:
                used_mappings.add(path)
            if not isinstance(candidates, list) or any(not isinstance(c, str) or c not in compiler for c in candidates):
                raise ValueError(f'Invalid source mapping: {path}')
            # Exact identity can recover moved files without a maintained guess.
            exact = [c for c, body in compiler.items() if c.startswith('packages/')
                     and c.endswith('.dark') and path and body == upstream_files[path]]
            if exact:
                candidates = exact
            exception = config['source_exceptions'].get(path)
            source_rows.append(dict(id=record['id'], category=category, upstream=path,
                compiler=sorted(candidates), exception=exception))
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
    for key, used in [('source_mappings', used_mappings), ('builtin_mappings', used_builtins)]:
        if set(config[key]) - used:
            raise ValueError('Unused maintained mappings: ' + ', '.join(sorted(set(config[key]) - used)))
    for path, reason in config['source_exceptions'].items():
        if path not in upstream_files or not isinstance(reason, str) or not reason.strip():
            raise ValueError('Invalid source exception: ' + str(path))

    if compile_packages:
        compilation_context = dict(**provenance, compiler_context_sha256=p.compiler_context(root, compiler))
        receipt = dict(schema=1, **execute_packages(root, upstream_files, compilation_context))
        _, after = p.discover(root, upstream_files)
        if p.compiler_context(root, after) != compilation_context['compiler_context_sha256']:
            raise ValueError('Inputs changed during package compilation; previous results are unchanged')
        (root / PACKAGE_RECEIPT).write_text(encoded(receipt))
        if receipt['state'] != 'recorded' or any(r['status'] != 'compiled' for r in receipt['packages']):
            raise ValueError('Package compilation incomplete; see porting/package-results.json')

    declaration_rows, warnings = inventory.declaration_rows(upstream_files, compiler,
        declarations, source_rows)
    review.extend(warnings)
    tests, warnings = inventory.test_inventory(root, upstream_files, compiler, declarations,
                                              records, config['test_mappings'])
    review.extend(warnings)
    for path, body in sorted(upstream_files.items()):
        if path.startswith('backend/src/Builtins/') and path.endswith('.fs'):
            for item in p.builtin_declarations(body.decode())[1]:
                review.append(f"Computed builtin registration: {path}:{item['line']}")
    return dict(context=provenance, builtins=builtins, declarations=declaration_rows,
                tests=tests, review=sorted(set(review)))



def yaml_text(value):
    return yaml.safe_dump(value, sort_keys=False, allow_unicode=True, width=120)


def relative(path, root):
    if not path.startswith(root):
        raise ValueError('Path outside declared root: ' + path)
    return path[len(root):]


def unique(mapping, key, value):
    if key in mapping:
        raise ValueError('Repeated ledger identity: ' + key)
    mapping[key] = value


def capability_rows(root, filename):
    value = p.read_yaml(root / filename)
    if set(value) != {'schema', 'groups'} or value['schema'] != 2:
        raise ValueError('Invalid capability ledger: ' + filename)
    for group, rows in value['groups'].items():
        for name, row in rows.items():
            state = row if isinstance(row, str) else row.get('status')
            if state not in {'supported', 'partial', 'missing', 'unassessed', 'excluded'}:
                raise ValueError('Invalid capability status: ' + name)
            if isinstance(row, dict) and (set(row) - {'status', 'notes'} or not row.get('notes')):
                raise ValueError('Invalid capability notes: ' + name)
            if state in {'partial', 'missing', 'excluded'} and not isinstance(row, dict):
                raise ValueError('Explain capability gap: ' + name)
    return value['groups']


def compact(value, root):
    """Keep port decisions and source relationships; omit analysis intermediates."""
    stdlib, packages, builtin_files, test_sources = {}, {}, {}, {}
    identities = Counter((r['upstream']['source'], r['name']) for r in value['declarations'])
    for row in value['declarations']:
        if row['upstream'] is None:
            continue
        path = row['upstream']['source']
        is_stdlib = path.startswith('packages/darklang/stdlib/')
        path = relative(path, 'packages/darklang/stdlib/' if is_stdlib else 'packages/')
        files = stdlib if is_stdlib else packages
        entries = files.setdefault(path, {})
        item = {'status': row['source_status']}
        if row['source_exception']:
            item['notes'] = row['source_exception']
        # The same qualified name can legitimately have multiple declarations
        # (for example an impl and its method). Preserve kind for collisions only.
        key = row['name']
        duplicates = identities[(row['upstream']['source'], row['name'])]
        if duplicates > 1:
            key += ' [' + row['kind'] + ']'
            if key in entries:
                key += '@L' + str(row['upstream']['line'])
        unique(entries, key, item if len(item) > 1 else item['status'])

    for row in value['builtins']:
        path, ident = row['id'].removeprefix('builtins:').split('#', 1)
        name = row['name'] + '_v' + str(row['version'])
        if row['kind'] != 'fn':
            name = row['kind'] + '/' + name
        # Source counterparts are not evidence of an actual Builtin binding.
        status = 'present' if row['binding'] == 'mapped' else 'unassessed' if row['source_counterparts'] else 'missing'
        entries = builtin_files.setdefault(relative(path, 'backend/src/Builtins/'), {})
        unique(entries, name, status)

    for row in value['tests']['cases']:
        upstream = row['upstream']
        path = relative(upstream['source'], 'packages/' if upstream['source'].startswith('packages/')
                        else 'backend/testfiles/execution/')
        path = ('packages/' if upstream['source'].startswith('packages/') else 'execution/') + path
        item = {'status': 'unmapped' if row['status'] == 'missing' else row['status']}
        choices = row['compiler']
        if choices:
            ids = [relative(c['id'], 'test/fixtures/e2e/') if c['id'].startswith('test/fixtures/e2e/')
                   else 'repo:' + c['id'] for c in choices]
            item['compiler'] = ids[0] if len(ids) == 1 else ids
            disabled = {i: c['reason'] for i, c in zip(ids, choices) if not c['enabled']}
            if disabled:
                item['reason'] = (next(iter(disabled.values())) if len(set(disabled.values())) == 1
                                  else disabled)
            overrides = {i: c['expectation_override'] for i, c in zip(ids, choices) if c['expectation_override']}
            if overrides:
                item['expectation'] = next(iter(overrides.values())) if len(choices) == 1 else overrides
            if row['equivalency'] == 'different':
                item['notes'] = 'Matched by name; test bodies differ.'
        if row['status'] == 'partial':
            item['notes'] = 'Only part of the upstream assertion group has a counterpart.'
        if row['reason']:
            item['notes'] = (item.get('notes', '') + ' ' + row['reason']).strip()
        entries = test_sources.setdefault(path, {})
        unique(entries, upstream['name'], item if len(item) > 1 else item['status'])

    features = capability_rows(root, 'porting/language-features.yaml')
    parser = capability_rows(root, 'porting/parser.yaml')
    outputs = {}
    for name, path_root, entries in [('stdlib', 'packages/darklang/stdlib/', stdlib),
                                     ('packages', 'packages/', packages),
                                     ('builtins', 'backend/src/Builtins/', builtin_files)]:
        outputs['porting/generated/' + name + '.yaml'] = dict(root=path_root, files=entries)
    outputs['porting/generated/tests.yaml'] = dict(
        upstream_roots=dict(execution='backend/testfiles/execution/', packages='packages/'),
        compiler_root='test/fixtures/e2e/', sources=test_sources)
    summary = dict(upstream=dict(repository='darklang/dark', revision=value['context']['upstream_revision']))
    for name, groups in [('features', features), ('parser', parser)]:
        summary[name] = dict(Counter(v if isinstance(v, str) else v['status']
                                    for rows in groups.values() for v in rows.values()))
    for name, files in [('stdlib', stdlib), ('packages', packages), ('builtins', builtin_files), ('tests', test_sources)]:
        summary[name] = dict(Counter(v if isinstance(v, str) else v['status']
                                    for rows in files.values() for v in rows.values()))
    outputs[OUTPUT] = summary
    return outputs


def markdown(summary):
    lines = ['# Current compiler port status', '', '[How to read and update this ledger](../README.md).', '',
             'Interpreter revision: `' + summary['upstream']['revision'] + '`.', '',
             '| Area | Status counts |', '| --- | --- |']
    for name in ('features', 'parser', 'stdlib', 'packages', 'builtins', 'tests'):
        path = ('../../language-features.yaml' if name == 'features' else '../../parser.yaml' if name == 'parser'
                else '../../../porting/generated/' + name + '.yaml')
        # Maintained capability files live at repository/porting, too.
        if name in ('features', 'parser'):
            path = '../../../porting/' + ('language-features.yaml' if name == 'features' else 'parser.yaml')
        lines.append('| [' + name + '](' + path + ') | ' + ', '.join(str(n) + ' ' + state for state, n in summary[name].items()) + ' |')
    lines += ['', 'Excluded capabilities are intentional compiler differences, not pending porting work. '
              'StdLib and package states compare declaration source, not behavior. '
              'Builtin presence identifies a binding, not behavioral parity. '
              'Enabled tests have not necessarily been executed in this checkout. '
              'Packages exclude StdLib; all counts refer to upstream items only.', '']
    return '\n'.join(lines)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--upstream', type=Path, required=True)
    parser.add_argument('--compile-packages', action='store_true', help='Write a separate package compilation run receipt')
    parser.add_argument('--check', action='store_true', help='Check committed compact inventories without executing tests')
    args = parser.parse_args()
    if args.check and args.compile_packages:
        parser.error('--check does not execute compilation')
    try:
        value = build(ROOT, args.upstream.resolve(), args.compile_packages)
        outputs = compact(value, ROOT)
        rendered = {path: yaml_text(data) for path, data in outputs.items()}
        rendered[REPORT] = markdown(outputs[OUTPUT])
        for name, text in rendered.items():
            path = ROOT / name
            if args.check:
                if not path.exists() or path.read_text() != text:
                    raise ValueError('Ledger is stale: ' + name)
            else:
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(text)
        for warning in value['review']:
            print('Review: ' + warning, file=sys.stderr)
        print('Ledger is current.' if args.check else 'Regenerated compact compatibility ledgers.')
        return 0
    except (OSError, ValueError, KeyError, TypeError, subprocess.CalledProcessError) as error:
        print('ledger: ' + str(error), file=sys.stderr)
        return 2


if __name__ == '__main__':
    sys.exit(main())
