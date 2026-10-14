#!/usr/bin/env python3
"""Discover porting inputs and render policy-aware inventories, without executing code."""
from __future__ import annotations

import argparse
import hashlib
import re
import subprocess
import sys
from pathlib import Path

try:
    import yaml
except ImportError:
    sys.exit('Install scripts/porting-requirements.txt in a Python virtual environment.')

ROOT = Path(__file__).resolve().parent.parent
CATEGORIES = ('language', 'parser', 'tests', 'packages', 'builtins', 'stdlib')
FIELDS = {
    'source': {'identical', 'faithful-port', 'independent-implementation',
               'identical-with-adapter', 'undecided'},
    'behavior': {'identical', 'identical-with-aot-rules', 'undecided'},
    'freshness': {'follow-upstream', 'fixed-baseline', 'compiler-owned'},
    'disposition': {'required', 'deferred', 'outside-scope', 'undecided'},
    'destination': None,
}


class UniqueLoader(yaml.SafeLoader):
    """Reject duplicate keys instead of silently discarding knowledge."""


def unique_mapping(loader, node):
    result = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node)
        if key in result:
            raise ValueError(f'Duplicate YAML key: {key}')
        result[key] = loader.construct_object(value_node)
    return result


UniqueLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, unique_mapping)


def read_yaml(path):
    result = yaml.load(path.read_text(), Loader=UniqueLoader)
    if not isinstance(result, dict):
        raise ValueError(f'{path.name}: expected a YAML mapping')
    return result


def digest(data):
    return hashlib.sha256(data).hexdigest()


def within(path, scope):
    return not scope or path == scope or path.startswith(scope.rstrip('/') + '/')


def validate_node(node, parent=None):
    allowed = {'scope', 'item', 'policy', 'current', 'reason', 'questions',
               'decisions', 'depends_on', 'children', 'category', 'extends'}
    if not isinstance(node, dict) or set(node) - allowed:
        raise ValueError('Unknown manifest fields or non-mapping node')
    if ('scope' in node) == ('item' in node):
        raise ValueError('Each node needs exactly one scope or item selector')
    selector = node.get('scope', node.get('item'))
    if not isinstance(selector, str) or '..' in selector.split('/') or selector.startswith('/'):
        raise ValueError('Selectors must be relative, explicit paths or item IDs')
    if parent is not None:
        if 'item' in parent or ('scope' in node and not within(node['scope'], parent['scope'])):
            raise ValueError('Nested selector escapes its parent scope')
    policy = node.get('policy', {})
    if not isinstance(policy, dict) or set(policy) - set(FIELDS):
        raise ValueError('Unknown policy fields')
    for field, value in policy.items():
        if not isinstance(value, str) or not value or (FIELDS[field] and value not in FIELDS[field]):
            raise ValueError(f'Invalid policy {field}: {value}')
    for field in ('questions', 'decisions', 'depends_on'):
        values = node.get(field, [])
        if not isinstance(values, list) or any(not isinstance(v, str) for v in values):
            raise ValueError(f'{field} must be a list of strings')
    if parent is not None and policy and not node.get('reason'):
        raise ValueError('Policy overrides require a reason')
    children = node.get('children', [])
    if not isinstance(children, list):
        raise ValueError('children must be a list')
    for child in children:
        validate_node(child, node)
    for index, child in enumerate(children):
        for other in children[index + 1:]:
            if ('item' in child and child.get('item') == other.get('item')) or (
                'scope' in child and 'scope' in other and
                (within(child['scope'], other['scope']) or within(other['scope'], child['scope']))):
                raise ValueError('Overlapping sibling selectors; nest them explicitly')


def load_manifests(root):
    manifests = {}
    for category in CATEGORIES:
        node = read_yaml(root / 'porting/manifests' / f'{category}.yaml')
        validate_node(node)
        if node.get('category') != category:
            raise ValueError('Manifest category must match its filename')
        manifests[category] = node
    def inherited(category, visiting=()):
        if category in visiting or category not in manifests:
            raise ValueError('Invalid or cyclic manifest inheritance')
        node = manifests[category]
        result = inherited(node['extends'], visiting + (category,)) if 'extends' in node else {}
        result.update(node.get('policy', {}))
        if set(result) != set(FIELDS):
            raise ValueError(f'{category}: incomplete root policy')
        return result
    return manifests, {c: inherited(c) for c in CATEGORIES}


def policy_for(node, defaults, item, ancestors=()):
    policy = dict(defaults)
    trail = []
    dependencies = []
    def visit(current):
        policy.update(current.get('policy', {}))
        trail.append(current.get('scope', current.get('item')) or '(category root)')
        dependencies.extend(current.get('depends_on', []))
        matches = [child for child in current.get('children', []) if
                   child.get('item') == item['id'] or
                   ('scope' in child and within(item['scope'], child['scope']))]
        if len(matches) > 1:
            raise ValueError(f'Ambiguous policy for {item["id"]}')
        if matches:
            visit(matches[0])
    for ancestor in ancestors:
        visit(ancestor)
    visit(node)
    return policy, trail, sorted(set(dependencies))


def strip_comments(text, strings=False):
    """Mask nested F#/OCaml comments and literals, preserving line coordinates."""
    out = list(text)
    i, depth = 0, 0
    def mask(start, end):
        for j in range(start, end):
            if out[j] != '\n':
                out[j] = ' '
    while i < len(text):
        if text.startswith('(*', i):
            depth += 1
            mask(i, i + 2)
            i += 2
        elif depth and text.startswith('*)', i):
            depth -= 1
            mask(i, i + 2)
            i += 2
        elif depth:
            mask(i, i + 1)
            i += 1
        elif text.startswith('//', i):
            end = text.find('\n', i)
            end = len(text) if end < 0 else end
            mask(i, end)
            i = end
        elif text[i] == '"':
            start = i
            triple = text.startswith('"""', i)
            delimiter = '"""' if triple else '"'
            i += len(delimiter)
            while i < len(text) and not text.startswith(delimiter, i):
                i += 2 if text[i] == '\\' else 1
            i = min(len(text), i + len(delimiter))
            if strings:
                mask(start, i)
        else:
            i += 1
    return ''.join(out)


def module_names(text):
    return sorted(set(re.findall(r'^module\s+([\w.]+)', strip_comments(text, True), re.M)))


def builtin_declarations(text):
    """Enumerate literal registration candidates and expose computed-name gaps."""
    clean = strip_comments(text, True)
    literals, computed = [], []
    for match in re.finditer(r'\bname\s*=\s*(fn|value)\b', clean):
        literal = re.match(r'\s*"([^"\\\n]+)"\s+(\d+)\b', text[match.end():])
        line = text.count('\n', 0, match.start()) + 1
        if literal:
            literals.append(dict(kind=match[1], name=literal[1], version=int(literal[2]), line=line))
        else:
            computed.append(dict(kind=match[1], line=line))
    return literals, computed


def named_tests(text):
    """Discover source declarations, not runner cases, with indentation-scoped names.

    Fail closed on unfamiliar headers: this bounded scanner must not silently
    invent a module or lose a declaration when upstream syntax changes.
    """
    root, modules, result, seen = (), [], [], set()
    for number, line in enumerate(strip_comments(text, True).splitlines(), 1):
        if not line.strip():
            continue
        indent = len(line) - len(line.lstrip())
        while modules and indent <= modules[-1][0]:
            modules.pop()
        if modules and modules[-1][2] is None:
            column, names, _ = modules[-1]
            modules[-1] = (column, names, indent)
        declaration_indent = modules[-1][2] if modules else 0
        statement = line.strip()
        if indent == declaration_indent and re.match(r'module\b', statement):
            match = re.fullmatch(r'module\s+([\w\x27]+(?:\.[\w\x27]+)*)(\s*=)?\s*', statement)
            if not match or (not match[2] and indent):
                raise ValueError(f'Unsupported module header at line {number}')
            names = tuple(match[1].split('.'))
            if match[2]:
                modules.append((indent, names, None))
            else:
                root = names
                modules.clear()
        elif indent == declaration_indent and re.match(r'test\b', statement):
            match = re.match(r'test\s+([\w\x27]+)\s*=', statement)
            if not match:
                raise ValueError(f'Unsupported named test header at line {number}')
            name = '.'.join(root + tuple(part for _, names, _ in modules for part in names) + (match[1],))
            if name in seen:
                raise ValueError(f'Duplicate named test {name} at line {number}')
            seen.add(name)
            result.append(dict(name=name, line=number))
    return result


def variants(text, wanted):
    clean = strip_comments(text, True)
    matches = list(re.finditer(r'^(?:type|and)\s+(\w+)\b[^=\n]*=', clean, re.M))
    result = {}
    for index, match in enumerate(matches):
        if match[1].lower() not in wanted:
            continue
        end = matches[index + 1].start() if index + 1 < len(matches) else len(clean)
        block = clean[match.end():end]
        block = re.split(r'^(?:let|module)\s', block, maxsplit=1, flags=re.M)[0]
        for case in re.findall(r'(?:^\s*\||\A\s*)\s*([A-Z]\w*)\b', block, re.M):
            result[(match[1].lower(), case)] = match.start()
    return result


def git_revision(path):
    return subprocess.check_output(['git', '-C', str(path), 'rev-parse', 'HEAD'], text=True).strip()


def checked_files(upstream, revision):
    paths = subprocess.check_output(['git', '-C', str(upstream), 'ls-tree', '-r', '--name-only', revision], text=True)
    result = {}
    for path in paths.splitlines():
        if path.endswith(('.dark', '.fs', '.md', '.fsproj')) and path.startswith(('packages/', 'backend/src/', 'backend/tests/', 'backend/testfiles/')):
            result[path] = subprocess.check_output(['git', '-C', str(upstream), 'show', f'{revision}:{path}'])
    return result


def discover(root, upstream_files):
    records = []
    compiler_files = {p.relative_to(root).as_posix(): p.read_bytes() for directory in ('src', 'StdLib', 'packages', 'test')
                      for p in (root / directory).rglob('*') if p.is_file() and p.suffix in
                      ('.ml', '.mli', '.dark', '.e2e', '.syntax')}
    modules = {}
    for path, body in compiler_files.items():
        if path.endswith('.dark') and path.startswith(('StdLib/', 'packages/')):
            for name in module_names(body.decode()):
                modules.setdefault(name, []).append(path)
    builtin_candidates = {}
    for path in modules.get('Builtin', []):
        for name in re.findall(r'^let\s+([\w\x27]+)\b', strip_comments(compiler_files[path].decode(), True), re.M):
            builtin_candidates.setdefault(name, []).append(path)
    registry = 'src/DarkStdlib.ml'
    for name in re.findall(r'\bfn\s+"([^"\n]+)"', strip_comments(compiler_files.get(registry, b'').decode())):
        builtin_candidates.setdefault(name, []).append(registry)
    def add(category, key, source, counterparts=(), observation='candidate mapping; review required', detail=''):
        counterparts = sorted(set(counterparts))
        source_hash = digest(upstream_files[source]) if source in upstream_files else None
        records.append(dict(id=f'{category}:{key}', category=category, scope=source,
                            upstream=source if source in upstream_files else None,
                            compiler=counterparts, upstream_sha256=source_hash,
                            compiler_sha256={p: digest(compiler_files[p]) for p in counterparts},
                            observation=observation, detail=detail))
    # File-level mappings use declared root modules, not casing/path guesses.
    mapped = set()
    for path, body in sorted(upstream_files.items()):
        if not path.startswith('packages/') or not path.endswith('.dark'):
            continue
        names = module_names(body.decode())
        candidates = [p for name in names for p in modules.get(name, [])]
        mapped.update(candidates)
        observation = 'no root-module candidate' if not candidates else 'root-module candidates; split/adaptation review required'
        if len(candidates) == 1 and compiler_files[candidates[0]] == body:
            observation = 'byte-identical source'
        category = 'stdlib' if within(path, 'packages/darklang/stdlib') else 'packages'
        add(category, path, path, candidates, observation, ', '.join(names))
        # Package inventory includes StdLib and test sources, with cross-category references.
        if category == 'stdlib':
            add('packages', path, path, candidates, observation, 'Also listed in stdlib inventory')
        try:
            tests = named_tests(body.decode())
        except ValueError as error:
            raise ValueError(f'{path}: {error}') from error
        if tests:
            add('tests', path, path, (), 'named-test source; execution adapter unverified',
                f'{len(tests)} declaration candidates: ' + ', '.join(t['name'].split('.')[-1] for t in tests))
            for test in tests:
                add('tests', f'{path}#{test["name"]}', path, (),
                    'named-test declaration; runtime discovery and execution unverified', test['name'])
                records[-1].update(scope=f'{path}/named/' + test['name'].replace('.', '/'),
                                   source_line=test['line'], named_test=test['name'],
                                   source_group=f'tests:{path}')
    for path in sorted(set(p for p in compiler_files if p.endswith('.dark') and p.startswith(('StdLib/', 'packages/'))) - mapped):
        category = 'stdlib' if path.startswith('StdLib/') else 'packages'
        add(category, 'compiler/' + path, 'compiler/' + path, [path], 'compiler-only source; classify intended treatment')
    for path in sorted(upstream_files):
        if path.startswith('backend/src/LibParser/') and path.endswith('.fs'):
            stem = Path(path).stem
            counterpart = f'src/frontend/interpreter/{stem}.ml'
            add('parser', path, path, [counterpart] if counterpart in compiler_files else (),
                'same-stem port candidate' if counterpart in compiler_files else 'component mapping needed')
        if path.startswith('backend/tests/') and path.endswith('.fs') or path.startswith('backend/testfiles/'):
            add('tests', path, path, (), 'backend test/fixture source; adapter classification needed')
    for path in sorted(p for p in compiler_files if p.startswith('src/frontend/interpreter/') and p.endswith('.ml')):
        counterpart = 'backend/src/LibParser/' + Path(path).stem + '.fs'
        if counterpart not in upstream_files:
            add('parser', 'compiler/' + path, 'compiler/' + path, [path], 'compiler split component or adapter; classify')
    up = 'backend/src/LibParser/WrittenTypes.fs'
    cp = 'src/frontend/interpreter/WrittenTypes.ml'
    kinds = {'expr', 'declaration', 'typereference', 'matchpattern', 'letpattern'}
    up_variants = variants(upstream_files[up].decode(), kinds)
    cp_variants = variants(compiler_files[cp].decode(), kinds)
    for kind, case in sorted(set(up_variants) | set(cp_variants)):
        observation = ('constructor-name candidate; semantics unverified' if (kind, case) in cp_variants and (kind, case) in up_variants
                       else 'upstream constructor absent by name' if (kind, case) in up_variants else 'compiler-only constructor name')
        add('language', f'{kind}/{case}', up, [cp], observation,
            'Structural discovery only; same spelling does not establish the same shape or behavior')
    # Seed broad behavioral workstreams; trials refine these, not a guessed language spec.
    families = {
        'evaluation-and-control-flow': ('backend/src/LibExecution/Interpreter.fs', ['src/frontend/WrittenExpressions.ml', 'src/passes/anf/AST_to_ANF.ml']),
        'types-and-generic-functions': ('backend/src/LibExecution/TypeChecker.fs', ['src/frontend/WrittenChecking.ml', 'src/frontend/WrittenTypeSupport.ml']),
        'traits-and-bounds': ('backend/src/LibExecution/ProgramTypes.fs', ['src/frontend/WrittenDeclarations.ml', 'src/frontend/WrittenCallSupport.ml']),
        'effects': ('backend/src/LibExecution/Effects.fs', ['src/frontend/interpreter/LibExecution_Effects.ml']),
        'values-and-errors': ('backend/src/LibExecution/RuntimeTypes.fs', ['src/frontend/ValueRendering.ml', 'src/memory/RuntimeDataLayout.ml']),
        'name-resolution': ('backend/src/LibParser/NameResolver.fs', ['src/frontend/WrittenEnvironment.ml']),
    }
    for name, (source, candidates) in families.items():
        if source in upstream_files:
            add('language', 'family/' + name, source, [path for path in candidates if path in compiler_files],
                'curated behavioral workstream; requirements and focused probes need review')
    for path, body in sorted(upstream_files.items()):
        if not path.startswith('backend/src/Builtins/') or not path.endswith('.fs'):
            continue
        declarations, computed = builtin_declarations(body.decode())
        if declarations:
            add('builtins', path, path,
                sorted(p for p in compiler_files if p.startswith('StdLib/Builtin') and p.endswith('.dark')) + ['src/DarkStdlib.ml'],
                'builtin declaration source; assembled registry and signatures unverified',
                f'{len(declarations)} literal declaration candidates: ' + ', '.join(f'{d["kind"]} {d["name"]}_v{d["version"]}' for d in declarations) +
                f'; {len(computed)} computed registration constructors require assembled-registry discovery')
        else:
            add('builtins', path, path, (), 'registration/support source; assembled registry export needed',
                f'{len(computed)} computed registration constructors')
        records[-1]['builtin_declaration_counts'] = dict(literal=len(declarations), computed=len(computed))
        for declaration in declarations:
            kind, name, version = declaration['kind'], declaration['name'], declaration['version']
            counterparts = builtin_candidates.get(name, [])
            add('builtins', f'{path}#{kind}/{name}/v{version}', path, counterparts,
                'literal registration candidate; assembled binding, signature and behavior unverified',
                f'Builtin.{name}_v{version}; compiler candidates use same-spelling declaration heuristics only')
            records[-1].update(scope=f'{path}/declarations/{kind}/{name}/v{version}',
                               source_line=declaration['line'], builtin=dict(kind=kind, name=name, version=version),
                               source_group=f'builtins:{path}')
    # Retain all legacy imports and configuration observations, not inferred skip counts.
    runner = compiler_files.get('test/test-suite-tooling/E2EDiscovery.ml', b'').decode()
    def gate_block(name):
        if not runner:
            return ''
        if f'let {name} =' not in runner:
            raise ValueError(f'Runner gate structure changed: {name}; update discovery')
        return runner.split(f'let {name} =', 1)[1].split('\nlet ', 1)[0]
    disabled_paths = re.findall(r'"(test/fixtures/e2e/upstream/[^"\n]+)"', gate_block('disabledUpstreamFiles'))
    disabled = set(disabled_paths)
    raw_lines = re.findall(r'\(\s*"([^"]+)"\s*,\s*\[([^]]*)\]\s*\)', gate_block('disabledUpstreamLines'))
    line_entries = {path: [int(n) for n in re.findall(r'\d+', numbers)] for path, numbers in raw_lines}
    paths = sorted(p for p in compiler_files if p.startswith('test/fixtures/e2e/upstream/') and p.endswith('.dark'))
    if len(disabled) != len(disabled_paths) or len(line_entries) != len(raw_lines):
        raise ValueError('Duplicate configured runner gate paths')
    if (disabled | set(line_entries)) - set(paths):
        raise ValueError('Configured runner gate references an absent legacy fixture')
    for path, numbers in line_entries.items():
        if len(numbers) != len(set(numbers)) or any(n < 1 or n > len(compiler_files[path].decode().splitlines()) for n in numbers):
            raise ValueError('Duplicate or out-of-range configured runner line gate')
    for path in paths:
        add('tests', 'compiler/' + path, 'compiler/' + path, [path],
            'legacy imported fixture; real parser/gate YAML export available but not executed here',
            f'Configured whole-file gate: {path in disabled}; line entries: ' + ', '.join(map(str, line_entries.get(path, []))) +
            '; fixture metadata and runtime filters still require runner export')
        records[-1]['configured_gates'] = dict(whole_file=path in disabled, lines=line_entries.get(path, []))
    ids = [r['id'] for r in records]
    if len(ids) != len(set(ids)):
        raise ValueError('Duplicate discovered identities')
    return sorted(records, key=lambda item: item['id']), compiler_files


def check_selectors(manifests, records):
    for category, node in manifests.items():
        items = [r for r in records if r['category'] == category]
        def visit(current, candidates, root=True):
            matched = candidates if root else [r for r in candidates if current.get('item') == r['id'] or
                                               ('scope' in current and within(r['scope'], current['scope']))]
            if not matched and not root:
                raise ValueError(f'Unmatched exception in {category}: {current.get("scope", current.get("item"))}')
            for child in current.get('children', []):
                visit(child, matched, False)
        visit(node, items)


def cell(value):
    return str(value).replace('|', '\\|').replace('\n', ' ')


def compiler_context(root, compiler_files):
    """Hash source/build inputs, not timestamps, output artifacts or evidence."""
    inputs = dict(compiler_files)
    for directory in ('bin', 'scripts', 'src', 'test', 'tools', 'StdLib', 'packages'):
        for path in (root / directory).rglob('*'):
            if path.is_file() and (path.suffix in ('.ml', '.mli', '.c', '.h', '.S', '.s', '.sh', '.py') or path.name == 'dune'):
                inputs[path.relative_to(root).as_posix()] = path.read_bytes()
    for name in ('build', 'run-tests', 'dune-project', 'dune', 'dune-workspace',
                 'library-sources.list', 'dependencies.lock', 'scripts/porting-requirements.txt',
                 'porting/sources.yaml', 'porting/issues.yaml'):
        path = root / name
        if path.is_file():
            inputs[name] = path.read_bytes()
    for path in sorted((root / 'porting/manifests').glob('*.yaml')):
        inputs[path.relative_to(root).as_posix()] = path.read_bytes()
    context = hashlib.sha256()
    for path, body in sorted(inputs.items()):
        context.update(path.encode() + b'\0' + digest(body).encode() + b'\n')
    return context.hexdigest()


def validate_runner_export(value):
    """Validate an actual discovery document without interpreting Dark source."""
    fields = {'schema', 'target', 'mode', 'files', 'tests', 'parse_errors'}
    if not isinstance(value, dict) or set(value) != fields or value['schema'] != 1 or value['target'] != 'linux-x64' or value['mode'] != 'discovery-only':
        raise ValueError('Invalid runner export header')
    for name in ('files', 'tests', 'parse_errors'):
        if not isinstance(value[name], list):
            raise ValueError(f'Invalid runner export {name}')
    files = value['files']
    if any(not isinstance(path, str) or not path.startswith('test/fixtures/e2e/upstream/') or
           not path.endswith('.dark') or '\\' in path or any(part in ('', '.', '..') for part in path.split('/')) for path in files):
        raise ValueError('Runner export paths must be canonical relative legacy fixture paths')
    if len(files) != len(set(files)) or files != sorted(files):
        raise ValueError('Runner export files must be unique and sorted')
    tests, errors = set(), set()
    for entry in value['tests']:
        required = {'source_file', 'source_line', 'name', 'error_expectation', 'exclusion_origin', 'effective_skip', 'eligible'}
        if not isinstance(entry, dict) or set(entry) != required or entry['source_file'] not in files:
            raise ValueError('Invalid or orphaned runner case')
        if type(entry['source_line']) is not int or entry['source_line'] < 1 or not isinstance(entry['name'], str) or not entry['name']:
            raise ValueError('Invalid runner case identity')
        identity = (entry['source_file'], entry['name'])
        if identity in tests:
            raise ValueError('Duplicate runner case')
        tests.add(identity)
        if entry['error_expectation'] not in (None, 'any-error', 'compile-error') or entry['exclusion_origin'] not in (None, 'fixture-metadata', 'whole-file-gate', 'line-gate'):
            raise ValueError('Invalid runner expectation or exclusion origin')
        reason = entry['effective_skip']
        if (reason is not None and not isinstance(reason, str)) or type(entry['eligible']) is not bool or entry['eligible'] != (reason is None) or (entry['exclusion_origin'] is None) != (reason is None):
            raise ValueError('Inconsistent runner eligibility and skip metadata')
    for entry in value['parse_errors']:
        if not isinstance(entry, dict) or set(entry) != {'source_file', 'message'} or entry['source_file'] not in files or not isinstance(entry['message'], str) or not entry['message']:
            raise ValueError('Invalid runner parse error')
        path = entry['source_file']
        if path in errors or any(source == path for source, _ in tests):
            raise ValueError('Duplicate parse error or cases in a failed fixture')
        errors.add(path)


def reconcile_runner(root, context_hash, revision, records, compiler_files, document=None):
    document = read_yaml(root / 'porting/discovery.yaml') if document is None else document
    if not isinstance(document, dict) or set(document) != {'schema', 'legacy_runner'} or document['schema'] != 1:
        raise ValueError('Invalid discovery ledger')
    captured = document['legacy_runner']
    groups = [item for item in records if item['category'] == 'tests' and item['scope'].startswith('compiler/test/fixtures/e2e/upstream/')]
    if captured is None:
        for item in groups:
            item['runner_discovery'] = {'status': 'unverified'}
        return 'unverified'
    if not isinstance(captured, dict) or set(captured) != {'compiler_context_sha256', 'upstream_revision', 'export'} or not re.fullmatch('[0-9a-f]{64}', str(captured['compiler_context_sha256'])) or not re.fullmatch('[0-9a-f]{40}', str(captured['upstream_revision'])):
        raise ValueError('Invalid discovery provenance')
    value = captured['export']
    validate_runner_export(value)
    current = captured['compiler_context_sha256'] == context_hash and captured['upstream_revision'] == revision
    state = ('partial' if value['parse_errors'] else 'current') if current else 'stale'
    paths = {item['compiler'][0] for item in groups}
    if current and set(value['files']) != paths:
        raise ValueError('Current runner export does not cover the complete legacy corpus')
    by_file = {path: [] for path in value['files']}
    for case in value['tests']:
        by_file[case['source_file']].append(case)
        if current and case['source_line'] > len(compiler_files[case['source_file']].decode().splitlines()):
            raise ValueError('Runner case source line is outside its current fixture')
    errors = {entry['source_file']: entry['message'] for entry in value['parse_errors']}
    for item in groups:
        path = item['compiler'][0]
        cases = by_file.get(path, [])
        parser_lines = {int(match[1]) for case in cases if (match := re.match(r'^L(\d+):', case['name']))}
        item['runner_discovery'] = dict(status=state, reported_cases=len(cases),
                                        reported_eligible=sum(case['eligible'] for case in cases),
                                        reported_skipped=sum(not case['eligible'] for case in cases),
                                        parse_error=errors.get(path),
                                        unmatched_line_gates=sorted(set(item['configured_gates']['lines']) - parser_lines)
                                          if current and path not in errors else None)
    # Retain stale case selectors while their file exists; do not turn an input
    # edit into orphaned decisions merely because recollection is unavailable.
    group_by_file = {item['compiler'][0]: item for item in groups}
    for case in value['tests']:
        if case['source_file'] in group_by_file:
            group = group_by_file[case['source_file']]
            key = digest(case['name'].encode())
            records.append(dict(id=group['id'] + '#runner/' + key, category='tests',
                                scope=group['scope'] + '/runner/' + key, upstream=None,
                                compiler=list(group['compiler']), upstream_sha256=None,
                                compiler_sha256=dict(group['compiler_sha256']),
                                observation=('actual' if current else 'stale') + ' legacy parser case; discovery is not execution',
                                detail=case['name'], source_group=group['id'],
                                runner_discovery=dict(status=state, **case)))
    records.sort(key=lambda item: item['id'])
    return state


def build(root, upstream, discovery_override=None):
    sources = read_yaml(root / 'porting/sources.yaml')
    expected = {'schema', 'upstream_repository', 'upstream_revision', 'target', 'package_corpus',
                'integration_destination', 'upstream_changes', 'ci_behavior_enforcement'}
    if set(sources) != expected or sources['schema'] != 1 or sources['target'] != 'linux-x64' or sources['package_corpus'] != 'checked-in-only':
        raise ValueError('Invalid source configuration')
    revision = sources['upstream_revision']
    if not re.fullmatch('[0-9a-f]{40}', revision) or git_revision(upstream) != revision:
        raise ValueError('Upstream checkout HEAD must match the exact pinned revision')
    upstream_files = checked_files(upstream, revision)
    manifests, defaults = load_manifests(root)
    records, compiler_files = discover(root, upstream_files)
    context_hash = compiler_context(root, compiler_files)
    discovery_state = reconcile_runner(root, context_hash, revision, records, compiler_files, discovery_override)
    check_selectors(manifests, records)
    evidence = read_yaml(root / 'porting/evidence.yaml')
    if set(evidence) != {'entries'} or not isinstance(evidence['entries'], list):
        raise ValueError('Invalid evidence document')
    evidence_by_id = {}
    ids = {r['id'] for r in records}
    for entry in evidence['entries']:
        required = {'id', 'fingerprint', 'target', 'status', 'command', 'notes'}
        if not isinstance(entry, dict) or set(entry) != required or entry['id'] not in ids or entry['id'] in evidence_by_id:
            raise ValueError('Invalid, duplicate, or orphaned evidence entry')
        if entry['status'] not in ('pass', 'fail', 'blocked') or entry['target'] != sources['target'] or not entry['command']:
            raise ValueError('Invalid evidence outcome, target, or command')
        evidence_by_id[entry['id']] = entry
    issues = read_yaml(root / 'porting/issues.yaml')
    if set(issues) != {'issues'} or not isinstance(issues['issues'], list):
        raise ValueError('Invalid issues document')
    seen_issues = set()
    for issue in issues['issues']:
        fields = {'id', 'items', 'observed', 'intended', 'decision', 'upstream_permission', 'upstream_pr', 'fix_revision'}
        if not isinstance(issue, dict) or set(issue) != fields or issue['id'] in seen_issues or not issue['items'] or set(issue['items']) - ids:
            raise ValueError('Invalid, duplicate, or orphaned upstream issue')
        if issue['upstream_permission'] not in ('not-requested', 'requested', 'granted'):
            raise ValueError('Invalid upstream permission state')
        seen_issues.add(issue['id'])
    for item in records:
        category = item['category']
        ancestors = []
        parent = manifests[category].get('extends')
        while parent:
            ancestors.insert(0, manifests[parent])
            parent = manifests[parent].get('extends')
        policy, trail, dependencies = policy_for(manifests[category], defaults[category], item, ancestors)
        if set(dependencies) - (ids | set(CATEGORIES)):
            raise ValueError(f'Unknown dependencies for {item["id"]}')
        item.update(policy=policy, policy_trail=trail, depends_on=dependencies)
        item['fingerprint'] = digest((context_hash + revision + yaml.safe_dump(item, sort_keys=True)).encode())
        entry = evidence_by_id.get(item['id'])
        item['behavior'] = 'unverified' if entry is None else ('stale' if entry['fingerprint'] != item['fingerprint'] else entry['status'])
    by_id = {r['id']: r for r in records}
    def blocked_by(item):
        return [dep for dep in item['depends_on'] if
                (by_id[dep]['behavior'] != 'pass' if dep in by_id else
                 any(r['behavior'] != 'pass' and r['policy']['disposition'] == 'required' for r in records if r['category'] == dep))]
    for item in records:
        item['blocked_by'] = blocked_by(item)
    outputs = {}
    overview = ['# Porting inventory', '', f'Upstream: `{sources["upstream_repository"]}@{revision}`; target: `{sources["target"]}`.', '',
                'Generated by `python3 scripts/porting.py refresh --upstream CHECKOUT`.', '',
                'These are initial source inventories, not a claim of complete semantic/API/test coverage.',
                'Discovery gaps and same-name candidates remain explicit. No behavior tests run during refresh.', '',
                '| Category | Items | Unverified | Stale |', '| --- | ---: | ---: | ---: |']
    for category in CATEGORIES:
        items = [r for r in records if r['category'] == category]
        overview.append(f'| [{category}]({category}.md) | {len(items)} | {sum(r["behavior"] == "unverified" for r in items)} | {sum(r["behavior"] == "stale" for r in items)} |')
        node = manifests[category]
        lines = [f'# {category.capitalize()} porting inventory', '', '## Current section', '', node['current'], '', '## Intended section', '']
        lines += [f'- {key}: {value}' for key, value in defaults[category].items()]
        lines += ['', '## Open questions', ''] + [f'- {q}' for q in node.get('questions', [])]
        if category == 'tests':
            declarations = [r for r in items if 'named_test' in r]
            groups = {r['source_group'] for r in declarations}
            lines += ['', '## Named-test discovery', '',
                      f'{len(declarations)} named source declarations in {len(groups)} checked-in files.',
                      'The category total also includes file groups, backend sources/fixtures, and legacy imports; it is not an executable test count.',
                      'IDs use file paths and qualified names, never line numbers. Links use current source lines.',
                      'Module scopes use `FILE/named/Qualified/Module`; actual named-test runner discovery, support, and execution remain unverified.',
                      f'Legacy runner discovery: **{discovery_state}**. This is separate from behavioral verification.',
                      'Collect real parsed cases and effective skips with `python3 scripts/collect-upstream-discovery.py --upstream CHECKOUT`; the collector builds first and never invents an export.']
            legacy = [r for r in items if 'runner_discovery' in r and 'source_group' not in r]
            lines += ['', '## Legacy runner discovery', '',
                      '| File group | Discovery | Reported cases | Eligible | Skipped | Unmatched line gates | Parse error |',
                      '| --- | --- | ---: | ---: | ---: | --- | --- |']
            for item in legacy:
                observed = item['runner_discovery']
                lines.append('| ' + ' | '.join(map(cell, [item['id'], observed['status'],
                    observed.get('reported_cases', '—'), observed.get('reported_eligible', '—'),
                    observed.get('reported_skipped', '—'),
                    ', '.join(map(str, observed['unmatched_line_gates'])) if observed.get('unmatched_line_gates') is not None else '—',
                    observed.get('parse_error') or '—'])) + ' |')
            lines += ['', 'Stale counts describe an old export, not current selection. Eligible cases are not passing tests.',
                      'Legacy case IDs hash actual parser names (which include line coordinates); unlike named-package IDs, they may change when lines move.']
        if category == 'builtins':
            declarations = [r for r in items if 'builtin' in r]
            computed = sum(r.get('builtin_declaration_counts', {}).get('computed', 0) for r in items)
            lines += ['', '## Registration discovery', '',
                      f'{len(declarations)} literal function/value registration candidates; {computed} computed-name/version constructors remain explicit discovery gaps.',
                      'These are not assembled registry counts. Constructors can be invoked repeatedly, renamed, filtered, or combined by host-specific registries.',
                      'File policies cover all their declarations; nested scopes use `FILE/declarations/fn/NAME` or `FILE/declarations/value/NAME`.',
                      'Same-spelling compiler declarations are only candidates, not proven bindings, compatible signatures, or working implementations.']
        lines += ['', '## Source observations', '', '| Identity | Upstream | Compiler candidates | Observation | Behavior | Source policy | Disposition |', '| --- | --- | --- | --- | --- | --- | --- |']
        for r in items:
            url = f'https://github.com/{sources["upstream_repository"]}/blob/{revision}/{r["upstream"]}' if r['upstream'] else None
            if url and 'source_line' in r:
                url += f'#L{r["source_line"]}'
            source = f'[{r["upstream"]}]({url})' if url else '—'
            cp = ', '.join(f'[{p}](../../../{p})' for p in r['compiler']) or '—'
            lines.append('| ' + ' | '.join(map(cell, [r['id'], source, cp, r['observation'], r['behavior'], r['policy']['source'], r['policy']['disposition']])) + ' |')
        outputs[f'docs/porting/generated/{category}.md'] = '\n'.join(lines) + '\n'
    overview += ['', '## Discovery boundaries', '',
                 '- Packages/StdLib: all checked-in `.dark` files; root-module mappings are candidates, not declaration mappings.',
                 '- Language: written constructor names plus broad curated behavioral workstreams; full feature taxonomy still needs trials.',
                 '- Parser: upstream components and same-stem OCaml candidates; split components remain classified separately.',
                 '- Tests: qualified named-test source identities, backend sources/fixtures, and legacy imports. The real-parser/shared-gate export and fingerprinted collector are implemented; actual collection is unverified here and current named-test execution remains pending.',
                 '- Builtins: registration sources plus literal function/value candidates; computed constructors remain counted gaps. Assembled registries, aliases, signatures, and compiler bindings are not yet compared.',
                 '- Only x64 Linux evidence is accepted. Inventory discovery does not establish execution support.', '']
    overview += ['## Recorded upstream bugs', '']
    overview += [f'- {issue["id"]}: {issue["decision"]}; upstream permission: {issue["upstream_permission"]}' for issue in issues['issues']] or ['None recorded.']
    outputs['docs/porting/generated/index.md'] = '\n'.join(overview)
    outputs['porting/generated/inventory.yaml'] = yaml.safe_dump(dict(schema=1, sources=sources, compiler_context_sha256=context_hash, items=records), sort_keys=False, allow_unicode=True)
    return outputs, records


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('operation', choices=('refresh', 'check', 'next', 'show'))
    parser.add_argument('--upstream', required=True, type=Path)
    parser.add_argument('--category', choices=CATEGORIES)
    parser.add_argument('--item')
    args = parser.parse_args()
    try:
        outputs, records = build(ROOT, args.upstream.resolve())
        if args.operation == 'refresh':
            for relative, content in outputs.items():
                path = ROOT / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(content)
            print(f'Refreshed {len(records)} source inventory items in six categories; no behavior tests executed.')
        elif args.operation == 'check':
            stale = [p for p, content in outputs.items() if not (ROOT / p).exists() or (ROOT / p).read_text() != content]
            extra = [p.relative_to(ROOT).as_posix() for directory in ('porting/generated', 'docs/porting/generated')
                     for p in (ROOT / directory).glob('*') if p.is_file() and p.relative_to(ROOT).as_posix() not in outputs]
            if stale or extra:
                raise ValueError('Generated outputs differ; refresh needed: ' + ', '.join(sorted(stale + extra)))
            print('Manifest consistency and generated inventories pass; behavior enforcement remains undecided.')
        else:
            selected = [r for r in records if (not args.category or r['category'] == args.category) and (not args.item or r['id'] == args.item)]
            if not selected:
                raise ValueError('No matching inventory items')
            if args.operation == 'show':
                print(yaml.safe_dump(selected, sort_keys=False), end='')
            else:
                for r in selected:
                    if r['policy']['disposition'] == 'required' and r['behavior'] != 'pass':
                        action = 'blocked by ' + ', '.join(r['blocked_by']) if r['blocked_by'] else 'consult manifest before porting'
                        print(f'{r["id"]}: {r["observation"]}; behavior={r["behavior"]}; {action}')
        return 0
    except (ValueError, OSError, subprocess.CalledProcessError, yaml.YAMLError) as error:
        print(f'porting: {error}', file=sys.stderr)
        return 2


if __name__ == '__main__':
    sys.exit(main())
