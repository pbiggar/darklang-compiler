"""Read only the source inputs needed by the compact compatibility ledgers."""
from __future__ import annotations

import hashlib
import io
from pathlib import Path
import re
import subprocess

import yaml

ROOT = Path(__file__).resolve().parent.parent
CHAR_LITERAL = re.compile(r"'(?:\\.|[^'\\\n])'|'''")


class UniqueLoader(yaml.SafeLoader):
    """Reject duplicate keys instead of silently discarding ledger entries."""


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
        elif text[i] == "'" and (char := CHAR_LITERAL.match(text, i)):
            # A quote character must not start a string spanning later declarations.
            end = i + len(char[0])
            if strings:
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


def git_revision(path):
    return subprocess.check_output(['git', '-C', str(path), 'rev-parse', 'HEAD'], text=True).strip()


def checked_files(upstream, revision):
    paths = subprocess.check_output(['git', '-C', str(upstream), 'ls-tree', '-r', '--name-only', revision], text=True)
    selected = [path for path in paths.splitlines()
                if path.endswith(('.dark', '.fs', '.md', '.fsproj'))
                and path.startswith(('packages/', 'backend/src/', 'backend/tests/', 'backend/testfiles/'))]
    requests = ''.join(f'{revision}:{path}\n' for path in selected).encode()
    contents = io.BytesIO(subprocess.check_output(
        ['git', '-C', str(upstream), 'cat-file', '--batch'], input=requests))
    result = {}
    for path in selected:
        header = contents.readline().split()
        if len(header) != 3 or header[1] != b'blob':
            raise ValueError('Could not read pinned source blob: ' + path)
        size = int(header[2])
        body = contents.read(size)
        if len(body) != size or contents.read(1) != b'\n':
            raise ValueError('Incomplete pinned source blob: ' + path)
        result[path] = body
    return result


def runner_gates(compiler):
    """Read existing suite exclusions without maintaining a second gate table."""
    runner = compiler.get('test/test-suite-tooling/E2EDiscovery.ml', b'').decode()

    def block(name):
        if not runner:
            return ''
        if f'let {name} =' not in runner:
            raise ValueError(f'Runner gate structure changed: {name}; update discovery')
        return runner.split(f'let {name} =', 1)[1].split('\nlet ', 1)[0]

    disabled_paths = re.findall(r'"(test/fixtures/e2e/upstream/[^"\n]+)"', block('disabledUpstreamFiles'))
    raw_lines = re.findall(r'\(\s*"([^"]+)"\s*,\s*\[([^]]*)\]\s*\)', block('disabledUpstreamLines'))
    lines = {path: [int(n) for n in re.findall(r'\d+', numbers)] for path, numbers in raw_lines}
    paths = sorted(p for p in compiler if p.startswith('test/fixtures/e2e/upstream/') and p.endswith('.dark'))
    if len(set(disabled_paths)) != len(disabled_paths) or len(lines) != len(raw_lines):
        raise ValueError('Duplicate configured runner gate paths')
    if (set(disabled_paths) | set(lines)) - set(paths):
        raise ValueError('Configured runner gate references an absent fixture')
    for path, numbers in lines.items():
        if len(numbers) != len(set(numbers)) or any(n < 1 or n > len(compiler[path].decode().splitlines()) for n in numbers):
            raise ValueError('Duplicate or out-of-range configured runner line gate')
    return [dict(compiler=[path], configured_gates=dict(whole_file=path in disabled_paths, lines=lines.get(path, [])))
            for path in paths]


def discover(root, upstream):
    """Locate package counterparts, builtin registrations and suite exclusions."""
    compiler = {path.relative_to(root).as_posix(): path.read_bytes()
                for directory in ('src', 'packages', 'test')
                for path in (root / directory).rglob('*')
                if path.is_file() and path.suffix in ('.ml', '.mli', '.dark', '.e2e', '.syntax')}
    modules = {}
    for path, body in compiler.items():
        if path.endswith('.dark') and path.startswith('packages/'):
            for name in module_names(body.decode()):
                modules.setdefault(name, []).append(path)
    records = []
    for path, body in sorted(upstream.items()):
        if path.startswith('packages/') and path.endswith('.dark'):
            candidates = sorted({p for name in module_names(body.decode()) for p in modules.get(name, [])})
            records.append(dict(id='packages:' + path, category='packages', upstream=path, compiler=candidates))
        elif path.startswith('backend/src/Builtins/') and path.endswith('.fs'):
            for row in builtin_declarations(body.decode())[0]:
                records.append(dict(id=f"builtins:{path}#{row['kind']}/{row['name']}/v{row['version']}",
                    category='builtins', upstream=path, compiler=[], source_line=row['line'],
                    builtin={key: row[key] for key in ('kind', 'name', 'version')}))
    records.extend(runner_gates(compiler))
    return records, compiler


def compiler_context(root, compiler_files):
    """Fingerprint compilation inputs only when collecting a package run receipt."""
    inputs = dict(compiler_files)
    for directory in ('bin', 'scripts', 'src', 'test', 'tools', 'packages'):
        for path in (root / directory).rglob('*'):
            if path.is_file() and (path.suffix in ('.ml', '.mli', '.c', '.h', '.S', '.s', '.sh', '.py') or path.name == 'dune'):
                inputs[path.relative_to(root).as_posix()] = path.read_bytes()
    for name in ('build', 'run-tests', 'dune-project', 'dune', 'dune-workspace',
                 'library-sources.list', 'dependencies.lock', 'scripts/porting-requirements.txt', 'porting/sources.yaml'):
        path = root / name
        if path.is_file():
            inputs[name] = path.read_bytes()
    context = hashlib.sha256()
    for path, body in sorted(inputs.items()):
        context.update(path.encode() + b'\0' + digest(body).encode() + b'\n')
    return context.hexdigest()
