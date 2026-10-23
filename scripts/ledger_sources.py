"""Read strict YAML configuration and committed pinned source blobs."""
from __future__ import annotations

from pathlib import Path
import io
import subprocess

import yaml

ROOT = Path(__file__).resolve().parent.parent



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
