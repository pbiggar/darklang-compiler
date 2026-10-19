#!/usr/bin/env python3
"""Build and capture real legacy parser/gate discovery with deterministic provenance."""
import argparse
import os
from pathlib import Path
import subprocess
import tempfile

import yaml
import porting as p


def gather(root, upstream, run=subprocess.run):
    p.build(root, upstream)
    source_config = p.read_yaml(root / 'porting/sources.yaml')
    # build() already validates the pin and ledger. Compute the same context
    # directly so the empty-inventory case has provenance too.
    _, compiler_files = p.discover(root, p.checked_files(upstream, source_config['upstream_revision']))
    fingerprint = p.compiler_context(root, compiler_files)
    environment = dict(os.environ)
    environment.pop('LD_PRELOAD', None)
    built = run(['./build', '--ai'], cwd=root, env=environment,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    if built.returncode:
        raise ValueError('Native build failed; prior discovery is unchanged.\n' + built.stdout[-4000:])
    with tempfile.TemporaryDirectory(prefix='dark-porting-discovery-') as directory:
        raw = Path(directory) / 'runner.yaml'
        exported = run(['./run-tests', '--target=linux-x86_64', '--inventory-upstream=' + str(raw)],
                       cwd=root, env=environment, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        if exported.returncode not in (0, 1) or not raw.is_file():
            raise ValueError('Runner did not produce a discovery export; prior discovery is unchanged.\n' + exported.stdout[-4000:])
        value = p.read_yaml(raw)
        p.validate_runner_export(value)
        if (exported.returncode == 0) != (not value['parse_errors']):
            raise ValueError('Runner exit status disagrees with recorded parse errors')
    _, after = p.build(root, upstream)
    _, after_files = p.discover(root, p.checked_files(upstream, source_config['upstream_revision']))
    if fingerprint != p.compiler_context(root, after_files):
        raise ValueError('Source/build inputs changed during collection; prior discovery is unchanged')
    # Validate completeness and bounds before returning any new ledger content.
    groups = {r['compiler'][0] for r in after if r['category'] == 'tests' and
              r['scope'].startswith('compiler/test/fixtures/e2e/upstream/') and 'source_group' not in r}
    if set(value['files']) != groups:
        raise ValueError('Runner did not export the complete legacy corpus')
    if any(case['source_line'] > len(after_files[case['source_file']].decode().splitlines()) for case in value['tests']):
        raise ValueError('Runner case source line is outside its fixture')
    document = dict(schema=1, legacy_runner=dict(compiler_context_sha256=fingerprint,
                                               upstream_revision=source_config['upstream_revision'], export=value))
    # Reconcile selectors, evidence and dependencies against the prospective
    # capture before replacing shared knowledge on disk.
    p.build(root, upstream, discovery_override=document)
    return document


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--upstream', required=True, type=Path)
    args = parser.parse_args()
    try:
        document = gather(p.ROOT, args.upstream.resolve())
        path = p.ROOT / 'porting/discovery.yaml'
        path.write_text(yaml.safe_dump(document, sort_keys=False, allow_unicode=True))
        outputs, _ = p.build(p.ROOT, args.upstream.resolve())
        for name, content in outputs.items():
            destination = p.ROOT / name
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_text(content)
        if document['legacy_runner']['export']['parse_errors']:
            parser.exit(1, 'Recorded partial discovery and parse errors; no behavior tests executed.\n')
        print('Recorded current runner discovery and refreshed inventories; no behavior tests executed.')
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        parser.exit(2, f'porting discovery: {error}\n')


if __name__ == '__main__':
    main()
