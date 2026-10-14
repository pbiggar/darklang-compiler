#!/usr/bin/env python3
"""Exercise unchanged upstream paths/nested modules through the actual embedder."""
from __future__ import annotations

import argparse
import hashlib
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import porting

ROOT = Path(__file__).resolve().parent.parent
SAMPLES = {
    'packages/darklang/stdlib/cli/env.dark': ['packages/darklang/stdlib/cli/env.dark'],
    'packages/darklang/stdlib/cli/file.dark': ['packages/darklang/stdlib/cli/file.dark'],
    'packages/darklang/stdlib/cli/process.dark': ['packages/darklang/stdlib/cli/process.dark'],
    'packages/darklang/stdlib/cli/sys.dark': ['packages/darklang/stdlib/cli/sys.dark'],
    'packages/darklang/stdlib/cli/execution.dark': ['packages/darklang/stdlib/cli/execution.dark'],
    'packages/darklang/stdlib/cli/fileSystem.dark': [
        'StdLib/Cli/FileSystem.dark', 'StdLib/Cli/FileSystem/FileError.dark'],
}


def decode_ocaml_string(text):
    """Read the generator's byte-string subset independently of its encoder."""
    output = bytearray()
    index = 0
    escapes = {'n': 10, 'r': 13, 't': 9, '"': 34, '\\': 92}
    while index < len(text):
        if text[index] != '\\':
            output.append(ord(text[index]))
            index += 1
        elif text[index + 1] in escapes:
            output.append(escapes[text[index + 1]])
            index += 2
        else:
            digits = text[index + 1:index + 4]
            if len(digits) != 3 or not digits.isdigit() or int(digits) > 255:
                raise ValueError('Unexpected emitted byte-string escape')
            output.append(int(digits))
            index += 4
    return bytes(output)


def embedded_pairs(text):
    pattern = r'^  \("((?:[^"\\]|\\.)*)", "((?:[^"\\]|\\.)*)"\);$'
    return [(decode_ocaml_string(name).decode(), decode_ocaml_string(body))
            for name, body in re.findall(pattern, text, re.M)]


def source_snapshot():
    return {p.relative_to(ROOT).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
            for directory in ('StdLib', 'packages') for p in (ROOT / directory).rglob('*.dark')}


class LayoutTrial(unittest.TestCase):
    upstream = None
    revision = None

    def upstream_bytes(self, path):
        return subprocess.check_output(['git', '-C', str(self.upstream), 'show', f'{self.revision}:{path}'])

    def run_trial(self, selections):
        before = source_snapshot()
        manifest_before = (ROOT / 'library-sources.list').read_bytes()
        names = [line.strip() for line in manifest_before.decode().splitlines()
                 if line.strip() and not line.startswith('#')]
        removed = {path for sample in selections for path in SAMPLES[sample]}
        self.assertTrue(removed <= set(names), 'A sample mapping changed; review the trial.')
        replacements = {SAMPLES[sample][0]: sample for sample in selections}
        expected_names = [replacements.get(name, name) for name in names
                          if name not in removed or name in replacements]
        with tempfile.TemporaryDirectory(prefix='dark-package-layout-') as temp:
            stage = Path(temp)
            for directory in ('StdLib', 'packages'):
                shutil.copytree(ROOT / directory, stage / directory)
            for name in removed:
                (stage / name).unlink()
            for sample in selections:
                target = stage / sample
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(self.upstream_bytes(sample))
            (stage / 'library-sources.list').write_text('\n'.join(expected_names) + '\n')
            output, build_info = stage / 'EmbeddedStdlib.ml', stage / 'BuildInfo.ml'
            result = subprocess.run([sys.executable, str(ROOT / 'scripts/embed-stdlib.py'),
                                     str(stage), str(output), str(build_info), 'dev'],
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            pairs = embedded_pairs(output.read_text())
            self.assertEqual([name for name, _ in pairs], expected_names,
                             'Embedding changed declaration order or dropped a source.')
            embedded = dict(pairs)
            for sample in selections:
                self.assertEqual(embedded[sample], self.upstream_bytes(sample),
                                 'The copied upstream source was rewritten or split.')
            self.assertFalse((removed - set(selections)) & set(embedded), 'A replacement duplicated its old source.')
        self.assertEqual(source_snapshot(), before, 'Trial mutated production libraries.')
        self.assertEqual((ROOT / 'library-sources.list').read_bytes(), manifest_before)

    def test_simple_file_at_upstream_path(self):
        self.run_trial(['packages/darklang/stdlib/cli/env.dark'])

    def test_simple_file_production_mirrors_upstream(self):
        sample = 'packages/darklang/stdlib/cli/env.dark'
        self.assertTrue((ROOT / sample).is_file(), 'Cli.Env is not at its upstream path.')
        self.assertEqual((ROOT / sample).read_bytes(), self.upstream_bytes(sample))
        self.assertFalse((ROOT / 'StdLib/Cli/Env.dark').exists(), 'Old Env source would duplicate the module.')

    def test_all_simple_production_copies_match_upstream(self):
        for name in ('env', 'file', 'process', 'sys', 'execution'):
            sample = f'packages/darklang/stdlib/cli/{name}.dark'
            with self.subTest(source=sample):
                self.assertTrue((ROOT / sample).is_file(), 'Source has not reached the mirrored path.')
                self.assertEqual((ROOT / sample).read_bytes(), self.upstream_bytes(sample))
        for old in ('StdLib/Cli/Env.dark', 'StdLib/Cli/File.dark', 'StdLib/Cli/Process.dark',
                    'StdLib/Cli/Sys.dark', 'StdLib/Cli.dark'):
            self.assertFalse((ROOT / old).exists(), 'An old source duplicates a mirrored module.')

    def test_intact_nested_module_at_upstream_path(self):
        sample = 'packages/darklang/stdlib/cli/fileSystem.dark'
        self.assertIn(b'\nmodule FileError =\n', self.upstream_bytes(sample))
        self.run_trial([sample])

    def test_both_replacements_in_one_library(self):
        self.run_trial(list(SAMPLES))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--upstream', required=True, type=Path)
    args = parser.parse_args()
    sources = porting.read_yaml(ROOT / 'porting/sources.yaml')
    upstream = args.upstream.resolve()
    if porting.git_revision(upstream) != sources['upstream_revision']:
        parser.error('Use the exact interpreter revision pinned in porting/sources.yaml.')
    LayoutTrial.upstream, LayoutTrial.revision = upstream, sources['upstream_revision']
    result = unittest.TextTestRunner(verbosity=2).run(unittest.defaultTestLoader.loadTestsFromTestCase(LayoutTrial))
    if result.wasSuccessful():
        print('Embedding accepts unchanged upstream paths and intact nested-module bytes.')
        print('Five shared CLI sources use mirrored production paths; the trial does not modify production files.')
        print('Compilation and runtime behavior were NOT tested.')
    return 0 if result.wasSuccessful() else 1


if __name__ == '__main__':
    sys.exit(main())
