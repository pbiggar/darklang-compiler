#!/usr/bin/env python3
"""Check source discovery preserves literal registrations and suite exclusions."""
from pathlib import Path
import tempfile
import unittest

import yaml

import ledger_sources as sources


class SourceDiscovery(unittest.TestCase):
    def test_duplicate_yaml_keys_fail(self):
        with self.assertRaisesRegex(ValueError, 'Duplicate YAML key'):
            yaml.load('status: missing\nstatus: supported\n', Loader=sources.UniqueLoader)

    def test_comments_and_strings_do_not_create_modules(self):
        text = '(* outer (* module Fake *) *)\n// module Wrong\nval x = "module String"\nmodule Real\n'
        self.assertEqual(sources.module_names(text), ['Real'])
        self.assertEqual(text.count('\n'), sources.strip_comments(text, True).count('\n'))

    def test_builtin_literals_and_computed_names_stay_distinct(self):
        text = ('let docs = """name = fn "fake" 0"""\n'
                '// name = fn "comment" 0\n{ name = fn "boolNot" 0 }\n'
                '{ name = value "answer" 2 }\n{ name = fn computedName 0 }\n')
        literals, computed = sources.builtin_declarations(text)
        self.assertEqual(literals, [dict(kind='fn', name='boolNot', version=0, line=3),
                                    dict(kind='value', name='answer', version=2, line=4)])
        self.assertEqual(computed, [dict(kind='fn', line=5)])

    def test_nonliteral_builtin_versions_require_review(self):
        literals, computed = sources.builtin_declarations('{ name = fn "versioned" nextVersion }')
        self.assertEqual(literals, [])
        self.assertEqual(computed, [dict(kind='fn', line=1)])

    def compiler(self, files='[]', lines='[]'):
        fixture = 'test/fixtures/e2e/upstream/example.dark'
        runner = f'let disabledUpstreamFiles = {files}\nlet disabledUpstreamLines = {lines}\n'
        return {fixture: b'true = true\nfalse = false\n',
                'test/test-suite-tooling/E2EDiscovery.ml': runner.encode()}

    def test_existing_whole_file_and_line_exclusions_are_read(self):
        path = 'test/fixtures/e2e/upstream/example.dark'
        row = sources.runner_gates(self.compiler(f'[ "{path}" ]', f'[ ("{path}", [ 2 ]) ]'))[0]
        self.assertEqual(row['configured_gates'], dict(whole_file=True, lines=[2]))

    def test_invalid_exclusions_fail_instead_of_being_ignored(self):
        path = 'test/fixtures/e2e/upstream/example.dark'
        for files, lines in [(f'[ "{path}"; "{path}" ]', '[]'),
                             ('[]', f'[ ("{path}", [ 3 ]) ]'),
                             ('[ "test/fixtures/e2e/upstream/absent.dark" ]', '[]')]:
            with self.subTest(files=files, lines=lines), self.assertRaises(ValueError):
                sources.runner_gates(self.compiler(files, lines))

    def test_package_discovery_uses_modules_without_repeated_inventory_metadata(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'packages').mkdir()
            (root / 'packages/renamed.dark').write_text('module Acme.Example\nlet value = 1\n')
            records, _ = sources.discover(root, {'packages/acme/example.dark': b'module Acme.Example\nlet value = 1\n'})
            self.assertEqual(records, [dict(id='packages:packages/acme/example.dark', category='packages',
                                          upstream='packages/acme/example.dark', compiler=['packages/renamed.dark'])])


if __name__ == '__main__':
    unittest.main()
