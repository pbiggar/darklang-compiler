#!/usr/bin/env python3
"""Regressions for the source-owned ledger and byte-preserving pin verification."""
import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import yaml

spec = importlib.util.spec_from_file_location('ledger', Path(__file__).with_name('update-ledger.py'))
ledger = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ledger)


class SourceLedger(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        (self.root / 'porting').mkdir()
        for name in ('language-features', 'parser'):
            (self.root / ('porting/' + name + '.yaml')).write_text('schema: 2\ngroups: {}\n')
        (self.root / 'porting/sources.yaml').write_text('upstream_revision: pin\n')
        (self.root / 'porting/builtins.yaml').write_text('files: {}\n')

    def row(self, *items):
        return [{'source': 'upstream/packages/example.dark', 'items': list(items)}]

    def item(self, kind='function', line=1, status='run', **extra):
        return dict(name='Example.same', kind=kind, line=line, column=0, compiler=dict(status=status, **extra))

    def test_every_native_entry_counts_without_counterpart_matching(self):
        summary = ledger.summarize(self.root, self.row(self.item(), self.item('type'), self.item(line=2),
            self.item('named-test', status='blocked', reason='missing-dependency')))
        self.assertEqual(summary['dispositions'], {'packages': {'run': 3}, 'tests': {'blocked': 1}})
        self.assertEqual(summary['kinds']['named-test'], 1)

    def test_failure_classification_and_phase_are_independent(self):
        summary = ledger.summarize(self.root, self.row(
            self.item('named-test', line=1, status='xfail', classification='aot-rejection', phase='compile', reason='aot-type-error'),
            self.item('named-test', line=2, status='xfail', classification='missing-support', phase='compile', reason='upstream-dependency'),
            self.item('named-test', line=3, status='xfail', classification='missing-support', phase='runtime', reason='compiler-gap'),
            self.item('named-test', line=4, status='xfail', classification='upstream-bug', phase='runtime', reason='upstream-bug')))
        failures = summary['expected_failures']
        self.assertEqual(failures['aot-rejection']['phases'], {'compile': 1})
        self.assertEqual(failures['missing-support']['phases'], {'compile': 1, 'runtime': 1})
        self.assertEqual(failures['upstream-bug']['phases'], {'runtime': 1})
        self.assertEqual(failures['missing-support']['sources'], {'upstream/packages/example.dark': 2})
        report = ledger.markdown(summary)
        self.assertIn('| `aot-rejection` | 1 | 0 | 1 |', report)
        self.assertIn('| `missing-support` | 1 | 1 | 2 |', report)
        self.assertIn('| `upstream-bug` | 0 | 1 | 1 |', report)
        self.assertEqual(sum(sum(row['phases'].values()) for row in failures.values()),
                         summary['dispositions']['tests']['xfail'])

    def test_diagnostic_differences_are_runnable_negative_tests(self):
        summary = ledger.summarize(self.root, self.row(self.item(
            'named-test', status='run', reason='diagnostic-difference',
            error_phase='compile', error_message='compiler diagnostic')))
        self.assertEqual(summary['dispositions']['tests'], {'run': 1})
        self.assertEqual(summary['reasons']['tests'], {'diagnostic-difference': 1})
        self.assertEqual(summary['expected_failures'], {})

    def test_missing_unknown_and_runtime_aot_classes_are_rejected(self):
        for extra in [dict(phase='compile'), dict(phase='compile', classification='unknown'),
                      dict(phase='runtime', classification='aot-rejection')]:
            with self.subTest(extra=extra), self.assertRaises(ValueError):
                ledger.summarize(self.root, self.row(self.item('named-test', status='xfail', reason='compiler-gap', **extra)))

    def test_duplicate_native_entries_are_rejected(self):
        with self.assertRaisesRegex(ValueError, 'Duplicate native inventory'):
            ledger.summarize(self.root, self.row(self.item(), self.item()))

    def test_pending_test_cannot_enter_the_final_ledger(self):
        with self.assertRaisesRegex(ValueError, 'Unclassified imported test'):
            ledger.summarize(self.root, self.row(self.item('named-test', status='blocked', reason='pending-validation')))

    def test_exclusion_is_a_separate_assessment_requiring_notes(self):
        path = self.root / 'porting/parser.yaml'
        path.write_text(yaml.safe_dump(dict(schema=2, groups=dict(runtime=dict(reflection='excluded')))))
        with self.assertRaisesRegex(ValueError, 'Explain capability gap'):
            ledger.summarize(self.root, [])
        path.write_text(yaml.safe_dump(dict(schema=2, groups=dict(runtime=dict(
            reflection=dict(status='excluded', notes='Intentionally excluded.'))))))
        self.assertEqual(ledger.summarize(self.root, [])['parser'], {'excluded': 1})

    def test_only_lexer_identified_annotation_lines_are_removed(self):
        original = b'module Example\nval text = """\n// @compiler blocked: literal\n"""\n'
        path = self.root / 'upstream/packages/example.dark'
        path.parent.mkdir(parents=True)
        path.write_bytes(b'// @compiler blocked: missing-implementation\n' + original)
        rows = [{'source': 'upstream/packages/example.dark', 'annotation_lines': [1]}]
        with patch.object(ledger.sources, 'checked_files', return_value={'packages/example.dark': original}):
            ledger.verify_pin(self.root, self.root, rows)
            path.write_bytes(path.read_bytes().replace(b'module Example', b'module Altered'))
            with self.assertRaisesRegex(ValueError, 'differs from the pin'):
                ledger.verify_pin(self.root, self.root, rows)

    def test_manifest_cannot_hide_or_duplicate_a_file(self):
        directory = self.root / 'upstream/packages'
        directory.mkdir(parents=True)
        (directory / 'example.dark').write_text('val example = 1')
        manifest = self.root / 'upstream/sources.list'
        manifest.write_text('')
        with self.assertRaisesRegex(ValueError, 'complete source tree'):
            ledger.imported_paths(self.root)
        manifest.write_text('packages/example.dark\npackages/example.dark\n')
        with self.assertRaisesRegex(ValueError, 'unique and sorted'):
            ledger.imported_paths(self.root)


if __name__ == '__main__':
    unittest.main()
