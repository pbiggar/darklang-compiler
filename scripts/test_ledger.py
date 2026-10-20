#!/usr/bin/env python3
"""Check compact ledgers preserve port gaps and exceptional test decisions."""
import importlib.util
from pathlib import Path
import tempfile
import unittest

import yaml

spec = importlib.util.spec_from_file_location('ledger', Path(__file__).with_name('update-ledger.py'))
ledger = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ledger)


class CompactLedger(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        (self.root / 'porting').mkdir()
        for name in ('language-features', 'parser'):
            (self.root / ('porting/' + name + '.yaml')).write_text('schema: 2\ngroups: {}\n')
        self.value = dict(context=dict(upstream_revision='pinned'), declarations=[], builtins=[], tests=dict(cases=[]))

    def test_declaration_name_collisions_do_not_drop_entries(self):
        self.value['declarations'] = [dict(name='Acme.same', kind=kind,
            upstream=dict(source='packages/acme/main.dark', line=line),
            source_status=status, source_exception=None)
            for kind, line, status in [('type', 1, 'identical'), ('value', 4, 'different'), ('value', 8, 'missing')]]
        rows = ledger.compact(self.value, self.root)['porting/generated/packages.yaml']['files']['acme/main.dark']
        self.assertEqual(list(rows.values()), ['identical', 'different', 'missing'])
        self.assertEqual(len(rows), 3)

    def test_wrapper_counterpart_does_not_claim_builtin_binding(self):
        self.value['builtins'] = [dict(id='builtins:backend/src/Builtins/Example.fs#fn/f/v0',
            name='f', version=0, kind='fn', binding='not-mapped', source_counterparts=['Acme.f'])]
        rows = ledger.compact(self.value, self.root)['porting/generated/builtins.yaml']['files']
        self.assertEqual(rows['Example.fs']['f_v0'], 'unassessed')

    def test_partial_test_keeps_disabled_counterpart_and_expectation(self):
        self.value['tests']['cases'] = [dict(upstream=dict(source='packages/acme/tests.dark', name='Acme.group'),
            status='partial', equivalency='partial', reason=None,
            compiler=[dict(id='test/fixtures/e2e/upstream/acme.dark#L3', enabled=False,
                           reason='unsupported async', expectation_override='#compileerror=unsupported')])]
        row = ledger.compact(self.value, self.root)['porting/generated/tests.yaml']['sources']['packages/acme/tests.dark']['Acme.group']
        self.assertEqual(row['status'], 'partial')
        self.assertEqual(row['reason'], 'unsupported async')
        self.assertEqual(row['expectation'], '#compileerror=unsupported')
        self.assertEqual(row['compiler'], 'upstream/acme.dark#L3')

    def test_duplicate_test_identity_is_rejected_instead_of_overwritten(self):
        case = dict(upstream=dict(source='packages/acme/tests.dark', name='Acme.same'),
                    status='missing', equivalency='missing', compiler=[], reason=None)
        self.value['tests']['cases'] = [case, case]
        with self.assertRaisesRegex(ValueError, 'Repeated ledger identity'):
            ledger.compact(self.value, self.root)

    def test_capability_gap_requires_explanation(self):
        path = self.root / 'porting/parser.yaml'
        path.write_text(yaml.safe_dump(dict(schema=2, groups=dict(syntax=dict(interpolation='partial')))))
        with self.assertRaisesRegex(ValueError, 'Explain capability gap'):
            ledger.compact(self.value, self.root)


if __name__ == '__main__':
    unittest.main()
