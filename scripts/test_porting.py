#!/usr/bin/env python3
"""Regression checks for policy inheritance, discovery, and evidence invalidation."""
import importlib.util
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import yaml

spec = importlib.util.spec_from_file_location('porting', Path(__file__).with_name('porting.py'))
p = importlib.util.module_from_spec(spec)
spec.loader.exec_module(p)


class Policies(unittest.TestCase):
    def test_nested_exception_and_restoration(self):
        node = {'scope': '', 'policy': {'source': 'identical'}, 'children': [
            {'scope': 'packages/a', 'policy': {'source': 'independent-implementation'}, 'reason': 'native layout',
             'children': [{'scope': 'packages/a/shared.dark', 'policy': {'source': 'identical'}, 'reason': 'shared algorithm'}]}]}
        p.validate_node(node)
        self.assertEqual(p.policy_for(node, {}, {'id': 'x', 'scope': 'packages/a/shared.dark'})[0]['source'], 'identical')
        self.assertEqual(p.policy_for(node, {}, {'id': 'x', 'scope': 'packages/a/other.dark'})[0]['source'], 'independent-implementation')
        self.assertEqual(p.policy_for(node, {}, {'id': 'x', 'scope': 'packages/ab/x.dark'})[0]['source'], 'identical')

    def test_scope_boundaries(self):
        self.assertFalse(p.within('packages/abc/x.dark', 'packages/ab'))
        self.assertTrue(p.within('packages/ab/x.dark', 'packages/ab'))

    def test_overlaps_must_be_nested(self):
        with self.assertRaises(ValueError):
            p.validate_node({'scope': '', 'children': [{'scope': 'a'}, {'scope': 'a/b'}]})

    def test_child_cannot_escape_parent(self):
        with self.assertRaises(ValueError):
            p.validate_node({'scope': 'a', 'children': [{'scope': 'b'}]})

    def test_override_needs_reason(self):
        with self.assertRaises(ValueError):
            p.validate_node({'scope': '', 'children': [{'scope': 'a', 'policy': {'source': 'undecided'}}]})

    def test_unknown_fields_and_values(self):
        for node in ({'scope': '', 'polciy': {}}, {'scope': '', 'policy': {'source': 'close-enough'}}):
            with self.assertRaises(ValueError):
                p.validate_node(node)

    def test_item_override(self):
        node = {'scope': '', 'children': [{'item': 'language:expr/EIf', 'policy': {'disposition': 'deferred'}, 'reason': 'user decision'}]}
        p.validate_node(node)
        self.assertEqual(p.policy_for(node, {}, {'id': 'language:expr/EIf', 'scope': 'syntax'})[0]['disposition'], 'deferred')

    def test_cross_category_inheritance_keeps_nested_policy(self):
        parent = {'scope': '', 'children': [{'scope': 'packages/a', 'policy': {'source': 'undecided'}, 'reason': 'trial'}]}
        child = {'scope': 'packages/a'}
        self.assertEqual(p.policy_for(child, {'source': 'identical'}, {'id': 'x', 'scope': 'packages/a/x'}, [parent])[0]['source'], 'undecided')

    def test_orphan_selector(self):
        with self.assertRaises(ValueError):
            p.check_selectors({'packages': {'scope': '', 'children': [{'scope': 'absent'}]}}, [])

    def test_item_cannot_escape_scoped_parent(self):
        node = {'scope': '', 'children': [{'scope': 'a', 'children': [{'item': 'x'}]}]}
        with self.assertRaises(ValueError):
            p.check_selectors({'packages': node}, [{'category': 'packages', 'id': 'x', 'scope': 'b'}, {'category': 'packages', 'id': 'y', 'scope': 'a/z'}])

    def test_duplicate_yaml_keys(self):
        with self.assertRaises(ValueError):
            yaml.load('policy: one\npolicy: two\n', Loader=p.UniqueLoader)


class Discovery(unittest.TestCase):
    def test_comments_and_strings_do_not_create_modules(self):
        text = '(* outer (* module Fake *) *)\n// module Wrong\nval x = "module String"\nmodule Real\n'
        self.assertEqual(p.module_names(text), ['Real'])
        self.assertEqual(text.count('\n'), p.strip_comments(text, True).count('\n'))

    def test_triple_string_is_masked(self):
        self.assertEqual(p.module_names('val x = """\nmodule Fake\n"""\nmodule Real'), ['Real'])

    def test_variant_discovery_excludes_match_arms(self):
        text = 'type Expr =\n | EOne\n | ETwo of string\nlet f x =\n | NotACase -> x\ntype Other =\n | OtherCase\n'
        self.assertEqual(set(p.variants(text, {'expr'})), {('expr', 'EOne'), ('expr', 'ETwo')})

    def test_markdown_escaping(self):
        self.assertEqual(p.cell('a|b\nc'), 'a\\|b c')

    def test_named_tests_follow_nested_module_boundaries(self):
        text = ('module Acme.Tests\nmodule Left =\n  module Inner =\n'
                '    test same = true\n  test same = false\n'
                'module Right =\n  test same = true\ntest root = true\n')
        self.assertEqual(p.named_tests(text), [
            {'name': 'Acme.Tests.Left.Inner.same', 'line': 4},
            {'name': 'Acme.Tests.Left.same', 'line': 5},
            {'name': 'Acme.Tests.Right.same', 'line': 7},
            {'name': 'Acme.Tests.root', 'line': 8}])

    def test_named_tests_ignore_literals_and_comments(self):
        text = ('module Acme.Tests\n(* module Fake =\n test fake = true *)\n'
                'val x = """\nmodule Wrong =\n test wrong = true\n"""\n'
                '// test comment = true\ntest real = true\n')
        self.assertEqual(p.named_tests(text), [{'name': 'Acme.Tests.real', 'line': 9}])

    def test_duplicate_named_tests_fail_instead_of_collapsing(self):
        with self.assertRaisesRegex(ValueError, 'Duplicate named test'):
            p.named_tests('module Acme.Tests\ntest same = true\ntest same = false\n')

    def test_unrecognized_test_header_fails_instead_of_omitting(self):
        with self.assertRaisesRegex(ValueError, 'Unsupported named test'):
            p.named_tests('module Acme.Tests\ntest unsupported name = true\n')

    def test_unrecognized_module_header_fails_instead_of_guessing_name(self):
        with self.assertRaisesRegex(ValueError, 'Unsupported module'):
            p.named_tests('module Acme.Tests\nmodule unsupported name =\n  test real = true\n')

    def test_record_fields_and_local_expressions_are_not_test_declarations(self):
        text = ('module Acme.Tests\ntype Finished =\n  { name: String\n    test: Bool }\n'
                'module Nested =\n    type Record =\n      { test: Bool }\n'
                '    test real = true\nlet run test =\n  test true\n')
        self.assertEqual(p.named_tests(text), [{'name': 'Acme.Tests.Nested.real', 'line': 8}])


class Build(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        for path in ('porting/sources.yaml', 'porting/evidence.yaml', 'porting/issues.yaml', 'library-sources.list',
                     'dependencies.lock', 'scripts/porting.py', 'scripts/porting-requirements.txt'):
            destination = self.root / path
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_bytes((p.ROOT / path).read_bytes())
        for category in p.CATEGORIES:
            path = self.root / 'porting/manifests' / f'{category}.yaml'
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes((p.ROOT / 'porting/manifests' / f'{category}.yaml').read_bytes())
        path = self.root / 'src/frontend/interpreter/WrittenTypes.ml'
        path.parent.mkdir(parents=True)
        path.write_text('type expr = EUnit\nand declaration = DExpr of expr\n')
        self.upstream = {
            'backend/src/LibParser/WrittenTypes.fs': b'type Expr = EUnit\nand Declaration = DExpr of Expr\n',
            'packages/darklang/stdlib/list.dark': b'module Darklang.Stdlib.List\n',
            'packages/darklang/stdlib/dict.dark': b'module Darklang.Stdlib.Dict\n',
            'packages/acme/tests.dark': b'module Acme.Tests\n// test fake = x\ntest real = true\n',
            'backend/src/Builtins/Builtins.Pure/Libs/Bool.fs': b'let fns = [ { name = fn "boolNot" 0 } ]\n',
        }
        (self.root / 'src/DarkStdlib.ml').write_text('let allModules = []\n')
        self.revision = p.read_yaml(self.root / 'porting/sources.yaml')['upstream_revision']

    def tearDown(self):
        self.temp.cleanup()

    def build(self):
        with patch.object(p, 'git_revision', return_value=self.revision), patch.object(p, 'checked_files', return_value=self.upstream):
            return p.build(self.root, self.root)

    def test_deterministic_and_six_outputs(self):
        first, records = self.build()
        second, _ = self.build()
        self.assertEqual(first, second)
        self.assertTrue(all(f'docs/porting/generated/{c}.md' in first for c in p.CATEGORIES))
        self.assertTrue(all(r['behavior'] == 'unverified' for r in records))

    def test_relocation_does_not_change_outputs(self):
        first, _ = self.build()
        with tempfile.TemporaryDirectory() as relocated:
            target = Path(relocated) / 'different-worktree-name'
            shutil.copytree(self.root, target)
            with patch.object(p, 'git_revision', return_value=self.revision), patch.object(p, 'checked_files', return_value=self.upstream):
                second, _ = p.build(target, target)
            self.assertEqual(first, second)

    def test_wrong_upstream_head_is_rejected(self):
        with patch.object(p, 'git_revision', return_value='0' * 40):
            with self.assertRaises(ValueError):
                p.build(self.root, self.root)

    def test_test_source_count_ignores_comments(self):
        _, records = self.build()
        item = next(r for r in records if r['id'] == 'tests:packages/acme/tests.dark')
        self.assertIn('1 declaration candidates: real', item['detail'])

    def test_legacy_configuration_comes_from_shared_discovery_module(self):
        path = 'test/fixtures/e2e/upstream/example.dark'
        fixture = self.root / path
        fixture.parent.mkdir(parents=True)
        fixture.write_text('1L = 1L\n')
        module = self.root / 'test/test-suite-tooling/E2EDiscovery.ml'
        module.parent.mkdir(parents=True)
        module.write_text('let disabledUpstreamFiles = [ "' + path + '" ]\n'
                          'let disabledUpstreamLines = [ ("' + path + '", [ 1; 2 ]) ]\n'
                          'let loadE2ETests files = files\n')
        _, records = self.build()
        item = next(r for r in records if r['id'] == 'tests:compiler/' + path)
        self.assertIn('Configured whole-file gate: True; line entries: 1, 2', item['detail'])
        self.assertEqual(item['behavior'], 'unverified')

    def test_named_test_id_and_nested_policy_survive_line_insertion(self):
        path = self.root / 'porting/manifests/tests.yaml'
        manifest = p.read_yaml(path)
        manifest['children'] = [dict(scope='packages/acme/tests.dark/named/Acme/Tests',
                                    policy={'disposition': 'deferred'}, reason='Fixture module deferral')]
        path.write_text(yaml.safe_dump(manifest))
        outputs, records = self.build()
        identity = 'tests:packages/acme/tests.dark#Acme.Tests.real'
        item = next(r for r in records if r['id'] == identity)
        self.assertEqual(item['policy']['disposition'], 'deferred')
        self.assertEqual(item['source_line'], 3)
        self.assertIn('#L3', outputs['docs/porting/generated/tests.md'])
        self.upstream['packages/acme/tests.dark'] = b'// inserted\n' + self.upstream['packages/acme/tests.dark']
        _, records = self.build()
        moved = next(r for r in records if r['id'] == identity)
        self.assertEqual(moved['source_line'], 4)
        self.assertNotEqual(item['fingerprint'], moved['fingerprint'])

    def test_current_evidence_and_invalidation(self):
        _, records = self.build()
        item = records[0]
        entry = dict(id=item['id'], fingerprint=item['fingerprint'], target='linux-x64', status='pass', command='actual-check', notes='fixture')
        (self.root / 'porting/evidence.yaml').write_text(yaml.safe_dump({'entries': [entry]}))
        _, records = self.build()
        self.assertEqual(records[0]['behavior'], 'pass')
        (self.root / 'src/DarkStdlib.ml').write_text('let allModules = [changed]\n')
        _, records = self.build()
        self.assertEqual(records[0]['behavior'], 'stale')

    def test_unknown_evidence_is_rejected(self):
        entry = dict(id='absent', fingerprint='x', target='linux-x64', status='pass', command='check', notes='')
        (self.root / 'porting/evidence.yaml').write_text(yaml.safe_dump({'entries': [entry]}))
        with self.assertRaises(ValueError):
            self.build()

    def test_policy_edit_invalidates_evidence(self):
        _, records = self.build()
        item = next(r for r in records if r['category'] == 'language')
        entry = dict(id=item['id'], fingerprint=item['fingerprint'], target='linux-x64', status='pass', command='actual-check', notes='fixture')
        (self.root / 'porting/evidence.yaml').write_text(yaml.safe_dump({'entries': [entry]}))
        path = self.root / 'porting/manifests/language.yaml'
        node = p.read_yaml(path)
        node['questions'].append('New agreed review boundary')
        path.write_text(yaml.safe_dump(node))
        _, records = self.build()
        self.assertEqual(next(r for r in records if r['id'] == item['id'])['behavior'], 'stale')

    def test_dependencies_are_exposed(self):
        path = self.root / 'porting/manifests/tests.yaml'
        node = p.read_yaml(path)
        node['depends_on'] = ['language']
        path.write_text(yaml.safe_dump(node))
        _, records = self.build()
        self.assertEqual(next(r for r in records if r['category'] == 'tests')['blocked_by'], ['language'])

    def test_unknown_dependency_is_rejected(self):
        path = self.root / 'porting/manifests/tests.yaml'
        node = p.read_yaml(path)
        node['depends_on'] = ['nonexistent']
        path.write_text(yaml.safe_dump(node))
        with self.assertRaises(ValueError):
            self.build()

    def test_unknown_source_fields_are_rejected(self):
        path = self.root / 'porting/sources.yaml'
        node = p.read_yaml(path)
        node['unexpected'] = True
        path.write_text(yaml.safe_dump(node))
        with self.assertRaises(ValueError):
            self.build()


if __name__ == '__main__':
    unittest.main()
