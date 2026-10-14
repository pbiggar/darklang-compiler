#!/usr/bin/env python3
"""Regression checks for policy inheritance, discovery, and evidence invalidation."""
import importlib.util
import shutil
import tempfile
from types import SimpleNamespace
import unittest
from pathlib import Path
from unittest.mock import patch

import yaml

spec = importlib.util.spec_from_file_location('porting', Path(__file__).with_name('porting.py'))
p = importlib.util.module_from_spec(spec)
spec.loader.exec_module(p)
collect_spec = importlib.util.spec_from_file_location('collect_discovery', Path(__file__).with_name('collect-upstream-discovery.py'))
c = importlib.util.module_from_spec(collect_spec)
collect_spec.loader.exec_module(c)
c.p = p


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
    def test_builtin_candidates_distinguish_literal_and_computed_names(self):
        text = ('module Builtins.Example\nlet docs = """name = fn "fake" 0"""\n'
                '// name = fn "comment" 0\n{ name = fn "boolNot" 0 }\n'
                '{ name = value "answer" 2 }\n{ name = fn computedName 0 }\n')
        literals, computed = p.builtin_declarations(text)
        self.assertEqual(literals, [dict(kind='fn', name='boolNot', version=0, line=4),
                                    dict(kind='value', name='answer', version=2, line=5)])
        self.assertEqual(computed, [dict(kind='fn', line=6)])

    def test_computed_builtin_versions_remain_explicit_gaps(self):
        literals, computed = p.builtin_declarations('{ name = fn "versioned" nextVersion }')
        self.assertEqual(literals, [])
        self.assertEqual(computed, [dict(kind='fn', line=1)])

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
        for path in ('porting/sources.yaml', 'porting/evidence.yaml', 'porting/issues.yaml', 'porting/discovery.yaml', 'library-sources.list',
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
            'backend/src/LibParser/Parser.fs': b'module Parser\n',
            'packages/darklang/stdlib/list.dark': b'module Darklang.Stdlib.List\n',
            'packages/darklang/stdlib/dict.dark': b'module Darklang.Stdlib.Dict\n',
            'packages/darklang/stdlib/test.dark': b'module Darklang.Stdlib.Test\n',
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

    def test_builtin_nested_inventory_is_generated_not_handwritten(self):
        bridge = self.root / 'StdLib/Builtin/Bool.dark'
        bridge.parent.mkdir(parents=True)
        bridge.write_text('module Builtin\nlet boolNot (b: Bool) = b\n')
        _, records = self.build()
        identity = 'builtins:backend/src/Builtins/Builtins.Pure/Libs/Bool.fs#fn/boolNot/v0'
        item = next(r for r in records if r['id'] == identity)
        self.assertEqual(item['scope'], 'backend/src/Builtins/Builtins.Pure/Libs/Bool.fs/declarations/fn/boolNot/v0')
        self.assertEqual(item['compiler'], ['StdLib/Builtin/Bool.dark'])
        self.assertEqual(item['behavior'], 'unverified')

    def test_legacy_configuration_comes_from_shared_discovery_module(self):
        path = 'test/fixtures/e2e/upstream/example.dark'
        fixture = self.root / path
        fixture.parent.mkdir(parents=True)
        fixture.write_text('1L = 1L\n2L = 2L\n')
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
        node['children'] = []
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

    def legacy_export(self):
        path = 'test/fixtures/e2e/upstream/example.dark'
        fixture = self.root / path
        fixture.parent.mkdir(parents=True, exist_ok=True)
        fixture.write_text('1L = 1L\n2L = 2L\n')
        module = self.root / 'test/test-suite-tooling/E2EDiscovery.ml'
        module.parent.mkdir(parents=True, exist_ok=True)
        module.write_text('let disabledUpstreamFiles = []\n'
                          'let disabledUpstreamLines = [ ("' + path + '", [ 2 ]) ]\n')
        return dict(schema=1, target='linux-x64', mode='discovery-only', files=[path],
                    tests=[dict(source_file=path, source_line=2, name='L1: 1L', error_expectation=None,
                                exclusion_origin=None, effective_skip=None, eligible=True)], parse_errors=[])

    def write_capture(self, value, fingerprint=None):
        _, files = p.discover(self.root, self.upstream)
        document = dict(schema=1, legacy_runner=dict(compiler_context_sha256=fingerprint or p.compiler_context(self.root, files),
                        upstream_revision=self.revision, export=value))
        (self.root / 'porting/discovery.yaml').write_text(yaml.safe_dump(document))

    def test_current_discovery_adds_cases_but_not_passes(self):
        value = self.legacy_export()
        self.write_capture(value)
        outputs, records = self.build()
        cases = [r for r in records if '#runner/' in r['id']]
        self.assertEqual(len(cases), 1)
        self.assertEqual(cases[0]['runner_discovery']['status'], 'current')
        self.assertEqual(cases[0]['behavior'], 'unverified')
        group = next(r for r in records if r['id'] == 'tests:compiler/' + value['files'][0])
        self.assertEqual(group['runner_discovery']['reported_eligible'], 1)
        # Gate matching follows the actual L1 name, not source_line=2.
        self.assertEqual(group['runner_discovery']['unmatched_line_gates'], [2])
        self.assertIn('Legacy runner discovery: **current**', outputs['docs/porting/generated/tests.md'])

    def test_stale_discovery_keeps_selectors_without_current_claims(self):
        value = self.legacy_export()
        self.write_capture(value)
        _, first = self.build()
        identity = next(r['id'] for r in first if '#runner/' in r['id'])
        (self.root / 'dependencies.lock').write_text('changed build inputs')
        _, records = self.build()
        case = next(r for r in records if r['id'] == identity)
        self.assertEqual(case['runner_discovery']['status'], 'stale')
        self.assertTrue(case['observation'].startswith('stale'))
        group = next(r for r in records if r['id'] == 'tests:compiler/' + value['files'][0])
        self.assertIsNone(group['runner_discovery']['unmatched_line_gates'])

    def test_partial_discovery_retains_parse_failures(self):
        value = self.legacy_export()
        value['tests'] = []
        value['parse_errors'] = [dict(source_file=value['files'][0], message='fixture cannot parse')]
        self.write_capture(value)
        _, records = self.build()
        group = next(r for r in records if r['id'] == 'tests:compiler/' + value['files'][0])
        self.assertEqual(group['runner_discovery']['status'], 'partial')
        self.assertEqual(group['runner_discovery']['parse_error'], 'fixture cannot parse')
        self.assertIsNone(group['runner_discovery']['unmatched_line_gates'])

    def test_current_export_must_cover_every_fixture(self):
        value = self.legacy_export()
        value.update(files=[], tests=[])
        self.write_capture(value)
        with self.assertRaisesRegex(ValueError, 'complete legacy corpus'):
            self.build()

    def test_export_rejects_duplicates_paths_and_inconsistent_skips(self):
        import copy
        original = self.legacy_export()
        invalid = []
        duplicate = copy.deepcopy(original)
        duplicate['tests'] *= 2
        invalid.append(duplicate)
        inconsistent = copy.deepcopy(original)
        inconsistent['tests'][0]['eligible'] = False
        invalid.append(inconsistent)
        absolute = copy.deepcopy(original)
        absolute['files'] = ['/tmp/fixture.dark']
        invalid.append(absolute)
        traversal = copy.deepcopy(original)
        traversal['files'] = ['test/fixtures/e2e/upstream/../example.dark']
        invalid.append(traversal)
        failed = copy.deepcopy(original)
        failed['parse_errors'] = [dict(source_file=failed['files'][0], message='failed')]
        invalid.append(failed)
        for value in invalid:
            with self.subTest(value=value), self.assertRaises(ValueError):
                p.validate_runner_export(value)

    def test_discovery_rejects_out_of_range_actual_coordinates(self):
        value = self.legacy_export()
        value['tests'][0]['source_line'] = 3
        self.write_capture(value)
        with self.assertRaisesRegex(ValueError, 'outside its current fixture'):
            self.build()

    def test_configured_gates_reject_unknown_and_duplicate_entries(self):
        self.legacy_export()
        module = self.root / 'test/test-suite-tooling/E2EDiscovery.ml'
        for body in ('let disabledUpstreamFiles = [ "test/fixtures/e2e/upstream/absent.dark" ]\nlet disabledUpstreamLines = []\n',
                     'let disabledUpstreamFiles = []\nlet disabledUpstreamLines = [ ("test/fixtures/e2e/upstream/example.dark", [ 2; 2 ]) ]\n'):
            module.write_text(body)
            with self.assertRaises(ValueError):
                self.build()

    def gather_with(self, run):
        with patch.object(p, 'git_revision', return_value=self.revision), patch.object(p, 'checked_files', return_value=self.upstream):
            return c.gather(self.root, self.root, run=run)

    def test_collector_builds_first_and_does_not_write_before_validation(self):
        value = self.legacy_export()
        calls = []
        def run(args, **kwargs):
            calls.append(args)
            if args[0] == './run-tests':
                Path(args[-1].split('=', 1)[1]).write_text(yaml.safe_dump(value))
            self.assertFalse('LD_PRELOAD' in kwargs['env'])
            return SimpleNamespace(returncode=0, stdout='')
        document = self.gather_with(run)
        self.assertEqual(calls[0], ['./build', '--ai'])
        self.assertEqual(calls[1][:2], ['./run-tests', '--target=linux-x86_64'])
        self.assertEqual(document['legacy_runner']['export'], value)
        self.assertIsNone(p.read_yaml(self.root / 'porting/discovery.yaml')['legacy_runner'])

    def test_collector_build_failure_preserves_prior_ledger(self):
        self.legacy_export()
        original = (self.root / 'porting/discovery.yaml').read_bytes()
        with self.assertRaisesRegex(ValueError, 'Native build failed'):
            self.gather_with(lambda *args, **kwargs: SimpleNamespace(returncode=1, stdout='blocked'))
        self.assertEqual((self.root / 'porting/discovery.yaml').read_bytes(), original)

    def test_collector_rejects_input_changes_during_build(self):
        value = self.legacy_export()
        def run(args, **kwargs):
            if args[0] == './build':
                (self.root / 'dependencies.lock').write_text('changed during build')
            else:
                Path(args[-1].split('=', 1)[1]).write_text(yaml.safe_dump(value))
            return SimpleNamespace(returncode=0, stdout='')
        with self.assertRaisesRegex(ValueError, 'changed during collection'):
            self.gather_with(run)

    def test_collector_preserves_partial_export_and_exit_contract(self):
        value = self.legacy_export()
        value.update(tests=[], parse_errors=[dict(source_file=value['files'][0], message='parse failed')])
        def run(args, **kwargs):
            if args[0] == './run-tests':
                Path(args[-1].split('=', 1)[1]).write_text(yaml.safe_dump(value))
                return SimpleNamespace(returncode=1, stdout='partial')
            return SimpleNamespace(returncode=0, stdout='')
        self.assertEqual(self.gather_with(run)['legacy_runner']['export'], value)


if __name__ == '__main__':
    unittest.main()
