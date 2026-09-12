#!/usr/bin/env python3
"""Dependency boundary regressions; graphs model Cargo's resolved package IDs."""
import copy
import importlib.util
from pathlib import Path
import unittest

SCRIPT = Path(__file__).with_name('visual-dependencies.py')
ROOT = Path('/fixture')


def graph():
    packages = [
        ('source', 'hornvale-visual-source', 'clients/visual/source'),
        ('view', 'hornvale-bevy-view', 'clients/visual/bevy'),
        ('app', 'planetarium', 'clients/visual/planetarium'),
        ('sim', 'hornvale-kernel', 'kernel'),
        ('bevy', 'bevy', 'registry/bevy'),
        ('middle', 'helper', 'registry/helper'),
    ]
    return {
        'workspace_root': str(ROOT / 'clients/visual'),
        'workspace_members': ['source', 'view', 'app'],
        'packages': [{'id': i, 'name': n, 'manifest_path': str(ROOT / p / 'Cargo.toml')}
                     for i, n, p in packages],
        'resolve': {'nodes': [{'id': i, 'deps': []} for i, _, _ in packages]},
    }


def edge(meta, start, end, kind=None, alias='renamed'):
    node = next(n for n in meta['resolve']['nodes'] if n['id'] == start)
    node['deps'].append({'name': alias, 'pkg': end,
                         'dep_kinds': [{'kind': kind, 'target': None}]})


class BoundaryTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        assert SCRIPT.exists(), 'resolved visual dependency guard is missing'
        spec = importlib.util.spec_from_file_location('visual_dependencies', SCRIPT)
        cls.guard = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.guard)

    def test_allowed_application_and_source_edges(self):
        meta = graph()
        for start, end in [('app', 'source'), ('app', 'view'), ('view', 'bevy'), ('source', 'sim')]:
            edge(meta, start, end)
        self.assertEqual(self.guard.check(meta, ROOT), [])

    def test_direct_renamed_and_transitive_leaks_of_every_kind(self):
        for start, end in [('source', 'bevy'), ('source', 'view'), ('source', 'app'),
                           ('view', 'sim'), ('view', 'source'), ('view', 'app')]:
            for kind in [None, 'dev', 'build']:
                for transitive in [False, True]:
                    with self.subTest(start=start, end=end, kind=kind, transitive=transitive):
                        meta = graph()
                        if transitive:
                            edge(meta, start, 'middle', kind)
                            edge(meta, 'middle', end, kind)
                        else:
                            edge(meta, start, end, kind)
                        self.assertTrue(self.guard.check(meta, ROOT))

    def test_optional_resolved_edge_is_not_filtered(self):
        meta = graph()
        edge(meta, 'view', 'sim', alias='optional_sim')
        self.assertTrue(self.guard.check(meta, ROOT))

    def test_simulation_is_identified_by_path_not_name(self):
        meta = graph()
        next(p for p in meta['packages'] if p['id'] == 'sim')['name'] = 'innocent'
        edge(meta, 'view', 'sim')
        self.assertTrue(self.guard.check(meta, ROOT))

    def test_bevy_subcrate_is_forbidden_in_source(self):
        meta = graph()
        next(p for p in meta['packages'] if p['id'] == 'bevy')['name'] = 'bevy_ecs'
        edge(meta, 'source', 'bevy')
        self.assertTrue(self.guard.check(meta, ROOT))

    def test_unresolved_or_incomplete_metadata_fails_closed(self):
        original = graph()
        broken = []
        for key in ['resolve', 'packages', 'workspace_members']:
            m = copy.deepcopy(original)
            del m[key]
            broken.append(m)
        m = graph()
        edge(m, 'source', 'unknown')
        broken.append(m)
        m = graph()
        m['resolve']['nodes'].pop()
        broken.append(m)
        m = graph()
        m['workspace_members'].remove('source')
        broken.append(m)
        for meta in broken:
            with self.subTest(meta=meta):
                with self.assertRaises(ValueError):
                    self.guard.check(meta, ROOT)


if __name__ == '__main__':
    unittest.main()
