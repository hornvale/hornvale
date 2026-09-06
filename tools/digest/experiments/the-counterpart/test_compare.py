import copy
import json
from pathlib import Path
import unittest
from compare import suggest, score, imports_only

CONTRACT = json.loads(Path(__file__).with_name('contract.json').read_text())
IDS = {q['id'] for q in CONTRACT['questions']}

class ComparatorTests(unittest.TestCase):
    def test_unenrolled_and_unknown_paths_fall_back(self):
        for path in ['domains/settlement/src/lib.rs', 'README.md', 'other/src/lib.rs']:
            result = suggest('path', [path], {}, [], CONTRACT)
            self.assertTrue(result['unknown'])
            self.assertEqual(set(result['effective']), IDS)

    def test_thing_path_selects_contributor(self):
        result = suggest('path', ['domains/thing/src/lib.rs'], {}, [], CONTRACT)
        self.assertEqual(set(result['proposed']), IDS)
        self.assertFalse(result['unknown'])

    def test_cargo_reverse_closure_and_deleted_membership(self):
        graph = {'workspace_root':'/owned', 'packages':[
            {'id':'s', 'name':'hornvale-settlement','manifest_path':'/owned/domains/settlement/Cargo.toml'},
            {'id':'t', 'name':'hornvale-thing','manifest_path':'/owned/domains/thing/Cargo.toml'},
            {'id':'k', 'name':'hornvale-kernel','manifest_path':'/owned/kernel/Cargo.toml'}],
            'resolve':{'nodes':[{'id':'s','dependencies':['k']},{'id':'t','dependencies':['k']},{'id':'k','dependencies':[]}]}}
        result = suggest('cargo', ['kernel/src/lib.rs'], {'cargo':[graph, graph]}, [], CONTRACT)
        self.assertEqual(set(result['proposed']), IDS)
        deleted = copy.deepcopy(graph)
        deleted['packages'] = [p for p in graph['packages'] if p['id'] != 's']
        result = suggest('cargo', ['domains/settlement/src/lib.rs'], {'cargo':[graph,deleted]}, [], CONTRACT)
        self.assertEqual(set(result['proposed']), IDS - {'components'})
        self.assertFalse(result['unknown'])

    def test_outboard_metadata_maps_repository_packages(self):
        graph={'workspace_root':'/owned/tools/digest','packages':[
            {'id':'t','name':'hornvale-thing','manifest_path':'/owned/domains/thing/Cargo.toml'}],
            'resolve':{'nodes':[{'id':'t','dependencies':[]}]}}
        result=suggest('cargo',['domains/thing/src/lib.rs'],
                       {'cargo':[graph,graph], 'repository_roots':['/owned','/owned']},[],CONTRACT)
        self.assertFalse(result['unknown'])
        self.assertEqual(set(result['proposed']),IDS)

    def test_missing_graph_is_unknown(self):
        self.assertTrue(suggest('cargo',['kernel/src/lib.rs'],{},[],CONTRACT)['unknown'])

    def test_agreement_requires_named_negative_assumption(self):
        owner = {'owner':'thing','owned_paths':['domains/thing/src/lib.rs'],
            'supplies':['fresh'],'consumes':[], 'positive_assumptions':[],
            'negative_assumptions':['unclaimed:fresh — no earlier owner'],
            'affected_questions':list(IDS), 'variants':[{'id':'add','changed_subjects':['THING_KINDS'],
            'supplies':['fresh'],'consumes':[]}]}
        metadata = {'variants':['add']}
        before = suggest('agreement',['domains/thing/src/lib.rs'],metadata,[owner],CONTRACT)
        self.assertFalse(before['unknown'])
        owner['negative_assumptions'] = []
        after = suggest('agreement',['domains/thing/src/lib.rs'],metadata,[owner],CONTRACT)
        self.assertTrue(after['unknown'])
        self.assertEqual(set(after['effective']),IDS)

    def test_reviewed_settlement_name_subjects_and_prose_assumptions(self):
        owner=json.loads(Path(__file__).with_name('owners').joinpath('settlement.json').read_text())
        result=suggest('agreement',['domains/settlement/src/lib.rs'],{'variants':['settlement-safe']},[owner],CONTRACT)
        self.assertFalse(result['unknown'])
        self.assertEqual(set(result['proposed']),IDS-{'components'})
        owner['negative_assumptions']=[]
        missing=suggest('agreement',['domains/settlement/src/lib.rs'],{'variants':['settlement-safe']},[owner],CONTRACT)
        self.assertTrue(missing['unknown'])

    def test_imports_only_omission_keeps_full_fallback(self):
        owners=[json.loads(Path(__file__).with_name('owners').joinpath(name+'.json').read_text()) for name in ['thing','settlement']]
        result=imports_only(['domains/settlement/src/lib.rs'],{'variants':['settlement-collision']},owners,CONTRACT)
        self.assertEqual(result['proposed'],[])
        self.assertTrue(result['unknown'])
        self.assertEqual(set(result['effective']),IDS)
        lender=imports_only(['domains/settlement/src/lib.rs'],{'variants':['settlement-lender-rename']},owners,CONTRACT)
        self.assertIn('borrowing',lender['proposed'])

    def test_score_rejects_missing_question(self):
        with self.assertRaises(ValueError):
            score({'proposed':[], 'effective':[], 'unknown':[]},{})

    def test_scores_preserve_raw_miss_and_selected_unknown(self):
        outcomes = {q:{'outcome':'satisfied','reason':'equal'} for q in IDS}
        outcomes['registration']['outcome']='violated'
        outcomes['ownership']['outcome']='unknown'
        result = score({'proposed':['components','ownership'],'unknown':['missing scope'], 'effective':list(IDS)},outcomes)
        self.assertEqual(result['raw']['unselected_violating'],['registration'])
        self.assertEqual(result['raw']['selected_unknown'],['ownership'])
        self.assertEqual(result['effective']['selected_violating'],['registration'])

if __name__ == '__main__': unittest.main()
