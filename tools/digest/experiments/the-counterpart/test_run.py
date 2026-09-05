import base64
import copy
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch
import run as runner
from run import load_json, summarize, reconstruct, capture, sha256, construct_sources
from checker import evaluate
from compare import suggest, score

CONTRACT = json.loads(Path(__file__).with_name('contract.json').read_text())


def sample(raw=b'{}'):
    result = dict(exit_code=0, interrupted=False, harness_deadline_exceeded=False,
                  launch_error=None, cleanup_error=None, output_limit_exceeded=False,
                  direct_child_waited=True)
    for stream in ['stdout','stderr']:
        data = raw if stream=='stdout' else b''
        result.update({stream+'_base64':base64.b64encode(data).decode(),
                       stream+'_sha256':hashlib.sha256(data).hexdigest(), stream+'_bytes':len(data)})
    return result


class DossierTests(unittest.TestCase):
    def setUp(self):
        self.panel = {'schema':'counterpart-v1','identities':{'checker':'c','rules':'r','contract':'q','roster':'o','implementation':'i'},
                      'owner_records':[], 'arms':{'base':{'commit':'a','tree':'t','variants':[],'changed_paths':[], 'inputs':{'pin':{'sha256':'pin','bytes':3}}}}, 'base':'a', 'bundle_prerequisites':['a']}
        raw = Path(__file__).with_name('evidence').joinpath('thing-author/thing-safe-addition-raw.stdout').read_bytes()
        self.row = {'inputs':{'pin':{'sha256':'pin','bytes':3}}, 'arm':'base','source':{'commit':'a','tree':'t'}, 'identities':self.panel['identities'].copy(),
                    'commands':[dict(sample(), role=role) for role in ['metadata-base','metadata-arm','build']] + [dict(sample(raw),role='observe')], 'outcomes':evaluate(json.loads(raw)['facts'],CONTRACT)}
        graph={'workspace_root':'/fixture','packages':[], 'resolve':{'nodes':[]}}
        for command in self.row['commands'][:2]:
            command.update(sample(json.dumps(graph).encode()))
        self.row['metadata']={'cargo':[graph,graph],'repository_roots':['/fixture','/fixture'],'variants':[]}
        self.row['selections']={k:suggest(k,[],self.row['metadata'],[],CONTRACT) for k in ['path','cargo','agreement']}
        self.row['comparisons']={k:score(v,self.row['outcomes']) for k,v in self.row['selections'].items()}
        self.dossier = {'schema':'counterpart-v1','arms':{'base':self.row},'bundle_prerequisites':['a']}

    def test_complete_requires_entire_roster(self):
        self.assertTrue(summarize(self.dossier,self.panel,CONTRACT)['completed'])
        self.dossier['arms']={}
        with self.assertRaisesRegex(ValueError,'missing arms'): summarize(self.dossier,self.panel,CONTRACT)

    def test_corruption_and_failures_are_incomplete(self):
        mutations = [lambda r:r['outcomes'].pop('ownership'),
                     lambda r:r['identities'].update(checker='stale'),
                     lambda r:r['identities'].update(rules='stale'),
                     lambda r:r['source'].update(commit='stale'),
                     lambda r:r['inputs']['pin'].update(sha256='stale'),
                     lambda r:r.pop('selections'),
                     lambda r:r['comparisons']['path']['raw'].update(selected_violating=['registration']),
                     lambda r:r.update(arm='different'),
                     lambda r:r['commands'][0].update(stdout_base64=base64.b64encode(b'tampered').decode()),
                     lambda r:r['commands'][0].update(exit_code=1),
                     lambda r:r['commands'][0].update(harness_deadline_exceeded=True),
                     lambda r:r['commands'][0].update(cleanup_error='uncertain'),
                     lambda r:r['commands'][0].pop('interrupted')]
        for change in mutations:
            dossier=copy.deepcopy(self.dossier);change(dossier['arms']['base'])
            with self.subTest(change=change), self.assertRaises(ValueError): summarize(dossier,self.panel,CONTRACT)

    def test_missing_prerequisite_refuses(self):
        self.dossier['bundle_prerequisites']=[]
        with self.assertRaisesRegex(ValueError,'prerequisite'): summarize(self.dossier,self.panel,CONTRACT)

    def test_refusal_is_complete_observation(self):
        raw=json.loads(base64.b64decode(self.row['commands'][-1]['stdout_base64']))
        raw['facts']['thing_registration']={'outcome':'refused','detail':'fixture refusal'}
        self.row['commands'][-1]=dict(sample(json.dumps(raw).encode()),role='observe')
        self.row['outcomes']=evaluate(raw['facts'],CONTRACT)
        self.row['comparisons']={k:score(v,self.row['outcomes']) for k,v in self.row['selections'].items()}
        self.assertTrue(summarize(self.dossier,self.panel,CONTRACT)['completed'])

    def test_duplicate_and_invalid_json_refuse(self):
        for raw in ['{"a":1,"a":2}', '{']:
            with self.assertRaises(ValueError): load_json(raw)

    def test_real_capture_persists_nonzero_process(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory)
            result=capture([sys.executable,'-c',"import sys; print('kept'); sys.exit(7)"],root,root/'attempt.json')
            self.assertEqual(result['exit_code'],7)
            self.assertEqual(load_json((root/'attempt.json').read_text())['stdout_sha256'],hashlib.sha256(b'kept\n').hexdigest())
            with self.assertRaises(FileExistsError): capture([sys.executable,'-c','pass'],root,root/'attempt.json')

    def test_source_construction_uses_exact_patches_and_rejects_noop(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory); repo=root/'repo'; repo.mkdir()
            def git(*args):
                return subprocess.check_output(['git','-C',str(repo),*args],stderr=subprocess.DEVNULL,text=True).strip()
            git('init');git('config','user.email','fixture@example.invalid');git('config','user.name','Fixture')
            (repo/'left').write_text('base\n');(repo/'right').write_text('base\n')
            git('add','.');git('commit','-m','base');base=git('rev-parse','HEAD')
            variants={}
            for name in ['left','right']:
                (repo/name).write_text('changed\n')
                patch=root/(name+'.patch');patch.write_text(git('diff')+'\n')
                git('restore',name)
                variants[name]={'patch':patch,'owned_paths':[name]}
            arms=construct_sources(repo,base,variants,{'pair':['left','right']},root/'objects')
            checkout=root/'replay'
            reconstruct(repo,root/'objects/specimens.bundle',checkout,base,arms['pair-joint'])
            self.assertEqual((checkout/'left').read_text(),'changed\n')
            self.assertEqual((checkout/'right').read_text(),'changed\n')
            self.assertEqual(git('status','--porcelain'),'')
            with self.assertRaises(RuntimeError):
                construct_sources(repo,base,{'one':variants['left'],'two':variants['left']},{'conflict':['one','two']},root/'conflict')
            empty=root/'empty.patch';empty.write_text('')
            with self.assertRaises((ValueError,RuntimeError)):
                construct_sources(repo,base,{'noop':{'patch':empty,'owned_paths':['left']}},{},root/'bad')

    def test_git_preparation_is_bounded_and_persisted(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory)
            previous=runner.git_audit_directory if hasattr(runner,'git_audit_directory') else None
            try:
                runner.git_audit_directory=root/'git-evidence'
                actual=runner.measurement.measure
                calls=[]
                def measured(command,cwd,**kwargs):
                    calls.append(kwargs)
                    return actual(command,cwd,**kwargs)
                with patch.object(runner.measurement,'measure',side_effect=measured):
                    self.assertIn('git version',runner.git(root,'--version'))
                self.assertTrue(calls)
                self.assertEqual(calls[0]['deadline_seconds'],3600)
                self.assertEqual(calls[0]['output_limit_bytes'],16*1024*1024)
                self.assertEqual(len(list((root/'git-evidence').glob('*.json'))),1)
            finally:
                runner.git_audit_directory=previous

    def test_real_git_bundle_reconstruction_and_environment(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory); repo=root/'repo';repo.mkdir()
            def git(*args):
                return subprocess.check_output(['git','-C',str(repo),*args],stderr=subprocess.DEVNULL,text=True).strip()
            git('init');git('config','user.email','fixture@example.invalid');git('config','user.name','Fixture')
            (repo/'source').write_text('base');git('add','.');git('commit','-m','base');base=git('rev-parse','HEAD')
            (repo/'source').write_text('arm');git('commit','-am','arm');arm=git('rev-parse','HEAD');tree=git('rev-parse','HEAD^{tree}')
            bundle=root/'specimens.bundle';git('bundle','create',str(bundle),'HEAD','^'+base)
            checkout=root/'checkout'
            prerequisites=reconstruct(repo,bundle,checkout,base,{'commit':arm,'tree':tree})
            self.assertEqual(prerequisites,[base])
            self.assertEqual((checkout/'source').read_text(),'arm')
            self.assertEqual(git('rev-parse','HEAD'),arm)
            with self.assertRaises(ValueError): reconstruct(repo,bundle,root/'bad',base,{'commit':arm,'tree':'0'*40})

if __name__=='__main__': unittest.main()
