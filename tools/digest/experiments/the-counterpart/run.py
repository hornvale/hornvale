"""Owned, bounded Counterpart replay. Specimen commits are experimental data."""
import argparse
import base64
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import platform
import re
import signal
import sys
import time
import tempfile
import uuid
import unittest

from checker import evaluate
from compare import suggest, score, imports_only

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
_spec = importlib.util.spec_from_file_location('counterpart_measure', ROOT / 'scripts/charter_measure.py')
measurement = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(measurement)
LIMIT = 16 * 1024 * 1024
DEADLINE = 3600
ROLES = {'metadata-base','metadata-arm','build','observe'}
git_audit_directory = None


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def load_json(raw):
    def unique(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError('duplicate JSON key: ' + key)
            result[key] = value
        return result
    return json.loads(raw, object_pairs_hook=unique)


def persist(path, value):
    with Path(path).open('x', encoding='utf-8') as output:
        json.dump(value, output, indent=2, sort_keys=True)
        output.write('\n')


def capture(command, cwd, destination):
    destination = Path(destination)
    # Reserve before executing: an existing attempt never runs again.
    with destination.open('x'):
        pass
    result = measurement.measure(command, cwd, retain_output=True,
                                 output_limit_bytes=LIMIT, deadline_seconds=DEADLINE)
    destination.write_text(json.dumps(result, indent=2, sort_keys=True) + '\n')
    # Persist exact bytes before reacting to unsafe cleanup or interruption.
    if result['cleanup_error'] or result['interrupted'] or measurement.unsafe_cleanup:
        raise RuntimeError('stopped after persisted unsafe/interrupted attempt: ' + str(destination))
    return result


def git_bytes(root, *args):
    global git_audit_directory
    if git_audit_directory is None:
        git_audit_directory = Path(tempfile.mkdtemp(prefix='counterpart-git-evidence-'))
    git_audit_directory.mkdir(parents=True, exist_ok=True)
    sample = capture(['git','-C',str(root),*args],root,git_audit_directory/(uuid.uuid4().hex+'.json'))
    try:
        return validate_sample(sample)['stdout']
    except ValueError as error:
        raise RuntimeError('Git preparation failed: '+sample.get('stderr','')) from error


def git(root, *args):
    return git_bytes(root,*args).decode('utf-8').strip()


def prerequisites(bundle):
    with Path(bundle).open('rb') as stream:
        if stream.readline().strip() not in {b'# v2 git bundle', b'# v3 git bundle'}:
            raise ValueError('invalid bundle header')
        result = []
        for line in stream:
            if line == b'\n':
                break
            if line.startswith(b'-'):
                result.append(line[1:].split()[0].decode('ascii'))
        return sorted(result)


def verify_source(checkout, source):
    for key in ('commit','tree'):
        if not re.fullmatch('[0-9a-f]{40}', source[key]):
            raise ValueError('source requires full SHA: ' + key)
    if git(checkout,'rev-parse','HEAD') != source['commit'] or git(checkout,'rev-parse','HEAD^{tree}') != source['tree']:
        raise ValueError('source commit/tree mismatch')
    if git(checkout,'status','--porcelain','--untracked-files=no'):
        raise ValueError('source modified')


def reconstruct(repository, bundle, checkout, base, source):
    if prerequisites(bundle) != [base]:
        raise ValueError('absent or unexpected bundle prerequisite')
    if not re.fullmatch('[0-9a-f]{40}', base):
        raise ValueError('base requires full SHA')
    checkout = Path(checkout)
    if checkout.exists():
        raise FileExistsError(checkout)
    git(repository,'cat-file','-e',base+'^{commit}')
    git(repository,'clone','--no-hardlinks','--no-checkout',str(repository),str(checkout))
    git(checkout,'bundle','verify',str(Path(bundle).resolve()))
    git(checkout,'fetch',str(Path(bundle).resolve()),'refs/heads/*:refs/remotes/specimens/*')
    git(checkout,'checkout','--detach',source['commit'])
    verify_source(checkout,source)
    return prerequisites(bundle)


def construct_sources(repository, base, variants, pairs, destination):
    """Author only experimental commit-tree objects in an owned clone."""
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=False)
    checkout = destination/'source-objects'
    git(repository,'clone','--no-hardlinks','--no-checkout',str(repository),str(checkout))
    git(checkout,'config','user.name','Counterpart specimen data')
    git(checkout,'config','user.email','counterpart@example.invalid')
    base_tree = git(checkout,'rev-parse',base+'^{tree}')
    git(checkout,'checkout','--detach',base)
    arms = {'base':{'commit':base,'tree':base_tree,'variants':[],'changed_paths':[], 'inputs':input_hashes(checkout)}}
    combinations = {name:[name] for name in variants}
    combinations.update({name+'-joint':names for name,names in pairs.items()})
    for name, members in combinations.items():
        if not re.fullmatch('[a-z0-9-]+', name):
            raise ValueError('invalid arm ID')
        git(checkout,'checkout','--detach',base)
        for member in members:
            variant = variants[member]
            before_tree = git(checkout,'write-tree')
            git(checkout,'apply','--index','--whitespace=error',str(Path(variant['patch']).resolve()))
            tree = git(checkout,'write-tree')
            changed = git(checkout,'diff','--name-only',before_tree,tree).splitlines()
            if not changed or not set(changed) <= set(variant['owned_paths']):
                raise ValueError('unchanged or out-of-scope patch: '+member)
        tree = git(checkout,'write-tree')
        if tree == base_tree:
            raise ValueError('unchanged arm: '+name)
        commit = git(checkout,'commit-tree',tree,'-p',base,'-m','Counterpart experimental source: '+name)
        git(checkout,'update-ref','refs/heads/specimen-'+name,commit)
        paths = git(checkout,'diff','--name-only',base,commit).splitlines()
        arms[name] = {'commit':commit,'tree':tree,'variants':members,'changed_paths':paths,
                      'inputs':input_hashes(checkout), 'patches':[{ 'variant':m, 'sha256':sha256(Path(variants[m]['patch']).read_bytes())} for m in members]}
        # The checkout belongs only to this function, and no child remains.
        git(checkout,'restore','--source='+base,'--staged','--worktree','.')
    git(checkout,'bundle','create',str((destination/'specimens.bundle').resolve()),
        *['refs/heads/specimen-'+n for n in combinations],'^'+base)
    git(checkout,'bundle','verify',str((destination/'specimens.bundle').resolve()))
    return arms


def validate_sample(sample):
    flags = {'interrupted','harness_deadline_exceeded','output_limit_exceeded'}
    if any(sample.get(key) is not False for key in flags):
        raise ValueError('incomplete attempt failure flags')
    if sample.get('exit_code') != 0 or sample.get('direct_child_waited') is not True:
        raise ValueError('failed command or uncollected child')
    if any(key not in sample or sample[key] is not None for key in ('launch_error','cleanup_error')):
        raise ValueError('incomplete command cleanup/launch')
    streams = {}
    for name in ('stdout','stderr'):
        try:
            raw = base64.b64decode(sample[name+'_base64'], validate=True)
        except (KeyError, ValueError) as error:
            raise ValueError('missing/invalid raw bytes') from error
        if len(raw) > LIMIT or len(raw) != sample.get(name+'_bytes') or sha256(raw) != sample.get(name+'_sha256'):
            raise ValueError('raw output size/hash mismatch')
        streams[name] = raw
    return streams


def summarize(dossier, panel, contract):
    if dossier.get('schema') != 'counterpart-v1' or panel.get('schema') != 'counterpart-v1':
        raise ValueError('invalid dossier schema')
    missing = set(panel['arms']) - set(dossier['arms'])
    if missing:
        raise ValueError('missing arms: ' + ', '.join(sorted(missing)))
    if set(dossier['arms']) != set(panel['arms']):
        raise ValueError('unexpected arms')
    if dossier.get('bundle_prerequisites') != [panel['base']] or panel.get('bundle_prerequisites') != [panel['base']]:
        raise ValueError('bundle prerequisite mismatch')
    ids = {q['id'] for q in contract['questions']}
    summaries = {}
    for ident, arm in dossier['arms'].items():
        expected = panel['arms'][ident]
        if arm.get('arm') != ident or arm.get('source') != {k:expected[k] for k in ('commit','tree')}:
            raise ValueError('arm/source attribution mismatch')
        if arm.get('inputs') != expected.get('inputs'):
            raise ValueError('source input/lock/pin hash mismatch')
        if arm.get('identities') != panel['identities']:
            raise ValueError('checker/rule/roster/implementation identity mismatch')
        commands = arm.get('commands', [])
        roles = [c.get('role') for c in commands]
        if len(roles) != len(set(roles)) or set(roles) != ROLES:
            raise ValueError('missing/duplicate command result')
        streams = {c['role']:validate_sample(c) for c in commands}
        raw = load_json(streams['observe']['stdout'])
        if raw.get('schema') != 'counterpart-v1' or set(raw) != {'schema','facts','candidate'}:
            raise ValueError('invalid raw observation')
        observed = evaluate(raw['facts'], contract)
        if set(arm.get('outcomes', {})) != ids or arm['outcomes'] != observed:
            raise ValueError('missing/duplicate/stale question result')
        graphs = [load_json(streams[role]['stdout']) for role in ('metadata-base','metadata-arm')]
        metadata = arm.get('metadata', {})
        if metadata.get('cargo') != graphs or metadata.get('variants') != expected.get('variants', []):
            raise ValueError('recorded metadata/variant attribution mismatch')
        selections = {kind:suggest(kind,expected['changed_paths'],metadata,panel['owner_records'],contract) for kind in ('path','cargo','agreement')}
        comparisons = {kind:score(selection,observed) for kind,selection in selections.items()}
        if arm.get('selections') != selections or arm.get('comparisons') != comparisons:
            raise ValueError('missing/stale selector or confusion result')
        summaries[ident] = {'outcomes':observed, 'comparisons':comparisons}
    return {'completed':True, 'arms':summaries}


def input_hashes(checkout):
    paths = ['Cargo.lock','tools/digest/Cargo.lock','rust-toolchain.toml',
             'domains/thing/src/lib.rs','domains/settlement/src/lib.rs']
    paths.extend(p.relative_to(checkout).as_posix() for p in sorted((checkout/'.cargo').glob('*')) if p.is_file())
    result = {}
    for name in paths:
        path = checkout / name
        if path.is_file():
            data = path.read_bytes()
            result[name] = {'sha256':sha256(data),'bytes':len(data)}
    return result


def run_panel(panel_path, output):
    global git_audit_directory
    panel_path = panel_path.resolve()
    panel = load_json(panel_path.read_text())
    output.mkdir(parents=True, exist_ok=False)
    git_audit_directory = output/'git-preparation'
    contract = load_json((panel_path.parent/'contract.json').read_text())
    owners = [load_json((panel_path.parent/p).read_text()) for p in panel['owners']]
    expected = {'checker':sha256((HERE/'checker.py').read_bytes()),
                'rules':sha256((HERE/'compare.py').read_bytes()),
                'contract':sha256((panel_path.parent/'contract.json').read_bytes()),
                'measure':sha256((ROOT/'scripts/charter_measure.py').read_bytes()),
                'owners':sha256(json.dumps(owners,sort_keys=True,separators=(',',':')).encode()),
                'roster':sha256(json.dumps(panel['arms'],sort_keys=True,separators=(',',':')).encode()),
                'implementation':panel['identities']['implementation']}
    if owners != panel['owner_records']:
        raise ValueError('frozen owner declarations mismatch')
    if expected != panel['identities']:
        raise ValueError('frozen implementation/input identity mismatch')
    for name in ('run.py','compare.py','checker.py'):
        frozen = git_bytes(ROOT,'show',panel['identities']['implementation']+':tools/digest/experiments/the-counterpart/'+name)
        if frozen != (HERE/name).read_bytes():
            raise ValueError('implementation bytes differ: '+name)
    for name, identity in panel['frozen_inputs'].items():
        data = (panel_path.parent/name).read_bytes()
        if identity != {'sha256':sha256(data),'bytes':len(data)}:
            raise ValueError('frozen owner/patch/config mismatch: '+name)
    bundle = (panel_path.parent/panel['bundle']).resolve()
    if sha256(bundle.read_bytes()) != panel['bundle_sha256']:
        raise ValueError('bundle hash mismatch')
    checkout = output/'checkout'
    first = next(iter(panel['arms'].values()))
    prerequisite = reconstruct(ROOT,bundle,checkout,panel['base'],first)
    dossier = {'schema':'counterpart-v1','arms':{},'bundle_prerequisites':prerequisite,
               'git_preparation_directory':str(git_audit_directory), 'host':platform.platform(),'environment':{k:v for k,v in measurement.controlled_env().items() if k in {'PATH','LANG','LC_ALL','CARGO_HOME','RUSTUP_HOME'}},
               'features':'default','profile':'dev','target':str(output/'target'),
               'queue_seconds':None,'author_seconds':None,
               'cost_note':'Queue/author costs unavailable to runner; not inferred from execution wall.'}
    persist(output/'manifest.json',panel)
    for tool, command in [('rustc',['rustc','-Vv']),('cargo',['cargo','--version'])]:
        result = capture(command,checkout,output/(tool+'.json'))
        validate_sample(result)
    for ident, source in panel['arms'].items():
        arm_dir = output/ident
        arm_dir.mkdir()
        before = time.monotonic()
        commands = []
        for label, revision in [('metadata-base',panel['base']),('metadata-arm',source['commit'])]:
            git(checkout,'checkout','--detach',revision)
            result = capture(['cargo','metadata','--locked','--offline','--format-version','1','--manifest-path','tools/digest/Cargo.toml'],checkout,arm_dir/(label+'.json'))
            result['role']=label;commands.append(result)
            validate_sample(result)
        verify_source(checkout,source)
        hashes = input_hashes(checkout)
        if hashes != source['inputs']:
            raise ValueError('frozen source input/lock/pin mismatch')
        graphs = [load_json(base64.b64decode(c['stdout_base64'])) for c in commands]
        metadata = {'cargo':graphs, 'repository_roots':[str(checkout),str(checkout)], 'variants':source.get('variants',[])}
        selections = {kind:suggest(kind,source['changed_paths'],metadata,owners,contract) for kind in ('path','cargo','agreement')}
        # Suggestions are persisted before any build/behavioral observation.
        persist(arm_dir/'selections.json',selections)
        prepared = time.monotonic()-before
        command = ['cargo','build','--locked','--offline','--manifest-path','tools/digest/Cargo.toml','-p','digest-counterpart','--target-dir',str(output/'target')]
        result = capture(command,checkout,arm_dir/'build.json');result['role']='build';commands.append(result)
        validate_sample(result)
        result = capture([str(output/'target/debug/digest-counterpart')],checkout,arm_dir/'observe.json');result['role']='observe';commands.append(result)
        streams = validate_sample(result)
        raw = load_json(streams['stdout'])
        outcomes = evaluate(raw['facts'],contract)
        verify_source(checkout,source)
        if input_hashes(checkout) != hashes:
            raise ValueError('input hashes changed during attempt')
        row = {'arm':ident,'source':{k:source[k] for k in ('commit','tree')},'identities':panel['identities'],
               'commands':commands,'inputs':hashes,'metadata':metadata,'selections':selections,
               'outcomes':outcomes,'candidate':raw['candidate'],
               'comparisons':{k:score(v,outcomes) for k,v in selections.items()},
               'preparation_seconds':prepared,'wall_seconds':time.monotonic()-before,
               'execution_seconds':result['elapsed_seconds']}
        persist(arm_dir/'arm.json',row)
        dossier['arms'][ident]=row
        persist(output/('progress-'+ident+'.json'),dossier)
    supplements = {}
    for name, pair in panel.get('pairs', {}).items():
        joint = dossier['arms'][pair['joint']]
        right = panel['arms'][pair['right']]
        context = {'variants':right['variants']}
        declaration = imports_only(right['changed_paths'], context, owners, contract)
        supplements[name] = {'request_scope':'right owner applied to left source; no behavioral input',
                             'selection':declaration, 'score':score(declaration,joint['outcomes'])}
    dossier['imports_only_supplement'] = supplements
    summary = summarize(dossier,panel,contract)
    summary['scope'] = panel.get('scope','full unreserved panel')
    summary['parent_panel_sha256'] = panel.get('parent_panel_sha256')
    summary['properties'] = qualify_pairs(dossier,panel)
    persist(output/'dossier.json',dossier)
    persist(output/'summary.json',summary)
    return summary


def qualify_pairs(dossier, panel):
    """Describe observed properties; failed premises cannot become interactions."""
    def passing(arm):
        return all(value['outcome']=='satisfied' for value in dossier['arms'][arm]['outcomes'].values())
    result = {}
    for name, pair in panel.get('pairs', {}).items():
        solo_pass = all(passing(pair[k]) for k in ('base','left','right'))
        joint = dossier['arms'][pair['joint']]['outcomes']
        result[name] = {
            'classes':pair['classes'],
            'base_satisfied':passing(pair['base']),
            'both_solos_satisfied':passing(pair['left']) and passing(pair['right']),
            'safe_pair':solo_pass and passing(pair['joint']),
            'joint_only_interaction':solo_pass and any(v['outcome']=='violated' for v in joint.values()),
            'solo_dependency_failure':passing(pair['base']) and not (passing(pair['left']) and passing(pair['right'])),
            'joint_violating':[q for q,v in joint.items() if v['outcome']=='violated'],
            'joint_unknown':[q for q,v in joint.items() if v['outcome']=='unknown']}
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--panel',type=Path)
    parser.add_argument('--output',type=Path)
    parser.add_argument('--self-test',action='store_true')
    args = parser.parse_args()
    if args.self_test:
        suite = unittest.defaultTestLoader.discover(str(HERE),pattern='test_*.py')
        return 0 if unittest.TextTestRunner(verbosity=2).run(suite).wasSuccessful() else 1
    if not args.panel or not args.output:
        parser.error('--panel and --output are required')
    for sig in (signal.SIGINT,signal.SIGTERM):
        signal.signal(sig,measurement.request_stop)
    try:
        result=run_panel(args.panel,args.output.resolve())
        print(json.dumps({'completed':result['completed'],'arms':list(result['arms'])}))
        return 0
    except (ValueError,RuntimeError,OSError) as error:
        print('incomplete: '+str(error),file=sys.stderr)
        return 1

if __name__=='__main__':
    raise SystemExit(main())
