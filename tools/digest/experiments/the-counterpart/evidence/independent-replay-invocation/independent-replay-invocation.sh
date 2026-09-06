#!/usr/bin/env bash
# Independent full12 invocation; run only inside the ordinary claimed diagnostic stage.
set -euo pipefail

counterpart_panel=tools/digest/experiments/the-counterpart/panel-full12.json
counterpart_independent_parent="$(python3 - "$counterpart_panel" "$0" <<'PY'
from pathlib import Path
import os
import signal
import sys
import tempfile

root = Path.cwd().resolve()
panel_path = (root/sys.argv[1]).resolve()
invocation_path = Path(sys.argv[2]).resolve()
sys.path.insert(0, str(panel_path.parent))
import run

package_commit = 'ca4872744fe57801384ff9cd7149fe25ad197ef4'
expected_panel = '4d7588e3f25523358deae71a45c4e8d9529f73c5370511b0dd5b1810d759931c'
expected_bundle = '33ea34e8774bd11e1b237eb6f62d40601838e67d56f7b3b79ecc1eb4578102f8'
state = Path(os.environ.get('XDG_STATE_HOME', str(Path.home()/'.local/state')))/'hornvale'
state.mkdir(parents=True, exist_ok=True)
parent = Path(tempfile.mkdtemp(prefix='counterpart-independent-full12-', dir=state)).resolve()
print('Independent Counterpart retained parent: '+str(parent), file=sys.stderr, flush=True)
run.git_audit_directory = parent/'preparation-git'
for sig in (signal.SIGINT, signal.SIGTERM):
    signal.signal(sig, run.measurement.request_stop)

invocation_bytes = invocation_path.read_bytes()
(parent/'independent-replay-invocation.sh').write_bytes(invocation_bytes)
chamber_sha = run.git(root,'rev-parse','HEAD')
(parent/'invocation-source.sha').write_text(chamber_sha+'\n')
panel_bytes = panel_path.read_bytes()
panel = run.load_json(panel_bytes)
bundle = panel_path.parent/panel['bundle']
run.persist(parent/'invocation-provenance.json', {
    'role':'independent second full12 behavioral replay',
    'package_commit':package_commit, 'actual_chamber_sha':chamber_sha,
    'invocation_sha256':run.sha256(invocation_bytes),
    'panel_sha256':run.sha256(panel_bytes),
    'bundle_sha256':run.sha256(bundle.read_bytes()),
    'source_only_receipt_sha256':'3e12b5271d427dbfe6c93556c30f2a8246850fc8acf96126e90f107791acba8d',
    'command':['python3','tools/digest/experiments/the-counterpart/run.py','--panel',sys.argv[1],'--output',str(parent/'dossier')],
    'cwd':str(root),
    'cache_disclosure':'Shared canonical machine Cargo registry cache; separate new preparation checkout, dossier checkout and target. Timing is not a cold benchmark.',
    'independence_disclosure':'Committed panel/owner inputs include predictions and observations, never result authority. Independent from originating scratch/execution, not blinded outcome adjudication. No primary-run checkout or target is read or reused.',
})
if run.sha256(panel_bytes) != expected_panel:
    raise ValueError('unexpected full12 panel identity')
if run.sha256(bundle.read_bytes()) != expected_bundle or bundle.stat().st_size != 9823:
    raise ValueError('unexpected full12 bundle identity/size')
if len(panel['arms']) != 12:
    raise ValueError('full12 arm roster missing')
relative_panel = panel_path.relative_to(root).as_posix()
if run.git_bytes(root,'show',package_commit+':'+relative_panel) != panel_bytes:
    raise ValueError('panel differs from committed package')
for name in ('run.py','checker.py','compare.py'):
    relative = 'tools/digest/experiments/the-counterpart/'+name
    if run.git_bytes(root,'show',panel['identities']['implementation']+':'+relative) != (root/relative).read_bytes():
        raise ValueError('frozen implementation bytes differ: '+name)
if run.sha256((root/'scripts/charter_measure.py').read_bytes()) != panel['identities']['measure']:
    raise ValueError('frozen supervisor bytes differ')
base = panel['arms']['base']
checkout = parent/'preparation-checkout'
run.reconstruct(root,bundle,checkout,panel['base'],base)
before = run.input_hashes(checkout)
run.persist(parent/'preparation-before.json', {'source':base,'inputs':before})
if before != base['inputs']:
    raise ValueError('preparation source/locks differ from frozen base')
sample = run.capture(
    ['cargo','fetch','--locked','--manifest-path','tools/digest/Cargo.toml'],
    checkout,parent/'dependency-fetch.json',
    attribution={'role':'dependency-preparation',
                 'capture_context':{'arm':'base','source':{k:base[k] for k in ('commit','tree')}}})
after = run.input_hashes(checkout)
run.persist(parent/'preparation-after.json', {'source':base,'inputs':after})
run.verify_source(checkout,base)
if after != before:
    raise ValueError('locked dependency preparation changed source/locks')
run.validate_sample(sample)
print(parent)
PY
)"
printf 'Independent Counterpart retained evidence: %s\n' "$counterpart_independent_parent"
exec python3 tools/digest/experiments/the-counterpart/run.py \
  --panel "$counterpart_panel" \
  --output "$counterpart_independent_parent/dossier"
