from pathlib import Path
import signal
import sys

root = Path.cwd()
here = root / 'tools/digest/experiments/the-counterpart'
sys.path.insert(0, str(here))
import run

parent = Path(sys.argv[1]).resolve()
run.git_audit_directory = parent / 'preparation-git'
for sig in (signal.SIGINT, signal.SIGTERM):
    signal.signal(sig, run.measurement.request_stop)
chamber = run.git(root, 'rev-parse', 'HEAD')
(parent / 'chamber-source.sha').write_text(chamber + '\n')
panel = run.load_json((here / 'panel.json').read_text())
source = panel['arms']['base']
checkout = parent / 'preparation-checkout'
prerequisites = run.reconstruct(root, here / panel['bundle'], checkout,
                               panel['base'], source)
before = run.input_hashes(checkout)
run.persist(parent / 'preparation-before.json', {
    'chamber_source': chamber, 'source': {k: source[k] for k in ('commit', 'tree')},
    'inputs': before, 'bundle_prerequisites': prerequisites,
})
if before != source['inputs']:
    raise ValueError('preparation inputs differ from frozen base')
sample = run.capture(
    ['cargo', 'fetch', '--locked', '--manifest-path', 'tools/digest/Cargo.toml'],
    checkout, parent / 'dependency-fetch.json',
    attribution={'role': 'dependency-preparation',
                 'capture_context': {'arm': 'base',
                                     'source': {k: source[k] for k in ('commit', 'tree')}}},
)
after = run.input_hashes(checkout)
run.persist(parent / 'preparation-after.json', {
    'source': {'commit': run.git(checkout, 'rev-parse', 'HEAD'),
               'tree': run.git(checkout, 'rev-parse', 'HEAD^{tree}')},
    'inputs': after, 'fetch_exit_code': sample['exit_code'],
})
run.verify_source(checkout, source)
if after != before:
    raise ValueError('dependency preparation changed frozen source or locks')
run.validate_sample(sample)
print('counterpart: locked dependency preparation completed; offline assay follows')
