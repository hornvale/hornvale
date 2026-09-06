"""Frozen shadow rules; no selector accepts behavioral observations."""
from pathlib import PurePosixPath

RULE_VERSION = 'counterpart-shadow-v1'


def suggest(kind, changed_paths, metadata, owners, contract):
    ids = {q['id'] for q in contract['questions']}
    proposed, unknown, normalization = set(), [], []
    if kind == 'path':
        for path in changed_paths:
            if path.startswith(contract['scope'] + '/'):
                proposed.update(ids)
            else:
                unknown.append('no enrollment: ' + path)
    elif kind == 'cargo':
        graphs = metadata.get('cargo', [])
        if len(graphs) != 2 or any(not g.get('resolve') for g in graphs):
            unknown.append('missing base/changed Cargo graph')
        else:
            matched = set()
            reached_names = set()
            for graph_index, graph in enumerate(graphs):
                packages = {p['id']:p for p in graph['packages']}
                changed = set()
                for path in changed_paths:
                    # Non-source configuration can alter graph/build semantics.
                    if not path.endswith('.rs') and PurePosixPath(path).name != 'Cargo.toml':
                        unknown.append('non-Cargo source input: ' + path)
                        continue
                    candidates = []
                    for ident, package in packages.items():
                        try:
                            directory = PurePosixPath(package['manifest_path']).parent.relative_to(metadata.get('repository_roots', [g['workspace_root'] for g in graphs])[graph_index])
                        except ValueError:
                            continue
                        if PurePosixPath(path).is_relative_to(directory):
                            candidates.append((len(directory.parts), ident))
                    if candidates:
                        changed.add(max(candidates)[1])
                        matched.add(path)
                reached = set(changed)
                while True:
                    expanded = reached | {n['id'] for n in graph['resolve']['nodes'] if set(n['dependencies']) & reached}
                    if expanded == reached:
                        break
                    reached = expanded
                reached_names.update(packages[i]['name'] for i in reached if i in packages)
            unknown.extend('missing Cargo membership: ' + p for p in changed_paths if p not in matched)
            proposed.update(q['id'] for q in contract['questions'] if set(q['packages']) & reached_names)
    elif kind == 'agreement':
        active = set(metadata.get('variants', []))
        found = set()
        for path in changed_paths:
            matching = [o for o in owners if path in o.get('owned_paths', [])]
            if len(matching) != 1:
                unknown.append('missing/ambiguous owner scope: ' + path)
                continue
            owner = matching[0]
            normalization.append({'owner':owner['owner'], 'mode':'post-variant roster and name subjects' if owner['owner']=='settlement' else 'addition deltas and source locators', 'reason':'Explicit finite manual normalization of reviewed owner records; no outcome input.'})
            variants = [v for v in owner.get('variants', []) if v['id'] in active]
            if not variants:
                unknown.append('missing changed subject declaration: ' + path)
            for variant in variants:
                found.add(variant['id'])
                subjects = variant.get('changed_subjects', [])
                named_subjects = set(owner.get('supplies', [])) | set(variant.get('supplies', [])) | set(variant.get('consumes', []))
                allowed_subjects = {'THING_KINDS','thing_registry','BORROWED','register_concepts'} | named_subjects
                if not subjects or not set(subjects) <= allowed_subjects:
                    unknown.append('missing/unknown subject: ' + variant['id'])
                if 'supplies' not in variant or 'consumes' not in variant:
                    unknown.append('missing name delta: ' + variant['id'])
                assumptions = []
                for assumption in owner.get('negative_assumptions', []):
                    first = assumption.split(' ')[0]
                    if first.startswith('unclaimed:'):
                        assumptions.append(first)
                        normalization.append({'owner':owner['owner'], 'field':'negative_assumptions', 'original':assumption, 'normalized':first})
                    elif assumption.startswith(first + ' is not claimed by Thing or any earlier registration'):
                        assumptions.append('unclaimed:' + first)
                        normalization.append({'owner':owner['owner'], 'field':'negative_assumptions', 'original':assumption, 'normalized':'unclaimed:'+first})
                supplied = set(variant.get('supplies', []))
                if owner['owner'] == 'settlement':
                    supplied -= set(owner.get('supplies', []))
                for name in supplied:
                    if 'unclaimed:' + name not in assumptions:
                        unknown.append('missing negative assumption: unclaimed:' + name)
                positive = [a.split(' ')[0] for a in owner.get('positive_assumptions', [])]
                for name in variant.get('consumes', []):
                    if not any(a.startswith('lender:' + name + ':') for a in positive):
                        unknown.append('missing lender assumption: ' + name)
                questions = set(owner.get('affected_questions', []))
                if not questions or not questions <= ids:
                    unknown.append('missing/unknown question mapping: ' + owner['owner'])
                proposed.update(questions & ids)
        unknown.extend('unmapped variant: ' + v for v in sorted(active - found))
    else:
        raise ValueError('unknown selector: ' + kind)
    unknown = sorted(set(unknown))
    return {'kind':kind, 'rules':RULE_VERSION, 'proposed':sorted(proposed), 'unknown':unknown,
            'effective':sorted(ids if unknown else proposed), 'normalization':normalization}


def score(selection, outcomes):
    if set(outcomes) != {'registration','components','borrowing','ownership'}:
        raise ValueError('missing or unknown question outcome')
    if any(v['outcome'] not in {'satisfied','violated','unknown'} for v in outcomes.values()):
        raise ValueError('unknown checker outcome')
    def confusion(selected):
        selected = set(selected)
        if not selected <= outcomes.keys():
            raise ValueError('unknown selected question')
        return {
            'selected_violating':sorted(q for q,v in outcomes.items() if q in selected and v['outcome']=='violated'),
            'selected_satisfied':sorted(q for q,v in outcomes.items() if q in selected and v['outcome']=='satisfied'),
            'unselected_violating':sorted(q for q,v in outcomes.items() if q not in selected and v['outcome']=='violated'),
            'unknown':sorted(q for q,v in outcomes.items() if v['outcome']=='unknown'),
            'selected_unknown':sorted(q for q,v in outcomes.items() if q in selected and v['outcome']=='unknown')}
    return {'raw':confusion(selection['proposed']), 'effective':confusion(selection['effective']),
            'fallback_reasons':list(selection['unknown'])}


def imports_only(changed_paths, metadata, owners, contract):
    """Supplemental counterfactual: declarations with negative scope omitted.

    Raw proposals expose the omission; effective obligations always fall back.
    This is not a fourth primary selector and never skips execution.
    """
    result = suggest('agreement', changed_paths, metadata, owners, contract)
    imported = {name for owner in owners for name in owner.get('consumes', [])}
    proposed = set()
    for owner in owners:
        if not set(changed_paths) & set(owner['owned_paths']):
            continue
        for variant in owner['variants']:
            if variant['id'] not in metadata.get('variants', []):
                continue
            changed = set(variant['changed_subjects'])
            if owner['owner'] == 'thing':
                # Own source obligations remain visible under either view.
                proposed.update(owner['affected_questions'])
            elif changed & imported:
                proposed.update(owner['affected_questions'])
    result.update(kind='agreement-imports-only-supplement', proposed=sorted(proposed),
                  unknown=sorted(set(result['unknown']) | {'negative namespace assumptions intentionally omitted'}),
                  effective=sorted(q['id'] for q in contract['questions']))
    return result
