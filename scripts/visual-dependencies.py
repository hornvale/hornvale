#!/usr/bin/env python3
"""Check all resolved normal/build/dev edges, including aliases and features.

Package IDs identify graph nodes; local manifest paths identify ownership.
No --filter-platform or dependency-kind filter may narrow this guard.
"""
from collections import deque
import json
from pathlib import Path
import subprocess
import sys


def check(metadata, root):
    """Return forbidden paths, or raise ValueError for incomplete metadata."""
    root = root.resolve()
    try:
        packages = {p['id']: p for p in metadata['packages']}
        nodes = {n['id']: n for n in metadata['resolve']['nodes']}
        if len(packages) != len(metadata['packages']) or set(nodes) != set(packages):
            raise ValueError('package IDs and resolved nodes must match uniquely')
        if len(nodes) != len(metadata['resolve']['nodes']):
            raise ValueError('duplicate resolved node')
        if Path(metadata['workspace_root']).resolve() != root / 'clients/visual':
            raise ValueError('unexpected visual workspace root')
        ids = {}
        for role, name in [('source', 'hornvale-visual-source'), ('bevy', 'hornvale-bevy-view'),
                           ('planetarium', 'planetarium')]:
            matches = [p['id'] for p in packages.values()
                       if Path(p['manifest_path']).resolve() == root / 'clients/visual' / role / 'Cargo.toml'
                       and p['name'] == name]
            if len(matches) != 1:
                raise ValueError(f'missing or ambiguous {role} package')
            ids[role] = matches[0]
        if set(metadata['workspace_members']) != set(ids.values()):
            raise ValueError('visual workspace must contain exactly source, view and application')
        edges = {}
        for ident, node in nodes.items():
            edges[ident] = []
            for dep in node['deps']:
                if dep['pkg'] not in nodes or not dep['dep_kinds']:
                    raise ValueError(f'unresolved edge from {ident}')
                for kind in dep['dep_kinds']:
                    if kind['kind'] not in (None, 'dev', 'build'):
                        raise ValueError(f'unknown dependency kind from {ident}')
                edges[ident].append(dep['pkg'])
        for package in packages.values():
            if not isinstance(package['name'], str) or not Path(package['manifest_path']).is_absolute():
                raise ValueError('invalid package identity')
    except (KeyError, TypeError, AttributeError) as error:
        raise ValueError(f'incomplete Cargo metadata: {error}') from error

    def forbidden(role, ident):
        package = packages[ident]
        if ident == ids['planetarium']:
            return True
        if role == 'source':
            return ident == ids['bevy'] or package['name'] == 'bevy' or package['name'].startswith('bevy_')
        path = Path(package['manifest_path']).resolve()
        return ident == ids['source'] or any(
            path.is_relative_to(root / layer) for layer in ('kernel', 'domains', 'windows', 'cli'))

    failures = []
    for role in ('source', 'bevy'):
        pending = deque([(ids[role], [ids[role]])])
        seen = {ids[role]}
        while pending:
            current, path = pending.popleft()
            for target in edges[current]:
                if target in seen:
                    continue
                seen.add(target)
                chain = path + [target]
                if forbidden(role, target):
                    failures.append(' -> '.join(packages[p]['name'] for p in chain))
                pending.append((target, chain))
    return failures


def main():
    root = Path(__file__).resolve().parent.parent
    try:
        result = subprocess.run(
            ['cargo', '+1.96.1', 'metadata', '--format-version', '1', '--locked',
             '--all-features', '--manifest-path', str(root / 'clients/visual/Cargo.toml')],
            cwd=root, check=True, stdout=subprocess.PIPE, text=True)
        failures = check(json.loads(result.stdout), root)
        if failures:
            raise ValueError('forbidden reachability:\n  ' + '\n  '.join(failures))
    except (OSError, subprocess.CalledProcessError, ValueError) as error:
        print(f'FAIL: visual dependencies: {error}', file=sys.stderr)
        return 1
    print('ok: visual dependency boundaries (all features; normal/build/dev edges)')
    return 0


if __name__ == '__main__':
    sys.exit(main())
