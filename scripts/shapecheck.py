#!/usr/bin/env python3
"""Compare the SHAPE of two JSON documents: the set of key paths, ignoring values.

The question this answers is the one a drifted byte-golden's failure message
asks — "did the wire shape move, or only a value?" — and it is not answerable
by eyeballing a 67 KB single-line diff, nor by `git diff`, which sees bytes and
has no opinion about structure. An identical path set means every field that
existed still exists and nothing was added, renamed, or retyped at the
container level. Values may differ freely; that is the point.

Why it exists: deciding whether a golden's drift is an EPOCH (a save-format
change, per CLAUDE.md) or merely a moved value is a deliberate human call, and
the repo asks it every time a golden refuses. This makes the structural half of
that call mechanical. The Rhumb used it to establish that `vessel/session/v2`
had 133 key paths on both sides with 1 of 888 leaf values differing
(`.narration.prose`), so the schema had not moved and `v2` stayed `v2`.

Dependency-free by design: standard library only, no venv, no install.

Usage: scripts/shapecheck.py OLD.json NEW.json
Exit 0 if the shapes are identical, 1 if they differ (and prints how),
2 on a usage or parse error.
"""

import json
import sys


def paths(node, prefix=""):
    """Every key path in `node`, with a type tag at each leaf.

    List indices collapse to `[]` deliberately: a chart with more cells is not
    a shape change, but a cell that gained a field is.
    """
    out = set()
    if isinstance(node, dict):
        for key, value in node.items():
            out |= paths(value, f"{prefix}.{key}")
    elif isinstance(node, list):
        out.add(f"{prefix}[] <list>")
        for item in node:
            out |= paths(item, f"{prefix}[]")
    else:
        out.add(f"{prefix} <{type(node).__name__}>")
    return out


def load(path):
    """Parse a file that is either one JSON document or several joined by newlines."""
    with open(path, encoding="utf-8") as handle:
        text = handle.read()
    try:
        return [json.loads(text)]
    except json.JSONDecodeError:
        docs = []
        for line in text.split("\n"):
            line = line.strip()
            if line:
                docs.append(json.loads(line))
        if not docs:
            raise
        return docs


def main(argv):
    """Compare two files; return the process exit status."""
    if len(argv) != 3:
        print(f"usage: {argv[0]} OLD.json NEW.json", file=sys.stderr)
        return 2

    try:
        old_docs, new_docs = load(argv[1]), load(argv[2])
    except (OSError, json.JSONDecodeError) as err:
        print(f"shapecheck: {err}", file=sys.stderr)
        return 2

    if len(old_docs) != len(new_docs):
        print(f"DOC COUNT MOVED: {len(old_docs)} -> {len(new_docs)}")
        return 1

    old, new = set(), set()
    for old_doc, new_doc in zip(old_docs, new_docs):
        old |= paths(old_doc)
        new |= paths(new_doc)

    removed, added = old - new, new - old
    if not removed and not added:
        print(f"SHAPE IDENTICAL — {len(old)} key paths, {len(old_docs)} document(s)")
        return 0

    print(f"SHAPE MOVED — {len(removed)} removed, {len(added)} added")
    for path in sorted(removed):
        print(f"  - {path}")
    for path in sorted(added):
        print(f"  + {path}")
    return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
