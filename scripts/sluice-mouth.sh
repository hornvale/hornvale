#!/usr/bin/env bash
# scripts/sluice-mouth.sh — the checks that run OUTSIDE the lane claim.
#
# THE CANAL-LOCK RULE: turn a vessel away at the gate, never inside the
# chamber. A candidate that cannot merge must not consume a strictly serial
# resource whose mean queue wait, measured over the lane's first 46 jobs, is
# 903-1823 s per job. Everything here is read-only and takes no lock.
#
# EXIT CODES: 0 admit / 1 conflict / 2 invalid or unpushed / 3 already merged
# / 4 an out-of-band landing on main.
#
# EXIT 1 IS RESERVED FOR A REAL CONFLICT, NOT A BROKEN BASE REF. Verified on
# lefford (git 2.39.5): `git merge-tree --write-tree` against a base ref that
# does not resolve ("origin/main - not something we can merge") exits 1,
# identical to a genuine conflict — and `git merge-base --is-ancestor` against
# the same broken ref exits 128, not caught by the merge-tree branch at all.
# The two failures mean opposite things to the queue: a conflict bounces the
# candidate back to its campaign ("go absorb main"); a base ref that will not
# resolve is an infrastructure fault the campaign did not cause. So the base
# ref is resolved and verified ONCE, before either git call that depends on
# it, and a failure there exits 2 — never 1.
set -euo pipefail
branch="${1:?usage: sluice-mouth.sh <branch> <sha>}"
sha="${2:?usage: sluice-mouth.sh <branch> <sha>}"

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
last_pushed_file="$HV_SLUICE_DIR/last-pushed"

git rev-parse --verify --quiet "$sha^{commit}" >/dev/null \
    || { echo "sluice-mouth: '$sha' is not a commit in this repository" >&2; exit 2; }

# A full 40-char SHA, for the reason lane-dispatch.sh gives: a ref feeds
# checkout/reset on the far end and can land on a stale local branch of that
# name.
case "$sha" in
    [0-9a-f]*) [ "${#sha}" -eq 40 ] || { echo "sluice-mouth: REF must be a full 40-char SHA; got '$sha'" >&2; exit 2; } ;;
    *) echo "sluice-mouth: REF must be a full 40-char SHA; got '$sha'" >&2; exit 2 ;;
esac

if [ -z "${HV_SLUICE_ALLOW_UNPUSHED:-}" ]; then
    if [ -z "$(git branch -r --contains "$sha" 2>/dev/null)" ]; then
        echo "sluice-mouth: $sha is not on any remote branch — push first." >&2
        exit 2
    fi
fi

base="${HV_SLUICE_BASE:-origin/main}"

# Resolve and verify the base ref BEFORE either git call that depends on it
# (the out-of-band check below, the ancestor check, and merge-tree itself).
# A base that does not resolve is an infrastructure fault, not a property of
# the candidate — see the header note.
if ! git rev-parse --verify --quiet "$base^{commit}" >/dev/null; then
    echo "sluice-mouth: base ref '$base' does not resolve to a commit — cannot evaluate this candidate." >&2
    echo "  this is an infrastructure fault, not a conflict in $sha." >&2
    exit 2
fi

# An out-of-band landing breaks the queue's inductive guarantee: each merge
# builds on an already-proven main, so anything that lands another way makes
# every later green weaker than it advertises. Detect it loudly; never resume
# quietly.
if [ -f "$last_pushed_file" ]; then
    expected="$(cat "$last_pushed_file")"
    actual="$(git rev-parse "$base")"
    if [ "$expected" != "$actual" ]; then
        echo "sluice-mouth: OUT-OF-BAND LANDING on $base." >&2
        echo "  the queue last pushed: $expected" >&2
        echo "  $base is now:          $actual" >&2
        echo "  Something landed outside the queue. The inductive guarantee is broken" >&2
        echo "  until a human decides what happened." >&2
        exit 4
    fi
fi

if git merge-base --is-ancestor "$sha" "$base" 2>/dev/null; then
    echo "sluice-mouth: $sha is already an ancestor of $base — nothing to merge." >&2
    exit 3
fi

if ! out="$(git merge-tree --write-tree --name-only "$base" "$sha" 2>&1)"; then
    echo "sluice-mouth: MERGE CONFLICT between $base and $sha." >&2
    # Line 1 is the tree SHA; the conflicting paths follow, then a blank line
    # and git's own informational messages.
    printf '%s\n' "$out" | tail -n +2 | sed '/^$/q' | sed 's/^/  conflict: /' >&2
    exit 1
fi

behind="$(git rev-list --count "$(git merge-base "$base" "$sha")".."$base")"
echo "sluice-mouth: ADMIT $branch $sha (merge base is $behind commits behind $base)"
exit 0
