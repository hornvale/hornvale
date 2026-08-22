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
#
# EXIT 4 (out-of-band) OUTRANKS EXIT 3 (already merged) when both conditions
# hold at once — see the out-of-band check below, which runs BEFORE the
# ancestor check for exactly this reason: an out-of-band landing means the
# queue's inductive guarantee is broken, full stop, whether or not THIS
# particular candidate happens to already be included. Returning 3 in that
# case would read as "nothing to do" and let a caller silently skip past a
# broken invariant that may still be hiding an unmerged candidate behind the
# same base movement — a human must see it regardless.
#
# HERMETICITY: git exports GIT_DIR and GIT_INDEX_FILE to hooks, and they
# OUTRANK cwd/`-C` — from a linked worktree (where all campaign work
# happens) they are absolute paths into a DIFFERENT repository
# (scripts/CLAUDE.md's board incident: an unscrubbed `git -C <tempdir>`
# re-initialised a developer's real checkout as bare). This script is
# invoked by other machinery, so every git call below is scrubbed with
# `env -u GIT_DIR -u GIT_INDEX_FILE`, the same discipline
# scripts/sluice-queue.sh:58,121 uses for its own git calls.
set -euo pipefail
branch="${1:?usage: sluice-mouth.sh <branch> <sha>}"
sha="${2:?usage: sluice-mouth.sh <branch> <sha>}"

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
last_pushed_file="$HV_SLUICE_DIR/last-pushed"

# Format first, existence second: shape validation is free (no git call) and
# gives a more specific reason than "not a commit" when a caller passes
# something that could never be a real SHA regardless of repository state.
# A full 40-char SHA, for the reason lane-dispatch.sh gives: a ref feeds
# checkout/reset on the far end and can land on a stale local branch of that
# name. Every character must be hex, not just the first — the same shape
# sluice-queue.sh's validate_sha rejects, tightened to an exact 40 here.
case "$sha" in
    *[!0-9a-f]*|"") echo "sluice-mouth: REF must be a full 40-char SHA (hex only); got '$sha'" >&2; exit 2 ;;
esac
[ "${#sha}" -eq 40 ] || { echo "sluice-mouth: REF must be a full 40-char SHA; got '$sha'" >&2; exit 2; }

env -u GIT_DIR -u GIT_INDEX_FILE git rev-parse --verify --quiet "$sha^{commit}" >/dev/null \
    || { echo "sluice-mouth: '$sha' is not a commit in this repository" >&2; exit 2; }

if [ -z "${HV_SLUICE_ALLOW_UNPUSHED:-}" ]; then
    if [ -z "$(env -u GIT_DIR -u GIT_INDEX_FILE git branch -r --contains "$sha" 2>/dev/null)" ]; then
        echo "sluice-mouth: $sha is not on any remote branch — push first." >&2
        exit 2
    fi
fi

base="${HV_SLUICE_BASE:-origin/main}"

# Resolve and verify the base ref BEFORE either git call that depends on it
# (the out-of-band check below, the ancestor check, and merge-tree itself).
# A base that does not resolve is an infrastructure fault, not a property of
# the candidate — see the header note.
if ! env -u GIT_DIR -u GIT_INDEX_FILE git rev-parse --verify --quiet "$base^{commit}" >/dev/null; then
    echo "sluice-mouth: base ref '$base' does not resolve to a commit — cannot evaluate this candidate." >&2
    echo "  this is an infrastructure fault, not a conflict in $sha." >&2
    exit 2
fi

# An out-of-band landing breaks the queue's inductive guarantee: each merge
# builds on an already-proven main, so anything that lands another way makes
# every later green weaker than it advertises. Detect it loudly; never resume
# quietly. Runs BEFORE the ancestor check on purpose — see the header note on
# the 3-vs-4 ordering.
if [ -f "$last_pushed_file" ]; then
    expected="$(cat "$last_pushed_file")"
    actual="$(env -u GIT_DIR -u GIT_INDEX_FILE git rev-parse "$base")"
    if [ "$expected" != "$actual" ]; then
        echo "sluice-mouth: OUT-OF-BAND LANDING on $base." >&2
        echo "  the queue last pushed: $expected" >&2
        echo "  $base is now:          $actual" >&2
        echo "  Something landed outside the queue. The inductive guarantee is broken" >&2
        echo "  until a human decides what happened." >&2
        exit 4
    fi
fi

if env -u GIT_DIR -u GIT_INDEX_FILE git merge-base --is-ancestor "$sha" "$base" 2>/dev/null; then
    echo "sluice-mouth: $sha is already an ancestor of $base — nothing to merge." >&2
    exit 3
fi

if ! out="$(env -u GIT_DIR -u GIT_INDEX_FILE git merge-tree --write-tree --name-only "$base" "$sha" 2>&1)"; then
    echo "sluice-mouth: MERGE CONFLICT between $base and $sha." >&2
    # THE CONFLICT LIST IS FOUND BY SHAPE, NOT BY POSITION, AND THAT MATTERS
    # HERE MORE THAN IT USUALLY WOULD.
    #
    # `git merge-tree` prints the new tree's OID, then the conflicting paths,
    # then a blank line, then git's own informational messages. The obvious
    # reader is `tail -n +2` — drop line 1, take lines up to the blank — and
    # that is what this was. It assumes the OID is on line 1.
    #
    # IT IS NOT, IN THIS REPOSITORY. `.gitattributes` routes six generated
    # documents through `merge=hv-regenerate` (scripts/merge-regenerate.sh),
    # which runs cargo and type-audit and writes to stdout/stderr — and this
    # capture takes `2>&1`. So a real merge-tree run here begins with lines
    # like "Running `tools/type-audit/target/debug/type-audit report`", and
    # `tail -n +2` then reports the DRIVER'S OUTPUT and the tree OID itself as
    # conflicting paths. Observed live on campaign/the-burr, 2026-08-19: the
    # bounce listed a cargo banner and a bare 40-hex OID among four real
    # paths, and a reader hunting `62e4699797fb...` as a file would have found
    # nothing.
    #
    # THAT PARTICULAR POLLUTER IS GONE, AND THE SHAPE-BASED READ STAYS. The
    # `merge=hv-regenerate` driver was retired by decision 0160, so merge-tree
    # here no longer emits cargo banners. Keeping the positional `tail -n +2`
    # would still be wrong: `2>&1` means ANY future tool that writes to stderr
    # during a merge-tree reintroduces the identical bug, and the observed
    # failure — a bare 40-hex OID reported as a conflicting path — came from
    # trusting line position, not from the driver specifically. A read that
    # locates its anchor by shape cannot be broken by a new talker upstream.
    #
    # So: locate the OID line by its shape, print what follows until the blank.
    # If no OID line appears at all — a failure that produced no tree — fall
    # back to the raw output rather than printing nothing, because an empty
    # conflict list under a "MERGE CONFLICT" header is worse than noise.
    #
    # `length($0) == 40 && /^[0-9a-f]+$/`, NOT `/^[0-9a-f]{40}$/`. On this box
    # `awk` is mawk, which does not implement POSIX interval expressions, so
    # the `{40}` form matches NOTHING and silently reports an empty conflict
    # list — the same shape as the `git grep -E \b` trap already recorded on
    # the board. Caught here only because the fallback below printed raw
    # output instead of nothing.
    conflicts="$(printf '%s\n' "$out" \
        | awk 'length($0) == 40 && /^[0-9a-f]+$/ { seen = 1; next } seen && /^$/ { exit } seen { print }')"
    if [ -n "$conflicts" ]; then
        printf '%s\n' "$conflicts" | sed 's/^/  conflict: /' >&2
    else
        echo "  (no tree OID in merge-tree output; raw follows)" >&2
        printf '%s\n' "$out" | sed 's/^/  | /' >&2
    fi
    exit 1
fi

merge_base_sha="$(env -u GIT_DIR -u GIT_INDEX_FILE git merge-base "$base" "$sha")"
behind="$(env -u GIT_DIR -u GIT_INDEX_FILE git rev-list --count "$merge_base_sha".."$base")"
echo "sluice-mouth: ADMIT $branch $sha (merge base is $behind commits behind $base)"
exit 0
