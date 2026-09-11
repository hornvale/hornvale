#!/usr/bin/env bash
# scripts/worktree-reap.sh — remove worktrees whose branch is already merged,
# ACROSS EVERY POOL. Dry-run unless given --apply.
#
# WHY THIS EXISTS. `worktree-take.sh` recycles a pool member, but it filters
# candidates to the pool it owns (`case "$wt" in "$POOL"/*`), so nothing ever
# reached the worktrees created by the global superpowers convention at
# `~/.config/superpowers/worktrees/<project>/`. Measured 2026-09-10: 29 there,
# 11 on merged branches, ~161 GB, outside any recycling OR reaping mechanism.
# A campaign taking a worktree there always paid the cold build (a measured
# 771 s) and always left the corpse. `make sweep` reclaims their dead
# GENERATIONS, which is a smaller and different thing than reclaiming them.
#
# THE POPULATION COMES FROM `git worktree list`, NEVER A PATH PREFIX. That is
# the same rule `scripts/sweep-roots.sh` states and for the same reason: a
# prefix knows only the pools that existed when it was written, and this repo
# grew a second one without anybody noticing. Git knows every worktree of this
# repo wherever it lives, and knows nothing about other projects sharing that
# directory — scoping we want and would otherwise hand-maintain.
#
# THE TWO EXCLUSIONS ARE LOAD-BEARING AND ARE NOT STYLISTIC. `gate-run` writes
# `docs/timings.md` and `docs/timings/test-baseline-<host>.tsv` on every green
# local gate, including the last gate a campaign runs after its final commit,
# whose row nobody is left to commit. So "has run a gate" — the normal end
# state of a finished campaign — makes a worktree dirty by construction. A
# reaper that refuses all dirt would therefore reap NOTHING while reporting
# success, which is precisely the bug The Sexton found in `worktree-take`
# (9 of 11 stale merged members blocked by one file and nothing else).
#
# BECAUSE `docs/timings.md` IS APPEND-ONLY AND NOT REGENERABLE, ITS LOSS IS
# PRINTED. Unlike `worktree-take`, which recycles and leaves the tree in place,
# reaping destroys the worktree — so a dropped measurement is gone for good.
# The dry run shows those rows first, which is the moment to rescue one.
#
# A DETACHED WORKTREE IS NEVER REAPED. Merged-ness here is a question about a
# BRANCH; a detached HEAD has none, so it is reported and skipped rather than
# guessed at.
set -euo pipefail

BASE="${HV_REAP_BASE:-origin/main}"
apply=0
case "${1:-}" in
    --apply) apply=1 ;;
    ""|--dry-run) ;;
    *) echo "usage: worktree-reap.sh [--apply]" >&2; exit 2 ;;
esac

git rev-parse --verify --quiet "$BASE" >/dev/null || {
    echo "worktree-reap: base '$BASE' does not resolve — fetch first" >&2
    exit 2
}

ROOT="$(git worktree list --porcelain | awk '/^worktree /{print $2; exit}')"
reaped=0; skipped=0

# Locked worktrees are excluded in the awk, the same way worktree-take does it:
# a record is emitted only if no `locked` line appeared before its blank-line
# boundary, and the END block catches the last record, which has none.
while IFS= read -r wt; do
    [ -n "$wt" ] || continue
    [ "$wt" != "$ROOT" ] || continue
    [ -d "$wt" ] || continue

    br="$(git -C "$wt" branch --show-current 2>/dev/null || echo '')"
    if [ -z "$br" ]; then
        printf '  skip (detached, no branch to judge)  %s\n' "$wt"
        skipped=$((skipped + 1))
        continue
    fi

    if ! git merge-base --is-ancestor "$br" "$BASE" 2>/dev/null; then
        printf '  skip (unmerged: %s)  %s\n' "$br" "$wt"
        skipped=$((skipped + 1))
        continue
    fi

    dirty="$(git -C "$wt" status --porcelain 2>/dev/null \
        | grep -v 'docs/timings/test-baseline-' \
        | grep -v 'docs/timings\.md' || true)"
    if [ -n "$dirty" ]; then
        printf '  skip (dirty)  %s\n' "$wt"
        skipped=$((skipped + 1))
        continue
    fi

    lost="$(git -C "$wt" diff -- docs/timings.md | grep '^+' | grep -v '^+++' || true)"
    if [ -n "$lost" ]; then
        printf '  NOTE: reaping discards uncommitted docs/timings.md rows from %s:\n' "$wt"
        printf '%s\n' "$lost" | sed 's/^/        /'
    fi

    if [ "$apply" -eq 1 ]; then
        git worktree remove --force "$wt"
        printf '  reaped (%s)  %s\n' "$br" "$wt"
    else
        printf '  WOULD REAP (%s)  %s\n' "$br" "$wt"
    fi
    reaped=$((reaped + 1))
done < <(git worktree list --porcelain | awk '
    /^worktree /  { path = $2; locked = 0; next }
    /^locked/     { locked = 1; next }
    /^$/          { if (path != "" && !locked) print path; path = ""; locked = 0; next }
    END           { if (path != "" && !locked) print path }
')

[ "$apply" -eq 1 ] && git worktree prune
if [ "$apply" -eq 1 ]; then
    printf 'worktree-reap: reaped %d, skipped %d\n' "$reaped" "$skipped"
else
    printf 'worktree-reap: would reap %d, skipped %d (dry run — pass --apply)\n' "$reaped" "$skipped"
fi
