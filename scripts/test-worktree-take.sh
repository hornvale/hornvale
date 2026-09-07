#!/usr/bin/env bash
# scripts/test-worktree-take.sh — the pool's RECYCLABILITY predicate.
#
# WHY THIS EXISTS. `worktree-take.sh` recycles a pool member whose branch is
# merged and whose tree is clean, and it excluded exactly one machine-written
# file from "clean": `docs/timings/test-baseline-<host>.tsv`. It did not
# exclude `docs/timings.md`, which `gate-run` APPENDS a row to on every green
# local gate — including the last gate a campaign runs after its final commit,
# whose row nobody is left to commit.
#
# So the normal end state of a finished campaign was a worktree carrying one
# uncommitted timings row, which made it permanently unrecyclable. The pool
# then degraded to cold creation silently: every `make worktree-take` printed
# "no recyclable member", created a fresh worktree, and paid the full cold
# build the pool exists to avoid. Measured 2026-09-06: 33 live worktrees
# against a design that assumed ~3, ~700 GB, and 9 of the 11 stale merged
# members were blocked by this one file and nothing else.
#
# THE ASYMMETRY THIS FILE PINS. `test-baseline-*.tsv` is REWRITTEN by the next
# green gate, so discarding it loses nothing. `docs/timings.md` is APPEND-ONLY
# history: discarding a row destroys a measurement permanently. The rows are
# individually negligible against a ledger of thousands and were already being
# lost — just slowly, when someone eventually deleted the worktree — but
# "negligible" is not "nothing", so the discard must be LOUD. Test 3 is the
# one that holds that distinction; without it the fix is indistinguishable
# from a silent widening of the exclusion.
#
# NOTHING HERE TOUCHES THE REAL POOL. Every case runs against a throwaway
# repo in a temp dir, with its own bare `origin`.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }

tmp="$(mktemp -d "${TMPDIR:-/tmp}/hv-wt-take.XXXXXX")"
trap 'rm -rf "$tmp"' EXIT

# Build: a bare origin, a main checkout, and one pool member on a MERGED
# branch. `mkfixture <dirt-path> <dirt-content>` returns a fresh scenario so
# each case is independent.
mkfixture() {
    local name="$1" dirt="${2:-}" ; local d="$tmp/$name"
    rm -rf "$d"; mkdir -p "$d"
    git init --quiet --bare "$d/origin.git"
    git clone --quiet "$d/origin.git" "$d/repo" 2>/dev/null
    git -C "$d/repo" config user.email t@e.st
    git -C "$d/repo" config user.name Test
    mkdir -p "$d/repo/docs/timings" "$d/repo/src"
    printf 'row\n'  > "$d/repo/docs/timings.md"
    printf 'base\n' > "$d/repo/docs/timings/test-baseline-somehost.tsv"
    printf 'fn f(){}\n' > "$d/repo/src/lib.rs"
    git -C "$d/repo" add -A
    git -C "$d/repo" commit --quiet -m init
    git -C "$d/repo" branch -M main
    git -C "$d/repo" push --quiet -u origin main
    # A pool member on a branch that IS merged into main.
    mkdir -p "$d/repo/.claude/worktrees"
    git -C "$d/repo" worktree add --quiet "$d/repo/.claude/worktrees/done" -b campaign/done main
    # EVERY member is dirty in the baseline tsv, because every green gate
    # writes it — that is the file the original exclusion was built for, and a
    # finished campaign always has it. Making it case-specific instead left the
    # pre-existing baseline discard uncovered: a mutation deleting that line
    # survived, since a fixture with a clean baseline gives its `git checkout`
    # nothing to do. The case-specific dirt is layered on top.
    printf 'local base\n' >> "$d/repo/.claude/worktrees/done/docs/timings/test-baseline-somehost.tsv"
    [ -n "$dirt" ] && printf 'dirty\n' >> "$d/repo/.claude/worktrees/done/$dirt"
    # NOW ADVANCE MAIN'S OWN COPY OF BOTH MACHINE-WRITTEN FILES, and this is
    # the load-bearing half of the fixture rather than set dressing. Without
    # it, `git switch -c ... origin/main` finds the target content identical
    # to HEAD, carries the local modification across without complaint, and
    # the discard below it is never exercised at all — a mutation deleting
    # the discard outright SURVIVED this file until this was added, with all
    # assertions green. In production main is the busiest file in the repo:
    # every green gate anywhere appends a row, so the target content always
    # differs and the switch always refuses. The fixture has to reproduce the
    # state production is actually in, or it tests a branch nothing takes.
    printf 'later row\n' >> "$d/repo/docs/timings.md"
    printf 'later base\n' >> "$d/repo/docs/timings/test-baseline-somehost.tsv"
    git -C "$d/repo" commit --quiet -am "main moves on"
    git -C "$d/repo" push --quiet origin main
    echo "$d/repo"
}

take() { ( cd "$1" && NAME=next BASE=main bash "$repo_root/scripts/worktree-take.sh" 2>&1 ); }

echo "== a member dirtied ONLY by an uncommitted docs/timings.md row is recyclable"
r="$(mkfixture timings docs/timings.md)"
out="$(take "$r" || true)"
if [ -d "$r/.claude/worktrees/next" ] && [ ! -d "$r/.claude/worktrees/done" ]; then
    ok "the member was recycled into the new name"
else
    bad "not recycled; worktree-take said: $out"
fi
case "$out" in
    *"no recyclable member"*) bad "fell through to a COLD worktree — the pool degraded silently" ;;
    *) ok "no cold-creation fallback" ;;
esac

echo "== the discarded timings row is REPORTED, never silently dropped"
r="$(mkfixture loud docs/timings.md)"
out="$(take "$r" || true)"
case "$out" in
    *timings.md*) ok "the discard names docs/timings.md on stderr" ;;
    *) bad "the row was discarded silently: $out" ;;
esac

echo "== genuine work still blocks recycling (the exclusion did not widen)"
r="$(mkfixture real src/lib.rs)"
out="$(take "$r" || true)"
case "$out" in
    *"working tree is dirty"*) ok "a dirtied source file still refuses the member" ;;
    *) bad "a real edit was recycled over — the exclusion is too wide: $out" ;;
esac
if [ -d "$r/.claude/worktrees/done" ]; then
    ok "the member with real work survives untouched"
else
    bad "the member with real work was consumed"
fi

echo
printf '%s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
