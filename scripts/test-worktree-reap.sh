#!/usr/bin/env bash
# scripts/test-worktree-reap.sh — the reaper's population and its refusals.
#
# THIS TOOL DELETES WORKTREES, so the cases that matter most are the ones that
# must NOT fire. Case 5 is the load-bearing one in the other direction: it
# proves the reaper still reaps a worktree whose only dirt is the two files
# `gate-run` writes on every green gate. Without that exclusion the reaper
# refuses every finished campaign and reports success having done nothing —
# the exact defect The Sexton found in `worktree-take`, where 9 of 11 stale
# members were blocked by one machine-written file.
#
# NOTHING HERE TOUCHES THE REAL POOLS: every case runs in a throwaway repo
# under `mktemp -d`, and the reaper is pointed at it by cwd.
set -uo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
reap="$repo_root/scripts/worktree-reap.sh"

pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }

tmp="$(mktemp -d "${TMPDIR:-/tmp}/hv-reap.XXXXXX")"
trap 'rm -rf "$tmp"' EXIT
tmp="$(cd "$tmp" && pwd -P)"   # macOS $TMPDIR is a symlink; git reports the real path

d="$tmp/w"; mkdir -p "$d/outside"
git init --quiet "$d/repo"
git -C "$d/repo" config user.email t@e.st
git -C "$d/repo" config user.name Test
mkdir -p "$d/repo/docs/timings"
printf 'row\n' > "$d/repo/docs/timings.md"
printf 'base\n' > "$d/repo/docs/timings/test-baseline-somehost.tsv"
printf 'x\n' > "$d/repo/f"
git -C "$d/repo" add -A
git -C "$d/repo" commit --quiet -m init
git -C "$d/repo" branch -M main
# `origin/main` is what the reaper judges against; fake it locally.
git -C "$d/repo" update-ref refs/remotes/origin/main refs/heads/main

mkdir -p "$d/repo/.claude/worktrees"
git -C "$d/repo" worktree add --quiet "$d/repo/.claude/worktrees/merged-in"  -b merged-in  main
git -C "$d/repo" worktree add --quiet "$d/outside/merged-out"                -b merged-out main
git -C "$d/repo" worktree add --quiet "$d/outside/dirty"                     -b dirty-br   main
git -C "$d/repo" worktree add --quiet "$d/outside/timings-only"              -b timings-br main
git -C "$d/repo" worktree add --quiet "$d/outside/unmerged"                  -b unmerged   main
# Make `unmerged` genuinely unmerged: a commit origin/main does not have.
printf 'new\n' > "$d/outside/unmerged/g"
git -C "$d/outside/unmerged" add g
git -C "$d/outside/unmerged" commit --quiet -m ahead
# Real dirt, and dirt that is ONLY the two machine-written files.
printf 'edit\n' >> "$d/outside/dirty/f"
printf '| a row |\n' >> "$d/outside/timings-only/docs/timings.md"
printf 'newbase\n'   >> "$d/outside/timings-only/docs/timings/test-baseline-somehost.tsv"

out="$(cd "$d/repo" && bash "$reap" 2>&1)"

# 1/2 — BOTH POOLS. The dot-directory pool and an out-of-repo path; the second
# is the entire reason this tool exists.
if grep -qF "WOULD REAP (merged-in)" <<<"$out"; then
    ok "merged worktree in the dot-pool is reapable"
else
    bad "dot-pool worktree not reapable"
fi
if grep -qF "WOULD REAP (merged-out)" <<<"$out"; then
    ok "merged worktree OUTSIDE the repo is reapable"
else
    bad "out-of-repo worktree not reapable"
fi

# 3/4 — the refusals.
if grep -qF "skip (unmerged: unmerged)" <<<"$out"; then
    ok "unmerged worktree is skipped"
else
    bad "unmerged worktree not skipped"
fi
if grep -q "skip (dirty).*outside/dirty" <<<"$out"; then
    ok "dirty worktree is skipped"
else
    bad "dirty worktree not skipped"
fi

# 5 — THE LOAD-BEARING ONE. Dirty with ONLY the two gate-written files, which
# is the normal end state of a finished campaign. Must still reap, and must
# print the timings rows it would destroy.
if grep -qF "WOULD REAP (timings-br)" <<<"$out"; then
    ok "worktree dirty only with gate-written files is still reapable"
else
    bad "gate-written dirt blocks reaping — the reaper would reap nothing"
fi
if grep -qF "| a row |" <<<"$out"; then
    ok "the docs/timings.md rows it would destroy are printed"
else
    bad "destroyed timings rows are not printed"
fi

# 6 — the main checkout is never a candidate.
if grep -qF "WOULD REAP" <<<"$(grep -F "$d/repo " <<<"$out")"; then
    bad "main checkout appeared as a reap candidate"
else
    ok "main checkout is never reaped"
fi

# 7 — a dry run removes nothing. Asserted on the filesystem, not the wording.
if [ -d "$d/outside/merged-out" ] && [ -d "$d/repo/.claude/worktrees/merged-in" ]; then
    ok "dry run removed nothing"
else
    bad "dry run REMOVED a worktree"
fi

# 8 — and --apply actually removes, or every refusal above proves nothing.
out2="$(cd "$d/repo" && bash "$reap" --apply 2>&1)"
if [ ! -d "$d/outside/merged-out" ] && [ -d "$d/outside/unmerged" ]; then
    ok "--apply removes the merged ones and leaves the unmerged"
else
    bad "--apply did not remove merged / removed unmerged"
fi
if grep -qF "reaped 3, skipped" <<<"$out2"; then
    ok "--apply reports the count it actually reaped"
else
    bad "--apply count wrong: $(tail -1 <<<"$out2")"
fi

printf 'test-worktree-reap: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
