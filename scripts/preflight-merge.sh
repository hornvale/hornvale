#!/usr/bin/env bash
# scripts/preflight-merge.sh — GO/NO-GO preflight for integrating a campaign
# branch with main.
#
# Mechanizes the checkable half of the parallel-campaign integration ritual
# (parallel sessions have triple-collided on artifacts in two days — see
# decision 0026); prints the judgment half it cannot score as reminders.
# Read-only: never mutates anything.
#
# Run it from the campaign branch (worktree or checkout) you intend to merge.
# Run from main it degrades to checkout hygiene only — useful before any
# commit in the shared checkout, where `git add <file>` has swept another
# session's staged work into a commit before.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

section() { printf '\n== %s\n' "$1"; }

nogo=0
fail() { printf '  NO-GO: %s\n' "$1"; nogo=1; }
ok() { printf '  ok: %s\n' "$1"; }
warn() { printf '  WARN: %s\n' "$1"; }

branch="$(git branch --show-current)"
echo "hornvale merge preflight — branch: ${branch:-<detached>}"

section "Checkout hygiene (this checkout: $(pwd))"
dirty_count="$(git status --porcelain | wc -l | tr -d ' ')"
if [[ "$dirty_count" -eq 0 ]]; then
    ok "working tree clean"
else
    warn "$dirty_count dirty path(s) — if any are not yours, another session is active here: stop and coordinate"
    git status --porcelain | sed 's/^/    /'
fi
staged_count="$(git diff --cached --name-only | wc -l | tr -d ' ')"
if [[ "$staged_count" -eq 0 ]]; then
    ok "index empty"
else
    warn "$staged_count staged path(s) — a commit here would sweep them in; verify every one is yours"
    git diff --cached --name-only | sed 's/^/    /'
fi

if [[ "$branch" == "main" ]]; then
    section "Verdict"
    echo "  on main: branch-vs-main checks skipped. Run from the campaign"
    echo "  branch to preflight a merge; prefer campaign commits from the"
    echo "  campaign worktree, not the shared checkout."
    exit 0
fi

section "Ancestry (main must be an ancestor: merge main INTO the branch first)"
if git merge-base --is-ancestor main HEAD; then
    ok "main ($(git rev-parse --short main)) is an ancestor of HEAD"
else
    fail "main has moved — merge main into this branch, re-run the full gate there, then re-run this preflight"
fi

section "Both-sides-added slug collisions since the merge base (decision 0026)"
merge_base="$(git merge-base main HEAD)"
slug_dirs=(docs/decisions book/src/chronicle docs/retrospectives studies)
branch_added="$(git diff --name-only --diff-filter=A "$merge_base"..HEAD -- "${slug_dirs[@]}" | sort)"
main_added="$(git diff --name-only --diff-filter=A "$merge_base"..main -- "${slug_dirs[@]}" | sort)"
file_collisions="$(comm -12 <(printf '%s\n' "$branch_added") <(printf '%s\n' "$main_added") | sed '/^$/d')"
if [[ -n "$file_collisions" ]]; then
    fail "both sides added the same artifact file(s) — same slug usually means the same idea: resolve as a content merge, not a rename"
    printf '%s\n' "$file_collisions" | sed 's/^/    /'
else
    ok "no decision/chronicle/retrospective/study filename minted on both sides"
fi

registry=book/src/frontier/idea-registry.md
new_row_ids() { # row IDs added in the given range's registry diff
    git diff -U0 "$1" -- "$registry" \
        | sed -nE 's/^\+\| ([A-Z]+-[A-Za-z0-9-]+) \|.*/\1/p' | sort -u
}
id_collisions="$(comm -12 <(new_row_ids "$merge_base..HEAD") <(new_row_ids "$merge_base..main") | sed '/^$/d')"
if [[ -n "$id_collisions" ]]; then
    fail "registry row ID(s) minted on both sides — merge the rows' content under one ID"
    printf '%s\n' "$id_collisions" | sed 's/^/    /'
else
    ok "no idea-registry row ID minted on both sides"
fi

section "What this script cannot score (the judgment half)"
echo "  - semantic collisions hide under clean textual merges: after merging"
echo "    main in, re-run the FULL gate on the merged result — verify stream"
echo "    isolation kept unrelated artifacts byte-stable, don't assume it"
echo "  - keystone fixtures frozen from pre-campaign main go stale when"
echo "    main's physics move: refreeze from main's tip so invariants measure"
echo "    exactly this campaign's delta"
echo "  - fast-forward main only once HEAD is a superset; re-run this"
echo "    preflight after every time main moves"
echo "  - if any subagent was killed mid-run, sweep its worktree for orphaned"
echo "    scaffolding before trusting the gate"

section "Verdict"
if [[ "$nogo" -eq 0 ]]; then
    echo "  GO (mechanical checks passed; the judgment half above is yours)"
else
    echo "  NO-GO (fix the failures above, then re-run)"
    exit 1
fi
