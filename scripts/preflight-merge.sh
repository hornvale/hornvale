#!/usr/bin/env bash
# scripts/preflight-merge.sh — GO/NO-GO preflight for integrating a campaign
# branch with main.
#
# Mechanizes the checkable half of the parallel-campaign integration ritual
# (parallel sessions have triple-collided on artifacts in two days — see
# decisions 0026/0043); prints the judgment half it cannot score as reminders.
# Touches no working tree and no local branch. It DOES do network I/O:
# the Beacon section pushes and fetches the board's refs, and the
# ancestry section below fetches origin's branches. An earlier header
# claimed "read-only: never mutates anything", which was already false
# when board-sync landed -- worth stating precisely, because the false
# claim is probably why nobody added the fetch this check needs.
#
# Run it from the campaign branch (worktree or checkout) you intend to merge
# — at every plan-stage boundary, not only at close: small absorptions of
# main keep semantic drift next to its cause (CLAUDE.md Process). Run from
# main it degrades to checkout hygiene only — useful before any commit in
# the shared checkout, where `git add <file>` has swept another session's
# staged work into a commit before.
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

# From a campaign worktree, also peek at the shared main checkout: dirty or
# staged files THERE mean another session may be mid-landing — a bad moment
# to absorb main.
main_checkout="$(git worktree list --porcelain | sed -n '1s/^worktree //p')"
if [[ "$main_checkout" != "$(pwd)" ]]; then
    section "Main checkout peek ($main_checkout)"
    main_busy="$(git -C "$main_checkout" status --porcelain | wc -l | tr -d ' ')"
    main_staged="$(git -C "$main_checkout" diff --cached --name-only | wc -l | tr -d ' ')"
    if [[ "$main_busy" -eq 0 ]]; then
        ok "main checkout quiescent"
    else
        warn "main checkout has $main_busy dirty path(s) ($main_staged staged) — another session may be mid-landing; coordinate before absorbing or merging"
    fi
fi

if [[ "$branch" == "main" ]]; then
    section "Verdict"
    echo "  on main: branch-vs-main checks skipped. Run from the campaign"
    echo "  branch to preflight a merge; prefer campaign commits from the"
    echo "  campaign worktree, not the shared checkout."
    exit 0
fi

# THE FETCH THIS CHECK CANNOT WORK WITHOUT. Every comparison below resolves
# `main` as a LOCAL ref. A local main that is behind origin yields a cheerful
# GO against a ref nobody else is looking at — the failure this campaign hit,
# and the reason preflight is worth running at all. Best-effort, exactly like
# the Beacon: a fetch failure degrades to the old local-only behaviour with a
# warning, and never changes the verdict on its own.
section "Fetching origin (best-effort; a stale main is the failure this check exists to prevent)"
if git fetch --quiet origin 2>/dev/null; then
    ok "fetched origin"
else
    warn "could not fetch origin — the comparisons below use whatever refs are local, which may be stale"
fi

# Compare against the NEWER of local main and origin/main. Absorbing local
# main is not enough when origin/main has moved past it: the branch would
# merge cleanly here and be behind the moment it lands.
integration_ref=main
if git rev-parse --verify --quiet origin/main >/dev/null; then
    if ! git merge-base --is-ancestor origin/main main; then
        integration_ref=origin/main
        warn "local main is behind origin/main by $(git rev-list --count main..origin/main) commit(s) — comparing against origin/main"
    fi
fi

section "Ancestry ($integration_ref must be an ancestor: merge it INTO the branch first)"
if git merge-base --is-ancestor "$integration_ref" HEAD; then
    ok "$integration_ref ($(git rev-parse --short "$integration_ref")) is an ancestor of HEAD"
else
    behind="$(git rev-list --count "$(git merge-base "$integration_ref" HEAD)".."$integration_ref")"
    fail "$integration_ref has moved ($behind commit(s) unabsorbed) — merge it into this branch, re-run the full gate there, then re-run this preflight"
fi

section "Both-sides-added slug collisions since the merge base (decisions 0026/0043 — decision records are numbered again: confirm the next free number too)"
merge_base="$(git merge-base "$integration_ref" HEAD)"
slug_dirs=(docs/decisions book/src/chronicle docs/retrospectives studies)
branch_added="$(git diff --name-only --diff-filter=A "$merge_base"..HEAD -- "${slug_dirs[@]}" | sort)"
main_added="$(git diff --name-only --diff-filter=A "$merge_base".."$integration_ref" -- "${slug_dirs[@]}" | sort)"
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
id_collisions="$(comm -12 <(new_row_ids "$merge_base..HEAD") <(new_row_ids "$merge_base..$integration_ref") | sed '/^$/d')"
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

# The Beacon: publish this host's board and fetch the peers' BEFORE the
# hold-off read below, so a peer's notice reaches this preflight instead of
# sitting unfetched in refs/hornvale/hosts/* on origin. Non-fatal (B6: a
# push/fetch failure is best-effort by design and must never change this
# script's verdict, any more than the read below does) but NOT silenced —
# a rejected push here is worth a human's attention (0118 part 3's hostname-
# collision hazard), so its report stays visible rather than swallowed.
section "The Beacon: syncing with origin (best-effort, never changes the verdict)"
bash scripts/board-sync.sh || true

# The Cairn: other live branches' HOLD-OFF notices. Advisory only — this never
# changes the verdict (D7), because the board has no standing to block a merge.
board_bin=""
for candidate in tools/board/target/release/board tools/board/target/debug/board; do
  [ -x "${candidate}" ] && board_bin="${candidate}" && break
done
if [ -n "${board_bin}" ]; then
  holds="$("${board_bin}" read 2>/dev/null | grep -F 'polarity=hold-off' || true)"
  if [ -n "${holds}" ]; then
    printf '\nADVISORY — hold-off notices from other sessions:\n'
    printf '%s\n' "${holds}" | sed 's/^/  /'
    printf '  (advisory only; it does not change the verdict)\n'
  fi
fi

section "Verdict"
if [[ "$nogo" -eq 0 ]]; then
    echo "  GO (mechanical checks passed; the judgment half above is yours)"
else
    echo "  NO-GO (fix the failures above, then re-run)"
    exit 1
fi
