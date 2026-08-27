#!/usr/bin/env bash
# scripts/sluice-census.sh — run a census as a QUEUED job and deliver its
# goldens on a branch.
#
# WHY THIS IS A SIBLING OF sluice-run.sh AND NOT A PHASE INSIDE IT. A census
# cannot run under the chamber's claim: `census-run.sh` takes the shared flock
# itself (census-run.sh:132) and `rm -f`s the claim file on EXIT
# (census-run.sh:207), so nesting it inside a job that already wrote that claim
# would clobber the holder's claim and then delete it out from under a run
# still in progress. sluice-run.sh already refuses `census` as a phase for
# exactly this reason, and this script exists so the refusal stays true while
# the WORK still gets queued.
#
# WHAT THIS BUYS, and the ordering half is the part nobody asked for but
# everybody was paying: a census takes the same claim every chamber job takes,
# but until now it was INVISIBLE to the queue that claim exists to order.
# Measured 2026-08-24 — perf/world-plate-memo was queued at 11:30:37Z, the
# chamber asked for the staff at 11:50:12Z, an unqueued census took it at
# 11:50:14Z, and that merge waited ~19 minutes for ~600 s of work (1913 s
# total). The census was not jumping the queue; it was not IN the queue. Giving
# it a kind puts it under the same FIFO as everything else.
#
# WHAT IT DELIBERATELY DOES NOT DO: push `main`. The `pre-push` hook would
# permit it — a census holds the canonical box's live claim, which is the one
# thing that hook checks (decision 0139) — and that is precisely why the
# restraint has to be written down rather than left to the substrate. Census
# goldens are what the calibration batteries ASSERT AGAINST; landing them
# un-gated would move the reference without anything checking that the world
# still agrees with it. So this pushes a BRANCH, and the requestor submits that
# branch as an ordinary merge, which gates it. CLAUDE.md's standing rule that
# committing a moved column is a deliberate act survives intact; it just stops
# being manual labour.
#
# Decision 0079 (one enforced authoring host) is NOT re-implemented here:
# census-run.sh's own hostname guard is the single source of truth for it, and
# a second copy is the drift `cli/tests/lane_sets.rs` exists to fail on.
set -uo pipefail

repo_root="${HV_SLUICE_REPO_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
ref="${1:?usage: sluice-census.sh <full-sha> [study.json ...]}"
shift || true

case "$ref" in
    *[!0-9a-f]*|"") echo "sluice-census: REF must be a full 40-char SHA (hex only); got '$ref'" >&2; exit 2 ;;
esac
[ "${#ref}" -eq 40 ] || { echo "sluice-census: REF must be a full 40-char SHA; got '$ref'" >&2; exit 2; }

job_id="census-$(printf '%.12s' "$ref")-$(date -u +%Y%m%dT%H%M%SZ)"
HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
mkdir -p "$HV_SLUICE_DIR"
run_log="$HV_SLUICE_DIR/$job_id.log"
exec >>"$run_log" 2>&1
echo "sluice-census: $job_id started $(date -Is) on $(hostname -s) as pid $$ ref=$ref"

# The census worktree, resolved by the script that owns that decision rather
# than restated here (decision 0146 anchors the default to the MAIN worktree).
wt="$(bash "$repo_root/scripts/census-run.sh" worktree 2>/dev/null | tail -1)"
[ -n "$wt" ] || { echo "sluice-census: could not resolve the census worktree" >&2; exit 2; }
echo "sluice-census: census worktree is $wt"

start=$SECONDS
HV_CENSUS_REF="$ref" bash "$repo_root/scripts/census-run.sh" "$@"
rc=$?
elapsed=$((SECONDS - start))
if [ "$rc" -ne 0 ]; then
    echo "sluice-census: census-run.sh failed rc=$rc after ${elapsed}s — nothing committed, nothing pushed."
    exit "$rc"
fi
echo "sluice-census: census-run.sh rc=0 in ${elapsed}s"

[ -d "$wt" ] || { echo "sluice-census: worktree $wt vanished after the run" >&2; exit 2; }

# Stage the census's own output explicitly (so a NEW golden file is caught, not
# only a modified one), then any other tracked drift. NOT `add -A` over the
# whole tree: the census worktree is shared, and sweeping untracked debris into
# a delivery branch would hand the requestor somebody else's files.
git -C "$wt" add -A -- book/src/laboratory/ 2>/dev/null || true
git -C "$wt" add -u 2>/dev/null || true

if git -C "$wt" diff --cached --quiet 2>/dev/null; then
    echo "sluice-census: NO GOLDENS MOVED — nothing to deliver."
    echo "sluice-census: that is a result, not a failure: the census agrees with $ref."
    exit 0
fi

branch="census/$(printf '%.12s' "$ref")-$(date -u +%Y%m%dT%H%M%SZ)"
moved="$(git -C "$wt" diff --cached --name-only | wc -l)"
echo "sluice-census: $moved path(s) moved; delivering on $branch"
git -C "$wt" diff --cached --stat | sed 's/^/sluice-census:   /'

# HV_CENSUS_DELIVERY=1 tells pre-commit's golden-pins guard to stand down for
# THIS commit only. It is a scoped, named opt-out of ONE check; every other
# hook check still runs, and the hooks themselves stay installed and armed.
# The guard compares the census fixture against calibration.rs's pins, and a
# census refresh moves the fixture BY DEFINITION while the pins can only be
# re-pinned afterwards by the campaign that owns them. So the guard fires on
# exactly the commit it must not block, and a delivery branch with desynced
# pins is the CORRECT output of a census run, not a defect. The chamber runs
# census-check again when this branch is submitted as a merge, which is where
# the desync must actually be resolved.
# core.hooksPath is RELATIVE ('scripts/hooks'), so it resolves inside the
# worktree — which is checked out at the ref being censused, and therefore
# carries THAT ref's hooks, not this script's. A census of an older ref would
# run an older guard and could not honour the escape below however this script
# is fixed. The delivery commit is an act of the QUEUE, so it runs the queue's
# own hooks, from the main checkout.
if ! HV_CENSUS_DELIVERY=1 \
   git -C "$wt" -c core.hooksPath="$repo_root/scripts/hooks" \
             -c user.name="$(git -C "$repo_root" config user.name)" \
             -c user.email="$(git -C "$repo_root" config user.email)" \
    commit -q -m "chore(census): regenerate goldens at ${ref:0:12}

Authored on $(hostname -s), the canonical host (decision 0063/0079), through
the queue rather than by hand. NOT pushed to main: census goldens are what the
calibration batteries assert against, so they land through the chamber like any
other change. Submit this branch as an ordinary merge to gate it.

Census wall time: ${elapsed}s."; then
    echo "sluice-census: COMMIT REFUSED — the census ran and its output is NOT delivered." >&2
    echo "sluice-census: the $moved moved path(s) are staged in $wt; nothing was pushed." >&2
    echo "sluice-census: this is a hook refusal, not a census failure — read the log above." >&2
    exit 4
fi
if ! git -C "$wt" push -q origin "HEAD:refs/heads/$branch"; then
    echo "sluice-census: PUSH FAILED — the commit exists locally in $wt on a detached HEAD." >&2
    echo "sluice-census: recover it with: git -C $wt push origin HEAD:refs/heads/$branch" >&2
    exit 3
fi
echo "sluice-census: DELIVERED on $branch"
echo "sluice-census: submit it with — make sluice BRANCH=$branch REF=$(git -C "$wt" rev-parse HEAD)"
exit 0
