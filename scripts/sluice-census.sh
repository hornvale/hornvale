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

# Sourced BEFORE the HV_CENSUS_LIB early return below, because
# census_golden_count calls sluice_path_author and the tests source this file
# for exactly that function. Placing it after the guard left the classifier
# unloaded under test — the function errored to 0, and a 0 was what the
# counter-file assertion expected, so it PASSED VACUOUSLY. The new-file arm
# caught it; the arm under test would not have.
# shellcheck source=/dev/null
HV_PHASES_LIB=1 . "$(dirname "${BASH_SOURCE[0]}")/sluice-phases.sh"

# THE STAGED SET ALWAYS CONTAINS THE RUN'S OWN TIMINGS ROW, so "did anything
# move?" is the wrong question and asking it made the null arm UNREACHABLE for
# the whole life of this script. `timed.sh` resolves its ledger inside the
# census worktree, and the general `add -u` below is the only thing that
# commits that row (The Governor, Task 7 — 27 heavy runs had written rows
# nothing ever committed). So the row is staged on every run, `diff --cached
# --quiet` is never true, and every census announced "goldens delivered"
# including the ones that moved nothing. Observed twice on 2026-09-02:
# campaign/the-wicket a73d8ce3c9b4 and campaign/the-roll f1b21b58c5cf each
# delivered a branch whose entire content was `docs/timings.md | 1 +`.
#
# THE FIRST FIX WAS RIGHT IN DIRECTION AND WRONG IN DEFINITION, and it took two
# sightings to say so. It counted every staged path that was not docs/timings.md
# and called the result "golden path(s)". That mislabelled campaign/the-crosscut
# 4c69a5d6578d, where the one moved path was docs/generated-path-writes.tsv (a
# COUNTER of how many files each declared path wrote), and again
# campaign/the-brattice cccfcdad9f21 — where it mattered, because that candidate
# ADDED A STREAM LABEL and "goldens moved" is precisely the answer that would
# have meant the new draw shifted the world. It had not.
#
# A CENSUS GOLDEN IS A PATH THE CENSUS AUTHORS, and docs/generated-paths.txt's
# author column already says which those are: exactly `the-census/` and
# `census-of-the-meeting/`. Everything else under laboratory/generated is
# `artifacts` (the schema files), `heavy` (the-history), or `none` (nine frozen
# one-off studies). Reading the column is both narrower and correct, and it is
# the same authority scripts/sluice-phases.sh's conflict classifier reads.
# HV_CENSUS_LIB=1 sources this file for its functions without running a census,
# so both arms can be driven against a real git index (test-sluice-census.sh).
census_golden_count() {
    local wt="${1:?census_golden_count <worktree>}"
    local declared="$wt/docs/generated-paths.txt"
    [ -f "$declared" ] || declared="$repo_root/docs/generated-paths.txt"
    local pth n=0
    while IFS= read -r pth; do
        [ -n "$pth" ] || continue
        [ "$(sluice_path_author "$pth" "$declared")" = "census" ] && n=$((n + 1))
    done <<EOF
$(git -C "$wt" diff --cached --name-only 2>/dev/null)
EOF
    printf '%s' "$n"
}
# shellcheck disable=SC2317  # the exit is the fallback when this file is RUN, not sourced
if [ -n "${HV_CENSUS_LIB:-}" ]; then return 0 2>/dev/null || exit 0; fi

repo_root="${HV_SLUICE_REPO_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
ref="${1:?usage: sluice-census.sh <full-sha>|<request-id> [study.json ...]}"
shift || true

# A SINGLE ARGUMENT THAT LOOKS LIKE A REQUEST ID IS ONE — see the matching
# block in sluice-run.sh. Resolved BEFORE the SHA-format validation below,
# since a request id is not itself a 40-char hex SHA; the row's own sha
# column is what gets validated. `census_row_id` is set here so the
# interlock section further down can tell "resolved from an id" apart from
# "positional form, claim it ourselves" without an exported claim-id
# environment variable.
census_row_id=""
case "$ref" in
    req-*)
        # THE PIPELINE'S EXIT CODE IS CAPTURED, NOT DISCARDED (fix round 3,
        # Important 5). This file runs under `set -uo pipefail` with NO `-e`,
        # so `list`'s exit 3 (no built binary) fell on the floor, `_row` came
        # back empty, and the script reported "no queue row with id 'req-…'"
        # — a false statement about a row that exists, and a rc=2 that reads
        # as the operator's typo. An unreachable queue is a different fact and
        # gets rc=13, the same code the claim path below uses for it.
        _list="$(bash "$repo_root/scripts/sluice-queue.sh" list 2>&1)"
        _list_rc=$?
        if [ "$_list_rc" -ne 0 ]; then
            echo "sluice-census: REFUSING — could not reach the queue (list rc=$_list_rc):" >&2
            printf '%s\n' "$_list" >&2
            echo "sluice-census: cannot resolve '$ref' without the queue; refusing rather than guessing." >&2
            exit 13
        fi
        _row="$(printf '%s\n' "$_list" | awk -F'\t' -v i="$ref" '$2==i {print; exit}')"
        if [ -z "$_row" ]; then
            echo "sluice-census: no queue row with id '$ref'" >&2
            exit 2
        fi
        # THE ID IS NOT ITSELF A CLAIM — the mirror of sluice-run.sh's guard,
        # and see that file's comment for the three consequences an unclaimed
        # run carries (a second dispatch, a supersede mid-write, and no
        # terminal state, since `release_census_row` is gated on
        # `census_claimed`). A census costs ~15 minutes of the canonical box;
        # running one twice is the most expensive form of this defect.
        _state="$(printf '%s' "$_row" | cut -f5)"
        if [ "$_state" != "running" ]; then
            echo "sluice-census: REFUSING — the row for '$ref' is NOT claimed: state='$_state', want 'running'." >&2
            echo "sluice-census: an id is not a claim. Take the row first ('sluice-queue.sh claim --sha <sha>')," >&2
            echo "sluice-census: or let the drain loop claim and dispatch it." >&2
            exit 16
        fi
        # AND THE KIND IS VALIDATED, as sluice-run.sh validates its own (fix
        # round 3, Important 2). Without this, `sluice-census.sh req-<a stage
        # request>` ran a ~15-minute census against a row that asked for a
        # merge gate, and wrote `reported` on it afterwards as though the gate
        # had run. The row's kind is the only thing that says what was asked
        # for; an entry point that ignores it answers a question nobody put.
        _kind="$(printf '%s' "$_row" | cut -f6)"
        [ -n "$_kind" ] || _kind="merge"
        if [ "$_kind" != "census" ]; then
            echo "sluice-census: REFUSING — row '$ref' is kind='$_kind', not 'census'." >&2
            echo "sluice-census: a merge or stage request goes to scripts/sluice-run.sh; running a census" >&2
            echo "sluice-census: against it would spend ~15 minutes of the box answering a different question." >&2
            exit 2
        fi
        census_row_id="$ref"
        ref="$(printf '%s' "$_row" | cut -f4)"
        echo "sluice-census: resolved $census_row_id -> sha=${ref:0:12} kind=$_kind" >&2
        ;;
esac

case "$ref" in
    *[!0-9a-f]*|"") echo "sluice-census: REF must be a full 40-char SHA (hex only); got '$ref'" >&2; exit 2 ;;
esac
[ "${#ref}" -eq 40 ] || { echo "sluice-census: REF must be a full 40-char SHA; got '$ref'" >&2; exit 2; }

# --- THE QUEUE ROW, same interlock as every other chamber entry point -------
# This script never touched the queue, so a census dispatched through it left
# its row reading `queued` for the whole run and forever after. On 2026-09-04
# the operator had to set `req-fbe2f5a9003f-...` to `reported` by hand after
# the fact — the second time in one session a terminal state was written by a
# person instead of by the thing that knew the answer. A `queued` row that is
# actually running is also exactly what `sluice-queue.sh claim` hands to the
# next dispatcher, which is how a merge came to run twice the same day.
#
# Census is not run by sluice-run.sh (it takes the shared flock itself), so it
# cannot inherit that script's bookkeeping and needs its own copy here.
#
# `census_row_id` may already be set above (resolved from a `req-*` id) — and
# ONLY after the block above proved the row reads `running`. That proof is what
# licenses skipping the claim here: the row is already taken atomically by
# whoever ran `claim`. The same reasoning sluice-run.sh's interlock uses, down
# to the correction that an operator merely QUOTING an id owns nothing; and
# there is likewise no env var to consume or unset any more.
census_claimed=0
if [ -n "$census_row_id" ]; then
    :
else
    set +e
    # STDOUT AND STDERR ARE CAPTURED TOGETHER (fix round 2, Critical F1) —
    # see the matching comment in sluice-run.sh. On success `claim` writes
    # nothing to stderr, so folding the streams costs that path nothing.
    census_claim="$(bash "$repo_root/scripts/sluice-queue.sh" claim --sha "$ref" \
        "launched directly by operator; kind=census" 2>&1)"
    census_claim_rc=$?
    set -e
    case "$census_claim_rc" in
        0) census_row_id="$(printf '%s' "$census_claim" | cut -f2)"; census_claimed=1 ;;
        4) echo "sluice-census: REFUSING — the row for ${ref:0:12} is not queued; somebody else has it." >&2
           exit 9 ;;
        5) echo "sluice-census: no queue row for ${ref:0:12} — running AD HOC, unbookkept." >&2 ;;
        *) # REFUSE rather than proceed unbookkept (fix round 2, Critical
           # F1) — see the matching case in sluice-run.sh. If we cannot ask
           # the queue whether somebody else already holds this row, we do
           # not run a census against it either.
           echo "sluice-census: REFUSING — could not reach the queue (claim rc=$census_claim_rc):" >&2
           printf '%s
' "$census_claim" >&2
           echo "sluice-census: refusing to run unbookkept rather than risk a duplicate execution." >&2
           exit 13 ;;
    esac
fi

# Reached only through the EXIT trap below, which the linter cannot follow —
# same reason as the SC2317 directive further up this file. NOTE: no line of
# this comment may BEGIN with the linter's own name, which is inline-directive
# syntax and fails the whole file with SC1073.
# shellcheck disable=SC2317
release_census_row() {
    [ "$census_claimed" = "1" ] || return 0
    [ -n "$census_row_id" ] || return 0
    local rc="$1" state note
    if [ "$rc" = "0" ]; then
        state="reported"; note="census rc=0 (direct run)."
    else
        state="held"; note="CENSUS RED rc=$rc (direct run) — attribution pending."
    fi
    bash "$repo_root/scripts/sluice-queue.sh" set-state "$census_row_id" "$state" "$note" \
        >/dev/null 2>&1 || true
}
trap 'release_census_row $?' EXIT

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
# ALSO CARRIES docs/timings.md's census row, and that is load-bearing
# rather than incidental (The Governor, Task 7). `timed.sh` resolves
# its ledger with a bare `git rev-parse --show-toplevel`, so inside
# this worktree the row lands HERE, not in the canonical checkout.
# This general `add -u` is the only thing that sweeps it up. Narrowing
# it to the golden paths would silently reopen, for census, exactly
# the bug Task 7 fixed for heavy: 27 heavy runs between 2026-08-05
# and 2026-08-28 wrote a row that nothing ever committed.
git -C "$wt" add -u 2>/dev/null || true

if [ -z "$(git -C "$wt" diff --cached --name-only)" ]; then
    # STILL SAYS "NO GOLDENS MOVED", and that is load-bearing rather than
    # cosmetic. scripts/test-sluice.sh greps for exactly this line, and its own
    # comment explains why it must: "no new branch" alone is VACUOUS, because
    # `git commit` refuses an empty commit, so deleting the guard entirely also
    # pushes nothing and the test would pass for a reason unrelated to the
    # behaviour it claims to check. The distinctive line is the only thing only
    # the guard can produce.
    #
    # THIS IS ALSO WHY THE PRODUCTION BUG SURVIVED A TEST THAT LOOKS LIKE IT
    # COVERS IT. That harness stubs the census and writes NO timings row, so its
    # "nothing moved" is an EMPTY index — this branch. Production always stages
    # the row, so its "nothing moved" is the branch below, which nothing
    # exercised. The fixture did not reproduce the staging, and a test over the
    # wrong case reads exactly like coverage.
    echo "sluice-census: NO GOLDENS MOVED — the census agrees with $ref."
    echo "sluice-census: nothing staged at all, not even a timings row, so there is"
    echo "sluice-census: nothing to deliver and no branch is pushed."
    exit 0
fi

branch="census/$(printf '%.12s' "$ref")-$(date -u +%Y%m%dT%H%M%SZ)"
n_goldens="$(census_golden_count "$wt")"
if [ "$n_goldens" -eq 0 ]; then
    echo "sluice-census: NO GOLDENS MOVED — the census agrees with $ref."
    echo "sluice-census: delivering the timings row only on $branch"
    echo "sluice-census: there is nothing to merge for goldens' sake."
else
    echo "sluice-census: $n_goldens golden path(s) moved; delivering on $branch"
fi
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
    commit -q -m "chore(census): $(if [ "$n_goldens" -eq 0 ]; then echo "record the run at"; else echo "regenerate goldens at"; fi) ${ref:0:12}

Authored on $(hostname -s), the canonical host (decision 0063/0079), through
the queue rather than by hand. NOT pushed to main: census goldens are what the
calibration batteries assert against, so they land through the chamber like any
other change. Submit this branch as an ordinary merge to gate it.

Census wall time: ${elapsed}s."; then
    echo "sluice-census: COMMIT REFUSED — the census ran and its output is NOT delivered." >&2
    echo "sluice-census: $(git -C "$wt" diff --cached --name-only | wc -l) staged path(s) ($n_goldens golden) remain in $wt; nothing was pushed." >&2
    echo "sluice-census: this is a hook refusal, not a census failure — read the log above." >&2
    exit 4
fi
if ! git -C "$wt" push -q origin "HEAD:refs/heads/$branch"; then
    echo "sluice-census: PUSH FAILED — the commit exists locally in $wt on a detached HEAD." >&2
    echo "sluice-census: recover it with: git -C $wt push origin HEAD:refs/heads/$branch" >&2
    exit 3
fi
if [ "$n_goldens" -eq 0 ]; then
    echo "sluice-census: DELIVERED on $branch — TIMINGS ROW ONLY, no goldens moved."
    echo "sluice-census: the census agrees with $ref. Merging this branch lands the"
    echo "sluice-census: cost measurement and nothing else; it is not a golden refresh."
else
    echo "sluice-census: DELIVERED on $branch — $n_goldens golden path(s) moved."
fi
echo "sluice-census: submit it with — make sluice BRANCH=$branch REF=$(git -C "$wt" rev-parse HEAD)"
exit 0
