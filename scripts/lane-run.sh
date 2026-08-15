#!/usr/bin/env bash
# scripts/lane-run.sh — the lane's server side. Runs ON the canonical box.
#
# One strictly serial queue: every job takes the SAME claim the census and the
# heavy tier take, because the binding constraint is the machine and there is
# one lefford (decision 0086's "one claim, shared").
#
# ORDERING IS flock's, AND THAT IS MEASURED, NOT ASSUMED. Two trials on this
# box — six spaced waiters, then eight SIMULTANEOUS waiters against a held
# lock — granted strictly in arrival order, 8/8. That is why this script holds
# no ticket sequence of its own; `scripts/test-lane.sh` pins the property.
#
# DETACHMENT IS THE POINT. `flock -w` pins its caller, and a job can now wait
# tens of minutes behind a heavy tier or a census. A pinned caller is a lost
# run: The Siding lost one to a buffered `| tail -40` that made a job dead 60 s
# in look alive for an hour. So the caller gets a job id and leaves.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
set_name="${1:?usage: lane-run.sh <set> <full-sha>}"
ref="${2:?usage: lane-run.sh <set> <full-sha>}"

HV_LANE_DIR="${HV_LANE_DIR:-$HOME/.local/state/hornvale/lane}"
mkdir -p "$HV_LANE_DIR"
job_id="$set_name-$(printf '%.12s' "$ref")-$(date -u +%Y%m%dT%H%M%SZ)"
run_log="$HV_LANE_DIR/$job_id.log"
jobs_tsv="$HV_LANE_DIR/jobs.tsv"
began=$SECONDS

# `~/.local/state`, not /tmp: a jobs ledger is the ONLY record that a job ever
# existed, and /tmp does not survive a reboot. heavy-run.sh's /tmp default is
# inherited history, not a precedent worth copying here.
#
# JOBS.TSV COLUMNS: when (utc) / job / set / why / rc / wall_s / ref /
# waited_s / user_s / sys_s / cpu_ratio. The first seven are unchanged from
# the original shape, in the original order, so anything already parsing
# this file keeps working; the last four were added later, appended rather
# than interleaved, for the same reason.
#
# `wall_s` (col 6) is this SCRIPT's own total elapsed time since `began` was
# set, above — queue wait included, because it starts before the flock
# acquisition below. `waited_s` (col 8) is the QUEUE portion of that alone
# (set once acquisition succeeds, mirroring decision 0081's `waited_s` on
# census-run.sh/heavy-run.sh); subtracting it from `wall_s` is what isolates
# the actual work. `user_s`/`sys_s`/`cpu_ratio` (cols 9-11) are the numbers
# CLAUDE.md names as *the* diagnostic that separates contention from a real
# regression — timed.sh computes them internally for every run, but its own
# `docs/timings.md` row lands in the LANE'S SCRATCH WORKTREE, which the next
# dispatch's `checkout --force` + `reset --hard` (below) destroys before
# anyone reviews it. jobs.tsv is the durable, per-host-independent home
# already declared above as "the ONLY record that a job ever existed", so
# these numbers are captured here too, independently of timed.sh's own
# (still-written, still-discarded) row.
#
# All four default to empty (not 0): a job that never reached the relevant
# stage — refused by the canonical-host guard, an unknown set, a flock
# timeout — has no honest number to report, and an empty TSV field reads as
# "not applicable" without being mistaken for a measured zero.
waited_s=""
job_user_s=""
job_sys_s=""
job_cpu_ratio=""

why="exit"
record() {
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$job_id" "$set_name" \
        "$why" "$1" "$((SECONDS - began))" "$ref" \
        "$waited_s" "$job_user_s" "$job_sys_s" "$job_cpu_ratio" >> "$jobs_tsv"
}
trap 'why=SIGINT'  INT
trap 'why=SIGTERM' TERM
trap 'why=SIGHUP'  HUP
trap 'record $?' EXIT

# shellcheck source=scripts/census-canonical-host.sh
. "$repo_root/scripts/census-canonical-host.sh"
# WRONG-HOST REFUSAL HAPPENS BEFORE THE LOG REDIRECT BELOW, so its message
# goes to this process's own stderr, which lane-dispatch.sh's detached
# `setsid` launch sends to /dev/null — the caller never sees it. `jobs.tsv`
# still gets an `rc=1` row (the EXIT trap fires either way), so the job is
# not silently lost, but `<job-id>.log` is EMPTY for it. Debugging an empty
# lane log: check jobs.tsv's `rc` column before assuming the job never
# started at all.
require_canonical_host "$set_name" || exit 1

exec >>"$run_log" 2>&1
echo "lane-run: $job_id started $(date -Is) on $(hostname -s) as pid $$"

# Look the set up in the ONE roster. Read-only against repo_root (the
# PRIMARY checkout, never the shared scratch worktree below), so this is safe
# to do before the lock — nothing here touches state a concurrent job could
# be relying on.
row="$(grep -v '^#' "$repo_root/scripts/lane-sets.tsv" | awk -v s="$set_name" -F'\t' '$1==s')"
[ -n "$row" ] || { echo "lane-run: no such set '$set_name' in scripts/lane-sets.tsv" >&2; exit 2; }
# `$repo_root` HERE IS THE DISPATCHING checkout, NOT `$ref`'s. The command
# this job runs comes from the roster row in the checkout that happened to
# invoke lane-run.sh — read above, before the checkout/reset below ever
# touches the shared scratch worktree — not from `scripts/lane-sets.tsv` as
# it exists at the dispatched ref. A campaign editing the roster (a new set,
# a changed command) has its own edit ignored by every lane job dispatched
# against it until the branch merges and becomes reachable from whatever
# checkout is doing the dispatching.
command_line="$(printf '%s' "$row" | cut -f5)"

LOCK="${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
claim_path="${HV_CENSUS_CLAIM_PATH:-/tmp/hv-census.claim}"
exec 9>"$LOCK"
timeout_s="${HV_LANE_WAIT_TIMEOUT:-7200}"
echo "lane-run: queued for the staff ($LOCK; up to ${timeout_s}s) …"
wait_began=$SECONDS
if ! flock -w "$timeout_s" 9; then
    echo "lane-run: TIMED OUT after ${timeout_s}s waiting for the staff." >&2
    exit 75
fi
# `waited_s` stops HERE, at the moment the lock is actually acquired — before
# anything below touches the shared worktree. A future routing policy
# thresholds work on this number (root CLAUDE.md), so it must measure the
# QUEUE alone; git time folded in would make that threshold measure the
# wrong thing.
waited_s=$((SECONDS - wait_began))
echo "lane-run: holds the staff at $(date -Is) after ${waited_s}s queued"

# A warm per-branch worktree — MOVED HERE, behind the lock, deliberately.
# Doing this checkout BEFORE the lock (its original position) meant every
# queued job reset the shared tree the moment it started, including while an
# EARLIER job was still running inside it — the lock serialized the WORK, not
# the TREE the work runs in, which is precisely backwards for a lane whose
# entire point is serialization. A live campaign-gate dispatch (six sets at
# once) proved it: five jobs stomped the tree under the one holding the
# staff, `seam-guard` refused on a dirty working tree carrying `heavy`'s
# half-written `the-history`/`the-sounding` artifacts, and `heavy` itself
# reported two failures with unknowable provenance because its own worktree
# was reset out from under it mid-run. Task 1's fix (which invalidates a
# renamed/recycled worktree's stale compiled artifacts) is what makes this
# checkout safe at all; it says nothing about two checkouts racing each
# other, which is the failure mode this move closes.
wt="${HV_LANE_WORKTREE:-$repo_root/../hornvale-lane-wt}"
git -C "$repo_root" fetch --all --quiet
if [ -e "$wt/.git" ] || git -C "$repo_root" worktree list --porcelain | grep -qF "$wt"; then
    git -C "$wt" fetch --all --quiet
    git -C "$wt" checkout --force "$ref"
    git -C "$wt" reset --hard "$ref" --quiet
else
    git -C "$repo_root" worktree add --force "$wt" "$ref"
fi
echo "lane-run: HEAD in $wt is $(git -C "$wt" rev-parse --short HEAD)"
bash "$repo_root/scripts/test-worktree-freshness.sh" || true

cd "$wt"

export HV_CENSUS_LOCK_HELD=$$

{
    echo "pid=$$"
    echo "host=$(hostname -s)"
    echo "user=${USER:-unknown}"
    echo "started=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "label=lane:$set_name"
    echo "ref=$ref"
    echo "job=$job_id"
} > "$claim_path"
# shellcheck disable=SC2154  # code is assigned first thing inside this same trap string
trap 'code=$?; rm -f "$claim_path"; echo "lane-run: finished $(date -Is) rc=$code"; record "$code"' EXIT

# Measured HERE, independently of timed.sh's own internal measurement of the
# same command, so user_s/sys_s/cpu_ratio survive into jobs.tsv (the durable
# ledger) rather than existing only in timed.sh's docs/timings.md row in this
# scratch worktree, which the NEXT dispatch's checkout+reset above destroys
# before anyone reviews it. Same technique timed.sh itself uses (bash's
# builtin `time`, TIMEFORMAT, redirecting the command's own streams through
# 8/9 so only `time`'s own report lands in the temp file) — duplicated rather
# than parsed back out of timed.sh's stderr, so this has no dependency on that
# script's log-line format.
#
# Wrapped in an explicit `if`, not a bare assignment: under `set -e` a failing
# command directly after `{ time ...; }` would abort the script right here,
# before `job_rc` is ever read and before user_s/sys_s/cpu_ratio are computed
# — exactly the case (a failing command) most worth having cpu_ratio for, to
# tell a real regression from contention on the box.
_lane_time_tmp="$(mktemp)"
if { TIMEFORMAT='%R %U %S'; time bash scripts/timed.sh "lane:$set_name" -- sh -c "$command_line" 1>&8 2>&9; } 8>&1 9>&2 2>"$_lane_time_tmp"; then
    job_rc=0
else
    job_rc=$?
fi
read -r _lane_wall_s job_user_s job_sys_s < "$_lane_time_tmp"
rm -f "$_lane_time_tmp"
job_cpu_ratio="$(awk -v u="$job_user_s" -v s="$job_sys_s" -v r="$_lane_wall_s" \
    'BEGIN{ if (r+0>0) printf "%.2f", (u+s)/r; else print "?" }')"

# Copy the sub-floor roster out to durable storage — the exact fix this
# script's own header (above) already describes for timed.sh's
# docs/timings.md row, applied to the OTHER artifact that had no equivalent.
# `cargo run -p hornvale -- ci-record` (called from gate-run, inside the
# `gate` set's `gate-suite-run`) rewrites `docs/timings/subfloor-roster.tsv`
# in $wt on every GREEN full-workspace run, but $wt is the SAME shared
# scratch worktree the NEXT dispatch's `checkout --force` + `reset --hard`
# (above) destroys before anyone can review or commit it — which is why the
# documented remedy ("it enters on the next green stage gate, which measures
# it and rewrites the roster") had never once actually landed a byte.
#
# GREEN-ONLY, deliberately, not stylistically: `cli/src/main.rs`'s
# `cmd_ci_record` doc notes a red run's `run.json` is truncated, so a roster
# derived from one would silently DROP tests from the commit gate — the
# opposite of what a copy-out is for. `job_rc` is this script's own exit
# code, which is 0 only when the whole dispatched command line succeeded
# (gate-run's own `exit $$alarm_status` included), so it is the right gate
# for "was this a green run" without re-deriving that from nextest's own
# files.
#
# Named beside `$run_log` ($HV_LANE_DIR/$job_id.log), not beside the roster's
# repo path, because $HV_LANE_DIR — outside the worktree — is the whole
# reason either file survives the next dispatch.
#
# COPY ONLY IF THE RUN ACTUALLY REWROTE IT, which is a stricter test than
# "the file exists" and the difference is not academic. The roster is a
# COMMITTED file, so `checkout --force` + `reset --hard` leaves it present in
# $wt for EVERY set, not just `gate` — a bare `[ -f ]` would copy a
# byte-unchanged roster out of every green `artifacts`, `clients`, `heavy`,
# `census` or `seam-guard` job. `make lane-roster` takes the most RECENT
# copy-out, so it would then hand back a no-op diff from whichever set
# happened to finish last while the real `gate` roster sat unconsulted — an
# empty diff reading as "nothing changed" when nothing was ever rebuilt, the
# same shape as running a digest `render` without its redirect.
#
# `git diff --quiet` against the checked-out ref answers the actual question
# ("did this run modify it?") without hardcoding which set runs ci-record,
# so a future set that gains one is covered without editing this.
_subfloor_src="$wt/docs/timings/subfloor-roster.tsv"
if [ "$job_rc" -eq 0 ] && [ -f "$_subfloor_src" ] \
   && ! git -C "$wt" diff --quiet -- docs/timings/subfloor-roster.tsv 2>/dev/null; then
    cp "$_subfloor_src" "$HV_LANE_DIR/$job_id.subfloor-roster.tsv"
    echo "lane-run: copied the sub-floor roster to $HV_LANE_DIR/$job_id.subfloor-roster.tsv (make lane-roster brings it back)"
fi

exit "$job_rc"
