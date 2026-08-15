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
require_canonical_host "$set_name" || exit 1

exec >>"$run_log" 2>&1
echo "lane-run: $job_id started $(date -Is) on $(hostname -s) as pid $$"

# Look the set up in the ONE roster. Read-only against repo_root (the
# PRIMARY checkout, never the shared scratch worktree below), so this is safe
# to do before the lock — nothing here touches state a concurrent job could
# be relying on.
row="$(grep -v '^#' "$repo_root/scripts/lane-sets.tsv" | awk -v s="$set_name" -F'\t' '$1==s')"
[ -n "$row" ] || { echo "lane-run: no such set '$set_name' in scripts/lane-sets.tsv" >&2; exit 2; }
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
exit "$job_rc"
