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

why="exit"
record() {
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$job_id" "$set_name" \
        "$why" "$1" "$((SECONDS - began))" "$ref" >> "$jobs_tsv"
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

# Look the set up in the ONE roster.
row="$(grep -v '^#' "$repo_root/scripts/lane-sets.tsv" | awk -v s="$set_name" -F'\t' '$1==s')"
[ -n "$row" ] || { echo "lane-run: no such set '$set_name' in scripts/lane-sets.tsv" >&2; exit 2; }
command_line="$(printf '%s' "$row" | cut -f5)"

# A warm per-branch worktree. Task 1's fix is what makes this safe: a renamed
# or recycled worktree otherwise serves binaries with the previous path baked
# in, and on the lane a spurious red is far more expensive than locally,
# because the lane's verdict is the one everybody trusts.
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
echo "lane-run: holds the staff at $(date -Is) after $((SECONDS - wait_began))s queued"
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

bash scripts/timed.sh "lane:$set_name" -- sh -c "$command_line"
