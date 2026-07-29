#!/usr/bin/env bash
# scripts/heavy-run.sh — run the heavy tier on THIS box, the single canonical
# platform for the artifacts it authors (The Siding; decisions 0063/0079/0081).
#
# Why the heavy tier carries a canonical-host guard at all. It is not merely
# expensive, it is an AUTHORING path: three of its tests write committed
# artifacts —
#   cli/tests/history_battery.rs        -> book/src/laboratory/generated/the-history/
#   windows/chronicle/.../sounding_sweep -> book/src/laboratory/generated/the-sounding/
#   windows/worldgen/.../occupancy_readout -> tests/fixtures/occupancy.csv
# — and census_fixtures_match_a_probe_of_live_seeds compares a LIVE probe
# against lefford-authored census fixtures. 0063 measured that two boxes
# disagree by one unit on ~0.1% of discrete-count metrics, decided in the
# COMPUTE path upstream of quantize-at-emit, where nothing can absorb it. So a
# wrong-host run does not fail loudly: it commits values that then drift-check
# green forever, which is the exact pathology 0079 exists to prevent.
#
# Usage — locally on the canonical box, or from the other machine via SSH:
#   scripts/heavy-run.sh                      # run the heavy tier here
#   HV_HEAVY_REF=<sha> scripts/heavy-run.sh   # fetch + run that ref in a
#                                             # dedicated scratch worktree
#   scripts/heavy-run.sh status               # is a heavy job holding THIS box?
#
# `status` reports on the machine it runs on, because the claim lives in that
# machine's own /tmp. From the other machine use `make heavy-status`, which
# asks the canonical box rather than the local one — a local `status` on a dev
# machine is always "no" and is not the question you meant.
#
#   # from the other machine (push your branch first, then):
#   make heavy-remote REF=<full-sha>
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# `status` is handled before the host guard AND before the lock: asking is not
# authoring, so it is legal from any machine and must never block (0081).
if [ "${1:-}" = "status" ]; then
    cargo run --quiet --release -p hornvale -- lab claim-status
    exit $?
fi

# shellcheck source=scripts/census-canonical-host.sh
. "$(dirname "$0")/census-canonical-host.sh"
require_canonical_census_host heavy || exit 1

run_root="$repo_root"
if [ -n "${HV_HEAVY_REF:-}" ]; then
    # Run a specific ref in a dedicated worktree, so the caller's own checkout
    # (and this canonical one) are left untouched. Path is outside the repo to
    # keep `git status` clean here.
    wt="${HV_HEAVY_WORKTREE:-$repo_root/../hornvale-heavy-wt}"
    git -C "$repo_root" fetch --all --quiet
    if [ -d "$wt/.git" ] || git -C "$repo_root" worktree list --porcelain | grep -qF "$wt"; then
        git -C "$wt" fetch --all --quiet
        git -C "$wt" checkout --force "$HV_HEAVY_REF"
        git -C "$wt" reset --hard "$HV_HEAVY_REF" --quiet
    else
        git -C "$repo_root" worktree add --force "$wt" "$HV_HEAVY_REF"
    fi
    run_root="$wt"
    echo "heavy-run: running ref '$HV_HEAVY_REF' in $wt" >&2
    # Echo the resolved HEAD: HV_HEAVY_REF feeds `reset --hard`, which can land
    # on a stale LOCAL branch of that name on this box, so the caller needs to
    # see what actually got checked out rather than what they asked for.
    echo "heavy-run: HEAD there is $(git -C "$wt" rev-parse --short HEAD)" >&2
fi

cd "$run_root"

LOCK="${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
claim_path="${HV_CENSUS_CLAIM_PATH:-/tmp/hv-census.claim}"

# Serialize: open the lock fd, then block until it is ours. Closing the fd on
# exit (any exit, including a kill) releases it, so the next queued invocation
# proceeds. Measure the queue wait HERE, before timed.sh starts, so `wall_s`
# stays the work and `waited_s` the queue (decision 0081).
exec 9>"$LOCK"
timeout_s="${HV_HEAVY_WAIT_TIMEOUT:-2700}"
echo "heavy-run: waiting for the box claim ($LOCK; up to ${timeout_s}s) …" >&2
wait_began=$SECONDS
if ! flock -w "$timeout_s" 9; then
    # Bounded, so a wedged holder fails loudly instead of hanging forever.
    echo "heavy-run: TIMED OUT after ${timeout_s}s waiting for the box." >&2
    echo "heavy-run: $(cargo run --quiet --release -p hornvale -- lab claim-status 2>/dev/null || echo 'holder unknown')" >&2
    exit 75
fi
HV_HEAVY_WAITED_S=$((SECONDS - wait_began))
export HV_HEAVY_WAITED_S
if [ "$HV_HEAVY_WAITED_S" -gt 0 ]; then
    echo "heavy-run: box claimed at $(date -Is) after ${HV_HEAVY_WAITED_S}s queued" >&2
else
    echo "heavy-run: box claimed at $(date -Is)" >&2
fi
# Announce the hold so the nested gate-full-heavy.sh does not block against its
# own ancestor: flock is per open-file-description, so a child re-flocking this
# same path on a fresh fd would DEADLOCK against us (decision 0081).
export HV_CENSUS_LOCK_HELD=$$

# Publish the CLAIM the Rust seam reads, so both layers share ONE source of
# truth and `status` is truthful while this wrapper holds the box.
{
    echo "pid=$$"
    echo "host=$(hostname -s 2>/dev/null || echo '-')"
    echo "user=${USER:-unknown}"
    echo "started=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "goldens=$run_root/book/src/laboratory/generated"
    echo "label=heavy"
    echo "ref=$(git -C "$run_root" branch --show-current 2>/dev/null || echo '-')@$(git -C "$run_root" rev-parse --short HEAD 2>/dev/null || echo '-')"
    echo "cmdline=heavy-run.sh $*"
} > "$claim_path"
# Release the claim on EVERY exit path, including the error ones, so a failed
# run never wedges the box.
trap 'rm -f "$claim_path"; echo "heavy-run: finished at $(date -Is)" >&2' EXIT

bash scripts/timed.sh heavy -- bash scripts/gate-full-heavy.sh

echo "heavy-run: heavy tier finished. Review and commit the artifacts HERE —" >&2
echo "heavy-run: this box authors them (decisions 0063/0079):" >&2
git -C "$run_root" diff --stat book/src/laboratory/generated \
    windows/worldgen/tests/fixtures/occupancy.csv >&2 || true
