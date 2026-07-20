#!/usr/bin/env bash
# scripts/census-run.sh — run a census on THIS box, the single canonical
# platform for census goldens (decision 0063).
#
# Concurrent invocations — from either development machine, over SSH — serialize
# on a file lock, so runs QUEUE one-at-a-time instead of contending for the
# machine. Only this box authors goldens: a second machine that isn't
# byte-identical would produce ~0.1%-divergent census values (the discrete-count
# platform flips 0063 documents), so it triggers runs here rather than running
# its own.
#
# Usage — locally, or from the other machine via SSH:
#   scripts/census-run.sh                    # regenerate the canonical census
#                                            # goldens (~7 min), then review/commit
#   scripts/census-run.sh <study.json> ...   # run one or more specific studies
#   HV_CENSUS_REF=<git-ref> scripts/census-run.sh [...]
#                                            # fetch + run against that ref in a
#                                            # dedicated scratch worktree
#
#   # from the other machine (push your branch first, then):
#   ssh <thisbox> 'cd ~/Projects/hornvale && HV_CENSUS_REF=my-branch scripts/census-run.sh'
#
# The lock makes several such triggers queue behind each other automatically —
# no daemon, no scheduler; whoever holds the lock runs, the rest wait.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOCK="${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"

# Serialize: open the lock fd, then block until it is ours. Closing the fd on
# exit (any exit) releases it, so the next queued invocation proceeds.
exec 9>"$LOCK"
echo "census-run: waiting for the census lock ($LOCK) …" >&2
flock 9
echo "census-run: lock acquired at $(date -Is)" >&2
trap 'echo "census-run: finished at $(date -Is)" >&2' EXIT

run_root="$repo_root"
if [ -n "${HV_CENSUS_REF:-}" ]; then
    # Run a specific ref in a dedicated worktree, so the caller's own checkout
    # (and this canonical one) are left untouched. Path is outside the repo to
    # keep `git status` clean here.
    wt="${HV_CENSUS_WORKTREE:-$repo_root/../hornvale-census-wt}"
    git -C "$repo_root" fetch --all --quiet
    if [ -d "$wt/.git" ] || git -C "$repo_root" worktree list --porcelain | grep -qF "$wt"; then
        git -C "$wt" fetch --all --quiet
        git -C "$wt" checkout --force "$HV_CENSUS_REF"
        git -C "$wt" reset --hard "$HV_CENSUS_REF" --quiet
    else
        git -C "$repo_root" worktree add --force "$wt" "$HV_CENSUS_REF"
    fi
    run_root="$wt"
    echo "census-run: running ref '$HV_CENSUS_REF' in $wt" >&2
fi

cd "$run_root"
if [ "$#" -eq 0 ]; then
    echo "census-run: regenerating the canonical census goldens (HV_CENSUS=1, ~7 min) …" >&2
    HV_CENSUS=1 bash scripts/regenerate-artifacts.sh
    echo "census-run: goldens regenerated — review 'git diff book/src/laboratory/generated' and commit (this box is canonical, decision 0063)." >&2
else
    for study in "$@"; do
        echo "census-run: lab run $study …" >&2
        cargo run --release --quiet -p hornvale -- lab run "$study"
    done
fi
