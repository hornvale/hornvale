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
# Refuse outright on any box but the canonical one: this script's whole premise
# is that goldens come from one machine. This shell guard covers every SHELL
# entry point (this script, and the HV_CENSUS=1 branch of
# regenerate-artifacts.sh); `cargo run -p hornvale -- lab run <census study>`
# bypasses shell entirely, so it carries its own guard in Rust
# (windows/lab/src/census_guard.rs, invoked from publish()) reading the same
# scripts/census-canonical-host.txt this one does (decision 0063).
# `status` answers "is a census running right now?" without ps | grep
# (decision 0081). Handled before the host guard and the lock: asking is not
# authoring, so it is legal from any machine and must never block.
if [ "${1:-}" = "status" ]; then
    cargo run --quiet --release -p hornvale -- lab claim-status
    exit $?
fi

# shellcheck source=scripts/census-canonical-host.sh
. "$(dirname "$0")/census-canonical-host.sh"
require_canonical_census_host || exit 1

exec 9>"$LOCK"
timeout_s="${HV_CENSUS_WAIT_TIMEOUT:-2700}"
echo "census-run: waiting for the census lock ($LOCK; up to ${timeout_s}s) …" >&2
# Measure the queue wait here: this is where it actually happens, before
# timed.sh starts, so `wall_s` stays the work and `waited_s` the queue
# (decision 0081).
wait_began=$SECONDS
if ! flock -w "$timeout_s" 9; then
    # Bounded, so a wedged holder fails loudly instead of hanging forever
    # (decision 0081). Report WHO, not just that we gave up.
    echo "census-run: TIMED OUT after ${timeout_s}s waiting for the census lock." >&2
    echo "census-run: $(cargo run --quiet --release -p hornvale -- lab claim-status 2>/dev/null || echo 'claim holder unknown')" >&2
    exit 75
fi
HV_CENSUS_WAITED_S=$((SECONDS - wait_began))
export HV_CENSUS_WAITED_S
if [ "$HV_CENSUS_WAITED_S" -gt 0 ]; then
    echo "census-run: lock acquired at $(date -Is) after ${HV_CENSUS_WAITED_S}s queued" >&2
else
    echo "census-run: lock acquired at $(date -Is)" >&2
fi
# Announce the hold so the nested regenerate-artifacts.sh -> lab run path does
# not block against its own ancestor. flock is per open-file-description, so a
# child re-flocking this same path on a fresh fd would DEADLOCK against us
# (decision 0081).
export HV_CENSUS_LOCK_HELD=$$
trap 'echo "census-run: finished at $(date -Is)" >&2' EXIT

run_root="$repo_root"
if [ -n "${HV_CENSUS_REF:-}" ]; then
    # Run a specific ref in a dedicated worktree, so the caller's own checkout
    # (and this canonical one) are left untouched. Path is outside the repo to
    # keep `git status` clean here.
    # Resolve the path: `git worktree list` prints REAL paths, so an
    # unresolved `$repo_root/../hornvale-census-wt` never matches the grep
    # below. `pwd -P`, not plain `pwd`: the logical form still carries any
    # symlink in the path, which `git worktree list` will have resolved away.
    # The `if` keeps the unresolved form for the not-yet-created case — the
    # `cd` fails, the assignment never happens, and `$wt` is left alone.
    wt="${HV_CENSUS_WORKTREE:-$repo_root/../hornvale-census-wt}"
    if wt_resolved="$(cd "$wt" 2>/dev/null && pwd -P)"; then
        wt="$wt_resolved"
    fi
    git -C "$repo_root" fetch --all --quiet
    # `-e`, not `-d`: a linked worktree's `.git` is a FILE (a gitdir pointer),
    # never a directory. With `-d` this test is always false, and with the
    # unresolved path above the grep was always false too — so both guards
    # failed together and the `else` branch ran `worktree add` over an existing
    # worktree, which is fatal. That is what blocked The Tolerance's census.
    if [ -e "$wt/.git" ] || git -C "$repo_root" worktree list --porcelain | grep -qF "$wt"; then
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

# Publish the CLAIM the Rust seam reads, so both layers share ONE source of
# truth. Without this the flock and the claim are invisible to each other: a
# bare `cargo run -p hornvale -- lab run studies/the-census.study.json` checks
# only the claim file, finds none during a wrapper-driven regen, and runs
# CONCURRENTLY — the exact hole decision 0081 exists to close. It also makes
# `census-run.sh status` truthful while the wrapper is the one holding the
# box. Nested runs still skip acquisition via HV_CENSUS_LOCK_HELD, so this
# cannot deadlock against itself.
claim_path="${HV_CENSUS_CLAIM_PATH:-/tmp/hv-census.claim}"
{
    echo "pid=$$"
    echo "host=$(hostname -s 2>/dev/null || echo '-')"
    echo "user=${USER:-unknown}"
    echo "started=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "goldens=$run_root/book/src/laboratory/generated"
    echo "label=census-run"
    echo "ref=$(git -C "$run_root" branch --show-current 2>/dev/null || echo '-')@$(git -C "$run_root" rev-parse --short HEAD 2>/dev/null || echo '-')"
    echo "cmdline=census-run.sh $*"
} > "$claim_path"
# Replaces the earlier EXIT trap: release the claim on every exit path,
# including the error ones, so a failed regen never wedges the box.
trap 'rm -f "$claim_path"; echo "census-run: finished at $(date -Is)" >&2' EXIT

if [ "$#" -eq 0 ]; then
    echo "census-run: regenerating the canonical census goldens (HV_CENSUS=1, ~7 min) …" >&2
    HV_CENSUS=1 bash scripts/timed.sh census -- bash scripts/regenerate-artifacts.sh
    echo "census-run: goldens regenerated — review 'git diff book/src/laboratory/generated' and commit (this box is the canonical one, decision 0063)." >&2
else
    for study in "$@"; do
        echo "census-run: lab run $study …" >&2
        cargo run --release --quiet -p hornvale -- lab run "$study"
    done
fi
