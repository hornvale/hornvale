#!/usr/bin/env bash
# scripts/sluice-run.sh — the chamber. Runs ON the canonical box.
#
# Takes the SAME shared claim the census and the heavy tier take (decisions
# 0081/0086/0133) — one job, not six, so the queue wait is paid once. Over the
# lane's first 27.4 h, 14.2 h of 21.2 h wall time was queue wait (67%),
# because a campaign gate used to be six separate dispatches.
#
# DETACHED HEAD IS NOT A WORKAROUND. scripts/hooks/pre-commit refuses a commit
# to 'main' from a linked worktree, and this chamber is a linked worktree that
# commits regenerated artifacts. The guard keys on
# `git_dir != git_common_dir && branch == "main"` where
# `branch="$(git symbolic-ref --short -q HEAD || echo DETACHED)"`, so detached
# is exempt by construction. It is also the honest description: the chamber
# does not own main, it builds a candidate and offers it. main moves only when
# the push in Task 5's continuation of this script succeeds.
#
# HERMETICITY: every git call below runs under
# `env -u GIT_DIR -u GIT_INDEX_FILE`, the same discipline
# scripts/sluice-queue.sh and scripts/sluice-mouth.sh use — git exports
# GIT_DIR/GIT_INDEX_FILE to hooks and they OUTRANK `-C`/cwd, so an unscrubbed
# call from a linked worktree can silently operate on a different repository
# (scripts/CLAUDE.md's board incident).
set -euo pipefail

# Overridable so tests can point the chamber at a throwaway scratch repo
# instead of this one — every other path this script touches (the worktree,
# the roster, the phase list, the claim, the lock) is already overridable the
# same way (HV_SLUICE_WORKTREE, HV_SLUICE_LANE_SETS, HV_SLUICE_PHASES,
# HV_CENSUS_CLAIM_PATH, HV_CENSUS_LOCK, HV_CANONICAL_HOST_FILE); this is the
# one that was missing, and without it a test cannot avoid touching this
# repository's own origin/main and worktree registry.
repo_root="${HV_SLUICE_REPO_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
branch="${1:?usage: sluice-run.sh <branch> <full-sha>}"
sha="${2:?usage: sluice-run.sh <branch> <full-sha>}"

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
mkdir -p "$HV_SLUICE_DIR"
job_id="sluice-$(printf '%.12s' "$sha")-$(date -u +%Y%m%dT%H%M%SZ)"
run_log="$HV_SLUICE_DIR/$job_id.log"
jobs_tsv="$HV_SLUICE_DIR/jobs.tsv"
began=$SECONDS
waited_s=""
phase_failed=""

# jobs.tsv columns: when / job / branch / sha / rc / wall_s / waited_s /
# phase_failed. wall_s is this SCRIPT's own elapsed time since $began,
# including queue wait; waited_s isolates the queue portion alone (the same
# split scripts/lane-run.sh's own jobs.tsv makes). phase_failed is empty on a
# clean run and otherwise names the phase (or "merge") that reddened.
record() {
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$job_id" "$branch" "$sha" \
        "$1" "$((SECONDS - began))" "$waited_s" "$phase_failed" >> "$jobs_tsv"
}
trap 'record $?' EXIT

# shellcheck source=scripts/census-canonical-host.sh
. "$repo_root/scripts/census-canonical-host.sh"
require_canonical_host sluice || exit 1

exec >>"$run_log" 2>&1
echo "sluice-run: $job_id started $(date -Is) on $(hostname -s) as pid $$"

# The roster of phases and what each one runs — the SAME single source of
# truth scripts/lane-run.sh reads (scripts/lane-sets.tsv), never a second
# copy (cli/tests/lane_sets.rs fails on one). Both the roster file and the
# phase LIST are overridable so tests can drive the chamber with trivial
# stand-in phases instead of the real six-suite roster.
lane_sets_file="${HV_SLUICE_LANE_SETS:-$repo_root/scripts/lane-sets.tsv}"
phases="${HV_SLUICE_PHASES:-artifacts outboard gate seam-guard clients heavy}"
roster_col() {
    grep -v '^#' "$lane_sets_file" | awk -F'\t' -v s="$1" -v c="$2" '$1==s{print $c}'
}
cmd_for() { roster_col "$1" 5; }
# Read from the roster's own `authors` column (yes/no), not a hardcoded name
# list — a hardcoded copy is exactly the drift cli/tests/lane_sets.rs exists
# to fail on, and scripts/lane-run.sh's own `authors_col` already sets this
# precedent (it derives the claim's `goldens` field the same way, from the
# same column, rather than re-deriving or hardcoding it).
authors() { [ "$(roster_col "$1" 4)" = "yes" ]; }

LOCK="${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
claim_path="${HV_CENSUS_CLAIM_PATH:-/tmp/hv-census.claim}"
exec 9>"$LOCK"
timeout_s="${HV_SLUICE_WAIT_TIMEOUT:-7200}"
echo "sluice-run: queued for the staff ($LOCK; up to ${timeout_s}s) …"
wait_began=$SECONDS
if ! flock -w "$timeout_s" 9; then
    echo "sluice-run: TIMED OUT after ${timeout_s}s waiting for the staff." >&2
    exit 75
fi
waited_s=$((SECONDS - wait_began))
echo "sluice-run: holds the staff at $(date -Is) after ${waited_s}s queued"

# `goldens` reflects whether ANY phase in THIS run's roster authors
# artifacts, derived the same way `authors()` is (roster column 4), not
# hardcoded — a claim that lies about whether it writes goldens would be
# believed by every "is the box busy?" reader.
goldens="no"
for p in $phases; do
    if authors "$p"; then
        goldens="yes"
        break
    fi
done

# THE EIGHT-FIELD SHAPE, VERBATIM (Task 0's finding). `parse_claim`
# (windows/lab/src/census_claim.rs:100-129) requires all eight of
# pid/host/user/started/goldens/label/ref/cmdline via `?` — omit any one and
# every "is the box busy?" reader (`census-run.sh status`, `make
# heavy-status`, `lab claim-status`) reports "no heavy run in progress" while
# this job holds the box. scripts/lane-run.sh shipped missing two of these
# (goldens, cmdline) for exactly this reason; `cli/tests/
# lane_claim_roundtrip.rs` scrapes this file too now, so a future drift here
# fails loudly instead of silently. `job` is a ninth, informational field
# `parse_claim` does not require; it is here purely so a human reading the
# raw claim file can find this job's log without decoding `label`.
{
    echo "pid=$$"
    echo "host=$(hostname -s)"
    echo "user=${USER:-unknown}"
    echo "started=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "goldens=$goldens"
    echo "label=sluice:$branch"
    echo "ref=$sha"
    echo "cmdline=phases=$phases"
    echo "job=$job_id"
} > "$claim_path"
# shellcheck disable=SC2154  # code is assigned first thing inside this same trap string
trap 'code=$?; rm -f "$claim_path"; echo "sluice-run: finished $(date -Is) rc=$code"; record "$code"' EXIT

# The chamber's own worktree — NOT the lane's shared scratch tree, which every
# lane dispatch `checkout --force`s and `reset --hard`s. A dedicated tree
# always builds main-plus-a-delta, so its warm target/ stays hot and the
# measured 771 s cold build is paid once, ever.
wt="${HV_SLUICE_WORKTREE:-$repo_root/../hornvale-sluice-wt}"
base_ref="${HV_SLUICE_BASE:-origin/main}"
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" fetch --all --quiet
base_sha="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" rev-parse "$base_ref")"
if [ -e "$wt/.git" ]; then
    env -u GIT_DIR -u GIT_INDEX_FILE git -C "$wt" fetch --all --quiet
    env -u GIT_DIR -u GIT_INDEX_FILE git -C "$wt" checkout --force --detach "$base_sha"
    env -u GIT_DIR -u GIT_INDEX_FILE git -C "$wt" reset --hard "$base_sha" --quiet
    # -fd, NEVER -fdx: target/ is gitignored, and -x would destroy this box's
    # ~15 GB warm build cache, turning every chamber job into a cold build.
    env -u GIT_DIR -u GIT_INDEX_FILE git -C "$wt" clean -fd --quiet
else
    env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" worktree add --force --detach "$wt" "$base_sha"
fi
cd "$wt"
echo "sluice-run: detached at $(env -u GIT_DIR -u GIT_INDEX_FILE git rev-parse --short HEAD) ($base_ref)"

export HV_CENSUS_LOCK_HELD=$$

# THE MERGE COMMIT MESSAGE IS AN ARTIFACT, not decoration. tools/census/
# history.sh tags every committed census snapshot with an epoch label taken
# from `git log --follow --first-parent main -- <path>` — and under --no-ff
# the campaign's own commit is no longer on that line, so THIS subject
# becomes the label. It must read correctly as a census epoch label standing
# alone.
campaign="${branch#campaign/}"
headline="${HV_SLUICE_HEADLINE:-$(env -u GIT_DIR -u GIT_INDEX_FILE git log -1 --format=%s "$sha")}"
if ! env -u GIT_DIR -u GIT_INDEX_FILE git merge --no-ff --no-edit -m "merge($campaign): $headline

Gated as the merge product by sluice job $job_id.
main was $base_sha at test time." "$sha"; then
    echo "sluice-run: MERGE CONFLICT — holding. A human resolves this." >&2
    phase_failed="merge"
    env -u GIT_DIR -u GIT_INDEX_FILE git merge --abort || true
    exit 10
fi
merge_sha="$(env -u GIT_DIR -u GIT_INDEX_FILE git rev-parse HEAD)"
echo "sluice-run: merge product is $merge_sha"

# PHASE ORDER IS BY EXPECTED TIME-TO-RED, not by tree hygiene. `git clean -fd`
# between phases makes cleanliness free, which frees the order to optimise for
# detecting the class distinctive to a MERGE PRODUCT — interaction with main,
# which only `gate` and `artifacts` see. `heavy` is last because at a measured
# mean 1678 s it is 47% of the set's ~3602 s.
#
# NEVER `-fdx`: target/ is gitignored and -x destroys the ~15 GB warm cache.
#
# Cleaning between phases also closes by construction the bug that made
# seam-guard return rc=2 "refusing to run on a dirty working tree" in five of
# its first six lane runs: earlier sets in the same dispatch dirtied the tree,
# and rc=2 reads as "found survivors", so the breakage looked like a finding
# for a month.
for phase in $phases; do
    line="$(cmd_for "$phase")"
    [ -n "$line" ] || { echo "sluice-run: no such set '$phase' in $lane_sets_file" >&2; exit 2; }
    echo "sluice-run: === phase $phase ==="
    # Task 1's verdict decides whether `heavy` gates or reports. If it
    # REPORTS, replace this block for that one phase with a warning that does
    # not exit.
    if ! bash "$repo_root/scripts/timed.sh" "sluice:$phase" -- sh -c "$line"; then
        echo "sluice-run: PHASE $phase FAILED — holding." >&2
        phase_failed="$phase"
        exit 11
    fi
    if authors "$phase" && [ -n "$(env -u GIT_DIR -u GIT_INDEX_FILE git status --porcelain)" ]; then
        env -u GIT_DIR -u GIT_INDEX_FILE git add -A
        env -u GIT_DIR -u GIT_INDEX_FILE git commit -q -m "chore(artifacts): regenerate after $phase

Authored on the canonical host inside sluice job $job_id (decision 0079)."
        echo "sluice-run: committed artifact drift from $phase"
    fi
    env -u GIT_DIR -u GIT_INDEX_FILE git clean -fd --quiet
done

echo "sluice-run: all phases green at $(env -u GIT_DIR -u GIT_INDEX_FILE git rev-parse --short HEAD). Task 5's drift assertion and push pick up from here."
