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

# HV_CENSUS_WORKTREE, WHEN SET, MUST BE ABSOLUTE (decision 0146). Checked HERE
# — before the lock — because of the canal-lock rule this project already
# applies at the merge queue's mouth: turn a vessel away at the gate, never
# inside the chamber. A malformed override is a caller error knowable without
# touching the serial box, and the census lock's wait is measured in tens of
# minutes.
#
# WHY THE BOUND EXISTS. The value is used as a RAW path, so a relative one
# resolves against the caller's cwd — and the documented invocation runs
# `cd ~/Projects/hornvale` first. This project's own documented value for two
# months was `canonical`, which therefore created the worktree INSIDE the
# repo, defeating the invariant this script states about its own default
# ("outside the repo to keep `git status` clean here") and leaving an
# untracked, un-ignored directory holding a registered worktree — which a
# `git clean -fdx` in the main checkout would delete out from under git.
#
# THE REFUSAL NAMES ONLY THE BOUND ACTUALLY ENFORCED: absoluteness. It does
# NOT claim to enforce "outside the repo", because it does not test that — an
# absolute path inside the repo is still accepted, deliberately, so the
# override survives as a test seam. A refusal that named a bound it did not
# enforce would be a false claim in an error message.
if [ -n "${HV_CENSUS_WORKTREE:-}" ]; then
    case "$HV_CENSUS_WORKTREE" in
        /*) ;;
        *)
            echo "census-run: HV_CENSUS_WORKTREE must be an ABSOLUTE path; got '$HV_CENSUS_WORKTREE'." >&2
            echo "census-run: a relative value resolves against the caller's cwd, which the documented" >&2
            echo "census-run: invocation sets to the repo root — landing the worktree inside the repo." >&2
            echo "census-run: leave it UNSET to use the default (a sibling of the main checkout)." >&2
            exit 2
            ;;
    esac
fi

# WHERE A CENSUS WOULD RUN. One definition, used by the run path below and by
# the `worktree` subcommand, so a test can assert the resolution WITHOUT
# paying for a census — the alternative was asserting on a copy of this
# expression, which is the shape that lets the copy and the original drift.
#
# THE DEFAULT IS ANCHORED TO THE MAIN WORKTREE, NOT $repo_root. Invoked from a
# linked worktree, $repo_root IS that worktree, so `$repo_root/..` would
# resolve inside `.claude/worktrees/` — a different directory per campaign,
# silently, and each one a fresh cold build. `git worktree list --porcelain`
# lists the main worktree FIRST (git's own ordering);
# scripts/scheduled/nightly-census.sh and scripts/worktree-take.sh already
# rely on exactly this resolution.
census_worktree_path() {
    local main_root
    main_root="$(git -C "$repo_root" worktree list --porcelain \
        | awk '/^worktree /{print $2; exit}')"
    [ -n "$main_root" ] || main_root="$repo_root"
    printf '%s\n' "${HV_CENSUS_WORKTREE:-$main_root/../hornvale-census-wt}"
}

# Asking where is not authoring, so — like `status` — this takes no lock and
# is legal from any box. Placed BEFORE the host guard for that reason.
if [ "${1:-}" = "worktree" ]; then
    census_worktree_path
    exit 0
fi

# shellcheck source=scripts/census-canonical-host.sh
. "$(dirname "$0")/census-canonical-host.sh"
require_canonical_census_host census || exit 1


# An ANCESTOR already holds this lock (HV_CENSUS_LOCK_HELD names a live pid):
# flock is per open-file-description, so re-flocking the same path on a fresh
# fd would DEADLOCK against our own parent — bounded, so it would not hang
# forever, but it would burn the full ${HV_CENSUS_WAIT_TIMEOUT:-2700}s and
# then fail with a message that sends the reader hunting for another job that
# was never there. This is the same re-entrancy guard gate-full-heavy.sh and
# regenerate-artifacts.sh already carry; mirror them rather than inventing a
# third shape.
held_by="${HV_CENSUS_LOCK_HELD:-}"
if [ -n "$held_by" ] && kill -0 "$held_by" 2>/dev/null; then
    echo "census-run: box already claimed by ancestor pid $held_by — proceeding" >&2
    HV_CENSUS_WAITED_S=0
else
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
    if [ "$HV_CENSUS_WAITED_S" -gt 0 ]; then
        echo "census-run: lock acquired at $(date -Is) after ${HV_CENSUS_WAITED_S}s queued" >&2
    else
        echo "census-run: lock acquired at $(date -Is)" >&2
    fi
    # Announce the hold so the nested regenerate-artifacts.sh -> lab run path
    # does not block against its own ancestor (this same guard, one level down).
    export HV_CENSUS_LOCK_HELD=$$
fi
export HV_CENSUS_WAITED_S
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
    wt="$(census_worktree_path)"
    if wt_resolved="$(cd "$wt" 2>/dev/null && pwd -P)"; then
        wt="$wt_resolved"
    fi
    # FETCH origin EXPLICITLY IF `--all` FAILS. A second remote was added to this
# repository (tangled.org) on 2026-09-07, and `--all` contacts every remote:
# an unreachable secondary now aborts this script under `set -e`, BEFORE the
# merge, with rc=1 — a code outside this script's own vocabulary, which the
# drain records as "attribution pending" and blames the candidate for. Found
# by a test that added a deliberately-broken remote and watched a chamber run
# die at rc=1 immediately after its queue-row line. Falling back to origin
# keeps the failure fatal only when the remote the chamber actually needs is
# unreachable.
git -C "$repo_root" fetch --all --quiet || git -C "$repo_root" fetch origin --quiet
    # `-e`, not `-d`: a linked worktree's `.git` is a FILE (a gitdir pointer),
    # never a directory. With `-d` this test is always false, and with the
    # unresolved path above the grep was always false too — so both guards
    # failed together and the `else` branch ran `worktree add` over an existing
    # worktree, which is fatal. That is what blocked The Tolerance's census.
    if [ -e "$wt/.git" ] || git -C "$repo_root" worktree list --porcelain | grep -qF "$wt"; then
        git -C "$wt" fetch --all --quiet || git -C "$wt" fetch origin --quiet
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
    # Report this run's wall against the alarm threshold NOW, while an operator
    # is still watching and on the box that measured it. The Rust ratchet reads
    # docs/timings.md on main, and since the census joined the merge queue a
    # run's row lands on its census/* DELIVERY BRANCH — so the ratchet cannot
    # see a run until a campaign merges that branch. On 2026-08-27 that meant
    # three of four runs past the threshold while the ledger on main still
    # showed a two-day-old figure as its most recent census.
    # Reads the wall back out of the row timed.sh just wrote, so the number
    # alarmed on is the number recorded, not a second measurement of it.
    census_wall="$(grep '| census |' docs/timings.md 2>/dev/null | tail -1 \
                   | awk -F'|' '{gsub(/ /, "", $4); print $4}')"
    if [ -n "${census_wall:-}" ]; then
        bash scripts/census-duration-alarm.sh "$census_wall" || true
    fi
    echo "census-run: goldens regenerated — review 'git diff book/src/laboratory/generated' and commit (this box is the canonical one, decision 0063)." >&2
else
    for study in "$@"; do
        echo "census-run: lab run $study …" >&2
        cargo run --release --quiet -p hornvale -- lab run "$study"
    done
fi
