#!/usr/bin/env bash
# scripts/lane-dispatch.sh — the caller's side. Runs on ANY machine.
# Validates, ssh's, prints a job id, RETURNS. It never waits.
#
# THE GENERAL PROBLEM THIS FILE'S HV_LANE_REMOTE_DIR EXISTS TO SOLVE: any
# remote-dispatch script that `cd`s to the canonical box's PRIMARY checkout
# (`~/Projects/hornvale`, tracking `main`) is unrunnable end-to-end until it
# has merged — the very thing it dispatches (`scripts/lane-run.sh`) does not
# exist there yet. `heavy-run.sh` had this same shape when it was introduced,
# unremarked; a near-miss dispatch from this campaign, mid-review, is what
# surfaced it here. Left alone, every campaign that adds a remote-dispatch
# script re-discovers the wall the same way: a dispatch that "succeeds" by
# printing a job id while the remote side silently no-ops (`nohup ... &
# echo dispatched` backgrounds unconditionally, so the caller sees success
# even when the backgrounded command was "file not found").
#
# THE FIX: HV_LANE_REMOTE_DIR overrides which DIRECTORY on the canonical box
# gets `cd`'d into before the SSH'd command runs. Point it at a scratch
# checkout on the canonical box that already carries the pushed campaign
# branch (e.g. a worktree checked out there ahead of merge) to exercise the
# lane for real before this file lands on main. Unset, it defaults to
# `~/Projects/hornvale` — normal operation is unchanged.
#
# THIS DOES NOT WEAKEN DECISION 0063's HOST GUARANTEE. `require_canonical_host`
# runs on the REMOTE side, inside `lane-run.sh`, downstream of wherever this
# variable pointed — so redirecting it can only ever choose which directory
# ON the canonical box is used. It has no power to choose a different HOST:
# the canonical-host check still runs, on the same box, regardless of this
# variable's value. An env var that redirects the dispatcher reads alarming
# until you see that the guard sits downstream of it, unaffected.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
set_name="${1:?usage: lane-dispatch.sh <set> <full-sha>}"
ref="${2:-}"

host="$(cat "$repo_root/scripts/census-canonical-host.txt")"
remote_dir="${HV_LANE_REMOTE_DIR:-~/Projects/hornvale}"

# A FULL SHA, not a branch name. HV refs feed `reset --hard` over there, which
# can land on a stale local branch of that name. 40 hex characters or refuse.
case "$ref" in
    [0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]*)
        [ "${#ref}" -eq 40 ] || { echo "lane-dispatch: REF must be a full 40-char SHA; got '$ref'" >&2; exit 2; } ;;
    *)  echo "lane-dispatch: REF must be a full 40-char SHA (never a branch name); got '$ref'" >&2; exit 2 ;;
esac

# The SHA must be PUSHED, or the canonical box cannot fetch it. Checking here
# turns a confusing remote failure tens of minutes later into an instant one.
if ! git -C "$repo_root" branch -r --contains "$ref" >/dev/null 2>&1 \
   || [ -z "$(git -C "$repo_root" branch -r --contains "$ref" 2>/dev/null)" ]; then
    echo "lane-dispatch: $ref is not on any remote branch — push first." >&2
    exit 2
fi

grep -v '^#' "$repo_root/scripts/lane-sets.tsv" | awk -F'\t' -v s="$set_name" '$1==s{f=1} END{exit !f}' \
    || { echo "lane-dispatch: no such set '$set_name' in scripts/lane-sets.tsv" >&2; exit 2; }

# setsid + nohup so the job outlives this ssh. Without it a dropped connection
# kills a job that may be an hour into a queue.
# shellcheck disable=SC2029  # $set_name/$ref/$remote_dir are meant to expand client-side, into the remote command line
job="$(ssh "$host" "cd $remote_dir && setsid nohup sh -c \
    'scripts/lane-run.sh $set_name $ref' >/dev/null 2>&1 & echo dispatched")"
echo "lane-dispatch: $job set=$set_name ref=${ref:0:12} host=$host"
echo "lane-dispatch: read it back with 'make lane-log' or 'make lane-status'"
