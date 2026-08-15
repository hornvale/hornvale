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
# script re-discovers the wall the same way.
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
#
# A DISPATCH THAT DID NOT START MUST NOT REPORT SUCCESS. The first cut of
# this override PROVED that hazard rather than just naming it: pointed at a
# directory that does not exist on the canonical box, the remote `cd` failed,
# but the failure was inside the backgrounded segment (`cd ... && setsid
# nohup ... & echo dispatched`) — `&` backgrounds unconditionally, so `echo
# dispatched` ran regardless, ssh exited 0, and the caller printed a cheerful
# "dispatched" for a job that never started. This is the same shape as the
# other four checks-that-cannot-go-red this campaign has already found, now
# at the outermost layer, where it would be believed the most.
#
# THE FIX KEEPS THE PREFLIGHT (cd, and confirming the target actually has
# scripts/lane-run.sh) OUTSIDE the backgrounded segment, so its exit status
# is the remote command's own exit status, which becomes ssh's exit status,
# which this script checks explicitly below and refuses to call "dispatched"
# on. Only once the preflight succeeds does anything get backgrounded — and
# starting a background job is itself near-instant (a fork, not a wait), so
# this adds no meaningful latency: the caller still returns in about a
# second, and the job itself still legitimately runs for tens of minutes,
# unobserved, exactly as before.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
set_name="${1:?usage: lane-dispatch.sh <set> <full-sha>}"
ref="${2:-}"

host="$(cat "$repo_root/scripts/census-canonical-host.txt")"
remote_dir="${HV_LANE_REMOTE_DIR:-~/Projects/hornvale}"

# THE QUOTING HAZARD THIS VALIDATES AGAINST: `HV_LANE_REMOTE_DIR=~/foo` typed
# at a shell prompt (unquoted) is expanded by the CALLER's OWN shell before
# this script ever reads it — the '~' never reaches here, only its already-
# resolved local answer does. That is precisely how a "redirect to the
# canonical box" ends up meaning "the Mac's own home directory": the running
# example is HV_LANE_REMOTE_DIR=~/Projects/hornvale-staff-wt silently
# becoming /Users/<you>/Projects/hornvale-staff-wt, a path lefford has never
# heard of, and the ssh preflight above then failing (correctly — the fix
# above closes exactly the case where it wouldn't). Documenting "quote your
# tildes" in a comment relies on the reader having read this comment at the
# moment they need it, which is exactly the condition that failed here once
# already. VALIDATING is stronger and specific to this repo's own topology —
# the canonical box is Linux (`~` there resolves under /home), the caller is
# a Mac (`~` resolves under /Users) — so catching "the override, once fully
# resolved, sits under the CALLER's own $HOME" catches the mistake by
# construction rather than by memory, with one acceptable false-positive
# edge (a caller whose home directory genuinely is a meaningful remote path)
# traded for closing the common, silent one.
if [ -n "${HV_LANE_REMOTE_DIR+x}" ] && [ -n "${HOME:-}" ]; then
    case "$remote_dir" in
        "$HOME" | "$HOME"/*)
            cat >&2 <<EOF
lane-dispatch: HV_LANE_REMOTE_DIR resolved to a path under YOUR OWN \$HOME
($HOME) before this script ever ran:
  HV_LANE_REMOTE_DIR=$remote_dir
That is what an unquoted '~' in a shell assignment does — it expands against
the CALLER's home, not the canonical box's, so this almost certainly is not
the path you meant on '$host'.

Quote it, so the '~' survives to reach the remote host and expands THERE,
against its own \$HOME instead:
  HV_LANE_REMOTE_DIR='~/Projects/hornvale-staff-wt' make lane SET=$set_name REF=$ref

or pass an absolute path that is meaningful on '$host' directly.
EOF
            exit 2
            ;;
    esac
fi

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
#
# THE PREFLIGHT (`cd $remote_dir && [ -x scripts/lane-run.sh ]`) IS DELIBERATELY
# OUTSIDE the `&`-backgrounded segment, and that placement is the whole fix.
# `A && B & echo C` backgrounds `A && B` as one unit — so a naive `cd ... &&
# setsid ... & echo dispatched` reports C's (echo's) exit status, which is
# always 0, no matter how A or B fared. Guarding with `if PREFLIGHT; then
# BACKGROUND-THE-LONG-RUN; echo dispatched; else echo ...>&2; exit 1; fi`
# makes the remote command's own exit status — and so ssh's, and so this
# script's — reflect the PREFLIGHT alone. The preflight is a `cd` and an `[
# -x ]`, both near-instant; only the actual set (which legitimately runs for
# tens of minutes) is backgrounded, so this adds no observable latency.
# shellcheck disable=SC2029  # $set_name/$ref/$remote_dir are meant to expand client-side, into the remote command line
remote_cmd="if cd $remote_dir && [ -x scripts/lane-run.sh ]; then \
setsid nohup sh -c 'scripts/lane-run.sh $set_name $ref' >/dev/null 2>&1 & \
echo dispatched; else \
echo \"lane-dispatch-remote: cd '$remote_dir' failed, or scripts/lane-run.sh is missing/not executable there on '$host' -- NOTHING WAS DISPATCHED\" >&2; \
exit 1; fi"

# Checked explicitly — not left to `set -e`'s implicit abort on the failing
# assignment below — so a failed preflight gets ITS OWN clear, top-level
# message here, on top of whatever the remote side already printed to
# stderr (which passes straight through; only stdout is captured into $job).
# shellcheck disable=SC2029  # $remote_cmd is already fully client-side-expanded by construction, above
if ! job="$(ssh "$host" "$remote_cmd")"; then
    echo "lane-dispatch: remote dispatch FAILED (see the message above) — nothing was queued." >&2
    exit 1
fi
echo "lane-dispatch: $job set=$set_name ref=${ref:0:12} host=$host"
echo "lane-dispatch: read it back with 'make lane-log' or 'make lane-status'"
