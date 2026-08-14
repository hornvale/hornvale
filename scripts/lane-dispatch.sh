#!/usr/bin/env bash
# scripts/lane-dispatch.sh — the caller's side. Runs on ANY machine.
# Validates, ssh's, prints a job id, RETURNS. It never waits.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
set_name="${1:?usage: lane-dispatch.sh <set> <full-sha>}"
ref="${2:-}"

host="$(cat "$repo_root/scripts/census-canonical-host.txt")"

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
# shellcheck disable=SC2029  # $set_name/$ref are meant to expand client-side, into the remote command line
job="$(ssh "$host" "cd ~/Projects/hornvale && setsid nohup sh -c \
    'scripts/lane-run.sh $set_name $ref' >/dev/null 2>&1 & echo dispatched")"
echo "lane-dispatch: $job set=$set_name ref=${ref:0:12} host=$host"
echo "lane-dispatch: read it back with 'make lane-log' or 'make lane-status'"
