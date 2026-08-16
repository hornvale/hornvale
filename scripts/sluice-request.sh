#!/usr/bin/env bash
# scripts/sluice-request.sh — the caller's side. Runs on ANY machine.
# Validates, ssh's, prints a request id, RETURNS. It never waits.
#
# Same shape as scripts/lane-dispatch.sh, including its two hard-won guards:
# a REF must be a full 40-char SHA (it feeds checkout on the far end), and the
# remote preflight stays OUTSIDE the backgrounded segment so a dispatch that
# did not start cannot report success. This script has no backgrounded
# segment of its own (the remote side, `sluice-queue.sh add`, is a fast,
# lock-protected TSV write, not a long-running job), but the same "a request
# that did not land must not print success" discipline applies: the ssh call
# below is checked explicitly and nothing is echoed as queued unless it
# actually returned an id.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
branch="${1:?usage: sluice-request.sh <branch> <full-sha>}"
ref="${2:?usage: sluice-request.sh <branch> <full-sha>}"
host="$(cat "$repo_root/scripts/census-canonical-host.txt")"
remote_dir="${HV_SLUICE_REMOTE_DIR:-~/Projects/hornvale}"

case "$ref" in
    [0-9a-f]*) [ "${#ref}" -eq 40 ] || { echo "sluice-request: REF must be a full 40-char SHA; got '$ref'" >&2; exit 2; } ;;
    *) echo "sluice-request: REF must be a full 40-char SHA (never a branch name); got '$ref'" >&2; exit 2 ;;
esac

if [ -z "$(git -C "$repo_root" branch -r --contains "$ref" 2>/dev/null)" ]; then
    echo "sluice-request: $ref is not on any remote branch — push first." >&2
    exit 2
fi

# shellcheck disable=SC2029  # meant to expand client-side into the remote command
remote_cmd="if cd $remote_dir && [ -x scripts/sluice-queue.sh ]; then \
scripts/sluice-queue.sh add '$branch' '$ref'; else \
echo \"sluice-request-remote: cd '$remote_dir' failed or sluice-queue.sh missing on '$host' -- NOTHING WAS QUEUED\" >&2; \
exit 1; fi"

# shellcheck disable=SC2029
if ! req="$(ssh "$host" "$remote_cmd")"; then
    echo "sluice-request: remote enqueue FAILED — nothing was queued." >&2
    exit 1
fi
echo "sluice-request: $req branch=$branch ref=${ref:0:12} host=$host"
echo "sluice-request: read it back with 'make sluice-status'"
