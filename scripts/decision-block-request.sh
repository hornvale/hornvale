#!/usr/bin/env bash
# scripts/decision-block-request.sh — reserve a decision-number block from any
# machine. The caller's side; the allocator itself runs on the canonical box.
#
# Same topology as sluice-request.sh, and for the same reason: an allocator
# must have exactly ONE authority, and campaigns run on several machines. This
# ssh's to the canonical box, which holds the flock and the ledger, and prints
# the reserved range.
#
# FAILS CLOSED. If the ssh fails, nothing is printed and the exit status is
# non-zero. A campaign that cannot reach the allocator must NOT fall back to
# picking a number locally — that is precisely the behaviour blocks exist to
# remove, and a silent fallback would reintroduce it at the worst moment.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
name="${1:?usage: decision-block-request.sh <campaign>}"
host="$(cat "$repo_root/scripts/census-canonical-host.txt")"
remote_dir="${HV_BLOCK_REMOTE_DIR:-~/Projects/hornvale}"

# shellcheck disable=SC2029  # meant to expand client-side into the remote command
remote_cmd="if cd $remote_dir && [ -x scripts/decision-block.sh ]; then \
scripts/decision-block.sh take '$name'; else \
echo \"decision-block-remote: cd '$remote_dir' failed or decision-block.sh missing on '$host' -- NOTHING WAS RESERVED\" >&2; \
exit 1; fi"

# shellcheck disable=SC2029
if ! out="$(ssh "$host" "$remote_cmd")"; then
    echo "decision-block-request: remote reservation FAILED — nothing was reserved." >&2
    echo "  Do NOT pick a number locally; that is the failure blocks remove." >&2
    exit 1
fi
start="$(printf '%s' "$out" | awk '{print $1}')"
end="$(printf '%s' "$out" | awk '{print $2}')"
echo "decision-block-request: $name reserved $start-$end on $host"
echo "  Author your records as docs/decisions/$start-<slug>.md upward."
echo "  The range is yours; gaps inside it are fine and cost nothing."
