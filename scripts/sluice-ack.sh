#!/usr/bin/env bash
# scripts/sluice-ack.sh — adjudicate an out-of-band landing on main.
#
# THE MOUTH DEMANDS A HUMAN DECISION AND, UNTIL THIS EXISTED, GAVE THEM NO WAY
# TO RECORD ONE. `sluice-mouth.sh` exits 4 when `origin/main` has moved away
# from the SHA the queue last pushed, with the message "Something landed
# outside the queue. The inductive guarantee is broken until a human decides
# what happened." Only the chamber ever wrote `last-pushed`. So the operator's
# only route was `printf '%s\n' <sha> > "$HV_SLUICE_DIR/last-pushed"` by hand —
# which happened on 2026-08-19 for ca6f34310, and left the next person to hit
# exit 4 reverse-engineering it from a queue note.
#
# What this adds over that bare write, and why each part is here rather than
# left to the operator's care:
#
#   - it SHOWS the range being adjudicated before writing anything, because
#     the exit-4 hazard is not the landing you can see but a second one hiding
#     behind the same base movement;
#   - it REQUIRES a reason, recorded durably, because "a human decided" with
#     no record of what they decided is the same broken guarantee wearing a
#     hat;
#   - it REFUSES a target that is not `origin/main`'s current tip, so an
#     adjudication cannot quietly set the baseline to something that was never
#     actually landed;
#   - it REFUSES when there is nothing to adjudicate, rather than rewriting
#     the file and reporting success — the set-state ghost taught that a
#     no-op reported as success is how state drifts.
#
# It does NOT verify that main is green, and says so rather than implying it:
# that is the adjudicator's job, and the whole point is that a human looked.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
unset GIT_DIR GIT_INDEX_FILE

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
last_pushed_file="$HV_SLUICE_DIR/last-pushed"
ack_log="$HV_SLUICE_DIR/out-of-band.log"
base="${HV_SLUICE_BASE:-origin/main}"

reason="${1:-}"
if [ -z "$reason" ]; then
    cat >&2 <<'USAGE'
usage: sluice-ack.sh "<why this landing is accepted>"

Adjudicates an out-of-band landing: advances the merge queue's baseline to
origin/main's current tip and records who decided what, so `sluice-mouth.sh`
stops returning exit 4.

The reason is mandatory and is stored. Say what you checked, not that you
checked — "book-only commit, docs_consistency 23/23 and mdbook build green"
beats "verified fine".
USAGE
    exit 2
fi

actual="$(git -C "$repo_root" rev-parse "$base")"
expected="$(cat "$last_pushed_file" 2>/dev/null || true)"

if [ -z "$expected" ]; then
    echo "sluice-ack: no last-pushed baseline recorded yet — nothing to adjudicate." >&2
    echo "sluice-ack: the queue writes it on its first successful push." >&2
    exit 1
fi
if [ "$expected" = "$actual" ]; then
    echo "sluice-ack: baseline already matches $base ($actual) — NOTHING TO ADJUDICATE." >&2
    echo "sluice-ack: nothing was written. If the mouth is returning 4, re-run it; it may have been fixed already." >&2
    exit 1
fi

echo "sluice-ack: adjudicating an out-of-band landing on $base" >&2
echo "  queue last pushed: $expected" >&2
echo "  $base is now:      $actual" >&2
echo "  commits between (THIS is what you are accepting):" >&2
git -C "$repo_root" log --format='    %h %an  %s' "$expected..$actual" >&2 || true
count="$(git -C "$repo_root" rev-list --count "$expected..$actual")"
echo "  ($count commit(s))" >&2
echo >&2

mkdir -p "$HV_SLUICE_DIR"
printf '%s\t%s\t%s\t%s\t%s\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$expected" "$actual" "$count" "$reason" >> "$ack_log"
printf '%s\n' "$actual" > "$last_pushed_file"

echo "sluice-ack: baseline advanced $expected -> $actual" >&2
echo "sluice-ack: recorded in $ack_log" >&2
echo "sluice-ack: this asserts a human LOOKED. It does not assert main is green." >&2
