#!/usr/bin/env bash
# scripts/test-lane.sh — the lane's own tests.
#
# DIRECTION: each case below states which direction it enforces. Note in
# particular that the refusal case is a POSITIVE CONTROL — asserting the guard
# PASSES on the canonical host proves nothing about whether it can refuse.
set -euo pipefail
root="$(git rev-parse --show-toplevel)"
fails=0

note() { printf '  %s\n' "$*"; }
ok()   { printf 'ok   %s\n' "$*"; }
bad()  { printf 'FAIL %s\n' "$*" >&2; fails=$((fails+1)); }

# --- the guard REFUSES off-host (positive control) --------------------------
# Direction enforced: `a non-canonical host is refused`. Driven by overriding
# the canonical-host file, because we cannot change `hostname` in a test.
echo "== guard refuses off-host =="
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
printf 'definitely-not-this-host\n' > "$tmp/host.txt"
if HV_CANONICAL_HOST_FILE="$tmp/host.txt" \
   bash -c '. '"$root"'/scripts/census-canonical-host.sh; require_canonical_host gate' \
   >/dev/null 2>&1; then
    bad "guard ACCEPTED a non-canonical host — it cannot fail closed"
else
    ok "guard refuses a non-canonical host"
fi

# --- and ACCEPTS on the canonical host --------------------------------------
echo "== guard accepts on-host =="
printf '%s\n' "$(hostname -s)" > "$tmp/host.txt"
if HV_CANONICAL_HOST_FILE="$tmp/host.txt" \
   bash -c '. '"$root"'/scripts/census-canonical-host.sh; require_canonical_host gate' \
   >/dev/null 2>&1; then
    ok "guard accepts the canonical host"
else
    bad "guard refused the canonical host — it fails closed on everything"
fi

if [ "$fails" -ne 0 ]; then
    echo "test-lane: $fails failure(s)" >&2
    exit 1
fi
echo "test-lane: all cases passed"
