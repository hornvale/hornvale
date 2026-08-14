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

# --- the guard REFUSES when the host file is missing ------------------------
# Direction enforced: `an undetermined canonical host is refused, not read as
# a match`. Fail-closed here is STRUCTURAL (checked once, at the shared point
# both entry points read), not incidental on the caller's own `set -e` — this
# is the positive control for that: without the fix, a bare `cat` crash under
# `set -e` would ALSO make this case exit non-zero, so the second assertion
# (the message is the guard's own, not a stack trace) is the part that
# actually distinguishes "handled" from "crashed".
echo "== guard refuses a missing host file =="
missing="$tmp/does-not-exist.txt"
# `if out=$(...); then rc=0; else rc=$?; fi`, not a bare assignment: under
# `set -e` a bare `out="$(cmd)"` whose `cmd` fails aborts THIS script (the
# assignment's own exit status is `cmd`'s), which is exactly what happened
# the first time this case was written. Wrapping it as an `if` condition is
# the one context `set -e` exempts, so a refusal here is data, not a crash.
if out="$(HV_CANONICAL_HOST_FILE="$missing" \
   bash -c '. '"$root"'/scripts/census-canonical-host.sh; require_canonical_host gate' \
   2>&1)"; then
    rc=0
else
    rc=$?
fi
if [ "$rc" -eq 0 ]; then
    bad "guard ACCEPTED a missing host file — it cannot fail closed"
elif printf '%s' "$out" | grep -q 'could not be determined'; then
    ok "guard refuses a missing host file, with its own message"
else
    bad "guard refused a missing host file but not cleanly: $out"
fi

# --- and REFUSES when the host file is present but empty --------------------
# Direction enforced: `an EMPTY canonical host is refused, not treated as
# unset-and-therefore-fine`. This is the case a bare `[ -z ... ]`-less
# comparison would miss silently: two empty strings compare equal.
echo "== guard refuses an empty host file =="
empty="$tmp/empty.txt"
: > "$empty"
if out="$(HV_CANONICAL_HOST_FILE="$empty" \
   bash -c '. '"$root"'/scripts/census-canonical-host.sh; require_canonical_host gate' \
   2>&1)"; then
    rc=0
else
    rc=$?
fi
if [ "$rc" -eq 0 ]; then
    bad "guard ACCEPTED an empty host file — it cannot fail closed"
elif printf '%s' "$out" | grep -q 'could not be determined'; then
    ok "guard refuses an empty host file, with its own message"
else
    bad "guard refused an empty host file but not cleanly: $out"
fi

# --- and the OLD entry point inherits the same fix ---------------------------
# Direction enforced: the fix lives at the SHARED point both functions read,
# so require_canonical_census_host (untouched itself, per the brief) refuses
# too — proving "harden at the shared point" rather than merely asserting it.
echo "== the untouched entry point inherits the fix =="
rc=0
HV_CANONICAL_HOST_FILE="$empty" \
   bash -c '. '"$root"'/scripts/census-canonical-host.sh; require_canonical_census_host census' \
   >/dev/null 2>&1 || rc=$?
if [ "$rc" -eq 0 ]; then
    bad "require_canonical_census_host ACCEPTED an empty host file"
else
    ok "require_canonical_census_host refuses an empty host file too"
fi

if [ "$fails" -ne 0 ]; then
    echo "test-lane: $fails failure(s)" >&2
    exit 1
fi
echo "test-lane: all cases passed"
