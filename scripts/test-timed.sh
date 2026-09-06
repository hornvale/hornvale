#!/usr/bin/env bash
# test-timed.sh — the timing wrapper must never veto the command it times.
#
# WHY THIS FILE EXISTS. On 2026-09-06 campaign/the-warrant's merge was reported
# CHAMBER RED rc=11 at the `artifacts` phase. The phase had SUCCEEDED — its
# last line is "regenerate-artifacts: done." after the full seed-42 regen, the
# domesday survey and the anomaly report. What failed was the stopwatch: two
# mktemp files vanished during the ~290 s run, `read` left its variables unset,
# and `set -u` aborted on the first expansion. A campaign paid a slot on a
# strictly serial box for a defect in the instrument.
#
# The cause of the disappearance is still unknown and this suite does not
# depend on it. It pins the property that makes the cause academic: an absent
# measurement is a missing row, never a failed command.
set -uo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
pass=0; fail=0
ok()  { echo "  ok: $1"; pass=$((pass+1)); }
bad() { echo "  FAIL: $1"; fail=$((fail+1)); }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
export HV_TIMINGS_LEDGER="$tmp/timings.md"

echo "== timed: a vanished tempfile does not change the exit code"
# The command deletes every mktemp file in its own TMPDIR, reproducing what
# happened to the-warrant deterministically instead of by timing.
TMPDIR="$tmp" bash "$root/scripts/timed.sh" probe -- bash -c "rm -f $tmp/tmp.*; exit 7" >/dev/null 2>"$tmp/e7"
rc=$?
if [ "$rc" -eq 7 ]; then ok "a failing command still reports its own rc (7), not the wrapper's"
else bad "expected rc=7, got $rc"; fi

TMPDIR="$tmp" bash "$root/scripts/timed.sh" probe -- bash -c "rm -f $tmp/tmp.*; exit 0" >/dev/null 2>"$tmp/e0"
rc=$?
if [ "$rc" -eq 0 ]; then ok "THE REGRESSION — a SUCCEEDING command whose timing vanished still reports 0"
else bad "a successful command was failed by its own stopwatch: rc=$rc — this is the-warrant's red"; fi

if grep -q "NOT recorded" "$tmp/e0"; then ok "the loss is announced rather than silent"
else bad "no notice that the timing was dropped: $(cat "$tmp/e0")"; fi

echo "== timed: nothing is written to the ledger when the timing is unreadable"
if [ ! -s "$HV_TIMINGS_LEDGER" ] || ! grep -q "| probe |" "$HV_TIMINGS_LEDGER" 2>/dev/null; then
    ok "no malformed row was appended (a '?' row would break the census-duration reader)"
else bad "a row was written for an unreadable timing: $(grep '| probe |' "$HV_TIMINGS_LEDGER")"; fi

echo "== timed: ANTI-VACUITY — an ordinary run still records"
TMPDIR="$tmp" bash "$root/scripts/timed.sh" probe -- true >/dev/null 2>"$tmp/eok"
rc=$?
if [ "$rc" -eq 0 ] && grep -q "(recorded) rc=0" "$tmp/eok"; then
    ok "a healthy run is still measured and recorded — the guard above is not blanket suppression"
else bad "a healthy run stopped recording: rc=$rc $(cat "$tmp/eok")"; fi
if grep -q "| probe |" "$HV_TIMINGS_LEDGER" 2>/dev/null; then
    ok "the recorded row lands in the ledger the seam points at, not the tracked one"
else bad "no row in $HV_TIMINGS_LEDGER"; fi

echo "== timed: the seam is a TEST seam — production is unset"
if ! grep -rn "HV_TIMINGS_LEDGER" "$root/scripts" "$root/Makefile" 2>/dev/null | grep -vE "test-timed.sh|timed.sh:" | grep -q .; then
    ok "nothing in production sets HV_TIMINGS_LEDGER"
else bad "HV_TIMINGS_LEDGER is set somewhere in production: $(grep -rn "HV_TIMINGS_LEDGER" "$root/scripts" "$root/Makefile" | grep -vE "test-timed.sh|timed.sh:")"; fi

printf '\ntest-timed.sh: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
