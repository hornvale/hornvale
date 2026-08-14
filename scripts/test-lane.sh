#!/usr/bin/env bash
# scripts/test-lane.sh — the lane's own tests.
#
# DIRECTION: each case below states which direction it enforces. Note in
# particular that the refusal case is a POSITIVE CONTROL — asserting the guard
# PASSES on the canonical host proves nothing about whether it can refuse.
set -euo pipefail
root="$(git rev-parse --show-toplevel)"
fails=0
skips=0
passed=0

ok()   { printf 'ok   %s\n' "$*"; passed=$((passed+1)); }
bad()  { printf 'FAIL %s\n' "$*" >&2; fails=$((fails+1)); }
# A case group that cannot run on this host (no flock) is neither a pass nor
# a failure — it is its own outcome, printed unambiguously so the closing
# line can report it separately rather than folding it silently into
# "passed". Every case group below must call ok/bad/skip at least once: a
# header with nothing after it is exactly the "did this run?" ambiguity this
# helper exists to remove.
skip() { printf 'SKIP %s\n' "$*"; skips=$((skips+1)); }

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


# --- census-run.sh's nested-acquisition guard: proceeds under a held ancestor,
# --- genuinely contends otherwise -------------------------------------------
# Direction enforced, BOTH ways: `HV_CENSUS_LOCK_HELD` naming a LIVE ancestor
# pid makes census-run.sh SKIP its own flock and proceed, because `lane-run.sh`
# (and gate-full-heavy.sh's nested regenerate-artifacts.sh call) may already
# hold `/tmp/hv-census.lock` on a different fd, and flock is per open-file-
# description — a second acquisition on a fresh fd would deadlock against its
# own ancestor, bounded only by the wait timeout (2700s by default; this is
# what "make lane SET=census" self-deadlocked into before this guard existed:
# 45 minutes burned, then a TIMED OUT message pointing at a job that was never
# there). A test that only proved the GUARDED path proceeds would not have
# caught this — the guard could vacuously skip flock on every path, guarded
# or not, and still pass. So the UNGUARDED case must be shown to actually
# contend too, not merely "not crash".
#
# A short HV_CENSUS_WAIT_TIMEOUT (2s) is the lever that makes the unguarded
# case affordable to assert here at all, instead of the real 2700s default.
#
# `flock` itself is faked, not the real utility: this box (macOS) ships none,
# same as `setsid`, so the census-canonical-host.sh guard is instead asked
# WHETHER it invoked flock (a marker file written on every call) rather than
# relying on real advisory-lock contention — that is precisely the
# control-flow question the bug is about ("does the guard skip acquisition"),
# and it is what the real bug on lefford (real flock, real deadlock) reduces
# to at this level.
echo "== census-run.sh: nested acquisition under a held ancestor proceeds; unguarded genuinely contends =="
mkdir -p "$tmp/census-bin" "$tmp/census-scripts"
printf '%s\n' "$(hostname -s)" > "$tmp/census-host.txt"

# ALWAYS reports contended (exit 1) after honoring `-w <seconds>` — this lets
# the unguarded case be asserted as REAL contention (it waits out the full
# bounded timeout and then fails), not merely "returned nonzero somehow".
cat > "$tmp/census-bin/flock" <<'EOF'
#!/usr/bin/env sh
echo "CALLED $*" >> "$FAKE_FLOCK_MARKER"
if [ "$1" = "-w" ]; then
    sleep "$2"
fi
exit 1
EOF
chmod +x "$tmp/census-bin/flock"

# Only reached by the TIMED-OUT branch's claim-status fallback; faked so the
# test never pays for a real cargo build.
cat > "$tmp/census-bin/cargo" <<'EOF'
#!/usr/bin/env sh
echo "fake-claim-status: test double"
exit 0
EOF
chmod +x "$tmp/census-bin/cargo"

# The prelude under test: census-run.sh's OWN source, verbatim, up to (not
# including) `run_root="$repo_root"` — everything after that point is the
# actual census/regen work this test has no business paying for. Written to a
# real script FILE (not `bash -c "$str"`) so `${BASH_SOURCE[0]}` populates
# naturally; census-canonical-host.sh copied alongside so the prelude's own
# `$(dirname "$0")/census-canonical-host.sh` source line resolves.
awk '/^run_root="\$repo_root"/{exit} {print}' "$root/scripts/census-run.sh" \
    > "$tmp/census-scripts/census-run.sh"
cp "$root/scripts/census-canonical-host.sh" "$tmp/census-scripts/census-canonical-host.sh"
chmod +x "$tmp/census-scripts/census-run.sh"

# -- unguarded: no live ancestor named, so it must actually wait out the
#    bounded timeout and then fail loudly (rc=75), proving real contention.
marker_unguarded="$tmp/census-marker-unguarded"
: > "$marker_unguarded"
if out="$(PATH="$tmp/census-bin:$PATH" HV_CANONICAL_HOST_FILE="$tmp/census-host.txt" \
   HV_CENSUS_LOCK="$tmp/hv-census-test.lock" HV_CENSUS_WAIT_TIMEOUT=2 \
   FAKE_FLOCK_MARKER="$marker_unguarded" \
   bash "$tmp/census-scripts/census-run.sh" 2>&1)"; then
    rc=0
else
    rc=$?
fi
if [ "$rc" -eq 75 ] && grep -q '^CALLED' "$marker_unguarded"; then
    ok "unguarded nested call genuinely contends (calls flock, times out rc=75) — the guarded case below is not vacuous"
else
    bad "unguarded nested call did not contend as expected (rc=$rc, marker=[$(cat "$marker_unguarded")]): $out"
fi

# -- guarded: HV_CENSUS_LOCK_HELD names a live pid ($$ — this test process is
#    always alive), so it must proceed WITHOUT ever calling flock.
marker_guarded="$tmp/census-marker-guarded"
: > "$marker_guarded"
if out="$(PATH="$tmp/census-bin:$PATH" HV_CANONICAL_HOST_FILE="$tmp/census-host.txt" \
   HV_CENSUS_LOCK="$tmp/hv-census-test.lock" HV_CENSUS_WAIT_TIMEOUT=2 \
   FAKE_FLOCK_MARKER="$marker_guarded" HV_CENSUS_LOCK_HELD="$$" \
   bash "$tmp/census-scripts/census-run.sh" 2>&1)"; then
    rc=0
else
    rc=$?
fi
if [ "$rc" -eq 0 ] && [ ! -s "$marker_guarded" ] \
   && printf '%s' "$out" | grep -q "already claimed by ancestor pid $$"; then
    ok "nested acquisition under a held ancestor proceeds instead of deadlocking (flock never called)"
else
    bad "nested acquisition under a held ancestor did not proceed cleanly (rc=$rc, marker=[$(cat "$marker_guarded")]): $out"
fi

# --- the claim EXCLUDES ------------------------------------------------------
# Direction enforced: `while one holder has the claim, a second acquirer waits`.
# It does NOT prove the first holder ever releases; the ordering case below
# covers that. Skipped where there is no flock (macOS ships none).
echo "== the claim excludes =="
if ! command -v flock >/dev/null 2>&1; then
    skip "no flock on $(uname -s) — skipping (this case is meaningful on the canonical box)"
else
    lock="$tmp/staff.lock"
    ( exec 9>"$lock"; flock 9; sleep 3 ) &
    holder=$!
    sleep 0.5
    if flock -w 1 -E 99 "$lock" -c true; then
        bad "a second acquirer took a HELD claim — the lane does not serialize"
    else
        ok "a second acquirer is refused while the claim is held"
    fi
    wait $holder
fi

# --- ordering is FIFO --------------------------------------------------------
# Direction enforced: `waiters are granted in arrival order`. This pins a
# MEASURED property of this kernel's flock, which is why lane-run.sh keeps no
# ticket sequence of its own. If this ever goes red, the lane needs a ticket
# spool and the spec's section 6 has to be revisited.
echo "== ordering is FIFO =="
if command -v flock >/dev/null 2>&1; then
    lock="$tmp/order.lock"; out="$tmp/order.log"; : > "$out"
    ( exec 9>"$lock"; flock 9; sleep 2 ) &
    holder=$!
    sleep 0.3
    for i in 1 2 3 4 5 6 7 8; do
        ( flock "$lock" -c "echo w$i >> '$out'" ) &
    done
    wait $holder; wait
    if [ "$(tr '\n' ' ' < "$out")" = "w1 w2 w3 w4 w5 w6 w7 w8 " ]; then
        ok "eight simultaneous waiters were granted in arrival order"
    else
        bad "grant order was not FIFO: $(tr '\n' ' ' < "$out")"
    fi
else
    skip "no flock on $(uname -s) — skipping (this case is meaningful on the canonical box)"
fi

if [ "$fails" -ne 0 ]; then
    echo "test-lane: $fails failure(s), $passed passed, $skips skipped" >&2
    exit 1
fi
if [ "$skips" -ne 0 ]; then
    echo "test-lane: $passed passed, $skips skipped (no flock)"
else
    echo "test-lane: $passed passed"
fi
