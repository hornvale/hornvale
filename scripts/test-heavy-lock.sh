#!/usr/bin/env bash
# scripts/test-heavy-lock.sh — prove the heavy claim EXCLUDES, not merely that
# it exists. Asserting a lock is present is not asserting it serialises; this
# repo has shipped tests that assert nothing, so each case here is one that
# would FAIL if the lock were a no-op.
#
# `flock` is util-linux and is NOT shipped with macOS. The claim only ever
# matters on the canonical box (a 40-core Linux machine whose other jobs are
# long), so on a box without flock this test SKIPS rather than fails — a
# skip is honest, a green pass on a machine that never ran the assertions
# would not be.
set -uo pipefail
# `|| exit`: this script does not set -e, so a failed cd would otherwise carry
# on in the wrong directory (SC2164).
cd "$(git rev-parse --show-toplevel)" || exit 1

if ! command -v flock >/dev/null 2>&1; then
    echo "test-heavy-lock: SKIP — no flock on this host ($(uname -s));"
    echo "test-heavy-lock:        the claim is only load-bearing on the canonical box."
    exit 0
fi

LOCK="$(mktemp -u /tmp/hv-siding-test-XXXXXX.lock)"
fail=0

# --- case 1: a second acquirer is refused while the lock is held -------------
( exec 9>"$LOCK"; flock 9; sleep 3 ) &
holder=$!
sleep 0.5

if ( exec 9>"$LOCK"; flock -w 1 9 ); then
    echo "FAIL: second acquirer got the lock while it was held" >&2
    fail=1
else
    echo "ok: second acquirer was excluded while the lock was held"
fi

wait "$holder"

# --- case 2: the lock is free once the holder exits normally -----------------
if ( exec 9>"$LOCK"; flock -w 5 9 ); then
    echo "ok: lock released when the holder exited"
else
    echo "FAIL: lock still held after the holder exited" >&2
    fail=1
fi

# --- case 3: a KILLED holder releases it too ---------------------------------
# This is why flock is the right primitive over a claim file alone: the kernel
# frees the fd on process death, so a -9'd run cannot wedge the box.
( exec 9>"$LOCK"; flock 9; sleep 30 ) &
killed=$!
sleep 0.5
kill -9 "$killed" 2>/dev/null
wait "$killed" 2>/dev/null

if ( exec 9>"$LOCK"; flock -w 5 9 ); then
    echo "ok: lock released when the holder was killed"
else
    echo "FAIL: lock wedged after the holder was killed" >&2
    fail=1
fi

rm -f "$LOCK"
if [ "$fail" -eq 0 ]; then
    echo "test-heavy-lock: PASS"
else
    echo "test-heavy-lock: FAIL" >&2
fi
exit "$fail"
