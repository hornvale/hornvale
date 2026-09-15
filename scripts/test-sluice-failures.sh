#!/usr/bin/env bash
# Tests for scripts/sluice-failures.sh.
#
# THE LOAD-BEARING TEST IS THE REFUSAL. This script exists because a truncated
# failure list reads exactly like a complete one, so the only property worth
# having is that an under-length list STOPS rather than prints. Everything else
# here is convenience.
set -u
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
S="$root/scripts/sluice-failures.sh"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT

mklog() {  # mklog <file> <reported-failed> <how-many-FAIL-lines-to-write>
    local f="$1" reported="$2" lines="$3" i=0
    : > "$f"
    while [ "$i" -lt "$lines" ]; do
        printf '        FAIL [   1.000s] (%d/100) crate::suite mod::test_%d\n' "$i" "$i" >> "$f"
        i=$((i + 1))
    done
    printf '     Summary [ 100.000s] 100 tests run: %d passed, %d failed, 0 skipped\n' \
        $((100 - reported)) "$reported" >> "$f"
}

echo "== a healthy log: the list matches what the run reported"
mklog "$tmp/good.log" 5 5
if out="$(bash "$S" "$tmp/good.log" 2>&1)"; then
    ok "a complete log exits 0"
else
    bad "a complete log was refused: $out"
fi
if printf '%s' "$out" | grep -q "failing tests (5 distinct)"; then
    ok "it counts the distinct failures"
else
    bad "wrong distinct count"
fi
if printf '%s' "$out" | grep -q "ok: the list is not shorter"; then
    ok "the cross-check passes and says so"
else
    bad "no cross-check line"
fi

echo "== THE DEFECT: a list shorter than the run reported must REFUSE"
# 20 failures reported, only 7 FAIL lines present -- precisely the shape a
# `tail -8` produces, and precisely what was reported as fact on 2026-09-14.
mklog "$tmp/truncated.log" 20 7
if bash "$S" "$tmp/truncated.log" >"$tmp/t.out" 2>&1; then
    bad "a TRUNCATED list exited 0 — this is the whole defect, printed as if it were the whole list"
else
    ok "a truncated list is refused (exit non-zero)"
fi
if grep -q "REFUSING" "$tmp/t.out"; then
    ok "the refusal says REFUSING"
else
    bad "no REFUSING in the output"
fi
if grep -q "subset wearing the whole list" "$tmp/t.out"; then
    ok "it names what is wrong with quoting the list anyway"
else
    bad "the refusal does not warn against quoting the subset"
fi

echo "== a log with several nextest invocations uses the LARGEST reported count"
mklog "$tmp/multi.log" 3 9
printf '     Summary [  10.000s] 50 tests run: 41 passed, 9 failed, 0 skipped\n' >> "$tmp/multi.log"
if bash "$S" "$tmp/multi.log" >/dev/null 2>&1; then
    ok "9 extracted against a max of 9 is accepted"
else
    bad "a multi-summary log was wrongly refused"
fi
mklog "$tmp/multi2.log" 3 4
printf '     Summary [  10.000s] 50 tests run: 41 passed, 9 failed, 0 skipped\n' >> "$tmp/multi2.log"
if bash "$S" "$tmp/multi2.log" >/dev/null 2>&1; then
    bad "4 extracted against a max of 9 was accepted — it is reading the wrong Summary"
else
    ok "it takes the LARGEST failed count as the floor, not the first"
fi

echo "== no summary at all: say so, do not crash and do not pretend"
printf '        FAIL [ 1.000s] (1/2) crate::suite mod::only\n' > "$tmp/nosum.log"
if out="$(bash "$S" "$tmp/nosum.log" 2>&1)"; then
    if printf '%s' "$out" | grep -q "cross-check below cannot run"; then
        ok "it says the cross-check cannot run"
    else
        bad "silent about the missing summary"
    fi
else
    bad "a log with no summary exited non-zero"
fi

echo "== membership: a count is not a membership"
# Same total either side (3 -> 3), entirely different members.
: > "$tmp/before.log"
for t in a b c; do printf '        FAIL [1.0s] (1/9) crate::suite mod::%s\n' "$t" >> "$tmp/before.log"; done
printf '     Summary [1.0s] 9 tests run: 6 passed, 3 failed, 0 skipped\n' >> "$tmp/before.log"
: > "$tmp/after.log"
for t in c d e; do printf '        FAIL [1.0s] (1/9) crate::suite mod::%s\n' "$t" >> "$tmp/after.log"; done
printf '     Summary [1.0s] 9 tests run: 6 passed, 3 failed, 0 skipped\n' >> "$tmp/after.log"
out="$(bash "$S" "$tmp/after.log" "$tmp/before.log" 2>&1)"
fixed="$(printf '%s' "$out" | sed -n '/FIXED/,/NEW/p' | grep -c 'mod::' || true)"
newf="$(printf '%s' "$out"  | sed -n '/NEW/,/STILL/p'  | grep -c 'mod::' || true)"
still="$(printf '%s' "$out" | sed -n '/STILL/,$p'      | grep -c 'mod::' || true)"
if [ "$fixed" = 2 ]; then
    ok "FIXED names the two that stopped failing"
else
    bad "FIXED=$fixed, want 2"
fi
if [ "$newf"  = 2 ]; then
    ok "NEW names the two that started failing"
else
    bad "NEW=$newf, want 2"
fi
if [ "$still" = 1 ]; then
    ok "STILL names the one that never stopped"
else
    bad "STILL=$still, want 1"
fi
if printf '%s' "$out" | grep -q "mod::a"; then
    ok "the fixed set is named, not just counted"
else
    bad "no names in the diff"
fi

echo "== a LIBTEST log is read, not reported as zero failures"
# THE DEFECT THIS FILE EXISTED TO PREVENT AND DID NOT. Two harnesses run in the
# chamber: the workspace phases use nextest (`FAIL [ ... ] name`), and the
# `clients` phase runs plain `cargo test`, because the clients are outside the
# cargo workspace. libtest writes `test name ... FAILED` and
# `test result: FAILED. P passed; F failed` instead, and this reader saw
# neither.
#
# On 2026-09-15 campaign/the-tidemark died at `clients` with FOUR failing
# portolan tests and this tool reported ZERO, printing "ok: the list is not
# shorter than the run reported" underneath — because with no nextest summary
# the cross-check compared 0 against 0 and was satisfied. A confident, silent,
# wrong all-clear from the guard against exactly that.
mklibtest() {  # mklibtest <file> <reported-failed> <how-many-FAILED-lines>
    {
        printf 'running 293 tests\n'
        i=0; while [ "$i" -lt "$3" ]; do
            printf 'test driver::portolan_tests::case_%s ... FAILED\n' "$i"
            i=$((i+1))
        done
        printf 'test result: FAILED. 289 passed; %s failed; 0 ignored; 0 measured\n' "$2"
    } > "$1"
}
mklibtest "$tmp/libtest.log" 4 4
out="$(bash "$root/scripts/sluice-failures.sh" "$tmp/libtest.log" 2>&1)"; rc=$?
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"(4 distinct)"*) ok "reads libtest failure names" ;; *) bad "did not read libtest names: $out" ;; esac
case "$out" in *"count in a Summary line: 4"*) ok "reads libtest's reported count" ;; *) bad "did not read libtest's count" ;; esac
if [ "$rc" -eq 0 ]; then
    ok "a complete libtest list is not refused"
else
    bad "refused a complete libtest list (rc=$rc)"
fi

echo "== a TRUNCATED libtest log is refused, like a nextest one"
# Anti-vacuity: the case above must not pass because the reader accepts
# anything. Report 9 failures, write 3 names.
mklibtest "$tmp/libtrunc.log" 9 3
set +e
out="$(bash "$root/scripts/sluice-failures.sh" "$tmp/libtrunc.log" 2>&1)"; rc=$?
set -e
case "$rc" in 0) bad "accepted a libtest list shorter than the run reported" ;; *) ok "refuses a short libtest list (rc=$rc)" ;; esac
case "$out" in *REFUSING*) ok "says it is refusing" ;; *) bad "refused without saying so" ;; esac

echo "== a MIXED log counts both harnesses"
# The real chamber shape: nextest phases then a libtest clients phase.
cat "$tmp/good.log" "$tmp/libtest.log" > "$tmp/mixed.log"
out="$(bash "$root/scripts/sluice-failures.sh" "$tmp/mixed.log" 2>&1)"
case "$out" in *"(9 distinct)"*) ok "counts nextest and libtest names together (5+4)" ;; *) bad "lost one harness in a mixed log: $out" ;; esac

echo "== a truncated PREVIOUS log is refused too"
if bash "$S" "$tmp/good.log" "$tmp/truncated.log" >"$tmp/p.out" 2>&1; then
    bad "a truncated previous log was accepted — the diff would silently invent FIXED entries"
else
    ok "a truncated previous log is refused (it would fabricate FIXED entries)"
fi

printf '\ntest-sluice-failures: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
