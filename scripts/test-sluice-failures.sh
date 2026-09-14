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
printf '%s' "$out" | grep -q "failing tests (5 distinct)" \
    && ok "it counts the distinct failures" || bad "wrong distinct count"
printf '%s' "$out" | grep -q "ok: the list is not shorter" \
    && ok "the cross-check passes and says so" || bad "no cross-check line"

echo "== THE DEFECT: a list shorter than the run reported must REFUSE"
# 20 failures reported, only 7 FAIL lines present -- precisely the shape a
# `tail -8` produces, and precisely what was reported as fact on 2026-09-14.
mklog "$tmp/truncated.log" 20 7
if bash "$S" "$tmp/truncated.log" >"$tmp/t.out" 2>&1; then
    bad "a TRUNCATED list exited 0 — this is the whole defect, printed as if it were the whole list"
else
    ok "a truncated list is refused (exit non-zero)"
fi
grep -q "REFUSING" "$tmp/t.out" && ok "the refusal says REFUSING" || bad "no REFUSING in the output"
grep -q "subset wearing the whole list" "$tmp/t.out" \
    && ok "it names what is wrong with quoting the list anyway" \
    || bad "the refusal does not warn against quoting the subset"

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
    printf '%s' "$out" | grep -q "cross-check below cannot run" \
        && ok "it says the cross-check cannot run" || bad "silent about the missing summary"
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
[ "$fixed" = 2 ] && ok "FIXED names the two that stopped failing" || bad "FIXED=$fixed, want 2"
[ "$newf"  = 2 ] && ok "NEW names the two that started failing"   || bad "NEW=$newf, want 2"
[ "$still" = 1 ] && ok "STILL names the one that never stopped"   || bad "STILL=$still, want 1"
printf '%s' "$out" | grep -q "mod::a" && ok "the fixed set is named, not just counted" || bad "no names in the diff"

echo "== a truncated PREVIOUS log is refused too"
if bash "$S" "$tmp/good.log" "$tmp/truncated.log" >"$tmp/p.out" 2>&1; then
    bad "a truncated previous log was accepted — the diff would silently invent FIXED entries"
else
    ok "a truncated previous log is refused (it would fabricate FIXED entries)"
fi

printf '\ntest-sluice-failures: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
