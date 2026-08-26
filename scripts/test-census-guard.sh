#!/usr/bin/env bash
# scripts/test-census-guard.sh — the golden-pins tripwire fires on BOTH operands.
#
# WHY THIS TEST EXISTS. `tools/census/queries/calibrate/golden-pins.sql`
# duplicates every pinned calibration constant and checks it against the
# committed census. `make census-check` is in no gate, so `scripts/hooks/
# pre-commit` guards it with a trigger list — and that list has now been wrong
# once in a way that let 28 pins drift onto main.
#
# A PIN COMPARES TWO OPERANDS: a value COMPUTED FROM THE CENSUS, and a hardcoded
# LITERAL in Rust or SQL. The trigger originally named only the literal-side
# files, so a census refresh moved `computed` for every pin while staging none
# of them, the hook fired on nothing, and the drift landed (c54fb62c9,
# 2026-08-25 — the third staleness after 2026-07-13 and 2026-07-20).
#
# DIRECTION THIS TEST ENFORCES: both operands trigger. It asserts the literal
# side AND the census side each match, and that an unrelated path does NOT —
# because a trigger widened until it fires on everything is a 148-second tax on
# every commit, which is how a guard gets deleted rather than fixed.
set -uo pipefail
cd "$(git rev-parse --show-toplevel)" || exit 1
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }

# Read the pattern out of the hook itself rather than restating it: a second
# copy here would drift from the hook exactly as the hook drifted from reality.
pat="$(grep -m1 '^census_guard_files=' scripts/hooks/pre-commit | sed "s/^census_guard_files='//; s/'$//")"
[ -n "$pat" ] || { echo "  FAIL: could not read census_guard_files from the hook"; exit 1; }
ok "read the trigger pattern from scripts/hooks/pre-commit"

fires() { printf '%s\n' "$1" | grep -qE "^(${pat})$"; }

for f in windows/lab/tests/suite/calibration.rs \
         windows/lab/tests/suite/branches_family_calibration.rs \
         windows/lab/tests/suite/gathering_calibration.rs \
         tools/census/queries/calibrate/golden-pins.sql; do
    if fires "$f"; then ok "LITERAL side triggers: $f"
    else bad "literal side does NOT trigger: $f"; fi
done

for f in book/src/laboratory/generated/the-census/rows.csv \
         book/src/laboratory/generated/census-of-the-meeting/rows.csv; do
    if fires "$f"; then ok "COMPUTED side triggers: $f"
    else bad "census rows do NOT trigger: $f — a census refresh would move every pin's computed operand with the hook firing on nothing, which is how 28 pins drifted onto main at c54fb62c9"; fi
done

for f in README.md windows/lab/src/metrics.rs book/src/domesday/index.md \
         book/src/laboratory/generated/the-census/schema.json; do
    if fires "$f"; then bad "trigger is too broad — fires on $f, taxing unrelated commits ~148s"
    else ok "unrelated path does not trigger: $f"; fi
done

printf '\ntest-census-guard: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
