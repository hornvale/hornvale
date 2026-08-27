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

# --- the HV_CENSUS_DELIVERY escape -------------------------------------------
# The pattern tests above are static; these two EXECUTE the hook, because the
# escape is a control-flow branch and a pattern cannot see it. A census refresh
# moves the fixture BY DEFINITION while the pins can only be re-pinned after the
# run, so the guard fires on exactly the commit a census delivery must be allowed
# to make. sluice-census.sh sets the escape for that one commit; if this branch
# ever stops working, census delivery silently stops (it did, on 2026-08-27:
# the commit was refused, an unchanged HEAD was pushed, and DELIVERED was
# printed anyway).
#
# Staged path is the .sql pin, deliberately NOT a .rs one: a staged Rust path
# would also take the hook's gate-commit branch and muddy what is being asserted.
scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT
# Scrub git's environment: an inherited GIT_DIR/GIT_INDEX_FILE would point these
# commands at the REAL repository. That is not hypothetical — it staged a file
# and landed a junk commit on main here once.
hook_env() { env -u GIT_DIR -u GIT_INDEX_FILE -u GIT_WORK_TREE -u GIT_OBJECT_DIRECTORY "$@"; }
hook_env git init -q "$scratch" 2>/dev/null
mkdir -p "$scratch/tools/census/queries/calibrate"
echo "-- a pin" > "$scratch/tools/census/queries/calibrate/golden-pins.sql"
hook_env git -C "$scratch" add tools/census/queries/calibrate/golden-pins.sql

hook="$PWD/scripts/hooks/pre-commit"
out_esc="$(cd "$scratch" && hook_env HV_CENSUS_DELIVERY=1 bash "$hook" 2>&1)" || true
out_bare="$(cd "$scratch" && hook_env bash "$hook" 2>&1)" || true

if printf '%s' "$out_esc" | grep -q 'skipping the golden-pins guard'; then
    ok "HV_CENSUS_DELIVERY=1 stands the guard down"
else
    bad "HV_CENSUS_DELIVERY=1 did NOT stand the guard down — census delivery cannot commit"
fi
if printf '%s' "$out_esc" | grep -q "running 'make census-check'"; then
    bad "escape set, yet the hook still ran census-check — the branch is not short-circuiting"
else
    ok "escape set: census-check is not attempted"
fi
# The control. Without this the test above passes even if the guard never fires
# at all, which would make it a tautology rather than evidence.
if printf '%s' "$out_bare" | grep -q "running 'make census-check'"; then
    ok "CONTROL: without the escape the guard still fires on the same staged pin"
else
    bad "control failed: the guard did not fire without the escape, so the escape test proves nothing"
fi

printf '\ntest-census-guard: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
