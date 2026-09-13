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

# --- the column-count witness stands down for a delivery (The Spillway) ----
# `domesday::anomaly::tests::evaluable_columns_measured_surface_on_the_<N>_column_census`
# carries the census's column count in its NAME and is in the sub-floor roster,
# so a census that grows the registry reds it on the delivery commit — it is
# what refused The Warp's delivery FIRST (spec §1, leg 3). It can only be
# re-stated by a human, so the delivery DEFERS it and the merge demands it.
# TWO-WAY AGREEMENT, not execution: the gate-commit path builds and runs the
# roster, so this reads the pattern OUT of the hook and drives the roster
# script with it, asserting the omitted set is exactly one term.
xpat="$(grep -m1 "^ *subfloor_exclude='" scripts/hooks/pre-commit | sed "s/^[^']*'//; s/'$//")"
if [ -n "$xpat" ]; then ok "read subfloor_exclude from the hook: $xpat"
else bad "could not read subfloor_exclude from scripts/hooks/pre-commit"; fi
without="$(bash scripts/subfloor-roster.sh)"
with="$(HV_SUBFLOOR_EXCLUDE="$xpat" bash scripts/subfloor-roster.sh)"
terms() { printf '%s' "$1" | tr '|' '\n' | grep -c 'test(='; }
n_without="$(terms "$without")"; n_with="$(terms "$with")"
if [ "$((n_without - n_with))" -eq 1 ]; then
    ok "the exclusion omits EXACTLY one roster term ($n_without -> $n_with)"
else bad "the exclusion omitted $((n_without - n_with)) term(s), want 1 — the pattern has rotted or matches too widely"; fi
if printf '%s' "$without" | grep -qE "test\(=[^)]*${xpat}\)"; then
    ok "CONTROL: without the escape the witness IS selected"
else bad "control failed: the witness is not in the roster at all, so the stand-down proves nothing"; fi
if printf '%s' "$with" | grep -qE "${xpat}"; then
    bad "with the escape the witness is STILL selected"
else ok "with the escape the witness is not selected"; fi
same="$(HV_SUBFLOOR_EXCLUDE='no_such_test_zzz_[0-9]+' bash scripts/subfloor-roster.sh)"
if [ "$same" = "$without" ]; then ok "an exclusion matching nothing leaves the filterset byte-identical (rot fails SAFE: the witness runs)"
else bad "a non-matching exclusion changed the filterset"; fi
if printf '%s' "$with" | grep -q '(' && ! printf '%s' "$with" | grep -q 'and not'; then
    ok "the excluded filterset is still FLAT (no wrapping) — subfloor-run-chunked.sh splits on ' | '"
else bad "the excluded filterset is wrapped, which the chunker cannot split"; fi
# THE STAND-DOWN COUNT. Three checks stand down for a delivery: the golden-pins
# guard, the yellow-census alarm, and now the column-count witness. A fourth
# is placed by the rule in decision 0836 and edits this expectation with its
# reason, never silently.
# shellcheck disable=SC2016  # single-quoted on purpose: a literal grep -F pattern, not a shell expansion
n_sd="$(grep -cF 'if [ -n "${HV_CENSUS_DELIVERY:-}" ]' scripts/hooks/pre-commit)"
if [ "$n_sd" = "3" ]; then ok "HV_CENSUS_DELIVERY stands down exactly three checks"
else bad "HV_CENSUS_DELIVERY stand-down branches: $n_sd, want 3"; fi

# --- the docs-tests stand-down, in two-way agreement with the source ---------
#
# DIRECTION THIS CHECK ENFORCES: every test in cli/tests/suite/census_duration.rs
# that renders a VERDICT against a CENSUS_*_SECS threshold is named in the
# hook's delivery stand-down. It does NOT check the converse (a name in the
# exclusion that no longer exists is caught separately below), and it says
# nothing about whether the thresholds themselves are right.
#
# WHY THIS SHAPE AND NOT A LIST OF TWO NAMES. The defect it exists to prevent
# already happened once: the yellow alarm was stood down for a delivery in 2026-09,
# `the_latest_census_is_under_the_refusal_ceiling` was added later reading the
# same latest row of the same file, nobody added it here, and a census slower
# than the ceiling became unable to deliver its own goldens at all -- each
# attempt destroying the previous attempt's staged output in the shared census
# worktree. A hard-coded pair would be satisfied forever while a THIRD threshold
# test walked into the identical trap. Deriving the set from the source is what
# makes the next one loud.
#
# A threshold VERDICT is distinguished from an instrument test by whether it
# compares against a CENSUS_*_SECS constant -- `the_chronologically_latest_row_
# wins_even_when_it_is_not_last_in_the_file` calls the same helper on synthetic
# rows and is correctly NOT excluded, because a delivery can satisfy it.
echo "== docs-tests stand-down: every threshold verdict is deferred for a delivery"
cd_src="cli/tests/suite/census_duration.rs"
dte="$(grep -m1 '^ *docs_tests_exclude="test(' scripts/hooks/pre-commit | sed 's/^[^"]*"//; s/"$//')"
if [ -n "$dte" ]; then ok "read docs_tests_exclude from the hook"
else bad "could not read docs_tests_exclude from scripts/hooks/pre-commit"; fi

verdicts="$(awk '/^fn /{n=$0; sub(/^fn /,"",n); sub(/\(.*/,"",n)}
                 /CENSUS_[A-Z]+_SECS/{if(n!="" && !seen[n]++) print n}' "$cd_src")"
n_v="$(printf '%s\n' "$verdicts" | grep -c .)"
if [ "$n_v" -ge 2 ]; then
    ok "found $n_v threshold verdict test(s) in census_duration.rs"
else
    bad "found $n_v threshold verdicts — the derivation has rotted, so the checks below prove nothing"
fi
for v in $verdicts; do
    if printf '%s' "$dte" | grep -qF "test($v)"; then
        ok "the delivery stand-down covers $v"
    else
        bad "$v compares against a CENSUS_*_SECS threshold but is NOT stood down for a delivery — a census that trips it cannot deliver its own goldens, and each retry destroys the last one's staged output"
    fi
done

# THE CONVERSE. A name in the exclusion that no longer exists in the source
# silences nothing and hides that the roster has rotted.
for nm in $(printf '%s' "$dte" | tr ' ' '\n' | sed -n 's/^test(\(.*\))$/\1/p'); do
    if grep -q "^fn $nm(" "$cd_src"; then
        ok "excluded name $nm still exists in the source"
    else
        bad "the exclusion names $nm, which no longer exists in $cd_src"
    fi
done

# THE CONTROL. Without it, a grep that matched anything would satisfy every
# assertion above.
if grep -q "^fn no_such_census_test_zzz(" "$cd_src"; then
    bad "control failed: a name that cannot exist was found in the source"
else
    ok "CONTROL: a name that does not exist is not found (the existence check can fail)"
fi

# AND THE INSTRUMENT TESTS MUST STILL RUN. Standing down the whole module would
# satisfy everything above while deferring checks a delivery CAN satisfy --
# over-deferral is the failure mode on the other side of this rule.
for keep in the_census_ledger_has_rows_this_test_can_read \
            the_chronologically_latest_row_wins_even_when_it_is_not_last_in_the_file; do
    if printf '%s' "$dte" | grep -qF "test($keep)"; then
        bad "$keep is stood down, but a delivery CAN satisfy it — the stand-down is over-broad"
    else
        ok "$keep still runs during a delivery"
    fi
done

printf '\ntest-census-guard: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
