#!/usr/bin/env bash
# scripts/test-sluice-census.sh — the census delivery classification.
#
# WHY THIS FILE EXISTS. sluice-census.sh's "NO GOLDENS MOVED" arm was
# UNREACHABLE for the whole life of the script, and nothing noticed because
# nothing ever ran it. The staged set always contains the run's own
# docs/timings.md row (the general `add -u`, which is deliberate — The
# Governor's Task 7 made that row durable after 27 heavy runs wrote rows
# nothing committed), so `git diff --cached --quiet` was never true. Every
# census announced "goldens delivered", including the two on 2026-09-02 whose
# delivery branches contained exactly one path: docs/timings.md.
#
# THE NULL ARM IS THE LOAD-BEARING TEST HERE, and that is the whole point. A
# silent arm is indistinguishable from a working one, so the arm that was dead
# is the one that needs a positive control most. These tests drive a REAL git
# index rather than a pure function over a path list, because the defect lived
# in the interaction with staging, not in the classification logic.
set -uo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }

# shellcheck source=/dev/null
HV_CENSUS_LIB=1 . "$root/scripts/sluice-census.sh"

if ! declare -f census_golden_count >/dev/null 2>&1; then
    echo "test-sluice-census: HV_CENSUS_LIB=1 did not expose census_golden_count" >&2
    exit 1
fi

# A scratch repo, never the real one. `git -C` alone does NOT scope which
# repository git acts on — GIT_DIR is exported into hooks and outranks -C, the
# lesson tools/board/src/git.rs carries — so the location vars are scrubbed.
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
g() { env -u GIT_DIR -u GIT_INDEX_FILE -u GIT_WORK_TREE -u GIT_COMMON_DIR git -C "$tmp" "$@"; }
g init -q 2>/dev/null
g config user.name "census test" 2>/dev/null
g config user.email "census@test" 2>/dev/null
mkdir -p "$tmp/docs" "$tmp/book/src/laboratory/generated/the-census"
printf 'baseline\n' > "$tmp/docs/timings.md"
printf 'seed,value\n1,1\n' > "$tmp/book/src/laboratory/generated/the-census/rows.csv"
g add -A 2>/dev/null
g commit -q -m baseline 2>/dev/null

count_via_lib() { census_golden_count "$tmp"; }

# --- ARM 1: the NULL. Only the timings row moved. -------------------------
# This is the arm that was dead. It must report ZERO goldens.
printf 'baseline\n| census | 879 |\n' > "$tmp/docs/timings.md"
g add -u 2>/dev/null
n="$(count_via_lib)"
if [ "$n" -eq 0 ]; then
    ok "NULL ARM: a staged timings row alone counts 0 goldens"
else
    bad "NULL ARM: timings-row-only counted $n goldens, want 0 — the dead arm is still dead"
fi
# And the anti-vacuity half: prove something IS staged, so the 0 above means
# "no goldens" and not "nothing happened".
if [ -n "$(g diff --cached --name-only)" ]; then
    ok "NULL ARM is non-vacuous: the index really does hold a staged change"
else
    bad "NULL ARM proved nothing: nothing was staged at all"
fi

# --- ARM 2: a real golden move, alongside the timings row. -----------------
printf 'seed,value\n1,2\n' > "$tmp/book/src/laboratory/generated/the-census/rows.csv"
g add -u 2>/dev/null
n="$(count_via_lib)"
if [ "$n" -eq 1 ]; then
    ok "MOVED ARM: one golden + the timings row counts 1 golden"
else
    bad "MOVED ARM: counted $n, want 1 (the timings row must not be counted)"
fi

# --- ARM 3: a NEW golden file, not merely a modified one. ------------------
printf 'x\n' > "$tmp/book/src/laboratory/generated/the-census/schema.json"
g add -A -- book/src/laboratory/ 2>/dev/null
n="$(count_via_lib)"
if [ "$n" -eq 2 ]; then
    ok "NEW-FILE ARM: an added golden is counted, not just a modified one"
else
    bad "NEW-FILE ARM: counted $n, want 2"
fi

# --- ARM 4: an empty index is 0, and is a DIFFERENT case from the null. ----
g reset -q 2>/dev/null
n="$(count_via_lib)"
if [ "$n" -eq 0 ]; then
    ok "EMPTY ARM: an empty index counts 0 without erroring"
else
    bad "EMPTY ARM: counted $n on an empty index, want 0"
fi
if [ -z "$(g diff --cached --name-only)" ]; then
    ok "EMPTY ARM really is empty (distinct from the null arm, which stages a row)"
else
    bad "EMPTY ARM: the index was not actually reset"
fi

printf '\ntest-sluice-census: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
