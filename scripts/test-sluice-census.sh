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
# The author column is the authority the count now reads. `the-census/` is
# census-authored; the two bookkeeping files are not, and BOTH have fooled the
# old "anything but timings.md" rule on real runs.
cat > "$tmp/docs/generated-paths.txt" <<'DECL'
# path	author
book/src/laboratory/generated/the-census/	census
book/src/laboratory/generated/the-census/schema.json	artifacts
docs/generated-path-writes.tsv	artifacts
DECL
printf 'baseline\n' > "$tmp/docs/timings.md"
printf 'path\t1\t1\n' > "$tmp/docs/generated-path-writes.tsv"
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
# A NEW file under the census-authored directory. NOT schema.json: that is
# declared `artifacts` (regenerate-artifacts.sh rewrites it unconditionally),
# so under the corrected semantics it is deliberately not a census golden —
# an expectation of 2 here was wrong about the rule, not about the code.
printf 'extra\n' > "$tmp/book/src/laboratory/generated/the-census/extra.csv"
g add -A -- book/src/laboratory/ 2>/dev/null
n="$(count_via_lib)"
if [ "$n" -eq 2 ]; then
    ok "NEW-FILE ARM: an ADDED census golden is counted, not just a modified one"
else
    bad "NEW-FILE ARM: counted $n, want 2"
fi
# And the artifacts-authored schema beside it must NOT count.
printf 'x\n' > "$tmp/book/src/laboratory/generated/the-census/schema.json"
g add -A -- book/src/laboratory/ 2>/dev/null
if [ "$(count_via_lib)" -eq 2 ]; then
    ok "SCHEMA: an artifacts-authored file under a census directory is not a golden"
else
    bad "the artifacts-authored schema.json counted as a census golden"
fi

# --- ARM 3b: THE COUNTER FILE IS NOT A GOLDEN -------------------------------
# The case that fooled the first fix TWICE on real runs — the-crosscut
# 4c69a5d6578d and the-brattice cccfcdad9f21. docs/generated-path-writes.tsv is
# `artifacts`-authored bookkeeping; a census that moves only it has moved NO
# golden, and saying otherwise told a campaign its new stream label had shifted
# the world when it had not.
# Reset the WORKING TREE too, not just the index: `git reset` alone leaves the
# earlier arms' modified goldens in place, and `add -u` then re-stages them, so
# this arm counted their movement as its own. It reported 1 golden for a case
# whose entire point is that it must report 0.
g reset -q --hard 2>/dev/null
g clean -qfd 2>/dev/null
printf 'path\t2\t2\n' > "$tmp/docs/generated-path-writes.tsv"
printf 'baseline\nrow\n'  > "$tmp/docs/timings.md"
g add -u 2>/dev/null
n="$(count_via_lib)"
if [ "$n" -eq 0 ]; then
    ok "COUNTER FILE: generated-path-writes.tsv + timings row counts 0 goldens"
else
    bad "the counter file counted as $n golden(s) — the mislabel that fooled two campaigns"
fi
if [ -n "$(g diff --cached --name-only)" ]; then
    ok "and that 0 is non-vacuous: two paths really are staged"
else
    bad "nothing was staged; the counter-file case proved nothing"
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

# --- THE ARMS' COLUMN SET (The Spillway) ------------------------------------
# A census delivery re-authors the Gnomon arms when the world moved OR when an
# arm's columns differ from the census's. The second trigger is this pair of
# functions. Synthetic schemas in the serde pretty-print shape both real files
# have: the study's own "name" at indent 2, column names at indent 6.
if ! declare -f injection_arms_stale >/dev/null 2>&1; then
    bad "HV_CENSUS_LIB=1 did not expose injection_arms_stale"
else
    ok "injection_arms_stale is exposed by the library"
fi
arms="$tmp/windows/lab/tests/fixtures/injection"
mkdir -p "$arms/baseline-a" "$arms/karst"
write_schema() {  # $1 = path, $2 = study name, $3.. = column names
    local p="$1" study="$2"; shift 2
    {
        printf '{\n  "columns": [\n'
        local sep=""
        for c in "$@"; do
            printf '%s    {\n      "kind": "numeric",\n      "name": "%s"\n    }' "$sep" "$c"
            sep=$',\n'
        done
        printf '\n  ],\n  "name": "%s"\n}\n' "$study"
    } > "$p"
}
census="$tmp/book/src/laboratory/generated/the-census/schema.json"
write_schema "$census" the-census seed pin_set karst-fraction
write_schema "$arms/baseline-a/schema.json" gnomon-injection seed pin_set karst-fraction
write_schema "$arms/karst/schema.json"      gnomon-injection seed pin_set karst-fraction

cols="$(census_schema_columns "$census" | tr '\n' ' ')"
if [ "$cols" = "karst-fraction pin_set seed " ]; then
    ok "census_schema_columns lists the columns sorted and IGNORES the study's own name"
else
    bad "census_schema_columns gave '$cols'"
fi
if [ -z "$(injection_arms_stale "$tmp")" ]; then
    ok "matching arms whose STUDY NAME differs from the census's read as not stale"
else
    bad "arms with identical columns were called stale: $(injection_arms_stale "$tmp")"
fi

# The census gains a column (a campaign registered a metric).
write_schema "$census" the-census seed pin_set karst-fraction warp-lift
stale="$(injection_arms_stale "$tmp")"
if printf '%s\n' "$stale" | grep -q '^baseline-a: 1 column' && printf '%s\n' "$stale" | grep -q '^karst: 1 column'; then
    ok "a column the census has and the arms lack marks EVERY arm stale, by name"
else
    bad "census-gained-a-column: got '$stale'"
fi

# An arm carries a column the census lacks (authored against another registry).
write_schema "$census" the-census seed pin_set karst-fraction
write_schema "$arms/karst/schema.json" gnomon-injection seed pin_set karst-fraction ghost
stale="$(injection_arms_stale "$tmp")"
if [ "$(printf '%s\n' "$stale" | grep -c .)" -eq 1 ] && printf '%s\n' "$stale" | grep -q '^karst: 0 column(s) the census has and the arm lacks, 1 the arm has'; then
    ok "a column only an arm has marks THAT arm stale and no other"
else
    bad "arm-has-extra-column: got '$stale'"
fi

# No census schema at all: nothing to compare, nothing printed, exit 0.
rm -f "$census"
if [ -z "$(injection_arms_stale "$tmp")" ]; then
    ok "an absent census schema compares nothing (prints nothing, exits 0)"
else
    bad "absent census schema produced output: $(injection_arms_stale "$tmp")"
fi
rm -rf "$arms"

printf '\ntest-sluice-census: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
