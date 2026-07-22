#!/usr/bin/env bash
# scripts/test-merge-regenerate.sh — integration test for the
# regenerate-on-conflict merge driver (PROC-12). Not part of `make gate`
# (this campaign adds no Rust code); run manually or wire into `make
# quick` at a later campaign's discretion. Exercises three scenarios in
# a throwaway clone: Tier B success (regenerates past a real conflict),
# Tier B failure (the driver-governed file is left unmerged in the
# index, not textually marked, when the crate doesn't build — a
# genuinely UNDRIVEN conflict elsewhere, e.g. kernel/src/lib.rs, still
# gets git's ordinary <<<<<<< markers), and Tier A (union keeps both
# sides' additions).
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
work="$(mktemp -d "${TMPDIR:-/tmp}/hv-merge-regen-test.XXXXXX")"
trap 'rm -rf "$work"' EXIT

pass() { echo "PASS: $1"; }
fail() { echo "FAIL: $1" >&2; exit 1; }

clone="$work/clone"
git clone -q "$repo_root" "$clone"
cd "$clone"
git config merge.hv-regenerate.driver 'scripts/merge-regenerate.sh %O %A %B %P'

base_branch="$(git branch --show-current)"

# --- Scenario 1: Tier B success path ---
echo "--- Scenario 1: Tier B regenerates past a real conflict ---"

git checkout -b scenario1-a -q
echo "stray-a" >> book/src/reference/phonology.md
git add book/src/reference/phonology.md
git commit -q -m "test: stray-a"

git checkout "$base_branch" -q
git checkout -b scenario1-b -q
echo "stray-b" >> book/src/reference/phonology.md
git add book/src/reference/phonology.md
git commit -q -m "test: stray-b"

if ! git merge scenario1-a -q; then
    fail "scenario 1: expected the merge to succeed via the driver, but it conflicted"
fi

if grep -q "^<<<<<<<\|^=======\|^>>>>>>>" book/src/reference/phonology.md; then
    fail "scenario 1: conflict markers left in phonology.md"
fi
if grep -q "stray-a\|stray-b" book/src/reference/phonology.md; then
    fail "scenario 1: a stray line survived — the driver picked a side instead of regenerating"
fi

fresh="$work/fresh-phonology.md"
cargo run -q -p hornvale -- phonology > "$fresh"
if ! diff -q "$fresh" book/src/reference/phonology.md > /dev/null; then
    fail "scenario 1: resolved phonology.md does not byte-match a fresh regeneration"
fi
pass "scenario 1: Tier B conflict resolved by regeneration, byte-matches fresh output"

# --- Scenario 2: Tier B failure path leaves the driver-governed file unmerged ---
echo "--- Scenario 2: Tier B is left unmerged (no textual markers) when the crate can't build ---"

git checkout "$base_branch" -q
git checkout -b scenario2-a -q
echo "fn this_does_not_compile_a( {" >> kernel/src/lib.rs
git add kernel/src/lib.rs
git commit -q -m "test: break the build on scenario2-a"
echo "stray-a2" >> book/src/reference/phonology.md
git add book/src/reference/phonology.md
git commit -q -m "test: stray-a2 alongside the broken build"

git checkout "$base_branch" -q
git checkout -b scenario2-b -q
# Both branches must touch kernel/src/lib.rs, not just scenario2-a: a
# one-sided change auto-merges cleanly (git takes the only side that
# changed it), so the file would sit on disk in its OLD, still-buildable
# form at the moment the driver runs for phonology.md's conflict --
# the build check would then spuriously succeed. Touching it on BOTH
# sides forces a genuine two-sided conflict, so git deposits real
# <<<<<<< markers into kernel/src/lib.rs immediately as part of its own
# standard (undriven) conflict handling -- invalid Rust syntax on disk
# before any driver for any OTHER file ever runs, guaranteeing the build
# is actually broken when merge-regenerate.sh's dispatch tries it.
echo "fn this_does_not_compile_b( {" >> kernel/src/lib.rs
git add kernel/src/lib.rs
git commit -q -m "test: also modify kernel/src/lib.rs on scenario2-b (forces a genuine two-sided conflict)"
echo "stray-b2" >> book/src/reference/phonology.md
git add book/src/reference/phonology.md
git commit -q -m "test: stray-b2"

if git merge scenario2-a -q 2>/dev/null; then
    fail "scenario 2: expected the merge to report a conflict (build broken), but it succeeded"
fi

# A custom `merge=<driver>`-attributed path does NOT get textual
# <<<<<<< diff3 markers on driver failure (that's only git's default,
# undriven conflict handling) -- verified directly against real git
# behavior, not assumed from documentation prose: git leaves the path
# marked unmerged in the index (three conflict stages: base/ours/
# theirs) with the working-tree file at its pre-merge "ours" content.
# `git ls-files -u` is the correct signal to check.
if ! git ls-files -u -- book/src/reference/phonology.md | grep -q .; then
    fail "scenario 2: expected phonology.md to be left unmerged (conflict stages present) when the driver can't build the crate, found none"
fi
if [ ! -s book/src/reference/phonology.md ]; then
    fail "scenario 2: phonology.md is empty — the driver must never leave partial/empty output on failure"
fi
git merge --abort
pass "scenario 2: build failure correctly leaves the driver-governed file unmerged (no textual markers, no partial output)"

# --- Scenario 3: Tier A union merge keeps both sides' additions ---
echo "--- Scenario 3: Tier A union merge keeps both sides' SUMMARY.md bullets ---"

git checkout "$base_branch" -q
git checkout -b scenario3-a -q
printf -- '- [Scenario Three A](./chronicle/scenario-three-a.md)\n' >> book/src/SUMMARY.md
git add book/src/SUMMARY.md
git commit -q -m "test: scenario3-a SUMMARY bullet"

git checkout "$base_branch" -q
git checkout -b scenario3-b -q
printf -- '- [Scenario Three B](./chronicle/scenario-three-b.md)\n' >> book/src/SUMMARY.md
git add book/src/SUMMARY.md
git commit -q -m "test: scenario3-b SUMMARY bullet"

if ! git merge scenario3-a -q; then
    fail "scenario 3: expected the union merge to succeed, but it conflicted"
fi
if ! grep -q "Scenario Three A" book/src/SUMMARY.md; then
    fail "scenario 3: branch-a's bullet is missing after the union merge"
fi
if ! grep -q "Scenario Three B" book/src/SUMMARY.md; then
    fail "scenario 3: branch-b's bullet is missing after the union merge"
fi
pass "scenario 3: union merge kept both sides' SUMMARY.md bullets"

echo "All scenarios passed."
