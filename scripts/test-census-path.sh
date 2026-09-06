#!/usr/bin/env bash
# scripts/test-census-path.sh — the census worktree's PATH resolution.
#
# WHY THIS EXISTS. `HV_CENSUS_WORKTREE` is used as a raw path. This project's
# own documented value for two months was `canonical` — a bare relative name,
# passed from an invocation that `cd`s to the repo root first — so the census
# worktree was created INSIDE the repo, untracked and un-ignored, defeating
# the invariant census-run.sh states about its own default. Decision 0146
# retires that convention; these are its executable half.
#
# NOTHING HERE RUNS A CENSUS. `census-run.sh worktree` resolves and prints the
# path under no lock, which is the whole reason that subcommand exists — the
# alternative was asserting against a COPY of the resolution expression, and a
# copy is the shape that drifts from its original silently.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }

run() { bash "$repo_root/scripts/census-run.sh" "$@" 2>&1; }

echo "== census path: a RELATIVE override is refused, and refused cheaply"
out="$(HV_CENSUS_WORKTREE=canonical HV_CENSUS_REF=deadbeef run || true)"
rc=0; HV_CENSUS_WORKTREE=canonical HV_CENSUS_REF=deadbeef run >/dev/null 2>&1 || rc=$?
if [ "$rc" = "2" ]; then
    ok "a relative HV_CENSUS_WORKTREE exits 2"
else
    bad "expected exit 2 for a relative override, got $rc"
fi
case "$out" in
    *"must be an ABSOLUTE path"*) ok "the refusal names the bound it enforces (absoluteness)" ;;
    *) bad "the refusal does not name absoluteness: $out" ;;
esac
# THE CANAL-LOCK RULE: turn a vessel away at the gate, never inside the
# chamber. A malformed override is knowable without the serial box, whose
# queue wait is measured in tens of minutes.
case "$out" in
    *"waiting for the census lock"*|*"lock acquired"*)
        bad "the refusal took the census lock before refusing — it must refuse at the gate" ;;
    *) ok "the refusal never touched the census lock" ;;
esac

echo "== census path: the default is anchored to the MAIN worktree, not the caller's"
main_root="$(git worktree list --porcelain | awk '/^worktree /{print $2; exit}')"
expected="$main_root/../hornvale-census-wt"
got="$(run worktree)"
if [ "$got" = "$expected" ]; then
    ok "the default resolves against the main checkout ($got)"
else
    bad "default resolved to '$got', expected '$expected'"
fi
# COMPARE RESOLVED PATHS, NOT LEXICAL PREFIXES. This was a bare
# `case "$got" in "$repo_root"/*)`, and `$got` legitimately contains `/../`:
# the default is anchored to the MAIN worktree as "<main>/../hornvale-census-wt".
# From a LINKED worktree that string does not start with $repo_root and the
# check passed, which is every run in the chamber. From the MAIN CHECKOUT it
# does start with it, so the guard reported a path that escapes the repo as
# being inside it — while the assertion four lines above accepted the very same
# value. A verdict that depends on which worktree you run from is not a verdict.
#
# Observed 2026-09-05 running `scripts/lane-outboard.sh` by hand from the main
# checkout: 6 passed, 1 failed, on a tree where nothing was wrong.
resolve_path() {
    local raw="$1" d b
    d="$(dirname "$raw")"; b="$(basename "$raw")"
    if cd "$d" 2>/dev/null; then printf '%s/%s' "$(pwd -P)" "$b"; cd - >/dev/null || true
    else printf '%s' "$raw"; fi
}
got_real="$(resolve_path "$got")"
repo_real="$(cd "$repo_root" && pwd -P)"
case "$got_real" in
    "$repo_real"/*) bad "the default resolved INSIDE the repo ($got -> $got_real) — the invariant census-run.sh states about its own default" ;;
    *) ok "the default is outside the repo ($got_real)" ;;
esac

# POSITIVE CONTROL. The check above now normalises, and a normaliser that
# silently returned its input would make the guard vacuous while it still
# printed ok. So drive a path that IS inside the repo through the same
# comparison and require it to be caught. Without this, the fix to a
# false-positive could have installed a false-negative and looked identical.
inside_real="$(resolve_path "$repo_root/some/census/dir")"
case "$inside_real" in
    "$repo_real"/*) ok "POSITIVE CONTROL: a path genuinely inside the repo is still caught by this comparison" ;;
    *) bad "POSITIVE CONTROL FAILED: $inside_real was not recognised as inside $repo_real — the inside-the-repo check can no longer fire at all" ;;
esac

echo "== census path: an ABSOLUTE override is still honoured (the test seam survives)"
got="$(HV_CENSUS_WORKTREE=/tmp/hv-census-seam run worktree)"
if [ "$got" = "/tmp/hv-census-seam" ]; then
    ok "an absolute override is returned unchanged"
else
    bad "an absolute override was not honoured: got '$got'"
fi

echo "== census path: MUTATION — without the absoluteness check, the relative value is accepted"
# MUTATE INTO A COPY (`--to`), never in place. The original is untouched, so
# there is no restore step to get wrong — `git checkout --` would revert
# uncommitted work in the same file along with the mutation, and The Axes
# (retrospective §4) read the resulting absence of a test as that test having
# passed.
#
# THE MUTANT IS DRIVEN THROUGH `worktree`, NOT A RUN. That subcommand sits
# after the validation and before the host guard, the lock and the fetch — so
# this takes no claim, touches no remote, and is safe inside a chamber phase.
# Driving a real run instead would have made the assertion host-dependent: on
# a non-canonical box the host guard refuses first, and the mutant would look
# "caught" for a reason that has nothing to do with the check under test.
mut="$(mktemp)"; trap 'rm -f "$mut"' EXIT
python3 scripts/mutate.py --to "$mut" scripts/census-run.sh \
    "            echo \"census-run: HV_CENSUS_WORKTREE must be an ABSOLUTE path; got '\$HV_CENSUS_WORKTREE'.\" >&2" \
    "            : " >/dev/null
mut_out="$(HV_CENSUS_WORKTREE=canonical bash "$mut" worktree 2>&1 || true)"
case "$mut_out" in
    *"must be an ABSOLUTE path"*)
        bad "the mutant still refused — the refusal above may not come from the check under test" ;;
    *)
        ok "MUTATION CONFIRMED: without the check the relative value is accepted (the real one refuses it)" ;;
esac

printf '\n%s: %d passed, %d failed\n' "$(basename "$0")" "$pass" "$fail"
[ "$fail" -eq 0 ]
