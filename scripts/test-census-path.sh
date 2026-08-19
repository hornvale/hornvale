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
case "$got" in
    "$repo_root"/*) bad "the default resolved INSIDE the repo ($got) — the invariant census-run.sh states about its own default" ;;
    *) ok "the default is outside the repo" ;;
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
