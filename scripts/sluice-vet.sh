#!/usr/bin/env bash
# Vet a queued sluice candidate: everything the operator checks before draining.
#
#   bash scripts/sluice-vet.sh <branch> <ref>
#
# WHY THIS IS A SCRIPT AND NOT A HABIT. The operator typed this vet by hand for
# a whole session, and one clause of it was wrong: the mouth's verdict was piped
# through `tail -2`. That is correct when a candidate has ONE conflict and
# silently wrong when it has eight — the truncation hides the very thing the
# check exists to find, and it reads identically either way.
#
# It cost campaign/the-legend (2026-08-31) a false "two mechanical conflicts"
# verdict and an operator claim post that had to be retracted: the real set was
# EIGHT, including a code conflict in clients/game/bin/src/driver.rs and a
# committed byte-golden session fixture, neither of which an operator may
# resolve. The mouth had reported all eight. The truncation was the operator's.
#
# So: THIS SCRIPT NEVER TRUNCATES THE MOUTH. Everything else here is a
# convenience; that one line is the reason the file exists.

set -u

branch="${1:?usage: sluice-vet.sh <branch> <full-sha>}"
ref="${2:?usage: sluice-vet.sh <branch> <full-sha>}"
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root" || exit 1

git fetch -q origin "+refs/heads/${branch}:refs/remotes/origin/${branch}" 2>/dev/null
git fetch -q origin 2>/dev/null

sha="$(git rev-parse "$ref" 2>/dev/null)"
if [ -z "$sha" ]; then
    echo "sluice-vet: $ref does not resolve" >&2
    exit 2
fi
printf 'ref      %s\n' "$sha"
printf 'branch   %s   (behind main: %s)\n' "$branch" \
    "$(git rev-list --count "$sha"..origin/main 2>/dev/null)"

# --- the mouth, IN FULL. Never `head`, never `tail`. ------------------------
echo
echo "MOUTH"
# The exit code is deliberately not consumed: this script REPORTS, it does not
# gate. sluice-drain.sh consumes the mouth's verdict; duplicating that decision
# here would give two places to disagree about whether a candidate may run.
# HV_VET_MOUTH is a TEST SEAM, not a caller-facing override: it lets
# test-sluice-vet.sh feed a known multi-conflict verdict and assert every line
# survives. Without it the no-truncation property is untestable, and an
# untestable property is how `tail -2` survived a whole session.
mouth="$(${HV_VET_MOUTH:-bash scripts/sluice-mouth.sh} "$branch" "$sha" </dev/null 2>&1)" || true
printf '%s\n' "$mouth" | sed 's/^/  /'
conflicts="$(printf '%s\n' "$mouth" | grep -c 'conflict:')"
[ "$conflicts" -gt 0 ] && printf '  >> %s conflict(s) — count them, do not skim\n' "$conflicts"

# --- decisions and their block ---------------------------------------------
echo
echo "DECISIONS"
ledger="$HOME/.local/state/hornvale/decision-blocks/blocks.tsv"
block="$(grep -iE "	(campaign/)?${branch#campaign/}	" "$ledger" 2>/dev/null | awk -F'\t' '{print $3"-"$4}')"
# ADDED files only. `--diff-filter=A` is the whole fix and it is not cosmetic:
# a three-dot diff lists every decision file the branch TOUCHED, and an
# AMENDMENT to a ratified decision is a modification, not a mint. Without the
# filter, campaign/the-minute's additive amendment to 0226 read as a fresh mint,
# then "collided" with the seven campaign branches that carry 0226 because it has
# been on main since February. Seven loud false lines on a clean candidate — the
# cry-wolf failure that trains an operator to skim the one report that must not
# be skimmed.
minted="$(git diff --name-only --diff-filter=A "origin/main...$sha" 2>/dev/null | grep '^docs/decisions/0' | sed 's|.*/||; s/-.*//')"
# Amendments are still worth SEEING — an append-only amendment to a ratified
# decision is a real act — they are just not mints and must not drive the block
# or collision checks below.
amended="$(git diff --name-only --diff-filter=M "origin/main...$sha" 2>/dev/null | grep '^docs/decisions/0' | sed 's|.*/||; s/-.*//')"
printf '  reserved block: %s\n' "${block:-<none reserved>}"
# shellcheck disable=SC2086  # $minted is a deliberate word-split list of numbers
printf '  minted:         %s\n' "$(printf '%s ' $minted)"
# shellcheck disable=SC2086  # deliberate word-split of a number list
[ -n "$amended" ] && printf '  amended:        %s  (existing decisions, not mints)\n' "$(printf '%s ' $amended)"
if [ -n "$minted" ] && [ -n "$block" ]; then
    lo="${block%%-*}"; hi="${block##*-}"
    for n in $minted; do
        d=$((10#$n))
        [ "$d" -ge "$((10#$lo))" ] && [ "$d" -le "$((10#$hi))" ] \
            || printf '  >> %s is OUTSIDE %s — check the ledger before assuming a mis-mint\n' "$n" "$block"
    done
elif [ -n "$minted" ]; then
    printf '  >> minted with no reserved block — verify the range is free and its owner finished\n'
fi

# --- cross-branch decision collisions ---------------------------------------
# THE ONE THING NO GATE CAN SEE, and the reason this block is worth its cost.
# `cli/tests/suite/docs_consistency.rs` reds on duplicate decision IDs WITHIN
# the object it gates, and the chamber gates main plus ONE branch. So two
# branches each carrying their own 0517 are individually consistent and both
# pass green; the duplicate only becomes visible in an object containing both,
# which is main AFTER the second merge — landed, not preventable. That is
# exactly how two campaigns both minted 0134 through a passing gate.
#
# So this reads the one thing the chamber never assembles: every campaign
# branch at once. It is a REPORT, like everything else here — the vet script
# does not gate, and must not start. HV_VET_REFS is a test seam.
vet_refs() {
    # shellcheck disable=SC2086  # HV_VET_REFS is a deliberate word-split ref list
    if [ -n "${HV_VET_REFS:-}" ]; then printf '%s\n' ${HV_VET_REFS}; return; fi
    git for-each-ref --format='%(refname:short)' refs/remotes/origin/campaign/ 2>/dev/null
}
for n in $minted; do
    d=$((10#$n)); nn="$(printf '%04d' "$d")"
    owner="$(awk -F'\t' -v d="$d" '$3+0<=d && d<=$4+0 {print $2}' "$ledger" 2>/dev/null | tail -1)"
    if [ -n "$owner" ] && [ "${owner#campaign/}" != "${branch#campaign/}" ]; then
        printf '  >> %s falls inside a block reserved by %s — NOT this campaign\n' "$nn" "$owner"
    fi
    for r in $(vet_refs); do
        case "$r" in origin/"${branch#origin/}"|"$branch") continue ;; esac
        if git ls-tree -r --name-only "$r" -- docs/decisions/ 2>/dev/null \
             | grep -q "^docs/decisions/${nn}-"; then
            printf '  >> COLLISION: %s also exists on %s\n' "$nn" "$r"
        fi
    done
done

# --- the surfaces that cost a chamber run when missed ----------------------
echo
echo "SURFACES"
pat="$(grep -m1 '^census_guard_files=' scripts/hooks/pre-commit | sed "s/^census_guard_files='//; s/'$//")"
printf '  census pins (hook list):  %s\n' "$(git diff --name-only "origin/main...$sha" 2>/dev/null | grep -cE "^(${pat})$")"
# disposition_calibration is NOT in the hook's list and census values feed it —
# it cost campaign/the-winze a 1718 s chamber run on 2026-08-30. Reported
# separately so the gap is visible rather than implied.
printf '  heavy-tier calibrations:  %s  (disposition/touchstone/hearsay — outside the census guard)\n' \
    "$(git diff --name-only "origin/main...$sha" 2>/dev/null | grep -cE 'disposition_calibration|touchstone|hearsay/tests')"
printf '  world fixture:            %s\n' \
    "$(git diff --numstat "origin/main...$sha" -- cli/tests/fixtures/world-seed-42.json 2>/dev/null | awk '{print "+"$1" -"$2}')"
printf '  registry/accession:       %s\n' "$(git diff --name-only "origin/main...$sha" 2>/dev/null | grep -cE 'accession\.rs|concept-registry-generated')"
printf '  lab metrics.rs:           %s  (a change here invalidates the prior census)\n' \
    "$(git diff --name-only "origin/main...$sha" 2>/dev/null | grep -c 'windows/lab/src/metrics\.rs')"
printf '  scene schema constants:   %s\n' \
    "$(git diff "origin/main...$sha" -- windows/scene/src/lib.rs 2>/dev/null | grep -cE '^[+-]pub const .*SCHEMA')"

# --- close-only checks ------------------------------------------------------
echo
echo "DEFINITION OF DONE (a merge is the last act of a campaign; a stage is exempt)"
for p in book/src/chronicle/ docs/retrospectives/ book/src/SUMMARY.md book/src/frontier/idea-registry.md; do
    printf '  %-34s %s\n' "$p" "$(git diff --name-only "origin/main...$sha" 2>/dev/null | grep -c "$p")"
done
printf '  headline trailer: %s\n' \
    "$(git log --format='%(trailers:key=Sluice-Headline,valueonly)' "origin/main..$sha" 2>/dev/null | grep -v '^$' | head -1)"

echo
echo "SHAPE"
git diff --shortstat "origin/main...$sha" 2>/dev/null | sed 's/^ */  /'
printf '  non-prose paths: %s\n' "$(git diff --name-only "origin/main...$sha" 2>/dev/null | grep -cvE '^(book/|docs/)')"

echo
printf 'operator checkout: %s at %s  (== origin/main: %s)\n' \
    "$(git rev-parse --abbrev-ref HEAD)" "$(git rev-parse --short HEAD)" \
    "$([ "$(git rev-parse HEAD)" = "$(git rev-parse origin/main)" ] && echo yes || echo 'NO — ff before draining')"
exit 0
