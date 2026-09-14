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

# The shared conflict/author classifier, so the vet, the mouth and the chamber
# cannot disagree about which conflicts are bookkeeping. One implementation,
# several callers --- the rule decision 0079 applies to the host check.
# shellcheck source=/dev/null
HV_PHASES_LIB=1 . "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/sluice-phases.sh"

# The allocator's ledger. HV_BLOCK_DIR is scripts/decision-block.sh's own
# variable, honoured here so the two agree about where the ledger lives and so
# a test can point both at a scratch copy.
ledger="${HV_BLOCK_DIR:-$HOME/.local/state/hornvale/decision-blocks}/blocks.tsv"

# Which campaign's reserved block contains a decision number, if any. Prints
# "<campaign>\t<lo>-<hi>", or nothing when the allocator has never issued it.
# The campaign column is written both bare and `campaign/`-prefixed over the
# ledger's history, so it is normalised here rather than at every call site.
block_owner_of() {
    [ -s "$ledger" ] || return 0
    awk -F'\t' -v n="$((10#$1))" '
        NF >= 4 {
            lo = $3 + 0; hi = $4 + 0
            if (n >= lo && n <= hi) {
                name = $2; sub(/^campaign\//, "", name)
                printf "%s\t%04d-%04d\n", name, lo, hi
            }
        }' "$ledger" | tail -1
}

# THREE-VALUED, and the third value is the point. This answers "can that
# campaign still mint into its block?", which is the question an operator
# actually has when a candidate mints a number someone else reserved --- and
# which this script used to hand back as homework ("verify ... its owner
# finished"). It was done by hand three times before being written down.
#
#   CLOSED  - no pushed branch is ahead of main AND main carries the campaign's
#             chronicle and retrospective. Nothing more can come from it, so a
#             number inside its block is permanently free.
#   LIVE    - a pushed branch is ahead of main. It can still mint.
#   UNKNOWN - neither. A campaign running in a worktree that has never pushed
#             looks exactly like this, so UNKNOWN must never be read as safe.
#
# The allocator has no release operation, so a CLOSED campaign's unused tail
# is dead space: nobody else will ever be issued it either, which is precisely
# why reusing it is safe rather than merely tolerated.
campaign_status() {
    local slug="$1" ref="refs/remotes/origin/campaign/$1"
    if git rev-parse --verify --quiet "$ref" >/dev/null 2>&1; then
        if ! git merge-base --is-ancestor "$ref" origin/main 2>/dev/null; then
            echo LIVE; return
        fi
    fi
    if git cat-file -e "origin/main:book/src/chronicle/$slug.md" 2>/dev/null &&
        git cat-file -e "origin/main:docs/retrospectives/$slug.md" 2>/dev/null; then
        echo CLOSED; return
    fi
    echo UNKNOWN
}

# Report one minted number that falls outside the candidate's own block.
adjudicate_number() {
    local n="$1" own owner range status
    own="$(block_owner_of "$n")"
    if [ -z "$own" ]; then
        printf '  >> %s lies in NO reserved block — the allocator has never issued it\n' "$n"
        return
    fi
    owner="${own%%	*}"; range="${own##*	}"
    status="$(campaign_status "$owner")"
    case "$status" in
    CLOSED)
        printf '  >> %s lies inside the %s block reserved by %s, which is CLOSED\n' "$n" "$range" "$owner"
        printf '     (no branch ahead of main; chronicle and retrospective both landed) — the number is safe\n'
        ;;
    LIVE)
        printf '  >> %s lies inside the %s block reserved by %s, which is LIVE and can still mint it\n' "$n" "$range" "$owner"
        printf '     COLLISION RISK — check that branch before dispatching\n'
        ;;
    *)
        printf '  >> %s lies inside the %s block reserved by %s, whose state is UNKNOWN\n' "$n" "$range" "$owner"
        printf '     (no pushed branch and no close package — it may be running unpushed). Do NOT read this as safe\n'
        ;;
    esac
}

# Does the census a candidate ships still describe its own tip? Takes the sha;
# prints the CENSUS section. Driven directly by test-sluice-vet-blocks.sh under
# HV_VET_LIB=1, for the reason given above: a decision rule that cannot be
# driven directly gets tested through whatever end-to-end path happens to
# exist.
census_freshness() {
    local sha="$1" census_dir base census_commit after n_after world_all n_all
    census_dir='book/src/laboratory/generated/the-census/'
    echo
    echo "CENSUS"
    base="$(git merge-base "$sha" origin/main 2>/dev/null)"
    census_commit="$(git log -1 --format=%H "$base..$sha" -- "$census_dir" 2>/dev/null)"
    if [ -z "$census_commit" ]; then
        printf '  ships no census\n'
        world_all="$(world_since "$base" "$sha")"
        n_all="$(printf '%s' "$world_all" | grep -c .)"
        if [ "$n_all" -gt 0 ]; then
            printf '  >> but %s world-producing source file(s) changed on this branch\n' "$n_all"
            printf '     If any of them moves a golden, census_sentinel reds this at merge.\n'
        fi
        return
    fi
    after="$(world_since "$census_commit" "$sha")"
    n_after="$(printf '%s' "$after" | grep -c .)"
    printf '  census last moved at %s\n' "$(git rev-parse --short=9 "$census_commit")"
    if [ "$n_after" -eq 0 ]; then
        printf '  no world-producing source changed after it — the goldens describe this tip\n'
    else
        printf '  >> %s world-producing source file(s) changed AFTER the census:\n' "$n_after"
        printf '%s\n' "$after" | sed 's/^/       /'
        printf '     The goldens may not describe this tip. File-level: a hunk inside\n'
        printf '     a test module is counted here and is a false alarm — read the hunks.\n'
    fi
}

# World-PRODUCING sources changed in (FROM, TO]. A change under tests/ or
# benches/ cannot move a golden, and counting one turns a freshness signal into
# noise nobody reads.
world_since() {
    git log --format='' --name-only "$1..$2" -- kernel/ domains/ windows/ 2>/dev/null |
        grep -E '\.rs$' | grep -vE '(^|/)(tests|benches)/' | sort -u
}

# Did a candidate that MOVES census goldens also touch the pins that assert
# against them? This is the rule that would have saved 1033 s of gate time on
# campaign/anchor-orbital-coherence, 2026-09-14: 17 golden files moved, zero
# calibration pin sources touched, nine calibration tests red in the chamber
# with messages of the form "mean name-collision-rate drifted: 0.5245073".
#
# Measured over the three candidates in flight that night --- it discriminates,
# which is the only reason it is printed:
#
#   anchor          2 golden rows.csv moved, 0 pin sources touched   <- the red
#   the-tidemark    3 moved, all 4 pin sources touched               <- re-pinned
#   the-coherence   0 moved, 0 touched                               <- quiet
#
# THE PIN LIST IS NOT THE HOOK'S LIST, and the difference is the whole check.
# scripts/hooks/pre-commit's census_guard_files deliberately includes
# `book/src/laboratory/generated/[^/]+/rows.csv` --- a GOLDEN, not a pin ---
# because the hook must fire when either side moves. Counting that pattern here
# scores anchor as "2 pin files touched" and reports the exact red it exists to
# catch as clean. The SURFACES section's `census pins (hook list)` line uses
# the hook's pattern and is right to; this one uses the pin SOURCES alone.
#
# TOUCHING A PIN SOURCE PROVES IT WAS CONSIDERED, NEVER THAT IT IS CORRECT, and
# the report says "considered" rather than implying a guarantee it cannot make.
CENSUS_PIN_SOURCES='windows/lab/tests/suite/(calibration|branches_family_calibration|gathering_calibration)\.rs|tools/census/queries/calibrate/golden-pins\.sql'

census_pins() {
    local sha="$1" gold pin
    gold="$(git diff --name-only "origin/main...$sha" -- 'book/src/laboratory/generated/*/rows.csv' 2>/dev/null | grep -c .)"
    pin="$(git diff --name-only "origin/main...$sha" 2>/dev/null | grep -cE "^(${CENSUS_PIN_SOURCES})\$")"
    [ "$gold" -gt 0 ] || return 0
    if [ "$pin" -gt 0 ]; then
        printf '  %s golden rows.csv moved; %s calibration pin source(s) touched — considered\n' "$gold" "$pin"
        return 0
    fi
    printf '  >> %s golden rows.csv moved and NO calibration pin source was touched\n' "$gold"
    printf '     The calibration batteries assert against these goldens; moving one\n'
    printf '     without re-pinning the other is the commonest red this queue produces.\n'
    printf "     Reproduce in seconds: cargo nextest run -p hornvale-lab -E 'test(calibration::)'\n"
}

# The reconciliation row for a candidate, or an honest absence.
#
# A BRANCH NAME IS NOT A CAMPAIGN SLUG, and assuming it is reported a false
# absence on a live candidate. campaign/the-coherence's campaign is
# `the-coherent-ground`: the row was present and correct, the substring
# fallback below could not reach it ("the-coherence" is not a substring of
# "the-coherent-ground" --- they diverge at the eleventh character), and the
# vet told the operator there was NO ROW at the exact moment the operator was
# deciding whether the close package was complete.
#
# The authority is what the candidate ADDS under book/src/chronicle/ and
# docs/retrospectives/: a merge is the last act of a campaign, so those files
# name the campaign whose row this is. The branch-derived name stays as a
# fallback for a candidate that adds neither.
#
# The absence is still worth reporting honestly --- 440 rows read `active`, so
# a genuinely missing row is a real signal --- which is why a miss names every
# key it tried rather than just the last one.
reconciliation_disposition() {
    local sha="$1" branch="$2" tsv keys k row
    tsv="$(git show "$sha:docs/audits/campaign-reconciliation.tsv" 2>/dev/null)"
    keys="$(git diff --name-only --diff-filter=A "origin/main...$sha" 2>/dev/null \
        -- book/src/chronicle/ docs/retrospectives/ | sed 's|.*/||; s|\.md$||' | sort -u)"
    keys="$keys
${branch#campaign/}"
    for k in $keys; do
        [ -n "$k" ] || continue
        row="$(printf '%s\n' "$tsv" | awk -F'\t' -v k="$k" '$1==k{print $2; exit}')"
        if [ -n "$row" ]; then
            printf '  reconciliation disposition: %s  (key: %s)\n' "$row" "$k"
            return 0
        fi
    done
    # Older rows use a `plan-<date>-campaign-<name>` key rather than the bare
    # name, so a substring match is tried for every key before reporting none.
    for k in $keys; do
        [ -n "$k" ] || continue
        row="$(printf '%s\n' "$tsv" | awk -F'\t' -v k="$k" 'index($1,k){print $2" (key: "$1")"; exit}')"
        if [ -n "$row" ]; then
            printf '  reconciliation disposition: %s\n' "$row"
            return 0
        fi
    done
    # shellcheck disable=SC2086  # deliberate word-split of the key list
    printf '  reconciliation disposition: <NO ROW; tried %s>\n' \
        "$(printf '%s ' $keys)"
}

# Will this candidate still merge once the candidates AHEAD of it have landed?
#
# THE ONE GAP THE MOUTH HAS, and it is expensive every time it opens. The mouth
# checks a candidate against TODAY's main, where it merges cleanly and is
# correctly admitted. The chamber merges main AT DISPATCH TIME, by which point
# the rows ahead have landed. So a candidate can pass the mouth, take the
# strictly serial claim, fail at the <merge> step with rc=10, and have tested
# nothing --- a whole slot for no information. campaign/the-weft did exactly
# this on 2026-09-04; it is why the overlap advisory exists, and the advisory
# reports SHARED PATHS, which is a weaker statement than this one.
#
# Measured by hand on campaign/anchor-orbital-coherence, 2026-09-14, which is
# why this exists: 48 conflicts against a simulated main-plus-the-tidemark, of
# which the chamber regenerates 10 and cannot touch 38. The overlap advisory
# saw the shared paths and could not say the merge would FAIL.
#
# IT IS A PROJECTION, NOT A VERDICT, and it says so. A row ahead may red and
# never land --- campaign/the-kerf was once held for a collision with a
# candidate that died on a formatting check twenty minutes later. So this
# prints and never gates, like every other line here.
#
# Rows are folded on in queue order with merge-tree, so the simulated main is
# built the same way the chamber will build the real one. A row that conflicts
# with the simulation is dropped from it rather than aborting the projection:
# it will not land either, so leaving it out is the better guess.
projected_merge() {
    local sha="$1" branch="$2" ahead simulated tree rbranch rsha
    local mine_ts conf real noise a p decl
    mine_ts="$(awk -F'\t' -v s="$sha" '$4==s{print $1; exit}' \
        "${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}/queue.tsv" 2>/dev/null)"
    [ -n "$mine_ts" ] || return 0   # not queued: nothing is ahead of it
    ahead="$(awk -F'\t' -v t="$mine_ts" -v b="$branch" \
        '($5=="queued"||$5=="running") && $6=="merge" && $1<t && $3!=b {print $1"\t"$3"\t"$4}' \
        "${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}/queue.tsv" 2>/dev/null | sort)"
    echo
    echo "PROJECTED MERGE (after the rows ahead of this one land)"
    if [ -z "$ahead" ]; then
        printf '  nothing queued ahead — the mouth verdict against main is the whole story\n'
        return 0
    fi
    simulated="$(git rev-parse origin/main)"
    while IFS=$'\t' read -r _ rbranch rsha; do
        [ -n "$rsha" ] || continue
        # FOLD THE ROW ON EVEN WHEN IT CONFLICTS, and this is the correction
        # that makes the projection usable at all. `merge-tree --write-tree`
        # emits a tree on its first line whether or not the merge conflicted,
        # and the first draft of this dropped any conflicting row instead ---
        # which silently removed campaign/the-tidemark from the projection,
        # because its conflict with the row ahead of it was almost entirely
        # ARTIFACTS-authored bookkeeping the chamber regenerates. Dropping it
        # reported campaign/anchor-orbital-coherence as merging cleanly when a
        # hand-run of the same simulation found 38 unresolvable conflicts. A
        # false CLEAN is the one direction this section must never fail in.
        tree="$(git merge-tree --write-tree "$simulated" "$rsha" 2>/dev/null | head -1)"
        if [ -z "$tree" ]; then
            printf '  UNRESOLVED %-34s merge-tree produced nothing; left out of the projection\n' "$rbranch"
            continue
        fi
        simulated="$(git commit-tree "$tree" -p "$simulated" -p "$rsha" \
            -m "projected: $rbranch" 2>/dev/null)"
        printf '  folded on  %-34s %s\n' "$rbranch" "$(git rev-parse --short=9 "$rsha")"
    done <<AHEAD
$ahead
AHEAD
    if git merge-tree --write-tree "$simulated" "$sha" >/dev/null 2>&1; then
        printf '  still merges cleanly against that projection\n'
        return 0
    fi
    conf="$(git merge-tree --write-tree --name-only "$simulated" "$sha" 2>&1 \
        | awk 'length($0)==40 && /^[0-9a-f]+$/ {seen=1;next} seen && /^$/ {exit} seen {print}')"
    decl="$(mktemp)"; git show "$sha:docs/generated-paths.txt" > "$decl" 2>/dev/null || : > "$decl"
    real=""; noise=0
    while IFS= read -r p; do
        [ -n "$p" ] || continue
        a="$(sluice_path_author "$p" "$decl")"
        if [ "$a" = artifacts ]; then noise=$((noise + 1)); else real="$real$p
"; fi
    done <<PATHS
$conf
PATHS
    rm -f "$decl"
    real="${real%
}"
    if [ -z "$real" ]; then
        printf '  conflicts on %s path(s), ALL artifacts-authored — the chamber regenerates those\n' "$noise"
        return 0
    fi
    printf '  >> WILL NOT MERGE: %s path(s) the chamber cannot regenerate\n' \
        "$(printf '%s\n' "$real" | grep -c .)"
    printf '%s\n' "$real" | sed 's/^/       /' | head -12
    [ "$(printf '%s\n' "$real" | grep -c .)" -gt 12 ] && printf '       ... and %s more\n' \
        "$(( $(printf '%s\n' "$real" | grep -c .) - 12 ))"
    [ "$noise" -gt 0 ] && printf '  (%s artifacts-authored conflicts not listed; the chamber resolves those)\n' "$noise"
    printf '     This candidate would pass the mouth against TODAY main, take the claim,\n'
    printf '     and die at <merge> rc=10 having tested nothing. Absorb and resubmit.\n'
}

# HV_VET_LIB=1 sources the adjudication functions above --- decision blocks,
# census freshness and census pins --- without vetting anything, on sluice-drain.sh's HV_DRAIN_LIB precedent. The three-valued
# verdict is a DECISION RULE, and a decision rule that cannot be driven
# directly is one that gets tested through whatever end-to-end path happens to
# exist --- which is how the truncation this whole script exists to prevent
# survived a session.
[ "${HV_VET_LIB:-0}" = 1 ] && return 0

branch="${1:?usage: sluice-vet.sh <branch> <full-sha>}"
ref="${2:?usage: sluice-vet.sh <branch> <full-sha>}"
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root" || exit 1

git fetch -q origin "+refs/heads/${branch}:refs/remotes/origin/${branch}" 2>/dev/null
git fetch -q origin 2>/dev/null

# --verify, AND ^{commit}, AND both are load-bearing.
#
# `git rev-parse <40-hex>` does NOT verify: given a well-formed sha for an
# object this repository has never seen, it ECHOES IT BACK and exits 0. So the
# emptiness test below never fired, `$sha` was set to a commit that does not
# exist, and every git command after it returned nothing --- quietly, since
# each one is `2>/dev/null` or `|| true`. The vet then printed a complete,
# plausible, entirely empty report: no decisions minted, no census, no
# surfaces, no gate machinery, a clean Definition of Done.
#
# That is the worst failure available to an instrument whose whole job is to
# say what a candidate contains. A refusal is read; a clean report is acted on.
# Found 2026-09-14 by pasting a 12-character prefix padded out by hand --- the
# vet answered as confidently for the invented sha as for a real one.
#
# ^{commit} rejects a well-formed object that is not a commit (a tree, a blob,
# an annotated tag pointing at one), which --verify alone admits. This is the
# same pair sluice-mouth.sh already uses; the two now agree.
sha="$(git rev-parse --verify --quiet "${ref}^{commit}" 2>/dev/null)"
if [ -z "$sha" ]; then
    echo "sluice-vet: '$ref' is not a commit in this repository." >&2
    echo "  Nothing was vetted. If this is a 40-character sha, it may be well-formed" >&2
    echo "  and absent — fetch the branch, or check the ref was copied and not typed." >&2
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
        if [ "$d" -lt "$((10#$lo))" ] || [ "$d" -gt "$((10#$hi))" ]; then
            printf '  >> %s is OUTSIDE the block this candidate reserved (%s)\n' "$n" "$block"
            adjudicate_number "$n"
        fi
    done
elif [ -n "$minted" ]; then
    printf '  minted with no reserved block of its own — each number adjudicated below\n'
    for n in $minted; do
        adjudicate_number "$n"
    done
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
    # The ledger question — whose block is this number in, and can that
    # campaign still mint into it — is answered ONCE, by adjudicate_number in
    # the DECISIONS section above. A weaker second copy lived here and did its
    # own awk over the ledger to say only "NOT this campaign": the alarm
    # without the adjudication, in a different voice, immediately under a line
    # that had already called the same number safe. Deleted rather than kept
    # as a cross-check, because it was not an independent derivation — same
    # script, same ledger, strictly less information.
    for r in $(vet_refs); do
        case "$r" in origin/"${branch#origin/}"|"$branch") continue ;; esac
        if git ls-tree -r --name-only "$r" -- docs/decisions/ 2>/dev/null \
             | grep -q "^docs/decisions/${nn}-"; then
            printf '  >> COLLISION: %s also exists on %s\n' "$nn" "$r"
        fi
    done
done

# --- census freshness AGAINST THE BRANCH'S OWN TIP --------------------------
# THE MOST EXPENSIVE RED THIS QUEUE PRODUCES, and the one the SURFACES section
# below cannot see. Those lines report what a candidate TOUCHES. This reports
# ORDER: whether the world-producing sources moved AFTER the census that ships
# with them. A census measures the world THROUGH the code at the moment it
# runs, so a golden taken before the branch's own last world commit describes
# a world that no longer exists, and census_sentinel says so on the canonical
# box after the claim has already been spent.
#
# It discriminates, which is the only reason it is worth printing. Measured
# 2026-09-14 over the three candidates then in flight:
#
#   the-tidemark    census c3e45424e, 5 world-producing files after it
#   anchor          census 965a0e7db, 0 after it
#   the-coherence   census d858f5212, 0 after it
#
# All three changed world-producing code. Only one censused before doing so,
# and it is the one whose goldens disagreed with anchor's on 101 columns.
#
# IT IS FILE-LEVEL AND SAYS SO. A hunk that lands inside `mod tests` in a
# world-producing file still counts here, exactly as it does for the
# `lab metrics.rs` surface below --- campaign/the-tidemark's metrics.rs change
# was entirely inside `mod tests` and was a false alarm on both lines. Reading
# the hunks is the operator's job; this narrows where to look.
projected_merge "$sha" "$branch"
census_freshness "$sha"
census_pins "$sha"

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

# THE CLOSE PACKAGE. A campaign's close record is knowable BEFORE the merge --
# disposition, close date, which gates passed -- and writing it afterwards costs
# a whole second merge slot for a few lines of status text. campaign/the-staple-d4
# paid 1,299 s of canonical-box time on 2026-09-09 to flip `active` to `shipped`:
# artifacts 331 s + outboard 102 s + gate 866 s, on a diff containing no code at
# all. The prose-only phase skip saves `clients` and `heavy`; it does not save
# the two phases that actually cost, because neither scales with the diff.
#
# So this block REPORTS the close package at vet time, where it is still free to
# fix. It does not gate -- same as every other line here, the operator judges.
# A `disposition` still reading `active` on a MERGE is the tell.
# Only a campaign branch has a reconciliation row; a census/* or tooling/*
# branch has none by design and asking is noise.
case "$branch" in
    campaign/*)
        reconciliation_disposition "$sha" "$branch"
        ;;
    *)
        printf '  reconciliation disposition: <n/a — not a campaign branch>\n'
        ;;
esac

# --- does this candidate change the gate that judges it? ---------------------
#
# WHY THIS SECTION EXISTS. A campaign can change its own gate, and should be
# able to: the chamber cds into the merge product to run each phase, so the
# Makefile target a roster line names is the CANDIDATE'S. That is the sanctioned
# lever and it is completely unguarded -- gutting `clients-check-run` or
# `gate-suite-run` weakens the very phase that would have objected, and no
# instrument shows it.
#
# It is not enforceable mechanically: a gate change can be a repair, a
# tightening, or a retreat, and only a reader can tell which. So this REPORTS,
# like every other line here, at the one moment a human is deciding.
#
# THE SECOND HALF IS THE ONE THAT COST A CYCLE. Some gate machinery is read
# from MAIN, not from the candidate, so editing it has NO EFFECT on the run that
# judges it:
#
#   scripts/lane-sets.tsv     sluice-run.sh:502 reads $repo_root/scripts/…,
#                             and repo_root is the script's own location: the
#                             MAIN checkout.
#   scripts/sluice-*.sh       sluice-drain.sh runs $repo_root/scripts/…, same
#                             main checkout. The chamber is main's program.
#
# campaign/the-coherence registered a container runner in lane-sets.tsv on
# 2026-09-14, correctly, and the chamber never read it -- the run took the
# native path and segfaulted exactly as before. Two hours and two merge slots.
# Nothing said the change was inert. This does.
#
# DIRECTION THIS ENFORCES: it names gate machinery the candidate touches and
# says whether this run will read it. It does NOT judge whether the change is
# good, and it does not detect a gate weakened through a path not listed here.
echo
echo "GATE MACHINERY (does this candidate change what judges it?)"
_gate_touched=0
_changed="$(git diff --name-only "origin/main...$sha" 2>/dev/null)"

# Read from MAIN: editing these cannot affect this run.
for p in scripts/lane-sets.tsv scripts/sluice-run.sh scripts/sluice-drain.sh \
         scripts/sluice-queue.sh scripts/sluice-mouth.sh; do
    if printf '%s\n' "$_changed" | grep -qx "$p"; then
        _gate_touched=1
        _inert_paths="${_inert_paths:-}$p
"
        printf '  %-34s INERT HERE — the chamber reads this from MAIN\n' "$p"
    fi
done

# Read from the CANDIDATE: these take effect on this very run.
#
# THE PHASE DRIVERS, and this section was silent about them until 2026-09-14.
# The roster (scripts/lane-sets.tsv) is read from MAIN, but the SCRIPT a roster
# row names is executed inside the chamber worktree, which holds the merge
# product --- so a candidate that edits scripts/lane-outboard.sh changes what
# outboard runs on its own merge.
#
# MEASURED, not reasoned. tooling/the-adjudicator added two suites to
# lane-outboard.sh, and its own merge ran them:
#
#   == outboard: sluice vet blocks
#   == outboard: sluice vet census
#
# The section reported "none — no gate machinery in this diff" for that
# candidate, which is the worst answer available: a confident absence. The
# INERT list above covers the roster and the queue plumbing; nothing covered
# the drivers in between, so the one class of gate change that is BOTH
# self-affecting and easy to get wrong was the class this section could not
# see.
# DERIVED FROM REACHABILITY, NOT FROM A NAME PATTERN. The first version of
# this matched `lane-*.sh` and `gate-*.sh` by name, which is how the ledger
# recorded it as a deliberately narrow stand-in --- and it was too narrow on
# the very next candidate. tooling/the-attribution changes three test SUITES
# that scripts/lane-outboard.sh runs from the merge product, and the section
# reported only an INERT line for sluice-mouth.sh. A candidate that WEAKENS one
# of those suites is the "retreat" this whole section exists to surface, and it
# was the case the name pattern could not see.
#
# Two levels, matching the Makefile handling beside this: the ROSTER is read
# from main (the chamber reads it there), and every script it names is read
# from the CANDIDATE, because that is the copy that executes. Anything either
# of them invokes and this diff changes takes effect on this very run.
#
# SCOPED TO THE PHASES A MERGE ACTUALLY RUNS, derived from sluice-run.sh's own
# `merge_phases=` rather than restated here. The roster also carries rows the
# chamber never runs --- `census` (which sluice-run.sh refuses as a phase
# outright), `seam-guard`, and the commit-rung sets --- and scanning those
# reaches scripts that judge nothing here. census-run.sh mentions
# scripts/worktree-take.sh, so an unscoped scan reported an ordinary helper as
# gate machinery: a SUBJECT under test, which is judged rather than judging.
_phases="$(grep -m1 '^merge_phases=' "$root/scripts/sluice-run.sh" 2>/dev/null \
    | sed 's/^merge_phases="//; s/"$//')"
_roster_scripts=""
for _ph in $_phases; do
    _roster_scripts="$_roster_scripts
$(grep -v '^#' "$root/scripts/lane-sets.tsv" 2>/dev/null \
    | awk -F'\t' -v ph="$_ph" 'NF>=5 && $1==ph {print $5}' | grep -oE 'scripts/[a-z0-9._-]+\.sh')"
done
_roster_scripts="$(printf '%s\n' "$_roster_scripts" | grep -v '^$' | sort -u)"
_reachable="$_roster_scripts"
for _d in $_roster_scripts; do
    _reachable="$_reachable
$(git show "$sha:$_d" 2>/dev/null | grep -oE 'scripts/[a-z0-9._-]+\.sh' | sort -u)"
done
_reachable="$(printf '%s\n' "$_reachable" | grep -v '^$' | sort -u)"
for p in $_reachable; do
    if printf '%s\n' "$_changed" | grep -qx "$p"; then
        # NEVER REPORT ONE PATH BOTH WAYS. A file the block above already
        # called INERT is read from MAIN, and printing a contradicting
        # TAKES EFFECT line beneath it would be worse than printing neither.
        #
        # This is a coherence rule, not an observed fix, and saying so matters:
        # as written today NO path is both --- the merge phases reach 30
        # scripts and none of them is the queue plumbing --- so the guard never
        # fires. It was first written as a hardcoded second copy of the INERT
        # list, which is the worse shape of the same dead code: two lists that
        # can drift apart with nothing to notice. It now reads the one list.
        if printf '%s' "${_inert_paths:-}" | grep -qx "$p"; then
            continue
        fi
        _gate_touched=1
        printf '  %-34s TAKES EFFECT — a phase reaches this script FROM the\n' "$p"
        printf '  %-34s   merge product, so it judges its own merge\n' ""
    fi
done

if printf '%s\n' "$_changed" | grep -q '^scripts/hooks/'; then
    _gate_touched=1
    printf '  %-34s TAKES EFFECT — core.hooksPath is relative, so the\n' "scripts/hooks/"
    printf '  %-34s   chamber runs the hooks from THIS candidate\n' ""
fi

# The Makefile targets the roster actually names, DERIVED from the roster rather
# than hardcoded — a hardcoded copy is the drift cli/tests/suite/lane_sets.rs
# exists to fail on.
if printf '%s\n' "$_changed" | grep -qx Makefile; then
    # ONE LEVEL OF EXPANSION, because the roster names an AGGREGATE. The
    # `clients` row invokes `clients-check-run`, whose recipe fans out to six
    # others -- and campaign/the-coherence changed `visual-check-run`, which
    # the roster never mentions. Checking only roster-named targets missed the
    # very change that motivated this section.
    _roster="$(grep -v '^#' "$root/scripts/lane-sets.tsv" 2>/dev/null \
        | awk -F'\t' 'NF>=5{print $5}' | grep -oE '[a-z0-9-]+(-run|-check)' | sort -u)"
    _called=""
    for _r in $_roster; do
        _called="$_called $(git show "$sha:Makefile" 2>/dev/null \
            | awk -v t="$_r:" '$0 ~ "^"t {f=1;next} f && /^[^\t]/ {f=0} f' \
            | grep -oE '[a-z0-9-]+(-run|-check)' | sort -u)"
    done
    _targets="$(printf '%s %s' "$_roster" "$_called" | tr ' ' '\n' | grep -v '^$' | sort -u)"
    for t in $_targets; do
        _before="$(git show "origin/main:Makefile" 2>/dev/null | awk -v t="$t:" '$0 ~ "^"t {f=1;next} f && /^[^\t]/ {f=0} f')"
        _after="$(git show "$sha:Makefile" 2>/dev/null | awk -v t="$t:" '$0 ~ "^"t {f=1;next} f && /^[^\t]/ {f=0} f')"
        if [ "$_before" != "$_after" ]; then
            _gate_touched=1
            printf '  %-34s TAKES EFFECT — a phase runs this target\n' "Makefile: $t"
        fi
    done
fi

if [ "$_gate_touched" = 0 ]; then
    echo "  none — no gate machinery in this diff"
else
    echo "  ^ read the diff for these before dispatching. A gate change may be a"
    echo "    repair or a retreat and only you can tell which."
fi

echo
echo "SHAPE"
git diff --shortstat "origin/main...$sha" 2>/dev/null | sed 's/^ */  /'
printf '  non-prose paths: %s\n' "$(git diff --name-only "origin/main...$sha" 2>/dev/null | grep -cvE '^(book/|docs/)')"

echo
printf 'operator checkout: %s at %s  (== origin/main: %s)\n' \
    "$(git rev-parse --abbrev-ref HEAD)" "$(git rev-parse --short HEAD)" \
    "$([ "$(git rev-parse HEAD)" = "$(git rev-parse origin/main)" ] && echo yes || echo 'NO — ff before draining')"
exit 0
