#!/usr/bin/env bash
# scripts/test-sluice-vet-blocks.sh — the vet's decision-block adjudication.
#
# WHAT THIS PINS AND WHY IT IS WORTH A FILE. `sluice-vet.sh` used to answer a
# minted-outside-its-block number with homework: "verify the range is free and
# its owner finished". The operator then did that by hand — three times, the
# last of which (campaign/the-tidemark's 0959, inside campaign/the-planetarium's
# reserved 0956-0965) reversed a hold that had already been posted to the
# board. A rule enforced by hand twice belongs in code.
#
# The verdict is THREE-VALUED and the third value is the whole point. A
# campaign running in a worktree that has never pushed is indistinguishable
# from one that never existed, so UNKNOWN exists to stop the adjudicator
# reporting that absence as safety. Each arm below asserts its own text AND
# the absence of the other two, because an arm that merely fires is not an arm
# that discriminates.
#
# GIT HERMETICITY. Every git call goes through `g`, which strips GIT_DIR and
# GIT_INDEX_FILE. A temp directory is not isolation when the environment names
# a repository (scripts/CLAUDE.md), and this suite's sibling
# test-sluice-vet-gate.sh rewrote the chamber's real index on 2026-09-14 by
# getting exactly this wrong. The self-check at the end is that incident's
# durable half.
set -u

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
g() { env -u GIT_DIR -u GIT_INDEX_FILE git "$@"; }

fails=0
ok()  { printf '  ok: %s\n' "$1"; }
bad() { printf '  FAIL: %s\n' "$1"; fails=$((fails + 1)); }

# The index of the worktree this suite runs in, before it does anything. Any
# difference at the end means this file wrote to a repository it does not own.
index_before="$(g -C "$root" write-tree 2>/dev/null || echo unavailable)"

scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT

# --- a scratch repository with three campaigns in three states --------------
repo="$scratch/repo"
mkdir -p "$repo"
g -C "$repo" init -q -b main
g -C "$repo" config user.email vet@test
g -C "$repo" config user.name vet-test

mkdir -p "$repo/book/src/chronicle" "$repo/docs/retrospectives"
echo seed > "$repo/seed.txt"
g -C "$repo" add -A
g -C "$repo" commit -q -m seed

# the-closed: merged, with both halves of a close package on main.
echo chronicle > "$repo/book/src/chronicle/the-closed.md"
echo retro     > "$repo/docs/retrospectives/the-closed.md"
g -C "$repo" add -A
g -C "$repo" commit -q -m close
g -C "$repo" update-ref refs/remotes/origin/main HEAD
# Its branch points at an ancestor of main, which is what "merged" looks like.
g -C "$repo" update-ref refs/remotes/origin/campaign/the-closed HEAD~1

# the-live: a branch ahead of main.
g -C "$repo" checkout -q -b tmp-live
echo work > "$repo/work.txt"
g -C "$repo" add -A
g -C "$repo" commit -q -m work
g -C "$repo" update-ref refs/remotes/origin/campaign/the-live HEAD
g -C "$repo" checkout -q main

# the-silent: no ref, no chronicle, no retrospective.

# --- a synthetic allocator ledger -------------------------------------------
export HV_BLOCK_DIR="$scratch/blocks"
mkdir -p "$HV_BLOCK_DIR"
{
    printf '2026-01-01T00:00:00Z\tthe-closed\t0100\t0109\n'
    printf '2026-01-02T00:00:00Z\tcampaign/the-live\t0110\t0119\n'
    printf '2026-01-03T00:00:00Z\tthe-silent\t0120\t0129\n'
} > "$HV_BLOCK_DIR/blocks.tsv"

# --- drive the decision rule directly ---------------------------------------
# shellcheck source=/dev/null  # resolved at runtime from $root
HV_VET_LIB=1 . "$root/scripts/sluice-vet.sh"

# campaign_status reads the repository it is run in, so run it in the scratch.
# shellcheck disable=SC2016  # the body is deliberately unexpanded; it
# takes $root and the number as positional arguments to the inner shell.
adj() ( cd "$repo" && env -u GIT_DIR -u GIT_INDEX_FILE bash -c '
    HV_VET_LIB=1 . "$1/scripts/sluice-vet.sh"; adjudicate_number "$2"' _ "$root" "$1" )

echo "== the ledger's campaign column is normalised"
owner="$(cd "$repo" && block_owner_of 0115 | cut -f1)"
if [ "$owner" = "the-live" ]; then
    ok "a 'campaign/'-prefixed ledger row reports the bare slug ($owner)"
else
    bad "expected 'the-live' from a 'campaign/the-live' ledger row, got '$owner'"
fi

echo "== CLOSED — merged, chronicle and retrospective both on main"
out="$(adj 0105)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *CLOSED*) ok "reports CLOSED" ;; *) bad "did not report CLOSED" ;; esac
case "$out" in *"the number is safe"*) ok "licenses the number" ;; *) bad "did not license the number" ;; esac
case "$out" in *LIVE*|*UNKNOWN*) bad "CLOSED arm also emitted another verdict" ;; *) ok "no other verdict leaked in" ;; esac

echo "== LIVE — a pushed branch ahead of main can still mint"
out="$(adj 0115)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *LIVE*) ok "reports LIVE" ;; *) bad "did not report LIVE" ;; esac
case "$out" in *"COLLISION RISK"*) ok "names the risk" ;; *) bad "did not name the risk" ;; esac
case "$out" in *CLOSED*|*"is safe"*) bad "LIVE arm licensed the number" ;; *) ok "licenses nothing" ;; esac

echo "== UNKNOWN — no pushed branch and no close package is NOT safety"
out="$(adj 0125)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *UNKNOWN*) ok "reports UNKNOWN" ;; *) bad "did not report UNKNOWN" ;; esac
case "$out" in *"Do NOT read this as safe"*) ok "refuses to license it" ;; *) bad "did not refuse to license it" ;; esac
case "$out" in *CLOSED*|*"is safe"*) bad "UNKNOWN arm licensed the number" ;; *) ok "licenses nothing" ;; esac

echo "== a number the allocator never issued"
out="$(adj 0500)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"NO reserved block"*) ok "says the allocator never issued it" ;; *) bad "did not say so" ;; esac
case "$out" in *CLOSED*|*LIVE*|*UNKNOWN*) bad "invented an owner for an unissued number" ;; *) ok "invents no owner" ;; esac

# --- the reconciliation row: a branch name is not a campaign slug -----------
# campaign/the-coherence's campaign is `the-coherent-ground`, and the vet
# reported "<NO ROW for the-coherence>" while the row sat there, correct,
# keyed by the slug. The substring fallback could not save it either --- the
# two names diverge at the eleventh character. The authority is the chronicle
# and retrospective a merge candidate ADDS, because a merge is the last act of
# a campaign.
# shellcheck disable=SC2016  # deliberately unexpanded; positional args.
recon() ( cd "$repo" && env -u GIT_DIR -u GIT_INDEX_FILE bash -c '
    HV_VET_LIB=1 . "$1/scripts/sluice-vet.sh"; reconciliation_disposition "$(git rev-parse HEAD)" "$2"' _ "$root" "$2" )

build_recon() {
    rm -rf "$repo"; mkdir -p "$repo"
    g -C "$repo" init -q -b main
    g -C "$repo" config user.email vet@test
    g -C "$repo" config user.name vet-test
    mkdir -p "$repo/book/src/chronicle" "$repo/docs/retrospectives" "$repo/docs/audits"
    printf 'slug\tdisposition\n' > "$repo/docs/audits/campaign-reconciliation.tsv"
    g -C "$repo" add -A; g -C "$repo" commit -q -m base
    g -C "$repo" update-ref refs/remotes/origin/main HEAD
    g -C "$repo" checkout -q -b work
}

echo "== the row is found by the SLUG the candidate's chronicle names"
build_recon
printf 'chronicle\n' > "$repo/book/src/chronicle/the-coherent-ground.md"
printf 'the-coherent-ground\tactive\n' >> "$repo/docs/audits/campaign-reconciliation.tsv"
g -C "$repo" add -A; g -C "$repo" commit -q -m close
out="$(recon x campaign/the-coherence)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *active*) ok "finds the row under a slug the branch name does not contain" ;; *) bad "did not find it: $out" ;; esac
case "$out" in *"NO ROW"*) bad "reported a false absence — the exact bug" ;; *) ok "reports no absence" ;; esac
case "$out" in *the-coherent-ground*) ok "names the key it matched on" ;; *) bad "did not name the key" ;; esac

echo "== the branch name still works when it IS the slug"
build_recon
printf 'chronicle\n' > "$repo/book/src/chronicle/the-tidemark.md"
printf 'the-tidemark\tshipped\n' >> "$repo/docs/audits/campaign-reconciliation.tsv"
g -C "$repo" add -A; g -C "$repo" commit -q -m close
out="$(recon x campaign/the-tidemark)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *shipped*) ok "the ordinary case is unaffected" ;; *) bad "broke the ordinary case: $out" ;; esac

echo "== a genuinely missing row is still reported, naming every key tried"
# ANTI-VACUITY. 440 rows still read `active`, so a real absence is a real
# signal; a lookup that widened until it always matched would be worse than
# the bug it replaced.
build_recon
printf 'chronicle\n' > "$repo/book/src/chronicle/the-absent.md"
g -C "$repo" add -A; g -C "$repo" commit -q -m close
out="$(recon x campaign/the-absent-branch)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"NO ROW"*) ok "a real absence is still reported" ;; *) bad "invented a row: $out" ;; esac
case "$out" in *the-absent*) ok "names the keys it tried" ;; *) bad "did not name the keys tried" ;; esac

# --- the incident's durable half -------------------------------------------
index_after="$(g -C "$root" write-tree 2>/dev/null || echo unavailable)"
if [ "$index_before" != "$index_after" ]; then
    bad "THIS SUITE REWROTE THE INDEX of the worktree it ran in ($index_before -> $index_after)"
fi

if [ "$fails" -eq 0 ]; then
    echo "test-sluice-vet-blocks: PASS"
else
    echo "test-sluice-vet-blocks: $fails FAILED"
fi
exit $((fails > 0))
