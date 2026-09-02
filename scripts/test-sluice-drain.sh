#!/usr/bin/env bash
# Tests for the merge queue's drain harness (scripts/sluice-drain.sh).
#
# WHY THIS EXISTS. Every script the drain CALLS was committed and tested; the
# drain itself was not, and it caused two defects in one night (2026-08-27):
# it gated a census on a merge verdict that could not apply to it, and it read
# a job's kind correctly while dispatching on it in a second, separate place.
# Both were one-line rules with no test.
#
# These tests exercise the RULES, not the box: sourcing with HV_DRAIN_LIB=1
# loads the functions without draining anything.
set -u
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=/dev/null
HV_DRAIN_LIB=1 . "$root/scripts/sluice-drain.sh"

# --- the mouth rule --------------------------------------------------------
# A census does not merge main, so a merge-conflict verdict cannot gate it.
if mouth_applies_to census; then
    bad "the mouth gates a census — this refuses a valid run over a conflict a census never reads"
else
    ok "the mouth does NOT gate a census"
fi

# The control. Without these the rule above is satisfied by a function that
# always returns false, which would delete the mouth from the queue entirely.
for k in merge stage; do
    if mouth_applies_to "$k"; then
        ok "the mouth DOES gate kind=$k"
    else
        bad "the mouth stopped gating kind=$k — nothing now refuses a conflicting $k before the box"
    fi
done

# An unknown kind must be GATED, not waved through: failing toward the check is
# the safe direction when the queue grows a kind this file has not heard of.
if mouth_applies_to some-future-kind; then
    ok "an unknown kind is gated by the mouth (fails toward checking)"
else
    bad "an unknown kind bypasses the mouth — a new kind would silently skip the gate"
fi

# --- the dispatch rule -----------------------------------------------------
# census-run.sh takes the shared claim itself and removes the claim file on
# exit, so a census must NEVER be handed to sluice-run.sh.
if [ "$(dispatch_for census)" = "scripts/sluice-census.sh" ]; then
    ok "kind=census dispatches to sluice-census.sh"
else
    bad "kind=census dispatches to $(dispatch_for census) — nesting it in a chamber job clobbers that job's own claim"
fi
for k in merge stage some-future-kind; do
    if [ "$(dispatch_for "$k")" = "scripts/sluice-run.sh" ]; then
        ok "kind=$k dispatches to sluice-run.sh"
    else
        bad "kind=$k dispatches to $(dispatch_for "$k")"
    fi
done

# --- the two rules must agree ---------------------------------------------
# The defect was not either rule alone: it was the two disagreeing about what a
# census IS. Anything the mouth declines to gate must also be the thing that
# does not go to the chamber, or the exemption is attached to the wrong job.
for k in census merge stage some-future-kind; do
    skips_mouth=no;  mouth_applies_to "$k" || skips_mouth=yes
    off_chamber=no;  [ "$(dispatch_for "$k")" != "scripts/sluice-run.sh" ] && off_chamber=yes
    if [ "$skips_mouth" = "$off_chamber" ]; then
        ok "kind=$k: mouth-exempt and chamber-exempt agree ($skips_mouth)"
    else
        bad "kind=$k: mouth-exempt=$skips_mouth but chamber-exempt=$off_chamber — the two rules disagree about what this kind is"
    fi
done

# --- census_note: the queue row must READ the verdict, not assert one --------
# The row said "Goldens delivered" for every rc=0 census, including the ones that
# moved nothing. Fixing sluice-census.sh's log left this second copy of the claim
# standing on the surface `make sluice-status` shows. campaign/the-pawl printed
# "NO GOLDENS MOVED" in its log and "Goldens delivered" in its row, same minute.
tmpl="$(mktemp -d)"
trap 'rm -rf "$tmpl"' EXIT
printf 'sluice-census: NO GOLDENS MOVED — the census agrees with abc.\n' > "$tmpl/null.log"
printf 'sluice-census: 3 golden path(s) moved; delivering on census/x\n'  > "$tmpl/moved.log"

case "$(census_note "$tmpl/null.log")" in
    *"NO GOLDENS MOVED"*) ok "census_note reports a NULL run as no goldens moved" ;;
    *) bad "census_note called a null run a delivery" ;;
esac
case "$(census_note "$tmpl/moved.log")" in
    *"Goldens moved"*) ok "census_note reports a real move as goldens moved" ;;
    *) bad "census_note called a real golden move a null" ;;
esac
# FAILS TOWARD "MOVED": a missing log must not be reported as a null, because a
# false null invites skipping a delivery branch that was needed.
case "$(census_note "$tmpl/does-not-exist.log")" in
    *"Goldens moved"*) ok "a missing log fails toward 'moved', not toward a false null" ;;
    *) bad "a missing log was reported as a null — the unsafe direction" ;;
esac
case "$(census_note "")" in
    *"Goldens moved"*) ok "an empty log path also fails toward 'moved'" ;;
    *) bad "an empty log path was reported as a null" ;;
esac

printf '\ntest-sluice-drain: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
