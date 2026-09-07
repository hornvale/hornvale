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

# --- run_one DISPATCHES, and passes the request ID -------------------------
# THE GAP (fix round 3, Important 3): this file only ever SOURCED the library
# and tested its two decision rules. `run_one` — the thing that actually calls
# a runner — was invoked by nothing, so this campaign's change to the dispatch
# line (`bash "$runner" "$ID"`, replacing the old branch/sha/kind positionals)
# had no witness at all. A drain that picks the right runner and then calls it
# with the wrong argv dispatches nothing, and every assertion above still
# passes.
#
# kind=census, deliberately: it is the one kind `mouth_applies_to` skips, so
# this reaches the dispatch line without invoking sluice-mouth.sh or any real
# merge machinery. `repo_root` and `dispatch_for` are both overridden AFTER
# sourcing, so nothing here can touch the real checkout — and the git
# environment is scrubbed for the same reason, since `run_one`'s own `git -C`
# calls do not scrub it themselves and GIT_DIR outranks `-C`.
unset GIT_DIR GIT_INDEX_FILE GIT_WORK_TREE GIT_COMMON_DIR
wt="$tmpl/drain-scratch"
mkdir -p "$wt/scripts"
git init -q -b main "$wt" 2>/dev/null
git -C "$wt" config user.email d@d 2>/dev/null
git -C "$wt" config user.name d 2>/dev/null
printf 'x\n' > "$wt/f.txt"
git -C "$wt" add -A 2>/dev/null
git -C "$wt" commit -qm root 2>/dev/null
# `run_one` reads origin/main before and after; a local ref satisfies it
# without a remote, and the `fetch` it attempts fails quietly by design.
git -C "$wt" update-ref refs/remotes/origin/main HEAD 2>/dev/null

argv_log="$tmpl/dispatch-argv"; : > "$argv_log"
cat > "$wt/scripts/dispatch-stub.sh" <<STUB
#!/usr/bin/env bash
printf '%s\n' "\$#" > "$argv_log"
printf '%s\n' "\$@" >> "$argv_log"
STUB
chmod +x "$wt/scripts/dispatch-stub.sh"
# A stub queue too, so the terminal `set-state` needs neither the real state
# directory nor a built binary. What is under test is the DISPATCH argv.
setstate_log="$tmpl/setstate-argv"; : > "$setstate_log"
cat > "$wt/scripts/sluice-queue.sh" <<STUB
#!/usr/bin/env bash
printf '%s\n' "\$@" >> "$setstate_log"
STUB
chmod +x "$wt/scripts/sluice-queue.sh"

# shellcheck disable=SC2034  # read by run_one, which was sourced from sluice-drain.sh
repo_root="$wt"
dispatch_for() { echo "scripts/dispatch-stub.sh"; }
run_one "$(printf 'TS\treq-witness\tcampaign/w\tabcdef012345\trunning\tcensus\tnote')" \
    >/dev/null 2>&1

if [ "$(sed -n 1p "$argv_log")" = "1" ] && [ "$(sed -n 2p "$argv_log")" = "req-witness" ]; then
    ok "run_one calls the runner with exactly one argument, the request ID"
else
    bad "run_one dispatched argv '$(tr '\n' ' ' < "$argv_log")' — the runner is being told WHAT to run instead of WHICH ROW authorised it"
fi
# The terminal state is still written, and to the same id. A dispatch witness
# that ignored this would pass on a drain that ran the job and then left the row
# reading `running` forever — the ghost row this whole interlock exists to
# prevent.
if grep -q '^req-witness$' "$setstate_log" && grep -q '^reported$' "$setstate_log"; then
    ok "run_one writes the terminal state for the same id it dispatched"
else
    bad "run_one's set-state argv was '$(tr '\n' ' ' < "$setstate_log")' — a finished job must not stay 'running'"
fi


echo "== announce: a terminal state reaches the board, and can never fail the drain"
# The queue read the board on the way IN (sluice-request.sh's hold-off
# advisory) and wrote nothing on the way OUT, so a campaign's only routes to
# its own outcome were polling sluice-status or being told by the operator.
# These pin the two properties that make announcing safe to add.
# The drain is already sourced at the top of this file (line 19); sourcing it
# a second time here made shellcheck FOLLOW it under the full-glob invocation
# and then report the stub `dispatch_for` below as unreachable. One source is
# enough and the warning was real about my redundancy, not about the stub.
if type announce >/dev/null 2>&1; then
    ok "announce is exposed at module scope (the HV_DRAIN_LIB seam can reach it)"
else
    bad "announce is not defined after sourcing with HV_DRAIN_LIB=1 — it is nested inside a function"
fi
# shellcheck disable=SC2034  # read by announce(), sourced from sluice-drain.sh
BR="test/announce"
# ASSERT THE OUTPUT, NOT THE EXIT CODE. announce returns 0 unconditionally by
# design — the whole point is that it cannot fail a landed merge — so `if
# announce ...` is true whatever happens inside and proves nothing. An earlier
# draft of this test asserted exactly that and passed against a mutant with the
# `-x` guard removed. The observable that actually differs is stderr: a missing
# binary is SILENT, a present-but-failing one WARNS.
_saved_root="$repo_root"; repo_root="$tmpl/no-such-repo"
_out="$(announce "TEST" "no board binary here" 2>&1)"; _rc=$?
if [ "$_rc" -eq 0 ] && [ -z "$_out" ]; then
    ok "a checkout with no board binary is silent and returns 0"
else
    bad "missing-binary path: rc=$_rc output='$_out' (expected rc=0 and silence)"
fi
# A binary that EXISTS but cannot post must warn, and still return 0.
mkdir -p "$tmpl/fakerepo/tools/board/target/release"
printf '#!/bin/sh\nexit 3\n' > "$tmpl/fakerepo/tools/board/target/release/board"
chmod +x "$tmpl/fakerepo/tools/board/target/release/board"
repo_root="$tmpl/fakerepo"
_out="$(announce "TEST" "board refuses" 2>&1)"; _rc=$?
if [ "$_rc" -eq 0 ] && printf '%s' "$_out" | grep -q "could not announce"; then
    ok "a failing board warns on stderr and still returns 0 (the row stays authoritative)"
else
    bad "failing-post path: rc=$_rc output='$_out' (expected rc=0 and a warning)"
fi
repo_root="$_saved_root"
if HV_SLUICE_SKIP_BOARD=1 announce "TEST" "suppressed"; then
    ok "HV_SLUICE_SKIP_BOARD=1 suppresses the post and still returns 0"
else
    bad "the skip path returned non-zero"
fi

printf '\ntest-sluice-drain: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]