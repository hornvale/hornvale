#!/usr/bin/env bash
# The one property sluice-vet.sh exists to hold: IT NEVER TRUNCATES THE MOUTH.
#
# `tail -2` on the mouth's verdict survived an entire session of hand-typed
# vets because it is correct whenever a candidate has one conflict. It gave
# campaign/the-legend a false "two mechanical conflicts" reading when the real
# set was eight, two of which an operator must never resolve. This test is the
# thing that would have caught it.
set -u
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

stub="$(mktemp)"; trap 'rm -f "$stub"' EXIT
cat > "$stub" <<'STUB'
#!/usr/bin/env bash
echo "sluice-mouth: MERGE CONFLICT between origin/main and deadbeef."
for i in 1 2 3 4 5 6 7 8; do echo "  conflict: path/number-$i.rs"; done
exit 1
STUB
chmod +x "$stub"

out="$(cd "$root" && HV_VET_MOUTH="$stub" bash scripts/sluice-vet.sh main HEAD 2>&1)"

seen=0
for i in 1 2 3 4 5 6 7 8; do
    printf '%s' "$out" | grep -q "number-$i.rs" && seen=$((seen+1))
done
if [ "$seen" -eq 8 ]; then
    ok "all 8 conflict lines survive to the operator's screen"
else
    bad "only $seen of 8 conflict lines survived — the mouth is being truncated"
fi

if printf '%s' "$out" | grep -q '8 conflict(s)'; then
    ok "the count matches what was printed"
else
    bad "the conflict count disagrees with the printed lines"
fi

# Control: with a single-conflict verdict the same code path must still work,
# because that is the case under which truncation looks correct.
cat > "$stub" <<'STUB'
#!/usr/bin/env bash
echo "sluice-mouth: MERGE CONFLICT between origin/main and deadbeef."
echo "  conflict: only/one.rs"
exit 1
STUB
out1="$(cd "$root" && HV_VET_MOUTH="$stub" bash scripts/sluice-vet.sh main HEAD 2>&1)"
if printf '%s' "$out1" | grep -q 'only/one.rs' && printf '%s' "$out1" | grep -q '1 conflict(s)'; then
    ok "CONTROL: a single-conflict verdict still reports correctly"
else
    bad "control failed: single-conflict path is broken"
fi

# --- cross-branch decision collisions --------------------------------------
# The check these pin reads the one object no gate ever assembles: every
# campaign branch at once. A duplicate decision ID is invisible to
# docs_consistency.rs because the chamber gates main plus ONE branch, so both
# colliding branches pass green and the duplicate lands. Two campaigns both
# minted 0134 that way.
#
# THE POSITIVE CONTROL IS THE LOAD-BEARING ONE. When this check was written the
# live case produced NO collision line — correctly, since only one branch
# carried the number — and a silent check is indistinguishable from a broken
# one. It is only evidence once you force the effect and watch it fire.
probe="refs/remotes/origin/campaign/zz-vet-probe"
# The probe branch must MINT something — a decision new relative to main. The
# vet computes `minted` from `git diff origin/main...<sha>`, so a number already
# on main is never in that list and no collision loop ever runs for it. Picking
# the last decision in the branch TREE instead is what this test did first, and
# it produced a red that looked like a broken check and was a broken test.
cand=""
for c in $(cd "$root" && git for-each-ref --format="%(refname:short)" refs/remotes/origin/campaign/ 2>/dev/null); do
    if (cd "$root" && git diff --name-only "origin/main...$c" 2>/dev/null | grep -q "^docs/decisions/0"); then
        cand="$c"; break
    fi
done
if [ -z "$cand" ]; then
    ok "SKIP: no campaign branches present to build a collision probe from"
else
    num="$(cd "$root" && git diff --name-only "origin/main...$cand" 2>/dev/null \
             | sed -n "s|^docs/decisions/\([0-9]\{4\}\)-.*|\1|p" | tail -1)"
    (cd "$root" && git update-ref "$probe" "$cand" 2>/dev/null)
    br="${cand#origin/}"
    outp="$(cd "$root" && HV_VET_REFS="origin/campaign/zz-vet-probe" bash scripts/sluice-vet.sh "$br" "$cand" 2>&1)"
    if [ -z "$num" ]; then
        ok "SKIP: $cand mints no decision, so no collision can be forced"
    elif printf "%s" "$outp" | grep -q "COLLISION: $num also exists on origin/campaign/zz-vet-probe"; then
        ok "POSITIVE CONTROL: a second ref carrying $num really fires COLLISION"
    else
        bad "the collision check did NOT fire against a ref that carries $num"
    fi
    outn="$(cd "$root" && HV_VET_REFS="origin/main" bash scripts/sluice-vet.sh "$br" "$cand" 2>&1)"
    if [ "$(printf "%s" "$outn" | grep -c COLLISION)" -eq 0 ]; then
        ok "NEGATIVE CONTROL: a ref without the number stays silent"
    else
        bad "the collision check fired against a ref that does not carry the number"
    fi
    outs="$(cd "$root" && HV_VET_REFS="$cand" bash scripts/sluice-vet.sh "$br" "$cand" 2>&1)"
    if [ "$(printf "%s" "$outs" | grep -c COLLISION)" -eq 0 ]; then
        ok "SELF-EXCLUSION: a candidate does not collide with its own branch"
    else
        bad "the candidate collided with itself"
    fi
    (cd "$root" && git update-ref -d "$probe" 2>/dev/null)
fi

printf "\ntest-sluice-vet: %d passed, %d failed\n" "$pass" "$fail"
[ "$fail" -eq 0 ]
