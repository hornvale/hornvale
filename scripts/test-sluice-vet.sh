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

printf '\ntest-sluice-vet: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
