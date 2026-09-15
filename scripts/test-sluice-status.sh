#!/usr/bin/env bash
# Tests for scripts/sluice-status.sh.
#
# THE ONE INVARIANT THAT MATTERS: the LIVE section is never truncated. This
# command exists so an operator can see what needs acting on; a status view
# that hides a held row to stay short is worse than the 614-line dump it
# replaced, because the omission is invisible. History may be trimmed freely.
set -u
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
q="$tmp/queue.tsv"

row() { printf '%s\treq-%s\t%s\t%040d\t%s\t%s\t%s\n' "$1" "$2" "$3" "$2" "$4" "$5" "$6" >> "$q"; }

# 12 finished rows, then 3 live ones.
i=0
while [ "$i" -lt 12 ]; do
    row "2026-09-01T00:00:0${i}Z" "$i" "campaign/old-$i" landed merge "all merge phases rc=0"
    i=$((i + 1))
done
# A LIVE row with BOTH a 400-char note and a branch longer than any real one.
# The live section is never trimmed, so this is the only place an overlong
# value is guaranteed to reach the renderer -- which is exactly why the width
# assertion must be driven from here and not from history.
row 2026-09-02T00:00:00Z 90 census/079f6963cd3a-20260910T002427Z-warrantfloorfix-and-then-some held stage "$(printf 'x%.0s' $(seq 1 400))"
row 2026-09-02T00:00:01Z 91 campaign/b queued merge ""
row 2026-09-02T00:00:02Z 92 campaign/c running census "launched by the drain loop"

out="$(HV_SLUICE_DIR="$tmp" bash "$root/scripts/sluice-status.sh")"

echo "== the live section is complete"
for br in census/079f6963cd3a campaign/b campaign/c; do
    if printf '%s' "$out" | grep -q "$br"; then ok "live row $br is shown"
    else bad "live row $br was HIDDEN — a status view that omits live work is worse than no pruning"; fi
done

echo "== history is trimmed, and to the RECENT tail"
n_old="$(printf '%s' "$out" | grep -c "campaign/old-" || true)"
if [ "$n_old" -le 8 ]; then ok "finished rows trimmed to the tail ($n_old of 12 shown)"
else bad "finished rows were not trimmed ($n_old of 12)"; fi
# The control: trimming must keep the NEWEST, not whichever the filter met first.
if printf '%s' "$out" | grep -q "campaign/old-11"; then ok "the most recent finished row survives the trim"
else bad "the trim dropped the newest finished row — it is keeping the wrong end"; fi
if printf '%s' "$out" | grep -q "campaign/old-0 "; then
    bad "the oldest finished row is still shown — the tail is not being taken"
else ok "the oldest finished row is trimmed away"; fi

echo "== a long note cannot blow up the width"
# MEASURED WITH `wc -L`, WHICH IS DISPLAY WIDTH. The first version used
# `awk length()`, which counts BYTES in a byte-oriented locale, and reported
# 161 for a 157-column line: the two "…" ellipses this renderer inserts are
# three bytes each. The assertion failed while the code was right, which is the
# worst kind of test -- it sends you to fix the wrong file.
longest="$(printf '%s\n' "$out" | wc -L)"
if [ "$longest" -le 160 ]; then ok "no line exceeds 160 columns (longest $longest) despite a 400-char note and a 58-char branch"
else bad "a line ran to $longest columns — the note is not being truncated"; fi
# Control: truncation must not eat a SHORT note.
if printf '%s' "$out" | grep -q "launched by the drain loop"; then
    ok "a short note is printed in full"
else bad "a short note was mangled by the truncation"; fi

echo "== nothing is lost: ALL=1 still prints every row"
n_all="$(HV_SLUICE_DIR="$tmp" HV_STATUS_ALL=1 bash "$root/scripts/sluice-status.sh" | grep -c . || true)"
if [ "$n_all" -ge 15 ]; then ok "ALL=1 prints every row ($n_all)"
else bad "ALL=1 printed only $n_all of 15 rows"; fi

echo "== an empty or absent queue does not crash"
if HV_SLUICE_DIR="$tmp/nope" bash "$root/scripts/sluice-status.sh" >/dev/null 2>&1; then
    ok "a missing queue exits 0 with a message"
else bad "a missing queue exited non-zero"; fi
: > "$q"
if out_empty="$(HV_SLUICE_DIR="$tmp" bash "$root/scripts/sluice-status.sh" 2>&1)"; then
    if printf '%s' "$out_empty" | grep -q "nothing queued, running or held"; then
        ok "an empty queue says so rather than printing a bare header"
    else bad "an empty queue printed no explanation"; fi
else bad "an empty queue exited non-zero"; fi

printf '\ntest-sluice-status: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
