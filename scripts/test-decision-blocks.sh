#!/usr/bin/env bash
# scripts/test-decision-blocks.sh — property tests for the block allocator.
#
# Shaped after test-sluice.sh: pin the properties the allocator would be
# worthless without, rather than asserting a file exists. SKIPs on a host with
# no flock, since the allocator only ever runs on the canonical box.
set -uo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
if ! command -v flock >/dev/null 2>&1; then
    echo "test-decision-blocks: SKIP — no flock on this host ($(uname -s));"
    echo "test-decision-blocks:        decision-block.sh only runs on the canonical box."
    exit 0
fi
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
export HV_BLOCK_DIR="$tmp/blocks"
blk() { bash "$repo_root/scripts/decision-block.sh" "$@"; }

echo "== blocks: two campaigns never overlap"
a="$(blk take campaign/a 2>/dev/null)"; b="$(blk take campaign/b 2>/dev/null)"
a_end=$((10#$(echo "$a" | awk '{print $2}'))); b_start=$((10#$(echo "$b" | awk '{print $1}')))
if [ "$b_start" -gt "$a_end" ]; then
    ok "the second block starts strictly above the first's end"
else
    bad "blocks overlap: a=$a b=$b — two campaigns would author the same number"
fi

echo "== blocks: a block never starts at or below main's real ceiling"
ceil=$((10#$(blk ceiling))); a_start=$((10#$(echo "$a" | awk '{print $1}')))
if [ "$a_start" -gt "$ceil" ]; then
    ok "the first block starts above main's ceiling ($ceil), so it cannot collide with a landed record"
else
    bad "a block started at or below main's ceiling — it would collide with records already on main"
fi

echo "== blocks: the ledger is the authority, not the ceiling alone"
# A third take must clear the LAST BLOCK's end, not merely main's ceiling —
# otherwise every campaign taking a block on the same unchanged main would be
# handed the identical range.
c="$(blk take campaign/c 2>/dev/null)"; c_start=$((10#$(echo "$c" | awk '{print $1}')))
b_end=$((10#$(echo "$b" | awk '{print $2}')))
if [ "$c_start" -gt "$b_end" ]; then
    ok "a third block clears the second's end, so an unchanged main does not re-issue a range"
else
    bad "third block $c does not clear the second's end $b_end — the ledger is being ignored"
fi

echo "== blocks: MUTATION — an allocator that ignored the ledger would re-issue the same range"
mut="$tmp/mutant.sh"
# shellcheck disable=SC2016  # matching the mutant's SOURCE TEXT, not evaluating it
sed -e 's/^    \[ "\$last_end" -gt "\$watermark" \] && watermark="\$last_end"$/    :/' \
    "$repo_root/scripts/decision-block.sh" > "$mut"
if grep -qE '^    :$' "$mut"; then
    ok "test setup: the mutant ignores the ledger's high-water mark"
else
    bad "the mutation sed did not apply — the assertion below would pass vacuously"
fi
export HV_BLOCK_DIR="$tmp/mutblocks"
m1="$(bash "$mut" take campaign/m1 2>/dev/null)"; m2="$(bash "$mut" take campaign/m2 2>/dev/null)"
if [ "$m1" = "$m2" ]; then
    ok "MUTATION CONFIRMED: ignoring the ledger hands two campaigns the identical range (the real one does not)"
else
    bad "the mutant produced different ranges ($m1 vs $m2) — the ledger test above is not pinning anything"
fi
export HV_BLOCK_DIR="$tmp/blocks"

echo "== blocks: an invalid campaign name is refused, not sanitised"
if blk take 'bad name; rm -rf /' >/dev/null 2>&1; then
    bad "a campaign name with a space and a semicolon was accepted"
else
    ok "an invalid campaign name is refused (identifiers reject, they do not strip)"
fi

echo ""
if [ "$fail" -ne 0 ]; then echo "$pass passed, $fail failed"; exit 1; fi
echo "$pass passed, 0 failed"
