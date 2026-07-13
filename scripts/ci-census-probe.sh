#!/usr/bin/env bash
# scripts/ci-census-probe.sh — cross-platform census spot-check for CI.
#
# The full 1000-world censuses are author-time statistical evidence;
# re-deriving them live on every push cost CI 15-25 min. But cross-platform
# drift is SYSTEMATIC — a platform op that differs shows on (almost) every
# seed, not one rare one — so regenerating the first N seeds of each census
# and confirming they match the committed rows.csv head proves cross-platform
# byte-identity just as well, in seconds. The lab runner reassembles rows by
# seed offset, so a count=N study yields the same rows as the first N seeds of
# the full study (validated). The full-census re-verification runs once per
# campaign on the AWS box (scripts/aws-gate/regen-git.sh), just before the
# merge to main — never per-push, never on the local Mac.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
N="${CI_PROBE_SEEDS:-8}"
STUDIES=(census-lands-drift census-of-the-meeting branches-family)

cleanup() { for s in "${STUDIES[@]}"; do rm -rf "book/src/laboratory/generated/${s}-ciprobe"; done; }
trap cleanup EXIT

fail=0
for s in "${STUDIES[@]}"; do
    probe="$(mktemp --suffix=.study.json 2>/dev/null || mktemp)"
    python3 - "$s" "$N" "$probe" <<'PY'
import json, sys
study, n, out = sys.argv[1], int(sys.argv[2]), sys.argv[3]
d = json.load(open(f"studies/{study}.study.json"))
d["seeds"]["count"] = min(n, d["seeds"]["count"])
d["name"] = d["name"] + "-ciprobe"
json.dump(d, open(out, "w"))
PY
    cargo run --release -q -p hornvale -- lab run "$probe" >/dev/null
    pr="book/src/laboratory/generated/${s}-ciprobe/rows.csv"
    cm="book/src/laboratory/generated/${s}/rows.csv"
    # Compare by SEED KEY, not `head -N`: rows.csv is ordered pin_set-major,
    # seed-minor, so the committed rows with seed<N (across all pin_sets) are
    # exactly the count=N probe's rows, in the same order. A `head -N` compare
    # would misalign the moment a study has more than one pin_set.
    if awk -F, -v n="$N" 'NR==1 || $1+0<n' "$cm" | diff -q - "$pr" >/dev/null; then
        echo "  ok: ${s} — first ${N} seeds match committed"
    else
        echo "  DRIFT: ${s} — regenerated first ${N} seeds differ from committed:" >&2
        awk -F, -v n="$N" 'NR==1 || $1+0<n' "$cm" | diff - "$pr" | head -8 >&2
        fail=1
    fi
    rm -f "$probe"
done
[ "$fail" = 0 ] || { echo "census-probe: cross-platform DRIFT — regen on the AWS box (scripts/aws-gate/regen-git.sh) and investigate" >&2; exit 1; }
echo "census-probe: all ${#STUDIES[@]} studies match on the first ${N} seeds"
