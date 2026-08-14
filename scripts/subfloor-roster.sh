#!/usr/bin/env bash
# scripts/subfloor-roster.sh — print the nextest filterset selecting exactly
# the sub-floor roster for this host.
#
# DIRECTION THIS ENFORCES: it selects `tests present in the roster`. A test
# ABSENT from the roster is not selected — the exclude-unknown rule. That is
# deliberate (a new test enters on the next green stage gate) and it means this
# script can never be read as "the commit gate covers everything new".
#
# Exit 3 when no roster exists for this host, so a caller can distinguish
# "nothing to run" from "the roster says run nothing" — which are opposite
# situations that would otherwise both produce an empty filterset and a
# vacuously green gate.
set -euo pipefail
root="$(git rev-parse --show-toplevel)"
host="$(hostname -s 2>/dev/null || hostname)"
roster="$root/docs/timings/subfloor-$host.tsv"

if [ ! -f "$roster" ]; then
    echo "subfloor-roster: no roster at $roster" >&2
    echo "  The commit gate's test tier is derived from a roster written by a" >&2
    echo "  green \`make gate-stage\` on this host. Run one, or dispatch:" >&2
    echo "    make gate-stage REF=\$(git rev-parse HEAD)" >&2
    exit 3
fi

# `<crate>::<binary>$<test path>` -> `test(=<test path>)`, joined with `|`.
# Test-name matching rather than binary_id: the baseline's binary component is
# libtest-json's target name (`hornvale-book::hornvale_book`) and nextest's
# filterset wants its own binary_id (`hornvale-book`), which are not the same
# string. Names collide across binaries only rarely, and a collision
# over-selects — which costs a little time and never hides a failure.
awk -F'$' '!/^#/ && NF>1 {printf "%stest(=%s)", sep, $2; sep=" | "}' "$roster"
