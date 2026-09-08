#!/usr/bin/env bash
# scripts/subfloor-roster.sh — print the nextest filterset selecting exactly
# the committed sub-floor roster.
#
# NOT host-keyed. A test's IDENTITY (whether it belongs in the commit gate)
# does not vary by machine, even though the DURATION that decides membership
# does -- that asymmetry is why the baseline is per-host and this roster is
# not. Concretely: the commit gate runs on the Macs, and by this campaign's
# whole design the Macs never run a full workspace suite, so a host-keyed
# roster would make this script exit 3 on every Mac, forever. The roster is
# authored once, on the canonical gating host, and every machine (including
# ones that have never run a full suite) reads the same committed file.
# Full reasoning: `subfloor_path`'s doc in windows/lab/src/timings.rs.
#
# DIRECTION THIS ENFORCES: it selects `tests present in the roster`. A test
# ABSENT from the roster is not selected — the exclude-unknown rule. That is
# deliberate (a new test enters on the next green stage gate) and it means this
# script can never be read as "the commit gate covers everything new".
#
# Exit 3 when no roster is committed, so a caller can distinguish "nothing to
# run" from "the roster says run nothing" — which are opposite situations
# that would otherwise both produce an empty filterset and a vacuously green
# gate.
set -euo pipefail
root="$(git rev-parse --show-toplevel)"
roster="$root/docs/timings/subfloor-roster.tsv"

if [ ! -f "$roster" ]; then
    echo "subfloor-roster: no roster at $roster" >&2
    echo "  The commit gate's test tier is derived from a roster authored on" >&2
    echo "  the canonical gating host, not this machine -- submit a stage" >&2
    echo "  gate rather than trying to author one locally:" >&2
    echo "    make sluice-stage BRANCH=\$(git branch --show-current) REF=\$(git rev-parse HEAD)" >&2
    exit 3
fi

# `<crate>::<binary>$<test path>` -> `test(=<test path>)`, joined with `|`.
# Test-name matching rather than binary_id: the baseline's binary component is
# libtest-json's target name (`hornvale-book::hornvale_book`) and nextest's
# filterset wants its own binary_id (`hornvale-book`), which are not the same
# string. Names collide across binaries only rarely, and a collision
# over-selects — which costs a little time and never hides a failure.
#
# HV_SUBFLOOR_EXCLUDE narrows this roster for ONE caller: a census delivery
# commit (scripts/hooks/pre-commit under HV_CENSUS_DELIVERY), which cannot
# satisfy the column-count witness by construction — its name carries a count
# that does not exist until the census does, and only a human re-states it
# (The Spillway, decision 0836). Lines whose test path matches are OMITTED,
# never wrapped in `and not (...)`: subfloor-run-chunked.sh splits the
# filterset on a literal ' | ' and is lossless only while it stays flat. A
# pattern that matches nothing is a no-op, and that is the safe direction —
# the witness then runs and reds the delivery loudly. Empty for every other
# caller, so the default roster is unchanged.
awk -F'$' -v X="${HV_SUBFLOOR_EXCLUDE:-}" \
    '!/^#/ && NF>1 && (X=="" || $2 !~ X) {printf "%stest(=%s)", sep, $2; sep=" | "}' "$roster"
