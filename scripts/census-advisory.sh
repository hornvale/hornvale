#!/usr/bin/env bash
# scripts/census-advisory.sh — advisory only, never blocking (decision 0081).
#
# A gate is not a measurement, and a developer waiting twelve minutes to START
# a four-minute gate is a worse experience than the contention. So the gates
# do not take the census claim; they print the context and get out of the way.
#
# Never fails the build: every path exits 0. If this script is the reason a
# gate went red, it is broken.
set -uo pipefail

status="$(cargo run --quiet --release -p hornvale -- lab claim-status 2>/dev/null || true)"
case "$status" in
    "census running"*)
        echo "note: $status" >&2
        echo "note: your timings will be contended — see docs/timings.md's cpu_ratio column." >&2
        ;;
esac
exit 0
