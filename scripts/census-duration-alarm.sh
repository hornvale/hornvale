#!/usr/bin/env bash
# Report a census run's wall time against the alarm threshold, AT THE MOMENT IT
# IS MEASURED, on the box that measured it.
#
# WHY THIS EXISTS. `cli/tests/suite/census_duration.rs` is the ratchet, and it
# reads `docs/timings.md`. Since the census moved into the merge queue
# (2026-08-27) a run's timing row is committed onto its `census/*` DELIVERY
# BRANCH along with the goldens, so it reaches `main` only when a campaign
# merges that branch — which may be days later, or never. Measured that day:
# four census runs, three of them past the threshold, and `docs/timings.md` on
# main still showed a figure from two days earlier as its most recent census.
#
# So the alarm could not see the runs that would have tripped it, while the
# ledger CLAUDE.md tells you to consult for census cost read as current. This
# script closes that gap the cheap way — the census knows its own duration
# immediately, so it says so immediately, to the operator watching the run.
#
# IT DOES NOT REPLACE THE RATCHET. The Rust test still guards the committed
# history and still fails a gate. This is the live half: a warning at the
# moment a human can still act on it, on a number that has not yet had to
# travel through a branch to be seen.
#
#   bash scripts/census-duration-alarm.sh <wall_seconds> [timings_file]
#
# Exit 0 always: this reports, it does not gate. A census that ran long is not
# a census that failed, and refusing its goldens over a slow box would be a
# worse trade than printing loudly.

set -u

wall="${1:-}"
if [ -z "$wall" ]; then
    echo "usage: census-duration-alarm.sh <wall_seconds> [timings_file]" >&2
    exit 2
fi

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
rust_src="$root/cli/tests/suite/census_duration.rs"

# READ THE THRESHOLD OUT OF THE RUST SOURCE, never restate it. A second copy
# would drift from the ratchet exactly as this project's golden pins drifted
# from their fixture, twice, for the same reason. Same discipline as
# `test-census-guard.sh`, which greps `census_guard_files` out of the hook.
threshold="$(grep -m1 'const CENSUS_ALARM_SECS' "$rust_src" 2>/dev/null \
             | sed 's/.*= *//; s/;.*//' | tr -d ' ')"
if [ -z "$threshold" ]; then
    echo "census-duration-alarm: could not read CENSUS_ALARM_SECS from $rust_src" >&2
    echo "census-duration-alarm: NOT reporting a threshold I could not find — a made-up" >&2
    echo "census-duration-alarm: bound is worse than none, because it asserts a fact." >&2
    exit 0
fi

over="$(awk -v w="$wall" -v t="$threshold" 'BEGIN { print (w > t) ? "yes" : "no" }')"

if [ "$over" = "yes" ]; then
    pct="$(awk -v w="$wall" -v t="$threshold" 'BEGIN { printf "%.1f", (w / t - 1) * 100 }')"
    cat >&2 <<EOF

  ============================================================
  CENSUS PAST THE ALARM THRESHOLD
      wall      ${wall}s
      threshold ${threshold}s   (+${pct}%)
  ============================================================
  This run is slower than the committed alarm bound. The Rust
  ratchet (cli/tests/suite/census_duration.rs) will NOT see it
  until this run's timing row reaches main, which happens when
  the census delivery branch is merged — so treat this as the
  live signal, not that one.

  Per the standing decision, a run past the threshold is FLAGGED
  FOR FLAMEGRAPHING. Before raising the bound again, attribute
  it: more registered metrics is a different finding from the
  same work getting slower, and only the second is a regression.
  Raising a threshold that keeps firing is how a real regression
  gets normalised.
EOF
else
    echo "census-duration-alarm: ${wall}s, under the ${threshold}s threshold." >&2
fi
exit 0
