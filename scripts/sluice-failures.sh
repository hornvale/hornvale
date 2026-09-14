#!/usr/bin/env bash
# scripts/sluice-failures.sh — what actually failed in a chamber run, in full.
#
#   sluice-failures.sh <log> [previous-log]
#
# WHY THIS EXISTS. Reading a chamber log means grepping a failure list, and a
# grep is read through a pager, a `head`, or a `tail`. Every truncation reports
# a SUBSET as if it were the whole, and reads identically either way. On
# 2026-09-14 the operator reported "7 failures" for two runs that had 20 and 17:
# the figure came from `tail -8`. The conclusions drawn from it were wrong in
# both directions -- a campaign was told it had four stragglers when it had
# seventeen, and told nothing about the three its change had actually fixed.
#
# scripts/sluice-vet.sh's header records the same defect one surface over ("the
# mouth's verdict was piped through `tail -2` ... correct when a candidate has
# ONE conflict and silently wrong when it has eight"), and the same operator
# then did it twice more in one session while quoting that warning. A rule
# enforced by attention keeps losing to attention.
#
# THE MECHANISM THAT MAKES IT STRUCTURAL: this script cross-checks its own
# extraction against the run's own Summary line. Two independent readings of one
# log -- the list it greps, and the count nextest printed -- must agree, or it
# REFUSES rather than printing a plausible subset. That catches a truncated
# pipe, a mis-written regex, and a log format change, without the reader having
# to suspect any of them.
#
# DIRECTION THIS ENFORCES: the printed list is never shorter than the run
# reported. It does NOT check that the tests are the right tests, that the run
# is the right run, or that a failure is the candidate's fault.
set -u

log="${1:?usage: sluice-failures.sh <log> [previous-log]}"
prev="${2:-}"
[ -r "$log" ] || { echo "sluice-failures: cannot read $log" >&2; exit 2; }

# Every distinct failing test path. A FAIL line's last field is the test path;
# nextest prints each one twice (once live, once in the failure block), so
# distinct names is the meaningful figure.
names_of() { grep -aE '^ *FAIL \[' "$1" | awk '{print $NF}' | sort -u; }

# What the run itself said. A log may hold several nextest invocations (the gate
# phase, docs-tests, the sub-floor tier), so every Summary is reported; the
# LARGEST failed-count is the floor the list must clear.
summaries_of() { grep -a 'Summary \[' "$1" | sed 's/^ *//'; }
max_failed_of() {
    grep -a 'Summary \[' "$1" \
        | sed -n 's/.*: [0-9]* passed[^,]*, \([0-9]*\) failed.*/\1/p' \
        | sort -rn | head -1
}

printf '%s\n' "== the run's own summary lines"
if [ -n "$(summaries_of "$log")" ]; then
    summaries_of "$log" | sed 's/^/  /'
else
    echo "  (none — this log has no nextest summary; the cross-check below cannot run)"
fi

n_names="$(names_of "$log" | grep -c . || true)"
max_failed="$(max_failed_of "$log")"
max_failed="${max_failed:-0}"

printf '\n== failing tests (%s distinct)\n' "$n_names"
names_of "$log" | sed 's/^/  /'

# THE CROSS-CHECK. Under-reporting is the defect; over-reporting (a test failing
# in two invocations of the same log) is expected and fine.
printf '\n== cross-check\n'
printf '  distinct names extracted: %s\n' "$n_names"
printf '  largest "failed" count in a Summary line: %s\n' "$max_failed"
if [ "$n_names" -lt "$max_failed" ]; then
    echo "  REFUSING: the list is SHORTER than the run reported." >&2
    echo "  Something truncated it -- a pipe, a regex, or a changed log format." >&2
    echo "  Do not quote this list; it is a subset wearing the whole list's clothes." >&2
    exit 1
fi
echo "  ok: the list is not shorter than the run reported"

# --- membership against a previous run --------------------------------------
#
# A COUNT IS NOT A MEMBERSHIP, and this pair is the reason the option exists:
# two runs of campaign/the-tidemark failed 20 then 17, and reading those two
# numbers suggests "three fixed". That happens to be true here, and it is not
# what the numbers show -- a run can fix three and break three and hold at 20.
# Only the set difference distinguishes them.
[ -n "$prev" ] || exit 0
[ -r "$prev" ] || { echo "sluice-failures: cannot read $prev" >&2; exit 2; }
p_names="$(names_of "$prev" | grep -c . || true)"
p_max="$(max_failed_of "$prev")"; p_max="${p_max:-0}"
if [ "$p_names" -lt "$p_max" ]; then
    echo "REFUSING: the PREVIOUS log's list is shorter than its own summary ($p_names < $p_max)." >&2
    exit 1
fi
printf '\n== against %s\n' "$(basename "$prev")"
printf '  FIXED (failing before, passing now)\n'
comm -23 <(names_of "$prev") <(names_of "$log") | sed 's/^/    /'
printf '  NEW (passing before, failing now)\n'
comm -13 <(names_of "$prev") <(names_of "$log") | sed 's/^/    /'
printf '  STILL FAILING\n'
comm -12 <(names_of "$prev") <(names_of "$log") | sed 's/^/    /'
