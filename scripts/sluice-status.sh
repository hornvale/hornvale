#!/usr/bin/env bash
# scripts/sluice-status.sh — the queue, showing what an operator can act on.
#
# WHY THIS EXISTS. `make sluice-status` used to `cat` the whole queue.tsv
# through `column -t`. That file is append-only and a month old: 614 rows and
# 131 KB when this was written, of which 579 were terminal history. The five
# rows an operator can actually do something about — queued, running, held —
# scrolled off the top, which is the opposite of what a status command is for.
#
# The note column made it worse. A held row's reason is prose, routinely
# several hundred characters, and `column -t` pads every other column out to
# the width of the longest one. One verbose refusal could make the whole table
# unreadable.
#
# DIRECTION THIS SHOWS: everything still LIVE, then a short tail of what
# recently finished. It hides history, never live work — a queued, running or
# held row is always printed, however many there are. Nothing is lost: ALL=1
# prints the raw file, and `make sluice-log JOB=<id>` reads a job back in full.
set -u

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
QUEUE="$HV_SLUICE_DIR/queue.tsv"
RECENT="${HV_STATUS_RECENT:-8}"
NOTE_WIDTH="${HV_STATUS_NOTE_WIDTH:-72}"
BRANCH_WIDTH="${HV_STATUS_BRANCH_WIDTH:-38}"

[ -r "$QUEUE" ] || { echo "sluice-status: no queue at $QUEUE"; exit 0; }

if [ "${HV_STATUS_ALL:-0}" = 1 ]; then
    column -t -s "$(printf '\t')" < "$QUEUE"
    exit 0
fi

# One row renderer for both sections, so LIVE and RECENT cannot drift apart.
# EVERY VARIABLE-WIDTH COLUMN IS BUDGETED, not just the note. The first cut
# truncated only the note and measured 159 columns against real data -- which
# was luck, not a guarantee: the 50-52 character `census/<sha>-<stamp>-<suffix>`
# branches that would have overflowed all happened to sit in rows the RECENT
# trim removed. A branch that long in the LIVE section, which is deliberately
# never trimmed, would have run the line past 170. Bounding both columns makes
# the width a property of the renderer rather than of today's queue.
render() {
    awk -F'\t' -v w="$NOTE_WIDTH" -v b="$BRANCH_WIDTH" '
        { br = $3
          if (length(br) > b) br = substr(br, 1, b - 1) "…"
          note = $7
          if (length(note) > w) note = substr(note, 1, w - 1) "…"
          printf "  %-20s  %-*s  %-10s  %-7s  %s\n", $1, b, br, $5, $6, note }'
}

live="$(awk -F'\t' '$5=="queued" || $5=="running" || $5=="held"' "$QUEUE")"
n_live="$(printf '%s' "$live" | grep -c . || true)"

echo "LIVE — queued, running, held (all of them; this section is never truncated)"
if [ "$n_live" -gt 0 ]; then
    printf '%s\n' "$live" | render
else
    echo "  (nothing queued, running or held)"
fi

echo
echo "RECENT — last $RECENT to finish"
awk -F'\t' '$5!="queued" && $5!="running" && $5!="held"' "$QUEUE" | tail -n "$RECENT" | render

total="$(grep -c . "$QUEUE" || true)"
echo
echo "  $total rows total. ALL=1 for the raw file; make sluice-log JOB=<id> for one job in full."
