#!/usr/bin/env bash
# Tests for scripts/sluice-watch.sh.
#
# TWO PROPERTIES, AND THE SECOND IS THE ONE THAT DECIDES WHETHER ANYONE READS
# THE OUTPUT. A new row must be vetted; a row that was ALREADY queued when the
# watcher started must not be, because announcing a backlog as if it had just
# arrived is a wall of stale text that trains the reader to skip the channel --
# which is how sluice-vet.sh came to be unread in the first place.
set -u
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

tmp="$(mktemp -d)"
cleanup() { [ -n "${w:-}" ] && kill -9 "$w" 2>/dev/null; rm -rf "$tmp"; }
trap cleanup EXIT

# Real refs, so sluice-vet.sh has something to resolve. origin/main always
# exists; a vet of main against main is degenerate but exercises every section.
sha="$(git -C "$root" rev-parse origin/main 2>/dev/null)"
[ -n "$sha" ] || { echo "test-sluice-watch: SKIP — no origin/main to vet against"; exit 0; }

row() { printf '%s\treq-%s\t%s\t%s\tqueued\t%s\t\n' "$1" "$2" "$3" "$sha" "$4" >> "$tmp/queue.tsv"; }

# A row present BEFORE the watcher starts — must NOT be announced.
row 2026-09-14T00:00:00Z aaaaaaaaaaaa tooling/already-there merge

HV_SLUICE_DIR="$tmp" HV_WATCH_INTERVAL=1 bash "$root/scripts/sluice-watch.sh" > "$tmp/out" 2>&1 &
w=$!
# `disown` so killing it at cleanup does not print a job-control "Killed" line
# to stderr. That line is harmless and it makes a fully green run read as if
# something errored, which is its own small tax on whoever runs this next.
disown "$w" 2>/dev/null || true
# Wait for the seed line rather than sleeping a guessed amount.
n=0; while [ "$n" -lt 60 ] && ! grep -q "watching for new candidates" "$tmp/out" 2>/dev/null; do sleep 0.2; n=$((n+1)); done
if grep -q "seeded with 1 already queued" "$tmp/out" 2>/dev/null; then
    ok "it seeds from what is already queued and says how many"
else
    bad "no seed line: $(head -2 "$tmp/out" 2>/dev/null)"
fi

# Now a NEW row arrives.
row 2026-09-14T00:00:01Z bbbbbbbbbbbb tooling/arrived-later merge
# WAIT FOR THE END OF THE VET, NOT ITS HEADER. The first version waited for
# the QUEUED banner and then checked for MOUTH -- but the banner prints before
# sluice-vet.sh has run its two `git fetch`es, so every section assertion raced
# a vet that had not printed yet and the code looked broken while it was right.
# The dispatch hint is the last line the watcher emits for a candidate.
n=0; while [ "$n" -lt 300 ] && ! grep -q "to run it:" "$tmp/out" 2>/dev/null; do sleep 0.2; n=$((n+1)); done

if grep -q "QUEUED  tooling/arrived-later" "$tmp/out" 2>/dev/null; then
    ok "a newly queued row is announced"
else
    bad "the new row was never announced"
fi
# THE PROPERTY: it ran the real vet, not a summary of its own.
for section in MOUTH DECISIONS "DEFINITION OF DONE" SHAPE; do
    if grep -q "$section" "$tmp/out" 2>/dev/null; then
        ok "the vet's $section section was printed"
    else
        bad "the vet's $section section is missing — this is not running sluice-vet.sh"
    fi
done
if grep -q "sluice-drain.sh watch --kinds=merge" "$tmp/out" 2>/dev/null; then
    ok "it prints the command that would run this candidate's kind"
else
    bad "no dispatch hint, so the reader must reconstruct it"
fi

# THE CONTROL for seeding: the pre-existing row must never be announced.
if grep -q "QUEUED  tooling/already-there" "$tmp/out" 2>/dev/null; then
    bad "a row queued BEFORE the watcher started was announced as new — a backlog would replay as a wall of stale vets"
else
    ok "CONTROL: the pre-existing row is never announced"
fi
# And it must not announce the same row twice.
if [ "$(grep -c "QUEUED  tooling/arrived-later" "$tmp/out" 2>/dev/null || true)" = "1" ]; then
    ok "a row is announced exactly once, not on every poll"
else
    bad "the row was announced more than once — the seen-set is not being updated"
fi

printf '\ntest-sluice-watch: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
