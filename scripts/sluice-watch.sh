#!/usr/bin/env bash
# scripts/sluice-watch.sh — notice a newly queued candidate and vet it.
#
# WHY THIS EXISTS. scripts/sluice-vet.sh does the whole pre-dispatch vet and
# NOTHING RUNS IT. An operator ran this queue for a full session -- six vets,
# seven merges -- without knowing it was there, hand-rolling a thinner vet each
# time and, on 2026-09-13, piping the mouth through `tail -3`: the exact defect
# that script's header says it exists to prevent. A tool nobody invokes is
# indistinguishable from a tool nobody wrote.
#
# WHAT IT DELIBERATELY DOES NOT DO: dispatch. sluice-vet.sh reports and does not
# gate, on purpose and for a stated reason, and this inherits that. The operator
# reading the vet and then typing the drain IS the gate -- a wrongly dispatched
# merge costs a queue slot rather than correctness (the chamber never pushes on
# red), so the friction is not protecting against damage, it is the only thing
# that makes the vet get read. Automating the dispatch would remove the gate and
# leave this script as unrun as sluice-vet.sh was.
#
# DIRECTION THIS ENFORCES: every row that becomes `queued` while this is running
# gets its vet printed once. It says nothing about rows that were already queued
# when it started (deliberately -- see the seeding below), nothing about whether
# the vet is good, and nothing about whether anyone read it.
set -u

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root" || exit 1
INTERVAL="${HV_WATCH_INTERVAL:-60}"

# The queued set, as "id<TAB>branch<TAB>sha".
#
# READS THE FILE, NOT `sluice-queue.sh list`. That subcommand is `cat "$QUEUE"`
# -- but the script refuses before reaching it unless tools/sluice's release
# binary is built, so routing a pure read through it would give this watcher a
# BUILD dependency to do a `cat`. Its read-only siblings (sluice-drainers.sh,
# sluice-status.sh) read the file directly for the same reason. Anything that
# MUTATES the queue must still go through sluice-queue.sh, which is where the
# locking lives.
HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
queued() {
    awk -F'\t' '$5=="queued"{print $2"\t"$3"\t"$4}' \
        "$HV_SLUICE_DIR/queue.tsv" 2>/dev/null
}

# SEEDED, NOT REPLAYED. Starting with an empty `seen` would vet every row that
# is already queued and announce them as if they had just arrived -- which on a
# queue with a backlog is a wall of stale output that trains the reader to skip
# it. What is already there is the operator's existing business.
seen="$(queued)"
n_seen="$(printf '%s' "$seen" | grep -c . || true)"
echo "sluice-watch: watching for new candidates every ${INTERVAL}s (seeded with $n_seen already queued)"

while :; do
    cur="$(queued)"
    # Rows in cur that are not in seen.
    new="$(comm -13 <(printf '%s\n' "$seen" | sort) <(printf '%s\n' "$cur" | sort))"
    printf '%s\n' "$new" | while IFS=$'\t' read -r id branch sha; do
        [ -n "${branch:-}" ] || continue
        echo
        echo "================================================================"
        echo "QUEUED  $branch"
        echo "        $id"
        echo "================================================================"
        bash scripts/sluice-vet.sh "$branch" "$sha" 2>&1
        echo
        echo "  to run it:  bash scripts/sluice-drain.sh watch --kinds=$(awk -F'\t' -v i="$id" '$2==i{print $6; exit}' "$HV_SLUICE_DIR/queue.tsv" 2>/dev/null) --max-jobs=1"
    done
    seen="$cur"
    sleep "$INTERVAL"
done
