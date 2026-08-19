#!/usr/bin/env bash
# scripts/decision-block.sh — reserve a disjoint range of decision numbers.
#
# THE DEFECT THIS REMOVES. `docs/decisions/NNNN-slug.md` has a required slot
# whose correct value cannot be known when the author writes it, because it
# depends on other campaigns' merge order — facts that do not exist yet. Every
# failure follows from that one property: collisions (two campaigns take the
# same next-free), gaps (a campaign takes N+3 while N..N+2 are held), and the
# reshuffles, whose cost grows with detection lateness while detection happens
# at somebody ELSE's merge.
#
# A block makes the number knowable at authoring time. It is the only option
# on the table that does — assign-at-merge makes the number IRRELEVANT to the
# author, which is different and buys less.
#
# WHY A BLOCK RATHER THAN THE OTHER FOUR ANSWERS. Lifted, this is concurrent
# writers allocating from a shared monotonic sequence before commit, and that
# shape has a stocked library: assign-at-commit (SVN, auto-increment),
# content-addressing (git hashes — this repo tried it as decision 0026 and
# reversed it in 0043), derive-on-read (`git describe --number`), and
# pre-allocated blocks (CVE CNAs, DOI prefixes, sequence caching). Blocks are
# the only one that touches none of the twelve readers of docs/decisions/ and
# leaves both existing guards intact.
#
# GAPS ARE THE COST AND THEY ARE ALREADY PAID. A block reserved and partly
# unused leaves holes. That was fatal while `no_gaps_in_the_decision_log`
# enforced contiguity; it is not now, because that guard lost its
# discriminating power under parallel campaigns and was replaced by
# `the_decision_log_starts_at_0001`. Uniqueness — the invariant that actually
# protects a citation handle — is untouched and still runs in gate-commit.
#
# WHY THE CANONICAL BOX. An allocator must have exactly one authority, and
# `worktree-take.sh` deliberately runs on ANY machine with no host constraint,
# so a file under $HOME cannot be it. The canonical box already is the single
# authority every campaign talks to, and this reuses the sluice queue's own
# pattern: an append-and-rewrite TSV under its OWN flock, never the shared
# lane claim, so reserving a block never waits behind a running merge.
#
# NOT WIRED INTO `worktree-take`, deliberately. That script works offline and
# is run at a moment when a campaign may never author a decision at all.
# Asking for a block is explicit and happens when a number is actually needed.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

HV_BLOCK_DIR="${HV_BLOCK_DIR:-$HOME/.local/state/hornvale/decision-blocks}"
mkdir -p "$HV_BLOCK_DIR"
LEDGER="$HV_BLOCK_DIR/blocks.tsv"
LOCK="$HV_BLOCK_DIR/blocks.lock"
touch "$LEDGER"
BLOCK_SIZE="${HV_BLOCK_SIZE:-10}"

with_lock() { exec 9>"$LOCK"; flock 9; }

# A campaign name is an identifier, so REJECT rather than strip — the same
# reasoning sluice-queue.sh applies to a branch name. A name with a tab or a
# newline cannot be a real campaign and silently repairing it would
# manufacture a ledger row pointing at something that does not exist.
validate_name() {
    case "$1" in
        ""|*[!a-zA-Z0-9/_-]*) return 1 ;;
        *) return 0 ;;
    esac
}

# The highest number actually present on main. The ledger alone is not enough:
# a campaign that predates blocks still lands numbers the old way, and a block
# allocated below main's real ceiling would collide with one of them.
main_ceiling() {
    env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" \
        ls-tree -r --name-only "${HV_BLOCK_BASE:-origin/main}" -- docs/decisions/ 2>/dev/null \
        | sed -n 's#^docs/decisions/\([0-9][0-9][0-9][0-9]\)-.*#\1#p' \
        | sort -n | tail -1 | sed 's/^0*//'
}

cmd="${1:?usage: decision-block.sh take <campaign> | list | ceiling}"
case "$cmd" in
take)
    name="${2:?usage: decision-block.sh take <campaign>}"
    if ! validate_name "$name"; then
        echo "decision-block: '$name' is not a valid campaign name" >&2
        exit 2
    fi
    with_lock
    ceiling="$(main_ceiling)"; ceiling="${ceiling:-0}"
    # The watermark is the HIGHER of main's real ceiling and the last block's
    # end. Taking the max of both is what keeps blocks disjoint from each
    # other AND from records landing by the pre-block route during transition.
    last_end=0
    while IFS=$'\t' read -r _when _name _start end; do
        [ -n "${end:-}" ] || continue
        [ "$end" -gt "$last_end" ] 2>/dev/null && last_end="$end"
    done < "$LEDGER"
    watermark="$ceiling"
    [ "$last_end" -gt "$watermark" ] && watermark="$last_end"
    start=$((watermark + 1))
    end=$((start + BLOCK_SIZE - 1))
    printf '%s\t%s\t%s\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$name" "$start" "$end" >> "$LEDGER"
    printf 'decision-block: %s reserved %04d-%04d (main ceiling %04d)\n' "$name" "$start" "$end" "$ceiling" >&2
    printf '%04d %04d\n' "$start" "$end"
    ;;
list)
    if [ -s "$LEDGER" ]; then
        while IFS=$'\t' read -r when name start end; do
            printf '  %s  %-28s %04d-%04d\n' "$when" "$name" "$start" "$end"
        done < "$LEDGER"
    else
        echo "  (no blocks reserved)"
    fi
    ;;
ceiling)
    printf '%04d\n' "$(main_ceiling)"
    ;;
*)
    echo "decision-block: unknown command '$cmd' (take|list|ceiling)" >&2
    exit 2
    ;;
esac
