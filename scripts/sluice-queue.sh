#!/usr/bin/env bash
# scripts/sluice-queue.sh — the merge queue's durable state.
#
# Append-and-rewrite TSV under its OWN flock, deliberately NOT the shared lane
# claim: enqueueing must never block behind a running gate, or a caller trying
# to queue work would wait tens of minutes to write one line.
#
# COALESCING IS BY ANCESTRY, NOT BRANCH NAME. `git merge-base --is-ancestor`
# survives rebases and detached refs, which branch-keying does not. This is
# TOOL-lane-supersession's prescribed fix. Two constraints it must respect:
# it matches per-BRANCH, and it must NEVER supersede a RUNNING request — an
# authoring job already inside the chamber would be orphaned mid-write.
#
# HERMETICITY: git exports GIT_DIR and GIT_INDEX_FILE to hooks, and they
# OUTRANK `git -C`/cwd. This script may be invoked from a hook or another
# wrapper that has them set for a different repository, so its own
# merge-base check runs under `env -u GIT_DIR -u GIT_INDEX_FILE` too.
set -euo pipefail

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
mkdir -p "$HV_SLUICE_DIR"
QUEUE="$HV_SLUICE_DIR/queue.tsv"
LOCK="$HV_SLUICE_DIR/queue.lock"
touch "$QUEUE"

with_lock() { exec 8>"$LOCK"; flock 8; }

# A note is caller-supplied free text (a merge conflict summary, a failing
# phase name) that has to fit inside ONE field of a 6-field TSV row. A tab
# shifts which field everything after it prints as; a newline is worse — the
# next rewrite's `while read` sees it as a whole SEPARATE row (when=<the
# second line>, every other field empty) and re-emits that garbage forever,
# since nothing downstream ever removes a row. STRIP rather than reject:
# rejecting only pushes this same check onto every caller (most of them
# forwarding text they did not generate themselves, e.g. captured stderr),
# duplicating it at every call site instead of once here, and a queue whose
# job is durability should never fail closed on cosmetic input.
sanitize_note() {
    local s="$1"
    s="${s//$'\t'/ }"
    s="${s//$'\r'/ }"
    s="${s//$'\n'/ }"
    printf '%s' "$s"
}

# `branch` and `sha` are IDENTIFIERS, not free text, and the right answer for
# an identifier is the opposite of a note's: REJECT, don't strip. A branch
# name containing a control character cannot be a real git ref — git's own
# rule (`git help check-ref-format`, rule 4: no ASCII control chars, space,
# `~^:?*[`) forbids it, verified empirically with `git check-ref-format
# --branch` before writing this — so silently stripping the bad character
# would manufacture a queue row pointing at a branch that cannot exist, which
# hides a caller bug instead of surfacing it. A git object id is similarly
# never anything but lowercase hex, 4-64 characters (sha1's 40 through
# sha256's 64): there is no real sha a tab or newline could ever be hiding
# inside, so the same reasoning applies.
validate_branch() {
    env -u GIT_DIR -u GIT_INDEX_FILE git check-ref-format --branch "$1" >/dev/null 2>&1
}

validate_sha() {
    case "$1" in
        *[!0-9a-f]*|"") return 1 ;;
    esac
    if [ "${#1}" -lt 4 ] || [ "${#1}" -gt 64 ]; then
        return 1
    fi
    return 0
}

# `state` is a CLOSED VOCABULARY, the third shape: neither free text to
# strip nor an external identifier to validate against git, just an enum
# that should not accept a seventh value. Listed here, not derived from the
# TSV, because the set of legal states is a property of this script, not of
# whatever happens to already be on disk.
validate_state() {
    case "$1" in
        queued|running|held|landed|superseded|dropped) return 0 ;;
        *) return 1 ;;
    esac
}

cmd="${1:?usage: sluice-queue.sh add|next|set-state|list ...}"
shift || true

case "$cmd" in
add)
    branch="${1:?usage: add <branch> <sha>}"
    sha="${2:?usage: add <branch> <sha>}"
    if ! validate_branch "$branch"; then
        echo "sluice-queue: add: '$branch' is not a valid branch name (git check-ref-format --branch rejected it)" >&2
        exit 1
    fi
    if ! validate_sha "$sha"; then
        echo "sluice-queue: add: '$sha' is not a valid object id (expected lowercase hex, 4-64 characters)" >&2
        exit 1
    fi
    with_lock
    id="req-$(printf '%.12s' "$sha")-$(date -u +%Y%m%dT%H%M%SZ)"
    # Supersede queued ancestors of THIS sha on THIS branch. `running` is
    # excluded by the state test, not by ordering — see the header.
    #
    # The replacement file is created IN $HV_SLUICE_DIR, not the default
    # $TMPDIR/tmp: `mv` is only atomic within one filesystem, and a bare
    # `mktemp` can land on a tmpfs while $HV_SLUICE_DIR lives under $HOME —
    # different filesystems make `mv` degrade to copy-then-unlink, which is
    # exactly the "truncated partway through a rewrite" failure this queue
    # (the only record a request existed) must not have.
    #
    # The EXIT trap removes it on any early exit (a signal, a failing
    # command between here and the `mv`) — moving the temp file INTO the
    # durable directory closed one hole (cross-filesystem `mv`) and opened
    # another (a leaked temp file landing permanently in the one directory
    # meant to be the clean durable record) unless something sweeps it. A
    # successful `mv` renames the file away, so this is a harmless no-op on
    # the normal path.
    tmp="$(mktemp "$HV_SLUICE_DIR/.queue.tmp.XXXXXX")"
    trap 'rm -f "$tmp"' EXIT
    while IFS=$'\t' read -r when rid rbranch rsha rstate rnote; do
        if [ "$rstate" = "queued" ] && [ "$rbranch" = "$branch" ] \
           && env -u GIT_DIR -u GIT_INDEX_FILE git merge-base --is-ancestor "$rsha" "$sha" 2>/dev/null; then
            rstate="superseded"
            rnote="superseded by $id"
        fi
        printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$when" "$rid" "$rbranch" "$rsha" "$rstate" "$rnote"
    done < "$QUEUE" > "$tmp"
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$id" "$branch" "$sha" "queued" "" >> "$tmp"
    mv "$tmp" "$QUEUE"
    printf '%s\n' "$id"
    ;;
next)
    with_lock
    awk -F'\t' '$5=="queued"{print; exit}' "$QUEUE"
    ;;
set-state)
    id="${1:?usage: set-state <id> <state> [note]}"
    state="${2:?usage: set-state <id> <state> [note]}"
    if ! validate_state "$state"; then
        echo "sluice-queue: set-state: '$state' is not a known state (queued|running|held|landed|superseded|dropped)" >&2
        exit 1
    fi
    note="$(sanitize_note "${3:-}")"
    with_lock
    tmp="$(mktemp "$HV_SLUICE_DIR/.queue.tmp.XXXXXX")"
    trap 'rm -f "$tmp"' EXIT
    while IFS=$'\t' read -r when rid rbranch rsha rstate rnote; do
        if [ "$rid" = "$id" ]; then
            rstate="$state"
            if [ -n "$note" ]; then
                rnote="$note"
            fi
        fi
        printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$when" "$rid" "$rbranch" "$rsha" "$rstate" "$rnote"
    done < "$QUEUE" > "$tmp"
    mv "$tmp" "$QUEUE"
    ;;
list)
    cat "$QUEUE"
    ;;
*)  echo "sluice-queue: unknown command '$cmd'" >&2; exit 2 ;;
esac
