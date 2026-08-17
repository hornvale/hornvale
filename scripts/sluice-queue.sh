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
# ANCESTRY IS A QUESTION THIS BOX CAN FAIL TO ANSWER, and for two days it
# failed silently. `--is-ancestor` is THREE-valued — 0 yes, 1 no, 128 "I
# cannot resolve that object" — and the original `if git merge-base ...
# 2>/dev/null; then` collapsed 128 into 1 while discarding the `fatal:` that
# distinguished them. Nothing in the request path fetched, so the box
# routinely did not have a just-pushed commit and answered "not an ancestor"
# to a question it could not read. Observed live on 2026-08-16:
# campaign/the-rhumb queued three stage requests in one ancestry chain and
# none coalesced, while the author's own machine confirmed exit 0 for each
# pair. Both halves are fixed below — a best-effort fetch before the lock,
# and an explicit three-way read of the exit code that stamps the row when
# the answer is unavailable rather than pretending it was "no".
#
# SUPERSEDABLE STATES ARE NOW LISTED, NOT IMPLIED. The state test used to be
# `= "queued"`, which silently excluded `held` — so a request the chamber
# reddened could never be superseded by the fix that replaced it, and every
# red left a permanent row. `queued` and `held` supersede; `running` never
# does; terminal states are history.
#
# HERMETICITY: git exports GIT_DIR and GIT_INDEX_FILE to hooks, and they
# OUTRANK `git -C`/cwd. This script may be invoked from a hook or another
# wrapper that has them set for a different repository, so its own
# merge-base check runs under `env -u GIT_DIR -u GIT_INDEX_FILE` too.
#
# THE `kind` COLUMN (The Sluice, Task 12) IS WHY THERE IS NO SECOND QUEUE.
# `gate-stage` used to be its own dispatch path — a caller on the Mac, a
# detached job on the canonical box, a shared scratch worktree, a jobs.tsv.
# Absorbing it here makes a stage gate a queue entry like any other: same
# mouth, same chamber, same claim, same FIFO. The ONLY difference is one
# branch at the push step (`sluice-run.sh`), which is why this is a column
# and not a second code path.
#
# THE COLUMN IS SIXTH, NOT SEVENTH, DELIBERATELY. `state` stays field 5, so
# `next`'s `$5=="queued"` and every existing reader of a queue row keep
# working unchanged; `note` — the one free-text field, and the only one whose
# width varies — stays last, where a `column -t` render degrades gracefully.
# A row written before this column existed has six fields and no `kind`; an
# empty kind reads as `merge`, which is what every such row was.
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
# that should not accept an eighth value. Listed here, not derived from the
# TSV, because the set of legal states is a property of this script, not of
# whatever happens to already be on disk.
#
# `reported` is `landed`'s counterpart for a `kind=stage` request: the
# chamber ran every phase and told the author the answer, and there was
# never anything to push. It is a SEPARATE terminal state rather than a
# reuse of `landed` because the two make different claims about `main` —
# reading a stage gate as "landed" would say main moved when it did not,
# on the one file that is the durable record a request existed.
validate_state() {
    case "$1" in
        queued|running|held|landed|reported|superseded|dropped) return 0 ;;
        *) return 1 ;;
    esac
}

# `kind` is the second closed vocabulary, and it is closed for the same
# reason `state` is: it selects a code path in the chamber (whether the run
# pushes), so a third value arriving by typo must fail here rather than be
# silently treated as one of the two. Empty is accepted and normalised to
# `merge` by the caller, for rows written before this column existed.
validate_kind() {
    case "$1" in
        merge|stage) return 0 ;;
        *) return 1 ;;
    esac
}

cmd="${1:?usage: sluice-queue.sh add|next|set-state|list ...}"
shift || true

case "$cmd" in
add)
    branch="${1:?usage: add <branch> <sha> [merge|stage]}"
    sha="${2:?usage: add <branch> <sha> [merge|stage]}"
    kind="${3:-merge}"
    if ! validate_kind "$kind"; then
        echo "sluice-queue: add: '$kind' is not a known kind (merge|stage)" >&2
        exit 1
    fi
    if ! validate_branch "$branch"; then
        echo "sluice-queue: add: '$branch' is not a valid branch name (git check-ref-format --branch rejected it)" >&2
        exit 1
    fi
    if ! validate_sha "$sha"; then
        echo "sluice-queue: add: '$sha' is not a valid object id (expected lowercase hex, 4-64 characters)" >&2
        exit 1
    fi
    # FETCH BEFORE THE LOCK, NOT INSIDE IT. Coalescing asks a question about
    # two commits, and it can only answer if this box HAS them. Nothing else
    # in the request path fetches: `sluice-request.sh` ssh's straight here,
    # so the objects arrive only if something happened to fetch them for an
    # unrelated reason. That made coalescing silently conditional on luck —
    # observed live 2026-08-16, when campaign/the-rhumb queued three stage
    # requests in one chain and NONE of them coalesced, though the author
    # confirmed the ancestry held on their own machine.
    #
    # CONDITIONAL on the object actually being missing, so the common path
    # costs nothing and no test needs an opt-out: if this box can already
    # resolve the sha, there is nothing to go and get. `cat-file -e` on the
    # peeled commit is the cheap form of exactly the question
    # `--is-ancestor` is about to ask.
    #
    # Best-effort, and deliberately so: a request must be enqueued even with
    # no network. A failed fetch degrades coalescing to what it already did,
    # which is the pre-existing behaviour rather than a new failure. It sits
    # outside `with_lock` because the queue's own header is explicit that
    # enqueueing must never block behind slow work — a network round trip
    # inside the lock would make every add wait on it.
    if ! env -u GIT_DIR -u GIT_INDEX_FILE \
            git cat-file -e "$sha^{commit}" >/dev/null 2>&1; then
        env -u GIT_DIR -u GIT_INDEX_FILE \
            git fetch --quiet origin "$branch" >/dev/null 2>&1 || true
    fi
    with_lock
    id="req-$(printf '%.12s' "$sha")-$(date -u +%Y%m%dT%H%M%SZ)"
    # Supersede supersedable ancestors of THIS sha on THIS branch. `running`
    # is excluded by the state test, not by ordering — see the header.
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
    while IFS=$'\t' read -r when rid rbranch rsha rstate rkind rnote; do
        # Coalescing is scoped to the SAME KIND as well as the same branch.
        # A stage gate and a merge on one branch are different requests
        # asking for different things — a queued merge must not be silently
        # dropped because a later stage gate happens to be its descendant,
        # and vice versa. Only the ancestor test is shared.
        [ -n "$rkind" ] || rkind="merge"
        # WHICH STATES ARE SUPERSEDABLE, decided explicitly rather than by
        # omission. `queued` is obvious. `held` is the case this used to miss
        # BY CONSTRUCTION: a held request is one the chamber reddened, so the
        # author fixes it and resubmits — the descendant IS the replacement,
        # and leaving the held row queued-forever means every red accretes a
        # permanent row nobody will ever act on. `running` must never be
        # superseded (an authoring job mid-write would be orphaned), and the
        # terminal states — landed, reported, superseded, dropped — are
        # history and must not be rewritten.
        case "$rstate" in
            queued|held) supersedable=1 ;;
            *)           supersedable=0 ;;
        esac
        if [ "$supersedable" = "1" ] && [ "$rbranch" = "$branch" ] && [ "$rkind" = "$kind" ]; then
            # THE EXIT CODE IS THREE-VALUED AND THE OLD CODE READ IT AS TWO.
            # `git merge-base --is-ancestor` exits 0 for yes, 1 for no, and
            # 128 when it cannot resolve an argument at all. Under `if ...;
            # then` every non-zero is one bucket, so "I do not have that
            # object" was indistinguishable from "not an ancestor" — and the
            # `2>/dev/null` discarded the `fatal:` line that says which.
            # A silent no-op was the result. Capture the code and split it.
            # `|| anc_rc=$?`, never a bare call followed by `anc_rc=$?`:
            # this script runs under `set -e`, so a bare `--is-ancestor`
            # returning 1 — the ordinary "no" answer — would kill the script
            # before the assignment ran. Caught by the existing post-rebase
            # test, which is the one case that exercises a genuine 1.
            anc_rc=0
            env -u GIT_DIR -u GIT_INDEX_FILE \
                git merge-base --is-ancestor "$rsha" "$sha" >/dev/null 2>&1 || anc_rc=$?
            case "$anc_rc" in
                0)
                    rstate="superseded"
                    rnote="superseded by $id"
                    ;;
                1)
                    : # genuinely not an ancestor — a real answer, leave it
                    ;;
                *)
                    # UNANSWERABLE. Do not fail the add — the queue's job is
                    # durability, and refusing here would lose a request over
                    # a missing object. But do not stay silent either, which
                    # is the whole defect: stamp the row so `sluice-status`
                    # shows an operator that coalescing could not decide, and
                    # they can check ancestry by hand.
                    rnote="coalescing indeterminate vs $id (git merge-base rc=$anc_rc; object not resolvable here)"
                    ;;
            esac
        fi
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$when" "$rid" "$rbranch" "$rsha" "$rstate" "$rkind" "$rnote"
    done < "$QUEUE" > "$tmp"
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$id" "$branch" "$sha" "queued" "$kind" "" >> "$tmp"
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
        echo "sluice-queue: set-state: '$state' is not a known state (queued|running|held|landed|reported|superseded|dropped)" >&2
        exit 1
    fi
    note="$(sanitize_note "${3:-}")"
    with_lock
    tmp="$(mktemp "$HV_SLUICE_DIR/.queue.tmp.XXXXXX")"
    trap 'rm -f "$tmp"' EXIT
    while IFS=$'\t' read -r when rid rbranch rsha rstate rkind rnote; do
        [ -n "$rkind" ] || rkind="merge"
        if [ "$rid" = "$id" ]; then
            rstate="$state"
            if [ -n "$note" ]; then
                rnote="$note"
            fi
        fi
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$when" "$rid" "$rbranch" "$rsha" "$rstate" "$rkind" "$rnote"
    done < "$QUEUE" > "$tmp"
    mv "$tmp" "$QUEUE"
    ;;
list)
    cat "$QUEUE"
    ;;
*)  echo "sluice-queue: unknown command '$cmd'" >&2; exit 2 ;;
esac
