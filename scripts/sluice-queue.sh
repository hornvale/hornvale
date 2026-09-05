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
# `census` is a THIRD kind, and unlike merge/stage it is not run by
# sluice-run.sh — scripts/sluice-census.sh runs it, because census-run.sh takes
# the shared claim itself and deletes the claim file on exit, so it cannot be
# nested inside a job already holding it (sluice-run.sh refuses it as a phase
# for that reason). It is in the queue for ORDERING, not for dispatch: a census
# competes for the same claim every chamber job takes, and before it had a kind
# it was invisible to the FIFO that claim exists to serve — one unqueued census
# left a merge waiting ~19 minutes on 2026-08-24.
validate_kind() {
    case "$1" in
        merge|stage|census) return 0 ;;
        *) return 1 ;;
    esac
}

cmd="${1:?usage: sluice-queue.sh add|next|set-state|claim|list ...}"
shift || true

# THE PORTED VERBS GO TO tools/sluice. `add` stays here for now: it sources
# sluice-headline.sh, resolves three-valued ancestry and coalesces, all of
# which shell out to git, and moving it is Task 6 rather than a side effect
# of this one. The binary is built on demand and the path is resolved from
# this script's own location so a caller's cwd cannot change which one runs.
#
# A CACHE FALLBACK EXISTS FOR ONE CASE ONLY: a copy of this ONE file with no
# `tools/sluice` sibling beside it. Found live by `scripts/test-sluice.sh`'s
# own T7/T8 (2026-09-05) — its chamber tests copy just this script into a
# throwaway scratch repo to exercise `sluice-run.sh`'s claim-refusal logic,
# a pattern that worked when this file was pure bash with no external
# dependency. Without the fallback, the copy's `cargo build` fails (no
# manifest to build) and `claim` never runs at all: rc=101, indistinguishable
# to a caller from the binary itself panicking. The primary (sibling) path
# is tried FIRST and is what every real checkout uses, so a live checkout's
# own edits to tools/sluice are never shadowed by a stale cache; the cache
# is refreshed from the sibling on every call that has one, so by the time a
# sibling-less copy needs it, it is never staler than this same run's own
# most recent real invocation.
case "$cmd" in
    claim|set-state|list)
        sluice_bin="$(dirname "$0")/../tools/sluice/target/release/sluice"
        sluice_manifest="$(dirname "$0")/../tools/sluice/Cargo.toml"
        cache_bin="$HOME/.cache/hornvale/sluice/sluice"
        if [ -f "$sluice_manifest" ]; then
            if [ ! -x "$sluice_bin" ]; then
                cargo build --quiet --release --manifest-path "$sluice_manifest" >&2
            fi
            mkdir -p "$(dirname "$cache_bin")"
            cp -f "$sluice_bin" "$cache_bin" 2>/dev/null || true
        elif [ -x "$cache_bin" ]; then
            sluice_bin="$cache_bin"
        fi
        exec "$sluice_bin" "$cmd" "$@"
        ;;
esac

case "$cmd" in
add)
    branch="${1:?usage: add <branch> <sha> [merge|stage]}"
    sha="${2:?usage: add <branch> <sha> [merge|stage]}"
    kind="${3:-merge}"
    if ! validate_kind "$kind"; then
        echo "sluice-queue: add: '$kind' is not a known kind (merge|stage|census)" >&2
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
    # THE HEADLINE CHECK LIVES HERE TOO, AND THIS IS THE ONE THAT ENFORCES.
    # `sluice-request.sh` checks the same thing, but it runs on the
    # SUBMITTER's machine from the SUBMITTER's checkout — so a campaign whose
    # scripts/ predates the trailer rule silently gets the old subject-based
    # check and never sees this one. That is not hypothetical: it happened on
    # the rule's first outside submission, from a checkout two commits stale
    # on that one file, and the submitter reported to Nathan that the new
    # mechanism had caught something when it had not. A client-side check that
    # falls through SILENTLY is worse than none, because it manufactures false
    # confidence in its own coverage.
    #
    # `add` runs on the canonical box, over ssh, from the box's own checkout —
    # the one place every submission passes through regardless of what the
    # caller is running. So the refusal belongs here and the caller-side one
    # is demoted to what it actually is: a fast local pre-check.
    #
    # THREE-VALUED, for the same reason coalescing is. Absent-and-answerable
    # refuses; UNANSWERABLE (the box cannot resolve the objects) must NOT,
    # because the queue's first duty is durability and refusing a real request
    # over a missing object would lose it. That case accepts and stamps the
    # row so an operator sees it, exactly as an indeterminate coalesce does.
    #
    # HV_SLUICE_SKIP_HEADLINE is for `scripts/test-sluice.sh`, which adds many
    # merge rows in scratch repos that carry no trailers. It is safe because
    # nothing in the production path sets it — `sluice-request.sh`'s remote
    # command is a fixed string that does not mention it, and a test in that
    # suite asserts so, which is what keeps this knob from quietly becoming
    # the way the check gets turned off.
    headline_note=""
    if [ "$kind" = "merge" ] && [ "${HV_SLUICE_SKIP_HEADLINE:-}" != "1" ]; then
        # shellcheck source=scripts/sluice-headline.sh
        . "$(dirname "$0")/sluice-headline.sh"
        hl_base="${HV_SLUICE_BASE:-origin/main}"
        if ! env -u GIT_DIR -u GIT_INDEX_FILE \
                git rev-parse --verify --quiet "$hl_base" >/dev/null 2>&1 \
           || ! env -u GIT_DIR -u GIT_INDEX_FILE \
                git cat-file -e "$sha^{commit}" >/dev/null 2>&1; then
            headline_note="headline indeterminate: $hl_base or ${sha:0:12} unresolvable here"
            echo "sluice-queue: add: headline UNCHECKED — $headline_note" >&2
        else
            hl="$(sluice_headline_of "$PWD" "$hl_base" "$sha")"
            if sluice_headline_is_junk "$hl"; then
                echo "sluice-queue: add: REFUSED — no usable Sluice-Headline: trailer in $hl_base..${sha:0:12}." >&2
                echo "  The merge commit's subject is permanent and human-read, and" >&2
                echo "  tools/census/history.sh reads it as the census epoch label when" >&2
                echo "  the merge moves the census. It must be authored, not inferred." >&2
                echo "" >&2
                echo "  Add to the body of any commit in the range — not necessarily the" >&2
                echo "  last, and a later commit will not displace it:" >&2
                echo "" >&2
                echo "      Sluice-Headline: <what landed, in one line>" >&2
                echo "" >&2
                echo "  Keep it ADJACENT to your other trailers with no blank line" >&2
                echo "  between: git's parser reads only the message's LAST block, so a" >&2
                echo "  blank line above a Claude-Session: line strands it." >&2
                echo "" >&2
                echo "  Write only the text; the chamber composes merge($(sluice_short_name "$branch")): <text>." >&2
                exit 2
            fi
            echo "sluice-queue: add: headline OK (trailer rule) — merge($(sluice_short_name "$branch")): $hl" >&2
        fi
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
    # The note carries the headline verdict ONLY when it was indeterminate —
    # a normal row's note stays empty, so `sluice-status` reads clean and the
    # unchecked ones stand out rather than being buried in uniform text.
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$id" "$branch" "$sha" "queued" "$kind" \
        "$(sanitize_note "$headline_note")" >> "$tmp"
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
    matched=0
    while IFS=$'\t' read -r when rid rbranch rsha rstate rkind rnote; do
        [ -n "$rkind" ] || rkind="merge"
        if [ "$rid" = "$id" ]; then
            matched=1
            rstate="$state"
            if [ -n "$note" ]; then
                rnote="$note"
            fi
        fi
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$when" "$rid" "$rbranch" "$rsha" "$rstate" "$rkind" "$rnote"
    done < "$QUEUE" > "$tmp"
    # AN ID THAT MATCHES NO ROW IS A REFUSAL, NOT A SILENT SUCCESS. Until
    # 2026-08-19 this loop simply rewrote every row unchanged and exited 0, so a
    # mistyped id reported success for an operation it had not performed. That
    # is how a GHOST is made: the operator believed a finished run's row had
    # been set terminal, the row stayed `running`, and coalescing then refused
    # to supersede it (running rows never are, deliberately) — so the campaign's
    # resubmission queued BEHIND a job that had already exited. Observed live:
    # `req-1fa24f761611-20260819T173353Z` typed for `…163353Z`, one digit, and
    # the queue said nothing.
    #
    # The rewrite is discarded rather than committed on the failing path: with
    # no match the temp file is byte-identical to the queue anyway, so refusing
    # before `mv` costs nothing and cannot half-apply.
    if [ "$matched" = "0" ]; then
        rm -f "$tmp"
        echo "sluice-queue: set-state: no row with id '$id' — NOTHING WAS CHANGED." >&2
        echo "sluice-queue: ids are exact; list them with 'sluice-queue.sh list'." >&2
        exit 1
    fi
    mv "$tmp" "$QUEUE"
    ;;
claim)
    # ATOMIC SELECT-AND-MARK. `next` reads the first queued row and returns;
    # marking it `running` was a SEPARATE process, and the lock is released at
    # process exit, so the row sat `queued` across the caller's whole mouth
    # check. Two drains, or a drain and a direct `sluice-run.sh`, both saw an
    # unclaimed row and both launched it. Observed live 2026-09-04: one merge
    # ran twice, pids 1240741 and 1253175, two ~800 KB logs for
    # 48aa9373b6f2, both rc=0.
    #
    # The row state IS the interlock, so selecting and marking must happen
    # under ONE lock acquisition. That is the whole of this subcommand, and it
    # is why `next` is left in place but is no longer what a dispatcher should
    # call: `next` is a read, `claim` is a transaction.
    #
    # WITHOUT --sha it claims the first queued row (a dispatcher draining the
    # queue). WITH --sha it claims the queued row for that ref (an executor
    # launched directly, which knows its ref and not its id). The two exit
    # codes below exist for that second caller: it must be able to tell "this
    # is mine now" from "somebody else already has it".
    claim_sha=""
    if [ "${1:-}" = "--sha" ]; then
        claim_sha="${2:?usage: claim --sha <sha> [note]}"
        shift 2
    fi
    note="$(sanitize_note "${1:-}")"
    with_lock
    tmp="$(mktemp "$HV_SLUICE_DIR/.queue.tmp.XXXXXX")"
    trap 'rm -f "$tmp"' EXIT
    claimed=""
    seen_sha=0
    while IFS=$'\t' read -r when rid rbranch rsha rstate rkind rnote; do
        [ -n "$rkind" ] || rkind="merge"
        if [ -n "$claim_sha" ] && [ "$rsha" = "$claim_sha" ]; then
            seen_sha=1
        fi
        if [ -z "$claimed" ] && [ "$rstate" = "queued" ] \
           && { [ -z "$claim_sha" ] || [ "$rsha" = "$claim_sha" ]; }; then
            rstate="running"
            [ -n "$note" ] && rnote="$note"
            claimed="$(printf '%s\t%s\t%s\t%s\t%s\t%s\t%s' \
                "$when" "$rid" "$rbranch" "$rsha" "$rstate" "$rkind" "$rnote")"
        fi
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$when" "$rid" "$rbranch" "$rsha" "$rstate" "$rkind" "$rnote"
    done < "$QUEUE" > "$tmp"
    if [ -z "$claimed" ]; then
        rm -f "$tmp"
        if [ -n "$claim_sha" ] && [ "$seen_sha" = "1" ]; then
            echo "sluice-queue: claim: a row for ${claim_sha:0:12} exists but is NOT queued — somebody else has it. NOTHING WAS CHANGED." >&2
            exit 4
        fi
        if [ -n "$claim_sha" ]; then
            echo "sluice-queue: claim: no row at all for ${claim_sha:0:12}." >&2
            exit 5
        fi
        # No --sha and nothing queued is the ordinary drained queue, not an
        # error: `next` printed nothing and exited 0, and callers test for
        # empty output. Keep that contract.
        exit 0
    fi
    mv "$tmp" "$QUEUE"
    printf '%s\n' "$claimed"
    ;;
list)
    cat "$QUEUE"
    ;;
*)  echo "sluice-queue: unknown command '$cmd'" >&2; exit 2 ;;
esac
