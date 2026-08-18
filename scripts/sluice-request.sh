#!/usr/bin/env bash
# scripts/sluice-request.sh — the caller's side. Runs on ANY machine.
# Validates, ssh's, prints a request id, RETURNS. It never waits.
#
# Same shape as scripts/lane-dispatch.sh, including its two hard-won guards:
# a REF must be a full 40-char SHA (it feeds checkout on the far end), and the
# remote preflight stays OUTSIDE the backgrounded segment so a dispatch that
# did not start cannot report success. This script has no backgrounded
# segment of its own (the remote side, `sluice-queue.sh add`, is a fast,
# lock-protected TSV write, not a long-running job), but the same "a request
# that did not land must not print success" discipline applies: the ssh call
# below is checked explicitly and nothing is echoed as queued unless it
# actually returned an id.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
branch="${1:?usage: sluice-request.sh <branch> <full-sha> [merge|stage]}"
ref="${2:?usage: sluice-request.sh <branch> <full-sha> [merge|stage]}"
kind="${3:-merge}"
case "$kind" in
    merge|stage) ;;
    *) echo "sluice-request: unknown kind '$kind' (merge|stage)" >&2; exit 2 ;;
esac
host="$(cat "$repo_root/scripts/census-canonical-host.txt")"
remote_dir="${HV_SLUICE_REMOTE_DIR:-~/Projects/hornvale}"
# The base a merge is against, and the base the headline trailer is searched
# over. Same variable and same default `sluice-run.sh` uses, so the refusal
# below asks its question over exactly the range the chamber will.
HV_SLUICE_BASE="${HV_SLUICE_BASE:-origin/main}"
# shellcheck source=scripts/sluice-headline.sh
. "$repo_root/scripts/sluice-headline.sh"

# THE QUOTING HAZARD lane-dispatch.sh's own HV_LANE_REMOTE_DIR guard exists
# to catch: `HV_SLUICE_REMOTE_DIR=~/foo` typed unquoted at a shell prompt is
# expanded by the CALLER's OWN shell before this script ever reads it — the
# '~' never reaches here, only its already-resolved LOCAL answer does. Same
# topology fix-round-1 named for lane-dispatch.sh's own guard: catching "the
# override, once fully resolved, sits under the caller's own $HOME" closes
# the common, silent case (a redirect meant for the canonical box silently
# becoming a path on the CALLER's machine that the canonical box has never
# heard of) at the cost of one acceptable false positive (a caller whose home
# genuinely is a meaningful remote path).
if [ -n "${HV_SLUICE_REMOTE_DIR+x}" ] && [ -n "${HOME:-}" ]; then
    case "$remote_dir" in
        "$HOME" | "$HOME"/*)
            cat >&2 <<EOF
sluice-request: HV_SLUICE_REMOTE_DIR resolved to a path under YOUR OWN \$HOME
($HOME) before this script ever ran:
  HV_SLUICE_REMOTE_DIR=$remote_dir
That is what an unquoted '~' in a shell assignment does — it expands against
the CALLER's home, not the canonical box's, so this almost certainly is not
the path you meant on '$host'.

Quote it, so the '~' survives to reach the remote host and expands THERE,
against its own \$HOME instead:
  HV_SLUICE_REMOTE_DIR='~/Projects/hornvale-scratch' make sluice BRANCH=$branch REF=$ref

or pass an absolute path that is meaningful on '$host' directly.
EOF
            exit 2
            ;;
    esac
fi

# A FULL 40-CHAR SHA, hex-pure — not merely hex-STARTING. The brief's
# original `case "$ref" in [0-9a-f]*)` is a GLOB, not a regex: it matches
# anything that starts with one hex digit, so a 40-character string with a
# non-hex 2nd-through-40th character (`0` followed by 39 garbage bytes, say)
# passed cleanly. This exact bug was already found and fixed in
# sluice-mouth.sh during Task 3 of this same plan
# (`*[!0-9a-f]*|""` — reject anything CONTAINING a non-hex character, or
# empty — then check length separately); this reuses that corrected pattern
# rather than the older lane-dispatch.sh shape this script was modelled on,
# since a campaign's own fixed code is the precedent once it has one, not
# the file that predates the fix.
case "$ref" in
    *[!0-9a-f]*|"") echo "sluice-request: REF must be a full 40-char SHA (hex only); got '$ref'" >&2; exit 2 ;;
esac
[ "${#ref}" -eq 40 ] || { echo "sluice-request: REF must be a full 40-char SHA; got '$ref'" >&2; exit 2; }

if [ -z "$(git -C "$repo_root" branch -r --contains "$ref" 2>/dev/null)" ]; then
    echo "sluice-request: $ref is not on any remote branch — push first." >&2
    exit 2
fi

# THE HEADLINE IS REFUSED, NOT DEFAULTED — enforced here, by the script, not
# left as advice in a skill. It becomes the merge commit's subject, which is
# permanent and human-read, and which tools/census/history.sh reads as the
# census epoch label (`git log --follow --first-parent main -- <census CSV>`,
# path-scoped, so only a merge that MOVES the census mints one). Only the
# campaign that authored $ref knows whether a subject is fit to be that. An
# operator triaging the queue later must never be the one inventing or
# silently accepting a placeholder — which is why this refuses rather than
# defaulting, and why nothing in this path threads a caller-supplied override.
#
# IT ASKS FOR A TRAILER NOW, NOT THE TIP COMMIT'S SUBJECT, and the reason is
# measured rather than aesthetic: inferring the subject from whichever commit
# happened to be last failed on FOUR of the first four real merges — doubled
# once, branch-path-leaked twice, redundantly prefixed once — plus a fifth
# failure that leaves no trace in what landed, a headline commit DISPLACED by
# an ordinary commit added after it. `scripts/sluice-headline.sh` carries the
# full account and the shared implementation.
#
# The junk check below is unchanged in spirit and much narrower in load. It
# used to be the only thing standing between a tidy-up commit and a permanent
# label, which it could not do — it knows placeholder WORDS, and an ordinary
# subject on an ordinary commit is not a placeholder word. Now it is a
# backstop on a string someone deliberately wrote, which is the job it can
# actually perform.
#
# A `stage` REQUEST IS EXEMPT, and the exemption is the same reasoning read
# forwards. The refusal exists because the subject becomes a PERMANENT
# artifact label on `main`'s first-parent line. A stage gate never pushes —
# its merge commit is discarded with the chamber's worktree — so no subject
# it carries can ever become that label, and refusing a mid-campaign `wip`
# commit a plan-stage boundary legitimately sits on would block the one
# thing a stage gate is for. The check returns in full the moment the same
# branch asks to merge.
if [ "$kind" = "merge" ]; then
    # Refresh the base before asking: the trailer search excludes commits
    # already on it, so a base that has fallen behind admits main's commits
    # into the range — and once this convention is in use those carry trailers
    # of their own, from other campaigns. "Newest wins" mitigates it but does
    # not close it.
    #
    # GIT_TERMINAL_PROMPT=0 IS LOAD-BEARING, NOT TIDINESS. Against an https
    # remote with no cached credential, `git fetch` prompts — and a prompt in
    # a non-interactive path does not fail, it HANGS, forever. Adding this
    # fetch without it took scripts/test-sluice.sh from 17.9 s to past 20
    # minutes with no output, which is how it was found. Fail fast instead.
    #
    # HV_SLUICE_SKIP_FETCH exists for the same reason HV_SLUICE_SKIP_BOARD
    # does, one line of precedent below: this suite drives the real script
    # dozens of times, and a test run must not depend on — or wait for — the
    # network. Skipping only degrades the check to the pre-fetch behaviour.
    if [ "${HV_SLUICE_SKIP_FETCH:-}" != "1" ]; then
        GIT_TERMINAL_PROMPT=0 git -C "$repo_root" fetch --quiet origin main \
            >/dev/null 2>&1 || true
    fi
    headline="$(sluice_headline_of "$repo_root" "$HV_SLUICE_BASE" "$ref")"
    if sluice_headline_is_junk "$headline"; then
        short="$(sluice_short_name "$branch")"
        echo "sluice-request: refusing — no usable Sluice-Headline: trailer in $HV_SLUICE_BASE..${ref:0:12}." >&2
        echo "" >&2
        echo "  A merge commit's subject is permanent and human-read, and" >&2
        echo "  tools/census/history.sh reads it as the census epoch label when" >&2
        echo "  the merge moves the census. It must be AUTHORED, not inferred:" >&2
        echo "  inferring it from whichever commit happened to be last failed on" >&2
        echo "  four of the first four real merges." >&2
        echo "" >&2
        echo "  Add this trailer to the body of any commit in the range — it" >&2
        echo "  does not have to be the last one, and a later commit will not" >&2
        echo "  displace it:" >&2
        echo "" >&2
        echo "      Sluice-Headline: <what landed, in one line>" >&2
        echo "" >&2
        echo "  The chamber composes the whole subject itself, so write only the" >&2
        echo "  text — no merge(...) prefix of your own:" >&2
        echo "" >&2
        echo "      merge($short): <what landed, in one line>" >&2
        echo "" >&2
        echo "  IF YOU ALREADY ADDED ONE, CHECK ITS PLACEMENT. This reads git's" >&2
        echo "  trailers, which are only ever the message's LAST block, so a" >&2
        echo "  blank line above another trailer strands it:" >&2
        echo "" >&2
        echo "      Sluice-Headline: what landed      <- ignored, wrong block" >&2
        echo "                                        <- this blank line" >&2
        echo "      Claude-Session: https://…" >&2
        echo "" >&2
        echo "  Keep it adjacent to the other trailers, no blank line between." >&2
        exit 2
    fi
fi

# THE CAIRN'S HOLD-OFF READ, INHERITED FROM `scripts/preflight-merge.sh`.
# That script is deleted (Task 12); this is the seam of it that had no
# successor. The mouth subsumes preflight's ancestry check and the chamber's
# `gate` phase subsumes its registry-ID collision check (`docs_consistency`
# asserts ID uniqueness on the real merge product, where preflight only
# compared two diffs), but NOTHING else read the board at the moment work
# asks to integrate — which is exactly when another session's `hold-off`
# matters. Submitting is now that moment, so the read lives here.
#
# Advisory only, and never fatal, for the two reasons its old home gave: the
# board has no standing to block a merge (D7), and `board-sync.sh` is
# best-effort by design (B6) — a hung network must not cost a submission.
# The binary is deliberately never compiled here; a checkout without one
# reads nothing and says nothing, exactly as before.
if [ "${HV_SLUICE_SKIP_BOARD:-}" != "1" ]; then
    bash "$repo_root/scripts/board-sync.sh" || true
    for candidate in tools/board/target/release/board tools/board/target/debug/board; do
        if [ -x "$repo_root/$candidate" ]; then
            holds="$("$repo_root/$candidate" read 2>/dev/null | grep -F 'polarity=hold-off' || true)"
            if [ -n "$holds" ]; then
                printf '\nADVISORY — hold-off notices from other sessions:\n' >&2
                printf '%s\n' "$holds" | sed 's/^/  /' >&2
                printf '  (advisory only; it does not change this submission)\n\n' >&2
            fi
            break
        fi
    done
fi

# shellcheck disable=SC2029  # meant to expand client-side into the remote command
remote_cmd="if cd $remote_dir && [ -x scripts/sluice-queue.sh ]; then \
scripts/sluice-queue.sh add '$branch' '$ref' '$kind'; else \
echo \"sluice-request-remote: cd '$remote_dir' failed or sluice-queue.sh missing on '$host' -- NOTHING WAS QUEUED\" >&2; \
exit 1; fi"

# shellcheck disable=SC2029
if ! req="$(ssh "$host" "$remote_cmd")"; then
    echo "sluice-request: remote enqueue FAILED — nothing was queued." >&2
    exit 1
fi
echo "sluice-request: $req branch=$branch ref=${ref:0:12} kind=$kind host=$host"
echo "sluice-request: read it back with 'make sluice-status'"
