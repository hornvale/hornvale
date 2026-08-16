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
# left as advice in a skill. Under sluice-run.sh's `--no-ff` merge, this
# commit's own subject becomes the merge commit's subject
# (`headline="${HV_SLUICE_HEADLINE:-$(git log -1 --format=%s "$sha")}"`),
# which tools/census/history.sh reads as the census epoch label
# (`git log --follow --first-parent main`). Only the campaign that authored
# $ref knows whether that subject is fit to become a permanent artifact
# label — an operator triaging the queue later must never be the one
# inventing or silently accepting a placeholder one. So this derives the
# SAME subject sluice-run.sh will actually use (not a separately
# caller-supplied string, which could say anything and still not match what
# actually lands) and refuses to enqueue if it looks like a placeholder.
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
    headline="$(git -C "$repo_root" log -1 --format=%s "$ref" 2>/dev/null || true)"
    case "$headline" in
        ""|wip|WIP|fixup!*|squash!*|.|tmp|temp|TODO)
            echo "sluice-request: refusing — the commit at ${ref:0:12}'s subject ('$headline') is not a real headline." >&2
            echo "  Under --no-ff this becomes the merge commit's subject, which" >&2
            echo "  tools/census/history.sh reads as the census epoch label. Amend" >&2
            echo "  the commit (or add a proper final commit) and push again." >&2
            exit 2
            ;;
    esac
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
