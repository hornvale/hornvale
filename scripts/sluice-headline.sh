#!/usr/bin/env bash
# scripts/sluice-headline.sh — how a merge commit's SUBJECT is decided.
#
# Sourced by `sluice-request.sh` (which REFUSES a merge without one) and by
# `sluice-run.sh` (which COMPOSES the subject from it). One implementation
# deliberately, not two: the caller-side refusal is only worth anything if it
# derives the same string the chamber will actually use, and a second copy
# drifts the moment either side is edited. This repo has been bitten by two
# guards sharing a string more than once.
#
# WHY A TRAILER AND NOT THE TIP COMMIT'S SUBJECT. The tip's subject was the
# original rule and it failed FOUR times out of four real merges, in three
# distinct ways, all traceable to one root: the headline was inferred from
# POSITION, and position is fragile.
#
#   merge(the-rhumb): merge(the-rhumb): the compass now means what it says
#       ^ DOUBLED. The author wrote a correct merge headline; the chamber
#         prefixed it blindly, because a prefixer cannot know what is already
#         in the string.
#   merge(fix/sluice-coalescing): fix(sluice): coalescing could not see …
#   merge(follow-up/census-ratchet): chore(census): ratchet the ceiling …
#       ^ LEAKED BRANCH PATH. `${branch#campaign/}` stripped only that one
#         prefix, so every other branch carried its whole path into the label.
#   merge(the-begat): perf(begat): walk the founding tree down …
#       ^ legible, but a redundant conventional-commit prefix inside a subject
#         that already names the campaign.
#
# And the failure that cost the most, which none of the above shows because it
# leaves no trace in what landed: a headline commit is DISPLACED by anything
# committed after it. One campaign hit that twice in one evening — the second
# time after it already knew — because the fix for the first (adding a
# chronicle section) was itself a commit. Its eventual workaround was an
# EMPTY commit, since a headline had to be last and force-pushing is refused.
# That is a sound workaround for a mechanism that should not require one.
#
# A trailer is not positional. It can sit on any commit in the range, later
# commits do not displace it, and it cannot be written by accident — which is
# the half the `wip|fixup|tmp` refusal never covered, since an ordinary
# tidy-up subject on an ordinary commit sails straight through a check that
# only knows placeholder words.
#
# THE VALUE IS THE SUBJECT WITHOUT ITS PREFIX. The chamber composes
# `merge(<short-name>): <trailer>`, so a trailer should read as the thing that
# landed and nothing more. A leading `merge(...): ` is stripped defensively
# anyway — an author who writes the whole subject is doing something
# reasonable, and refusing them over a prefix would be pedantry.

# The short name that goes inside `merge(...)`. The LAST path segment, always:
# `campaign/the-rhumb` -> `the-rhumb`, `fix/sluice-coalescing` ->
# `sluice-coalescing`, `main` -> `main`. Replaces `${branch#campaign/}`, which
# only ever handled one prefix and silently passed everything else through
# whole. Deliberately not a lookup table of known prefixes: a new prefix is
# coined every few weeks and would leak again the first time.
sluice_short_name() {
    printf '%s' "${1##*/}"
}

# Strip a leading `merge(anything): ` so composing can never double it.
sluice_strip_merge_prefix() {
    printf '%s' "${1#merge(*): }"
}

# The headline for a candidate, or empty if the range carries no trailer.
#   $1 repo root   $2 base ref (what the merge is against)   $3 candidate sha
#
# NEWEST WINS. `git log` yields reverse-chronological, so the first non-empty
# trailer is the author's latest intent — an author who changes their mind
# adds another commit rather than rewriting history, which is the same shape
# every other rule here assumes (force-pushing is refused by a hook).
#
# THE RANGE IS `base..sha`, WHICH MAKES A STALE BASE THE ONE HAZARD. Commits
# already on `main` are excluded, so a current base sees only the candidate's
# own work. A base that has fallen behind admits main's commits into the
# range — and once this convention is in use those carry trailers of their
# own, from other campaigns. Both callers therefore fetch before asking.
#
# THE TRAILER MUST BE IN THE MESSAGE'S FINAL BLOCK. This uses git's own
# trailer parser, which by definition reads only the last paragraph — so
#
#     Sluice-Headline: what landed
#                                     <- this blank line breaks it
#     Claude-Session: https://…
#
# is TWO blocks, and git reports only the second. The commit introducing this
# convention was written exactly that way and its own headline did not
# resolve, which is how the trap was found. Keep `Sluice-Headline:` adjacent
# to whatever other trailers the commit carries, with no blank line between.
#
# The failure is silent HERE by construction — an unparsed trailer is
# indistinguishable from an absent one — so it is made loud at the only place
# that can: `sluice-request.sh`'s refusal names the placement rule, and
# `scripts/test-sluice.sh` pins both halves (a final-block trailer resolves; a
# trailer stranded above a blank line does not, and is refused).
sluice_headline_of() {
    local repo="$1" base="$2" sha="$3" line
    while IFS= read -r line; do
        [ -n "$line" ] || continue
        sluice_strip_merge_prefix "$line"
        return 0
    done < <(git -C "$repo" log --format='%(trailers:key=Sluice-Headline,valueonly)' \
                 "$base..$sha" 2>/dev/null)
    printf ''
}

# Is a candidate subject fit to be a permanent label? Shared so the mouth's
# refusal and any later check agree on what "junk" means. Returns 0 for junk.
sluice_headline_is_junk() {
    case "$1" in
        ""|wip|WIP|fixup!*|squash!*|.|tmp|temp|TODO) return 0 ;;
        *) return 1 ;;
    esac
}
