#!/usr/bin/env bash
# scripts/scheduled/nightly-drift.sh — the drift check, unattended.
#
# THE CONSTITUTIONAL CONSTRAINT OF THIS DIRECTORY: a scheduled job NEVER
# commits and NEVER touches main's working tree. It reports. The precedent is
# decision 0129's lane rule — "the lane must never be wired to auto-implement a
# suggestion... that would make the board self-modifying with no human in the
# loop" — and the concrete hazard is a nightly job committing while a session
# is mid-landing, which `make preflight` warns about and cannot prevent.
#
# Wall-clock time is used freely here. The determinism ban governs the SIM;
# scheduling is outside that boundary, exactly as clients/ is.
set -uo pipefail

# Sanitise anything interpolated into `make board-post NOTE=`.
#
# TWO SEPARATE HAZARDS, and the second is the one that bites hardest:
#  - Shell: a double quote splits NOTE into extra words, tools/board rejects
#    the malformed argv with exit 2, and the call site's `|| true` swallows it.
#  - MAKE: NOTE is a make command-line variable, and GNU Make recursively
#    expands `$(...)` in variable values BEFORE the shell is involved. A
#    `$(shell ...)` in the text EXECUTES. Verified against this repo's own
#    Makefile with `make -n` — the dry run still ran the command.
# So `$` and backticks must go, not just quotes. These are diagnostic strings;
# losing a literal `$` costs nothing. Every value interpolated into a NOTE=
# below is captured into a variable and piped through this FIRST — never a
# raw `$(...)` substitution inline inside the NOTE="..." string.
board_safe() { tr -d '\\`$"' | tr '\n' ' '; }

REPO="${HV_SCHED_REPO:?HV_SCHED_REPO must name a checkout this job owns}"
cd "$REPO" || exit 1

git fetch origin --quiet || { echo "nightly-drift: fetch failed" >&2; exit 1; }
git reset --hard origin/main --quiet || exit 1

make rebaseline >/tmp/hv-nightly-drift.log 2>&1
rc=$?

paths="$(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | tr '\n' ' ')"
if [ -z "$paths" ]; then
    echo "nightly-drift: docs/generated-paths.txt yielded no paths; not diffing" >&2
    exit 1
fi
# shellcheck disable=SC2086
drift="$(git diff --stat -- $paths)"

# `git checkout -- .` restores tracked files only; untracked files survive it
# and survive the next run's `reset --hard` too. This job runs unattended in a
# worktree a human also uses, so it must neither accumulate cruft nor delete
# anything silently: report first, then clean.
#
# `-fd` and deliberately NOT `-x`: ignored paths (target/, the warm build this
# whole scheduler depends on) must survive.
#
# `--quiet` must come BEFORE the `--` pathspec separator: `git checkout --
# . --quiet` treats `--quiet` as a pathspec, fails, and (because the failure
# is swallowed) silently leaves the working tree dirty — caught by the dry
# run in Step 4, not by reading.
# ONLY untracked entries. An earlier version captured the whole
# `git status --porcelain`, which on any night with real drift is dominated by
# modified TRACKED files — the very drift this script exists to report — so the
# cruft notice fired redundantly and described reverted files as "removed".
stray="$(git status --porcelain 2>/dev/null | grep '^??' || true)"
git checkout --quiet -- . 2>/dev/null || true
if [ -n "$stray" ]; then
    n="$(printf '%s\n' "$stray" | wc -l | tr -d ' ')"
    # The paths go to stderr (journalctl), NOT into the note: `git status
    # --porcelain` escapes unusual names, and a quote or space in a path splits
    # `make board-post`'s NOTE into extra shell words, which tools/board rejects
    # with exit 2 — swallowed by the `|| true` below. A count cannot break it.
    echo "nightly-drift: removing $n untracked path(s):" >&2
    printf '%s\n' "$stray" >&2
    git clean -fdq 2>/dev/null || true
    n_safe="$(printf '%s' "$n" | board_safe)"
    make board-post KIND=technique BY=scheduler PATHS='scripts/' \
      NOTE="nightly-drift: the checkout carried ${n_safe} untracked path(s) and has been cleaned. See journalctl -u hornvale-nightly.service for the list." || true
fi

if [ "$rc" -ne 0 ]; then
    tail_safe="$(tail -5 /tmp/hv-nightly-drift.log | board_safe)"
    make board-post KIND=technique BY=scheduler PATHS='scripts/' \
      NOTE="nightly-drift: make rebaseline FAILED on main (rc=$rc). Tail: ${tail_safe}" || true
elif [ -n "$drift" ]; then
    drift_safe="$(printf '%s' "$drift" | board_safe)"
    make board-post KIND=notice BY=scheduler FIELDS='polarity=fyi' PATHS='book/ docs/audits/ docs/digest/' \
      NOTE="nightly-drift: main has UNCOMMITTED generated-artifact drift. Someone merged without running make rebaseline. ${drift_safe}" || true
fi

make board-sync >/dev/null 2>&1 || true
exit 0
