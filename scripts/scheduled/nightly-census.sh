#!/usr/bin/env bash
# scripts/scheduled/nightly-census.sh — the census, off the critical path.
#
# The Rill's refresh took 19,207 s (5 h 20 m) to move three of the census's 206
# columns (203 metrics plus the identifying columns), at campaign close, with a
# human waiting. Nothing about that run needed to be synchronous. This runs it
# overnight on the canonical box and POSTS THE DIFF; committing a moved column
# stays a deliberate human act (see this directory's README, rule 1).
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

git fetch origin --quiet || exit 1
git reset --hard origin/main --quiet || exit 1
sha="$(git rev-parse HEAD)"

# census-run.sh enforces the canonical host (0079) and serialises on the box's
# claim (0081). If another heavy job holds it, that is not a failure — skip.
#
# FAIL CLOSED, AND MATCH EXACTLY. Three verified facts force this shape:
#  1. `hornvale lab claim-status` (which both status wrappers call) ALWAYS
#     exits 0 — it only prints. So the exit code is not a predicate and the
#     prose is the sole shell-visible signal.
#  2. `census_claim.rs` itself warns against this: "Callers that must *decide*
#     something need the fields, not the sentence: parsing prose back into data
#     is how a measurement acquires a second, disagreeing model of its own
#     format." We have no typed shell path, so we take the narrowest possible
#     prose dependency and make every deviation safe.
#  3. Therefore: proceed ONLY on an exact match of the known free-state string.
#     Held, empty, error, or REWORDED output all fall through to skip. A loose
#     pattern (`grep -qi 'no .*run'`) is wrong in the permissive direction —
#     it can match a held line, and a five-hour census started on a contended
#     box is the expensive failure here.
free="no heavy run in progress"
status="$(bash scripts/census-run.sh status 2>/dev/null || true)"
if [ "$status" != "$free" ]; then
    echo "nightly-census: not starting — status was: ${status:-<empty>}" >&2
    echo "  (expected exactly: $free)" >&2
    exit 0
fi

# THE WORKTREE PATH MUST BE ABSOLUTE. `census-run.sh` uses it two ways that
# disagree about relative paths, and the disagreement is silent:
#   - `:99` probes for an existing worktree with
#     `git worktree list --porcelain | grep -qF "$wt"` — a SUBSTRING match. The
#     literal string `canonical` appears inside
#     `/home/nathan/Projects/hornvale/canonical` in that listing, so a relative
#     `canonical` matches from ANY directory, including this job's
#     `WorkingDirectory=%h/Projects/hornvale-scheduled`.
#   - having "found" it, `:100` runs `git -C canonical fetch`, resolved
#     against the CALLER's cwd, where no such directory exists → exit 128.
# The net effect of the relative form is a job that fails every single night
# and posts `census-run.sh FAILED (rc=128)` to the board forever.
#
# Derived from the scheduled checkout's own MAIN worktree rather than
# hardcoded, so it follows the repo if the checkout ever moves.
# `git worktree list --porcelain` lists the main worktree FIRST (git's own
# ordering) — the same resolution `scripts/worktree-take.sh` relies on.
main_root="$(git worktree list --porcelain | awk '/^worktree /{print $2; exit}')"
if [ -z "$main_root" ]; then
    echo "nightly-census: could not resolve the main worktree from $REPO" >&2
    exit 1
fi
# HV_CENSUS_WORKTREE IS DELIBERATELY NOT SET (decision 0146). It used to be
# "$main_root/canonical", which put the census worktree INSIDE the repo —
# untracked, un-ignored, and deletable by a `git clean -fdx` in the main
# checkout. census-run.sh now anchors its own default to the main worktree
# using this same `git worktree list --porcelain` resolution, so there is
# nothing left for this script to compute.
#
# The two readers below still need to know WHERE the census wrote, so they ask
# census-run.sh rather than recomputing it — `worktree` resolves and prints
# the path under no lock, and asking the one definition is what stops this
# script and that one drifting apart the way the old duplicated path did.
census_wt="$(bash scripts/census-run.sh worktree)"
if [ -z "$census_wt" ]; then
    echo "nightly-census: could not resolve the census worktree path" >&2
    exit 1
fi

HV_CENSUS_REF="$sha" bash scripts/census-run.sh \
    >/tmp/hv-nightly-census.log 2>&1
rc=$?

sha_safe="$(printf '%s' "${sha:0:8}" | board_safe)"

if [ "$rc" -ne 0 ]; then
    tail_safe="$(tail -5 /tmp/hv-nightly-census.log | board_safe)"
    make board-post KIND=technique BY=scheduler PATHS='windows/lab/' \
      NOTE="nightly-census: census-run.sh FAILED on main at ${sha_safe} (rc=$rc). Tail: ${tail_safe}" || true
    exit 0
fi

# DIFF THE TREE THAT WAS ACTUALLY WRITTEN. With HV_CENSUS_REF set,
# `census-run.sh` does `run_root="$wt"; cd "$run_root"` (`:106`/`:110`) and
# publishes `goldens=$run_root/book/src/laboratory/generated` into its own
# claim file (`:126`) — so every golden this job produced landed in
# $census_wt, never in $REPO. Running `make lab-diff` here in $REPO
# diffs an UNTOUCHED tree against its own HEAD: unconditionally "no metric
# moved", so the COLUMNS MOVED notice — the entire point of this job — could
# never fire. `make -C` runs the recipe with that tree as cwd, so both its
# `git show HEAD:…` and its `book/src/laboratory/generated/…` read the tree
# the census wrote.
diff_out="$(make -C "$census_wt" --no-print-directory lab-diff STUDY=the-census 2>/dev/null)"
diff_rc=$?

# THE PREDICATE IS A POSITIVE MATCH, NOT `-n`. `render_diff` ALWAYS emits a
# header ("## Lab diff: …" plus a "Rows N → M" line) and then either
# "No metric moved." or "<k> of <n> metric × pin-set distributions moved.".
# So its output is never empty, and the `-n "$diff_out"` this replaced would
# have posted COLUMNS MOVED every night once the tree above was corrected —
# the same defect in the opposite direction. Three outcomes, all of them
# loud except the genuinely quiet one:
#   moved    -> the fyi notice this job exists to send
#   quiet    -> nothing; a silent night means the census agrees with main
#   anything else (rc != 0, empty, unrecognised wording) -> a technique post,
#            because a nightly reporter that cannot read its own instrument
#            must not look like a clean night.
diff_head="$(printf '%s' "$diff_out" | head -40)"
diff_safe="$(printf '%s' "$diff_head" | board_safe)"
if [ "$diff_rc" -eq 0 ] && printf '%s' "$diff_out" | grep -qF 'distributions moved.'; then
    make board-post KIND=notice BY=scheduler FIELDS='polarity=fyi' PATHS='book/src/laboratory/' \
      NOTE="nightly-census on main at ${sha_safe}: COLUMNS MOVED. Refresh and commit on lefford before your close. ${diff_safe}" || true
elif [ "$diff_rc" -ne 0 ] || ! printf '%s' "$diff_out" | grep -qF 'No metric moved.'; then
    make board-post KIND=technique BY=scheduler PATHS='windows/lab/' \
      NOTE="nightly-census on main at ${sha_safe}: the census ran but lab-diff was unreadable (rc=$diff_rc) — this night reported nothing and is NOT a clean result. Output: ${diff_safe:-<empty>}" || true
fi

# Own no changes: the goldens this run wrote are a report, not a commit. Clean
# the CENSUS worktree, for the same reason the diff reads it — $REPO never
# received a golden, so cleaning $REPO cleaned the wrong tree and left the
# census worktree permanently dirty for the next run's `checkout --force`.
# `--quiet` MUST precede the `--` pathspec separator. After it, git reads it as
# a PATHSPEC: `git checkout -- . --quiet` exits 1 with "pathspec '--quiet' did
# not match any file(s)", and the `2>/dev/null || true` swallows that — leaving
# the tree DIRTY, which is the exact opposite of this line's job. Proven in an
# isolated repo: `-- . --quiet` -> exit 1, file still ` M`; `--quiet -- .` ->
# exit 0, clean. Found by Task 8's dry run, which checked `git status` rather
# than trusting `exit=0`.
git -C "$census_wt" checkout --quiet -- . 2>/dev/null || true
make board-sync >/dev/null 2>&1 || true
exit 0
