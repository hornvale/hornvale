#!/usr/bin/env bash
# scripts/scheduled/nightly-census.sh — the census, off the critical path.
#
# The Rill's refresh took 19,207 s (5 h 20 m) to move three of 205 columns, at
# campaign close, with a human waiting. Nothing about that run needed to be
# synchronous. This runs it overnight on the canonical box and POSTS THE DIFF;
# committing a moved column stays a deliberate human act (see this directory's
# README, rule 1).
set -uo pipefail

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

HV_CENSUS_WORKTREE=canonical HV_CENSUS_REF="$sha" bash scripts/census-run.sh \
    >/tmp/hv-nightly-census.log 2>&1
rc=$?

if [ "$rc" -ne 0 ]; then
    make board-post KIND=technique BY=scheduler PATHS='windows/lab/' \
      NOTE="nightly-census: census-run.sh FAILED on main at ${sha:0:8} (rc=$rc). Tail: $(tail -5 /tmp/hv-nightly-census.log | tr '\n' ' ' | tr -d '"')" || true
    exit 0
fi

diff_out="$(make lab-diff STUDY=the-census 2>/dev/null | head -40)"
if [ -n "$diff_out" ]; then
    make board-post KIND=notice BY=scheduler FIELDS='polarity=fyi' PATHS='book/src/laboratory/' \
      NOTE="nightly-census on main at ${sha:0:8}: COLUMNS MOVED. Refresh and commit on lefford before your close. $(echo "$diff_out" | tr '\n' ' ' | tr -d '"')" || true
fi

# Own no changes: the goldens this run wrote are a report, not a commit.
# `--quiet` MUST precede the `--` pathspec separator. After it, git reads it as
# a PATHSPEC: `git checkout -- . --quiet` exits 1 with "pathspec '--quiet' did
# not match any file(s)", and the `2>/dev/null || true` swallows that — leaving
# the tree DIRTY, which is the exact opposite of this line's job. Proven in an
# isolated repo: `-- . --quiet` -> exit 1, file still ` M`; `--quiet -- .` ->
# exit 0, clean. Found by Task 8's dry run, which checked `git status` rather
# than trusting `exit=0`.
git checkout --quiet -- . 2>/dev/null || true
make board-sync >/dev/null 2>&1 || true
exit 0
