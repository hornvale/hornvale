#!/usr/bin/env bash
# scripts/worktree-take.sh — claim a RECYCLED worktree (The Sexton, Task 6).
#
# 73 branches went through this repo in one month against 3 live worktrees,
# each carrying 6-29 GB of target/ and each new one paying a full cold build
# (a measured 771 s). The cost is DESTROYING worktrees, not lacking a cache.
#
# A pool member keeps its warm target/ across campaigns; `git switch -c` costs
# an incremental rebuild instead of a full one.
#
# SWEEPS THE SCRATCH. `.superpowers/sdd/` is per-worktree and git-ignored, so a
# recycled worktree would otherwise hand the next campaign the previous one's
# decision ledger — silently, and it would read as its own.
set -euo pipefail

NAME="${NAME:?usage: make worktree-take NAME=<campaign> [BASE=main]}"
BASE="${BASE:-main}"

# ROOT MUST BE THE MAIN CHECKOUT, NOT "wherever the caller happens to be
# standing". `git rev-parse --show-toplevel` resolves to whichever worktree
# cwd is inside — from inside a linked worktree it returns THAT worktree's
# own root, not the pool's home. Verified: run from inside
# `.claude/worktrees/the-sexton`, `--show-toplevel` returns
# `.../the-sexton`, which makes POOL become `.../the-sexton/.claude/worktrees`
# — a path sharing no members with the real pool. Every real candidate then
# gets filtered out, the script reports "no recyclable member", and creates
# the new worktree NESTED INSIDE the current one. That is bad on its own and
# worse later: if the host worktree is later removed by ordinary cleanup, the
# nested worktree's files vanish while its registry entry survives — the same
# dangling-registry hazard this script exists to close. And running
# `make worktree-take` for your next campaign from inside the one you are
# finishing is an entirely natural thing to do, so this is not an edge case.
#
# Fix: `git worktree list --porcelain` always lists the MAIN worktree first
# (git's own ordering, not something this script imposes), so taking the
# first `worktree ` path resolves to the main checkout regardless of cwd.
# Deliberately not `--path-format=absolute --git-common-dir` (needs git
# 2.31+); this reuses machinery the script already depends on below.
ROOT="$(git worktree list --porcelain | awk '/^worktree /{print $2; exit}')"
POOL="$ROOT/.claude/worktrees"
DEST="$POOL/$NAME"

if [ -d "$DEST" ]; then
    echo "worktree-take: $DEST already exists; nothing to do" >&2
    exit 0
fi

# Prefer an existing pool member whose branch is already merged into BASE.
#
# CANDIDATES COME FROM `git worktree list`, NEVER FROM `find`. This is the
# whole safety property of this loop, and getting it wrong is destructive.
#
# `.claude/worktrees/` accumulates ORPHAN DIRECTORIES — a worktree removed from
# git's registry whose directory survives. `git -C <plain-dir-inside-the-repo>`
# does not fail: it walks UP and operates on the ENCLOSING repository. So a
# `find`-based scan asks an orphan for its branch and gets **the main
# checkout's** branch back, judges it merged (main is always an ancestor of
# origin/main), and then runs `git switch -c` against the main checkout —
# yanking `main` onto a campaign branch underneath every other session.
# Measured on a real orphan (`the-illumination`, 2026-08-13):
#   $ git -C .claude/worktrees/the-illumination branch --show-current
#   main            # <- the MAIN CHECKOUT's branch, not the orphan's
#
# Same family as PROC-git-dir-leak: a directory is not isolated from the
# repository it sits inside.
#
# LOCKED WORKTREES ARE NEVER CANDIDATES. `git worktree list --porcelain`
# emits records separated by blank lines: a `worktree <path>` line, then
# attribute lines, one of which is `locked [reason]` when the worktree is
# locked. `git worktree lock` is git's own "do not touch this" mechanism;
# overriding it here would defeat the reason someone locked it. So the
# porcelain output is parsed by RECORD, not by grepping a single line in
# isolation: buffer the path, note whether a `locked` line appeared before
# the blank-line record boundary, and only emit the path if it did not. The
# `END` block matters because the LAST record has no trailing blank line.
recycled=""
while IFS= read -r wt; do
    # Never the main checkout, whatever the registry says.
    [ "$wt" != "$ROOT" ] || continue
    case "$wt" in "$POOL"/*) ;; *) continue ;; esac

    br="$(git -C "$wt" branch --show-current 2>/dev/null || echo '')"
    [ -n "$br" ] || continue

    # Never recycle a tree someone is working in. A merged branch is not the
    # same as an idle worktree: a session mid-cleanup after its own merge has
    # a merged branch and live uncommitted work.
    #
    # ONE EXCLUSION, and without it this whole script is dead code.
    # `docs/timings/test-baseline-<host>.tsv` is rewritten by every green gate
    # (The Sexton folded ci-record into gate-run), so "has run a gate" — the
    # normal end state of a finished campaign — would otherwise make every pool
    # member permanently unrecyclable and silently degrade the pool to cold
    # creation, defeating the point. It is machine-written and regenerable;
    # genuine work is never only in this file.
    dirty="$(git -C "$wt" status --porcelain 2>/dev/null | grep -v 'docs/timings/test-baseline-' || true)"
    if [ -n "$dirty" ]; then
        echo "worktree-take: skipping $wt — working tree is dirty" >&2
        continue
    fi

    if git -C "$ROOT" merge-base --is-ancestor "$br" "origin/$BASE" 2>/dev/null; then
        recycled="$wt"; break
    fi
done < <(git -C "$ROOT" worktree list --porcelain | awk '
    /^worktree /  { path = $2; locked = 0; next }
    /^locked/     { locked = 1; next }
    /^$/          { if (path != "" && !locked) print path; path = ""; locked = 0; next }
    END           { if (path != "" && !locked) print path }
')

if [ -n "$recycled" ]; then
    echo "worktree-take: recycling $recycled (its branch is merged into $BASE)" >&2
    git -C "$recycled" fetch origin
    # DISCARD THE BASELINE BEFORE SWITCHING, or the exclusion above is worse
    # than the bug it fixes. `git switch -c` refuses to overwrite a locally
    # modified file whose content differs at the target commit — and main's
    # baseline moves constantly, because every green gate rewrites it. So the
    # normal case is exactly the conflicting one. Measured in a throwaway pool:
    #   error: Your local changes to the following files would be overwritten
    #          by checkout: docs/timings/test-baseline-ambrose.tsv
    #   Aborting                                                    (rc=1)
    # Under `set -e` that aborts the script after it has already printed
    # "recycling", turning a silent degradation into a hard stop. The file is
    # machine-written and regenerable — the next green gate rewrites it — which
    # is the same premise that let us ignore it above, so restoring it here is
    # the other half of one decision, not a separate judgement about someone's
    # work.
    git -C "$recycled" checkout --quiet -- 'docs/timings/test-baseline-*.tsv' 2>/dev/null || true
    git -C "$recycled" switch -c "campaign/$NAME" "origin/$BASE"
    rm -rf "$recycled/.superpowers/sdd"
    mv "$recycled" "$DEST"
    # THE RENAME INVALIDATES COMPILED PATHS AND CARGO DOES NOT KNOW IT.
    # `env!("CARGO_MANIFEST_DIR")` and `CARGO_TARGET_TMPDIR` are baked at compile
    # time; `cargo build --workspace --all-targets` reports the tree fresh after a
    # rename, so every cached test binary still points at the previous campaign's
    # directory. Measured taking the-axes -> the-staff: six failures whose panics
    # named the old path, which read exactly like a red main.
    #
    # Touch the sources that read either macro, derived by grep rather than
    # hand-listed, so a new call site is covered the day it lands. 29 of the 31 are
    # under tests/ and recompile only their own test binary; 2 are under src/ and
    # rebuild their crate. That is far cheaper than the 771 s cold build this pool
    # exists to avoid.
    #
    # `|| true`: a worktree with no matching file is fine, and `worktree-take` must
    # not fail on a convenience step.
    grep -rl 'env!("CARGO_MANIFEST_DIR")\|CARGO_TARGET_TMPDIR' \
        --include='*.rs' "$DEST" 2>/dev/null | xargs -r touch || true
    # `mv` leaves the MAIN REPO's back-pointer stale. Verified, and the naive
    # assumption is wrong in an important way: the moved worktree's own
    # commands keep working (its `.git` file is an absolute path to an
    # unchanged admin dir), so nothing looks broken. What breaks is
    # `.git/worktrees/<name>/gitdir`, which still names the OLD path — so
    # `git worktree list` reports a path that no longer exists and
    # `git worktree prune` may reap a live worktree. Probe output:
    #   $ git worktree repair /tmp/hv-mv-probe-moved
    #   repair: gitdir incorrect: .../.git/worktrees/hv-mv-probe/gitdir
    #
    # BY THIS POINT THE WORKTREE HAS ALREADY BEEN SWITCHED, SWEPT, AND MOVED.
    # If `repair` itself fails, `set -e` would otherwise abort silently,
    # leaving a LIVE worktree at $DEST whose registry entry still names the
    # OLD path — exactly the state the comment above says `git worktree
    # prune` can exploit to reap a live worktree. This is the worst moment to
    # be quiet, so failure here is handled explicitly instead of falling
    # through to `set -e`.
    if ! git -C "$ROOT" worktree repair "$DEST"; then
        echo "worktree-take: FATAL - worktree repair failed after the move." >&2
        echo "  $DEST is LIVE but the registry still names its old path." >&2
        echo "  Do NOT run 'git worktree prune' until this is fixed: it can reap a live worktree." >&2
        echo "  Fix by hand: git -C $ROOT worktree repair $DEST" >&2
        exit 1
    fi
    echo "worktree-take: $DEST is warm — no prewarm needed" >&2

    # VERIFY THE INVALIDATION AT THE MOMENT IT MATTERS. Measured cost: ~1 s per
    # sibling worktree scanning ONLY executables (target/debug/deps holds 24,544
    # entries; unrestricted this would cost ~5 s across a six-member pool), cheap
    # enough to run on every take.
    #
    # NOTE WHAT THIS DOES AND DOES NOT VERIFY. `git worktree repair` (just above)
    # has already run, and repair rewrites the registry entry to the NEW path —
    # so by this point $DEST's OWN FORMER NAME is gone from `git worktree list`
    # entirely; the freshness check cannot see it and this call is trivially
    # green with respect to the rename just performed. (Verified directly: a
    # scratch worktree renamed WITHOUT repair is correctly flagged red for its
    # stale self-reference; the SAME scratch worktree, repaired, goes green
    # while the identical stale binary is still sitting there untouched.) The
    # touch invalidation above is what actually fixes THIS rename — this call
    # exists to catch a DIFFERENT class of contamination: an artifact baking
    # some OTHER, currently-live sibling worktree's path. `|| true`: a hit here
    # reflects pre-existing state this take did not cause, so it must not abort
    # a take that has already completed its real work — it only needs to be
    # visible.
    ( cd "$DEST" && bash "$ROOT/scripts/test-worktree-freshness.sh" ) || true
else
    echo "worktree-take: no recyclable member; creating a cold worktree" >&2
    git -C "$ROOT" worktree add "$DEST" -b "campaign/$NAME" "origin/$BASE"
    echo "worktree-take: run 'make prewarm' in $DEST, in the background" >&2
fi
