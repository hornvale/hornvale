#!/usr/bin/env bash
# scripts/test-worktree-freshness.sh — a worktree must never serve a binary
# compiled under a different path.
#
# Usage: test-worktree-freshness.sh [old-path]
#
#   old-path (optional) — an EXTRA path to check artifacts against, beyond
#   whatever `git worktree list` currently reports. Exists to close a real
#   gap found in review: once `git worktree repair` has run after a rename,
#   the worktree's OWN FORMER name is gone from `git worktree list`
#   entirely — the registry now reports only the new path — so a call made
#   after repair, with no argument, is architecturally blind to the exact
#   self-rename staleness this task exists to catch; it can only ever detect
#   contamination from a DIFFERENT, currently-live sibling. `worktree-take.sh`
#   knows the old name directly ($recycled, a plain shell variable that
#   `git worktree repair` does not touch or erase) and passes it here,
#   closing the gap for the one caller that has the information. A caller
#   with no such extra knowledge (lane-run.sh, a human at a shell) can omit
#   it; the check then falls back to the sibling-only behaviour below.
#
# DIRECTION THIS CHECK ENFORCES: it asserts `no compiled artifact references
# a SIBLING worktree's path, or the explicit old-path argument when given`.
# It is blind to any OTHER path that is neither a currently-registered
# sibling nor the supplied argument — some third worktree's former name that
# nobody passed in — so a green result means "no contamination from what
# this run was told to check for", not "every artifact is fresh".
#
# The defect: `make worktree-take` renames a pool member and keeps target/.
# CARGO_MANIFEST_DIR and CARGO_TARGET_TMPDIR are baked at compile time, cargo
# considers the tree fresh, and 31 files under kernel/ domains/ windows/ cli/
# read one of those two macros. Six tests fail with a panic naming the OLD path.
set -euo pipefail
old_path="${1:-}"
root="$(git rev-parse --show-toplevel)"
deps="$root/target/debug/deps"

if [ ! -d "$deps" ]; then
    echo "worktree-freshness: no $deps yet — nothing compiled, trivially clean."
    exit 0
fi

# Every OTHER worktree's path, from git's own list, EXCLUDING any ancestor of
# $root. Measured: every nested campaign worktree lives at
# <main-repo>/.claude/worktrees/<name>, so the main checkout's own path is a
# literal PREFIX of every campaign worktree's path. Every compiled artifact's
# legitimate, CORRECT self-reference (its own CARGO_MANIFEST_DIR) therefore
# contains the main checkout's path as a substring too — grep -F would flag
# that as "foreign" on every single artifact, in every worktree, always,
# regardless of staleness. Confirmed empirically: a worktree untouched by any
# recent rename (the-docket) still matched the main-checkout path on 214/214
# of its executables and 0 on any genuine contamination marker. Without this
# exclusion the check can never go green, which defeats it entirely — a check
# that is always red trains people to ignore it, exactly the "worse than no
# check" failure this task exists to avoid.
# `|| true` guards the WHOLE substitution, not just the grep: under
# `pipefail`, an empty result is nonzero from either half — `grep -vxF` when
# there are no other worktrees at all, or the trailing `while read` loop
# itself (a loop that consumes zero lines still exits nonzero, from its own
# final failed `read`) when every candidate got filtered out as an ancestor.
# Either is a legitimate "no others" outcome, not a script-ending error.
others="$(
    git -C "$root" worktree list --porcelain \
        | awk '/^worktree /{print $2}' \
        | grep -vxF "$root" \
        | while IFS= read -r w; do
            case "$root" in
                "$w"/*) ;;              # $w is an ancestor of $root — not foreign
                *) printf '%s\n' "$w" ;;
            esac
          done
)" || true

# Fold in the caller-supplied old path, if any. Deduplicated against what git
# already reported: a call made BEFORE `repair` runs would otherwise see the
# same old name twice (once from git's still-stale registry, once from this
# argument). Guarded against equalling $root itself, which would only ever
# indicate a caller bug, not real contamination.
if [ -n "$old_path" ] && [ "$old_path" != "$root" ]; then
    if ! printf '%s\n' "$others" | grep -qxF "$old_path"; then
        others="$(printf '%s\n%s\n' "$others" "$old_path" | grep -v '^$')"
    fi
fi

if [ -z "$others" ]; then
    echo "worktree-freshness: no sibling worktrees to be contaminated by."
    exit 0
fi

found=0
for other in $others; do
    # Executables only. `.rlib`/`.rmeta`/`.d` are most of the 24,544 entries
    # in deps/ and are not what a test runs; scanning them costs ~1 s per
    # sibling worktree and buys nothing.
    hits="$(find "$deps" -maxdepth 1 -type f -perm -u+x -print0 2>/dev/null \
        | xargs -0 grep -lF "$other" 2>/dev/null || true)"
    if [ -n "$hits" ]; then
        found=1
        echo "worktree-freshness: artifacts bake the FOREIGN path $other:" >&2
        echo "$hits" | sed 's/^/  /' | head -10 >&2
    fi
done

if [ "$found" -ne 0 ]; then
    cat >&2 <<'EOF'

These binaries were compiled while this directory had a different name, so
env!("CARGO_MANIFEST_DIR") and CARGO_TARGET_TMPDIR point somewhere that no
longer exists. Tests reading them fail with a panic naming the old path, which
reads exactly like a red main and is not one.

Fix: re-run `make worktree-take`, which now invalidates them, or force a
rebuild of the affected crates.
EOF
    exit 1
fi
echo "worktree-freshness: clean — no artifact references a sibling worktree."
