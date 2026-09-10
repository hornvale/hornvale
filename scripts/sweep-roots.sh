#!/usr/bin/env bash
# scripts/sweep-roots.sh — print every directory a reclamation pass must
# descend into, one absolute path per line, NUL-safe via the caller's xargs.
#
# WHY THIS EXISTS. `make sweep` ran `cargo sweep -r .` and reported a plausible
# number while reaching almost nothing. Two independent reasons, both silent:
#
#   1. `cargo sweep -r` DEFAULTS TO SKIPPING DOT-DIRECTORIES (its `--hidden`
#      flag exists to turn that off). This project's campaign pool is
#      `.claude/worktrees/`, so every campaign worktree was invisible to it.
#      Measured 2026-09-10 from the repo root: plain `-r .` visits 10 target
#      dirs, `-r --hidden .` visits 81.
#   2. THERE IS A SECOND POOL, AND IT IS NOT UNDER THE REPO AT ALL.
#      `~/.claude/CLAUDE.md` sets the worktree dir to
#      `~/.config/superpowers/worktrees/<project>/`; `scripts/worktree-take.sh`
#      uses `$ROOT/.claude/worktrees`. Both are live. A recursion rooted at the
#      repo cannot reach the first no matter what flags it is given.
#
# Between them, 723 of 805 GB of Hornvale build cache sat outside the reach of
# the command whose whole job was reclaiming it, while that command exited 0.
#
# THE SOURCE OF TRUTH IS `git worktree list`, NOT A PATH LIST. Hardcoding the
# two known pools would fix today and rot on the next tool that puts a worktree
# somewhere new — which is exactly how the second pool came to exist unnoticed.
# Git already knows where every worktree of this repo is, whatever created it
# and wherever it lives, and it knows nothing about OTHER projects' worktrees
# that happen to share the superpowers directory. That scoping is a property we
# want and would have to hand-maintain under any path-list scheme.
#
# A PRUNED WORKTREE IS SKIPPED, NOT REPORTED. `git worktree list` can name a
# directory that no longer exists (removed by hand, `prune` not yet run).
# Passing it to cargo-sweep is a hard error that would abort the whole pass, so
# a vanished path is dropped here — the one case where silence is right, since
# a directory that is gone has no bytes to reclaim.
set -euo pipefail

git rev-parse --git-dir >/dev/null 2>&1 || {
    echo "sweep-roots: not a git repository" >&2
    exit 2
}

n=0
while IFS= read -r line; do
    case "$line" in worktree\ *) ;; *) continue ;; esac
    path="${line#worktree }"
    [ -d "$path" ] || continue
    printf '%s\n' "$path"
    n=$((n + 1))
done < <(git worktree list --porcelain)

# Exit 3 on an empty roster so a caller can tell "nothing to sweep" from "the
# enumeration broke", which would otherwise both hand xargs an empty list and
# let cargo-sweep silently fall back to its own default path. Same three-valued
# discipline as scripts/subfloor-roster.sh.
[ "$n" -gt 0 ] || { echo "sweep-roots: no worktree directories found" >&2; exit 3; }
