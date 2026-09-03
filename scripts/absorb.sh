#!/usr/bin/env bash
# scripts/absorb.sh — absorb main into a campaign branch, regenerating the
# artifacts that cannot be merged instead of asking a human to merge them.
#
# WHY THIS EXISTS. Six candidates were refused at the sluice's mouth in one
# session (2026-09-02) over `docs/audits/type-audit-report.md`, and THREE
# conflicted in nothing else: campaign/the-hallmark 0e21ed0c6843,
# campaign/the-rack d5bee7a1e3de, campaign/the-brattice 91e40b0669ba. Each
# arrived with complete, green work and was turned away by a file that
# regenerates in one command. That file is 68 lines of repo-wide AGGREGATE
# COUNTS, so any pub-boundary change anywhere moves the same lines, and ~10% of
# the commits landing on main touch it.
#
# THE REMEDY IS NOT NEW, ONLY UNPACKAGED. Decision 0166 retired
# `merge=hv-regenerate` after measuring that a merge DRIVER cannot regenerate a
# merge product — git invokes a driver BEFORE the product exists on disk, so it
# measures the wrong tree and silently emits one side. Its "Alternatives
# considered" names the shape that does work:
#
#   "Regenerate in `post-merge`. The cheap form of the same idea, and the one
#    worth pursuing: `post-merge` runs at the one moment the tree genuinely IS
#    the merge product, so it is not structurally impossible the way a driver
#    is. ... it belongs in an idea-registry row and its own change."
#
# This is that change, in its manual form: regeneration AFTER the merge, when
# the tree is the product. Every campaign was already doing this by hand, and
# six of them got some step of it wrong in one night.
#
# WHAT IT WILL NOT DO. It resolves ONLY paths whose `docs/generated-paths.txt`
# author is `artifacts` — the invocation `make rebaseline` runs. A `census` or
# `heavy` or `none` path is a human's conflict and this script stops on it. It
# never touches a source conflict, never uses --no-verify, and refuses outright
# on a dirty tree rather than merging over uncommitted work.
set -uo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root" || exit 1
# shellcheck source=/dev/null
HV_PHASES_LIB=1 . "$root/scripts/sluice-phases.sh"

say() { printf 'absorb: %s\n' "$1"; }
die() { printf 'absorb: %s\n' "$1" >&2; exit "${2:-1}"; }

branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"
if [ -z "$branch" ] || [ "$branch" = "HEAD" ]; then
    die "detached HEAD — check out a campaign branch first" 2
fi
[ "$branch" != "main" ] || die "on main; absorbing main into main is a no-op. Check out your campaign branch." 2

# A dirty tree is refused, not stashed. `git stash` is a SHARED stack across
# worktrees despite reading as personal, so stashing here would put a campaign's
# work somewhere another worktree can pop it.
if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
    git status --short --untracked-files=no | sed 's/^/  /' >&2
    die "tracked changes are uncommitted — commit or discard them before absorbing" 2
fi

say "fetching origin"
git fetch -q origin || die "git fetch failed" 3
before="$(git rev-parse --short HEAD)"
behind="$(git rev-list --count "HEAD..origin/main" 2>/dev/null || echo '?')"
say "$branch is $behind commit(s) behind origin/main"
if [ "$behind" = "0" ]; then
    say "already up to date; nothing to absorb."
    exit 0
fi

if git merge --no-edit origin/main >/tmp/absorb-merge.$$ 2>&1; then
    sed 's/^/  /' /tmp/absorb-merge.$$; rm -f /tmp/absorb-merge.$$
    say "merged cleanly"
else
    rm -f /tmp/absorb-merge.$$
    conflicts="$(git diff --name-only --diff-filter=U)"
    [ -n "$conflicts" ] || die "merge failed for a reason that is not a conflict — resolve by hand" 4
    printf '%s\n' "$conflicts" | sed 's/^/  conflict: /'

    # THE WHOLE JUDGMENT OF THIS SCRIPT IS THIS ONE CALL. It is the same
    # function `sluice-mouth.sh` and `sluice-run.sh` use, so the three cannot
    # disagree about what is regenerable — the shared-source-of-truth rule that
    # decision 0079 applies to the host check.
    if sluice_is_regenerated_only "$conflicts" "$root"; then
        n="$(printf '%s\n' "$conflicts" | grep -c .)"
        say "all $n conflict(s) are artifacts-authored — resolving by regeneration, not by merge"
        # Take ANY side to clear the conflict. This is a placeholder, not an
        # answer: `make rebaseline` below overwrites every one of these paths
        # from source, at the one moment the tree IS the merge product. Which
        # side is taken cannot survive that, which is exactly why decision
        # 0166's objection to a merge driver does not apply here.
        while IFS= read -r pth; do
            [ -n "$pth" ] || continue
            git checkout --ours -- "$pth" 2>/dev/null || die "could not stage a side of $pth" 4
            git add -- "$pth" || die "could not stage $pth" 4
        done <<EOF
$conflicts
EOF
    else
        echo >&2
        say "STOPPING. Not every conflict is artifacts-authored, so this needs you." >&2
        while IFS= read -r pth; do
            [ -n "$pth" ] || continue
            a="$(sluice_path_author "$pth" "$root/docs/generated-paths.txt")"
            printf '  %-56s author=%s\n' "$pth" "${a:-<not a declared generated path>}" >&2
        done <<EOF
$conflicts
EOF
        echo >&2
        say "the merge is left in progress. Resolve them, 'git add' them, then" >&2
        say "'git commit' to finish it — or 'git merge --abort' to back out." >&2
        say "There is no --continue here on purpose: re-running this script after" >&2
        say "a partial hand-resolve would regenerate over work you had not staged." >&2
        exit 4
    fi
fi

# HV_ABSORB_REGEN is a TEST SEAM, not a caller-facing override. scripts/test-
# absorb.sh substitutes a stub so the resolution path can be driven in a scratch
# repo; a real regeneration needs the whole toolchain and cannot run there. The
# property under test is which conflicts get resolved and which stop the script,
# and that property is untestable without this hook.
say "regenerating (make rebaseline)"
if ! ${HV_ABSORB_REGEN:-make rebaseline} >/tmp/absorb-regen.$$ 2>&1; then
    tail -20 /tmp/absorb-regen.$$ | sed 's/^/  /' >&2
    rm -f /tmp/absorb-regen.$$
    die "make rebaseline failed — the merge is staged but NOT committed; fix and re-run" 5
fi
rm -f /tmp/absorb-regen.$$

# The drift check, reading its path list from the one file that declares it.
# An array, not an unquoted expansion: the pathspec list must survive a path
# with a space in it, and `mapfile` is bash 4 while this repo is pinned to 3.2
# (scripts/hooks/pre-commit's check-bash32).
declared_paths=()
while IFS= read -r _p; do
    [ -n "$_p" ] && declared_paths+=("$_p")
done < <(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
drifted="$(git diff --name-only -- "${declared_paths[@]}" 2>/dev/null)"
if [ -n "$drifted" ]; then
    printf '%s\n' "$drifted" | sed 's/^/  regenerated: /'
    git add -- "${declared_paths[@]}" 2>/dev/null
fi

if git diff --cached --quiet && [ -z "$(git rev-parse -q --verify MERGE_HEAD 2>/dev/null)" ]; then
    say "nothing to commit after regeneration"
else
    git commit --no-edit -q || die "the merge commit failed — see the hook output above" 6
fi

say "absorbed origin/main into $branch: $before -> $(git rev-parse --short HEAD)"
say "NOT gated and NOT pushed. Run 'make gate-commit', then submit."
exit 0
