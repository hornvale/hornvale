#!/usr/bin/env bash
# test-scratch-worktree-anchor.sh — the chamber's and heavy's scratch worktrees
# must be anchored to the MAIN worktree, never to the caller's.
#
# WHY. `$repo_root` is wherever the script was invoked from. Both sluice-run.sh
# and heavy-run.sh derived their scratch worktree as `$repo_root/../<name>`, so
# running either from a LINKED worktree created it INSIDE .claude/worktrees/ —
# untracked, un-ignored, and destroyable by a `git clean -fdx` in the checkout
# it sits under. Decision 0146 restored exactly this invariant for the census
# after HV_CENSUS_WORKTREE=canonical created one at ~/Projects/hornvale/canonical,
# but the fix landed in census-run.sh alone.
#
# It happened. Three strays were on disk on 2026-09-06:
# .claude/worktrees/hornvale-sluice-wt (4.5G), .../tooling/hornvale-sluice-wt,
# and a lane-era hornvale-lane-wt beside them. Measured from a real linked
# worktree, the old expression resolved to precisely the second of those.
#
# THIS GUARD IS STATIC, AND SAYS SO. Neither script exposes a path-query
# subcommand the way census-run.sh does (`census-run.sh worktree`), and adding
# one to a script that takes the shared claim is more surface than this
# deserves. So it pins the SHAPE: the anchored resolution must be present and
# the bare caller-relative form must be absent. A static check cannot prove the
# runtime path; it can prove the expression that produced the strays is gone,
# which is the regression worth catching.
set -uo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
pass=0; fail=0
ok()  { echo "  ok: $1"; pass=$((pass+1)); }
bad() { echo "  FAIL: $1"; fail=$((fail+1)); }

for pair in "sluice-run.sh:hornvale-sluice-wt" "heavy-run.sh:hornvale-heavy-wt"; do
    f="${pair%%:*}"; name="${pair##*:}"
    echo "== $f"
    if grep -q "worktree list --porcelain" "$root/scripts/$f"; then
        ok "$f resolves the main worktree with 'worktree list --porcelain'"
    else
        bad "$f no longer resolves the main worktree — it is caller-relative again"
    fi
    # Judge CODE, not prose: strip comments first. heavy-run.sh carries a
    # comment that legitimately names the path expression, and an earlier
    # draft of this test failed on it. A guard that reds on a comment is a
    # guard people delete.
    if sed 's/#.*//' "$root/scripts/$f" | grep -qE "\\\$repo_root/\.\./$name"; then
        bad "$f still derives $name from \$repo_root — this is the expression that made the strays"
    else
        ok "$f does not derive $name from \$repo_root"
    fi
done

echo "== census-run.sh, the precedent, still anchored"
if grep -q "worktree list --porcelain" "$root/scripts/census-run.sh"; then
    ok "census-run.sh still anchors (decision 0146 intact)"
else
    bad "census-run.sh lost its anchoring — 0146 regressed"
fi

echo "== CONTROL: the check can distinguish the two forms"
# Without this, a grep that matched nothing would report both scripts clean.
probe="$(mktemp)"
# shellcheck disable=SC2016  # the single quotes are the point: this probe must
# contain the LITERAL text `$repo_root/../hornvale-sluice-wt`, unexpanded, so
# the grep above has a known-positive to find. Expanding it would defeat the
# control entirely.
printf 'wt="${X:-$repo_root/../hornvale-sluice-wt}"\n' > "$probe"
if grep -qE "\\\$repo_root/\.\./hornvale-sluice-wt" "$probe"; then
    ok "CONTROL: the caller-relative pattern IS detected when present"
else
    bad "CONTROL FAILED: the pattern is not detectable, so the assertions above are vacuous"
fi
rm -f "$probe"

printf '\ntest-scratch-worktree-anchor.sh: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
