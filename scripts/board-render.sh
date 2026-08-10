#!/usr/bin/env bash
# The Cairn's ambient render, for the SessionStart hook.
#
# MUST NEVER break or slow a session (D7): every failure path is silent, and
# the whole thing is best-effort. It prints nothing when the board is empty.
#
# DELIBERATELY SYNCHRONOUS, unlike this hook's `git fetch` sibling. `async:
# true` is documented only as "runs in the background without blocking"
# (fire-and-forget); nothing documents whether an async hook's stdout still
# reaches the session's context, and the likely answer is that it does not.
# Going async would probably make the board silently never render -- the
# read seam dead with no signal -- which is worse than a small, bounded
# synchronous cost. So this stays synchronous, and the timeout below is what
# keeps that safe.
#
# THE TIMEOUT IS DELIBERATELY TIGHT (2s), not generous. Measured cost: ~0.57s
# for `board render` with only two posts on the board (five runs). The cost is
# subprocess spawns, ~30-38 ms each on this box, and it grows on BOTH axes --
# the earlier claims that it grows "with post count" (a subprocess per author)
# and, in the spec, "with authors, not posts" were each half right and each
# wrong about the mechanism. What the code actually does, per render:
#
#   * one `git cat-file` per post at the tip                     (per POST)
#   * one `ps` per claim naming this host                        (per CLAIM)
#   * two `git rev-parse` per post whose author's branch does
#     NOT resolve -- `LiveContext::probe` memoises an author only
#     by inserting it into live_branches or merged_branches, so an
#     unresolved author is never cached and is re-probed on every
#     render, forever                                     (per UNRESOLVED POST)
#   * plus a fixed ~10: hostname, the tip resolve, ls-tree, the log
#     walk, the changed-paths diff, and the cursor read
#
# So it is ~38 ms/post at best and ~115 ms/post once authors stop resolving --
# and `technique` posts are durable by design while their branches are torn
# down, so the unresolved term grows toward the post count. Against a 2 s
# ceiling with ~0.4 s fixed cost, the render starts timing out somewhere
# between 15 and 40 posts. A board that has grown slow must not silently slow
# every session start; 2s is comfortable headroom today and a real ceiling
# once the board has more history. On timeout, print a one-line notice rather
# than nothing -- the failure this guards against is not "the render is slow"
# (bounded, fine) but "the render is slow AND nobody is told," which degrades
# to no board, silently: the exact failure mode this whole campaign exists to
# close.
#
# Depends on GNU `timeout` (present via Homebrew coreutils on this box; not
# part of stock macOS). This degrades SAFELY, not silently-dangerously, when
# absent. The mechanism, stated exactly, because an earlier version of this
# comment credited a trailing `|| true` that no longer exists on that line --
# and a comment naming a safety mechanism that is not there invites deleting
# the one that is:
#
#   * this script sets `-uo pipefail` and NOT `-e`, so a failing command does
#     not abort it;
#   * the `2>/dev/null` on the `timeout` line suppresses bash's own "command
#     not found" message;
#   * a missing `timeout` yields rc 127, which is not 124, so no notice prints;
#   * and the explicit `exit 0` at the end is what guarantees the hook sees
#     success. It is NOT redundant -- without it the script's status is that
#     of the last command, i.e. the render's.
#
# (There is a surviving `|| true`, but it is in `.claude/settings.json`'s hook
# command, wrapping the call to this script -- belt and braces, not this
# script's own mechanism.)
set -uo pipefail

root="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null)}" || exit 0
[ -n "${root}" ] || exit 0
[ -f "${root}/tools/board/Cargo.toml" ] || exit 0

# Use a prebuilt binary if there is one; never trigger a compile at session
# start, which would cost seconds on a cold target.
bin="${root}/tools/board/target/release/board"
[ -x "${bin}" ] || bin="${root}/tools/board/target/debug/board"
[ -x "${bin}" ] || exit 0

cd "${root}" || exit 0
timeout 2 "${bin}" render 2>/dev/null
rc=$?
if [ "${rc}" -eq 124 ]; then
  # shellcheck disable=SC2016 # single-quoted deliberately: no expansion wanted
  echo 'board render exceeded its 2s budget and was skipped; run `make board`'
fi
exit 0
