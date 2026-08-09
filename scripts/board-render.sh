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
# for `board render` with only two posts on the board (five runs), and it
# grows with post count -- the render spawns a subprocess per distinct
# author plus `hostname`, `ps`, and a diff. A board that has grown slow must
# not silently slow every session start; 2s is comfortable headroom today
# and a real ceiling once the board has more history. On timeout, print a
# one-line notice rather than nothing -- the failure this guards against is
# not "the render is slow" (bounded, fine) but "the render is slow AND
# nobody is told," which degrades to no board, silently: the exact failure
# mode this whole campaign exists to close.
#
# Depends on GNU `timeout` (present via Homebrew coreutils on this box; not
# part of stock macOS). This degrades SAFELY, not silently-dangerously, when
# absent: bash's "command not found" (exit 127) is swallowed by the `2>/dev/
# null` on that exact line plus the trailing `|| true`, so the render simply
# never fires -- no hang. Only the feature goes inert; the safety property
# holds either way.
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
