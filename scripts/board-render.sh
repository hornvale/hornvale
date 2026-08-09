#!/usr/bin/env bash
# The Cairn's ambient render, for the SessionStart hook.
#
# MUST NEVER break or slow a session (D7): every failure path is silent, and
# the whole thing is best-effort. It prints nothing when the board is empty.
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
timeout 5 "${bin}" render 2>/dev/null || true
exit 0
