#!/usr/bin/env bash
# The Beacon's transport, for call sites that must never trigger a compile
# and must never block on a hung network (`scripts/preflight-merge.sh`, and
# any future SessionStart-style caller).
#
# Mirrors scripts/board-render.sh's binary-discovery block exactly: prefer a
# prebuilt release binary, fall back to debug, and do nothing at all --
# silently -- if neither exists. A fresh worktree with no board binary built
# yet must not fail `preflight-merge.sh`, which calls this before its own
# hold-off read.
#
# NEVER FAILS THE CALLER (B6, extended to this wrapper): `board sync` itself
# already reports push/fetch failures on stderr while exiting 0 (the local
# append this session cares about has already succeeded by the time sync
# ever runs, and a non-zero exit here would make an automated caller treat a
# down network as a broken tool). The explicit `exit 0` below is not
# redundant with that: it is what keeps this SCRIPT's own exit status
# decoupled from `timeout`'s (124 on a hang), for exactly the same reason
# board-render.sh's trailing `exit 0` is not redundant with its render's.
#
# A TIMEOUT IS INCLUDED, unlike board-render.sh's local-only render: this
# script does real network I/O (a push and a fetch against `origin`), and an
# unreachable or credential-prompting remote can hang far longer than any
# local git call would. 20s is generous for the tiny ref this board pushes
# (a few dozen small posts) while still bounding a caller like
# `preflight-merge.sh` that a human is waiting on.
set -uo pipefail

root="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null)}" || exit 0
[ -n "${root}" ] || exit 0
[ -f "${root}/tools/board/Cargo.toml" ] || exit 0

bin="${root}/tools/board/target/release/board"
[ -x "${bin}" ] || bin="${root}/tools/board/target/debug/board"
[ -x "${bin}" ] || exit 0

cd "${root}" || exit 0
timeout 20 "${bin}" sync "$@" 2>&1
rc=$?
if [ "${rc}" -eq 124 ]; then
  echo "board sync exceeded its 20s budget and was skipped; run \`make board-sync\`"
fi
exit 0
