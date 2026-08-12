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
#
# `board sync`'s own stdout/stderr are DELIBERATELY MERGED (2>&1) below, not
# suppressed the way board-render.sh's `2>/dev/null` suppresses its render:
# a rejected push here is worth a human's attention (0118 part 3's
# hostname-collision hazard), so `preflight-merge.sh` -- the caller that
# actually cares -- sees it rather than having it swallowed.
#
# WHICH IS WHY THE MISSING-`timeout` CASE IS HANDLED SEPARATELY, ahead of
# that merge: with the merge already in effect, a missing `timeout` binary
# does NOT degrade silently the way board-render.sh's does -- bash's own
# "timeout: command not found" would land on the merged stream and print,
# not vanish (measured: `PATH=/usr/bin:/bin` reproduces this exactly). So
# this checks for `timeout` FIRST and skips the sync entirely if it is
# absent, rather than attempting one that cannot actually be bounded --
# unbounded is exactly the hang risk this timeout exists to remove, so
# skipping is the safer degradation. It prints one line before doing so
# (The Beacon, task 11): the 124-timeout path below already prints on its
# own skip, and a `command -v timeout || exit 0` that said nothing was the
# one remaining way this script could skip PERMANENTLY and SILENTLY on a
# box lacking coreutils, with no signal anywhere that sync had stopped
# running at all.
set -uo pipefail

root="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null)}" || exit 0
[ -n "${root}" ] || exit 0
[ -f "${root}/tools/board/Cargo.toml" ] || exit 0

bin="${root}/tools/board/target/release/board"
[ -x "${bin}" ] || bin="${root}/tools/board/target/debug/board"
[ -x "${bin}" ] || exit 0

if ! command -v timeout >/dev/null 2>&1; then
  echo "board sync skipped: 'timeout' not found (install coreutils, e.g. 'brew install coreutils')"
  exit 0
fi

cd "${root}" || exit 0
# GIT_TERMINAL_PROMPT=0: `timeout` does not put its child in a new process
# group, so a credential prompt from a grandchild `git` process cannot be
# answered and would otherwise simply burn the full 20s budget every time.
# Failing the credential lookup immediately is exactly this script's
# fail-open convention -- a sync that cannot authenticate degrades to the
# single-box behaviour that shipped before this campaign, the same as any
# other push/fetch failure.
GIT_TERMINAL_PROMPT=0 timeout 20 "${bin}" sync "$@" 2>&1
rc=$?
if [ "${rc}" -eq 124 ]; then
  echo "board sync exceeded its 20s budget and was skipped; run \`make board-sync\`"
fi
exit 0
