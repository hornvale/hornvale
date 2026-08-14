#!/usr/bin/env bash
# scripts/lane-outboard.sh — the `outboard` set: the suites that guard this
# repo from OUTSIDE the cargo workspace, and which nothing ran before The Staff.
#
# DIRECTION: this runs the four out-of-workspace suites and reports every
# failure. It is blind to anything inside the workspace — `make gate-stage`'s
# `gate` set owns that.
#
# NOT fail-fast, deliberately: these are independent suites and a reader wants
# the whole picture in one pass. `expensive runs emit their own evidence`.
#
# `env -u GIT_DIR -u GIT_INDEX_FILE` on the board suite is LOAD-BEARING, not
# defensive. Git exports those to hooks and to anything it invokes, and from a
# linked worktree they are absolute paths into the real repository; GIT_DIR
# outranks `git -C`. Without the scrub the board's hermetic-looking tests
# operate on the developer's own checkout — `git init` re-initialised it as
# BARE, `git config` overwrote user.name, and a loose-ref write broke `git
# fetch` repository-wide. See scripts/CLAUDE.md.
set -uo pipefail
root="$(git rev-parse --show-toplevel)"
cd "$root" || exit 1
fails=0
run() {
    echo "== outboard: $1"
    shift
    if "$@"; then echo "   ok"; else echo "   FAILED" >&2; fails=$((fails+1)); fi
}

run "tools/board"      env -u GIT_DIR -u GIT_INDEX_FILE cargo test --manifest-path tools/board/Cargo.toml
run "tools/digest"     cargo test --manifest-path tools/digest/Cargo.toml
run "tools/type-audit" cargo test --manifest-path tools/type-audit/Cargo.toml
run "seam-guard"       make --no-print-directory seam-guard

if [ "$fails" -ne 0 ]; then
    echo "outboard: $fails suite(s) failed" >&2
    exit 1
fi
echo "outboard: all suites passed"
