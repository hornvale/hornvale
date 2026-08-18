#!/usr/bin/env bash
# scripts/lane-outboard.sh — the `outboard` set: the fast suites that guard
# this repo from OUTSIDE the cargo workspace, and which nothing ran before
# The Staff.
#
# DIRECTION: this runs the three out-of-workspace test suites and reports
# every failure. It is blind to anything inside the workspace — the chamber's
# own `gate` phase owns that.
#
# seam-guard is DELIBERATELY NOT HERE. It shipped here first, but Task 8's
# own measurement showed its 7 call sites cost 853.284 s (the
# `lane:seam-guard` row in docs/timings.md) against well under a minute for
# the three suites below combined — 99.8% of the set's 855.222 s total (the
# `lane:outboard` row) — and cost aside, what it guards (which functions no
# test pins)
# moves on a campaign cadence, not a per-plan-stage-boundary one, matching
# the `outboard`/`gate`/`artifacts`/`clients` sets here. It now lives at its
# own `campaign`-rung row in scripts/lane-sets.tsv; see the Makefile's
# `seam-guard` target for why it refuses on an unclean tree (by design, not
# a defect to route around).
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
# THE QUEUE'S OWN SUITE, which until now was run by NOBODY. `test-sluice.sh`
# was referenced only from prose — no make target, no set, no phase — so the
# 151 property tests guarding the merge queue ran only when someone
# remembered. That is not a theoretical gap: the coalescing defect this line
# ships alongside (exit 128 read as exit 1) survived because the suite that
# would have hosted its test was never executed after Task 12 wrote it.
#
# It belongs HERE rather than in a heavier set because it is genuinely fast —
# measured 17.86 s wall on the canonical box, against `outboard`'s own 5.5-20 s
# — and because the queue is the CAS/append path, where a bug means silent
# write loss rather than a loud failure. Same reasoning that put `tools/board`
# on this line.
#
# It is safe to run INSIDE a chamber phase despite driving a chamber of its
# own: every path it touches is overridden to scratch (HV_SLUICE_REPO_ROOT,
# HV_SLUICE_WORKTREE, HV_CENSUS_CLAIM_PATH, HV_CENSUS_LOCK), so it never takes
# the real claim the enclosing run is already holding. It SKIPs cleanly on a
# host without flock, which is why it costs nothing on a Mac.
run "sluice queue"     bash scripts/test-sluice.sh
# The block allocator's own suite, here for the reason the queue's is: it is
# the only thing that runs it, it is fast, and an allocator nobody tests is an
# allocator that hands two campaigns the same range without anyone noticing.
run "decision blocks"  bash scripts/test-decision-blocks.sh
# THE SHELL LINT, for the same reason and with the proof attached. The
# `make shellcheck` target was in the Makefile's .PHONY list and NOWHERE
# else — no gate, no set, no hook ran it — and it was RED on `main` when this
# line was written (SC2119 at two call sites, unnoticed for as long as it took
# to write them).
#
# (A comment line here must not START with the linter's own name followed by a
# space: that is the syntax for an inline directive, and shellcheck fails the
# whole file with SC1073 when it cannot parse one. Learned immediately.)
# A lint nothing runs is a lint that reports on whatever the last person to
# type it happened to see. 8.16 s, against this set's ~24 s.
#
# `scripts/**` is the connective tissue of every gate, the census, and this
# queue, and none of it is covered by `cargo clippy` — the workspace lint
# stops at the Rust boundary, so shell is the one language here with no
# automatic checker at all.
run "shellcheck"       make --no-print-directory shellcheck

if [ "$fails" -ne 0 ]; then
    echo "outboard: $fails suite(s) failed" >&2
    exit 1
fi
echo "outboard: all suites passed"
