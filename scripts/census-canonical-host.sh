#!/usr/bin/env bash
# scripts/census-canonical-host.sh — the guard that keeps census goldens
# authored on ONE machine (decision 0063).
#
# Why this exists as code rather than prose. 0063 says "this box is the single
# canonical platform", and every script and plan that repeats it was written ON
# that box — so on any OTHER machine the same sentence reads as if the machine
# you happen to be sitting at is canonical. Nothing enforced it: setting
# HV_CENSUS=1 anywhere ran the census and rewrote the committed goldens.
#
# That matters because the machines are NOT byte-identical. 0063 measured it:
# on ~0.1% of census values — discrete-count metrics like `divergence-magnitude`,
# where a count is settled by a comparison in the COMPUTE path, upstream of the
# quantize-at-emit boundary (0033) — two boxes disagree by one unit.
# Quantization absorbs last-ULP float noise at serialization; it cannot un-flip
# a count already decided. So a census run on the wrong machine does not fail
# loudly, it commits ~1-in-1000 wrong values that then drift-check green
# forever.
#
# Fails CLOSED: if the hostname does not match, no census runs. If the
# canonical box is ever renamed or replaced, change the hostname in
# census-canonical-host.txt — one line, in version control, visible in
# review.
#
# `windows/lab/src/census_guard.rs` (the Rust-side guard on `lab run`'s
# publish path, cli/src/main.rs's `cmd_lab_run`, decision-0063 C1) reads the
# SAME file, baked in at compile time via `include_str!`. One file, two
# readers, so the canonical hostname is never hardcoded twice.

# The short hostname (`hostname -s`) of the one box that may author census
# goldens. Compared case-insensitively.
#
# The host file is overridable so the guard's own REFUSAL path is testable.
# Without this the only way to exercise a refusal is to be on the wrong
# machine, which means the refusal branch is never tested on the machine that
# has to trust it. `scripts/test-lane.sh` drives it.
#
# TEST-ONLY. `windows/lab/src/census_guard.rs` bakes the canonical hostname
# in at COMPILE TIME via `include_str!` on census-canonical-host.txt itself
# (see that file's module doc) — it has no environment to read, so
# HV_CANONICAL_HOST_FILE moves only this shell-side guard. Set it in a real
# run and the two layers disagree about what "canonical" means, which is
# exactly the silent cross-host divergence decision 0063 exists to prevent.
CANONICAL_HOST_FILE="${HV_CANONICAL_HOST_FILE:-$(dirname "${BASH_SOURCE[0]}")/census-canonical-host.txt}"
# `2>/dev/null || true`: a missing/unreadable file must not crash here with a
# bare `cat` stack trace — it must fall through to the empty-string check
# below and get the guard's OWN refusal message instead.
CANONICAL_CENSUS_HOST="$(cat "$CANONICAL_HOST_FILE" 2>/dev/null || true)"

# Fail closed if the canonical host could not be determined AT ALL — the file
# is missing, unreadable, or present but empty. This is STRUCTURAL, not
# incidental: without it, "unknown" is an empty string, and an empty string
# only fails to compare-equal to `here_lc` by accident of `hostname` also
# never returning empty. Every caller of this guard happens to run under
# `set -euo pipefail` today, which is what turned the OLD bare-`cat` crash
# above into a hard stop — but that was the CALLER's discipline, not this
# guard's, and it would take exactly one future caller without it for "I
# don't know" to silently read as "yes". This repo has already shipped that
# shape once: an empty nextest.rc read as green until gate-run started
# treating missing/empty/non-numeric as failure.
#
# Checked ONCE, here, at the point both entry points share, so neither
# require_canonical_census_host nor require_canonical_host below has to
# re-derive it.
if [ -z "$CANONICAL_CENSUS_HOST" ]; then
    cat >&2 <<EOF
census-canonical-host: REFUSING — the canonical host could not be determined
from '$CANONICAL_HOST_FILE' (missing, unreadable, or empty after reading).

If this is a real run: check that scripts/census-canonical-host.txt exists,
is readable, and contains a hostname (one line, in version control).

If this is a test: HV_CANONICAL_HOST_FILE pointed somewhere that could not be
read as a non-empty hostname.
EOF
    return 1
fi

# Exit 0 on the canonical box; otherwise print why and exit 1.
#
# Optional $1 is the JOB KIND, for the refusal text only: omitted (or
# "census") keeps the census wording verbatim; "heavy" explains the heavy
# tier's own reason for being host-locked and suggests heavy-run.sh instead of
# census-run.sh. The HOST RULE is identical either way — only the prose that
# tells the reader what to do next differs (The Siding).
require_canonical_census_host() {
    local here here_lc want_lc job
    job="${1:-census}"
    here="$(hostname -s 2>/dev/null || hostname)"
    # `tr`, not ${var,,}: bash 3.2 ships on macOS and lacks case expansion,
    # and this guard has to behave predictably on the machine it exists to
    # stop.
    here_lc="$(printf '%s' "$here" | tr '[:upper:]' '[:lower:]')"
    want_lc="$(printf '%s' "$CANONICAL_CENSUS_HOST" | tr '[:upper:]' '[:lower:]')"

    if [ "$here_lc" = "$want_lc" ]; then
        return 0
    fi

    if [ "$job" = "heavy" ]; then
        cat >&2 <<EOF
heavy: REFUSING to run on '$here' ($(uname -s)).

The heavy tier may only run on '$CANONICAL_CENSUS_HOST' (The Siding; decisions
0063/0079). It is an AUTHORING path, not just an expensive one: three of its
tests write committed artifacts (the-history, the-sounding, occupancy.csv), and
census_fixtures_match_a_probe_of_live_seeds compares a LIVE probe against
census fixtures authored on that box. The boxes are not byte-identical:
~0.1% of discrete-count metrics differ by one unit, decided upstream of
quantize-at-emit, so a run here would commit values that silently disagree
with the canonical ones and then drift-check green forever.

Trigger the run on the canonical box instead — push your branch first, then:

  make heavy-remote REF=<full-sha>
EOF
        return 1
    fi

    cat >&2 <<EOF
census: REFUSING to run on '$here' ($(uname -s)).

Census goldens may only be authored on '$CANONICAL_CENSUS_HOST' (decision 0063).
The boxes are not byte-identical: ~0.1% of discrete-count census metrics differ
by one unit, decided upstream of quantize-at-emit, so a run here would commit
values that silently disagree with the canonical ones and then drift-check
green forever.

Trigger the run on the canonical box instead — push your branch first, then:

  ssh $CANONICAL_CENSUS_HOST 'cd ~/Projects/hornvale && \\
    HV_CENSUS_WORKTREE=canonical \\
    HV_CENSUS_REF=<full-sha> \\
    scripts/census-run.sh'

Pass a SHA rather than a branch name: HV_CENSUS_REF feeds 'reset --hard', which
can otherwise land on a stale LOCAL branch of that name on the canonical box.
Verify HEAD there matches your SHA before trusting the output.

If '$CANONICAL_CENSUS_HOST' is no longer the canonical box, change
CANONICAL_CENSUS_HOST in scripts/census-canonical-host.sh — deliberately, in a
reviewable commit.
EOF
    return 1
}

# Refuse unless this is the canonical box, naming the dispatch line for `$1`
# (a set name from scripts/lane-sets.tsv, or a gate name).
#
# DIRECTION THIS ENFORCES: `this host is the canonical one`. It says nothing
# about whether the SET is one that belongs on the lane — `lane-dispatch.sh`
# owns that, reading the roster.
#
# `require_canonical_census_host` above is untouched and keeps its own two
# callers (census-run.sh, heavy-run.sh) and its own carefully-worded prose.
# This is the general entry point The Staff's lane sets dispatch through.
require_canonical_host() {
    local set_name here here_lc want_lc
    set_name="${1:-gate}"
    here="$(hostname -s 2>/dev/null || hostname)"
    here_lc="$(printf '%s' "$here" | tr '[:upper:]' '[:lower:]')"
    want_lc="$(printf '%s' "$CANONICAL_CENSUS_HOST" | tr '[:upper:]' '[:lower:]')"
    if [ "$here_lc" = "$want_lc" ]; then
        return 0
    fi
    cat >&2 <<EOF
$set_name: REFUSING to run on '$here' ($(uname -s)).

Since The Staff, every check above the commit gate runs in one strictly serial
lane on '$CANONICAL_CENSUS_HOST'. Running it here would produce a verdict on a
contended, non-canonical box — and the Macs saturate at cpu_ratio 8.25-8.50 on
ten cores, so a second concurrent run roughly doubles both.

Locally you may run:   make gate-commit

Push, then dispatch:   make gate-stage     REF=<full-sha>
                       make sluice BRANCH=<branch> REF=<full-sha>
                       make lane SET=$set_name REF=<full-sha>

A SHA, not a branch name: the ref feeds 'reset --hard', which can land on a
stale local branch of that name over there.

There is deliberately no force override (spec 2.1). If '$CANONICAL_CENSUS_HOST'
is no longer the canonical box, change scripts/census-canonical-host.txt — one
line, in version control, visible in review.
EOF
    return 1
}
