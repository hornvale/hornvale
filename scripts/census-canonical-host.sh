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
# canonical box is ever renamed or replaced, change CANONICAL_CENSUS_HOST
# below — one line, in version control, visible in review.

# The short hostname (`hostname -s`) of the one box that may author census
# goldens. Compared case-insensitively.
CANONICAL_CENSUS_HOST="lefford"

# Exit 0 on the canonical box; otherwise print why and exit 1.
require_canonical_census_host() {
    local here here_lc want_lc
    here="$(hostname -s 2>/dev/null || hostname)"
    # `tr`, not ${var,,}: bash 3.2 ships on macOS and lacks case expansion,
    # and this guard has to behave predictably on the machine it exists to
    # stop.
    here_lc="$(printf '%s' "$here" | tr '[:upper:]' '[:lower:]')"
    want_lc="$(printf '%s' "$CANONICAL_CENSUS_HOST" | tr '[:upper:]' '[:lower:]')"

    if [ "$here_lc" = "$want_lc" ]; then
        return 0
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
