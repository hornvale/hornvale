#!/usr/bin/env bash
# scripts/gate-full-heavy.sh — run the heavy tier (the `heavy:`-tagged
# #[ignore] tests) via nextest, and nothing else that is #[ignore]d.
#
# `#[ignore]` is overloaded in this tree: it marks BOTH cost-deferred heavy
# batteries (censuses, the full pin product — greppable via the `heavy:`
# reason token, see cli/tests/heavy_tier.rs and decision 0040) AND
# genuinely-deferred tests (WIP, flaky, superseded, or a documented physics
# limitation like the single-craton hypsometry). `--run-ignored all` would
# run the latter too and be red by design, so `make gate-full` must not use
# it. This script selects ONLY the heavy tier by name, so gate-full stays a
# meaningful green/red signal.
#
# The heavy roster is discovered from the source (never hand-maintained): the
# fn on the line after each `#[ignore = "heavy:` tag. cli/tests/heavy_tier.rs
# asserts every such tag is canonical, so this grep and that guard agree.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

# The heavy tier claims the box (The Siding; decision 0081's seam rule). It
# shares ONE claim with the census: both saturate the machine, and the binding
# constraint is the machine, not the directory — there is one canonical box.
#
# The claim lives HERE rather than only in scripts/heavy-run.sh because a
# wrapper cannot guard a direct `make gate-full`, which is exactly the hole
# 0081 was written to close for the census.
#
# Two deliberate escapes:
#   - No flock (macOS ships none): proceed unserialised with a note. Campaigns
#     run on the Mac and the heavy tier runs on the canonical box, so an
#     unserialised gate-full on a dev machine is the discouraged path, not the
#     contended one. Failing here would break `make gate-full` on the Mac.
#   - An ANCESTOR already holds it (HV_CENSUS_LOCK_HELD names a live pid):
#     flock is per open-file-description, so a child re-flocking the same path
#     on a fresh fd would DEADLOCK against its own parent.
LOCK="${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
held_by="${HV_CENSUS_LOCK_HELD:-}"
if ! command -v flock >/dev/null 2>&1; then
    echo "gate-full-heavy: no flock on this host ($(uname -s)) — running unserialised." >&2
elif [ -n "$held_by" ] && kill -0 "$held_by" 2>/dev/null; then
    echo "gate-full-heavy: box already claimed by ancestor pid $held_by — proceeding" >&2
else
    exec 9>"$LOCK"
    timeout_s="${HV_HEAVY_WAIT_TIMEOUT:-2700}"
    echo "gate-full-heavy: waiting for the box claim ($LOCK; up to ${timeout_s}s) …" >&2
    wait_began=$SECONDS
    if ! flock -w "$timeout_s" 9; then
        # Bounded, so a wedged holder fails loudly instead of hanging forever.
        # Report WHO, not just that we gave up (decision 0081).
        echo "gate-full-heavy: TIMED OUT after ${timeout_s}s waiting for the box." >&2
        echo "gate-full-heavy: $(cargo run --quiet --release -p hornvale -- lab claim-status 2>/dev/null || echo 'holder unknown')" >&2
        exit 75
    fi
    waited=$((SECONDS - wait_began))
    if [ "$waited" -gt 0 ]; then
        echo "gate-full-heavy: box claimed after ${waited}s queued" >&2
    fi
    export HV_CENSUS_LOCK_HELD=$$
fi

# The heavy #[ignore] tag sits directly above its `fn`; grab the fn names.
names="$(grep -rEA1 '#\[ignore = "heavy:' --include='*.rs' . \
    | grep -oE 'fn [a-z0-9_]+' | sed 's/^fn //' | sort -u)"
[ -n "$names" ] || { echo "gate-full-heavy: no heavy-tier tests found" >&2; exit 1; }

# Guard against layout drift: if a heavy tag is ever NOT directly above its
# fn (e.g. an intervening #[allow(...)]), grep -A1 grabs the wrong line, the
# name is lost, and the test would silently vanish from gate-full. Assert one
# fn name per heavy tag.
tag_count="$(grep -rc '#\[ignore = "heavy:' --include='*.rs' . | awk -F: '{s+=$2} END{print s+0}')"
name_count="$(printf '%s\n' "$names" | grep -c .)"
if [ "$tag_count" != "$name_count" ]; then
    echo "gate-full-heavy: $tag_count heavy: tags but $name_count fn names extracted —" >&2
    echo "  a heavy #[ignore] tag is not directly above its fn (intervening attribute?)," >&2
    echo "  or two heavy tests share a name. Fix the layout; gate-full must not silently" >&2
    echo "  skip a heavy test." >&2
    exit 1
fi

# Build a nextest filterset: test(/name$/) | ... — end-anchored regex, so it
# matches both bare integration-test names and module-qualified unit-test
# names (e.g. runner::tests::parallel_run_matches_sequential). `test(=name)`
# would miss the module-qualified ones.
filter="$(printf '%s\n' "$names" | awk '{printf "%stest(/%s$/)", sep, $0; sep=" | "}')"

echo "== heavy tier: $(printf '%s\n' "$names" | grep -c .) tests"
# --run-ignored only: run the ignored tests; the filterset restricts to heavy.
#
# `--profile heavy` is load-bearing and the reason is written out in
# .config/nextest.toml: it turns fail-fast OFF. Without it this command
# inherits [profile.default], and ONE red test cancels every battery still
# running — the 2026-08-05 run lost its last long battery to SIGTERM 14,143 s
# in, then reported it as a failure it had never actually run. The Siding
# diagnosed this and fixed `make ci`; the heavy tier's own path did not get
# the fix until The Scatter. A tier where a deliberate red is normal (a
# preregistered falsification pinned as a witness) must never be fail-fast.
cargo nextest run --profile heavy --workspace --run-ignored only -E "$filter"
