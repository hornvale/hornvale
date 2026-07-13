#!/usr/bin/env bash
# scripts/gate-full-heavy.sh — run the heavy tier (the `heavy:`-tagged
# #[ignore] tests) via nextest, and nothing else that is #[ignore]d.
#
# `#[ignore]` is overloaded in this tree: it marks BOTH cost-deferred heavy
# batteries (censuses, the full pin product — greppable via the `heavy:`
# reason token, see cli/tests/heavy_tier.rs and decision 0027) AND
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

# The heavy #[ignore] tag sits directly above its `fn`; grab the fn names.
names="$(grep -rEA1 '#\[ignore = "heavy:' --include='*.rs' . \
    | grep -oE 'fn [a-z0-9_]+' | sed 's/^fn //' | sort -u)"
[ -n "$names" ] || { echo "gate-full-heavy: no heavy-tier tests found" >&2; exit 1; }

# Build a nextest filterset: test(/name$/) | ... — end-anchored regex, so it
# matches both bare integration-test names and module-qualified unit-test
# names (e.g. runner::tests::parallel_run_matches_sequential). `test(=name)`
# would miss the module-qualified ones.
filter="$(printf '%s\n' "$names" | awk '{printf "%stest(/%s$/)", sep, $0; sep=" | "}')"

echo "== heavy tier: $(printf '%s\n' "$names" | grep -c .) tests"
# --run-ignored only: run the ignored tests; the filterset restricts to heavy.
cargo nextest run --workspace --run-ignored only -E "$filter"
