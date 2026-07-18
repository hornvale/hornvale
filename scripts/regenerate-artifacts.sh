#!/usr/bin/env bash
# scripts/regenerate-artifacts.sh — regenerate every committed generated
# artifact (TOOL-15).
#
# This is the SINGLE source of truth for "how the drift-checked artifacts are
# produced." CI's "Artifacts are current" step and the local `make rebaseline`
# target both call it, so the two can never silently diverge (they used to be a
# hand-copied command list in two places).
#
# It performs GENERATION ONLY — it never asserts freshness. CI wraps this call
# with its verification tail (`release_determinism`, `type-audit check`, and
# the `git diff --exit-code` drift assertion). Locally, run it to regenerate,
# then review and commit the diff yourself:
#
#   make rebaseline        # or: bash scripts/regenerate-artifacts.sh
#   git diff               # review what moved
#
# Canonical numeric artifacts are byte-identical across platforms (floats are
# quantized at every serialization boundary — decision
# 0033). The PNG maps
# and scene/tiles are rendered per-cell views whose pixels/indices come from
# host-libm-divergent transcendentals; CI excludes those from its byte drift
# check (see ci.yml), but this script still regenerates them so a local
# rebaseline produces the full set.
set -euo pipefail

# Root from the script's own location, not `git rev-parse` — the remote gate
# runs this in an rsync'd tree that is not a git repository.
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# Intermediate world files are throwaway; their path never enters artifact
# bytes. A dedicated temp dir keeps them out of the tree.
work="$(mktemp -d "${TMPDIR:-/tmp}/hv-regen.XXXXXX")"
trap 'rm -rf "$work"' EXIT
w42="$work/hv-42.json"       # seed 42, tier-0 constant sun
wsky="$work/hv-sky.json"     # seed 42, generated sky (default)
wlocked="$work/hv-locked.json" # seed 42, tidally locked

run() { cargo run -q "$@"; }
run_release() { cargo run -q --release "$@"; }

echo "regenerate-artifacts: first light + seed-42 worlds" >&2
run -p hornvale-kernel --example first_light
run -p hornvale -- new --seed 42 --sky constant --out "$w42"
run -p hornvale -- new --seed 42 --out "$wsky"
run -p hornvale -- new --seed 42 --rotation locked --out "$wlocked"

echo "regenerate-artifacts: almanacs" >&2
run -p hornvale -- almanac --world "$w42" > book/src/gallery/almanac-seed-42.md
run -p hornvale -- almanac --world "$wsky" > book/src/gallery/almanac-seed-42-sky.md
run -p hornvale -- almanac --world "$wlocked" > book/src/gallery/almanac-seed-42-locked.md

echo "regenerate-artifacts: the book" >&2
run -p hornvale -- book > book/src/gallery/the-book.md

echo "regenerate-artifacts: explain" >&2
run -p hornvale -- explain --world "$wsky" sky > book/src/gallery/explain-seed-42-sky.md

echo "regenerate-artifacts: reference dumps" >&2
run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
run -p hornvale -- phonology > book/src/reference/phonology.md
run -p hornvale -- dictionary --world "$wsky" > book/src/reference/dictionary-generated.md
run -p hornvale -- proto > book/src/reference/proto-goblinoid-generated.md
run -p hornvale -- locale --world "$wsky" --room 1015166224 --json > book/src/reference/locale-seed-42.json
# The live-pane preamble is hand-authored framing (The Casement, decision
# 0052): the possess dump replaces the whole file, so re-emit the preamble
# here rather than losing it on every regen — it was clobbered twice by
# earlier regen runs before this step carried it.
possess_tmp="$(mktemp)"
run -p hornvale -- possess --world "$wsky" --script scripts/possession-walk.txt > "$possess_tmp"
{
    head -n 1 "$possess_tmp"
    printf '\n*(This transcript is frozen. [The live pane](./possession-live.md) derives\nthe same world in your browser — same crates, same bytes.)*\n'
    tail -n +2 "$possess_tmp"
} > book/src/gallery/possession-seed-42.md
rm -f "$possess_tmp"

# The over-time transcript (the-quickening, T4): a NEW, separate recording —
# the day-0 transcript above never advances time, so it cannot show the
# world moving. This one `wait`s across an activity phase, so a derived NPC
# departs and returns (named on `look`/`wait`'s own narration) and a `why`
# recounts its dated `agent-at` history. Wiring it here (rather than editing
# the day-0 script) is what keeps the day-0 transcript byte-identical.
possess_ot_tmp="$(mktemp)"
run -p hornvale -- possess --world "$wsky" --script scripts/possession-over-time-walk.txt > "$possess_ot_tmp"
{
    head -n 1 "$possess_ot_tmp"
    printf '\n*(This transcript is frozen too — a recording, not a live session — but\nunlike the [day-0 transcript](./possession-seed-42.md), it `wait`s: watch a\nderived NPC depart and return as sim time advances, and `why` recount its\ndated history. The world still moves only inside a possess session; a\nfreshly built world commits none of this.)*\n'
    tail -n +2 "$possess_ot_tmp"
} > book/src/gallery/possession-over-time-seed-42.md
rm -f "$possess_ot_tmp"

echo "regenerate-artifacts: gallery maps (rendered per-cell views)" >&2
run -p hornvale -- map --world "$wsky" --out book/src/gallery/elevation-seed-42.png \
    > book/src/gallery/elevation-seed-42.md
run -p hornvale -- biome-map --world "$wsky" --out book/src/gallery/biome-seed-42.png \
    > book/src/gallery/biome-seed-42.md
run -p hornvale -- biome-map --world "$wlocked" --out book/src/gallery/biome-seed-42-locked.png \
    > book/src/gallery/biome-seed-42-locked.md
run -p hornvale -- settlement-map --world "$wsky" --out book/src/gallery/settlement-seed-42.png \
    > book/src/gallery/settlement-seed-42.md
run -p hornvale -- settlement-map --world "$wlocked" --out book/src/gallery/settlement-seed-42-locked.png \
    > book/src/gallery/settlement-seed-42-locked.md
run -p hornvale -- paleo-map --world "$wsky" --out book/src/gallery/paleo-seed-42.png \
    > book/src/gallery/paleo-seed-42.md
# The sediment/carve-delta lens (Sculpting): PNG only — no committed .md
# sibling yet, so the markdown goes to /dev/null.
run -p hornvale -- map --world "$wsky" --out book/src/gallery/sediment-seed-42.png \
    --field sediment > /dev/null
run -p hornvale -- star-chart --world "$wsky" --out book/src/gallery/star-chart-seed-42.png \
    > book/src/gallery/star-chart-seed-42.md

echo "regenerate-artifacts: scene exports" >&2
run -p hornvale -- scene tiles --world "$wsky" > book/src/gallery/scene-tiles-seed-42.json
run -p hornvale -- scene tiles-region --world "$wsky" --face 0 --level 3 --ix 4 --iy 4 --samples 16 > book/src/gallery/scene-tiles-region-seed-42.json
run -p hornvale -- scene moons --world "$wsky" > book/src/gallery/scene-moons-seed-42.json
run -p hornvale -- scene neighbors --world "$wsky" > book/src/gallery/scene-neighbors-seed-42.json

# OWNER DIRECTIVE (2026-07-13): the full censuses NEVER regenerate on a
# local box unless Nathan explicitly says so. The sanctioned path is the
# AWS spot box (`make regen-remote` / scripts/aws-gate/regen-git.sh), which
# invokes this script with HV_CENSUS=1. Locally the censuses are therefore
# skipped BY DEFAULT; SKIP_CENSUS=1 (CI's fast probe path) also skips.
# Cadence: once per campaign, just before the merge to main, with warning
# given to Nathan first — census/validation coverage deliberately lags
# (the local gate stays < 5 min; rapid development is the ratified trade).
if [ "${HV_CENSUS:-0}" = 1 ] && [ "${SKIP_CENSUS:-0}" != 1 ]; then
    echo "regenerate-artifacts: lab censuses (release; HV_CENSUS=1)" >&2
    run_release -p hornvale -- lab run studies/the-census.study.json
    run_release -p hornvale -- lab run studies/census-of-the-meeting.study.json
else
    echo "regenerate-artifacts: censuses SKIPPED — census regeneration is AWS-only (make regen-remote); HV_CENSUS=1 only on explicit owner direction" >&2
fi

echo "regenerate-artifacts: type-audit report" >&2
run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md

echo "regenerate-artifacts: done." >&2
