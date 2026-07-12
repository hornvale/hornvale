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
# serialized-floats-are-quantized-for-cross-platform-determinism). The PNG maps
# and scene/tiles are rendered per-cell views whose pixels/indices come from
# host-libm-divergent transcendentals; CI excludes those from its byte drift
# check (see ci.yml), but this script still regenerates them so a local
# rebaseline produces the full set.
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
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

echo "regenerate-artifacts: reference dumps" >&2
run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
run -p hornvale -- phonology > book/src/reference/phonology.md
run -p hornvale -- dictionary --world "$wsky" > book/src/reference/dictionary-generated.md
run -p hornvale -- proto > book/src/reference/proto-goblinoid-generated.md

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
run -p hornvale -- star-chart --world "$wsky" --out book/src/gallery/star-chart-seed-42.png \
    > book/src/gallery/star-chart-seed-42.md

echo "regenerate-artifacts: scene exports" >&2
run -p hornvale -- scene tiles --world "$wsky" > book/src/gallery/scene-tiles-seed-42.json
run -p hornvale -- scene system --world "$wsky" > book/src/gallery/scene-system-seed-42.json

echo "regenerate-artifacts: lab censuses (release)" >&2
run_release -p hornvale -- lab run studies/census-lands-drift.study.json
run_release -p hornvale -- lab run studies/census-of-the-meeting.study.json
run_release -p hornvale -- lab run studies/branches-family.study.json

echo "regenerate-artifacts: orrery ephemeris golden" >&2
run -p hornvale-scene --example ephemeris_golden > clients/orrery/testdata/ephemeris-seed-42.json

echo "regenerate-artifacts: type-audit report" >&2
run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md

echo "regenerate-artifacts: done." >&2
