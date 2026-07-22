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

echo "regenerate-artifacts: the chorus study (C4/LANG-41, 50 seeds; live, not a census)" >&2
run_release -p hornvale -- lab run studies/the-chorus.study.json

echo "regenerate-artifacts: explain" >&2
run -p hornvale -- explain --world "$wsky" sky > book/src/gallery/explain-seed-42-sky.md

echo "regenerate-artifacts: reference dumps" >&2
run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
run -p hornvale -- concepts --manifest > book/src/reference/concept-manifest-generated.md
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

# The over-time transcript (the-quickening, T4; the-wanting, T4): a NEW,
# separate recording — the day-0 transcript above never advances time, so it
# cannot show the world moving. This one `wait`s across a full drive cycle,
# so a derived NPC's homeostatic thirst rises and is satisfied (narrated by
# `wait`, felt directly through `needs`, and recounted with its own reason
# by `why`). Wiring it here (rather than editing the day-0 script) is what
# keeps the day-0 transcript byte-identical.
#
# THE CONFLUENCE (settlement condensation re-pointed at the real river
# network): this world's flagship settlement now sits directly on fresh
# water, so the NPC drinks in place rather than walking to it — `why`
# recounts a drink, not a journey. Not every settlement's fate (condensation
# lands most, not all, towns on the river network — a real, measured
# fraction, not every seed/settlement), but this world's own flagship
# settlement's real, measured outcome.
possess_ot_tmp="$(mktemp)"
run -p hornvale -- possess --world "$wsky" --script scripts/possession-over-time-walk.txt > "$possess_ot_tmp"
{
    head -n 1 "$possess_ot_tmp"
    printf '\n*(This transcript is frozen too — a recording, not a live session — but\nunlike the [day-0 transcript](./possession-seed-42.md), it `wait`s across a\nfull homeostatic drive cycle: watch a derived NPC grow thirsty and\nsatisfy it — narrated by `wait`, felt directly through `needs`, and\nrecounted with its own reason by `why`. This settlement condenses\ndirectly onto fresh water (settlements-near-rivers): the NPC drinks in\nplace rather than walking to it, so `why` recounts a drink, not a\njourney — not every settlement'"'"'s fate (condensation lands most, not\nall, towns on the river network), but this world'"'"'s own flagship\nsettlement'"'"'s real, measured outcome. The world still moves only\ninside a possess session; a freshly built world commits none of this.)*\n'
    tail -n +2 "$possess_ot_tmp"
} > book/src/gallery/possession-over-time-seed-42.md
rm -f "$possess_ot_tmp"

# The legibility surface (living-community, T7): a real seed-42 site read back
# off the ledger as prose — its stratigraphy of occupation layers plus the
# derived flesh in the present-day grass. Cell 36918 is a genuine abandoned
# clearing on this world: a bugbear lineage that clung to the same ground
# across five generations, dwindling as the ice crept down, until the last few
# walked away and left a child's doll behind. The framing line below is
# hand-authored (the render replaces the file body, so re-emit it here); the
# fenced block is the `history` verb's exact, drift-checked output.
echo "regenerate-artifacts: the legibility surface (a site's deep history)" >&2
{
    printf '# An Abandoned Clearing — Seed 42\n\n'
    printf 'A site read back out of the ledger by the `history` verb: the stratigraphy\n'
    printf 'of every people that ever settled one cell, oldest layer deepest, and the\n'
    printf 'derived flesh — the structures they raised, the residue in the grass\n'
    printf 'today. Nothing here replays the deep-history bake; it is all a\n'
    printf '*present-as-query* over committed occupation facts, with the flesh\n'
    printf '(structures, residue) derived on demand and never committed. This is a\n'
    printf 'real clearing on the world of seed 42 — cell 36918 — where a bugbear\n'
    printf 'lineage returned five times over two centuries, smaller each time, as the\n'
    printf 'glaciers advanced, until the ice won and a doll was left in the grass.\n\n'
    printf '```text\n'
    run -p hornvale -- history --world "$wsky" --site 36918
    printf '```\n'
} > book/src/gallery/history-seed-42.md

# The transport topology's legibility surface (The Connection Graph, T6): two
# real seed-42 sites read off the derived ConnectionGraph as prose, plus the
# world-level reachability overview. Cell 13980 is this world's flagship
# settlement -- inside the largest connected region, reached only by land
# routes here. Cell 28435 sits on a *different* landmass (a real, separate
# region under natural travel) and shows both a sea-lane and land routes at
# once, so the page demonstrates every edge kind the graph derives. Framing
# lines are hand-authored (the render replaces the file body, so re-emit
# them here); the fenced blocks are the `connections` verb's exact,
# drift-checked output.
echo "regenerate-artifacts: the legibility surface (the transport topology)" >&2
{
    printf '# The Transport Topology — Seed 42\n\n'
    printf 'The connection graph'\''s legibility surface: a site'\''s natural sea-lanes and\n'
    printf 'overland routes, and which of the world'\''s naturally-connected regions it\n'
    printf 'belongs to, read off the `connections` verb. Nothing here is authored\n'
    printf 'infrastructure -- a "route" is always a natural corridor the terrain and\n'
    printf 'currents make easy, never a built road (see `EdgeKind`). The graph itself\n'
    printf 'is purely derived (no epoch, no seed draw): the same world always yields\n'
    printf 'the same topology.\n\n'
    printf '## A well-linked capital\n\n'
    printf 'The flagship settlement, on the world'\''s largest connected landmass. Its\n'
    printf 'own overland routes reach two neighboring settlements directly.\n\n'
    printf '```text\n'
    run -p hornvale -- connections --world "$wsky" --site 13980
    printf '```\n\n'
    printf '## A hub on a different shore\n\n'
    printf 'Cell 28435 sits on a *separate* landmass under natural travel -- close\n'
    printf 'enough to its neighbors to reach several by both sea-lane and land route,\n'
    printf 'but with no natural corridor at all bridging it back to the flagship'\''s\n'
    printf 'larger region.\n\n'
    printf '```text\n'
    run -p hornvale -- connections --world "$wsky" --site 28435
    printf '```\n\n'
    printf '## The world, in sum\n\n'
    printf 'The world-level reachability summary: how many real regions natural\n'
    printf 'travel divides this world into, the largest, and the rest.\n\n'
    printf '```text\n'
    run -p hornvale -- connections --world "$wsky" --overview
    printf '```\n'
} > book/src/gallery/connections-seed-42.md

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
run -p hornvale -- map --world "$wsky" --out book/src/gallery/column-seed-42.png \
    --field column > book/src/gallery/column-seed-42.md
run -p hornvale -- star-chart --world "$wsky" --out book/src/gallery/star-chart-seed-42.png \
    > book/src/gallery/star-chart-seed-42.md

echo "regenerate-artifacts: scene exports" >&2
run -p hornvale -- scene tiles --world "$wsky" > book/src/gallery/scene-tiles-seed-42.json
run -p hornvale -- scene tiles-region --world "$wsky" --face 0 --level 3 --ix 4 --iy 4 --samples 16 > book/src/gallery/scene-tiles-region-seed-42.json
run -p hornvale -- scene moons --world "$wsky" > book/src/gallery/scene-moons-seed-42.json
run -p hornvale -- scene neighbors --world "$wsky" > book/src/gallery/scene-neighbors-seed-42.json
run -p hornvale -- scene eclipses --world "$wsky" --from 0 --until 2000 > book/src/gallery/scene-eclipses-seed-42.json

# Censuses are still opt-in (HV_CENSUS=1) so the everyday gate stays fast:
# skipped BY DEFAULT, and SKIP_CENSUS=1 (CI's fast probe path) also skips.
# But since decision 0063 (The Local Census cut the per-world cost ~285 → ~8
# CPU-s) the sanctioned refresh is LOCAL: run `HV_CENSUS=1 bash
# scripts/regenerate-artifacts.sh` once per campaign at the pre-merge close —
# the full ~2000-world census takes ~7 min — keeping the fixtures current with
# main instead of lagging. `make regen-remote` (the AWS box) is ABANDONED —
# this box is the single canonical platform (AWS differs on ~0.1% of
# discrete-count metrics, so it can't be a parallel reference; supersedes the
# AWS-only mandate of 0046, decision 0063).
if [ "${HV_CENSUS:-0}" = 1 ] && [ "${SKIP_CENSUS:-0}" != 1 ]; then
    echo "regenerate-artifacts: lab censuses (release; HV_CENSUS=1; ~7 min local)" >&2
    run_release -p hornvale -- lab run studies/the-census.study.json
    run_release -p hornvale -- lab run studies/census-of-the-meeting.study.json
else
    echo "regenerate-artifacts: censuses SKIPPED (HV_CENSUS=1 to refresh; ~7 min local since decision 0063)" >&2
fi

echo "regenerate-artifacts: type-audit report" >&2
run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md

echo "regenerate-artifacts: done." >&2
