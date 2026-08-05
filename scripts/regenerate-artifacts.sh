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

# CENSUS HOST GUARD, hoisted to the top: with HV_CENSUS=1 this script writes
# the committed census goldens, which only the canonical box may author
# (decision 0063). Checked BEFORE the ~4 minutes of other regeneration, so a
# wrong-machine run is refused in a second rather than after the work.
if [ "${HV_CENSUS:-0}" = 1 ] && [ "${SKIP_CENSUS:-0}" != 1 ]; then
    # shellcheck source=scripts/census-canonical-host.sh
    . "$(dirname "$0")/census-canonical-host.sh"
    require_canonical_census_host || exit 1

    # Serialize with any other heavy run on this box (decision 0081). This
    # script is one of three entry points that write census goldens and was
    # the only unguarded one a doc told you to use.
    #
    # RE-ENTRANCY IS NOT OPTIONAL: `flock` is per open-file-description, so if
    # census-run.sh already holds this lock and we re-flock the same path on a
    # fresh fd, we DEADLOCK against our own parent — and under a bounded wait
    # that means hanging the box for the full timeout. An ancestor that says
    # it holds the lock, and is still alive, means "already serialized".
    if [ -z "${HV_CENSUS_LOCK_HELD:-}" ] || ! kill -0 "${HV_CENSUS_LOCK_HELD}" 2>/dev/null; then
        exec 9>"${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
        census_timeout_s="${HV_CENSUS_WAIT_TIMEOUT:-2700}"
        echo "regenerate-artifacts: waiting for the census lock (up to ${census_timeout_s}s) …" >&2
        if ! flock -w "$census_timeout_s" 9; then
            echo "regenerate-artifacts: TIMED OUT after ${census_timeout_s}s waiting for the census lock." >&2
            exit 75
        fi
        export HV_CENSUS_LOCK_HELD=$$
    fi
fi


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
    # Both transcripts start at day 0, so `possess`'s own H1 is identical for
    # the two pages (The Running Head). Override it here rather than teaching
    # `possess` about the book's page layout: the day-0 page above keeps the
    # command's real heading, and only this page — which is defined by the
    # time it covers, not the day it opens on — is retitled.
    printf '# A Possession of Seed 42 — over time\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf '\n*(This transcript is frozen too — a recording, not a live session — but\nunlike the [day-0 transcript](./possession-seed-42.md), it `wait`s across a\nfull homeostatic drive cycle: watch a derived NPC grow thirsty and\nsatisfy it — narrated by `wait`, felt directly through `needs`, and\nrecounted with its own reason by `why`. This settlement condenses\ndirectly onto fresh water (settlements-near-rivers): the NPC drinks in\nplace rather than walking to it, so `why` recounts a drink, not a\njourney — not every settlement'"'"'s fate (condensation lands most, not\nall, towns on the river network), but this world'"'"'s own flagship\nsettlement'"'"'s real, measured outcome. The world still moves only\ninside a possess session; a freshly built world commits none of this.)*\n'
    tail -n +2 "$possess_ot_tmp"
} > book/src/gallery/possession-over-time-seed-42.md
rm -f "$possess_ot_tmp"

# The legibility surface (living-community, T7): a real seed-42 site read back
# off the ledger as prose — its stratigraphy of occupation layers plus the
# derived flesh in the present-day grass. The framing paragraph below is
# hand-authored; the fenced block is the `history` verb's exact, drift-checked
# output. THE TWO MUST AGREE. `HISTORY_SITE` is the single source of the cell
# id for both, and `cli/tests/docs_consistency.rs` asserts that the id named in
# the prose is the one the block reports and that the block is not empty —
# because they silently disagreed once. The moving-sea epoch (The Sundering)
# emptied the previously-pinned cell 36918 while its hand-authored paragraph
# went on describing a lineage that no longer existed there; the drift check
# passed throughout, since the *generated* half was current.
#
# The Contour (position-aware conflict, decision 0096): the same class of
# drift recurred a second time. The paragraph below IS the hand-authored
# half, embedded here rather than typed directly into the committed .md —
# editing the committed file alone (as a merge-reconciliation pass once did)
# does not survive the next `make rebaseline`, which re-emits this exact
# text. Fix drift HERE, not in the .md file, or the fix is silently undone by
# the next regen.
#
# The Contour epoch v2 (history/bake/v2, the BAKE label bump): a THIRD
# occurrence of the same drift class, from the label bump alone rather than
# from the mechanism itself — cell 28414 emptied again. Repointed at cell
# 1400, chosen because it is the richest single-people stratigraphy in the
# new world and because its shape is a small showcase of the mechanism this
# whole campaign adds: several of its completed layers ended not in cold but
# in eviction by a RIVAL gnoll band wanting the same defensible ground — a
# people fighting only itself over position, which a single strength scalar
# could not do.
#
# The Generalist (human joins the roster): a FOURTH occurrence of the same
# drift class, from a new competing people entering the settlement packer's
# roster rather than from any label or mechanism change — cell 1400 is still
# the richest single-people (all-gnoll) stratigraphy, but the packer's
# resolution against a sixth competitor shifted its stratigraphy from 20
# layers (year 500-1950) to 16 (year 550, still standing at the time of this
# regen). The counts below are read off the live block each time this
# comment is touched, not carried forward from memory of the last count.
#
# The Tolerance (warlikeness became a per-settlement draw instead of a
# per-species constant): a FIFTH occurrence, and the first that had to move
# the SITE rather than only re-count it. Cell 1400 collapsed to two layers,
# one of them zero-tenure — no longer a showcase for stratigraphy at all.
# Re-found by enumerating occ-site over the live seed-42 ledger: cell 21953
# is now the richest single-people (all-gnoll) column at 8 layers, and it is
# the better fit for this page's own thesis besides — it is genuinely
# CONTESTED, three of its seven completed layers ending in flight from
# another gnoll band and three more ending because the occupants won
# somewhere else and carried the settlement onto the ground they took.
# (Cell 3518 has 9 layers but every one of them ends in ice or famine, which
# would leave the page titled "The Contested Clearing" describing a quiet
# one.) All counts below re-read off the live block, per the rule above.
history_site=21953
echo "regenerate-artifacts: the legibility surface (a site's deep history)" >&2
{
    printf '# The Contested Clearing of Seed 42\n\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'A site read back out of the ledger by the `history` verb: the stratigraphy\n'
    printf 'of every people that ever settled one cell, oldest layer deepest, and the\n'
    printf 'derived flesh — the structures they raised, the residue in the grass\n'
    printf 'today. Nothing here replays the deep-history bake; it is all a\n'
    printf '*present-as-query* over committed occupation facts, with the flesh\n'
    printf '(structures, residue) derived on demand and never committed.\n\n'
    printf 'This is a real clearing on the world of seed 42 — cell %s — and eight\n' "$history_site"
    printf 'gnoll steadings have risen on it, one settling atop the ruins of the\n'
    printf 'last, from the year 850 down to the present. Only one of the seven\n'
    printf 'completed layers was ended by winter. Three were put to flight by\n'
    printf 'another gnoll band wanting the same defensible ground, and three\n'
    printf 'more ended the other way about — the occupants won a fight somewhere\n'
    printf 'else and carried the settlement onto the land they had taken. It is a\n'
    printf 'people with only itself to fight, and this column is what that looks\n'
    printf 'like from the ground. The eighth was founded in the year 1950 and\n'
    printf 'stands yet, fifty years on: some twelve souls, two huts and a\n'
    printf 'granary, and no ruin yet to read.\n\n'
    printf '```text\n'
    run -p hornvale -- history --world "$wsky" --site "$history_site"
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
    printf '# The Transport Topology of Seed 42\n\n'
    printf 'The connection graph'\''s legibility surface: a site'\''s natural sea-lanes and\n'
    printf 'overland routes, and which of the world'\''s naturally-connected regions it\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'belongs to, read off the `connections` verb. Nothing here is authored\n'
    printf 'infrastructure -- a "route" is always a natural corridor the terrain and\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
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
run -p hornvale -- map --world "$wsky" --out book/src/gallery/features-seed-42.png \
    --field features > book/src/gallery/features-seed-42.md
run -p hornvale -- vestige-map --world "$wsky" --out book/src/gallery/vestige-seed-42.png \
    > book/src/gallery/vestige-seed-42.md
run -p hornvale -- star-chart --world "$wsky" --out book/src/gallery/star-chart-seed-42.png \
    > book/src/gallery/star-chart-seed-42.md

echo "regenerate-artifacts: scene exports" >&2
run -p hornvale -- scene tiles --world "$wsky" > book/src/gallery/scene-tiles-seed-42.json
run -p hornvale -- scene tiles-region --world "$wsky" --face 0 --level 3 --ix 4 --iy 4 --samples 16 > book/src/gallery/scene-tiles-region-seed-42.json
run -p hornvale -- scene moons --world "$wsky" > book/src/gallery/scene-moons-seed-42.json
run -p hornvale -- scene neighbors --world "$wsky" > book/src/gallery/scene-neighbors-seed-42.json
run -p hornvale -- scene eclipses --world "$wsky" --from 0 --until 2000 > book/src/gallery/scene-eclipses-seed-42.json
run -p hornvale -- scene surrounds --world "$wsky" > book/src/gallery/scene-surrounds-seed-42.json

# The Purview's legibility surface (The Margin): the same scene/surrounds/v1
# chart the JSON export above carries, rendered through --render ascii at
# three genuinely different seed-42 observers -- the flagship settlement
# (uniform, kept for continuity with the possession transcript), a coastline
# half a degree east of Mjoexaenoenoa where the neighbourhood's own room mix
# reads land against ocean, and a room at latitude -10, longitude 0 that
# crosses a base-icosahedron face seam.
#
# `book/src/gallery/surrounds-seed-42.md` is hand-authored prose, NOT
# generated here -- edit it directly. Only the three CHARTS it `{{#include}}`s
# are regenerated, each to its own small file under generated/surrounds-
# seed-42/, following the `{{#include generated/<study>/...}}` convention the
# lab pages already use (book/src/laboratory/). This is the fix for the
# failure mode the previous shape had: the whole page used to be `printf`'d
# from here, so a direct edit to the committed .md was silently destroyed on
# the next regen. These chart files are `scene surrounds --render
# ascii`'s exact, drift-checked output -- excluded from CI's strict
# byte-drift check (ci.yml) for the same libm-threshold reason as
# scene-surrounds-seed-42.json, since they render the identical
# `biome`/`water`/`relief` classifications; the hand-authored .md that
# includes them carries no such exposure and is checked normally.
# The variety surface (the-shoal, T4): a global sample of rooms, so the book
# shows what the world's places actually read like. Roughly two thirds of any
# sample is sea — which is exactly why this page exists. Before The Shoal every
# one of those rows said "broken terrain", and no committed artifact sampled a
# marine room, so the gap was invisible in the book for as long as it existed.
echo "regenerate-artifacts: the variety surface (a global room sample)" >&2
{
    printf '# The Look of the World — Seed 42\n\n'
    printf 'A Fibonacci-lattice sample of rooms spread evenly over the globe, each\n'
    printf 'rendered by the locale window: its biome, its strangeness, and the\n'
    printf 'descriptor drawn for it. Most of any honest sample of a world is ocean,\n'
    printf 'so most of this page is ocean — the sea read at its own depths, with the\n'
    printf 'sunlit water described by its light and the lightless water not.\n\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'Generated by `hornvale locale --world world.json --sample 48`.\n\n'
    printf '```text\n'
    run -p hornvale -- locale --world "$wsky" --sample 48
    printf '```\n'
} > book/src/gallery/room-sample-seed-42.md

# The findability surface (the-occlusion, T7): the placed exotic sites. The
# strangeness budget keeps them a rare minority of land by design, so a random
# `locale --sample` essentially never lands on one — the tier was generated but
# unreachable. This listing is where it becomes visible.
echo "regenerate-artifacts: the findability surface (placed exotic sites)" >&2
{
    printf '# The Strange Sites of Seed 42\n\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'The world'"'"'s placed exotic regimes: where each is, and what makes it\nstrange. Generated by `hornvale locale --world world.json --strange`.\n\n'
    run -p hornvale -- locale --world "$wsky" --strange
} > book/src/gallery/strange-sites-seed-42.md

echo "regenerate-artifacts: the legibility surface (the purview, off a possession)" >&2
mkdir -p book/src/gallery/generated/surrounds-seed-42
{
    printf '$ hornvale scene surrounds --world world.json --render ascii\n'
    run -p hornvale -- scene surrounds --world "$wsky" --render ascii
} > book/src/gallery/generated/surrounds-seed-42/flagship.txt
{
    printf '$ hornvale scene surrounds --world world.json --room 897392747 --render ascii\n'
    run -p hornvale -- scene surrounds --world "$wsky" --room 897392747 --render ascii
} > book/src/gallery/generated/surrounds-seed-42/coastline.txt
{
    printf '$ hornvale scene surrounds --world world.json --room 724698318 --render ascii\n'
    run -p hornvale -- scene surrounds --world "$wsky" --room 724698318 --render ascii
} > book/src/gallery/generated/surrounds-seed-42/seam.txt

# Censuses are still opt-in (HV_CENSUS=1) so the everyday gate stays fast:
# skipped BY DEFAULT, and SKIP_CENSUS=1 (CI's fast probe path) also skips.
# But since decision 0063 (The Local Census cut the per-world cost ~285 → ~8
# CPU-s) the sanctioned refresh is a local run ON THE CANONICAL BOX
# (`lefford`): `HV_CENSUS=1 bash scripts/regenerate-artifacts.sh` once per
# campaign at the pre-merge close — the full ~2000-world census takes ~7 min —
# keeping the fixtures current with main instead of lagging. `make
# regen-remote` (the AWS box) is ABANDONED. Note "local" means "not AWS", NOT
# "whichever machine you are on": `lefford` is the single canonical platform,
# because boxes differ on ~0.1% of discrete-count metrics (0063). The guard
# below enforces that; see scripts/census-canonical-host.sh.
if [ "${HV_CENSUS:-0}" = 1 ] && [ "${SKIP_CENSUS:-0}" != 1 ]; then
    # (host already verified at the top of this script)
    echo "regenerate-artifacts: lab censuses (release; HV_CENSUS=1; ~7 min, canonical box)" >&2
    run_release -p hornvale -- lab run studies/the-census.study.json
    run_release -p hornvale -- lab run studies/census-of-the-meeting.study.json
else
    echo "regenerate-artifacts: censuses SKIPPED (HV_CENSUS=1 on the canonical box to refresh; ~7 min, decision 0063)" >&2
fi

echo "regenerate-artifacts: type-audit report" >&2
run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md

echo "regenerate-artifacts: trope coverage report" >&2
run -p hornvale -- tropes report > docs/audits/trope-coverage.md

echo "regenerate-artifacts: done." >&2
