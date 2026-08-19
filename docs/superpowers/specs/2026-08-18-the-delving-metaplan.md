# The Delving — the program for walking the underworld

*Metaplan. Three campaigns. Status: campaign 0 (The Adit) starting; campaigns
1 and 2 unstarted and unnamed.*

## 1. What occasioned it

The Underworld (Chorography campaign 2) closed 2026-08-18 having given every
chamber real conditions — a depth rung, a stratigraphic rock band, a
vadose/phreatic water-table state, a routed temperature, a named community
from a 22-point corpus — and explicitly declined to resolve what a chamber
*looks like*:

> **The underworld's own chart.** `MAP-underworld-chart` is unresolved and
> this campaign does not resolve it; the pane still shows the country
> overhead.
> — [The Underworld §7](2026-08-16-the-underworld-design.md)

That deferral is the third in a row. A pinned test,
`the_underground_band_folds_into_walk_as_map_does`
(`windows/vessel/src/session.rs:5137`), asserts that the `map` verb and the
session's spatial pane cannot *drift apart* underground — both fold to the
surface chart standing overhead — without ever asserting that fold is
*correct*. Its own doc concedes the oddity: *"Standing in a cave chamber,
the pane shows a chart of the country overhead — which is odd."* The Lantern
(2026-08-07) considered lighting the underworld and found there was no view
to light, and deferred; The Underworld found there was real content and
nothing to stand it on, and deferred again. This program is the resolution.

## 2. Keystone

> **A chamber is a bucket, not a place — you can put a temperature in it, but
> you cannot stand in it.**

`ChamberAddr { cell, entrance, band, slot }` (`windows/worldgen/src/
chamber.rs:107-127`) is a coarse address: which of up to `SLOTS_PER_BAND`
(4) interchangeable chamber-flavors exists at this rung under this cell. It
carries no room, no corridor, no wall — the address was built to let
population and capacity math reason about *how many* distinguishable
habitats exist, not to describe what one looks like or how you'd walk
through it. That address stays exactly as it is; this program builds a
second, independent layer underneath it.

## 3. The campaigns

    0. THE ADIT           the level generator: a recursive partition-tree
                           scaffold, pluggable per-leaf algorithms keyed to
                           CaveKind/ChamberOrigin/community, composite and
                           nested regions, water-table-carved flooding,
                           depth-coherent style across rungs. Derived,
                           FRAME-tier, never serialized. No session or
                           render wiring.

    1. [unnamed]           underground movement: wires `delve`/`climb` to a
                           real position inside a generated level, walking
                           within it, vertical connectivity between rungs
                           via the stairs Campaign 0 already emits.
                           Consumer of 0.

    2. [unnamed]           the chart: production `map`/`SpatialChannel`
                           wiring, replacing the surface fold; retires
                           `the_underground_band_folds_into_walk_as_map_does`
                           (or replaces its assertion — the campaign states
                           which); per-algorithm glyph styling. Resolves
                           `MAP-underworld-chart`. Consumer of 0 and 1.

Each campaign is a hard prerequisite for the next: there is nothing to walk
before there is a level, and nothing to chart before there is something to
walk. Campaigns 1 and 2 are named when their own brainstorms happen.

## 4. What is deliberately NOT in this program

- **Terrain-footprint-conformant entrances.** Considered and dropped
  (2026-08-18 brainstorm): `domains/terrain::Cave` carries only
  `{kind, deepest_band, depth_reach_m}` — a vertical budget, no lateral
  shape — and matching a level's footprint to the surface above would need
  new terrain data this program does not add. A level gets a width and
  height from its own generator, not from the mountain overhead.
- **Authored/WFC vault content.** Captured as `MAP-underworld-vaults` — a
  small pool of hand-authored set-piece halls (a great hall, a treasury)
  seed-selected into a generated level. Deferred: it commits to an authored-
  content library, a heavier and separable decision from "an algorithm
  generates this."
- **Unifying with the building-interior embedder, or the wider
  pattern-language settlement vision.** Captured as
  `TOOL-underworld-embedder-unification` and
  `MAP-pattern-language-settlements`. The Adit's module is written with
  generic names (region, leaf, partition tree — not cave-specific) so a
  later program could point a settlement generator at the same scaffold,
  but this program does not touch `windows/vessel/src/lattice/`, and no
  unification is attempted until a second concrete instance exists to
  generalize from.
- **Underworld dressing prose** (`MAP-underworld-dressing`) — vocabulary for
  what a chamber looks like in words. Waits on Campaign 2 having a chart to
  attach words to.
- **`BandKind::Underneath`.** Carried in the stratigraphic ladder and
  deliberately never occurs — asserted empty
  ([The Deep Realm retrospective](../../retrospectives/the-deep-realm.md),
  line "`BandKind::Underneath` is carried and never occurs. Deliberate").
  No campaign in this program needs to generate content for it.

## 5. Provenance

Brainstorm of 2026-08-18, under `campaign-autopilot`. Two research passes
established that (a) the existing building-interior embedder
(`windows/vessel/src/lattice/allocate.rs` + `grow.rs`) is real, working
precedent for seeded, non-serialized floor-plan generation, but is
architecturally pinned (`MAX_CHAMBERS <= 4`, compile-time asserted, small
fixed extent) to a use it cannot be pointed at directly, and (b) no
cellular-automata, BSP-room, or maze dungeon generator exists anywhere else
in the workspace. One `ideonomy-plain` pass (negation × combination, spectrum
organon) on the level generator's algorithm-selection question enriched
rather than overturned the working recommendation — composite/nested
levels, water-table-as-geometry, depth-coherence across rungs, and
community-keyed selection alongside raw `CaveKind` all trace to that pass.
Decision ledger: `.superpowers/sdd/decision-ledger.md` in this campaign's
worktree.
