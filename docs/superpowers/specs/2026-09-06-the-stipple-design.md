# The Stipple — design

**Spun out of The Newel at its G3 stop (Nathan, 2026-09-06).** Stippling is
the representation of a continuous tone by discrete marks; this campaign is
about what a 1.1 km map box may say when the field under it is sampled
every 110 km.

Reproduction evidence and the decisions behind this document are in **The
Newel's ledger**, `docs/superpowers/ledgers/2026-09-06-the-newel.md`,
entries R5, R11, R12 and decision #5.

## 1. The report

> **Bug Report (Nathan):** Looks like we're still drawing terrain at the
> vertex level, not facet level.

## 2. What it is

Two grids. The world's physics — elevation, biome, rock — is sampled at
icosphere vertices ~110 km apart. The map draws facets ~1.1 km across, so
~2,400 map boxes sit inside one physics sample.

Height is already blended bilinearly across a facet's four corner
vertices. Colour is not: `reflectance_at_facet`
(`windows/locale/src/lib.rs:979-993`, `:1050-1058`) takes
`dominant_corner` — a single vertex — for **biome expression** and for
**lithology**, the two dominant colour drivers. Measured at the shipped
default rung: **12,000 distinct facets render as one colour**, because they
share five vertices.

Worse, `plate::color_for` (`plate.rs:769-779`) branches on `tile.water`
*before* it reaches reflectance, and `water`/`ocean` are strictly
per-vertex. **So the coastline drawn at rung 13 is a 110 km Voronoi edge
between icosphere vertices, not a coastline.**

The written confession at `plate.rs:938-940` is a red herring: it sits on
`MARK_COLOR`, is about the observer's own marker, and is correct.

## 3. The two halves, which want different fixes

### 3.1 The land/water boundary — a continuous cause, read in the SIM

`tile.height_asl` is already blended across the facet's corners and the
scene already carries `sea_level_m`. Comparing them is decision 0687
exactly: a continuous cause at facet resolution, inside the convex hull of
its samples (0676).

**It must move in `windows/locale`, not the client.** The walk-band prose
decides "open water" from `v.locale.biome_kind.is_marine()`
(`windows/vessel/src/focalize.rs:228`), which is per-vertex. A client-only
refinement means a player stands on a box drawn as ocean and is told they
are in a forest — decision 0141's one-turn observable contradiction, and
0117 forbids the client re-deriving a decision the sim makes.

So: prose fallout, committed-fixture fallout, and
`plate_vocabulary.rs:153-157`'s
`assert_eq!(terrain.is_ocean(tile.vertex), tile.ocean)` moves with the
change — it is the statement of the defect, not a guard against a
regression.

### 3.2 The nominal fields — a finer partition, not a blend

Biome expression and lithology are categorical. **Decision 0121 forbids
banding a blend for a nominal field**; it must take a partition, per room.
So the fix is a finer partition, not an interpolation. Decision **0667** is
the ratified precedent (a placed site re-sited vertex-to-facet by a seeded
draw).

**Two candidates, and the choice is a fidelity call — Nathan's:**

| | what it does | what it invents |
|---|---|---|
| seeded weighted draw | each facet picks among its four corners by a seeded draw weighted by `corner_weights` | high-frequency structure: a salt-and-pepper transition that reads as real patchiness in a world with one sample per 110 km |
| noise-warped boundary | a hard per-facet partition, but a low-frequency field decides which corner wins | only the boundary's path; patches stay coherent |

Both satisfy 0676 (every value returned IS one of the samples) and 0121
(both are partitions). The draw is **honest per value and dishonest per
pattern**.

Stated plainly because it is the crux: **the map already invents boundary
shape.** Today's hard edge at 1.1 km is derived from 110 km data and is no
more justified than any other curve. The question is not whether to invent
but which invention misleads less.

`domains/terrain` already warps with `Fbm` and a per-facet `micro` term
already exists on this path, so the second candidate's instrument is
precedented rather than new.

## 4. Blast radius, and what it is not

`reflectance_at_facet` feeds the sim's own chart colouring
(`windows/scene/src/surrounds.rs:812`), so this moves the committed client
fixtures under `clients/game/core/tests/fixtures/`, which carry per-cell
colours.

**It is not an epoch and nothing here is irreversible.** Appearance is
derived, never committed (decision 0718); no save format, seed label or
stream order moves. The reason this is its own campaign is size and blast
radius, not risk.

## 5. What nothing currently pins

**No committed byte-golden covers the rendered plate.** Every pin is a
property assertion — palette count above 9 (`bin/tests/wash.rs:679`), a
chromaticity floor (`:727`), a bare majority of `color_for`'s colours
reaching the plate (`:742`). The pre-fix baseline for a resolution test is
in hand: 12,000 facets, 5 vertices, **1** colour at the shipped rung, and
it belongs in the test's own doc.

## 6. Non-goals

- The client's water palette taking the illuminant (The Newel's B6). It
  touches the same function and is a separate, smaller change.
- Biome on the map-cursor readout path (The Newel's B4 works around it).
- Everything in The Newel's remaining scope and in The Sett's.
