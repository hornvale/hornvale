# The Lexicon of Place — design

**Date:** 2026-08-24 · **Status:** DRAFT, written for a fresh session to execute ·
**Decider:** Nathan (scope approved in conversation, 2026-08-24) ·
**Author:** Claude (campaign-autopilot)

> **This document is a HANDOFF.** It was written at the end of a long session
> and carries findings that exist nowhere else in the repo. Read §4 ("What
> does NOT change, and why") before proposing anything — three of the renames
> a reader will independently want to make have already been considered and
> rejected on evidence, and one of them was ratified two days ago.

## 0. Deltas from the approved draft

Two departures, both settled with Nathan on 2026-08-24 during execution
planning, both on evidence this draft did not have. The plan
(`docs/superpowers/plans/2026-08-24-the-lexicon-of-place.md`) argues them at
length; they are recorded here because §3's scope table is wrong without them.

**D1. The vertex type is `Vertex`, not `Node`.** `Node` is already taken four
ways in this repository, and minting a fifth is the defect class this campaign
exists to remove:

| where | what "node" means there |
|---|---|
| `windows/chronicle/src/config.rs:28` | `pub struct NodeId(pub u32)` — a graph node, ~36 uses, re-exported from `lib.rs:17` |
| `domains/terrain/src/branch.rs:448` | `struct Node` — a river-network node |
| `domains/astronomy` | the **orbital** node — "node line", "node longitude" |
| `clients/` | DOM / Node.js — `node`, `createTextNode` |

`Vertex` is held by nothing (one private `VertexGrid` in
`domains/terrain/src/channel.rs:534`), is the word §2 below already uses to
state the diagnosis, appears in 231 doc-comment lines, and pairs with `Facet`
as ordinary mesh vocabulary — a vertex is a point, a facet is a patch, which
is precisely the distinction §2 says nothing currently signals. The cascade is
`VertexMap<T>`, `NearestVertexIndex`, `vertices()`, `vertex_count()`.

**D2. The sweep is total, and it ends in a guard.** This draft renames the
*type*; the campaign renames the *word*. Measured on `e2453a63b`:
**447 distinct `cell`-bearing identifiers, 16,201 occurrences in `*.rs`** —
the bare words `cell` (8,014) and `cells` (2,499) dominate, most of them doc
prose. Renaming only the type leaves `VertexMap<T>` documented as "a value per
cell", which is worse than not renaming at all, and leaves a glossary that has
to say "cell means vertex, except where it is called `Vertex`".

The guard is the durable half. "cell" arrived here by **convergent
emergence** — every author reached for it independently, out of the GIS/raster
convention §2 names — so a one-time rename decays. Only a check holds it, and
it takes the three-valued shape `tropes check`, the timings baseline and
type-audit's `waiver(...)` already use: it fails on novelty, not on existence.

## 1. What this is

A **rename campaign, not a redesign.** The spatial vocabulary has accreted
collisions that make the world hard to talk about: the same word means
different things in different crates, and the most-used type calls a continent
a room. Nothing about the world's structure changes here — only what we call
its parts.

It exists because a larger design conversation (a uniform square grid at every
zoom rung) stalled on the fact that the participants could not be sure they
meant the same thing by "level", "band", or "room". The vocabulary is a
prerequisite for that spec, not a nicety.

## 2. The diagnosis

**One sphere, two indexings, no naming signal.** `Geosphere` is an icosphere.
`CellId` is a **vertex** — where terrain and climate fields are sampled.
`RoomAddr` is a **face** — an area you can occupy. Nothing in either word says
which is a point and which is a patch, and the pair is a primal/dual
distinction that has already produced at least one measured defect elsewhere
in the project.

**`RoomAddr` calls a continent a room.** It addresses every depth from 0 to
`MAX_DEPTH` (29). "Room" is accurate for roughly the bottom third of that
range and misleading above it. `RoomAddr` at walk depth is an *outdoor
locale*, not a room.

**"cell" is actively wrong, not merely vague.** In GIS and raster convention a
cell is an *area*. Hornvale's `CellId` is a *point*. A reader who knows the
convention is misled; one who does not learns the wrong thing.

## 3. Scope

| rename | occurrences | files |
|---|---|---|
| `RoomAddr` → `Facet` | 757 | 51 |
| `RoomId` → `FacetId` | 43 | 15 |
| `CellId` → `Vertex` (D1) | 1,735 | 144 |
| `windows/vessel/src/band.rs` → a depth-named module | — | 1 |
| `level` → `depth`, **only** where it means mesh refinement | judgement | — |

**~2,500 sites for the types alone; ~16,200 for the whole word (D2).** Most of it is mechanical — the compiler finds every one —
but doc comments and prose also say "cell" and "room", and those need
judgement rather than substitution. A doc that says "the room's cells" is
saying something true about faces and vertices and must survive the rename
still saying it.

**`level` needs care: it means FOUR things, not three.** This draft named
three and missed the one that is hardest to change:

1. `Geosphere` subdivision level — **rename to `depth`.**
2. `RoomAddr` path depth — **rename to `depth`.**
3. `ChamberAddr.level` (*"a level is a screen-filling map, not a storey"* —
   The Drift) — **leave alone**, it is a different and correctly named thing.
4. `RegionAddr.level` / `RegionScene.level`
   (`windows/scene/src/region.rs:93,240`) — a **cube-face quadtree depth**, on
   a *different mesh* from the icosphere, and `RegionScene` derives
   `Serialize`, so `level` is a field of the `scene/tiles-region/v1` wire
   schema. **Frozen**, under the cross-repo additive-or-versioned rule, not
   merely "left alone" — and it earns a glossary row for exactly that reason.

## 4. What does NOT change, and why

**Read this section before proposing additions.** Each of these was proposed
during the design conversation and withdrawn on evidence.

**`Band` (kernel) stays.** `kernel/src/band.rs` is TWO DAYS OLD and is the
resolution of exactly the problem this campaign is about: The Drift
(2026-08-23) consolidated `hornvale_terrain::delve::DelveRung`, a mirrored
`hornvale_climate::underworld::DelveZone`, and a bare `u8` rank on
`ChamberAddr::band` into one kernel type, with a written argument about why
decision 0094 did not apply and why 0044's cross-domain clause places it in
the kernel. Renaming it to `Rung` would relitigate a deliberate recent
decision without new information.

**The "band means two things" collision is weaker than it looks.**
`windows/vessel/src/band.rs` defines **no `Band` type** — it holds
`CHAMBER_DEPTH_OFFSET`, `chamber_depth()` and `truncate_to_walk()`, all about
refinement depth. The collision is module-name-level only, and renaming that
module is the whole fix.

**`Stratum` stays.** It is realm-relative position in a column
(`Surface`; `Epipelagic`…`Hadal`; `Regolith`…`Underneath`). "Stratum" is the
accurate word for that. It was proposed for renaming to `Zone` on the grounds
that it and `Band` are confusably both-vertical; with `Band` clearly the cave
ladder and `Stratum` clearly column-position, that objection does not survive.

**`Medium`, `Aperture`, `Chamber` and `Anchor` stay.** A realm as
`(medium, access, strata)` rather than an enum of worlds is the best-designed
part of the current vocabulary, and *access rather than materiality* as the
discriminator is a sharp distinction. `Chamber` is unambiguous. `Anchor` — a
named thing with relations and no coordinate — is exact.

**Do not reuse the word "Room" for anything.** Once `RoomAddr` becomes
`Facet`, the temptation is to promote `Chamber` to `Room`. Resist: every
historical reference, retrospective and decision record uses "room" for the
old meaning, and re-pointing the word at a narrower one makes the archive lie.

## 5. THE FREEZE — the half that matters more than the renaming

**Every serialized spelling keeps saying "room" and "cell".** These are
save-format contracts; changing one silently corrupts every world (CLAUDE.md,
decision 0006).

Seed-derivation stream labels:

```
kernel/src/streams.rs        "room/face"                    ROOM_FACE
kernel/src/streams.rs        "room/child"                   ROOM_CHILD
windows/vessel/src/streams.rs "room/furnishing/v1"          ROOM_FURNISHING
windows/vessel/src/streams.rs "room/chambers/v1"            ROOM_CHAMBERS
windows/vessel/src/streams.rs "room/layout/v1/rectilinear"
windows/vessel/src/streams.rs "room/layout/v1/grown"
windows/vessel/src/streams.rs "room/layout/v1/anchors"
domains/settlement/src/lib.rs "cell-id"                     (a predicate name)
```

Epoch keys:

```
kernel/src/world.rs:346      "room/furnishing" => "v1"
kernel/src/world.rs:348      "room/layout/rectilinear" => "v1"
```

Wire and save strings:

```
windows/vessel/src/knowledge.rs:110   format!("room/{}", vantage.locale.id)
windows/vessel/src/purview.rs:146     strip_prefix("room/")
scene/surrounds/v2                    `cells`, and clients/game's ChartCell mirror of it
```

Committed data columns — **the first two are census goldens**, so moving one
is a census-refresh carve-out needing explicit authorization:

```
per-cell-diversity      book/src/laboratory/generated/the-census/rows.csv
cold-built-room-share   ...and census-of-the-meeting, and 8 lab fixtures
cell                    book/src/laboratory/generated/earth-mask-l6/rows.csv
cells_occupied          windows/worldgen/tests/fixtures/occupancy.csv
land_cells              windows/worldgen/tests/fixtures/repose-exposure.csv
```

Serialized JSON keys, counted in the committed artifacts and fixtures
themselves rather than inferred from the code — `"room"` ×306, `"cell"` ×30,
`"cells"` ×12 — across `book/src/gallery/scene-surrounds-seed-42.json`,
`book/src/reference/locale-seed-42.json`,
`clients/game/core/tests/fixtures/`, `windows/scene/tests/fixtures/` and
`windows/vessel/tests/fixtures/`.

And `RegionScene.level`, for the reason §3 gives.

**The constant NAMES may be renamed; the string VALUES may not.** After this
campaign the code will say `Facet` while the label says `"room/face"`, and
that mismatch is correct and permanent.

**For a serde field the name IS the value**, so the rule needs a second
clause: rename the Rust field and add `#[serde(rename = "<old wire name>")]`.
Do it that way round rather than leaving the field spelled `cell`, for exactly
the reason §5.1 gives below — an attribute is a *visible* tripwire, while a
silently-old field name is what the next sweep renames without noticing.

### 5.1 The rule, and why it needs a guard

**The next campaign to notice the mismatch will want to finish the job.** That
is the failure this section exists to prevent, and prose alone will not
prevent it.

Partial cover already exists: `stream_labels()` publishes into a generated,
drift-checked manifest, so changing a *stream label* reddens the drift check.

**The `"room/"` knowledge-key prefix has no cover at all.** It is written in
`knowledge.rs:110` and read in `purview.rs:146` — two literals, in two files,
with nothing asserting they agree. Rename one and the fog of war silently
stops working: no test fails, because each side is internally consistent.

**This is the identical shape to the delve-prose coupling fixed on
2026-08-23** (`fix(vessel): pin the delve success literal driver.rs's
discovery gate depends on`), where a client keyed on `starts_with("You worm
down into the dark.")` and the nearest guard checked a looser substring. Use
the same remedy: **a test co-located with the writing side, asserting the
exact literal the reading side depends on, naming that reader by path.**

Closing that gap is a first-class deliverable of this campaign, not a footnote.

## 6. Verification

- **The rename is behaviour-preserving.** The strongest available evidence is
  byte-identity: seed-42 artifacts and the committed fixtures must not move.
  `make rebaseline` followed by a clean `git diff --exit-code` over
  `docs/generated-paths.txt` is the check, and it is nearly sufficient on its
  own — a rename that changed a stream label would move every name in every
  world and light this up immediately.
- **`docs/audits/type-audit-report.md` will drift**, because the type names it
  reports are changing. Regenerate it in the same commit; it is drift-checked.
- The book, the decision records and the retrospectives contain thousands of
  uses of "room" and "cell" in their old senses. **Do not sweep them.** They
  are historical records of what was true when written. The glossary (below)
  is how a reader reconciles them.

## 7. Deliverable: the glossary

A book page fixing these words, including the collisions this campaign
removes and the ones it deliberately keeps: the two vertical ladders (`Band`
the cave rungs, `Stratum` the column position), the three meanings of `level`
before this campaign and the two after, and the frozen-spelling rule with its
reason. This is the artifact that makes the vocabulary durable rather than
merely current.

## 8. Deferred, deliberately: `Place = (Facet, Depth)`

The design conversation reached a further simplification and **explicitly did
not adopt it**: unify every place-address into `(Facet, Depth)` — where you
are on the sphere, and how far above or below its surface. A cave chamber
becomes a room-scale facet at a depth rather than a
`(cell, branch, band, level)` lattice coordinate; buildings, caves, the sea
column and open ground stop being four addressing schemes.

It is attractive, it satisfies decision 0102's positional-not-generational
requirement arguably better than the current scheme, and **it is an epoch to
end all epochs**: every address in every world changes meaning, the census
refreshes, `world-wasm` consumers re-pin. Nathan's ruling was to do the
renaming first and decide this separately, in clear words.

Recorded here so it is not rediscovered from scratch.

## 9. Practical notes for the executing session

- **This campaign conflicts with everything.** It touches kernel → domains →
  windows → clients. Nathan's timing call (2026-08-24) was that the queue is
  quiet and this should go quickly so parallel campaigns can absorb it.
  Submit promptly; expect to absorb `main` and resubmit if it bounces at the
  mouth.
- Claim a decision block (`make decision-block NAME=campaign/<name>`) before
  authoring any decision record. Minting outside your block passes every
  mechanical check and collides later.
- A merge needs an authored `Sluice-Headline:` trailer in the range.
- The blast-radius numbers in §3 were measured on `5ccde4bb1` and will drift;
  re-measure rather than citing them.
