# The Panes: the two-pane view — Design

**Campaign:** The Panes · **Date:** 2026-08-06 · **Branch:** `the-panes`

Rose Window metaplan §6.4, re-ordered by its Amendment 2 §1b.10 to follow The
Lintel and The Blocking. Both have shipped; this is the next rung.

## 1. The question, and the honest answer

*How close are we to a working combination roguelike and text adventure
two-pane view?*

Closer than the absence of any pane suggests, because **both pictures already
render** — they are simply serialized into one transcript on demand rather
than held side by side. Verified by running the shipped binary, not by
reading the code:

```
$ printf 'look\nmap\nenter\nmap\nrelease\n' | hornvale possess --seed 42

[lens: terrain · depth 12 · radius 4 · lattice-aligned, not north-up]
  + + +
 +++++++
++++@++++
 +++++++
  +++++
  ways on: SE, N, SW
  legend: Goodogododaga, bugbear of Goodogododaga, buttressed canopy, …

[plan: chamber 193703028372802, 1 of 2]
###################
#..........#......#
#.....@....+......#
#..........#......#
###################
  legend: . the floor, # a wall, + a doorway, @ you
```

So the campaign is not "build a map." It is "**stop making the map a verb**"
— carry the spatial state in the per-turn snapshot, and let a client hold it
open beside the prose.

## 2. What ships

A live two-pane Casement: prose on one side, a map on the other, both
redrawn every turn, neither costing a turn to consult.

**What does not ship, stated plainly.** The map draws walls, doorways and
`@`. It draws **no creatures and no field of view** — `windows/vessel/src/
lattice/occupancy.rs` opens with "No creature stands in a cell until The
Sighting," and that remains true after this campaign. The owner chose this
ordering at the opening fork knowing it: the seam is worth proving before the
picture is worth staring at.

## 3. The spatial channel

`vessel/session/v1` gains one channel, `spatial`. **Additive** — the schema's
own contract (`windows/vessel/src/snapshot.rs:8-9`) says additive changes are
free and only a meaning change mints `v2`. No epoch.

It carries **semantic content, never a picture**. Decision 0022 is
dispositive: the sim emits "semantic content only (what an observer can see,
never how to draw it)" and "clients render pixels." The `narration.prose`
exception does not generalise — prose ships verbatim because prose is the
constitutional primary (§3.5), and a map is not prose.

The channel is a tagged union over the band, because the session already
treats the bands as mutually exclusive (§4).

### 3.1 Walk band — embed `scene/surrounds/v1` verbatim

Nothing new is designed here. `windows/scene/src/surrounds.rs` already
defines `SurroundsScene` — schema-tagged `scene/surrounds/v1`, `Serialize`,
quantized at the emit boundary, with `SurroundsCell`, `Mark` and
`LegendEntry`. `Session::purview(0)` (`session.rs:798`) returns one as a pure
read.

So the walk band embeds it **verbatim, one schema one owner** — exactly the
move `sensed.room` already makes with `locale/room/v2`. This is the cheapest
half of the campaign by a wide margin.

A gift the schema was already holding: `SurroundsCell.state` is documented as
`"here"`, `"sensed"`, or `"remembered"` — and `"remembered"` is annotated
*"written only by a session-owning consumer."* The remembered-map slot was
designed in and has never had a writer. This campaign does not become its
writer (§6), but it stops being unreachable.

### 3.2 Chamber band — a new `vessel/plan/v1`

The chamber band has no serializable form today: `Lattice`
(`lattice/mod.rs:257`) derives `Clone, Debug, PartialEq, Eq` and **not**
`Serialize`. This is the campaign's real new schema surface.

`vessel/plan/v1` is a **palette plus a dense index grid**, which is the shape
`scene/surrounds/v1` already uses and not an invention:

```
{ "schema": "vessel/plan/v1",
  "chamber": 193703028372802,   // the chamber id the prose names
  "at": 0, "of": 2,             // "1 of 2", zero-based
  "extent": { "x":0, "y":0, "w":19, "h":10 },
  "palette": [                  // append-only order, like *_legend
    { "kind": "wall" },
    { "kind": "floor",     "chambers": [0] },
    { "kind": "floor",     "chambers": [1] },
    { "kind": "threshold", "chambers": [0, 1] }
  ],
  "cells": [0,0,0,1,1,…],       // row-major palette indices; length == w*h
  "marks": [],                  // sparse, per-cell instances (empty here)
  "you":   { "x":6, "y":5 } }
```

**Why a palette rather than parallel dense strings.** A per-cell string
carries exactly one attribute, so every later attribute is another array that
must stay length-synced with the grid — N chances to desync, and the totality
invariant to re-check N times. Worse, the interesting attributes are not
one character wide: a colour triple, an occupant's `EntityId`, a temperature.
A palette absorbs all of them as **fields on a palette entry**, costing
nothing per cell, and the grid never changes shape. This is the trajectory
that matters: `CLIENT-walls-bound-warmth` and `CLIENT-breach-and-rubble` are
shipped registry rows, `CellKind`'s own doc anticipates a `Rubble` variant,
and `CellKind::Wall` is documented as "a place in its own right — an alcove,
a screen or a fireplace is an anchor AT one of these." Cells will accrete.

**Colour specifically, since it is the obvious next ask.** Not emitted this
campaign — the client picks glyphs, per §5. But the palette is where it
lands, and the precedent for how is already set: `SurroundsCell.color` is
`Option<[u8; 3]>` with `#[serde(skip_serializing_if = "Option::is_none")]`,
written only by `surrounds_scene_colored_in`, so "an uncoloured document is
byte-for-byte what it was before the colour layer existed." A `color` field
on a palette entry inherits that property exactly: additive, opt-in, and free
when absent. Nothing about this campaign's bytes has to move to gain it.

Four properties the shape is chosen for:

- **Semantics, not glyphs.** `"wall"`/`"floor"`/`"threshold"` are `CellKind`
  discriminants. The client picks `#`/`.`/`+`, which is what leaves
  `CLIENT-atmosphere`, `CLIENT-alive-map` and MAP-60's render-style layer
  reachable as pure client changes (decision 0022).
- **Total, like its source.** `Lattice::cells` is documented as total over
  the extent so that "absent" cannot mean two things. `cells` preserves that:
  its length is exactly `w*h`, checked.
- **The doorway keeps both its answers.** A threshold's palette entry carries
  `chambers: [a, b]`. `CellKind::serves` is a predicate precisely because
  "whose is this doorway" has two right answers and the retired `owner` map
  could hold only one; the palette does not reintroduce that, and it needs no
  separate `doorways` list duplicating what the cells already say — which is
  the discipline `clients/vessel/src/snapshot.ts` states for `ways`, where
  the snapshot carries no `ways` field because "two representations of one
  truth would drift."
- **Instances go in `marks`, not the palette.** A palette entry is a *type*;
  an occupant is an individual. `marks` takes the same `{noun, kind, datum,
  salience}` shape as `scene/surrounds/v1`'s `Mark` plus its cell — and that
  shape is deliberately the focalizer's `Focalized.nouns` shape, "because
  that identity is what makes map and prose two grains of one lens." This is
  the slot The Sighting fills; it ships empty.

**The payload is small and provably bounded, not estimated.**
`structure::MAX_CHAMBERS` is 4 and `lattice::CHAMBER_SIDE` is 8, and
`extent_for` is `cols * CHAMBER_SIDE + (cols + 1)` per axis with
`(cols, rows)` at most `(2, 2)`. So the largest extent any structure can
produce is **19×19 = 361 cells**, and the palette is bounded by
`MAX_CHAMBERS × |CellKind|` — at most a dozen entries today. Seed 42's
opening chamber is 19×10 with a four-entry palette. Well under 1 KB, and it
stays there as attributes are added, because attributes land on the dozen
palette entries rather than on the 361 cells.

## 4. One pane, switching with the band

Not two spatial panes side by side. The sim forbids it: `session.rs:840`
answers `map out` indoors with `INDOOR_CHART_REFUSAL`, so the walk-band chart
is **not derivable while the possession is inside a building**.

Rose Window §4.3's "two panes at two scales" describes the destination.
Lifting the refusal is a sim change and belongs to whichever campaign wants
the two-scale layout; it is registered as a followup, not smuggled in here.

The rejected middle option is worth naming: keeping a *frozen last-seen*
chart on screen while indoors would present a stale picture as a live one —
the cheat-pane failure mode `snapshot.rs`'s own `social` doc warns about at
length. A pane that cannot be honest goes away instead.

## 5. The client

`clients/vessel/src/main.ts` is 151 lines, builds its DOM per container, and
its header already anticipates this: *"a future page can mount two casements
(the diptych)."*

- A new `pane_map.ts`, pure like `snapshot.ts` and `transcript.ts`: snapshot
  in, glyph rows out. Unit-tested with no DOM, matching the discipline those
  two already hold.
- `main.ts` gains a two-column layout and mounts the map pane beside the
  transcript. The prose pane is the existing transcript, untouched.
- Both panes stay **pure functions of one snapshot** (The Snapshot spec §3),
  so the redaction boundary stays structural: the map pane reads `spatial`
  and cannot see outside it.
- Degradation is already the house pattern — `parseSnapshot` returns `null`
  on an unknown schema and the client falls back to prose. A snapshot with no
  `spatial` channel renders no map pane and prose still works.

The `@` mark: `render.rs`'s `YOU` doc records that the plan and the chart
deliberately share one mark so a player need not learn two. The client honors
that. It is now a client-side choice, which is the point of §3.

## 6. Non-goals

- **Occupants and FOV** — The Sighting. The map is uninhabited and says so.
- **A remembered map.** Verified, not assumed: after entering a chamber,
  `knows` reports exactly `room/738918402`, `settlement/7/name`,
  `settlement/7/population` and **no chamber key of any shape**. The
  knowledge ledger does not track chambers, so a remembered *plan* is a new
  knowledge shape, not a pane. (The walk band is the asymmetric half — it
  accumulates `room/<id>` already and `scene/surrounds/v1` has the
  `"remembered"` state waiting. Registered, not built.)
- **Vitality, combat, inventory** — Rose Window §6.5 and later.
- **The Precincts** (districts, rung 2) — content richness, not a pane.
- **A CLI two-pane view.** The `map` verb is the CLI's map and is unchanged.
- **Replay, historiography and foresight panes** — §6.4 lists them; they need
  read surfaces shaped for a pane. Cheap, not free, and not this campaign.

## 7. Decisions

Full reasoning in the campaign ledger
(`.superpowers/sdd/decision-ledger.md`); material entries promoted here.

1. The spatial channel carries structured cells, never a rendered picture
   (decision 0022).
2. The map pane is instantaneous, read from `sensed`-tier state — justified
   by measurement (§6), not by taste.
3. One band-switching spatial pane, because the session refuses the
   alternative.
4. The `map` verb survives unchanged: `render.rs` §6's parity contract —
   walk what the render claims to depict, demand `examine` accept each item —
   is the check that caught The Lintel's look-named-but-`examine`-denied jar.
   Deleting the verb deletes the test's subject.
5. Additive to `vessel/session/v1`; no epoch, no `v2`.

Candidate for `docs/decisions/`: **"a spatial emit is cells, not a picture"**
— 0022 applied to the fine layer. The number is left unminted here to avoid
colliding with parallel sessions.

## 8. Risks

1. **`vessel/plan/v1` is a cross-boundary contract the moment the client
   parses it.** Same discipline as every scene schema: additive-or-versioned
   only. Mitigated by keeping the shape a projection of `Lattice` rather than
   an independent model — it cannot drift from a lattice it is derived from
   each turn.
2. **Two gates, one of them invisible.** `make gate` cannot see `clients/`.
   This campaign needs `make gate` *and* `make vessel-check`, explicitly, and
   the plan says so per task rather than at the end.
3. **Fixture ambiguity.** The Casement's byte-identity smoke asserts the wasm
   opening matches the committed native transcript. Adding a channel moves
   snapshot bytes but should not move transcript bytes — confirm which
   fixture actually moves before rebaselining anything. A moved *transcript*
   would mean the change leaked into prose, which is the bug.
4. **The empty map disappoints.** Accepted deliberately (§2). The mitigation
   is honesty in the chronicle, not a hedge in the code.

## 9. Testing

- **Rust:** `vessel/plan/v1` round-trips; `cells.len() == w*h` (the totality
  property, inherited from `Lattice::cells`); every index in `cells` is a
  legal `palette` subscript, and every `palette` entry is referenced by at
  least one cell (an unreferenced entry means the projection invented a type
  the building does not have); every `threshold` entry carries exactly two
  chambers and every `floor` entry exactly one; the projection agrees with
  `lattice::render` on what each cell is, so the two cannot diverge; a
  snapshot taken twice without an intervening verb is byte-identical
  (`snapshot` is a pure read).
- **Determinism:** same seed, same verb sequence → byte-identical snapshot
  sequence.
- **Client:** `pane_map.ts` unit tests with no DOM — a known plan payload
  renders known glyph rows; an absent `spatial` channel renders no pane and
  does not throw.
- **Negative controls:** a snapshot whose `cells` length disagrees with its
  extent must be refused by the client, not drawn short; and a cell index
  past the end of `palette` must be refused, not clamped to the last entry.
- **Forward-compatibility control:** a payload carrying an *unknown* extra
  field on a palette entry must still render. This is the property the whole
  §3.2 shape exists for, and it is worth an explicit test rather than an
  assumption — it is what lets colour, warmth or rubble ship later without
  touching the client that predates them.

## 10. Definition of done

Per decision 0013 / 0020, plus the book rule: chronicle entry
(`book/src/chronicle/the-panes.md`) and a freshness sweep; retrospective in
`docs/retrospectives/`; registry rows updated — `CLIENT-redaction-panes`
moves off "no panes yet beyond the transcript," `CLIENT-tile-view` gains the
pane, and the followup register's new rows are filed; `make gate` and
`make vessel-check` both green; artifact drift check clean.
