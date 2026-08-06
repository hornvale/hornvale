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

`vessel/plan/v1` is a dense, row-major projection of the lattice:

```
{ "schema": "vessel/plan/v1",
  "chamber": 193703028372802,   // the chamber id the prose names
  "at": 0, "of": 2,             // "1 of 2", zero-based
  "extent": { "x":0, "y":0, "w":19, "h":10 },
  "kinds":    "wwwwww…",        // row-major, one code per cell: w|f|t
  "chambers": "---0000…",       // row-major, owning chamber index; '-' = wall
  "doorways": [ [0, 1, 11, 5] ],// (a, b, x, y) — from Lattice::doorways
  "you":      { "x":6, "y":5 } }
```

Three properties this shape is chosen for:

- **Codes, not glyphs.** `w`/`f`/`t` are `CellKind` discriminants. The client
  picks `#`/`.`/`+`, which is what leaves `CLIENT-atmosphere`,
  `CLIENT-alive-map` and MAP-60's render-style layer reachable as pure client
  changes.
- **Total, like its source.** `Lattice::cells` is documented as total over
  the extent so that "absent" cannot mean two things. `kinds` preserves that:
  its length is exactly `w*h`, checked.
- **No second truth.** A threshold's chamber *pair* is not duplicated into
  `chambers`; it comes from `doorways`, which the lattice already owns. This
  is the discipline `clients/vessel/src/snapshot.ts` states for `ways` —
  the snapshot deliberately carries no `ways` field because the room owns
  exits, and "two representations of one truth would drift."

**The payload is small and provably bounded, not estimated.**
`structure::MAX_CHAMBERS` is 4 and `lattice::CHAMBER_SIDE` is 8, and
`extent_for` is `cols * CHAMBER_SIDE + (cols + 1)` per axis with
`(cols, rows)` at most `(2, 2)`. So the largest extent any structure can
produce is **19×19 = 361 cells**: two dense strings of 361 bytes plus a
handful of scalars, under 1 KB. Seed 42's opening chamber is 19×10.

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

- **Rust:** `vessel/plan/v1` round-trips; `kinds.len() == w*h` (the totality
  property, inherited from `Lattice::cells`); every `doorways` entry names a
  `t` cell and vice versa; the projection agrees with `lattice::render` on
  what each cell is, so the two cannot diverge; a snapshot taken twice
  without an intervening verb is byte-identical (`snapshot` is a pure read).
- **Determinism:** same seed, same verb sequence → byte-identical snapshot
  sequence.
- **Client:** `pane_map.ts` unit tests with no DOM — a known plan payload
  renders known glyph rows; an absent `spatial` channel renders no pane and
  does not throw.
- **Negative control:** a snapshot whose `kinds` length disagrees with its
  extent must be refused by the client, not drawn short.

## 10. Definition of done

Per decision 0013 / 0020, plus the book rule: chronicle entry
(`book/src/chronicle/the-panes.md`) and a freshness sweep; retrospective in
`docs/retrospectives/`; registry rows updated — `CLIENT-redaction-panes`
moves off "no panes yet beyond the transcript," `CLIENT-tile-view` gains the
pane, and the followup register's new rows are filed; `make gate` and
`make vessel-check` both green; artifact drift check clean.
