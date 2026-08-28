# The Gallery — the underworld becomes a band you can stand in

**Campaign:** The Gallery · **Branch:** `campaign/the-gallery` ·
**Decision block:** 0406–0415 (main ceiling 0388 at reservation) ·
**Drafted:** 2026-08-28 · **Status:** Draft, at G3.

**The Delving, campaign 1** — with campaign 2 absorbed. Predecessor: The Adit
(campaign 0, `docs/superpowers/specs/2026-08-18-the-adit-design.md`).
Metaplan: `docs/superpowers/specs/2026-08-18-the-delving-metaplan.md`.

*A gallery is a horizontal passage driven through rock, and it is also a room
you walk through to see what is in it. This campaign is both readings at once,
which is the argument for not splitting them.*

---

## 1. What occasioned it

The Adit shipped a level generator nine days ago that nothing can reach.

`windows/vessel/src/underworld_level/` is 1,896 lines — a recursive partition
tree, four per-leaf carving algorithms keyed to `CaveKind` and
`ChamberOrigin`, water-table-carved flooding, depth-coherent style inherited
down a descent, stairs placed between rungs. Its only consumers outside its
own module are `lib.rs`'s re-export and its own test file:

```
$ grep -rn "underworld_level" --include=*.rs windows/ cli/ clients/ \
    | grep -v "^windows/vessel/src/underworld_level/" | grep -v "//"
windows/vessel/tests/suite.rs:106:#[path = "suite/underworld_level_generation.rs"]
windows/vessel/tests/suite.rs:107:mod underworld_level_generation;
windows/vessel/src/lib.rs:36:pub mod underworld_level;
windows/vessel/src/lib.rs:57:pub use underworld_level::{
```

That is the whole of it. The unfiltered grep returns 12 lines; the other 8 are
doc comments in `cli/src/streams.rs` and the test file naming the module in
prose. No production path constructs a `Level`.

What a player gets instead, measured rather than described — `possess --seed 1`,
after clearing the rubble the seeded barrier puts at that cave mouth:

```
> delve
You worm down into the dark. The rock here is the regolith.
> look
[underground]
The rock here is the regolith. Ways on: out.
> go n
Not down here. Climb out first, then walk.
> map
      # # #
   # # #   #  #                  <- the FOREST, from inside the rock
# # #    # #
 #    # @   #  #
  ways on: W, SE, N
  legend: Babobokoxaba, a karst cave, buttressed canopy, tropical seasonal forest
```

One bucket with a stratum word, two examinable nouns (both the rock), no
lateral movement, and a chart of the country overhead. `Session.underground`
is `Option<Chamber>` — an address and a rock type, not a place.

Meanwhile the generator, asked directly for a three-rung descent, produces
this (extract; `>`/`<` are the stairs it already places, `~` the flooding it
already carves at a 100 m water table):

```
rung 1 · Undercroft · 44×26          rung 2 · Shallows · 48×28
#######.....################         ###<~~~~~~~~~~~~~~~~~~######
#######......###############         ##>~~~~#####~~~~~~~~~~~#####
####....####################         ###~~########~~~~~~~~~~~~###
##>....#################...#         ####~#########~~~~~~~~~~~~~~
##<...####################.#         ####~############~~~~~~~~####
```

Two campaigns have since deepened the underworld *underneath this floor*. The
Sources gave it a chemotrophic productive base — a per-vertex, per-rung energy
field answering to rock rather than sun. The Deep Realm re-authored the xorn's
and rust monster's niches for subterranean conditions, and relocated them in
every generated world. None of it is reachable, visible, or inhabited.

### 1.1 Why this is one campaign and not the metaplan's two

The metaplan splits movement (campaign 1) from the chart (campaign 2). That
split is wrong for this deliverable, and the reason is in the code rather than
in taste.

`SpatialChannel`'s doc states the property the union exists to hold:

> One pane switches; two do not coexist. … Pane and verb therefore still
> cannot disagree, which is the property this union exists to hold; what they
> agree ON, in those two bands, is a chart of the country overhead. Whether
> that is the right answer is an open question, not a settled one
> (`CLIENT-band-fold`) — but it is the *same* answer the sim already gives,
> and **changing it is a sim change before it is a schema change**. … so a
> fourth band cannot be added without meeting this question.

Shipping movement alone would mean walking a real cave while the pane and the
`map` verb *agree, correctly, on the forest overhead*. The fold would stay
pinned for a whole campaign, and the campaign's own product would be the
strongest possible argument that the pin is wrong. The two halves are one
decision; this campaign meets the question the code reserved for it.

## 2. Keystone

> **The underworld is not a bucket you fall into. It is a band you stand in —
> with a position, a geometry, a memory of where you have been, and something
> living in it.**

The Adit's keystone stands unchanged: `ChamberAddr` remains a coarse address
for *how many* distinguishable habitats exist, and this campaign changes
nothing about it. The level layer sits underneath it, exactly as The Adit
built it.

## 3. The mechanism, in six parts

### 3.1 Position

`Session.underground` becomes a struct mirroring `Inside`, which is the
existing precedent for "the possession is standing in a derived cell grid":

```
Inside      { structure, at,    lattice, cell, seed }
Underground { descent,   rung,  level,   cell, seed }
```

The session-state struct and the `SpatialChannel` variant are **different
types with different jobs**, and the precedent names them differently — the
session holds `Inside`, the channel carries `Chamber`. The plan picks the
session struct's name; this spec uses `Underground` for the channel variant
(the wire tag `band: "underground"`) and leaves the struct's name open rather
than shipping one word for two things.

`descent` is the `Vec<Level>` `generate_descent_for_character` already
returns; `rung` indexes it; `cell` is the occupied cell; `seed` is the one the
descent was derived from, carried rather than re-derived for the reason
`Inside::seed` is — it is a property of the place, fixed while you stand in
it, and re-deriving it per snapshot invites the two to disagree.

`delve` builds the descent instead of setting a bucket, and places the
possession at the entrance. `climb` exits from the top rung only.

**FRAME-tier throughout (decision 0069).** The descent is a pure function of
seed and address; nothing here is serialized.

### 3.2 Movement

`go <dir>` steps one cell; an impassable cell refuses with a physical reason,
the way a wall does indoors. `UNDERGROUND_LATERAL_REFUSAL` is deleted — the
constant whose doc says "there is nowhere down here for a bearing to mean" is
false the moment there is.

Stairs move between rungs. `LevelCellKind::StairsDown` / `StairsUp` already
exist and `place_connections` already emits them.

**Flooded cells are an open question this campaign must answer, not assume.**
The generator's own connectivity checks treat `Floor | Flooded` alike
(`mod.rs:218`, `:402`, `:1162`), so a level's reachability is computed *through*
water today. But the session has a `submerged` band with its own verbs
(`dive`/`surface`) and its own refusal. Three candidate rules — walk through
flooded cells as if floor; refuse them with a physical reason; or route them
into the `submerged` band — differ in what they imply about rung 2 above,
which is *mostly* water. §7's acceptance test requires the campaign to state
which and pin it; the plan's first task measures how much of a real descent is
flooded before the rule is chosen, because a rule that makes the deepest rungs
unreachable is a different campaign from one that does not.

### 3.3 Sight

The lattice's symmetric recursive shadowcaster (`lattice/sight.rs`, Ford's
algorithm, property-tested by `sight_is_symmetric` over every ordered floor
pair) is generalized over a **transparency predicate** so both bands call one
implementation. It is `shadowcast(&Lattice, Cell, i32)` today; `Level.cells`
is `BTreeMap<Cell, LevelCellKind>` against `Lattice.cells`'
`BTreeMap<Cell, CellKind>` — the same shape over the same `Rect`, a different
value type.

Converting a `Level` into a `Lattice` is rejected: `CellKind::Floor(usize)`
carries a chamber index, and a cave has no chambers. A second copy of the
shadowcaster is rejected: it would duplicate an implementation whose symmetry
is the thing a property test holds. `CellKind::passable`'s own doc already
argues the direction — *"a rule written against the variant breaks the day
`Rubble` arrives; a rule written against the predicate survives it."*

The refactor must be behaviour-preserving, with `sight_is_symmetric` and
`a_wall_blocks_what_lies_behind_it` still green against the chamber band.

### 3.4 Reach is a seam, never a constant

**The radius comes from one named function that answers "how far can this body
see from here, right now."** Today it returns the implicit torch's reach;
later it reads whatever the body is carrying.

This is not speculative generality. `chamber_sources` already gives the
possession an implicit torch — a `Source` at its own cell, `TORCH_KELVIN`,
scaled 4× by The Wick — and hardcodes `radius: SIGHT_RADIUS`, the same
constant the shadowcaster is called with separately. Two places hold one
number and neither is a place a lantern could plug into. Pouring that
assumption into a second band would make carried light a two-band change
later.

Nathan, at the design stop: *"the implicit light source is extremely temporary
(because we don't have objects yet). Eventually I imagine we will be able to
have lanterns, torches, candles, magic spells, magic staves with glowing orbs
on the end."*

The seam is the same move The Wanting made reserving `world_view` for belief:
the body swaps at zero schema cost because the parameter was there from the
start.

### 3.5 Fog of war: store the walk, derive the seen-set

Traditional field of view. What is lit now comes from the shadowcast; what you
have seen before stays on the pane, dimmer; creatures and objects are drawn
only while lit.

**The seen-set is not stored. The trail is, and the seen-set is a fold over
it:**

```
seen = ⋃ shadowcast(level, c, r)   for every (c, r) in the trail for this rung
```

A five-rung descent is ~7,000 cells but only a few hundred *steps*. This is
`UNI-20` — the project's own derived-view architecture, *"nothing stored that
re-derives"* — applied as written, and it is the third instance of the same
move: The Quickening folds NPC position over committed `agent-at`, The Wanting
folds a drive over the same log. `Session.trail: Vec<Facet>` already exists for
`back` and is the shape precedent — **not the storage**. Its entries are
walk-band `Facet` addresses, which name nothing underground; this band gets
its own trail, keyed per rung, and `back` stays refused down here for the same
reason it is refused indoors (a walk-band trail is not geometry).

**Each trail entry carries the reach in force at that step.** Without it, a
lantern acquired later would re-derive the past at the wider radius and
*retroactively illuminate* chambers you walked by in the dark — memory
improving because you picked something up. Storing `(cell, reach)` makes the
fold exact, keeps a lantern's effect in the future where it belongs, and
threads §3.4's seam through the stored data rather than bolting it on.

**Rejected: per-cell `Knowledge` keys.** `knowledge_is_subset`
(`knowledge.rs:159`) is default-deny over key shapes — *"Unknown key shapes
among the checked (non-heard) entries are violations"* — and the walk band's
one-key-per-locale granularity, whose value is the locale's whole serialized
description, does not survive 1,344 cells per rung on a wire clients read.

**Rejected: a stored bitset.** It bakes geometry, route and reach together at
write time, and reach is the one input Nathan named as temporary.

**Lifetime: session, and this is a choice rather than a limitation.** The
trail is session-tier, so fog dies when you climb out — the same lifetime The
Latch proved for a cleared passage, and no more. The save-tier home is named
and not built: committing steps as facts, for which `AGENT_AT` is already
registered every session and currently used only by NPCs. The Latch's
precedent is exactly this — prove the session claim, name the save round trip,
do not claim it.

### 3.6 Inhabitants

Nothing lives underground today. The session's own audit table records
"underground (no creature arm at all)", and The Deep Realm's Task 6 states the
placement half plainly: *"chambers are not wired into placement in this
campaign (spec §6: C2a has nobody underground)."*

The ingredients exist and are shipped:

- `subterranean_substrate(surface, depth_m, gradient, water_table_m, porosity)`
  → the conditions at a chamber, per point.
- `subterranean_energy` / `subterranean_energy_field_per_rung` → what there is
  to eat there, answering to rock rather than sun (The Sources).
- `dominant_source` → *which* of the seven sources wins, retained beside the
  scalar precisely so the differences survive the sum.
- Species niches already scored against a `Substrate`, two of them
  (`xorn`, `rust-monster`) re-authored for subterranean conditions.

**The property this campaign must demonstrate:** who appears in a cave chamber
is derived from that chamber's own substrate and energy — not from a spawn
table, not from the surface roster, and not from a constant. A chamber whose
dominant energy source differs should be able to hold a different creature
than one next to it, and a chamber that can feed nothing holds nothing.

They are drawn on the plan as marks and **filtered by sight**, which the
chamber band already does correctly: `marks` is a strict subset of `present`,
a creature drawn only when placed AND lit. That half is precedent, not
invention.

## 4. The wire — `vessel/level/v1`

`SpatialChannel` gains a third variant. The wire tag is `band`; the new value
is `underground`.

```
SpatialChannel::Walk        { chart }   band: "walk"       scene/surrounds/v2
SpatialChannel::Chamber     { plan  }   band: "chamber"    vessel/plan/v1
SpatialChannel::Underground { level }   band: "underground" vessel/level/v1   <- new
```

`vessel/level/v1` carries what `vessel/plan/v1` carries — extent, palette,
row-major cell indices, your position, marks — **plus a per-cell visibility
state**, plus the rung and its depth.

### 4.1 Visibility is a state, not a shade

`SessionPlan`'s palette is interned on `(CellKind, colour)`, which would have
made a dim remembered cell nearly free: same kind, darker colour. That is the
wrong answer, and the systems audit says why in its verdict on Field of View:

> the seen/remembered distinction is carried by a **glyph twin** rather than a
> dimmed tint — `faded()` maps `.`→`,`, `^`→`n`, `&`→`%` — so it survives an
> uncoloured cell, an uncoloured terminal, and the monochrome client alike

`clients/game` is a character grid, and all three renderers withhold tint from
a mark by explicit rule. Encoding fog as colour would put this band's field of
view *below* the walk band's on exactly the axis the audit credits. So the
document carries an explicit per-cell state, as `SurroundsCell.state` does —
the walk band's own vocabulary is `"here"` / `"remembered"`, and this band
needs a third for *lit but not here*.

**The states are a named quantization of illumination, not three free-standing
tags.** When reach becomes light-driven (§3.4), a light model produces a
falloff rather than three buckets; defining the states as a quantization now
makes that a re-quantization later rather than a schema break.

### 4.1.1 Never-seen cells are OMITTED, not flagged

The document carries only cells the possession has seen. A never-seen cell is
absent, not present-with-a-flag.

Two reasons, and the second is the load-bearing one. A 48×28 rung is 1,344
cells, so flagging would put the whole level in every snapshot from the first
step, growing nothing as you explore. And it would ship the client information
the possession has not earned, leaving the client *trusted* to hide it —
`Sighting`'s doc draws this line in the other direction already ("the
embedding may decide what a client is SHOWN, never what an agent comes to
BELIEVE"), and this is the same discipline pointed at the pane. A pane that
cannot reveal the map is better than one that is asked politely not to.

### 4.1.2 What a state entitles a client to draw

The three states are ordered by information, and entities enter at exactly one
rung:

```
              here            lit-not-here
                \                 /
                 \               /      <- entities MAY be drawn
                  +-----+-------+
                        |
                   remembered            <- terrain only
                        |
                   never-seen            <- absent from the document (4.1.1)
```

Terrain is drawn from `remembered` upward; a creature or object only from
`lit` upward. That is Nathan's rule — things vanish when unseen — stated as a
property of the ordering rather than as a special case, which is what §6.6
tests against.

**`remembered` is monotone**: once a cell is remembered it is never un-
remembered within a descent. `lit` oscillates freely as the possession moves;
`remembered` only accumulates. This is mechanically testable and is the
invariant a fold bug breaks silently, so §6 pins it.

### 4.2 Why a new variant rather than reusing `chamber`

Reusing `Chamber { plan }` would be the cheapest possible client change and is
rejected: it puts `chamber`, `at` and `of` on a cave where they name nothing,
and makes one `band` value mean two different kinds of place — the exact
ambiguity the union exists to prevent, in a field the doc says *"a client reads
before anything else."*

Generalizing both bands into one tile-field document was considered and
deferred. It re-versions a shipped schema for elegance with committed client
fixtures behind it, and this repo carries a fresh scar from that temptation:
CLAUDE.md records a `scene/eclipses` v1→v2 bump vetted as *correct* on the
strength of a sentence that had outlived its subject, while the change also
silently **removed** fields. Revisit when a third band asks for it.

### 4.3 The client

One arm in `clients/game/core/src/spread.rs`:

```rust
match &snapshot.spatial {
    Spatial::Walk { chart } => crate::chart::draw(chart, &mut plate, (0, 0)),
    Spatial::Chamber { plan } => crate::plan::draw(plan, &mut plate, (0, 0)),
    Spatial::Underground { level } => crate::level::draw(level, &mut plate, (0, 0)),
}
```

The match is exhaustive, so adding the variant is a **compile error** in
`clients/game` until it is handled. The update is enforced rather than
remembered. Note which gate catches it: `clients/` is outside the cargo
workspace, so `gate-commit`'s `cargo clippy --workspace` does not build it —
`make game-check` does.

## 5. The fold, retired

`the_underground_band_folds_into_walk_as_map_does` is **replaced, not
deleted.** Its own doc states the disposition:

> the invariant worth pinning is not "the pane is right here" but "the pane and
> the verb cannot drift apart here": whichever answer the sim settles on, one
> change must move both.

That invariant survives this campaign intact; only the answer changes. The
replacement asserts the same agreement against the new answer: the pane reads
`band: "underground"`, and `map` in the same state draws the level rather than
the country overhead. Deleting it would remove the guard at the exact moment a
fourth band makes it load-bearing.

`CLIENT-band-fold` is answered — for the underground. **The `submerged` band
still folds into `walk`, deliberately**, and this campaign does not touch it:
the water column has no lattice to step across, so a bearing under water still
has nowhere to go. The registry row narrows rather than closes, and must say so.

`MAP-underworld-chart` is resolved.

## 6. Acceptance test

A player descends, walks a real cave, sees where they have been, meets
something living, and comes back up — and the client's pane is the cave the
whole time.

Mechanically, the campaign is done when all of these hold:

1. `delve` places the possession at a cell of a generated level; `go` walks it;
   an impassable cell refuses with a physical reason.
2. Stairs move between rungs in both directions; `climb` exits from the top.
3. The pane emits `band: "underground"` carrying `vessel/level/v1`, and
   `clients/game` draws the level in the plate.
4. Cells lit now, cells remembered, and cells never seen are three
   distinguishable states **in a monochrome render**; a never-seen cell is
   absent from the document rather than flagged (§4.1.1).
4b. `remembered` is monotone across a descent — no walk sequence un-remembers
   a cell (§4.1.2).
5. Re-entering a level you have walked shows it still remembered; climbing out
   and back down does not (session lifetime, §3.5).
6. A creature underground is drawn only while lit, and which creature it is
   derives from that chamber's own substrate and energy.
7. The flooded-cell rule is stated, implemented and pinned (§3.2).
8. `the_underground_band_folds_into_walk_as_map_does`'s successor is green.
9. The same seed and pins produce a byte-identical descent, pane and transcript.

## 7. What drifts, and why that is expected

`vessel/session/v2` gains a variant, so every committed session fixture under
`clients/game/core/tests/fixtures/` that was captured underground drifts —
and none were, because nothing could get there. Fixtures captured on the walk
and chamber bands must be **byte-identical**: a new enum variant that nothing
emits changes no byte. **Decision rule, not a prediction:** run `make
rebaseline` and read the diff. If only `docs/audits/` moved, regenerate and
commit in the same commit (the type-audit report drifts on any pub-boundary
change, and this campaign adds several). If a walk-band or chamber-band
fixture moved, **STOP** — that is a schema regression, not drift.

`docs/generated-paths.txt` needs no new entry: no new generated directory is
created. If the campaign adds a committed underground fixture, the file must
be declared **by name** as well as by its directory — the already-declared-
directory hazard, where `git diff --exit-code` is silently vacuous against a
path with no index entry.

The census is **not** expected to move: nothing here changes world generation.
If it does, that is a finding to investigate before merge, not a rebaseline.

## 8. Deliberately NOT in this campaign

- **Objects underground.** The Chattel (arc IV.c) is building the object model
  in parallel. This campaign must not invent a second one, and must not model
  a carried lantern — §3.4 builds the *seam* a lantern plugs into, and stops.
- **Cave prose dressing.** `MAP-underworld-dressing`; the `variant_pool` is
  still `&[]`. The registry says it waits on a chart existing — this campaign
  delivers the chart and unblocks it, which is a different thing from doing it.
- **Underground settlements.** `MAP-69` makes surface-versus-underground its
  own future campaign. Nothing here founds anything below the surface.
- **The submerged band's fold.** §5.
- **Authored vaults.** `MAP-underworld-vaults`, deferred by the metaplan for
  its own reasons, which are unchanged.
- **Save-tier fog.** §3.5.
- **Combat.** Decision 0070 orders it after a vitality model that does not
  exist. Meeting something underground is not fighting it.

## 9. Risks

**The level is bigger than the pane.** Levels run 40×24 at the top rung to
48×28 deeper, and the plate is a fraction of a terminal. The chamber band never
faced this — a room fits. The Quadrat rebuilt the client's map drawing around
a rung ladder and a re-centring rule and is the precedent to read first; the
risk is that the answer there does not transfer, and this is the most likely
place the campaign's scope grows.

**The shadowcaster refactor touches shipped, property-tested code.** Mitigated
by requiring the chamber band's existing sight tests green throughout, and by
the refactor being predicate-extraction rather than reimplementation.

**Inhabitants may be a campaign of their own.** §3.6 composes shipped
ingredients, but nothing has ever placed a creature underground, and the
honest failure mode is discovering that the composition needs a placement
model rather than a query. **Decision rule:** if the derivation cannot be
expressed as a read over existing fields within its planned tasks, ship
geometry + pane + the empty-chamber case, register the placement model, and
say so — rather than growing a second campaign inside this one.

**Flooded rungs may be mostly unreachable** under a conservative water rule
(§3.2). Measured before the rule is chosen, not after.

## 10. Decisions this campaign proposes (block 0406–0415)

Numbered at ratification; this is the expected set.

- **The underworld is a band, not a fold.** `SpatialChannel` gains
  `Underground`; pane and verb move together (§5).
- **`vessel/level/v1`**: a new schema rather than a reuse or a generalization
  (§4.2).
- **Visibility is an explicit per-cell state, never a shade** — so field of
  view survives a monochrome client (§4.1).
- **A seen-set is derived from a trail, never stored** — `UNI-20` applied;
  trail entries carry the reach in force at that step (§3.5).
- **Sight radius is a seam, not a constant** (§3.4).
- **The flooded-cell rule** (§3.2), stated once the measurement is in.

## 11. Provenance

Brainstormed 2026-08-28 with Nathan. The campaign's shape changed materially
during the brainstorm: it began as the metaplan's campaign 1 (movement only)
and became the underworld-as-a-band campaign at Nathan's direction — *"we
definitely can't be navigating the underground based on a text only
description with a view of the forest"* — which absorbs most of the metaplan's
campaign 2. The metaplan's own sequencing argument (nothing to chart before
something to walk) is unchallenged; what changed is that both now land
together.

Every measurement in §1 was taken by running the program or grepping the tree,
not inferred. The decision ledger is `.superpowers/sdd/decision-ledger.md` in
this worktree.
