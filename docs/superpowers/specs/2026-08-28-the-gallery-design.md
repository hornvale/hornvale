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

**Flooded cells route into the `submerged` band — measured, not assumed.**
The generator's own connectivity checks treat `Floor | Flooded` alike
(`mod.rs:218`, `:402`, `:1162`), so a level's reachability is computed *through*
water today. The session already has a `submerged` band with its own verbs
(`dive`/`surface`) and its own refusal, and this is the rule the measurement
below selects.

**The measurement (Task 0).** Three candidate rules — walk through flooded
cells as if floor ("wading"); refuse them with a physical reason
("impassable"); or route them into the `submerged` band — differ in what they
imply about rung 2, which the spec originally worried was *mostly* water.
`windows/vessel/tests/suite/underworld_level_generation.rs`'s
`measure_flooded_cell_reachability_across_the_descent` swept 60 real seeds
(1..60, all 60 yielded a measurable descent — no skips) through the shipped
entry point (`underworld_level::generate_descent_for_character`), reaching a
cave-bearing vertex the way `session.rs`'s private `find_open_cave_vertex`
does (entrance chamber realized AND seeded barrier `Open`) and deriving every
rung's depth and the vertex's water table exactly as production does
(`windows/worldgen/src/lib.rs:3259-3267`: `terrain.geothermal_gradient_at`,
`terrain.material_at(vertex).porosity`,
`hornvale_terrain::water_table_depth_m`, `hornvale_terrain::
rung_evaluation_depth_m` per rung, `cave.kind` for the cave kind) — never an
invented depth or water table, since flooding is decided by depth against the
table and a fictional input would measure nothing about the world.

Per rung, averaged over the 60 seeds:

| rung | avg cells | avg walkable | avg flooded | flooded % | reach % (Flooded impassable) | reach % (Flooded passable) | onward stairs reachable % |
|---|---:|---:|---:|---:|---:|---:|---:|
| Undercroft | 1144.0 | 737.9 | 432.8 | 58.7 | 40.1 | 100.0 | 100.0* |
| Shallows | 1344.0 | 770.9 | 555.9 | 72.1 | 37.7 | 100.0 | 78.3 |
| Deeps | 1560.0 | 849.8 | 588.3 | 69.2 | 38.8 | 100.0 | 66.7 |
| Underdeep | 1792.0 | 972.2 | 713.3 | 73.4 | 34.8 | 100.0 | 71.7 |
| Nadir | 2040.0 | 1080.2 | 726.4 | 67.2 | 38.4 | 100.0 | 60.0 |

(\* Rung 0 has no `StairsUp` — its entry cell IS its own `StairsDown`, so its
"onward stairs reachable" figure is tautologically 100% by construction, not
a substantive reading; the signal lives in the four deeper rungs, F3 of the
plan's pre-flight scan.) Across all five rungs jointly, only **21/60 seeds
(35.0%)** have every rung's onward stairs reachable from its entry when
`Flooded` is impassable, and "reach % (Flooded passable)" is **100.0% at
every rung** — the partition tree connects every leaf and flooding only ever
fills a whole leaf, never severs the tree, so plain connectivity is never
actually at risk under wading.

**The rule (Nathan, at the Task 0 stop). The measurement was right and its
conclusion was wrong, because "wet" and "drowned" are two categories and the
code has one.**

The probe's own `reach % (Flooded passable)` column is 100.0 at every rung:
flooding fills whole partition-tree leaves and never severs the tree. So water
was never a connectivity problem. What made it look like one was a rule choice
that could not survive its own destination — see the rejected branch below.

**Wet is common and correct.** A cave with water in it and air above it is a
damp underworld, which is what `domains/terrain/src/water_table.rs` was
deliberately calibrated for: it carries a measured drowned-share, a named
`UNDERWORLD_DRYNESS_GAIN`, and states that `DRAWDOWN_SCALE_M` "sets the
absolute depth of every table, and therefore which delve rungs can ever be
dry." The measured 58.7%–73.4% is that calibration showing through, not a
defect. **Wet cells are walkable — you wade.**

**Drowned is rare and is a different thing.** A chamber filled floor to
ceiling, with no air, is not a wet room: it is not enterable sideways at all.
It is dived into from the rung above. Target: **under 5% of rungs.**

**Wetness keys on `LeafStyle.worked`, a field that already exists and already
varies.** `worked` means "this leaf reads as worked stone rather than natural
void", and it is already drawn per leaf from `ChamberOrigin` and `Character`
and compounded down the descent by inertia (DrowTier 0.85, WildCave 0.5,
FungalGardens 0.35). `is_sump` *already* returns `false` for
`ChamberOrigin::Made` with the comment "Drained by whoever cut it" — the model
already believes worked space is drained; it simply never gets to apply the
rule, because the shipped path only ever produces `Found`. Keying on `worked`
makes it live per leaf: **a worked leaf is drained, a natural leaf is wet.** A
drow-tier descent comes out dungeon-dry, a wild cave comes out wet, a fungal
garden wetter still — from a dial already turning. `leaf_styles` is index-
aligned with `region::leaves(&tree)` (both built in one pass), so the flooding
pass reads its own leaf's `worked` without re-deriving anything.

**Movement is a MODE, not a boolean.** One named seam answers "how can this
body move through this cell", returning `Walk` or `Wade` today, with `Swim`
and `Fly` as the variants it will return later (Nathan: *"swimming should be
just another mode of travel, as flight might be in some levels"*). This is the
same shape as §3.4's reach seam, and it is why drowned rungs and flight will
not each need a special case bolted on.

**REJECTED — routing flooded cells into the `submerged` band.** Task 0's
implementer selected this and it is self-defeating: the `submerged` band has
no geometry. `session.rs:2563` refuses `go` there because "it has no lattice
to step across, so a bearing under water still has nowhere to go (The
Column)." Routing 58.7%–73.4% of every rung there would make it *unwalkable* —
behaviourally identical to the impassable rule the same measurement had just
rejected at 35.0% reachability, but with a band transition and more machinery
to arrive at it. The branch label matched the situation; the destination band
could not do the job. **Recorded rather than quietly replaced, because the
reasoning error is reusable: a rule that names a mechanism must be checked
against what that mechanism can currently do, not against what its name
suggests.**

Note what this does NOT rescue: diving into a drowned rung *from above* is
vertical, and `dive`/`surface` are vertical, so the missing lateral geometry
does not block that. The rejected rule failed on lateral movement
specifically.

**Deliberately deferred to a sequel, with the seam built here.** Drowned rungs,
dive-entry from the rung above, and swimming as a travel mode are designed
above and **not implemented in this campaign**: they need a capability model
(amphibious, or a carried/worn item) that does not exist, and The Chattel is
building the object model that would carry it right now. Building a second one
in parallel is the failure this defers. What ships here is wading, the
`worked`-keyed wetness, and the mode seam — after which the sequel is a new
variant on an existing enum rather than a new concept.

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
number and neither is a place a lantern could plug into.

**Exactly one of `chamber_sources`' three radii is the body's, and the
distinction is load-bearing.** All three write `radius: SIGHT_RADIUS`
(`session.rs:4100`, `:4120`, `:4128`), which makes them look interchangeable
and they are not: 4100 is the implicit torch AT THE POSSESSION'S OWN CELL —
the body's carried light, and the only one the seam owns — while 4120 is the
hearth's throw and 4128 is daylight spilling through each doorway. Those two
are properties of a fire and of an opening. Routing them through the body's
reach would mean a lantern in your hand brightens every hearth and every
doorway in the building, which is not what carrying a lamp does. Pouring that
assumption into a second band would make carried light a two-band change
later.

Nathan, at the design stop: *"the implicit light source is extremely temporary
(because we don't have objects yet). Eventually I imagine we will be able to
have lanterns, torches, candles, magic spells, magic staves with glowing orbs
on the end."*

The seam is the same move The Wanting made reserving `world_view` for belief:
the body swaps at zero schema cost because the parameter was there from the
start.

### 3.5 Fog of war: a seen-bitset per rung

Traditional field of view. What is lit now comes from the shadowcast; what you
have seen before stays on the pane, dimmer; creatures and objects are drawn
only while lit.

**Each rung carries a bitset of seen cells**, one bit per cell of its extent,
indexed row-major over `Level::extent`. **Every arrival at a cell** ORs its
shadowcast into the bitset for the rung it happened on. Nothing else is stored.

"Arrival", not "step", and the distinction is a real bug the first draft of
this sentence caused: a lateral step is only one of three ways a possession
comes to occupy a cell. `delve` places it at the entrance, and stairs place it
on the rung below or above. Marking on lateral steps alone means the chamber
you descend into is the one place you do not remember — you see it, walk out
of it, and it is gone — and a rung you enter and immediately leave by the
stairs is never recorded at all. All three paths mark.

Extents run `40 + 4·rank` by `24 + 2·rank` with `rank ≤ 5` (five underground
rungs; `Band::Surface` is not one), so the largest rung is 60×34 = 2,040 cells
= 255 bytes, and a whole descent is **~1.2 KB**.

Three properties fall out of the representation rather than needing to be
enforced:

- **Monotone by construction.** Bits are only ever set. §4.1.2's monotonicity
  is structural, not a test that could fail.
- **Correct across a change in reach.** A bit is set with whatever reach was in
  force at the moment it was set. Acquire a lantern and later steps set more
  bits; earlier ones are untouched. The past stays as you saw it.
- **O(1) to read and to write**, with no cache to invalidate and no
  recomputation on redraw — which matters, because the client rebuilds a full
  snapshot **on every keypress** (The Quadrat), including keystrokes that are
  just typing.

#### Why not derive the seen-set from a stored trail

This was the draft design and it was wrong, in a way worth recording because
the argument for it sounded like the project's own architecture.

The draft stored a trail of `(cell, reach)` and folded the seen-set out of it
on demand, citing `UNI-20` — *"nothing stored that re-derives"*. **That
citation is circular.** A seen-set does not re-derive from the world: no
`(seed, address)` yields it, because it is playthrough history rather than
world truth. It re-derives only from the trail, which is *itself* stored
playthrough state. The draft traded one stored object for a larger, slower
stored object and called the result a derivation. `UNI-20` governs values
derivable from the ledger; fog is not one, and neither representation is more
derived than the other.

The draft's second argument was that storing a set "bakes the reach in at write
time." It does, and **that is the correct semantics, obtained for free.** The
bug the draft feared — a lantern retroactively illuminating chambers walked
past in the dark — is a hazard *created* by re-folding a trail, and the
per-step reach field existed only to defend against it. A bitset cannot exhibit
it.

What a trail genuinely buys is answers to a **different question**: not *which
cells have I seen* but *where have I walked, in what order* — retracing, a
`back` verb underground, "you have come this way before." That is a separate
feature, is not in §6, and is not in this campaign. Letting it choose the data
structure for fog was the error.

**If retracing is wanted later, a trail is added then, beside the bitset rather
than instead of it.** The two answer different questions and neither derives
the other.

#### Lifetime

Session, the same cut The Latch made: fog dies when you climb out, and the save
round trip is named rather than claimed.

The bitset makes that cut cheap to revisit, which is a point in its favour
rather than a deferral. Persisting ~1.2 KB of bitset is a smaller and better-
shaped change than committing several hundred position facts, so if fog should
survive a save, the work is a serialization decision and not a redesign.

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
`remembered` only accumulates. Under §3.5's bitset this is structural — bits
are only ever set — so §6.4b pins a property the representation already
guarantees, which is the cheap direction for a test to run.

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
- **Fog is a seen-bitset per rung** — ~1.2 KB for a whole descent, monotone by
  construction, correct across a change in reach without bookkeeping. Records
  why the trail-and-fold alternative was rejected, since its argument cited
  `UNI-20` and the citation was circular (§3.5).
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
