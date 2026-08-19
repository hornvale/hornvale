# The Adit — a shape for the underworld to stand in

*The Delving campaign 0. Predecessor:
[The Underworld](2026-08-16-the-underworld-design.md) (Chorography
campaign 2), which gave chambers real conditions and nothing to stand them
in.*

**Program:** [The Delving](2026-08-18-the-delving-metaplan.md)

---

## 1. What occasioned it

See [The Delving §1](2026-08-18-the-delving-metaplan.md#1-what-occasioned-it)
for the full chain of deferrals. In one line: `ChamberAddr` addresses a
chamber (`windows/worldgen/src/chamber.rs:107-127`) and The Underworld gave
that address real conditions, but neither carries a room, a corridor, or a
wall. Standing in one today prints exactly one line
(`describe_underground_here`, `windows/vessel/src/session.rs:1612-1620`):
`"[underground]\nThe rock here is {stratum}. Ways on: out."` There is
nothing to walk and nothing to chart.

## 2. Keystone

> **A chamber is a bucket, not a place — you can put a temperature in it, but
> you cannot stand in it.**

The Adit builds the place. `ChamberAddr`'s bucket keeps doing its own job
(population and capacity math); nothing about it changes.

## 3. The findings this rests on

Verified in the tree at `88379dfa` (this brainstorm's HEAD at write time).

### 3.1 A real generator already exists, and it is pinned to a use that does not fit

`windows/vessel/src/lattice/` embeds a building's chamber graph into a grid,
by two dispatched methods keyed to `Brief::built`
(`lattice/mod.rs:297-312`):

- **`allocate`** — BSP run inversely: splits the extent among chambers that
  already exist (`lattice/allocate.rs:1-95`), for places somebody built.
- **`grow`** — organic region-growing: chambers tunnel a short passage out
  of one another, then flood-fill claims the rest under a strict
  separation rule (`lattice/grow.rs:1-95`), for places nobody built.

Both derive their own seed stream from a permanent label
(`ROOM_LAYOUT_RECTILINEAR`, `ROOM_LAYOUT_GROWN` — `windows/vessel/src/
streams.rs:80-85`) and neither is ever serialized: cells are `FRAME`-tier,
derived on entry and discarded on exit, under decision
[0069](../decisions/0069-fine-position-is-never-serialized.md)
(`lattice/mod.rs:1-12`). This is the right pattern to imitate. It cannot be
reused directly: `MAX_CHAMBERS <= 4` is asserted at compile time
(`windows/vessel/src/structure.rs:15-27`), coupled to a base-4 address-digit
scan that only guarantees collision-freedom under that bound, and
`extent_for`'s block layout is separately asserted against the same
constant (`lattice/mod.rs:73-77`). An open dungeon floor is not a 4-chamber
path graph in a ≤19×19 grid.

### 3.2 Terrain gives a cave a vertical budget, never a shape

`domains/terrain::Cave` (`domains/terrain/src/features.rs:123-134`) carries
exactly `{kind: CaveKind, deepest_band: BandKind, depth_reach_m: f64}`. No
width, extent, footprint, or lateral-size field exists anywhere in
`domains/terrain` for a cave. `cave_depth_reach_m`'s own doc states the
model is a 1-D vertical budget under lithostatic load, nothing else
(`domains/terrain/src/cave_depth.rs:22-37`). Conforming a level's shape to
the mountain overhead was considered in this brainstorm and dropped for
exactly this reason: the data does not exist, and adding it is a separate,
heavier campaign. A level gets a width and height from its own generator.

### 3.3 No dungeon generator exists anywhere else in the workspace

A workspace-wide search for cellular automata, BSP room-carving, maze
generation, drunkard's-walk, and room-and-corridor techniques found nothing
outside §3.1's building embedder — which its own module doc explicitly
disclaims: *"This is floor-plan synthesis, not dungeon generation"*
(`lattice/mod.rs:3`). The nearest broader precedent is deterministic seeded
noise (`kernel/src/noise.rs`, value-noise/fBm with per-octave derived seeds)
used for continental elevation sculpting — a continuous scalar field over a
fixed mesh, a different shape of problem from carving discrete voids.

### 3.4 The chamber lattice's own "4" is a measurement, not a wall

`SLOTS_PER_BAND: u8 = 4` (`windows/worldgen/src/chamber.rs:59`) is a
lattice-sizing judgement call from Task 0's measured substrate (30 seeds,
55,947 caves, deepest reach spans 4 rungs), stated in its own doc to be
capable of only *widening*, never relocating an existing address — and,
unlike `MAX_CHAMBERS`, carries no compile-time assertion anywhere in the
workspace. The Adit does not touch this constant; §4.1 explains why it does
not need to.

### 3.5 The Underworld already computed real per-chamber signal to key off

Beyond `CaveKind`'s three values, a chamber now carries: a water-table
state (vadose/phreatic — measured 31.9/43.6/41.5% wholly phreatic across
three seeds, `domains/terrain/src/water_table.rs`), a routed temperature
(measured ΔT spread 0.7–58.4 K across seeds, depending on depth in metres),
and a named community from a 22-point corpus on a five-axis basis
(`domains/climate/src/underworld.rs`), each carrying a `genera` field
naming which cave formation(s) it belongs to. This is richer signal than
raw `CaveKind` alone and is available for the generator to read.

### 3.6 `BandKind::Underneath` never occurs

Carried in the stratigraphic ladder and deliberately asserted empty
([The Deep Realm retrospective](../../retrospectives/the-deep-realm.md)).
The generator needs no case for it.

## 4. The design

### 4.1 Scope of a level: one per `(cell, entrance, rung)`, not per `slot`

A generated level corresponds to a rung of a cave system under one surface
cell — not to one of the four ecology `slot`s. `slot` keeps addressing
"which of up to four chamber-flavors' worth of population capacity exists
here"; a level is generated once per rung and can be arbitrarily larger or
smaller than four rooms. The two systems don't need to agree on a count,
because they answer different questions (§2's keystone) — this is also why
§3.4's `SLOTS_PER_BAND` is untouched.

### 4.2 The partition-tree scaffold

A level is a recursive spatial partition: at each rectangle, a seeded
decision is either **leaf** (fill this rectangle with one content
generator, §4.3) or **split** (partition it — orientation and cut position
drawn the same way `allocate.rs`'s inverse-BSP already does — and recurse
into both halves, each independently deciding leaf-or-split). A
single-leaf level (the common case) is a depth-0 tree; two regions stitched
together (a natural cavern beside a mined wing) is depth-1; a region that is
itself subdivided again (a mine with its own outpost carved into a
corner) is depth-2.

Recursion stops at whichever bound fires first:

- **A minimum-area floor** on the rectangle being split — the same shape of
  check `allocate.rs` already applies when carving building chambers. This
  is what stops most levels well short of the ceiling below.
- **A hard depth ceiling of 2.** Past two levels of nesting the result
  reads as visual noise rather than distinct places sharing a level.

### 4.3 Leaf content: the algorithm suite

Each leaf independently selects a content generator:

- `CaveKind::Karst` biases toward a cellular-automata cave.
- `CaveKind::LavaTube` biases toward a tunneler / drunkard's-walk carve.
- `CaveKind::Fracture` biases toward angular, BSP-partitioned rooms —
  reusing the same partition primitive as §4.2's scaffold, in its other
  role as a leaf filler rather than the compositional spine.
- A leaf's own worked-vs-natural draw (independent of, but biased by, the
  chamber's `ChamberOrigin`) can select rooms-and-corridors instead: a
  `Found` chamber draws a low chance per leaf of reading as worked (someone
  has begun digging); a `Made` chamber draws a high chance, with a small
  chance of an untouched natural pocket. This is deliberately decoupled
  from `ChamberOrigin` itself, which stays the single authoritative,
  ecology-facing value (§4.1's decoupling principle again) — the geometry
  layer never writes back to it.
- The chamber's assigned community (§3.5) — not only raw `CaveKind` — is an
  eligible input to this selection, since it already encodes a richer
  signal (e.g. an ore-rich community could bias toward Voronoi-partitioned
  cells, modeling vein/crystal-growth boundaries) than the three-valued
  kind alone. The exact weighting is an implementation-time judgment call
  within this design, not a further open question.

### 4.4 Water table as a carving input, not just a description

A chamber's depth is a single scalar (§3.2 — a cave has one vertical budget,
not a per-cell elevation map), so `is_sump(chamber.origin, depth_m,
water_table_m)` (§1's `windows/worldgen/src/chamber.rs:519-529`) answers a
per-*chamber* question, not a per-*cell* one — reused directly, unchanged.
When it answers true, the level designates one region (the first leaf in
generation order) as the flooded basin and marks its floor cells with a
distinct cell state — a chamber-level fact expressed as a *partial* feature
of its level, not an all-or-nothing flood of the whole level. This reuses
committed per-chamber data directly as a geometry input rather than
inventing sub-chamber elevation data the model does not have.

### 4.5 Depth coherence across rungs: the worked/natural mix, not the algorithm family

**Correction from this campaign's own grounding research**, made before
implementation rather than during it: `CaveKind` is a property of the whole
cave system (`chamber_at` takes one shared `&Cave` regardless of which
rung's address is queried, §1), so there is no per-rung variation for an
algorithm-family draw to drift across — a single descent's `CaveKind` is
fixed by construction. What *can* vary per rung is `ChamberOrigin`
(resolved per-address, §1's `resolve_origin`), which drives §4.3's
worked/natural leaf mix. Depth coherence is therefore expressed there: a
rung's worked-fraction draw has seeded inertia toward the previous rung's
*realized* worked-fraction, rather than each rung's mix being drawn
independently. A whole descent can still read as uniformly natural,
uniformly worked, or genuinely transitioning between the two — same legal
outcomes, correct mechanism.

### 4.6 Connectivity: stairs, emitted but not yet consumed

Each generated level places a down-connection point (and, other than at
`Surface`, an up-connection point matching the rung above) even though no
session code reads them yet. This is the interface boundary Delving
campaign 1 needs, and it is free to include now rather than re-derive
later.

### 4.7 Data structure and module placement

A new module, sibling to `lattice/`, not inside it — e.g.
`windows/vessel/src/underworld_level/` (exact path decided at
implementation time). It follows §3.1's precedent exactly: `FRAME`-tier,
derived on entry and discarded on exit, never serialized, one seed stream
per leaf-algorithm label (e.g. `underworld/level/v1/karst`,
`.../lava-tube`, `.../fracture`, `.../mined`, plus a label for the
partition-tree's own split decisions). Per `TOOL-
underworld-embedder-unification` and `MAP-pattern-language-settlements`
(captured, not attempted — see §7), the module's own types are named
generically — a region, a leaf, a partition tree — rather than
cave-specifically, so a later program could point a different content
generator at the same scaffold without this campaign having built toward
that as a goal.

## 5. Determinism and save-format consequences

None. Nothing this campaign builds is serialized (§4.7, decision 0069);
every draw comes from a newly derived stream under a permanent label, so no
existing stream's consumption order changes and no epoch suffix is needed
anywhere. This is a pure addition, verifiable directly: `cargo test`
against the new module's own seed-purity tests, in the same style as
`lattice/mod.rs:436-455`'s `the_seed_is_read_at_all`.

## 6. Task shape

    0  the partition-tree scaffold        split-or-leaf, the two stopping
                                           bounds (§4.2), own stream label
    1  leaf content: the algorithm suite  CA, tunneler, BSP-rooms,
                                           rooms-and-corridors (§4.3)
    2  the worked/natural per-leaf draw   decoupled from ChamberOrigin
                                           (§4.3)
    3  water-table carving                flooded sub-regions (§4.4)
    4  depth coherence across rungs       the Markov-style continuation
                                           draw (§4.5)
    5  connectivity points                stairs up/down, unconsumed (§4.6)
    6  tests + a debug dump               determinism, connectivity,
                                           non-degeneracy, composite-depth-
                                           cap; a text dump for visual
                                           sanity-checking during
                                           development only — not the
                                           production `map` verb

Task 0 precedes every other task; nothing else can be tested without a
scaffold to generate into.

## 7. Non-goals

See [The Delving §4](2026-08-18-the-delving-metaplan.md#4-what-is-deliberately-not-in-this-program)
for the full list and reasoning (terrain-footprint-conformant entrances,
authored vault content, embedder/settlement unification, underworld
dressing, `BandKind::Underneath`). Specific to this campaign additionally:

- **Session and movement wiring.** `delve`/`climb` are untouched; nothing a
  player does changes. Delving campaign 1's job.
- **The `map` verb and `SpatialChannel`.** The pinned fold test
  (`the_underground_band_folds_into_walk_as_map_does`) is untouched and
  still passes unmodified — this campaign adds a generator nothing yet
  calls. Delving campaign 2's job.

## 8. Provenance

Brainstorm of 2026-08-18, under `campaign-autopilot`, in conversation with
Nathan. One `ideonomy-plain` pass (negation × combination operators,
spectrum organon, visibility/distribution/direction dimension prompts) on
the algorithm-selection question; enriched rather than overturned the
working recommendation — composite/nested levels, water-table-as-geometry,
depth-coherence, and community-keyed selection all trace to that pass.
Nathan's own refinement (BSP-of-BSP, bounded by a depth ceiling and a
minimum-area floor) replaced the pass's simpler two-region composite with
the general recursive form now in §4.2, and his observation that this
scaffold is architecturally adjacent to the building-interior embedder and
a wider settlement-generation vision is captured as `MAP-
pattern-language-settlements` rather than attempted here. Two other ideas
captured rather than built: `MAP-underworld-vaults`,
`TOOL-underworld-embedder-unification`. The decision ledger is
`.superpowers/sdd/decision-ledger.md` in this campaign's worktree and is
presented at G3.
