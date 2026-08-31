# The Legend — a map legend, and the register that keeps one honest

**Branch:** `campaign/the-legend`, from `origin/main` @ `b7c97a4df` ·
**Drafted:** 2026-08-28 · **Status:** COMPLETE — merged 2026-08-31. All eleven tasks landed; H1 supported (warm redraw 0.049-0.068 ms against a 0.20 ms bar, 15 replicates); coverage item 2.1 moved `absent` -> `present`.

> A legend is the part of a map that tells you what its marks mean. This
> campaign discovers that Hornvale's client has been assigning marks without
> one, and that three characters already mean three different things.

---

## 0. What occasioned it

Nathan started the game and reported that the world map draws ocean as `~`
and land as `.`, "nothing more detailed than that", and asked for creatures,
the hearth, rocks, rivers, mountains and geographic detail.

The report is exact. `clients/game/bin/src/plate.rs:184-187`:

```rust
const OCEAN_GLYPH: char = '~';
const LAND_GLYPH:  char = '.';
```

Their own doc comment records the provenance: *"The glyph vocabulary (`~`
ocean, `.` land) is the spike's own
(`windows/worldgen/examples/portolan_spike.rs`, `glyph_for`)."* **That is a
throwaway spike's table.** The Portolan inherited it, The Chroma coloured
around it, and The Quadrat rebuilt the entire sampling machinery beneath it
— 1,960,000 vertex candidates down to 90, chart coverage 70.5% -> 99.7% —
without ever revisiting the two-line table on top.

## 1. Keystone

> **The client has been allocating a shared alphabet privately, one pane at
> a time, and the collisions have already happened.**

Verified by reading every glyph constant in the client and the sim's own
renderer:

```
GLYPH  ALLOCATIONS TODAY                                        VERDICT
-----  ------------------------------------------------------  ----------
 @     here    (chart.rs:72, plan.rs:75, plate.rs:220)          CLEAN x3
 ~     ocean   (plate.rs:184) | water (surrounds_ascii)         CLEAN
 o     cave    (plate.rs:204)                                   CLEAN
 &     agent   (plate.rs:225) | creature (scene `map` verb)     CLEAN
 >     prompt  (entry.rs:79)                                    CLEAN (chrome)
 .     land    (plate.rs:187)                                   COLLIDE x3
       floor   (plan.rs:69)
       relief band 2 (surrounds_ascii:113)
 +     threshold (plan.rs:71)                                   COLLIDE x3
       "everything else" (chart.rs:79)
       a water glyph (surrounds_ascii)
 #     wall    (plan.rs:67)                                     COLLIDE x2
       settlement (plate.rs:202)
 _ : ^ A   relief ladder (surrounds_ascii) — sim only, unported
```

`+` is simultaneously a doorway, a creature, and water. The one character
all three panes agree on is `@` — the only one ever assigned by a rule
rather than by hand. **Adding glyphs without an allocation rule makes this
worse.** The deliverable is a partitioned codespace with a rule and a
register; the richer map falls out of it.

## 2. The channel contract (the ratified decision)

Promotes `CLIENT-four-channels` from `raw` to ratified, with one addition
the registry row does not carry.

- **Glyph** carries *order* (ink ascends with the quantity) or *identity*
  (the character is the referent's own initial). **Never an arbitrary
  category.**
- **Colour** carries category and substance. Colour may fail (`NO_COLOR`, a
  16-colour terminal, colour-blindness), so nothing a reader must trust may
  live only here.
- **Weight** carries attention and epistemic state. Already shipped;
  unchanged.
- **Position** carries position.

**Allocation rule: a character means one thing across the whole client.**
Two panes may use different *ladders* for different quantities; they may
never use the same character for two meanings.

### 2.1 Why the 22-glyph rejection is narrower than it reads

`CLIENT-glyphs-22-rejected` (`rejected`, confidence `high`) is routinely
read as "22 was too many". The count was not the defect. A set of 22
*nominal* marks needs a legend permanently, because nothing about `%`
implies "swamp". The five-glyph ladder `_ . : ^ A` needs no legend at all,
because ink density **is** the ordering and the reader decodes it unaided.

So the rule generalizes past this campaign: **ordinal marks self-legend;
nominal marks never do.** That is the licence for the ladders below and the
prohibition on a biome alphabet, stated as one principle instead of two
precedents.

### 2.2 Why the two panes get disjoint ladders

The intuitive move — port `_ . : ^ A` to the world map so the panes match —
is the trap, and it is how `+` came to mean three things. The quantities
genuinely differ with scale. At walk scale (1.87 km facets, the observer is
*inside* one) the meaningful reading is **impedance**: how hard is this to
cross. At globe scale (120 km facets) impedance is meaningless — the
observer is not crossing a facet, they are looking at a continent — and the
meaningful reading is **elevation**.

Both are ordinal and both ascend, so both may use an ascending ink ladder.
They may not use the same characters, or `^` means "rough going" in one pane
and "highland" in the other.

## 3. Where classification lives (the layering)

**This section replaces the campaign's first draft, which was wrong.** The
draft had `plate.rs` read ten more `GeneratedTerrain` accessors and classify
client-side. Nathan flagged that a web client and a graphical tile client
are both intended; checking that against the tree found a live defect rather
than a future one.

`clients/atlas` — the **browser** client — already consumes
`scene/tiles-region/v1`, and that wire already carries, per tile:

```
elevation_m (f64)     biome (index) + biome_legend (names)
ocean       (flag)    water (index) + water_legend (names)
drainage    (f64)     plate, unrest, t_mean_c, moisture, precip_mm_yr
sea_level_m (f64)
```

**Everything the draft proposed to compute client-side is already on the
wire, with legends.** Rivers are `drainage`. Water kinds are `water` +
`water_legend`. `clients/atlas/src/palette.ts` already binds `biome` -> RGB
and is already forward-compatible with a grown legend (its own test:
*"biome index beyond the palette renders neutral gray"*).

Meanwhile `plate.rs` reaches around all of it into the domain crate. The two
clients sit on opposite sides of the boundary, and the one this campaign set
out to enrich is the one on the wrong side. Enriching it would have deepened
a coupling no browser or tile client can follow, **and minted a second
classification beside the one atlas already uses** — this repo already has
three renderers independently implementing one projection, which is the
failure being repeated.

### 3.0 CORRECTION (found while writing the plan, before any code)

An earlier draft of this section said `tiles_region_scene_in` is
"region-addressed and level-parameterized with a `samples` knob, so the
shape already fits a rung ladder." **That is false, and the error is
instructive: level-parameterized is true, "therefore it fits the plate's
ladder" is a DIFFERENT claim and does not follow.**

`RegionScene.level` is, in its own doc's words, *"Quadtree depth on the
cube-face mesh (**a different mesh from the geosphere's own `depth`**)"*,
while `Window.depth` is a geosphere facet rung. `RegionAddr::node_units()`
builds cube-face nodes via `face_unit(face, a, b)`, and `interp`
**barycentrically interpolates** geosphere values onto them (discrete layers
are nearest-vertex).

**CORRECTION (2026-08-30), and it weakens this section's argument.** The
Pavement (`campaign/the-pavement`) challenged the citation of 0287 below and
is right. I re-read the record: 0287's decision text is *"a tile at rung `d`
is a facet at depth `d`, and zooming out truncates the path rather than
averaging the picture"*, and the word "vertex", "vertices" and "corner"
appear **nowhere in it**. That a facet's corners are geosphere vertices is a
corollary of the ICOSPHERE IMPLEMENTATION, not a claim 0287 makes.

So 0287's real role here is narrower than stated below: it does not
independently forbid resampling, it makes 0196's guarantee STRUCTURAL ("A
tile can never be finer than its datum — 0196's 'may never invent detail
below it' is now structural, not checked", 0287 §Consequences).

Two consequences worth having before this hardens:

- **The double-resample leg is contingent, not permanent.** It exists because
  two meshes exist. A change unifying them — which The Pavement proposes —
  removes it.
- **The 0196 leg is narrower than written.** The scene interpolates
  CONTINUOUS layers barycentrically and takes DISCRETE ones nearest-vertex
  (`region.rs:235`). `relief` is discrete and nearest-vertex by construction
  (`region.rs:431`, Task 4), so 0196 does not bite for the field this
  campaign actually added.

**The conclusion still stands, but on a simpler footing than the argument
below claims**: the TUI is a Rust client that can call the classifier
directly, so a fetch would buy a serialization round-trip and nothing else.
That is an argument from simplicity, not from correctness, and it should be
cited as such rather than as a two-decision prohibition.

So a plate consuming that scene would resample twice — geosphere facet ->
cube-face node -> Mercator tile — breaking two ratified decisions:

- **0287** (a zoom rung IS a mesh depth; The Quadrat's keystone is that a
  tile *is* a facet, reached by direct addressing, not by search or
  resampling), and
- **0196** (a view may disclose its own resolution but may never invent
  detail below it — interpolating between vertices is exactly that).

**Therefore the shared unit is a FUNCTION, not a fetch.**

**The layering that ships:**

```
  windows/scene::classify  ->  ordinal band index + nominal class id + legend
        (ONE classifier; every producer below calls it, none reimplements it)
             |                          |
   scene/tiles-region/v1          plate.rs (direct mesh addressing,
   (already consumed by atlas)     0287 and 0196 intact)
             |                          |
   clients/atlas (browser)        clients/game (TUI)
   RGB layers                     glyph + ANSI
             |
   tile client (future) -- consumes the SAME scene atlas does
   sprite / image asset
```

**The TUI calls the classifier directly; every other client reaches it
through the scene it already consumes.** The layering goal — one
classification, never a second implementation — is met either way, and this
way the plate keeps exact mesh addressing. A future tile client is served by
the scene, exactly as atlas is, and never needs to call Rust.

The register from §2 is therefore a **class registry**, and a glyph is one
*binding* of it. The specimen sheet binds ASCII only; a tile client later
binds sprites to the same classes without renegotiating what a class means.
The ordinal/nominal split survives the lift: an ordinal band maps to ink
density in ASCII and to relief shading in tiles; a nominal class maps to
colour in ASCII and to a sprite in tiles.

### 3.1 Additive only, and the floats stay

`scene/tiles/v1` has a golden byte pin whose test calls it *"the epoch
decision point"*. CLAUDE.md records a `scene/eclipses` v1->v2 bump that was
vetted as correct and quietly **removed** float fields — the gate caught it,
the review did not. A tile client will want raw `elevation_m` for shading
even once a band index exists, so **a band index is added beside the float,
never in place of it.**

## 4. Surface A — the world map (`plate.rs`)

`TileTerrain.ocean: bool` becomes a resolved class read from the scene:

- **Elevation ladder** — ordinal, its own characters, disjoint from the walk
  band's (§2.2).
- **Water** — `water` + `water_legend` splits ocean / lake / river;
  `drainage` sets river prominence. **Rivers appear for the first time.**
- **Landforms** — volcanoes, ranges and rifts, waterfalls, deltas, playas.
  Sim-side today via `has_edifice` / `nearest_boundary_at` / `waterfalls()`
  / `deltas()` / `playas()`; added to the scene additively per §3.1.
- **Rock** — nominal, so it rides the **colour** channel, never a glyph.

## 5. Surface B — the walk band (`chart.rs`)

`PLACED_GLYPH = '+'` dies. `clients/game/core` depends on no hornvale crate
(the containment rule), so it reads the wire — which already carries
`relief`, `micro`, `biome`, `water` and `cover` per cell and `Mark.noun` per
mark. Both halves are reachable without breaking containment:

- **Impedance ladder** `_ . : ^ A`, ported from `surrounds_ascii.rs`. This is
  the open half `CLIENT-illumination-deferred` names, *"with the wire data it
  needs"*.
- **Creature identity** from `Mark.noun`'s initial, collisions resolved by a
  deterministic registry-order rule.

**No authored species table.** The Radiation moved the roster from nine
peoples to fifteen on 2026-08-27 and seed 42 from 192 to 230 settlements; a
hand-kept glyph table would have gone stale that day, which is the failure
`MAP-derivation-outlives-its-wiki` names. The derived rule survives a
sixteenth people arriving with no edit, and degrades to "goblin and gnoll
both draw `g`" — ambiguous, but never wrong.

This closes the coverage audit's **first unmet item**, 2.1 Entities and
Components (`absent`), whose note reads: *"a creature and a boulder are the
same character, on every seed, with no flag that changes it."*

## 6. Surface C — the floor plan, and the hearth

The marks pass already exists and is deliberately inert —
`clients/game/core/src/plan.rs`'s own doc: *"Marks carry no glyph of their
own this campaign."* It gets the same identity rule as §5.

**The hearth is the one item on Nathan's list needing sim-side work.**
Furnishing anchors never reach the wire: `PlanMark` is built only from
creatures found by sight. This adds an anchor -> `PlanMark` emit path in
`windows/vessel`. `Mark.kind`'s own doc says this is designed to be
additive — *"a future kind needs no special case anywhere to appear"* — so
it is a new `kind` value and no special case anywhere.

This is the only task touching `windows/`.

## 7. Performance, and how far the ECS substrate is engaged

Campaign 4 of the ECS program **shipped its ledger half**:
`kernel/src/fact_index.rs` carries SPO/PSO/OSP permutation indexes with
predicate interning, and `Ledger.index` is `#[serde(skip)]`, rebuilt on load
and maintained incrementally, so the save format never widened. It did
**not** ship the component half — `component.rs:5` still reads *"The
dense-`Vec` backend for dense keys ... is the query engine's work (metaplan
§4.5, campaign 4), not here."* No live branch touches `component.rs` or
`fact_index.rs`.

**The win here is derive-once, not new storage.** Classification per *tile*
runs 40,000 tiles x 7 rungs on every redraw. As a **derived component over
vertex-space** it runs once per world over 40,962 vertices and every rung
and redraw becomes a `Vec` index. `VertexMap<T>` (`geosphere.rs:47`) is
already `Vec`-backed and dense-indexed by `Vertex`, and terrain is
vertex-keyed, so **the hot path needs no new substrate.** This is the same
derive-once idiom the kernel already names for `Fbm`.

**Expanding `ComponentStore` is a conditional stage with a named trigger:**
only if the class wants a **facet** key rather than a vertex key, which is a
real possibility at coarse rungs. If the trigger fires it is landing a piece
of ECS substrate and gets a board notice and its own stage boundary.

### 7.1 H1, preregistered

> **H1.** Extracting the terrain classifier into a single shared function —
> called by both `windows/scene`'s tile builder and `plate.rs` — preserves
> The Quadrat's redraw budget AND leaves every committed artifact
> byte-identical.

The Quadrat measured a **0.056 ms warm redraw** at 200x200 on the coarsest
rung, 893x under its 50 ms bar, built on direct mesh addressing and a
`(frame, rung, tile)` cache. Extraction is a refactor of where a
decision is written, not of how a tile is addressed, so the expectation is a
**no-op on both counts** — but an expectation is not a measurement, and a
classifier that newly allocates or newly indirects per tile could still cost
real time at 40,000 tiles.

Falsification has a designed response: if the shared function cannot hold
the budget, `plate.rs` keeps a monomorphized copy **generated from the same
source of truth** rather than hand-written, so the two can still never
disagree. **Recording the null is a result, not a failure** (decision 0016).

Success criteria, frozen before the code:

1. Warm redraw at 200x200, coarsest rung, stays under **0.20 ms** (a ~3.5x
   allowance against The Quadrat's 0.056 ms, 250x under its own 50 ms bar).
   The bar tightened from the draft's 1.0 ms because extraction is a refactor
   rather than a re-architecture: an 18x allowance would have passed a real
   regression silently. Measured on an idle box, replicated, per The
   Quadrat's finding that a loaded box moved the same measurement 1.40x.
2. Byte-identity of committed artifacts is unchanged.
3. `mesh_addressing_agrees_with_the_spatial_search` and the chart's
   `the_shape_matches_the_sims_own_ascii_render` stay green untouched.

## 8. The specimen sheet

Selection happens against a rendered sheet, not a table in this document —
the method that rejected the 22-glyph set. An example binary renders every
candidate ladder at **80x24, in monochrome and in colour, in a real
terminal**. A browser mockup is deliberately not used: the judgment is
"tellable apart at a glance in the medium it ships in".

The sheet is committed as a drift-checked artifact so the next campaign
inherits the *evidence*, not just the verdict — and so `docs/generated-
paths.txt` gains an entry, with the file declared by name as well as its
directory (the already-declared-directory hazard, The Stope Task 2b).

## 9. The Delving handoff

`MAP-underworld-chart` is `spec'd`. The Delving is three campaigns:
campaign 0 (The Adit) shipped the geometry 2026-08-19; **campaigns 1 and 2
are unstarted and unnamed**, and campaign 2 is where the underworld pane's
vocabulary would otherwise be invented. `MAP-underworld-dressing`
explicitly *"waits on MAP-underworld-chart to decide what a subterranean
pane shows."*

The register reserves an unallocated region for subterranean populations, so
Delving 2 **claims** from the register rather than minting a second
vocabulary. A board `notice` naming the paths and the reservation goes out at
campaign start, so the session heading down sees it before it chooses marks.

## 10. Out of scope

Animation, mouse tooltips, per-entity colour, the underworld chart itself,
and anything decision 0070 refuses (health bars, bloodstains). The water
half of `CLIENT-illumination-deferred` stays blocked at walk scale — water
is grid-resolution — which does **not** bind the world map, where the plate
resolves at grid level anyway.

## 11. Risks

1. **H1 falsifies** — designed for; §7.1 names the fallback.
2. **The scene schema needs a field the golden pins.** Additive only, floats
   retained (§3.1); the byte pin moving is the epoch decision point and is a
   hard stop, not a rebaseline.
3. **Delving 1 lands first and touches the marks path.** The reservation and
   the board notice are the mitigation; a conflict surfaces at the sluice
   mouth in milliseconds.
4. **Three renderers must stay in agreement.** `chart.rs`,
   `pane_chart.ts` and `surrounds_ascii.rs` already share a projection rule;
   this campaign adds a shared *vocabulary* and must not add a fourth
   independent implementation of it.
