# The Wash — the map's ink, and the two spines under it

*A wash is a thin layer of dilute colour laid over a drawing, and a
watercolour is built from successive washes. [The
Hachure](2026-09-02-the-hachure-design.md) fixed the world plate's
**geometry** — it reads elevation between the mesh's samples and draws rivers
as lines. This campaign fixes its **ink**, and lays the two spines that let
every later aesthetic campaign be additive.*

Decision ledger: [`2026-09-03-the-wash.md`](../ledgers/2026-09-03-the-wash.md).

## 1. What occasioned it

The map's colour is keyed on elevation, and only on elevation:

```rust
const RELIEF_GLYPHS: [char; 6] = [' ', '`', ',', ';', '^', '%'];
const RELIEF_COLORS: [[u8; 3]; 6] = [ /* blue, teal, green, olive, brown, white */ ];
fn glyph_and_color_for(water: u8, band: u32) -> (char, [u8; 3])
```

Both ladders are indexed by `relief_band(height)`. **The two channels carry
the same quantity, and biome carries nothing.** A tropical rainforest and a
high desert at 1200 m render identically.

Meanwhile `domains/climate` holds a compositional
`BiomeExpr { realm, formation, stratum }`, and `windows/scene/src/region.rs`
already ships `biome: Vec<u16>` and a `biome_legend` over the wire. The data
exists, reaches clients, and is discarded at the last step.

**This is a hypsometric tint** — the school-atlas convention where green
means *low* — and it carries that convention's famous defect: readers see
green and infer vegetation. Hornvale renders the Sahara green for the same
reason a 1900 atlas does.

Verified at `origin/main` **939829db5**, after `merge(the-prospect)` landed
+1121 lines on `plate.rs` mid-brainstorm: both ladders still six-wide and
band-indexed, `TileTerrain` still carries no biome, `glyph_and_color_for`
unchanged. The finding was first taken at `12612b1d1`; a stale read would
have been invisible, so it was re-taken rather than assumed.

## 2. The reframe: two constraints that changed the design

**(a) Colour is not the decoder.** Selecting a tile with the cursor conveys
the information a reader needs. So the hue budget is *not* a recall budget,
and the design's earlier anchor — "8–12 colours is all a reader can name" —
was answering a question nobody asked. Colour's job here is **perceived
variation**, not decoding.

This splits cardinality in two, and the split is what satisfies decision
[0389](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0389-a-glyph-carries-order-or-identity-never-category.md)
rather than amending it:

| layer | cardinality | fallback | 0389 |
| --- | --- | --- | --- |
| **trusted** | small, nameable, legend-backed | survives `NO_COLOR`, has a glyph twin | constrained by "nothing a reader must trust may live only here" |
| **ornamental** | continuous, unnameable | degrades to nothing, costs nothing | *licensed* by the same clause |

0389's restriction on the first layer is precisely what permits the second.

**(b) Appearance is data the sim should supply, richly.** Dirt colour, leaf
colour in autumn, snow, hyperlocal moisture and temperature, grass browning,
spring green, blooms, golden hour, moon-phase light, coastal
bioluminescence, aurorae. Every one of those is a **named scalar over
(space × time)** — the kernel's own definition of a *Field*.

## 3. Constraints inherited

- **Decision 0389** — glyph carries order or identity; colour carries
  category and substance; weight carries attention; position carries
  position. A character means one thing across the whole client.
- **Decision [0289](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0289-the-map-is-layers-with-distinct-cache-keys.md)** —
  the map is layers with distinct cache keys.
- **Decision [0287](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0287-a-zoom-rung-is-a-mesh-depth.md)** —
  a tile *is* a facet at the rung's depth. Untouched here.
- **Decision [0676](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0676-a-view-may-interpolate-between-its-samples-but-still-may-not-invent-below-them.md)** —
  a view may interpolate within the convex hull of its samples; it may not
  invent. The lightness modulation in §4.2 is interpolation, not invention.
- **Decision [0677](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0677-a-line-carried-feature-conserves-its-length-not-its-rasterized-area.md)** —
  a line-carried feature conserves length, never rasterized area.
- **Decision [0003](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0003-trace-protocol-is-the-only-cross-domain-channel.md)** —
  the phenomena channel does not carry a producer; a consumer receives
  appearances, never sources. §4.4 turns out to be this rule applied to
  rendering.
- **Decision [0346](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0346-an-affordance-is-derived-never-committed.md)** —
  an affordance is derived, never committed. §5 is the same rule for the
  same reason.
- **Clients are outside determinism** (0055). Ornamental layers owe nothing
  to byte-identity. The *sim-side axes* owe everything to it.

## 4. Design

### 4.1 The axis spine (sim): appearance is an open vector of named scalars

`kernel/src/ecology.rs` already carries the primitive:

```rust
pub struct EnvironmentVector(BTreeMap<u16, f64>);
// axes today: PHYSIOGNOMY · ENERGY · WATER · SUBSTRATE · LIGHT
```

Open, named, scalar, deterministic, `BTreeMap` not `HashMap`. New axes cost
nothing structural. **The sim exposes axes; the client composes pixels. The
sim never emits an RGB value** — that is a display fact, and quantizing to a
display budget in the compute path is the error this design exists to avoid.

The split is *discovery vs. invention*, and it is the kernel's own
quantize-at-emit-only discipline applied to ink:

- **discovered, sim-side**: what a formation's ground is *made of*;
  whether snow lies; where the sun is. Facts about the world.
- **invented, client-side**: how many hues this terminal has; which RGB
  triple stands for sand. Facts about the display.

Six of the twelve items in §2(b) are already modelled or directly derivable
(`SUBSTRATE`, `LIGHT`, `WATER`, `ENERGY`, `micro.wetness`, terrain sediment).
The rest are **additive axes on an existing vector** — never structural
change.

### 4.2 The rate spine (client): a layer's rate is its cache key

Decision 0289 already makes the map layers with distinct cache keys, and
those keys already sit at distinct rates. This spine names the ladder and
makes it extensible:

```
  rate           cache key changes on      layer          this campaign
  --------------------------------------------------------------------
  geological     never                     terrain        SHIPS (Stage 1)
  built          a build or destruction    (slot only)    —
  seasonal       season boundary           (slot only)    SHIPS (Stage 2)
  diurnal        sun/moon movement         (slot only)    SHIPS (Stage 3)
  per-turn       turn                      perception     exists
  ornamental     marquee tick (~300 ms)    (slot only)    —
  instantaneous  cursor move               selection      exists
```

**The invariant, and it is the one worth testing:** *a layer may never read
data that changes faster than its own rate.* Without it, a future seasonal
layer gets silently baked into the never-invalidated terrain cache — a bug
that renders correctly on the first frame and is nearly invisible
thereafter.

The **built** rung exists because roads and masonry change when something
builds or destroys them: slower than per-turn, faster than never. Without
the rung a road is either baked into the terrain cache (and survives its own
destruction) or redrawn every turn for nothing.

The ornamental rung already has its mechanism —
`clients/game/bin/src/main.rs`:

```rust
const MARQUEE_TICK: Duration = Duration::from_millis(300);
if driver.strip_is_scrolling() && !poll(MARQUEE_TICK)? { driver.tick_marquee(); }
```

Poll-with-timeout, animate on expiry, only when something wants animating,
zero cost when nothing moves. The Wash ships no ornamental layer but does
not need to invent this.

### 4.3 The three shapes of appearance source

| shape | example | mechanism | dense |
| --- | --- | --- | --- |
| **field** | cover, snow, sun altitude | sampled per tile | yes |
| **placed surface** | road, cobble, plaza | sparse override on the field | no |
| **emitter** | torch, campfire, wildfire, volcano, lightning | contributes to neighbours with falloff | no |

- A **road is a line**, drawn by the polyline rasterizer The Hachure built
  under 0677, whose cover differs from the ground it crosses. River, gallery
  forest and road are the same type; that rasterizer wants to become a
  general linear-feature renderer, which is noted and not acted on here.
- **Masonry splits**: a horizontal built surface is a cover override
  (`cover_at(t) = placed(t).unwrap_or(derived_cover(formation))`); a vertical
  structure is a **mark**, not a cover, because it occupies the tile rather
  than being the ground of it. Settlements are already drawn as marks.
- **Emitters need no change to the vector**, and §4.4 says why.

### 4.4 One axis, two composition rules

`LIGHT` is already an axis. Ambient sources — sun, moon, aurora — *write* it
as a sampled field. Emitters — torch, campfire, wildfire, volcano, lightning
— *contribute* to it by gathering within a falloff. **The client reads one
scalar and cannot tell which produced it.**

That is decision 0003's discipline exactly: the channel does not carry a
producer, and a consumer receives appearances rather than sources. The
appearance vector is the Phenomena pattern applied to rendering.

**The Wash implements sampling only.** Gathering is a second mechanism and is
a non-goal — see §7, where the reason is recorded, because the failure it
guards against is a later campaign finding the vector, assuming it covers
torches, and forcing an emitter into a sampled shape.

One case does not sort by rate alone and is recorded so it is not
rediscovered: **a torch is per-turn *and moving*.** Attached to a creature,
it can never live in the terrain cache; it belongs with the perception
layer, which already redraws per turn.

### 4.5 The three stages

**No stage requires new physics. Every input is shipped API**, verified
before the scope was accepted rather than after.

| stage | axis | rate | built from | new physics |
| --- | --- | --- | --- | --- |
| **1** | `cover` | geological | `Formation` (19 variants) | none — a derivation |
| **2** | `snow_cover` | seasonal | `temperature_at(…, vertex, day)` + moisture | none — shipped |
| **3** | `sun_altitude` | diurnal | `Calendar::solar_altitude_at(t, latitude)` | none — shipped |

**Stage 1 — cover, and the channel split.** `cover` is *derived* from
`Formation` rather than stored: no new state, no wire change, no epoch. A
table is where taste hides unaudited; a function beside `Formation` is a
claim anyone can read and dispute. Two properties it must have:

- **It does not collapse what looks different.** Reef, kelp, open water and
  sea ice are four covers because they are four substances. This is the
  structural repair of `biome_class`, which collapses all four plus ice,
  alpine and caves into `Barren` — correct for "can people live off this",
  catastrophic for "what does this look like". Two questions, two partitions
  of one set; that is where a second taxonomy is *earned*.
- **It is partial-safe.** A new formation returns a fallback cover rather
  than failing to compile the renderer. The taxonomy is going to hundreds
  and the map must not gate additions to it.

The client then carries:

| channel | carries | budget | when colour fails |
| --- | --- | --- | --- |
| hue | cover | ~10–13, one per cover | glyph still carries elevation |
| glyph | elevation band | 6, unchanged | unaffected |
| lightness | sub-band height (0676) | continuous | collapses to flat hue |

`glyph_and_color_for(water, band)` becomes `(cover, band, height_asl)`. The
plate owns the `Cover → RGB` table **and** its own downsampling to
16-colour and to none.

**Stage 2 — `snow_cover`, seasonal.** `temperature_at` already accepts a day
and models obliquity, insolation, year phase and rotation regime.

**Stage 3 — `sun_altitude`, diurnal.** `Calendar::solar_altitude_at(t,
latitude)` already exists, alongside `solar_azimuth_at`, `season_phase`,
`daylight_fraction_at`, `is_daylight` and `moon_phase`.

**The real cost is plumbing, not modelling.** `terrain_at_tile` currently
receives `&GeneratedTerrain`; Stages 2 and 3 need climate, the calendar and
a `WorldTime` to reach it. That is the honest risk of this campaign, and it
is a known quantity rather than a research question.

## 5. Determinism and the invariant

**Appearance is derived, never committed, and never read back by sim
logic.** No appearance axis enters the ledger; nothing in `domains/` or
`windows/` branches on one. Precedent: 0346, an affordance is derived, never
committed — the same rule, for the same reason, on a different derived
quantity.

This is what makes "it does not touch the simulation" **structurally** true
rather than a thing someone must remember across campaigns. It is stated as
a testable claim in §6 (H3), not as an intention.

No epoch. No new seed labels. No stream draws — every axis is a pure
function of committed state plus time.

## 6. Preregistered measurement

Frozen before the code that would move it (decision 0016).

**H1 (Stage 1) — the category error is fixed.** Distinct hues rendered per
plate strictly increases at every shipped rung, and two tiles of equal
elevation band but different formation render in different hues.

*Failure mode named in advance:* The Hachure's H2 was falsified because it
asserted about a **banded** quantity and the band was a quantizer coarse
enough to erase the refinement. So H1 measures **hue count**, which is
downstream of no quantizer, and never a banded value.

**H2 (Stages 2–3) — the seasonal and diurnal layers are real, and
directional.** At one seed and location: midwinter differs from midsummer,
and dawn differs from noon.

*Failure mode named in advance:* mere inequality is trivially satisfied by
any change whatever. So H2 asserts **direction**: snow extent strictly
greater at midwinter than midsummer; tint strictly warmer at low sun than at
high. A null on direction is a finding and ships as one.

**H3 (the control) — appearance changes no world.** A world generated with
the appearance axes present is **byte-identical** to one generated without
them. This is the one that can fail catastrophically and silently, and it is
the campaign's acceptance gate.

**H4 (the rate invariant) — no layer reads faster-changing data than its own
rate.** Asserted as a test over the layer registry, red before green: a
deliberately mis-declared layer must be caught.

## 7. Out of scope, with reasons

- **Gathering / emitters.** The axis spine covers *fields*: dense, sampled,
  per tile. Torches, campfires, wildfires, volcanoes and lightning
  *contribute* to an axis rather than sampling it, and that is a second
  mechanism this campaign does not build. **Recorded with its reason
  deliberately:** without this paragraph a later campaign finds the vector,
  assumes it covers emitters, and forces one into a sampled shape — losing
  falloff and occlusion, and producing a plausible result that is wrong.
- **Any ornamental layer.** The rung and its mechanism exist; no layer ships
  on it here. Ocean surface motion is the obvious first one and is a
  separate campaign.
- **`MAP-coherent-detail-field`** — The Hachure's deferred Stage 3, the
  coherent noise field and its `LOCALE_MICRO` epoch. Deliberately still
  deferred: it *invents* detail below the datum, and doing that before the
  map stops discarding the data it already holds is the wrong order.
- **Biome taxonomy depth** — sub-biomes, microbiomes, a vegetation-structure
  axis to separate scrub from tree savannah. `Shrubland` and `Savanna` are
  already distinct formations; the finer split is not, and it is sim work,
  not rendering work.
- **Gallery forests and roads as content.** The *mechanism* is named in §4.3;
  neither is modelled today.
- **Direction as a channel.** Currents, wind, flow and terminator sweep are
  vectors the world has and the map has no channel for. Surfaced by the
  second ideonomy pass; not closed here.
- **The `relief_band` floors.** Unchanged. They are load-bearing for a
  shipped wire field and `windows/scene/src/surrounds.rs` says so in terms.

## 8. Decisions needing ratification

1. **Appearance is exposed as named scalar axes; the sim never emits a
   colour.** Discovered facts sim-side, display quantization client-side —
   quantize-at-emit-only applied to ink. *This is the constitutional item.*
2. **A layer's rate is its cache key, and a layer may never read data that
   changes faster than its own rate.** Extends 0289 with the ladder and the
   invariant.
3. **Appearance is derived, never committed, never read back.** Extends
   0346's shape to a second derived quantity, and is what keeps the
   simulation untouched by construction.
4. **One axis may have two composition rules — sampling and gathering — and
   a consumer cannot tell which produced a value.** 0003's phenomena
   discipline, applied to rendering.
5. **Reported, not decided:** `biome_class` is the wrong partition for
   appearance and right for subsistence. No change is proposed to it; the
   observation is recorded so the next reader does not "fix" one by breaking
   the other.
