# The Wash — the map's ink, and the two spines under it

*A wash is a thin layer of dilute colour laid over a drawing, and a
watercolour is built from successive washes. [The
Hachure](2026-09-02-the-hachure-design.md) fixed the world plate's
**geometry** — it reads elevation between the mesh's samples and draws rivers
as lines. This campaign fixes its **ink**, and lays the two spines that let
every later aesthetic campaign be additive.*

Decision ledger: [`2026-09-03-the-wash.md`](../ledgers/2026-09-03-the-wash.md).

## 1. What occasioned it, and the mis-diagnosis it survived

The world plate's colour is keyed on elevation, and only on elevation:

```rust
const RELIEF_GLYPHS: [char; 6] = [' ', '`', ',', ';', '^', '%'];
const RELIEF_COLORS: [[u8; 3]; 6] = [ /* blue, teal, green, olive, brown, white */ ];
fn glyph_and_color_for(water: u8, band: u32) -> (char, [u8; 3])
```

Both ladders index `relief_band(height)`. The two channels carry the same
quantity, and a tropical rainforest and a high desert at 1200 m render
identically. This is a **hypsometric tint** — the school-atlas convention
where green means *low* — and it carries that convention's defect: readers
see green and infer vegetation.

**The first draft of this spec diagnosed that as missing data and proposed
building an appearance system. That was wrong, and the correction is the
reason this campaign is cheap.** Hornvale already has a complete, principled
colour architecture. The world plate is the only view that does not use it.

### What already exists

**`kernel/src/color.rs` — The Pigment.** Colour as a three-way product of
**illuminant × reflectance × observer**, ten spectral bands. Its own module
doc states the architecture this spec was groping toward:

> Colour is not a property of an object. A material has a *reflectance* — the
> fraction of light it returns per wavelength, identical in a cave and at
> noon. Light has a spectrum. An eye has sensitivity curves and collapses the
> arriving mixture to one number per channel. Colour exists only where all
> three meet, which is why every observer variation (species vision, colour
> blindness, a screen reader taking none of it) is the same operation with a
> different observer.

And it is wired, at room scale, across five crates:

| capability | where | shipped |
| --- | --- | --- |
| spectral primitives, `Observer`, `blackbody` | `kernel/src/color.rs` | The Pigment |
| ground reflectance for a place | `windows/locale` `reflectance_at(&Facet, &MicroField, WorldTime)` | yes |
| cover endmembers + spectral **mixture** | `windows/locale/src/surface.rs` `cover_weights` → `Vec<(Reflectance, f64)>` | yes |
| cover classes | `CoverClass { Bare, Chlorophyll, Litter, Snow, Sand, Silt }` | yes |
| rock reflectance | `domains/terrain/src/lithology.rs` | yes |
| built-material reflectance | `windows/vessel/src/fabric.rs` `reflectance_of(fabric, ctx)` | yes |
| daylight + **golden hour** | `domains/astronomy` `daylight(star)`, `at_elevation(base, sun_elevation_deg)` | yes |
| light **sources and gathering** | `windows/vessel/src/light.rs` — The Lantern: `Source`, `attenuate(illuminant, distance)`, `light_field(lattice, sources)`, occlusion via `shadowcast` | yes |
| per-species observer | `windows/vessel/src/eyes.rs` `resolve(eyes, npc) -> (Observer, String)` | yes |

**The plate consumes none of it.** `git grep -c
'Reflectance|Illuminant|CoverClass|color::' clients/game/` returns **0**,
verified at `origin/main` 939829db5.

### Why that answers questions this spec previously proposed to solve

| asked for | already modelled as |
| --- | --- |
| colour of the dirt | lithology reflectance |
| leaf colour, fall, spring green, grass browning | `CoverClass::Chlorophyll` as a **mixture weight**, time-varying |
| snow present or absent | `CoverClass::Snow` endmember |
| hyperlocal moisture and temperature | inputs to the mixture (`MicroField`, climate) |
| golden hour, sunrise, sunset | `at_elevation(base, sun_elevation_deg)` |
| moon-phase lighting | an illuminant; `Calendar::moon_phase(t, index)` exists |
| coastal bioluminescence, aurorae | an `Illuminant`, or a `Source` |
| roads — dirt vs cobble vs gravel | reflectance, over a line (§4.4) |
| masonry | `fabric::reflectance_of` |
| torches, campfires, wildfires, volcanoes, lightning | `light::Source` + `light_field`, with falloff and occlusion |

**Every item is already modelled.** The gap is one missing consumer.

### There is no scale gap

`reflectance_at` takes a **`Facet`** and a **`WorldTime`**; `cover_weights`
takes a **`Vertex`** and a `WorldTime`. `TileTerrain` already carries `facet`
and `vertex`. The pipeline is addressed by exactly the identifiers the plate
already holds, and is already time-varying.

## 2. The reframe that survived the rewrite

**Colour is not the decoder.** Selecting a tile with the cursor conveys what
a reader needs, so the hue budget is not a recall budget. Colour's job is
**perceived variation**. This splits cardinality in two and satisfies
decision
[0389](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0389-a-glyph-carries-order-or-identity-never-category.md)
rather than amending it: its "nothing a reader must trust may live only in
colour" constrains a small trusted layer and thereby *licenses* a continuous
ornamental one.

**And The Pigment answers 0389 better than a fallback table does.** `NO_COLOR`,
a 16-colour terminal, and colour-blindness are not three special cases — they
are three **observers**, and the collapse is one operation. The terminal is
an observer exactly as a creature's eyes are.

## 3. Constraints inherited

- **0389** — glyph carries order or identity; colour carries category and
  substance; weight carries attention; position carries position.
- **[0289](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0289-the-map-is-layers-with-distinct-cache-keys.md)** —
  the map is layers with distinct cache keys. §4.3 extends it.
- **[0287](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0287-a-zoom-rung-is-a-mesh-depth.md)** —
  a tile *is* a facet at the rung's depth. Untouched, and it is why
  `reflectance_at`'s `Facet` addressing fits the plate exactly.
- **[0676](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0676-a-view-may-interpolate-between-its-samples-but-still-may-not-invent-below-them.md)** —
  a view may interpolate within the hull of its samples; it may not invent.
- **[0677](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0677-a-line-carried-feature-conserves-its-length-not-its-rasterized-area.md)** —
  a line-carried feature conserves length. Roads inherit this.
- **[0003](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0003-trace-protocol-is-the-only-cross-domain-channel.md)** —
  the channel carries no producer; a consumer receives appearances, never
  sources. A tile's illuminant does not say whether the sun or a torch made
  it.
- **[0346](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0346-an-affordance-is-derived-never-committed.md)** —
  derived, never committed. §5.
- **Clients are outside determinism** (0055). The observer collapse is a
  client concern; the spectra it consumes are not.

## 4. Design

### 4.1 The plate joins the spectral pipeline

Per tile, replacing `glyph_and_color_for(water, band)`:

```
  reflectance   locale::reflectance_at(facet, micro, at)   the ground's own curve
  illuminant    astronomy::at_elevation(daylight(star),    ONE per frame, at the
                                        sun_elevation)     OBSERVER's latitude
  observer      the terminal's own                          truecolor / 256 / 16 / none
  ---------------------------------------------------------------------------
  channel       observer.collapse(reflectance x illuminant)
```

The sim supplies **spectra**; the client supplies the **observer** and owns
the collapse. That is quantize-at-emit-only applied to ink: the world says
what the ground returns and what light falls on it; the display says what a
screen can show. No sim code emits an RGB triple.

**The glyph is untouched.** It keeps elevation order (0389-legal, already
correct). What changes is that colour stops duplicating it.

### 4.2 The observer is where colour degrades, and it is one mechanism

Not a fallback table. Four observers over one pipeline:

| observer | collapse |
| --- | --- |
| truecolor | full three-channel |
| 256 / 16-colour | nearest in the terminal's own palette |
| `NO_COLOR` | none — the glyph carries elevation, unaffected |
| colour-vision variants | a different sensitivity curve; same operation |

`windows/vessel/src/eyes.rs` already resolves an observer per creature. The
terminal's observer is the same kind of object.

### 4.3 The rate spine, and why it is load-bearing on day one

A layer's rate is its cache key (extending 0289), and **a layer may never
read data that changes faster than its own rate.**

```
  rate           cache key changes on      layer          this campaign
  --------------------------------------------------------------------
  geological     never                     terrain        (existing key)
  built          a build or destruction    slot only      —
  seasonal       season boundary           REFLECTANCE    SHIPS
  diurnal        sun/moon movement         ILLUMINANT     SHIPS
  per-turn       turn                      perception     exists
  ornamental     marquee tick (~300 ms)    slot only      —
  instantaneous  cursor move               selection      exists
```

**The invariant is not hypothetical here — it is the campaign's first bug.**
`reflectance_at` takes a `WorldTime`, so reflectance is a **seasonal**-rate
quantity. The terrain layer's cache is currently **never invalidated**.
Putting reflectance into that layer without changing its key is exactly the
defect the invariant names: correct on the first frame, then frozen, and
nearly invisible thereafter.

The **built** rung exists because roads and masonry change when something
builds or destroys them — slower than per-turn, faster than never.

The ornamental rung already has its mechanism
(`clients/game/bin/src/main.rs`): `MARQUEE_TICK` at 300 ms, poll-with-timeout,
animate on expiry, only when something wants animating. No layer ships on it
here.

### 4.4 The three shapes of source, all of which already have mechanisms

| shape | example | mechanism | shipped |
| --- | --- | --- | --- |
| **field** | cover mixture, snow, chlorophyll | `reflectance_at` / `cover_weights` | yes |
| **placed surface** | road, cobble, plaza, masonry | `fabric::reflectance_of`, sparse override | yes |
| **emitter** | torch, campfire, wildfire, volcano | `light::Source` + `light_field` + `attenuate` | yes, room-scale |

- **A road is a line**, drawn by the polyline rasterizer The Hachure built
  under 0677, whose reflectance differs from the ground it crosses. River,
  gallery forest and road are one type; that rasterizer wants to become a
  general linear-feature renderer, noted and not acted on here.
- **Masonry splits**: a horizontal built surface is a reflectance override; a
  vertical structure is a **mark**, not a surface, because it occupies the
  tile rather than being the ground of it.
- **A torch is per-turn *and moving*** — attached to a creature, so it can
  never live in a geological or seasonal cache; it belongs with the
  perception layer, which already redraws per turn.

### 4.5 The illuminant is anchored to the observer, not to the tile

**CORRECTION (2026-09-03, before Task 5). An earlier draft of this section
said the illuminant is "diurnal and uniform across the plate". That is false
at coarse rungs and the campaign would have shipped it.**
`Calendar::solar_altitude_at(t, latitude)` depends on latitude *and* hour
angle. A plate at globe rung spans the whole planet — every latitude and
every longitude — so a single illuminant lights the night side as if it were
noon. There is no rung-independent sense in which one illuminant is correct
for a whole map.

**The decision: the map is lit as it is WHERE THE READER STANDS.** One
illuminant per draw, computed at the observer's own latitude and the
session's own instant, applied uniformly. This follows the precedent
`windows/vessel/src/eyes.rs:81-97` already sets at room scale, where a single
observer latitude is obviously right; the plate does the same thing at a
scale where the approximation is weaker, and says so.

**What that costs, stated rather than hidden:** no terminator sweeps the map.
At a coarse rung the far side of the world is lit by the reader's sun. That is
a cartographic convention — a map is a document you consult, not a satellite
photograph — and it is the reading
`CLIENT-map-is-invitation-not-data-dump` already asks for. A terminator, if
ever wanted, is a per-tile illuminant and a different campaign; it would
forfeit the hoist below.

**The hoist is what makes it cheap.** The naive implementation recomputes the
illuminant for all ~4,800 tiles of a plate; anchoring it to the observer makes
one computation per draw correct *by construction* rather than by
approximation. Only the reflectance varies per tile.

Per-tile reflectance is a **derived value keyed by facet**, which is what
`kernel/src/component.rs`'s `ComponentStore<K: Ord, C>` and
`kernel/src/derived.rs` exist for — one generic store per value *shape*,
deliberately not heterogeneous (`TypeId` ordering is not build-stable and
would put an unstable iteration order under a byte-identity guarantee).

So the cache is keyed `(Facet, season)` and invalidated on the season
boundary, per §4.3. This is a genuine use of the substrate rather than a
decorative one: without it, every redraw re-integrates a spectral mixture per
tile.

**Measured before optimised.** The plan must record a baseline redraw cost
before the change and after, on the client the player actually launches —
`clients/game` carries its own dev profile, and a measurement taken in a
different profile is not a measurement of this.

## 5. Determinism

**Appearance is derived, never committed, never read back by sim logic.** No
spectral value enters the ledger; nothing in `domains/` or `windows/`
branches on one. Precedent: 0346.

This is nearly free here, because the campaign is a **consumer**: it reads a
pipeline that already exists rather than writing new state.

**No epoch. No new seed labels. No stream draws.** H3 asserts byte-identity
as the acceptance gate.

## 6. Preregistered measurement

Frozen before the code that would move it (decision 0016).

**H1 — the category error is fixed.** Two tiles of equal elevation band but
different cover render in different channels, and distinct rendered colours
per plate strictly increases at every shipped rung.

*Failure mode named in advance:* The Hachure's H2 was falsified because it
asserted about a **banded** quantity whose quantizer erased the refinement.
So H1 measures **distinct rendered colour count**, downstream of no band, and
never a banded value.

**H2 — the seasonal and diurnal layers are real, and directional.** At one
seed and location: midwinter differs from midsummer, and dawn differs from
noon.

**Narrowed by §4.5's correction:** "dawn differs from noon" is about the
READER's dawn — the observer's own hour — not about a terminator crossing the
map. H2 asserts the map's ink responds to the reader's time of day. It does
not assert, and this campaign does not deliver, per-place lighting.

*Failure mode named in advance:* inequality is trivially satisfied by any
change. So H2 asserts **direction** — snow-endmember weight strictly greater
at midwinter; illuminant strictly warmer (lower colour temperature) at low
sun. A null on direction is a finding and ships as one.

**H3 (the control) — appearance changes no world.** A world generated with
the plate's spectral consumption present is **byte-identical** to one
without. The campaign's acceptance gate.

**H4 — the rate invariant holds.** No layer reads data changing faster than
its own rate. A deliberately mis-declared layer must be caught: **red before
green.**

**H5 — the observer degrades without loss of legibility.** Under `NO_COLOR`
the plate remains readable, because the glyph still carries elevation. Stated
as a test, not an intention.

## 7. Out of scope, with reasons

- **World-scale gathering.** `light_field` is room-scale: it walks a
  `Lattice` and reuses `shadowcast` for occlusion, neither of which exists at
  map scale. Wildfires and volcanoes as map-scale emitters need a different
  reach model. **The mechanism exists and does not transfer unchanged** —
  recorded precisely because the first draft of this spec wrongly called
  gathering unbuilt, and the opposite error (assuming it transfers) is
  equally available.
- **Any ornamental layer.** Rung and mechanism exist; no layer ships on it.
  Ocean surface motion is the obvious first and is a separate campaign.
- **`MAP-coherent-detail-field`** — The Hachure's deferred Stage 3 and its
  `LOCALE_MICRO` epoch. Still deferred: it *invents* detail below the datum,
  and doing that before the map consumes the detail it already has is the
  wrong order.
- **Biome taxonomy depth** — sub-biomes, a vegetation-structure axis
  separating scrub from tree savannah. `Shrubland` and `Savanna` are already
  distinct formations; the finer split is sim work, not rendering work.
- **Direction as a channel** — currents, wind, flow, terminator sweep are
  vectors the world has and the map has no channel for. Surfaced by the
  second ideonomy pass; not closed here.
- **The `relief_band` floors.** Unchanged; load-bearing for a shipped wire
  field, and `windows/scene/src/surrounds.rs` says so in terms.

## 8. Decisions needing ratification

1. **A view consumes spectra and owns its observer; the sim never emits a
   colour.** The world says what the ground returns and what light falls on
   it; the display says what it can show. Quantize-at-emit-only applied to
   ink. *The constitutional item.*
2. **A layer's rate is its cache key, and a layer may never read data that
   changes faster than its own rate.** Extends 0289. Load-bearing on day one:
   reflectance is seasonal and the terrain cache is never invalidated.
3. **Colour degradation is an observer, not a fallback.** `NO_COLOR`,
   16-colour and colour-vision variants are one operation with different
   sensitivity curves — the same claim The Pigment already makes for species
   vision, extended to the terminal.
4. **Reported, not decided:** `biome_class` collapses reef, kelp, open water,
   sea ice, ice, alpine and caves into `Barren` — right for subsistence,
   wrong for appearance. No change proposed; recorded so a later reader does
   not "fix" one by breaking the other.
