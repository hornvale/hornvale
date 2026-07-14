# The Ground — Design

**Date:** 2026-07-14
**Status:** Approved (brainstorming session)
**Campaign:** The Ground (slug-named per decision 0026)
**Provenance:** A new terrain-family substrate — *what the ground is made of*, distinct from the terrain roadmap's *shape* axis (Measured Coast → Crust → **Sculpting**). Sequenced **before Sculpting**: The Ground is a pure classification layer that changes no existing byte (no epoch), and Sculpting will later *consume* its hardness/erodibility. Four ideonomy passes (2026-07-14) hardened the design from a flat rock-type enum into a material-buffer-and-projections architecture; their load-bearing pulls are folded in throughout and called out where they changed a decision.

---

## 1. Goal

Give the world a **lithology and pedology substrate**: for any position, *what rock is here, what soil is on it, and what the ground does with water* — as a **neutral field that settlement, ecology, the ore/economy layer, and the walk each interpret independently**, none learning how the material got there. All of it is a **pure deterministic function of fields that already exist** (crust age/thickness, boundary tectonic setting, plate motion, hotspots, drainage, climate temperature/moisture). No new randomness, no new save-format contract, no change to any existing world. New *derived observations* are added (maps, an almanac section, census columns); the seed → world mapping is untouched.

This is **not a terrain epoch.** World identity — elevation, drainage, climate, every committed byte — is byte-for-byte unchanged. The campaign only *adds* readouts.

## 2. Architecture: the material buffer and its projections

The central design decision (ideonomy round 1's abstraction-lift, reconfirmed round 2 and named round 3): **do not emit a rock class as the primitive.** A single class enum is one *codebook* compressing a continuous material state, and the four consumers each want a different codebook — settlement wants workability and fertility, the ore layer wants mineralization, the walk wants appearance underfoot, ecology wants parent chemistry. Hard-coding one shared enum re-introduces at the taxonomy level exactly the mistake we avoid at the architecture level.

Instead, the primitive is a **material buffer**: a small per-cell vector of continuous petrogenetic properties. Every consumer-facing output is a **projection** — a pure function of the buffer (plus climate where the projection needs it). This mirrors two existing patterns: Hornvale's own doctrine (fields are the primitive prior; classes are a derived observation), and the language domain's **distinctive-feature bundles** (phonemes as points in feature space, not a flat list). The abstraction-lift's cleanest name for it is a **material G-buffer** — stored once, read by many shading passes, none of which re-derive the source.

**The buffer** (per cell, all continuous unless noted):

- `silica` — felsic↔mafic index (granite/rhyolite high, basalt/gabbro low)
- `grain` — grain size / crystallinity (plutonic-coarse ↔ volcanic-fine ↔ clastic)
- `induration` — hardness / erodibility (the axis Sculpting will read)
- `carbonate` — carbonate content (the karst and marble axis)
- `metamorphic_grade` — 0 (unaltered) → 1 (gneiss/granulite)
- `porosity` — permeability (the aquifer / karst / runoff axis)
- `margin` — active/passive/interior **polarity** (round 3, symmetry): derived from the cell's position relative to its plate's motion direction (the Euler pole already in `plates.rs`). Leading edge = active (arcs, narrow shelf); trailing = passive (wide shelf, thick sediment).
- `soil_depth` — regolith thickness (round 3, direction): an accumulation-vs-stripping balance of slope, drainage, weathering rate, and age. Distinct from soil *type*, and often more load-bearing — you cannot plow, root, or found a building on thin regolith over granite.
- `column` — a shallow **2-layer stack** (round 4, cardinality): `cover` (the surface depositional/volcanic rock) over `basement` (craton granite/gneiss — the crust field already distinguishes basement from cover). The full stratigraphic column is deferred (§8); this cheap two-layer version serves deep mining, well depth, and the underdark vertical axis for almost nothing.

**Placement.** The buffer and its terrain-native projections live in a new `domains/terrain/src/lithology.rs`, implementing the kernel `Field` trait (any grid at any level samples the same buffer; coarse-constrains-fine is exact). Projections that need climate (soil, hydrogeology) are **pure classifier functions in the terrain domain taking kernel/primitive inputs** (`Temperature` is a kernel type; moisture is a bare `f64`), **wired by the worldgen composition root**, which already hosts terrain×climate functions and already threads terrain into climate. Terrain gains **no** dependency on the climate crate — the same dependency-injection seam already in `windows/worldgen/src/lib.rs`.

**Typed quantities.** Buffer axes that are coherent physical quantities cross the API as newtypes (e.g. a `SoilDepth` in metres); dimensionless indices in [0,1] stay bare `f64` with `type-audit: bare-ok(ratio)` tags. Every new `pub` primitive carries its type-audit verdict (CI is default-deny).

## 3. The projections

Each is a pure function of the buffer (± climate). Each is independently addable and shippable (§7 sequencing).

- **Rock class** — the fine taxonomy (§4), a convenience projection over the buffer. Consumers who want coarse (igneous/sedimentary/metamorphic) project further; the buffer makes fine-vs-coarse a non-decision.
- **Appearance vector** — albedo/colour/grain the walk reads as "dark, fine-grained, hard underfoot," not "andesite." Optional **footstep-sound** channel (round 4, music→timbre): gravel crunch vs clay squelch vs stone. A near-free walk bonus.
- **Soil order + fertility** — the ~9 soil orders (§4), climate-dominated and parent-modulated, plus a **fertility vector** (round 1, terroir): not a scalar but a small suitability vector to be matched against crop needs the way MAP-22 matches niches to species. Even with no consumer yet, emitting the vector shape avoids baking in a scalar we would regret when agriculture arrives.
- **Hydrogeology** (round 2, the rock×water cross-product) — aquifer / spring / aquitard, a pure function of `porosity` × drainage. This is settlement's water story (wells, oases in arid zones) and half the walk's texture (springs), for the cost of one projection.
- **Karst / cave-proneness** (round 2, negation "solid → void") — `carbonate` × drainage → void-proneness. The one place lithology touches the **underdark vertical axis** MAP-22 reaches for; small and strategically connective.
- **Hazard-proneness** (round 4, rate) — *optional, low priority.* A static proneness readout (volcanic from arc/hotspot cells, seismic from the existing `unrest_at`, landslide from shale-on-slope, karst-collapse). Proneness only — hazard *events* need time and are out of scope.
- **Prospectivity** (round 1, distribution) — a derived P(mineral deposit) field from arc/fault/hydrothermal setting. Not point deposits themselves (those need their own draw — see §8); a probability field a later deposits campaign turns into actual ore bodies.

## 4. The taxonomy (fine)

The buffer makes the class list a projection, not a commitment — but the projection targets a fine, legible set, each class earning its place by meaning something different to at least one consumer. Organised by genesis (the well-formed tree of ideonomy round 2), with rock-cycle position carried as internal structure so provenance/destiny come free:

**Igneous** — granite (craton cores, collision roots) · gabbro (deep oceanic/rift) · basalt (ridges, hotspots, rift, ocean floor) · andesite (subduction arcs) · rhyolite (continental arc/caldera).
**Sedimentary, clastic** — sandstone (near-orogen lowland, coast) · shale (quiet deep basin) · conglomerate (proximal to uplift).
**Sedimentary, chemical precipitate** — evaporite (arid endorheic — the banked salt-basin hook) · chert (abyssal pelagic) · **ironstone/BIF** (round 2 — a rock that is *also* iron ore, the natural bridge between the neutral field and the ore consumer, living inside the field).
**Sedimentary, biogenic** — reef-limestone · **coal/lignite** (round 2 — ties soil, climate, and deep time).
**Metamorphic** — slate → schist → gneiss (graded by `metamorphic_grade`) · marble (carbonate parent in an orogen) · quartzite (sandstone parent).
**Unconsolidated** — alluvium (high-drainage lowland — the fertile floodplains).

**Soil orders (~9, climate-dominated, parent-modulated)** — laterite (hot+wet) · podzol (cold conifer) · chernozem (grassland — the breadbasket) · aridisol (desert; salt-flat over evaporite) · loam/alfisol (temperate forest — good farmland) · andosol (fresh volcanic — very fertile) · leptosol (thin rocky, steep/young peaks) · histosol/peat (waterlogged) · gley (poorly-drained mineral).

The active/passive **margin polarity × elevation position** chart (ideonomy round 3) is the classifier's backbone: it makes each continent's leading and trailing coasts genuinely different (narrow volcanic shelf vs wide carbonate shelf) from Euler-pole data already present, and it is well-formed enough (few nonsense cells) to double as the classifier's test oracle.

## 5. Determinism and the no-epoch guarantee

- The classifiers are **pure functions of existing fields.** Sub-cell patchiness reuses **existing noise** — **no new stream labels, no new stream consumption order, no save-format contract.** Elevation, drainage, climate, and every committed byte stay identical.
- World identity (seed → world) is untouched, so this is **not an epoch**: no re-baseline of existing artifacts, no golden-pin churn, no identity-keystone retirement.
- New *derived observations* are added — a lithology map, a soil map, an almanac "The Ground" section, new census columns. Those are additions, drift-checked like every artifact. Their **census regeneration happens once at campaign close on the AWS box** (per the census-on-AWS policy; censuses are never regenerated locally). The `lens_purity` world-identity fixture must stay green throughout — any failure means an unintended epoch.
- The buffer implements `Field`; the canonical-grid/views contract (decision 0038) holds: pointwise buffer sampled at any resolution, grid-bound projections (anything reading drainage/neighbours) computed on the canonical grid.

## 6. Deliverables and consumer surface

1. **Surface latent tectonic fields** (the cheap terrain-native prerequisite): expose hotspots as a queryable field, per-`BoundaryKind` distance bands, and a metamorphic-grade proxy from `maturity` × collision-distance. Pure surfacing of data the generators already hold.
2. **The material buffer** + `lithology_at(cell)` / `soil_at(cell)` / hydrogeology / karst queries on the terrain provider and (for climate-coupled projections) worldgen.
3. **Lenses:** a lithology map and a soil map (PPM, like the elevation map — strong visuals); an almanac "The Ground" section (dominant rock/soil, notables: karst country, salt flats, volcanic provinces, breadbasket soils).
4. **Census metrics:** rock-class fractions, soil-order fractions, fertile-land fraction, karst fraction, aquifer fraction — added to the-census, drift-checked, regenerated at close.
5. **Book (Definition of Done):** a chronicle entry and a laboratory/reference chapter; a freshness sweep of any chapter the new fields touch; a new idea-registry row for the substrate, and a Confidence-Gradient re-score if the campaign resolves one of its bets. A one-page retrospective in `docs/retrospectives/`.
6. **Concepts/facts:** register the lithology concepts; ledger facts/phenomena emission is deliberately minimal this campaign (§8) — the field and its observations are the deliverable.

## 7. Implementation sequencing

The complexity prompt (round 4) confirmed the **buffer is the load-bearing kernel**; every projection is independently addable. So:

1. Surface latent tectonic fields (§6.1).
2. The material buffer + `Field` impl + the margin-polarity and soil-depth axes, with the polarity×position chart as the test oracle.
3. Rock-class projection + the fine taxonomy + the lithology map lens.
4. Soil order + fertility vector (first climate-coupled projection, wired in worldgen) + the soil map lens.
5. Hydrogeology + karst projections.
6. Appearance vector (+ optional footstep-sound); optional hazard-proneness; prospectivity.
7. Almanac section, census metrics, book, retrospective, close (with AWS census regen).

Each step compiles, passes, and is a shippable increment.

## 8. Non-goals (deliberate scope fence, with hooks)

- **No consumer rewiring.** Settlement / ecology / the walk *reading* this field is follow-on work; they get a clean field, and actually consuming it is out of scope. Keeps the campaign tight and the no-epoch property clean.
- **Ore point-deposits** (veins, lenses, ore bodies) — a *different object* from the areal field, needing its own draw (→ streams, epoch-adjacent). Named here, built later; the `prospectivity` field is its down-payment.
- **The full stratigraphic column** — the shallow 2-layer cover/basement stack ships; deep multi-layer stratigraphy needs deposition history (deep-time / Sculpting).
- **Lithology-as-driver feedback** — hard rock standing proud, permeability reshaping drainage, mineral-dust albedo. That is the big-ticket coupling and belongs to **Sculpting**, which consumes `induration`.
- **Observer-relative perception** — the field stays neutral (correct); *knowledge* of it (a dwarf's "good delving stone" vs a neolith's "red rock") is cultural, the EXP-7 render-seam, a later consumer campaign.
- **Loess / sediment-in-flux** — aeolian and fluvial transport is Sculpting; `alluvium` is the only transported material this campaign classifies (statically, from drainage).

## 9. Ideonomy provenance

Four passes converged, which is itself the "we're done" signal — marginal new-and-worth-it items ran 6 → 5 → 2 → 1, and round 4's four operators all pointed at the same single remaining idea (the vertical column). The load-bearing pulls, folded into the sections above: **the material-buffer / projection architecture** (r1 abstraction-lift, r2/r3 reconfirmation); **fertility as a vector** (r1 terroir); **appearance + footstep-sound** (r1/r4 paint & timbre); **prospectivity for deferred deposits** (r1 distribution); **hydrogeology from the rock×water cross-product and karst from negation** (r2); **ironstone + coal from the genesis tree** (r2); **active/passive margin polarity** (r3 symmetry) and **soil depth** (r3 direction); **the shallow stratigraphic column** (r4 cardinality) and **hazard-proneness** (r4 rate). Considered and declined: collapsing bedrock and soil into a single weathering spectrum (it entangles the clean terrain-native / terrain×climate seam).
