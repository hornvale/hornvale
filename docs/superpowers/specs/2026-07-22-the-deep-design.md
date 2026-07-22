# The Deep — Design

**Date:** 2026-07-22
**Status:** Approved (brainstorming session)
**Campaign:** The Deep (slug-named per decision 0026) — campaign 1 of the
subsurface arc.
**Provenance:** The first stone of the *underworld* (MAP-10 / DOM-14). It
realizes **the full stratigraphic column that The Ground (§8) explicitly
deferred** — deepening The Ground's shallow 2-layer `column` axis into an
N-band vertical profile — and turns that profile into the world's **deep-time
archive**: the one place the solid earth's past becomes legible after Deep
Time deliberately froze it. Eight ideonomy passes across the brainstorm (four
on the spatial spine, two on fantasy-geology, one breadth-atlas, one on
domain placement) hardened the design and are folded in throughout; their
load-bearing pulls and one overturn are called out where they changed a
decision.

---

## 1. Goal

Give the world an **under**. For any cell, a coarse **stratigraphic column** —
what lies beneath, band by band, from soil to the primordial substrate — where
each band carries *what it is made of*, *how hot it is*, *how void-prone it
is*, and, crucially, **the era it records**. The column is simultaneously a
spatial object (the vertical dimension terrain lacks) and a temporal one (the
deep-time record): **depth ≡ age ≡ forgottenness** is one axis, because
stratigraphy *is* the archive of the past, read top-down.

All of it is a **pure deterministic function of fields that already exist**
(crust age/thickness, tectonic setting, plate motion, the lithology buffer,
drainage, sediment thickness — plus, optionally, paleoclimate's strata where
that campaign ran). No new randomness, no new save-format contract, no change
to any existing world. New *derived observations* are added (a column map, an
almanac section, census columns); the seed → world mapping is untouched. This
is **not a terrain epoch** — world identity is byte-for-byte unchanged; the
campaign only *adds* readouts, exactly as The Ground did.

The campaign is deliberately **purely geological** (scope decision ratified in
brainstorming): the `thaumic` axis stays reserved-at-zero, the numen leg of
the depth spine and everything metaphysical defers to its own campaign (§10).

## 2. Architecture: the column as a deepening of the material buffer

**Placement — extend `domains/terrain`, do not add a domain** (the
brainstorm's one overturn). The column's era-stamps derive from terrain's
*own narrated tectonic history* (crust age, the rift-and-fit breakup age and
spreading rate), and its material bands *are* the vertical extension of The
Ground's `MaterialBuffer.column`. A domain may depend only on the kernel, so a
paleoclimate-reading domain cannot exist — and paleoclimate is only an
*optional shallow-strata refinement*, wired at the worldgen composition root
exactly as The Ground wires climate. So the column lives in a new
`domains/terrain/src/strata.rs`, next to `lithology.rs`. A standalone
`subsurface`/`depths` crate (DOM-14) is *reserved* for the later **agentic**
rungs (ecology, features, gates) that will *read* this column — spun out when
cross-domain content justifies the crate, not before.

**The primitive is a column, not a rock class** — the same lesson as The
Ground. `StratigraphicColumn` is a per-cell function `depth → BandSample`,
discretized into a small set of named bands (§3). Every consumer-facing
readout (a dominant-rock-at-depth, a geothermal temperature, a void-proneness,
an era) is a **projection** over it. The column implements the kernel `Field`
sampling contract so any grid at any level samples the same profile
(coarse-constrains-fine is exact); grid-bound reads (era, unconformities —
anything reading drainage/uplift/neighbours) compute on the canonical grid
(decision 0038).

**Resolution degrades with depth** (the ideonomy "log-compaction" pull, fantasy
pass A): the shallow bands are thin and specific, the deep bands thick and
coarse. This is cheap *and* thematically exact — the deep past is blurry — and
it caps the column's cost at a handful of bands regardless of world size.

**The column is a lattice, not a strict stack** (fantasy pass A). The ordered
bands are comparable by depth ≡ age, but the data model must **not assume
strict vertical ordering**, because the later features rung introduces
*crosscutters* (dikes, fault-veins, gate-scars, delvings) that punch new
material down through old eras. Reserving that structural room now costs
nothing and lets features land additively.

## 3. The bands

Five bands, fantasy-reframed away from Earth-committed names (a fantasy world's
deep interior is a *threshold to elsewhere*, not necessarily a nickel-iron
core). Top → bottom, resolution coarsening downward:

- **Regolith / soil** — the living skin: weathered cover, the `soil_depth`
  axis The Ground already carries. Fine resolution; youngest era.
- **Sedimentary cover** — the archive proper: deposited/volcanic surface rock
  (The Ground's `cover`), where deep-time is most legible and **unconformities
  live** (§4). Medium resolution.
- **Basement** — crystalline craton (granite/gneiss). **Inherited unchanged
  from terrain's existing 2-layer `column`** — this is the byte-identity hinge
  that keeps the campaign a refining tier, not an epoch. Coarse; old.
- **The Roots** — deep crust: hot, high-pressure, high metamorphic grade.
  Very coarse.
- **The Underneath** — the primordial substrate and threshold to the not-here:
  deepest, hottest, oldest — **pre-life / primordial era**, where the reserved
  thaumic slot and (much later) the gates will live. A single coarse band.

The *wrought* (buried ruins, delvings, slag — the "delved too deep" residue) is
deliberately **a feature, not a band** (fantasy pass A, discovery-vs-invention):
it is *invented*, present only under old settlement, and belongs to the
features rung and the settlement palimpsest (MAP-30). Keeping it out of the
bands keeps the bands cleanly natural-and-global.

## 4. The archive — deep-time made legible, by narration

Each band is stamped with the **era it records**, and the column carries a
**shared per-cell age-timeline** the bands index into (fantasy pass B,
decomposability): the bands are not independent fields but coordinates on one
per-cell ordering. Sources, all already present:

- **Deep bands** (basement, the Roots, the Underneath) — from `crust_age`, the
  rift-and-fit **breakup age** and spreading rate. The Underneath is
  primordial by construction.
- **Shallow bands** (cover, regolith) — from a coarse **deposition/erosion
  balance** over drainage, `sediment_thickness`, and uplift.
- **Optional refinement** — where the paleoclimate campaign ran, its ice-extent
  envelope and fossil shoreline refine the shallow strata (a glaciation record
  in the cover), wired at worldgen and **degrading gracefully** to the
  terrain-only stamp when paleoclimate did not run.

**Unconformities are the payoff.** Where the deposition/erosion balance implies
*missing time*, the timeline has a **gap** — an era that left no record. This
is the geological instance of the memory economy's floating gap (MEM-2): the
archive can *lie by omission*, and reading the deep past is *supposed* to be
uncertain. Narratively this is the Lovecraftian epistemic texture, for free;
mechanically it is a measurable field (a lab candidate: unconformity fraction
vs the erosion/uplift intersection).

**Narration, never integration.** The column is the *terminal state* of a
narrated trajectory through the rock-cycle-as-grammar (fantasy pass B), not a
tick-marched forward integrator — the rift-and-fit precedent, and the Lorenz
guard-rail (never seed a chaotic integrator from quantized floats; there is no
integrator here at all).

**Proxy-read seam, exposed but not consumed.** The column exposes
`era_at(cell, depth)` and `unconformity_at(cell)`. *Cultures reading the strata*
— historiography of the earth, through the epistemic filter stack (LANG-36),
which can get it wrong — is a later consumer campaign; this campaign only cuts
the seam.

## 5. Geothermal — the deep's energy base

A **geothermal field**: temperature rises with depth along a per-cell gradient
derived from crust thickness/age and tectonic setting (thin/young/oceanic crust
runs a hotter gradient; thick old craton, cooler). Pure function of existing
fields, no draws.

The design pull from the breadth atlas: expose it as an **energy base**, not
merely a rock temperature — the Pellucidar "inner sun" of the deep, the
heat-flux a later chemotroph/lithotroph ecology (PSY-10) consumes. So the
readout is a **heat-flux-relevant quantity**, not only a `Temperature`.
Coherent physical quantities cross the API as newtypes per the typed-quantities
discipline (a `GeothermalGradient` in K/km; heat flux in mW/m²), with
`type-audit` verdicts on every new `pub` primitive (CI is default-deny);
dimensionless indices stay bare `f64` with `bare-ok(ratio)` tags.

## 6. Determinism and the no-epoch guarantee

- The column and geothermal field are **pure functions of existing fields.**
  Sub-band patchiness reuses **existing noise** — **target zero new stream
  labels, zero new stream consumption order, no save-format contract.** If a
  new label proves unavoidable it is *additive* (a safe new `streams.rs`
  const), never a rename or reuse of an existing one.
- The column **contains terrain's existing 2-layer `column`/basement
  unchanged** and grows bands around it (§3). World identity (seed → world) is
  untouched → **not an epoch**: no re-baseline of existing artifacts, no
  golden-pin churn, no identity-keystone retirement. The `lens_purity`
  world-identity fixture must stay green throughout — a failure means an
  unintended epoch.
- Quantization stays at the **emit boundary only** (the column JSON, the census
  CSV) — the geothermal gradient and the archive compute at full precision;
  quantize at serialization per decision 0033.
- The column implements `Field`; the canonical-grid/views contract (0038)
  holds — pointwise sampling at any resolution, grid-bound projections on the
  canonical grid.
- New *derived observations* (the column map, the almanac section, census
  columns) are drift-checked like every artifact and regenerated **locally**
  at campaign close (`HV_CENSUS=1 bash scripts/regenerate-artifacts.sh`; the
  AWS remote gate is abandoned — this box is canonical).

## 7. Deliverables and consumer surface

1. **`StratigraphicColumn`** in `domains/terrain/src/strata.rs` + a `Field`
   impl, deepening The Ground's `column` axis; `column_at(cell)` /
   `band_at(cell, depth)` / `era_at(cell, depth)` / `unconformity_at(cell)` /
   the geothermal query on the terrain provider (and, for the optional
   paleoclimate-refined shallow strata, worldgen).
2. **Lens:** a column map — a **depth-slice** (choose a depth; render the band,
   temperature, or era there) as the primary lens (PPM, like the
   elevation/lithology maps); a cross-section along a transect is an optional
   second view.
3. **Almanac "The Deep" section:** dominant band structure, notables — deep
   cratonic keels, thick sedimentary archives, prominent unconformities
   ("the world forgot an age here"), the hottest geothermal provinces.
4. **Census metrics:** mean depth-to-basement, geothermal-gradient
   distribution, unconformity fraction, deepest-band material mix — added to
   the-census, drift-checked, regenerated locally at close.
5. **Book (Definition of Done):** a chronicle entry and a laboratory/reference
   chapter; a freshness sweep of any chapter the new fields touch; **MAP-10 /
   DOM-14 re-scored on the Confidence Gradient** (this campaign resolves part
   of both bets). A one-page retrospective in `docs/retrospectives/`.
6. **Concepts/facts:** register the column/era concepts; ledger emission is
   deliberately minimal (summary facts only — mean depth-to-basement, deepest
   era) — the field and its observations are the deliverable.

## 8. Implementation sequencing

Each step compiles, passes, and is a shippable increment:

1. `StratigraphicColumn` type + the band set + the `Field` impl, deepening
   `column`; the byte-identity fixture (`lens_purity`) as the guard oracle.
2. The geothermal field + the gradient newtype + type-audit tags.
3. The shared age-timeline + era-stamps for the deep bands (terrain-only).
4. The deposition/erosion balance → shallow-band eras + **unconformities**;
   the optional paleoclimate refinement wired at worldgen (graceful degrade).
5. The column map lens.
6. Almanac "The Deep" section; census metrics.
7. Book (chronicle + chapter + freshness sweep + Confidence-Gradient re-score);
   retrospective; close with the local census regen.

## 9. Testing

- **Byte-identity / no-epoch:** `lens_purity` and every existing world-identity
  fixture stay green; a metamorphic test that the inherited basement band
  equals terrain's pre-existing `basement` bit-for-bit.
- **Coarse-constrains-fine:** the column sampled at two resolutions agrees
  where they overlap (the 0038 property, as for crust thickness).
- **Archive properties:** the age-timeline is monotone in depth within the
  ordered stack; unconformity fraction tracks the erosion/uplift intersection
  (the lab candidate); deepest band is always the primordial era.
- **Geothermal:** gradient monotone with the crust thickness/age inputs;
  temperature strictly increases with depth.
- **Determinism:** same seed + pins → byte-identical column JSON and census
  rows; pin-isolation unaffected (zero new draws → trivially true, asserted).

## 10. Non-goals (deliberate scope fence, with hooks)

Every deferral below is a *rung of the same coarse→fine, inert→agentic ladder*;
this campaign builds the inert substrate and reserves the hooks so each lands
additively.

- **Thaumic flux / lodes / the "anomalous" (4th) rock origin / the numen leg**
  — the whole magical-energy layer (Nathan's second-energy-base idea) defers to
  a dedicated **metaphysics-activation** campaign. The `thaumic` buffer slot
  stays ≡ 0 (The Ground's reserved hook); the deepest band's primordial
  character is the physical down-payment on where the numen will later peak.
- **Located features** — caves-as-features (the `cave_proneness`/void axis is
  the down-payment), ore point-deposits (prospectivity → bodies), vaults,
  seals, and **dimensional gates**. The features rung; the lattice-ready data
  model (§2) and provenance-readiness (`derived` vs future ledgered `event`)
  are the hooks. The **seal-as-maintained-containment ≡ MEM-1** mechanism
  ("delved too deep" = the abandoned library) rides this rung.
- **The fine underworld graph** — depth as a true coordinate, lateral
  connectivity, a lightless venue vocabulary (the second customer of The Eyes'
  `Venue` enum), settlements that descend. MAP-10's deferred deep tier; waits
  until a people lives down there.
- **Coarse lateral flow fields** — groundwater, heat advection, gate-spread
  (the connectivity pull). Arrive with features, not the bare column.
- **Subterranean ecology** — chemo/lithotroph mats, funga, refugia (MAP-22's
  underdark stratum); the archive-keeps-what-the-surface-lost relict reading.
  Geothermal-as-energy-base is the hook.
- **The vertical relationship** — surface↔under as its own campaign:
  speciation-by-stratum (Morlock/Eloi over deep time), over/under commerce,
  predation-inversion, chthonic emergence, subterranean valence (one people's
  tomb is another's cathedral, LANG-36's valence filter).
- **Cultural reading of the strata** — historiography of the earth through the
  epistemic filter stack (LANG-36). The proxy-read seam (§4) is cut; reading it
  is a later consumer campaign.

## 11. Ideonomy provenance

Eight passes across the brainstorm. **Spatial spine (four passes):** converged
on C+ — the column as organizing spine, exposed through fields, annotated with
provenance-tagged features, the fine graph deferred; the passes reframed the
"column vs fields vs graph" question as three rungs of one ladder (spectrum),
surfaced the derived-base + ledgered-event split (negation), lifted the column
to *the ledger made spatial* (append-and-truncate log; abstraction-lift), and
found the **seal ≡ maintained-containment ≡ MEM-1** unification via immunology
(cross-domain re-instantiation) — the strongest single output. **Fantasy
geology (two passes):** resolution-degrades-with-depth, the column-as-lattice,
the age-timeline + unconformities data model, the rock-cycle-as-narration-
grammar, and the fantasy band reframe (the Roots, the Underneath). **Breadth
atlas (one pass):** mapped the subterranean tradition (Lovecraft, hollow-earth
pulp, dungeon games, Wells, dwarven, chthonic) each to a Hornvale *mechanism*
(sim-first: mechanism, not authored trope), surfaced **geothermal = the inner
sun** and the **autonomy ladder** (inert → agentic) as the arc's sequencing
spine, and revealed the **vertical-relationship** future campaign. **Domain
placement (one pass):** an inversion overturned the earlier "new domain"
suggestion — the archive needs only narrated *terrain* history, so the column
extends terrain (The Ground's home), paleoclimate being an optional worldgen-
wired refinement.

Considered and declined: a coarse underworld *graph* as the v1 spine (deferred
to the fine tier — a re-derivable byte-identical connectivity graph is hard and
premature); **thaumic activation in v1** (Nathan chose reserve-only — the
metaphysics gate stays shut for its own campaign).
