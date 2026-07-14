# Sculpting — Design

**Date:** 2026-07-14
**Status:** Approved (brainstorming session)
**Campaign:** Sculpting (terrain epoch v3)
**Provenance:** Third and final leg of the ratified terrain-overhaul roadmap (Measured Coast → Crust → **Sculpting**). Crust's close handed forward two preregistered bands that its craton and lobing knobs provably cannot reach — coastline complexity and a true depositional shelf are products of *erosion and sediment* — and parked its non-goals here (terranes, microcontinents, fBm relief, belt anatomy, hotspot trails, discrete arcs). Nathan ratified the **full mandate**: all of it in one epoch. Depends on **The Ground** (merged first): the carve consumes its `induration` axis. Five ideonomy tuples (2026-07-14; two aimed at fantasy-setting modifications) shook loose fifteen pulls, all admitted; they are folded in throughout and called out where they changed a decision (§13).

---

## 1. Goal

Terrain epoch v3: a deliberate one-way regeneration of every world that (a) brings the two failed coast-texture bands inside — **shoreline-development 6.785 → 9.51–21.95** and **shelf-fraction 0.0483 → 0.08–0.22** — while keeping the four passing bands inside, and (b) lands the parked tectonic decorations. The engine is a **one-shot correction field** (engine A): erosion and deposition computed once at genesis as pure functions of fields that already exist, never a time-stepped simulation. Engines B (implicit steady-state solver) and C (fixed-N geomorphic iteration) are *designed-for but not built*: the carve sits behind a function seam they could fill, and a preregistered self-consistency diagnostic (§8) decides whether they are ever needed.

Performance is **measure-then-decide**: build the accurate thing, profile it, and bring the numbers to Nathan before tuning season. No fidelity cut is made silently.

## 2. Architecture: the genesis pipeline

One epoch, one regeneration; every stage remains a deterministic function of seed + pins.

1. **Plates + weights + edge noise** — unchanged skeleton; every existing stream survives with identical consumption order.
2. **Cratons → crust field, + terranes + microcontinents** — two new drawn sets with appended stream labels (`terrain/terranes`, `terrain/microcontinents`).
3. **Isostatic elevation + belt anatomy, discrete arcs, hotspot trails, trench, fBm relief** — derived terms and hash-noise; no draws.
4. **Induration field** — The Ground's axis, computed early: it depends only on crust age and boundary distance (both pre-elevation), so it precedes the carve. One function, two consumers (the carve here; the lithology buffer at stage 8).
5. **Provisional drainage** — on the pre-carve surface (the carve's input).
6. **THE CARVE (engine A)** — §5. Emits an elevation delta and a `sediment_thickness` field.
7. **Final elevation** = 3 + 6; **sea level re-solved** (percentile-exact, ocean-fraction-as-target per decision 0053); **final drainage** re-derived. Everything downstream reads only the final surface.
8. **Lithology buffer assembled in full** (The Ground) — over the carved surface, so soils and alluvium see real valleys; `soil_depth` and the alluvium classification upgrade from their drainage/slope proxies to consume `sediment_thickness` directly (a coordinated formula change inside the same epoch — one owner of the truth).
9. **Climate, biomes, placement, rooms** — consumers, unchanged code; orographic consequences flow through existing re-derivation.

**The swap seam.** `fn carve(elevation, drainage, induration_field, params) -> CarveDelta` — a pure function. Engine A is the only implementation this campaign; B or C would replace the body of stage 6 without touching stages 1–5 or 7–9. The seam is a function boundary, not a provider tier. It is also written **potential-agnostically** — semantically the carve takes *a potential field, a flow network derived from it, and a resistivity field*; water over elevation is its only instantiation this campaign, but nothing in the signature is water-specific (the banked ley-line re-instantiation — thaumic flux over a thaumic potential — reuses the machinery whole).

**Drainage computes twice** (provisional and final). That doubles the current single most expensive derivation; it is the first thing the §9 measurement watches.

## 3. The decorations

All upstream of the carve, so erosion works on their output.

- **Terranes** *(drawn set, `terrain/terranes`)* — exotic crustal slivers welded along leading-edge (active) margins: elongated crust-age/thickness discontinuities hugging the coast. They roughen coastal mountain structure, and The Ground's buffer picks up the age contrast automatically (lithology stripes for free).
- **Microcontinents** *(drawn set, `terrain/microcontinents`)* — tiny cratons drawn in ocean basins, Madagascar/Zealandia scale, deliberately sized **below** continent-count's 0.5%-of-land floor so they enrich oceans without moving that band.
- **Hotspot trails** *(derived, no draws)* — the existing 3–8 drawn hotspots smear into age-progressive chains along their plate's local Euler velocity (Hawaii–Emperor style): dome amplitude decays with implied age, chain length ∝ plate speed × a fixed age window.
- **Atoll chains** *(derived; ideonomy pull)* — trail seamounts whose decayed dome top sits below sea level in warm water (low |latitude|) cap with reef carbonate to just below sea level: Darwin's subsidence sequence at cell scale. The Ground's buffer reads the carbonate and classifies reef-limestone there without new code.
- **Discrete arcs** *(derived, hash-noise)* — island-arc boundary uplift stops being a continuous wall; along-strike modulation spaces it into chains of volcanic edifices.
- **Belt anatomy** *(derived)* — collision and coastal-range belts gain internal structure from boundary-distance bands: crest line, flanking foothills, and a **foreland basin** (subsidence trough on the continental side) that the carve then fills with sediment. Coastal ranges get their real ocean-steep asymmetry.
- **Trench** *(derived; ideonomy pull)* — a narrow deep line seaward of island-arc and coastal-range boundaries on the subducting side. Completes the active-margin asymmetry, thickens the hypsometric deep tail honestly, and gives the wedge's "the trench eats it" clause a real mouth to feed.
- **fBm relief** *(hash-noise)* — multi-octave zero-mean detail whose amplitude scales **up** with induration (hard rock stands craggy, soft rock lies smooth) and with belt proximity. Hash-noise labels documented in `streams.rs` as hash-noise-only (the `plate-edge` convention), never consumed as a `Stream`.

## 4. Shared field: induration

`induration_field(cell)` is the erodibility axis The Ground defines (metamorphic grade + grain, both pre-elevation inputs). Sculpting adds the requirement that it be computable at stage 4, before elevation exists; The Ground's buffer assembly at stage 8 reuses the same function. The carve's erodibility divisor reads **induration and carbonate together** — carbonate rock dissolves as well as abrades, so karst country lowers faster (the chemistry row of the ideonomy grid, one line of code, stated here so it is deliberate). The erodibility function is **total at its extremes**: behavior at induration → 1.0 is defined (asymptotically un-erodable), so the gated metaphysics overlay may later inject sentinel values (adamantine — exempt from incision and repose alike) without a formula change.

## 5. The carve (engine A)

Each term is a named tuning knob; the two failed bands each have a designated mover. **Knobs are global; heterogeneity comes only from fields** (induration, carbonate, margin polarity, drainage, per-mouth sediment supply) — no regionally jittered parameters. This is what keeps every knob attributable during the tuning season.

- **Incision** — stream power `(drainage area)^m × slope^n ÷ erodibility(induration, carbonate)`, applied along the provisional drainage tree: valleys deepen, interfluves stand. Where an incised valley meets the coast below sea level the sea floods it — **rias**, the shoreline-development mover.
- **Hillslope relaxation** *(ideonomy pull — the gravity agent)* — an angle-of-repose pass after incision: slopes above a critical gradient shed mass downslope until they respect it, mass-conserving. Without it, pure stream power produces knife-edge interfluves; every real landscape-evolution model pairs incision with a hillslope term.
- **Routing + floodplains** — eroded volume routes down-tree; where slope drops below a threshold a fraction deposits en route: flat-floored valleys and aggraded floodplains.
- **Coastal wedge (longshore drift, named)** — each mouth's exported volume spreads along-coast and offshore with distance decay — this *is* longshore transport, and the spec names it so its implied landforms stay visible (spits and barrier lagoons are a recognized extension, built only if the shoreline band needs the help). The wedge aggrades the seabed up to a cap just below sea level: the true **shelf mode**, the shelf-fraction mover. Spread radius reads margin polarity — wide on passive coasts, narrow on active ones, zero into the trench.
- **Deltas** *(ideonomy pull)* — the top-K mouths per world by sediment flux (K a tuning constant, initial 3) protrude as discrete deposition lobes rather than vanishing into the diffuse wedge. A landform, a provider query, and an almanac notable.
- **Endorheic basins** — interior sinks fill toward flat playa floors (pairs with The Ground's evaporite story).
- **`sediment_thickness`** *(ideonomy pull)* — the deposition field is exported as a first-class field, not discarded: The Ground's `soil_depth` and alluvium classification consume it (stage 8), and it is the natural substrate for any future stratigraphy.
- **Mass balance** — eroded ≈ deposited + oceanic loss, enforced by a property battery **asserted for the metaphysically-inert tier** — phrased tier-aware from day one so the banked gated overlay's ex-nihilo landforms (earthmotes, uphill rivers) never have to weaken a shipped battery.

**The ria–wedge antagonism (preregistered).** The two band-movers oppose each other at the same cells: incision carves rias (shoreline-development up); the wedge fills them into estuaries and coastal plain (shelf-fraction up, shoreline-development back down). Earth holds both bands simultaneously because sediment supply varies along the coast — starved stretches keep their rias while rich ones build plains. The per-mouth supply field provides that variation natively; the tuning season must watch the **joint** movement of the two bands, never each alone.

**Derived point observations** *(ideonomy pull — the intentionality prompt and the anti-repetition mandate)*: where a watercourse above a flux threshold crosses a sharp induration step, that is a **knickpoint — a waterfall site**; with deltas and playas these become provider queries and almanac notables ("the Great Falls of…", "the Great Delta"), the walk-scale landmarks that make carved terrain feel authored rather than generated. Each point observation carries an **open provenance source** (`Process`, the only member this campaign; `Mythic` banked, mirroring The Ground's rock provenance) — and since phenomena never reveal their producing system (constitutional), a later god-carved gorge lands indistinguishably beside an eroded one.

## 6. Determinism and epoch mechanics

- **Conservative epoch, Crust's precedent.** Every existing stream survives with identical consumption order; the two new drawn sets append after existing draws; pin isolation extends to them (pinned and unpinned paths consume identical draws — house pattern). All derived terms use hash-noise or no randomness.
- **Existing pins keep their semantics; no new pins this campaign.**
- The carve computes at full precision through `kernel::math` (libm, decision 0041); quantization stays at the emit boundary only. No wall-clock, no `HashMap`, `total_cmp` with deterministic tie-breaks — the carve's per-cell passes iterate in `CellId` order.
- **This is an epoch**: the `lens_purity` world-identity fixture is deliberately re-pinned (the mirror image of The Ground's must-stay-green); all committed artifacts re-baseline; golden pins re-pin **in the drifting commit**, never deferred to close. Censuses regenerate once, at close, on the AWS box (censuses are never regenerated locally).
- The eustatic dividend survives: Deep Time's sea-level swings over a *real* shelf flood and expose more area — checked as a regression probe, no paleo code changes.

## 7. Performance protocol (measure, then decide)

Benchmark seed-42 (and a small sweep) world-build time before the campaign and after each pipeline stage lands, in release mode; record the two drainage passes separately. The numbers go to Nathan **before tuning season begins**. Any fidelity cut (dropping a term, capping an iteration, coarsening a pass) is his call, made on evidence — never silent.

## 8. Acceptance: bands, batteries, and the escalation criterion

Judged on **v3@L6 medians at 2,000 seeds**; undershoot and overshoot both fail. Preregistered here, before any generator code:

```
metric                    band              status
------------------------  ----------------  --------------------------------
shoreline-development     9.51 - 21.95      must come inside (from 6.785)
shelf-fraction            0.08 - 0.22       must come inside (from 0.0483)
hypsometric-bimodality    2.0 - 8.0         must stay inside
continent-count           3 - 12            must stay inside
largest-continent-share   0.25 - 0.65       must stay inside
plate-size-gini           0.45 - 0.75       must stay inside
```

New features earn **batteries** (per-world property tests), not bands: mass balance; induration monotonicity (harder → less incised); shelf-width asymmetry (passive-margin median width > active-margin median); arc discreteness (component count along island-arc boundaries rises vs v2); trail existence (age-ordered seamount chains present); atoll placement (carbonate caps only on warm submerged trail seamounts); waterfall/delta existence probes.

**The A→B→C escalation criterion** *(ideonomy pull — preregistered so "see if it looks good" is a number)*: after the carve, re-derive drainage and measure the **rerouted-flow fraction** over each world's 20 largest rivers (share of pre-carve flux whose post-carve path diverges). Median across the tuning-probe seeds: **< 0.10** — engine A is self-consistent, ship it; **0.10–0.30** — flag, Nathan decides whether B enters evaluation; **> 0.30** — the one-shot lied to itself; A is rejected as sole engine and B enters. The diagnostic is cheap (both trees exist anyway) and becomes a permanent census column. A **second trigger** covers the consistent-but-underpowered state the reroute number cannot see: if, after the tuning season, a must-come-inside band is unreachable without pushing another band outside, B enters evaluation regardless of the reroute number.

Tuning iterates on modest-N lab probes; the 2,000-seed readout and census regeneration happen at close on AWS. A **Census of Coasts III** page records before/after, verdicts, and any supersession notes in the open. The Confidence Gradient bet this campaign resolves is re-scored in the same sweep (decision 0030).

## 9. Deliverables and consumer surface

1. **Terrain crate**: terranes + microcontinents in the crust stage; decoration terms in elevation; a new `carve` module behind the §2 seam; the shared induration-field function (coordination point with The Ground's lithology module); `sediment_thickness`, waterfall/delta/playa queries on the provider.
2. **Lenses**: the elevation map re-baselines; a carve-delta / sediment-thickness debug lens rides the rasterizer The Ground refactored.
3. **Census metrics**: shelf-width stats, sediment volume, waterfall count, delta count, rerouted-flow fraction — enrolled in the-census, regenerated at close.
4. **Almanac**: no new section; notables gain waterfalls/deltas/playas; numbers regenerate.
5. **Book**: chronicle entry; terrain.md v3 rewrite; Census of Coasts III; retrospective; epoch decision doc(s); idea-registry updates (aeolian/loess and spits/barriers rows; MAP-21 row re-dated to "judged now"); Confidence Gradient re-score.

## 10. Sequencing

The Ground merges first (its plan is in execution today); Sculpting's implementation plan starts only once the induration axis is real on main. Within the campaign, tasks follow the pipeline order (§2): decorations → induration seam → carve terms (incision → hillslope → routing → wedge → deltas/playas) → derived observations → lenses/metrics → tuning season → close. Campaign-branch absorptions of main at every plan-stage boundary, per process; never mid-measurement.

## 11. Testing

The genesis/tectonic property batteries extend, not replace: velocity tangency, plate-assignment totality, sea-level percentile exactness, and drainage determinism must all hold on the carved surface. New batteries per §8. Pin-isolation tests extend to the two new streams. Byte-identity (same seed + pins → identical world/almanac/artifacts) asserts across the epoch as always. Heavy sweeps carry the `heavy:` ignore-token and run in `make gate-full`/AWS; the commit gate stays ~4 min.

## 12. Non-goals

- **Engines B and C** — seam + preregistered escalation criterion only (§8).
- **MAP-21 rift-and-fit** — explicitly judged *after* Sculpting; the census-III baseline is its evidence.
- **Aeolian transport, loess, dune fields** — registry row; the parked list never included them.
- **Spits/barrier islands** — recognized longshore extension, built only if the shoreline band demands it (§5).
- **Glacial sculpting** — the contested cryosphere row (DOM-cryosphere).
- **Vegetation binding erosion** — circular (biomes derive from climate, downstream of terrain).
- **Abyssal fans / turbidites** — the deep-ocean cell of the grid stays empty this epoch.
- **Deposition history / full stratigraphy** — deep time; `sediment_thickness` is the hook, not the ledger.
- **New pins; adaptive mesh; any new climate-coupling code.**

## 13. Ideonomy provenance

Five tuples (2026-07-14; the last two aimed at fantasy-setting modifications), fifteen pulls, all admitted by Nathan. The fantasy pair produced four zero-cost inert-tier hooks — the potential-agnostic seam (§2), erodibility totality (§4), the tier-aware mass-balance battery (§5), the provenance slot on point observations (§5) — and six banked registry rows (ley-lines-as-thaumic-carve, Mythic landform drawn sets, anti-physics landforms, purposeful sculptors/wyrm tunnels, tidal causeways, and the single-metaphysics-dial umbrella row). The first three tuples: (1) hillslope/angle-of-repose term — the gravity agent was entirely absent from engine A (periodic-grid empty); (2) longshore drift named + spits/barriers banked; (3) discrete deltas at top-K mouths; (4) waterfall/knickpoint observations (intentionality prompt × the anti-repetition mandate); (5) atoll chains on trail seamounts (animacy prompt); (6) the trench; (7) `sediment_thickness` as an exported first-class field consumed by The Ground (materiality prompt); (8) the rerouted-flow escalation criterion (traffic-engineering re-instantiation of the one-shot's truncated feedback loop); (9) the ria–wedge antagonism preregistered as a joint-band tuning watch (state-machine trigger asymmetry); (10) the consistent-but-underpowered second escalation trigger (the state machine's missing transition); (11) the global-knobs / field-heterogeneity principle (homogeneity prompt).
