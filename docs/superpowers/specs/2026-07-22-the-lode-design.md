# The Lode — Design

**Date:** 2026-07-22
**Status:** Approved (brainstorming session)
**Campaign:** The Lode (slug-named per decision 0026) — campaign 2 of the
subsurface arc (MAP-10 / DOM-14 / MAP-39 / TECH-3). *Working name; blessing
requested at G3.*
**Provenance:** The Deep gave the world a stratigraphic column; The Ground
gave it `cave_proneness_at` and `prospectivity_at` as *probability fields* and
explicitly deferred "ore point-deposits ... a different object from the areal
field." The Lode turns those two fields into **located, typed features** — the
world's first caves and ore deposits — the next rung up the subsurface
autonomy ladder (inert column → emplaced-but-inert anomalies). Four ideonomy
passes (2026-07-22) hardened the taxonomy; their load-bearing pulls are folded
in throughout and called out where they changed a decision. Deliberately the
**derived, inert** half of the features rung: the event-emplaced, agentic half
(mines, sealed vaults, dimensional gates, the maintained-containment
mechanism) is a separate later campaign (§11).

---

## 1. Goal

Give the world **caves** and **ore deposits** — located, typed features that
live *in* The Deep's column. For any land cell: is there a cave (and of what
type, reaching how far down), and is there an ore deposit (of what commodity,
by what genetic process, at what depth, of what grade and tonnage)? Caves give
mining its entry, deep-dwellers their reason-to-sit, and lightless phenomena a
venue; deposits are the resource/economy/technology driver (TECH-3) — what
mining seeks and what a later over/under commerce trades.

All of it is a **pure deterministic function of fields that already exist**
(the lithology `MaterialBuffer`, `cave_proneness`, `prospectivity`, `hydro`,
`boundary_distance`, `unrest`, drainage, the column's void bands). No new
randomness beyond reused hash-noise, no new save-format contract, no committed
facts. New *derived observations* are added (a features map, an almanac
section, census columns); the seed → world mapping is untouched. This is
**not an epoch** — exactly The Deep's and The Ground's stance.

Deliberately **purely geological**: the ore taxonomy is real economic geology
(iron, copper, gold, gems, salt, coal, bauxite…). **Magical ores** (mithril,
adamantine, glowstone) are metaphysics-gated taxonomy members (The Ground §8)
and stay reserved for the later metaphysics campaign, exactly as The Deep kept
the `thaumic` axis at zero.

## 2. Architecture: derived features over the column

**Placement — extend `domains/terrain`** (The Deep's and The Ground's home): a
new `domains/terrain/src/features.rs` (or `deposits.rs` + `caves.rs`) of pure
free functions over fields terrain already owns, surfaced through delegating
accessors on the `GeneratedTerrain` provider (the `cave_proneness_at` /
`column_at` pattern). No `TectonicGlobe` state beyond what already exists; the
features are re-derived on query.

**The unifying find (ideonomy pass 1, symmetry) — caves and ore are two faces
of one fluid-flow substrate.** A cave is a *void* (dissolution removed rock); a
vein is a *concentration* (fluids precipitated metal): anti-symmetric outputs
of the same subsurface fluid flow. Carbonate country dissolves into karst
caves **and** hosts lead-zinc; fault zones open fracture caves **and** channel
hydrothermal gold; basalt provinces drain into lava-tubes **and** carry native
copper. So both features are gated by **one shared fluid-flow intensity**
(derived from `hydro`/`cave_proneness`, `prospectivity`, `boundary_distance`,
`unrest`) — which makes **"cave country" and "ore country" co-locate by
construction**, no explicit coupling. This is geologically true, thematically
rich (the delver who follows caves finds veins), and a lab candidate.

**Provenance.** Both caves and deposits are `derived-geological` (The Deep's
provenance rule): recomputed from the seed, never ledgered. The event-emplaced
provenance (mines/seals/gates) is the deferred campaign (§11).

## 3. The two features

**Caves** — presence gated by `cave_proneness` × belt; when present, a **type**
from lithology and a **column-depth reach** (how far the void penetrates the
column — derived from `cave_proneness` and the depth of the cavernous
cover/carbonate bands above basement, indexing into The Deep's band depths):
- **Karst** — carbonate dissolution (wet limestone); MAP-10's canonical cave.
- **Lava-tube** — basaltic/volcanic provinces.
- **Fracture** — fault/unrest zones.

**Ore deposits** — the deposit "formula" (ideonomy pass 1, notation):

```
DEPOSIT ::= <setting> · <host-lithology> · <genetic-PROCESS>
            →  <commodity>  @ <depth-band>  [grade~lognormal, tonnage]
```

The slots, each a load-bearing refinement the notation forced:
- **The taxonomy axis is genetic PROCESS, not commodity.** The same commodity
  via different processes is a *different* deposit (a deep primary gold vein vs
  a shallow young gold placer), and the process sets the **depth-band and
  genesis-era**, coupling deposits into The Deep's column and archive (primary
  = old + deep; secondary = young + shallow).
- **Grade is a distribution, not a scalar** — lognormal (many poor, few rich):
  the rare rich strike is the prize; you cannot know a deposit's grade until
  you "find" it. Drawn from hash-noise — deterministic yet *apparently*
  stochastic (pass 4, predictability), the prospecting surprise for free.
- **Tonnage × grade is the economic axis** (pass 1, distribution): narrow-rich-
  small veins (a lone delver's find) ↔ disseminated-poor-huge porphyries (an
  industrial operation). Who can work it falls out of the pair.

## 4. The taxonomy (the well-formed tree)

Ideonomy pass 4's genesis tree, with the empty "residual" branch filled:

```
subsurface anomaly
├─ void ────────── karst · lava-tube · fracture
└─ concentration (deposit) — by genetic PROCESS:
   ├─ magmatic ──────────── copper (arc)                     [disseminated, huge, deep]
   ├─ hydrothermal ──────── gold / silver (fault/orogen)     [narrow, rich, deep] ⇄ fracture cave
   ├─ sedimentary-chemical ─ lead-zinc (carbonate) ⇄ karst · iron (BIF) · salt (evaporite) · coal
   ├─ igneous/metamorphic ─ gems + rare (pegmatite; ruby/sapphire in marble)
   ├─ residual/weathering ─ bauxite / nickel (laterite, hot+wet)  [climate-coupled, shallow, young]
   └─ placer (secondary) ── gold / tin / gem (alluvium)      [young, shallow]
```

Two implementation kinds fall out (pass 4):
- **Areal bedded deposits** — iron (BIF/ironstone), salt (evaporite), coal:
  **already `RockClass`es**, so the deposit layer *projects* from lithology and
  adds a grade/extent readout; no new point body.
- **Point / vein / belt bodies** — the metallic and gem deposits (copper, gold-
  silver, lead-zinc, gems/rare, bauxite/nickel, placers): genuinely new located
  bodies, the point process below.

**Commodity rarity spectrum, with reserved headroom** (pass 2): mundane ores
span common (iron, coal, salt, copper) → uncommon (lead-zinc, silver) → rare
(gold, gems); the spectrum's top tier is left **extensible** — the magical
ores slot there in the metaphysics campaign (The Ground's extensible-taxonomy
precedent). Rarity = inverse abundance = value.

**Laterite** (bauxite/nickel) is **climate-coupled** — hot + wet weathering,
the way The Ground's laterite *soil* already is — so it is a projection wired
at the worldgen composition root (terrain × climate), like The Ground's soil.

## 5. Co-location, belts, and depth

- **Co-location** (§2) is free: caves and deposits share the fluid-flow gate,
  so karst + lead-zinc, fracture + gold, lava-tube + native copper appear
  together. Lab candidate: cave-cell fraction and ore-cell fraction correlate
  where wet carbonate / faults / basalt provinces are.
- **Belts** (pass 1, connectivity): the point process biases along
  `boundary_distance` (lineament proximity), so ore **belts** emerge (the
  Andes-copper pattern) rather than uniform scatter — cheap, the field exists.
- **Depth & era** (pass 2/4): each deposit carries a depth-band and genesis-era
  from its process, indexing into The Deep's column and archive (primary deep +
  old; placer/laterite shallow + young).

## 6. The derivation procedure (ideonomy pass 3)

Per land cell, one clean pass — no forward integration, no draws:

1. Read the shared **fluid-flow intensity** (hydro/cave_proneness,
   prospectivity, boundary_distance, unrest).
2. **Belt-bias** the presence probability by `boundary_distance`.
3. **Cave:** hash-noise gated by `cave_proneness` × belt → present? → type from
   lithology, depth-reach from `cave_proneness` × the cover/carbonate band depth.
4. **Deposit:** hash-noise gated by `prospectivity` × belt → present? →
   process from setting, commodity/depth/era from process, grade from a
   setting-appropriate lognormal (hash-noise), tonnage from form.
5. Co-location is automatic (shared step-1 input).

**Sparse, land-only, one dominant deposit per cell** — the coarse grain (pass
3): the gate keeps features rare; ocean cells get none (seafloor massive
sulfides deferred, §11); a cell carries at most one dominant deposit (sub-cell
multi-body deferred, §11). All draws are hash-noise off a new derived label
(`terrain_seed.derive(streams::FEATURES)`) — **zero sequential draws, no
consumption-order contract**, exactly The Ground's "zero new stream draws"
mechanism.

## 7. Determinism and the no-epoch guarantee

- Features are **pure functions of existing fields + reused/derived hash-
  noise.** No new sequential stream draw; no new committed fact. World identity
  (seed → world) is untouched → **not an epoch**; `cli/tests/lens_purity.rs`
  stays green.
- The one new stream label (`FEATURES`, hash-noise only) is **additive** (a
  safe new `streams.rs` const), never a rename/reuse.
- Quantization stays at the emit boundary only (grades in the census CSV);
  the point process computes at full precision.
- New derived observations (features map, almanac section, census columns) are
  drift-checked and regenerated locally at close (`HV_CENSUS=1`); the census
  refresh is the standard carve-out.

## 8. Deliverables and consumer surface

1. **`features.rs`** in `domains/terrain` + provider accessors:
   `cave_at(cell) -> Option<Cave>`, `deposit_at(cell) -> Option<Deposit>`,
   where `Cave { kind, depth_reach }` and `Deposit { process, commodity,
   depth_band, grade, tonnage }`. Laterite's climate-coupled deposits wired at
   worldgen (the soil-projection seam).
2. **Lens:** a features map — caves and deposits over the globe (categorical by
   type/commodity), the belts visible (PNG, like the lithology/column lenses).
3. **Almanac** "The Lode" section (or an extension of "The Deep"): notable cave
   country, rich ore provinces/belts, the dominant commodities, co-located
   cave-and-ore regions.
4. **Census metrics:** cave-cell fraction (MAP-10's lab candidate — tracks the
   wet-limestone intersection), deposit density, dominant commodity, mean ore
   grade, cave-ore co-location correlation.
5. **Book (Definition of Done):** a chronicle entry + a freshness sweep;
   **MAP-39 / MAP-10 / DOM-14 / TECH-3 re-scored** on the Confidence Gradient
   (this campaign delivers the deferred deposits and the cave features). A
   one-page retrospective.
6. **Concepts/facts:** register the cave/deposit concepts; **no new committed
   facts** (the derived query is the deliverable — The Deep's stance;
   committing facts would move the world-identity golden).

## 9. Implementation sequencing

Each step compiles, passes, is a shippable increment:

1. The `Cave` type + cave point process (types + depth-reach) + `cave_at`.
2. The `Deposit` type + the point/vein/belt process (process/commodity/depth/
   era/grade/tonnage, belt bias) + `deposit_at`.
3. The areal-projected deposits (iron/salt/coal from existing `RockClass`es).
4. Laterite (climate-coupled) wired at worldgen.
5. Byte-identity guard (features are a pure, pin-invariant projection;
   `lens_purity` green).
6. The features map lens.
7. Almanac section; census metrics.
8. Book (chronicle + freshness sweep + Confidence-Gradient re-score);
   retrospective; close (local census regen).

## 10. Testing

- **Byte-identity / no-epoch:** `lens_purity` and existing world-identity
  fixtures stay green; features are a pure projection unperturbed by pins
  (the guard test The Deep established).
- **Co-location:** in wet-carbonate cells, karst caves and lead-zinc deposits
  co-occur above chance; the shared-gate correlation holds (the lab candidate).
- **Belts:** deposit density is higher near `boundary_distance` minima than in
  cratonic interiors (belts, not uniform scatter).
- **Taxonomy invariants:** each process maps to its expected depth-band and
  commodity; grade is in (0,1] and lognormal-shaped in aggregate; a placer is
  always shallower/younger than a primary vein of the same commodity.
- **Determinism:** same seed + pins → byte-identical features and census rows;
  the new `FEATURES` label draws zero sequential values (pin-isolation trivial,
  asserted).

## 11. Non-goals (deliberate scope fence, with hooks)

Every deferral is a rung of the same ladder; this campaign builds the inert
derived features and reserves the hooks.

- **Event-emplaced features** — mines/delvings, **sealed vaults**, **dimensional
  gates**, and the **seal-as-maintained-containment ≡ MEM-1** mechanism. The
  agentic half of the features rung; the provenance tag (`derived` vs `event`)
  and the lattice-ready column are the hooks. Its own later campaign.
- **Magical ores** (mithril, adamantine, glowstone) — metaphysics-gated
  taxonomy members (The Ground §8); the rarity spectrum's reserved top tier is
  the hook. The metaphysics campaign.
- **Cultural knowledge of deposits** — prospecting-as-perception ("a dwarf's
  good delving stone" vs a neolith's "red rock"); the deposit ships as neutral
  truth, the EXP-7 render seam deferred (The Ground's stance).
- **The fine underworld graph** — depth as a true coordinate, a cave network
  the walk traverses, the lightless venue vocabulary; the cave's depth-reach is
  the coarse hook, MAP-10's deferred deep tier.
- **Sub-cell multi-body deposits** — one dominant deposit per cell ships; a
  full per-cell assemblage / sub-cell bodies is the rejected richer option.
- **Seafloor deposits** — ridge massive sulfides, manganese nodules: land-only
  ships; the ocean is deferred.
- **Placer–drainage linkage** — the realism coupling where a placer sits
  *downstream of its primary vein* via drainage (pass 4, direction). v1 places
  placers by alluvium × prospectivity generically; the drainage-following link
  to a specific upstream primary is a **flagged spec-time deferral** (kept out
  to preserve the coarse grain; a clean follow-on).
- **Depletion / mining dynamics** — a deposit's tonnage is a static genesis
  given; extraction that depletes it (the ghost mine) is the active-sim/mining
  rung. Tonnage is the hook.

## 12. Ideonomy provenance

Four passes, no overturn — the design held at "typed, column-coupled, derived,
fluid-flow-unified"; the passes enriched *what the features carry*. **Pass 1**
(dimension-identification + negation → notation): the deposit formula, the
**process-not-commodity** axis, **grade-as-lognormal**, the **tonnage × grade**
economic axis, the **caves ≡ ore anti-symmetry / shared fluid-flow** unification
(the strongest find), and **belts** along lineaments. **Pass 2** (cross-domain +
combination → spectrum): the **rarity spectrum with reserved magical headroom**,
the **belt → district → deposit** hierarchy, deposits carry a **genesis-era**,
and the cave×ore co-location pairings (carbonate → karst + lead-zinc; basalt →
lava-tube + copper; fault → fracture + gold). **Pass 3** (organon + substitution
→ procedure): validated the one-pass derivation and fixed scope — **land-only,
sparse, one dominant deposit per cell, neutral truth (cultural knowledge
deferred)**. **Pass 4** (tree + abstraction-lift → tree): filled the empty
**residual/laterite** branch (bauxite/nickel, climate-coupled), sharpened the
**areal-vs-point** split, surfaced the **placer–drainage** coupling (deferred),
and reconfirmed the deterministic-yet-surprising grade. Considered and declined:
sub-cell multi-body deposits (coarse grain); seafloor deposits (land-only);
magical ores (metaphysics-gated); anthropogenic "deposits" — slag/middens (the
event/wrought rung).
