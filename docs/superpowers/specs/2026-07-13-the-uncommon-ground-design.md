# The Uncommon Ground — Room-Scale Variety, First Campaign (MAP-29)

**Status:** Design (brainstormed, pre-plan). Feeds an implementation plan.
**Registry row:** [MAP-29](../../../book/src/frontier/idea-registry.md) —
room-scale variety, the biome bestiary. This campaign ships the *first slice*
of that multi-campaign arc.
**Builds on (shipped):** MAP-28 the Room Mesh (`kernel/src/room.rs`,
`RoomAddr`) and the Locale Window (`windows/locale`), which already inherit a
room's biome (max-weight corner) and blend its continuous fields. This
campaign fills the `SubCellTexture` placeholder those left behind
(`windows/locale/src/lib.rs`, comment: *"kept ≤ 4 entries so it does not
pre-empt MAP-29"*).
**Design method:** brainstormed and run through three ideonomy passes
(negation × cross-domain re-instantiation → lattice; organon-construction ×
abstraction-lift → spectrum; combination × dimension-identification → graph).
The passes' findings are folded in below and cited inline as *(ideonomy #N)*.

---

## 1. Goal

Give an Earth-sized world enough **room-scale variety** to defeat the
"1,000 bowls of oatmeal" sameness — *at the natural tier only* — by overlaying
a combinatorially-generated **strangeness regime** on the climate biome a room
already inherits. A room keeps its categorical biome ("temperate forest") and
additionally carries a regime: a **negation vector** (which of a biome's
defaults it deviates from, and how far), a rendered **descriptor**, and a
derived **strangeness magnitude** that sets the prose register. Everything is
a pure function of address; nothing varies with time.

This is the P3 (weighted cross-product generator) and P7 (rarity budget)
nodes of the room-scale synthesis
([synthesis.md](../../design/room-scale/synthesis.md) §2), scoped to the
natural strangeness rungs.

## 2. Scope

Strangeness is a scalar `0 → 100` in the bestiary
([cycle-02](../../design/room-scale/cycle-02-biomes-and-palimpsest.md)); this
campaign caps it at **exotic (30)** and keeps every regime **steady**
(time-invariant).

**In scope:**

- **Rung 15 (EXTREME), derived** — desert/aquatic *varieties* conditioned
  purely on inherited fields (biome, temperature, moisture, elevation, relief).
  Always-on, no placement pass.
- **Rung 30 (EXOTIC), placed** — chemo/geothermal-energy negations, natural
  fluid-substrate seas, non-plant kingdoms, endemic isolates — placed sparsely
  by a rarity budget.
- The **negation-slot engine** (P3) and the **kernel weighted-choice
  primitive** it draws on.
- The **rarity budget** (P7) as a bounded genesis pass.
- A **substrate proxy** (a conservative stand-in for the unbuilt DOM-14
  lithosphere).
- Replacing `SubCellTexture` with the real `Regime`; a thin **observation
  surface** so the variety is visible.

**Out of scope (named, each to its real home):**

| Deferred | Why | Home |
| --- | --- | --- |
| Rung 45 RELICT (scars, blooms, mid-succession) | "out of equilibrium" is a *time* property; needs the deep-time path | MAP-30 palimpsest |
| Ephemeral / periodic biomes | needs a temporal phase clock | P8 (a future phase campaign) |
| Rung 60 ENGINEERED | needs a *maintainer* (civilization/wizard) | settlement + MAP-7 demography |
| Rung 75/90 AETHERIC/FAERIE + the negative wing (curse/blight/undeath) | magic must be gated behind a metaphysics pin | UNI-2 / DOM-17 metaphysics |
| Radial core→margin gradient; contagion/spread | these are the halo primitive | P5 (a future halo campaign) |
| True lithology (crystal, bone, exotic rock) | no rock-type field exists | DOM-14 lithosphere |
| `Namer` refactor onto the P3 engine | naming is a drift-checked save contract; high-surface | a later, isolated move |
| Promoting the `Grammar` orchestration into the kernel | YAGNI until a 2nd domain consumer | a later move |

## 3. Design principles (the decisions this campaign commits to)

1. **Overlay, never replace.** Strangeness is a new room-scale layer computed
   in `windows/locale`; it never re-quantizes or replaces climate's 22-biome
   categorical classification (respects layering and the MAP-30 sharpening /
   decision 0038: biome is mesh-bound categorical truth; only continuous
   fields refine by blend).
2. **Everything steady.** A regime is a pure function of address; `WorldTime`
   stays threaded-but-unused, as today. No clock, no state, no deep-time walk.
3. **Compose, don't catalogue** *(ideonomy #lattice, #cancer-hallmarks)*. The
   ~70-entry bestiary is *emergent coordinates*, not an enum: author ~9
   default-negations (M) and compose them (M+N reach), don't hand-write the
   cells (M×N).
4. **Derived is free; placed is budgeted** *(ideonomy #negation)*. The EXTREME
   rung is derivable from inherited fields (no pass); only the EXOTIC rung
   earns the rarity budget.
5. **Isolate risk.** No touching the climate biome enum, no touching `Namer`.
   The only new workspace-wide contract is a tiny weighted-choice draw
   semantics in the kernel.

## 4. Architecture & layering

```
  kernel     + weighted choice on Stream (the ONLY new workspace contract)
  domains/terrain
             + a public per-cell tectonic-proximity accessor (additive; if
               not already exposed) — read by the substrate proxy & the budget
  windows/locale  (owns everything else)
     - the Regime data model (negation vector, slots, magnitude)
     - the substrate proxy (DOM-14 seam)
     - the derived tier (rung 15) and the descriptor grammar
     - the StrangenessBudget genesis pass (rung 30), behind a trait seam
     - LocaleContext::describe → Regime; LocaleContext::strange_sites() query
  cli        + a `locale` readout / sampling surface (observation + artifact)
```

`domains/terrain` publishing a per-cell tectonic-proximity signal is exactly
what the trace protocol is for (a domain publishing a field); it introduces no
domain→domain dependency (locale is a window and may read domains).

### 4.1 The kernel weighted-choice primitive

`Stream` today offers uniform `pick` but no **weighted** choice
(`kernel/src/seed.rs`). Add one, beside `pick` and beside
`refine::choose_consistent` (whose docstring establishes "deterministic choice
among candidates" as a kernel concern whose *shape* should stay stable for
years):

```rust
// cumulative weights + one f64 draw; returns the chosen index.
// Frozen draw-semantics: weights consumed in slice order, threshold = next_f64()
// * total_weight, first cumulative bucket that exceeds the threshold wins.
impl Stream {
    pub fn weighted_index(&mut self, weights: &[f64]) -> Option<usize>;
}
```

This is the sole new save-format contract at kernel scope, and it is tiny. The
`Slot`/`Grammar` *orchestration* stays in `windows/locale`; it promotes to the
kernel only when a second domain wants it.

## 5. The regime data model

A regime is a **negation vector over exclusion slots**, plus a rendered
descriptor and a derived magnitude *(ideonomy #spectrum: strangeness is a
derived magnitude of a vector, not a primitive; #graph: negations need
exclusion slots, not a free cross-product)*.

```rust
struct Regime {
    base: Biome,                 // inherited, unchanged
    negations: Negations,        // the vector (see slots below)
    micro: MicroField,           // sub-cell grounded texture (§7.1), quantized at emit
    descriptor: String,          // rendered prose ("a wind-scoured hamada of bare basalt")
    strangeness: f64,            // DERIVED magnitude, quantized at emit
}

// A few grounded per-room continuous axes, from address noise; the descriptor
// reads these so homogeneous biome still varies room-to-room (§7.1).
struct MicroField {
    relief:   f64,               // hollow .. rise   (subsumes the old relief_jitter)
    aspect:   f64,               // insolation / slope facing
    wetness:  f64,               // local drainage
    openness: f64,               // canopy / cover
}

// One value per exclusion slot → composites are always coherent.
struct Negations {
    substrate: Substrate,        // pick-one  (Ordinary | Sand | Evaporite | Basaltic | Ashen | ...proxy-earned only)
    energy:    EnergySource,     // pick-one  (Sunlit | Chemo | Geothermal)
    kingdom:   Kingdom,          // pick-one  (PlantAnimal | Fungal | Crystalline | Microbial)
    endemic:   bool,             // toggle    (connectivity modifier; gated on an isolation signal, §7.2)
}
```

- **`strangeness` is derived**, a weighted count over how far each slot departs
  from its mundane value. Two rung-30 rooms can be strange in *different* ways
  at the same magnitude (more honest variety).
- **Slots guarantee coherence** *(ideonomy #combination)*: one draw per slot
  means no "sand *and* glass" sea can be generated. A slot's mundane value is
  its identity element; a fully-mundane regime is `strangeness == 0`.
- The **Mendeleev audit** is a test: the set of *reachable* slot-coordinates
  (after eligibility gating) must match authored descriptor content — empty
  cells are predictions, asserted, not silent gaps.

## 6. The substrate proxy (the DOM-14 seam)

The derived tier conditions on inherited fields, but the world has **no
lithology field** — temperature, moisture, and elevation only *(ideonomy
#graph: the fields are the hidden hub; the substrate sub-axis has no ground)*.
DOM-14 (lithosphere) is unbuilt. Rather than block, this campaign adds a
**conservative substrate proxy** that infers only the substrate distinctions it
can honestly earn from existing signals:

```
  SIGNAL (already available)              -> PROXY SUBSTRATE
  aridity + low relief                    -> Evaporite (salt pan, playa)
  tectonic/volcanic proximity (terrain)   -> Basaltic / Ashen
  arid + coastal                          -> Sand
  (none of the above)                     -> Ordinary
```

Everything the proxy *cannot* honestly infer — crystal, bone, and true rock
petrology — is **deferred to DOM-14**, not faked from the room seed. The proxy
lives behind a single `fn substrate_at(ctx) -> Substrate` so a real lithology
field later replaces it **without touching any consumer** (the grammar, the
budget, or the tests that read substrate).

## 7. The two tiers

### 7.1 Derived tier (rung 15) — free, always-on

Conditioned purely on inherited fields (biome + temperature + moisture +
elevation + relief + the substrate proxy). No genesis pass; a pure function of
the room, exactly like the blend `describe` already computes. Examples:
`Desert` + high relief → *hamada*; `Desert` + low relief + high aridity →
*erg*/*reg*; freshwater biomes → chalk-stream / blackwater / oxbow varieties.
This is most of the anti-oatmeal win for nearly free.

**The sub-cell micro-field — the answer to "miles and miles of forest."**
Because most of the world is *homogeneous* biome, within-biome variety is the
main case, and the placeholder's lone `relief_jitter` scalar barely beats
oatmeal. It is generalized into a small **`MicroField`** (§5): a few grounded,
per-room continuous axes a real biome genuinely varies along — micro-relief
(subsumes the old jitter), aspect/insolation (a damp north-facing hollow vs a
sun-warmed south slope), local wetness/drainage, and canopy openness — sampled
from the room's address noise (steady, address-pure, quantized at emit — pure
P1 sub-cell field refinement, no new dependency). The descriptor grammar (§8)
conditions on the `MicroField`, so two adjacent forest rooms differ by grounded
micro-habitat rather than a random pool pick — realistic, because a real forest
*is* that mosaic. This is the in-campaign lever for within-biome interest; the
deeper levers (path-dependence/palimpsest MAP-30, halos P5, succession P8,
inhabitants cycle-03) stay deferred, and this campaign is explicitly **not the
last enhancement in this area**.

### 7.2 Placed tier (rung 30) — the rarity budget (P7)

A `StrangenessBudget` pass over the ~10,242 canonical-grid cells, computed once
at `LocaleContext::build` and cached (bounded `O(cells)`; room-scale sampling
stays lazy):

- **Budgeted blue-noise placement.** Iterate a seeded permutation of eligible
  cells; accept a cell as a strange source if it clears a mutual-repulsion
  radius from already-accepted sources and the budget's remaining *mass* allows
  it; assign the exotic slot-values by weighted choice conditioned on the
  cell's fields.
- **Founder floor before the dart-throw** *(ideonomy #abstraction-lift → PCG
  hero-placement; kin to MAP-22)*. Pure field-weighted dart-throwing is
  probabilistic — a world with a strong volcanic province could, by unlucky
  draw, mint *zero* geothermal vents. So before the competitive placement,
  **reserve the single most-eligible cell of each strongly-warranted regime**,
  guaranteeing a world always mints at least one of an exotic its geology
  strongly implies. This reuses the founder-floor concept already in the
  codebase's vocabulary (MAP-22: reserve each people its best cell before
  competitive placement) and turns "the exotic feels earned by the land" from a
  probability into a guarantee.
- **Field-weighted acceptance** *(ideonomy #homogeneity, #scope)*. The
  acceptance weight is modulated by fields, so a chemo/geothermal negation is
  *likelier* near a tectonic boundary — strangeness **clusters where geology
  warrants** it, a cheap "province" feel without P5 halos, and placement reads
  as earned rather than sprinkled.
- **Budget mass.** A tunable defaulting to a small fraction of land cells
  (target ≈ 1%); drift-checked by a census (see §10). Eligibility gates enforce
  Mendeleev impossibility (no vent-garden in a random meadow).
- **Interface seam** *(ideonomy #scope)*. The budget is a trait, so a global
  single-mass allocator today can be swapped for a province-nested allocator
  later without churning `describe`'s consumers. Start global.
- **Placement is repulsive; the future negative wing is excitatory.** Named
  now so contagion/spread can be added later without unwinding the repulsion
  assumption baked into blue-noise.
- **One placed site = one cell** *(ideonomy #dictionary)*. Because placement
  repels sources, an exotic occupies a single ~240 km canonical cell (large
  enough for a sand-sea or a vent field); the engine never grows *contiguous
  multi-cell provinces* — those, and the core→margin structure within a patch,
  are a P5 halo concern, deferred.

Rooms in a placed cell inherit its exotic slot-values. The `endemic` modifier
is **not a free toggle** *(ideonomy #dictionary)*: it is gated on a derivable
**isolation signal** (a small or sealed landmass, a cave pocket) — a cell only
becomes eligible for endemism where geography actually isolates it.

### 7.3 Findability — `strange_sites()` (derived, not stored)

*(ideonomy #side-effect)* The budget's real byproduct is a **map of where the
strange things are** — the substrate for future scout/quest hooks, and as
valuable as the per-room prose. Per UNI-20 (store only the ledger, derive the
rest) it is **not stored**: it re-derives from the seed. It is exposed via a
public `LocaleContext::strange_sites() -> &[StrangeSite]` query — first-class
output, zero save surface.

## 8. The descriptor grammar

An ordered slot pipeline, each slot a weighted choice from a pool conditioned
on prior slots + context, assembled to prose:

```
  base-variety  ->  substrate-detail  ->  micro-habitat  ->  [exotic regime clause]
     "hamada"        "of bare basalt"     "on a wind-scoured    (rung 30 only)
                                           sunlit rise"
```

The **micro-habitat** slot reads the §7.1 `MicroField` (relief / aspect /
wetness / openness), so it varies room-to-room *within* a homogeneous biome —
this is where "interesting sameness" is actually rendered.

Register scales with `strangeness`: *"unremarkable heath"* (0) →
*"a wind-scoured hamada"* (15) → *"a methane-seep garden over cold vents"* (30).
Per-slot **stream-consumption order is a save-format contract** (the
pin-isolation discipline). The descriptor renders through the project's
existing content→render conventions (EXP-7), not a parallel prose voice
*(ideonomy #materiality)*.

The descriptor pools, the bestiary leaf names, and the negation-module
vocabulary are an **authoring-time artifact** *(ideonomy #source / #negation)*:
they may be offline-model-amplified and committed (drift-checked) per decision
0009 / MAP-5 (*models author, dice roll*), not necessarily hand-typed — the
*runtime* stays a deterministic dice-roll over committed content. This is how
the bestiary scales past what is comfortable to hand-write.

## 9. Determinism & save-format contracts

The load-bearing subtlety of this campaign.

- **New frozen contracts:** the kernel `weighted_index` draw semantics; the
  budget's stream label (`locale/strangeness/place`); one label per grammar
  slot and the micro-field
  (`locale/regime/{substrate,energy,kingdom,endemic,micro,descriptor}`). The
  retired `locale/aspect` and `locale/jitter` labels are removed with the
  schema bump (deliberate regeneration, not a rename).
- **Cross-platform trap (decision 0041).** The budget's accept/reject,
  eligibility, and field-weighted comparisons are **discrete decisions on
  continuous values** — precisely the class that diverged Linux vs macOS
  before libm. Therefore every threshold comparison in placement keys off
  **quantized or integer** quantities, and any transcendental routes through
  `kernel::math` (pure-Rust libm). This gets an explicit both-platform guard
  test.
- **Strangeness draws are isolated streams**, so the pre-existing
  blend/inheritance pins (the §14 Q4 regression in
  `windows/locale/src/lib.rs`) stay byte-identical — pin-isolation preserved.
- **`strangeness` quantized at emit**; the negation vector is integer/enum
  (platform-exact by construction).
- **Schema bump `locale/room/v1` → `locale/room/v2`.** The emitted shape
  changes (`SubCellTexture` → `Regime`). Regenerate the committed artifact
  `book/src/reference/locale-seed-42.json` and the stream manifest.

## 10. Testing

- **Kernel `weighted_index`** — determinism; distribution matches weights;
  degenerate cases (empty, single, zero-weight) — unit.
- **Locale byte-identity** — extend the existing `describe` determinism test
  across two contexts.
- **Mendeleev coordinate audit** — reachable slot-coordinates ↔ authored
  descriptor content; empty = asserted prediction.
- **Slot coherence** — no draw produces two substrates / two energies; a
  fully-mundane regime has `strangeness == 0`.
- **Prose register monotonicity** — descriptor register is non-decreasing in
  `strangeness`.
- **Local variety** *(ideonomy #atlas, the phenomenological page)* — adjacent
  rooms within a *homogeneous* cell produce distinguishable descriptors, so a
  walk through uniform biome does not feel monotonous. Complements the existing
  sibling-differ test; guards the "miles and miles of forest" failure directly.
- **Placement census** (live-worldgen, `heavy:` `#[ignore]` tier per
  `cli/tests/heavy_tier.rs`) — strange-cell count within budget mass; mutual
  spacing ≥ repulsion radius; eligibility respected (no exotic regime in an
  ineligible cell); field-weighting biases toward tectonic proximity.
- **Cross-platform placement guard** — the discrete accept/reject decisions are
  byte-identical on both platforms (quantization/libm discipline).
- **Pin-isolation regression** — the existing blend/inheritance pins stay
  green (strangeness perturbs no prior draw).

## 11. Observation surface

The payoff is variety you can *see*, so the campaign ships a thin readout. At
minimum: a `hornvale locale` sampling command rendering N sampled rooms across
biomes and both tiers, whose output is the drift-checked artifact that
replaces/updates `book/src/reference/locale-seed-42.json`. A gallery prose page
built on that command is optional polish, not required.

## 12. Error handling

- Reuse `LocaleError`. An over-spent budget or empty eligible set is **not an
  error** — it yields a mundane world (no strange cells). A regime whose
  required fields are absent is simply inapplicable.
- The budget pass is **total**: every cell resolves to a (possibly mundane)
  regime; `describe` never returns a room without one.
- Fail-fast only on genuine contract violations (an over-deep address is
  already `Unaddressable`; a slot referencing a nonexistent biome is a
  compile-time match).

## 13. Open questions & risks

1. **Budget mass calibration.** Too generous → strangeness becomes wallpaper;
   too stingy → the exotic never appears (synthesis §6 open question 3). The
   census pins it; the default (≈1% land cells) is a starting estimate to be
   calibrated, not a commitment.
2. **Terrain accessor** — *resolved at plan time.* The substrate proxy and
   field-weighted placement need a per-cell tectonic-proximity signal;
   `GeneratedTerrain::globe()` already exposes `unrest`, `boundary`, `plate_of`,
   `elevation`, `sea_level`, and `endorheic` (the last doubles as the endemic
   isolation signal), so **no new terrain accessor is required**. The one small
   *kernel* addition the plan surfaces is a bounded `Geosphere::hops_between`
   (integer BFS) for the placement repulsion radius.
3. **Proxy honesty.** The substrate proxy must stay conservative — every
   distinction it draws must be defensible from its signal, or it is deferred
   to DOM-14. The risk is scope-creep into faux-lithology.

## 14. Deferred seams (recorded so later campaigns inherit them cleanly)

Relict → MAP-30 palimpsest; ephemeral → P8 phase; engineered → settlement +
MAP-7; aetheric/faerie/negative wing → UNI-2/DOM-17 metaphysics; radial
gradient + contagion → P5 halos (the excitatory placement the repulsive budget
is labeled against); true lithology → DOM-14; `Namer` unification and
`Grammar`-to-kernel promotion → later isolated moves; province-nested budget →
behind the §7.2 trait seam; strangeness→salience feed → keep the scalar
separable, let it feed later, do not merge. **Visibility** *(ideonomy
#visibility)*: the natural tier is fully public — a hamada looks like a hamada,
and `strange_sites()` is an *authoring/tooling* view of where strangeness is,
not player knowledge; a traveler discovers a site by arriving. Hidden or
discoverable regimes (a strangeness present but undescribed until searched) are
a deferred MAP-2 epistemic-layer concern — named here so we do not silently
assume all-public forever. **Affordances** *(ideonomy #tree-finding)*: a regime
is a future source of verbs (ford a sand-sea, forage a fungal forest) — the X1
affordance cross-cut (MAP-27, verb-as-reaction) reads the regime stack, so the
`Regime` data model is a future verb source, not only prose; the verbs
themselves are deferred to MAP-27.

## 15. Definition of Done

Per the project's campaign DoD (CLAUDE.md, decisions 0013/0020/0030):

- Chronicle entry (`book/src/chronicle/`).
- One-page retrospective (`docs/retrospectives/`).
- Book freshness sweep of chapters the change touches (locale, reference).
- MAP-29 registry status flip: `elaborated` → partially shipped, noting what
  shipped (rungs 15–30, steady) vs what remains deferred.
- Artifact regeneration: `book/src/reference/locale-seed-42.json`, the stream
  manifest, and the new `locale` sampling artifact.
- Keystone refreeze; Confidence Gradient re-score only if a bet moved.
