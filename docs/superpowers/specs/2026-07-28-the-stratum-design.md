# The Stratum — design

**Status:** spec, awaiting G3 review. **Design-only campaign: no
implementation.** The deliverable is this document and the campaign sequence
it proposes.
**Date:** 2026-07-28

## 1. The question

A room has exactly **one** biome slot. Three separate requests turn out to be
the same request against that slot:

1. **Can the sea's surface and its depths be different places?** A ship
   passing above; sea elves dwelling below.
2. **Should there be a two-tier biome system** — a niche (savanna) plus
   variants (cerrado, grass savanna, wooded savanna)?
3. **Can richer per-room features enrich descriptions, place names, and the
   rest?**

(1) and (2) are cardinality substitutions on the same slot, along two
different axes: (1) indexes it by *depth*, (2) by *variant*. (3) asks what
falls out downstream.

## 2. What exists today (measured, on `0c238f19`)

**One label per cell, chosen by the sea floor.**
`classify_marine(depth_m, sst_c, feature, upwelling) -> Biome`
(`domains/climate/src/biome.rs:349`) returns a single `Biome`. A cell over a
4,000 m plain is `Bathypelagic`. The classifier is handed the sea *surface*
temperature and then labels the cell by its *floor*.

**The enum conflates two taxonomies.** `Savanna` and `Bathypelagic` are not
siblings: one is a community type, the other a depth stratum. This is why the
classifier needs a precedence chain — `if feature == Ridge { return
HydrothermalVent }` returns a *community* where a *stratum* was expected. A
hydrothermal vent **is** abyssal; the enum forces a false choice. Request (1)
and that precedence hack are the same defect.

**The taxonomy is already fragmenting.** `culture::BiomeClass`
(`Forest | Grassland | Arid | Cold | Barren`, `domains/culture/src/
subsistence.rs:23`) is a hand-rolled formation-group facet invented because no
principled tier existed. The doc comment on `climate::Biome` reads "A biome
class — terrestrial or marine", describing something the enum is not.

**79% of sampled rooms have no authored prose.** A 120-room global sample:
80 marine, plus `Ice` (14) and `Shrubland` (1), all falling through
`variety_pool`'s two-item catch-all. Worse, `micro_habitat` applies land
clauses unconditionally, so the model emits category errors:

> `hydrothermal vent — broken terrain sun-warmed damp on a rise`
> `kelp forest — unremarkable ground shaded dry in a hollow`

A black smoker 2,500 m down, in permanent darkness, described as sun-warmed.

**The overlay facet exists and is degenerate.** `Regime` + `Negations
{substrate, energy, kingdom, endemic}` is a real variant overlay. Measured
across four seeds, the placed exotics are **92–98% fungal** (99/101, 128/134,
163/177, 137/147), because the candidate scores are not commensurable: fungal
scores `1.0 - unrest` (≥0.6 on any quiet land) against geothermal's bare
`unrest` (high only near plate boundaries).

**The vertical column is not new.** MAP-10 (the underworld) is `shipped` —
The Deep, The Lode, The Vestige — so a derived per-cell **rock** column
already exists. MAP-69 (the surface↔underground *relationship*) is the named
future campaign. The water column is the same shape as the rock column, and
should not be designed as if it were novel.

## 3. The taxonomy

Five facets. A room's biome becomes a **faceted expression**, not a single
enum value:

```
realm      : formation   : stratum      : variant      : overlay
-----------------------------------------------------------------
waterworld : reef        : epipelagic   : fringing     : —
waterworld : vent        : abyssal      : black-smoker : [endemic]
waterworld : open-water  : bathypelagic : —            : —
overworld  : savanna     : surface      : cerrado      : —
overworld  : savanna     : surface      : grass        : [fungal]
underworld : cavern      : (deep layer) : flowstone    : [chemosynthetic]
skyworld   : (later)     : high         : —            : —
```

Realm names above are illustrative; realm is a *triple*, not an enum — see
§3.4. The last two rows are not built by any campaign proposed here; they are
present to show the taxonomy expresses them without change.

| facet | what it answers | status today |
|---|---|---|
| **realm** | which world — `(medium, access, strata)`, see §3.4 | implicit (`is_marine()` predicate) |
| **formation** | which community | `Biome`, conflated with stratum |
| **stratum** | which layer of the column | conflated into `Biome` |
| **variant** | which expression of the formation | `variety_pool` strings, unnamed |
| **overlay** | what negates the ordinary | `Regime`/`Negations`, degenerate |

**Four of the five already exist in some form.** This is a disentangling, not
a rewrite — the single most important finding for scoping.

### 3.1 Formation vs stratum

`Biome`'s 22 variants split cleanly:

- **Formations** (community types): `Ice … Alpine`, `SeaIce`, `CoralReef`,
  `KelpForest`, `HydrothermalVent`, `Upwelling`.
- **Strata** (depth zones): `Epipelagic`, `Mesopelagic`, `Bathypelagic`,
  `Abyssal`, `HadalTrench`.

Once separated, `classify_marine` stops needing precedence: a ridge cell is
`(vent, abyssal)`, a warm shallow cell is `(reef, epipelagic)`, and the
"which wins" question disappears because nothing has to win.

### 3.2 Variants are constrained, not autonomous

Cerrado cannot occur in tundra. Variants are drawn from a **per-formation
pool** — which is exactly what `variety_pool` already is. Promoting those
authored strings to named variants with concept ids is most of the work, and
it is additive.

### 3.3 Strata and the room model

This is the expensive facet, and the reason it is spec'd here but built last.

A stratum is only *inhabitable* if a room can be at a depth. Three options,
with a clear recommendation:

- **(a) Extend `RoomAddr` with a depth index.** Touches addressing — which is
  save-format class. Most invasive.
- **(b) A depth *band*, following The Lintel.** The vessel already has a band
  mechanism: `self.inside` plus `enter`/`out` moves between the walk band and
  a structure's chambers. A water column is the same shape — `dive`/`surface`
  between strata at one coordinate. **Recommended:** it reuses a mechanism
  that shipped days ago, keeps `RoomAddr` untouched, and generalizes to
  MAP-69's surface↔underground relationship rather than competing with it.
- **(c) A describe-time parameter** (`locale --at LAT,LON --depth-m 500`).
  Cheapest, but strata become a query argument rather than a place, so nothing
  can *live* at one.

Note (b) makes the water column and the rock column the same mechanism, which
is the outcome MAP-69 wants anyway. That is the strongest argument for it.

### 3.4 Realm is a triple, not an enum

**Amended after G3 (owner: design for sky realms and, eventually, planes).**

The obvious `realm = {Terrestrial, Marine}` enum is wrong, and would have to be
torn out the first time a sky realm arrived. Three ideonomy passes converged
on a decomposition instead:

```
realm = (medium, access, strata)

Overworld      = (air-over-rock, default, [surface])
Waterworld     = (water,         dive,    [epipelagic .. hadal])
Underworld     = (rock,          descend, [The Deep's geological layers])
Skyworld       = (air,           fly,     [low .. high])
Plane of Fire  = (fire,          portal,  [...])
Astral         = (aether,        ritual,  [])
```

Three consequences, in ascending order of importance:

1. **`stratum` becomes realm-relative.** It is simply *position within this
   realm's column*. The pelagic zones and The Deep's geological layers stop
   being two constructs and become one — which is the same unification §3.3(b)
   argues for on the traversal side.
2. **Realms are never enumerated.** A new realm is a new *value*, not a new
   axis, so sky realms and eventually planes cost no taxonomy change.
3. **The discriminating property is `access`, not materiality.** Lattice-
   finding put the elemental planes in a diamond — material (they have a
   medium) yet reached like the immaterial ones — which rules materiality out
   as the discriminator. Column realms are reached by *continuous movement
   with a medium change* (walk / dive / fly / dig); planes by *transit*
   (portal, ritual, death). The engine already has both primitives: compass
   movement is continuous, and The Lintel's `enter`/`out` is a discontinuous
   band transition.

**The vertical scale is signed, and its extremes are already modelled.**
Placing realms on distance-from-surface gives `sky (+) / surface (0) /
sea (-) / rock (--)`, and the unnamed extremes turn out to be occupied
already: astronomy models what is above the air, and terrain/paleoclimate
model what is below the rock. What is missing at both ends is not content but
a *place representation* for it.

**The scale's limit is itself a design constraint.** Planes have no position on
it — the Astral is not at a depth. Forcing planes onto the depth axis would be
the error this section exists to prevent, which is why `access` and not
`position` is the primary discriminator.

**Nothing here is built in this campaign.** The requirement is only that the
taxonomy shipped by campaign 1 expresses realm as a triple, so that later
realms are values rather than migrations.

## 4. Determinism and epoch analysis

The load-bearing section. Each exposure was checked against the code, and the
one that matters most is checked against the *epoch* machinery, not inferred.

**Concepts: additive is free, renames are churn.** `EPOCH_COHORTS`
(`domains/language/src/accession.rs:34`) makes registry growth additive **by
construction** — a new concept appended as a new cohort lands strictly last
and displaces nothing. The existing biome concepts (`bathypelagic`, `alpine`,
…) sit in epoch 0. Therefore:

> **Design rule: the taxonomy must be additive over the existing biome concept
> names. Never rename or remove one.** A `bathypelagic` that becomes a stratum
> rather than a formation keeps its concept id; new formation and variant
> concepts append as a fresh cohort. This is the difference between a free
> change and one that moves every word derived from a displaced root.

**Scene is safe by construction.** `scene/tiles` emits `biome: Vec<u16>` as an
index into `biome_legend: Vec<String>` shipped in the same document
(`windows/scene/src/lib.rs:142`). The legend grows; no client breaks. Goldens
drift and regenerate.

**The census can be left untouched.** `dominant-land-biome` and
`flagship-biome` are Categorical, and categorical columns do **not** pin their
value domain in the schema (`windows/lab/src/schema.rs:53` writes only
`kind: "categorical"`).

> **Design rule: the biome metrics keep reporting the FORMATION.** Granularity
> stays where it is, so rows.csv does not move and no census regeneration is
> owed. Variant-level metrics, if ever wanted, are *new* columns — and adding a
> metric does owe a regen.

**`locale/room/v2` takes additive fields only.** `Locale.biome` is a
serialized `String` and save-format class. Adding `variant` / `stratum` as new
fields is additive and free; *changing what `biome` reports* is a meaning
change and would mint `v3`.

> **Design rule: `Locale.biome` keeps reporting the formation prose name,
> unchanged.** New facets arrive as new fields beside it.

**Committed facts.** Settlement `name-gloss` composes from site concepts and
is committed. Adding a variant concept to the composed list **would change
committed names** — a genuine save-format event.

> **O1 — RESOLVED at G3 (owner: yes, and the epoch is accepted).** Variants
> **do** participate in name glosses. This is the richest version of request
> (3): place names come to carry the land's own character. It rewrites every
> settlement name in every world, which is an epoch — accepted deliberately,
> not absorbed quietly. Campaign 2 owns the epoch and must state the new
> label suffix in its own spec; no campaign before it may change a gloss.

**What is free about request (3):** `SiteConcepts { concepts: &'a [&'a str] }`
(`domains/language/src/naming.rs:100`) is already **variadic**. If O1 resolves
toward participation, no naming-system change is needed at all — only the
epoch decision.

## 5. Proposed campaign sequence

Four campaigns. Each ships working software; none requires its successor.

1. **The formations** — split `Biome` into `formation` + `stratum` behind the
   existing public surface; `classify_marine` returns a pair and loses its
   precedence chain. Additive concepts, no epoch. Census-neutral by the rules
   in §4. **Must express realm as the §3.4 triple even though only the
   overworld and waterworld have values** — that is the whole cost of making
   sky realms and planes later cost nothing. *This is the enabler; nothing
   else is cheap without it.*
2. **The variants** — promote `variety_pool` strings to named variants;
   author marine formations (10) plus `Ice` and `Shrubland`; add the
   submersion guard so `micro_habitat` stops applying land clauses underwater.
   Fixes the 79% and the category errors. **Owns O1's epoch:** variants enter
   name glosses here, so every settlement name in every world is rewritten.
   That epoch is this campaign's headline risk and its spec must name the
   label suffix explicitly.
3. **The commensurable exotics** — rescale the overlay candidate scores onto a
   common footing. Small, and the placed tier stops being one repeated
   descriptor. Independent of 1 and 2; could go first if a quick win is wanted.
4. **The column** — the depth band (§3.3(b)), unifying the water column with
   the rock column and feeding MAP-69. The expensive one, deliberately last,
   and now with a taxonomy that already has a slot for what it produces.

## 6. Risks and open questions

- **O1 (above)** — variants in name glosses: rich but epoch-bearing. G3.
- **O2** — does the health battery move? `lab/health.rs` builds a
  `LocaleContext` and derives NPCs, so overlay changes *may* perturb the
  preregistered health metric. Unmeasured. Any campaign touching the overlay
  owes a before/after health run as its first step, not its last.
- **O3** — `culture::BiomeClass` overlaps the formation facet. It should
  probably be *derived from* formation rather than maintained in parallel, but
  it feeds subsistence and fertility, so folding it in touches the calibration
  battery. Deliberately out of scope here; noted so it is not forgotten.
- **The estimate that could be wrong.** "Four of five facets already exist"
  makes campaign 1 sound cheap. The disentangling touches nine crates that
  reference `Biome`. The claim is that no *consumer* breaks, because the
  public surface keeps returning a formation — but that has been reasoned, not
  demonstrated. **Campaign 1 owes a `cmp` of a seed-42 world as its first
  implementation step** (The Occlusion's lesson: a determinism claim checked
  along one route is not verified).

## 7. Out of scope

- Implementation of any kind. This campaign produces this document.
- Pronounceable settlement names (language-domain seed derivation; its own
  epoch-bearing campaign).
- MAP-69's full surface↔underground relationship; campaign 4 supplies the
  mechanism, not the content.
