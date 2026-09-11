# The Tidemark — the marine realm and its first tenant

**Campaign:** The Tidemark (provisional title).

**Classification:** Architectural continuation campaign.

**Base:** Current `origin/main` at campaign start (`f22860af3`). Takes up
`WAT-sea-peoples` (raw, high), whose structural parent is `MAP-11` (habitat
medium). Predecessors: The Vent and The Living Vent (the Waterworld overlay);
The Underworld, The Delvers, The Radiation and The Range (the non-surface
peopled realm template).

## 1. The one-sentence claim

A people can live in the sea, and the sea can stop supporting them.

## 2. What already exists — measured, not assumed

Every claim in this section is a command's output, taken at `f22860af3`.

**The realm axis already exists in climate, with three realms.**
`domains/climate/src/facets.rs` defines `Realm { medium, access }` with
`OVERWORLD` (`AirOverRock`/`Default`), `WATERWORLD` (`Water`/`Dive`) and
`UNDERDARK` (`Rock`/`CaveMouth`), and `Realm::strata()` gives the water column
five bands: `Epipelagic`, `Mesopelagic`, `Bathypelagic`, `Abyssal`, `Hadal`.

**The environment basis already carries marine communities.**
`domains/climate/src/axes.rs` assigns full vectors to `reef`, `kelp-forest`,
`vent`, `coral-head`, `kelp-canopy`, `smoker-field`, `vent-plume`,
`abyssal-plain`, and an open-water set in which "LIGHT descends the pelagic
ladder".

**The species realm axis is a partial duplicate of climate's.**
`species::HabitatRealm` is `{ Surface, Subterranean }` — the same axis as
`climate::Realm`, missing exactly the realm climate already names. The
duplication is **forced**: layering is constitutional and a domain may not
depend on a sibling, so `domains/species` cannot read `climate::Medium`. The
two can only be reconciled at the composition root.

**The Waterworld overlay has no non-test caller.**

```
$ grep -rn "waterworld_from(" --include=*.rs .
windows/worldgen/tests/suite/waterworld.rs:51      <- test
windows/worldgen/tests/suite/waterworld.rs:333     <- test
windows/worldgen/tests/suite/waterworld.rs:1326    <- test
windows/worldgen/src/waterworld.rs:405             <- the definition
```

`observe_waterworld`, `observe_waterworld_snapshot` and every construction of
`WaterWorldConfig` are the same: test-only. No `BuildDepth` rung constructs the
overlay, no CLI command reaches it, no window renders it.

**The Delvers' blocker does not bind here.** `habitat_realm_registry`'s own
comment records that two subterranean peoples were added and withdrawn because
"a kind whose identity is DEPTH cannot be expressed by an axis measured in
metres above sea level … They return when the underworld has biomes." The
marine realm already has biomes, strata and an access mode; that condition is
satisfied on arrival.

**The code anticipates a third realm.** `substrate_response`'s doc: "a third
realm variant stops the compiler here rather than falling into a default that
would silently score it as surface."

## 3. Design

### 3.1 `HabitatRealm::Marine`

A third variant. Per its own doc the `match` in `substrate_response` is
exhaustive with no wildcard, so **the compiler enumerates the sites** rather
than the spec guessing at them; the implementer reads the error list. The
registry stays sparse (absence means `Surface`).

`substrate_response(Marine)` follows the subterranean rationale one step
further. Subterranean devotion is 0.8 because "the habitat is the void and not
the floor"; for a marine kind the habitat is the water column and the seabed is
less accountable still, so devotion is lower again. The implementer authors the
three constants against that argument and states it in `plumb:` prose, as the
existing two do.

### 3.2 The availability mask

`per_species_suitability_masked` already carries `species_realm: &[HabitatRealm]`
and gates with an `availability` term that is a presence mask in `{0.0, 1.0}`,
deliberately outside the Liebig minimum because it is not a tolerance. Surface
is always `1.0`; Subterranean is `1.0` where any habitable rung has a cave and
`0.0` otherwise.

Marine mirrors it exactly: `1.0` where the vertex holds a water column, `0.0`
otherwise. The subterranean field is hoisted unconditionally and read only for a
`Subterranean` kind — "the derivation is pure, so this costs one map and no
draws" — and the marine field is built the same way.

### 3.3 Seating: the pelagic ladder is the delve ladder

The subterranean arm scores each rung of `Band::habitation()` and takes the
best. The marine arm scores each of `Realm::WATERWORLD.strata()`'s five bands
and takes the best. This is the campaign's structural bet: **the two ladders are
the same construct at different realms**, which `Stratum`'s own doc already
asserts ("the pelagic zones and (later) the underworld's geological layers are
the same construct at different realms, rather than two parallel ones").

### 3.4 The peopled kind

One kind, following drow's precedent exactly: a single authored separation from
its surface relatives — the realm gate — and no others. The trap `HabitatRealm`
names is "distinguishing two kinds by DEPTH, which nothing in the model can
say", so this campaign authors **one** marine people and not a merfolk/triton
pair. It carries an `EnvironmentNiche` row scored against the marine names
already in the basis, joining `drow` as the registry's second occupant.

### 3.5 The two-way agreement test

New, and the part with no precedent: a test at the composition root pinning
`species::HabitatRealm` against `climate::facets::Realm` as a **bijection**.
Both directions, because a one-directional check is structurally blind to
over-admission and still reads as total. This is what stops the forced
duplicate from drifting, and it is what makes the aerial realm cheap later.

## 4. The expiring habitat

This is what the campaign has that a port of the underworld template would not.

A cave does not cool and clog. A vent does: `VentState` runs `Absent → Nascent →
Active → Weakening → Failed`, the selected source may migrate within a fixed
candidate ring, and `Failed`'s doc records that "the source remains identifiable
after its contribution has failed". So the marine realm is the **first realm
whose habitat quality is time-varying**, and a settlement seated on a vent is
seated on something that can end.

The history domain already has the vocabulary: `OccupationRecord.cause` carries
`Famine`, `Burned`, `Plague`, `Fled` and `Migrated`. A vent entering `Failed`
under an occupied vertex should produce an occupation that ends with a cause,
not a settlement that silently persists on a dead vent.

**Scope limit.** This campaign makes vent phase an *input to seating and an
ending*. It does not model dispersal, larval recruitment, or a marine population
dynamic; see §6.

## 5. Wiring the overlay

`waterworld_from(world, terrain, climate, config)` needs exactly what the
`Settlements` rung of `build_to` already holds — `world`, `terrain` and
`climate` are all live there, and the rung returns at
`if depth <= BuildDepth::Settlements`. The overlay is constructed inside that
rung, before placement reads it, and handed on to `Full`.

`WaterWorldConfig { enabled }` stops being a test-only knob. Whether it survives
as a config at all, or the overlay simply builds whenever the world has marine
vertices, is an implementer's call to make against the rung's existing shape.

## 6. Determinism and save format

**This is additive, and that is verified rather than argued.**

`Seed::derive(label)` is FNV-1a over the label's bytes mixed with the parent,
so **every stream label is independently keyed**. A draw on one label cannot
perturb another's sequence.

`WaterVent` admission derives a *per-vertex sub-stream* —
`seed.derive(WATERWORLD_VENT).derive("vertex/<n>")` — then takes admission,
strength, temperature delta and chemistry from it in that order. Because the
sub-stream is keyed by vertex, vent admission at one vertex cannot shift draws
at another.

`WATERWORLD_VENT = "waterworld/vent/v1"` is **already** a registered label in
`windows/worldgen/src/streams.rs` and already appears in the committed stream
manifest. The Vent paid that cost; this campaign adds no stream label.

Consequently: no epoch suffix, no save-format break, and no reordering of any
existing stream's consumption. What *does* move is every artifact whose content
depends on placement, if a marine people places — which is the intended
observable and is handled by rebaseline, not by an epoch.

**The one open determinism question**, flagged for G3: seating reads a vent's
phase, which is a function of `WorldTime`. Placement must therefore name the
instant it reads. The campaign's position is that this is a pure read at a named
tick and consumes no draws; the implementer verifies it rather than assuming it.

## 7. What is deliberately NOT in this campaign

- **No trophic weight.** No marine kind gains a `CHEMOSYNTHATE` weight and no
  `TrophicMode` changes. `marine_chemosynthate_supply_field`'s doc assigns that
  to THE TENANT (underworld-larder metaplan, rung 4) by name. The split is by
  question, not by realm, and it is the split the underworld already uses:
  `drow` has a realm gate and no chemosynthate weight; `xorn` weights
  `CHEMOSYNTHATE` and settles nothing.
- **No species, metabolism, reproduction or per-organism ecology.** Explicit
  non-goals in The Vent and The Living Vent; unchanged here.
- **No aerial realm.** `MAP-11`'s third medium stays empty. The agreement test
  in §3.5 is what makes it cheap later; building it now would be speculative.
- **No second merfolk/triton kind.** §3.4.
- **No reef fragmentation, current networks or signal distortion.**
  `WAT-reef-fragmentation` and `WAT-signal-distortion` stay raw.

## 8. Preregistered measurement

Frozen before the code that would move it (decision 0016). Each states both
poles, so neither outcome is a failure.

**M1 — Do the two vent representations coincide?** There are two today:
`climate::Biome::HydrothermalVent` ("a hydrothermal-vent field on a spreading
ridge" — derived, no draw, and the one the live suitability path reads through
`marine_chemosynthate_supply_field`) and worldgen's `WaterVent` (a seeded
admission over seabed samples at threshold 0.25, carrying identity, strength and
succession phase). Report `|A|`, `|B|` and `|A ∩ B|` as **counts** at seeds 42,
7 and 3.

- *Prediction:* they do not coincide — the intersection is under half of the
  smaller set — because one is ridge-derived and the other is a seeded draw over
  seabed vertices.
- *If the prediction holds:* they are different phenomena and must be named
  differently; seating reads `WaterVent` (it is the only one carrying phase) and
  the campaign records that the biome is a separate, coarser thing.
- *If it fails* (large overlap): they are redundant representations of one
  phenomenon, and one must be derived from the other rather than both surviving.
  That is a larger finding than this campaign, and it is reported as the
  headline rather than absorbed.

**M2 — Does a marine people actually place?** Count marine settlements at seed
42 at `BuildDepth::Full`, with a floor **and** a ceiling: the campaign succeeds
if the count is at least 1 and under the count of surface settlements. Zero
means the realm gate admits nothing and the campaign has shipped an unreachable
kind; exceeding the surface count means the marine realm is outcompeting land,
which is a placement defect, not a success.

**M3 — Does the habitat expire?** Over a world-time sweep at seed 42, count
vertices whose marine availability is non-zero at one instant and zero at a
later one. The prediction is a non-zero count; a zero count means vent phase
reaches seating in name only, and would falsify §4 — the campaign's headline —
rather than merely underperforming.

**Negative control for M3:** the same sweep with vent phase held constant must
produce a count of zero. Without it, M3's non-zero count could come from any
time-varying term in the stack.

## 9. Open questions for G3

1. **The instant placement reads** (§6). Naming a tick is a determinism-contract
   call and leads this list for that reason.
2. **Which vent representation is authoritative** (§8, M1) — the campaign
   proposes `WaterVent` and measures before committing.
3. **Is one marine people right**, or does the campaign owe a second kind to
   prove the realm generalises? Drow's precedent says one; the counter-argument
   is that a single occupant cannot distinguish "the realm works" from "this kind
   works".
4. **The name.** "The Tidemark" is provisional; the branch is `campaign/the-tidemark`.
5. **The reading of the row** (ledger #2): residents of the water column, not
   Bronze Age Collapse raiders.
