# The Seam — ECS Program, Campaign 1 — Design

**Status:** Draft for G3 review (2026-07-14) · **Author:** Claude (campaign-
autopilot) · **Decider:** Nathan · **Program:**
[ECS metaplan](2026-07-14-ecs-program-metaplan-design.md) · **Registry:** UNI-22
(flips `raw` → `elaborated` on this campaign's merge), BIO-25/26/27

> Campaign 1 of the strangler-fig ECS program. It cuts the seam every later
> campaign grows behind, and it does so by *finishing* the work The Niche
> deferred: the menagerie and the genesis cutover onto the differentiated
> stack. Shared commitments, the rate model, and the determinism contracts live
> in the metaplan; this spec details only campaign 1's concrete scope.

## 1. What this delivers

Three things, in one campaign, because they are one seam:

1. **The split.** `SpeciesDef` divides into universal identity + a
   `BiosphereTraits` component + an `Option<PeopledTraits>` component. Fauna
   carry `peopled: None`.
2. **The menagerie.** The canonical-5E roster (decision-ledger #26) is authored
   as **biosphere-only** entities — the first non-peopled kinds the stack has
   ever held, and the payoff The Niche proved was load-bearing (real strongholds
   need distinct *resource* niches, not four near-identical omnivores).
3. **The cutover.** Genesis stops reading the flat carrying path and reads the
   niche-differentiated K (The Niche's shadow becomes the real path).

The exit condition is behavioral: the menagerie produces genuine biogeographic
structure (strongholds / refugia the goblinoids could not), and every existing
peopled world is byte-identical across the split (the seam moved *where* traits
live, not *what* they compute).

## 2. The split, concretely

Today `SpeciesDef` (`domains/species/src/lib.rs`) is a flat struct. It divides
along the graph The Niche's exploration drew:

```
SpeciesDef {
  // universal identity / taxonomy (stays flat; KindId formalized in campaign 2)
  name, family,
  // BiosphereTraits — the component every entity has; the packer/habitat's input
  biosphere: BiosphereTraits { mass, metabolic_class, niche, condition_niche, potency },
  // PeopledTraits — present only for settling, speaking peoples
  peopled: Option<PeopledTraits {
    noun, psych, perception, articulation,
    worker_override, warrior, artisan, shaman, top,
  }>,
}
```

- **`family` stays universal** (draconic, plant, goblinoid are taxonomy). The
  peopled-only proto-language vector (`family_registry`) is consumed only on
  guarded peopled paths — a lookup, not a field on the struct.
- **`BiosphereTraits` is the packer and habitat's sole input.** The Niche's K
  layer already reads mass/niche/condition_niche/potency; the split changes only
  the *type it reads them from*.
- This is Option **A** of the entity-model exploration — deliberately
  composition-lite (one struct, one `Option`, determinism-safe: no `HashMap`).
  It is step 1 of the component model, not an alternative to it; campaign 2
  promotes `name` to a `KindId` label and keys a registry by it, campaign 3
  dissolves the struct entirely.

## 3. Guarding the peopled paths

Every consumer of a peopled field must guard on `peopled.is_some()`. The
consumers (from a sweep of `windows/worldgen`, `windows/lab`):

- **settlement genesis** (`noun`, `worker_override`, `warrior`, `artisan`,
  `shaman`, `top`, `psych`-driven castes/morphology/voice) — fauna do not
  settle; the settlement pass iterates `peopled.is_some()` species only.
- **phonology** (`draw_phonology` over `articulation`, proto via `family`) —
  fauna have no language.
- **perception lensing** (`perception_lens`, `pack_depths`, night-vision) —
  a peopled observation concern.

The habitat / niche-K layer, by contrast, iterates **all** species (it reads
`biosphere`). So the split falls along a clean line already latent in the code:
**settlement genesis is peopled-gated; habitat placement is universal.**

**The `species_carrying_input(base, psych)` wrinkle** (`worldgen:510`,
`lab/metrics:1247`): the flat carrying path modulates by psychology. It is
retired by the cutover (§4) — genesis moves to the biosphere-based niche-K, so
no fauna-psych default is invented. Until the cutover lands within this
campaign, the flat path stays peopled-only (it only ever ran for settling
species), so nothing breaks in between.

## 4. The cutover

The Niche shipped niche-K as an additive **shadow** (the `demography_report`
accessor), leaving genesis on the flat path so seed-42 stayed byte-identical.
Campaign 1 flips genesis onto niche-K (the deferred a15a patch, decision-ledger
#20/#30). This is the one step that *changes* committed artifacts — so it is
staged last, behind the equivalence shadow, and it is where the census fixtures
drift (the shared deferred AWS regen absorbs it; never local).

## 5. Menagerie authoring discipline

The roster is decided (decision-ledger #26): canonical 5E creatures spanning the
resource axes — photosynthate (treant/twig blight), plant-forage (giant
elk/woolly mammoth/giant goat), detritus (otyugh), mineral (xorn/rust monster),
animal-prey apex (chromatic dragons white/red/black + owlbear), with mighty
`potency > 0` (the dragons, the treant) exercising the sovereignty span. Each is
authored **biosphere-only** (`peopled: None`), grounded the way the goblinoids
were (decision-ledger #2/#19): D&D 5E canon for mass, ecology inference for the
resource niche and condition niche, potency for the mighty. Draconic and plant
**families** carry taxonomy (and, for the dragons, a climate-tiling of one apex
kind). Actual trait values are execution-time authoring against the shipped
fields (the C1-measured seed-42 ranges), not pre-specified here — the same
discipline the goblinoids used.

## 6. Tests (the standing gate, this campaign's slice)

- **Equivalence shadow:** `biosphere.mass/.niche/.condition_niche/.potency`
  equals the pre-split `SpeciesDef` fields for every existing kind; the split is
  a pure move.
- **Byte-identity:** every existing peopled world (seed-42 almanac, artifacts)
  is byte-identical across the split, *before* the §4 cutover. After the
  cutover, the drift is intended and lands with the AWS regen.
- **Fauna carry no peopled state:** a property test that every `peopled: None`
  kind is skipped by every peopled-gated pass (no settlement, no phonology).
- **Biogeography emerges (the payoff):** with the menagerie packed, the
  composition shows structure the goblinoids could not — a `composition-variance`
  above the goblinoid-only 0.065, and at least one resource-partitioned
  stronghold (a mighty apex holding a climate tile). This is the behavioral
  exit criterion; it replaces the goblinoid-era "2-way is sufficient" (#24)
  because the menagerie is exactly the disparate-resource-niche species #24
  named as the fix.
- No-`HashMap` / ordered-iteration and `cargo fmt`/clippy/type-audit clean, per
  the commit gate.

## 7. Stages

1. **Split** — introduce `BiosphereTraits` + `PeopledTraits`; `SpeciesDef`
   becomes `{ name, family, biosphere, peopled: Option<…> }`; mechanical field
   moves; the four peoples carry `peopled: Some(…)`. Equivalence + byte-identity
   green.
2. **Guard** — gate every peopled-path consumer on `peopled.is_some()`; the
   packer/habitat read `.biosphere`. Still byte-identical.
3. **Menagerie** — author the #26 roster biosphere-only (Nathan's roster is
   signed off; the authoring is execution work). The habitat layer places them.
4. **Cutover** — flip genesis onto niche-K (a15a); artifacts drift intentionally.
5. **Verify + close** — the biogeography-emerges test; chronicle + retro +
   UNI-22 registry flip; census fixtures ride the deferred AWS regen.

Stages 1–2 are the reversible seam (byte-identical); 3 adds content; 4 is the
one intended-drift step, staged last behind the shadow.

## 8. Boundary

**In:** the `SpeciesDef` split, the peopled guards, the biosphere-only
menagerie, the genesis cutover onto niche-K. **Out (later campaigns):** `KindId`
as a stable label and a registry keyed by it (campaign 2); dissolving
`SpeciesDef` into domain-owned registries (campaign 3); everything downstream.
The menagerie's *mighty* behaviors that need instances (a dragon's named lair —
BIO-27's deferred place-identity) wait for campaign 5 (instances); campaign 1
gives them their biosphere kind.

## 9. Open items

- **Census drift** — the cutover drifts census fixtures; resolved only by the
  shared AWS regen at some program milestone, never locally (carve-out; standing
  deferred batch).
- **Trait values** — authored at execution against measured fields; if a mighty
  apex needs the sovereignty coefficients retuned to hold a stronghold, that is
  an execution-time calibration on not-yet-frozen authored values (the B2b
  precedent, #23), surfaced if it arises.
